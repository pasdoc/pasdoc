unit PasDoc_TagManager;

{$I DEFINES.INC}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Types,
  ObjectVector;

type
  TTagManager = class;
  TTag = class;

  { @seealso TTag.Execute }
  TTagExecuteEvent = procedure(ThisTag: TTag; EnclosingTag: TTag; 
    const TagParameter: string; var ReplaceStr: string) of object;
    
  TStringConverter = function(const s: string): string of object;
  
  TTagOption = (
    { This means that tag expects parameters. If this is not included 
      in TagOptions then tag should not be given any parameters,
      i.e. TagParameter passed to @link(TTag.Execute) should be ''.
      We will display a warning if user will try to give
      some parameters for such tag. }
    toParameterRequired, 
    
    { This means that parameters of this tag will be expanded
      before passing them to @link(TTag.Execute).
      This means that we will expand recursive tags inside
      parameters, that we will ConvertString inside parameters,
      that we will handle paragraphs inside parameters etc. --
      all that does @link(TTagManager.Execute).

      If toParameterRequired is not present in TTagOptions than
      it's not important whether you included toRecursiveTags.

      It's useful for some tags to include toParameterRequired
      without including toRecursiveTags, e.g. @@longcode or @@html,
      that want to get their parameters "verbatim", not processed. }
    toRecursiveTags,
    
    { This means that this tag is allowed at toplevel of description, 
      but not inside parameter of any other tag.
      
      (We're talking here only about the case when "other tag"
      does have toRecursiveTags included. Without toRecursiveTags,
      tag parameters are always valid, not parsed, not checked
      for anything that looks like a tag etc.) }
    toTopLevel);
      
  TTagOptions = set of TTagOption;

  { If a tag has toRecursiveTags in TagOptions, then values of these
    type will specify exactly what content is allowed inside
    parameter of this tag.
    If toRecursiveTags is not included in TagOptions
    then ContentAllowedInside value for this tag doesn't matter. 
    
    There are four groups of content that we consider:
    
    - toTopLevel tags. These are never allowed inside.
    
    - Self tag, e.g. is 
        @@code(This is some @@code(code) that I wrote) 
      allowed ?
      This is decided by aiSelfTag.
      Note that if self tag is toTopLevel, then whether aiSelfTag
      is specified does not matter -- toplevel tag is never allowed
      inside any other tag (even in itself).
      
    - Non-toTopLevel tags, that are not equal to self.
      This is decided by aiOtherTags. 
      
    - Other content (i.e. normal text, paragraph breaks,
      various dashes, URLs, and literal @@ character
      (expressed by @@@@ in descriptions)).
      This is decided by aiNormalText. 
      
      If aiNormalText will not be included,
      then normal text (not enclosed within other @@-tags) will
      not be allowed inside. Only whitespace will be allowed, and 
      it will be ignored anyway (i.e. will not be passed to
      ConvertString, empty line will not produce any Paragraph etc.).
      This is useful for tags like @@orderedList that should only contain
      other @@item tags inside. 
  }
  TContentAllowedInsideOption = (
    aiSelfTag,
    aiOtherTags,
    aiNormalText);
    
  TContentAllowedInside = set of TContentAllowedInsideOption;

  TTag = class
  private
    FOnExecute: TTagExecuteEvent;
    FTagOptions: TTagOptions;
    FContentAllowedInside: TContentAllowedInside;
    FName: string;
    FTagManager: TTagManager;
  public
    { Note that AName will be converted to lowercase before assigning 
      to Name. }
    constructor Create(ATagManager: TTagManager;
      const AName: string; AOnExecute: TTagExecuteEvent;
      const ATagOptions: TTagOptions;
      const AContentAllowedInside: TContentAllowedInside);

    property TagOptions: TTagOptions read FTagOptions write FTagOptions;
    
    { TagManager that will recognize and handle this tag.
      Note that the tag instance is owned by this tag manager
      (i.e. it will be freed inside this tag manager). 
      It can be nil if no tag manager currently owns this tag. 
      
      Note that it's very useful in @link(Execute) or
      @link(OnExecute) implementations.

      E.g. you can use it to report a message
      by @code(TagManager.DoMessage(...)), this is e.g. used
      by implementation of TPasItem.StoreAbstractTag.

      You could also use this to manually force recursive
      behavior of a given tag. I.e let's suppose that you
      have a tag with TagOptions = [toParameterRequired],
      so the TagParameter parameter passed to handler was
      not recursively expanded. Then you can do inside your handler
      @longcode# NewTagParameter := TagManager.Execute(TagParameter) #
      and this way you have explicitly recursively expanded the tag.

      This is not used anywhere for now, but this will be used
      when I will implement auto-linking (making links without
      the need to use @@link tag). Then I will have to make @@nolink
      tag, and TTagManager.Execute will get a parameter
      @code(AutoLink: boolean). Then inside @@nolink tag I will
      be able to call TTagManager.Execute(TagParameter, false)
      thus preventing auto-linking inside text within @@nolink. }
    property TagManager: TTagManager read FTagManager;
    
    property ContentAllowedInside: TContentAllowedInside
      read FContentAllowedInside write FContentAllowedInside;

    { Name of the tag, that must be specified by user after the "@@" sign.
      Value of this property must always be lowercase. }
    property Name: string read FName write FName;
    
    property OnExecute: TTagExecuteEvent 
      read FOnExecute write FOnExecute;

    { This will be used to do main work when this
      @@-tag occured in description.

      EnclosingTag parameter specifies enclosing tag. This
      is useful for tags that must behave differently in different
      contexts, e.g. in plain-text output @@item tag will behave
      differently inside @@orderedList and @@unorderedList.
      EnclosingTag is nil when the tag occured at top level of the
      description.
   
      In this class this method simply calls @link(OnExecute) 
      (if assigned). }
    procedure Execute(EnclosingTag: TTag; const TagParameter: string;
      var ReplaceStr: string); virtual;
  end;

  { All Items of this list must be non-nil TTag objects. }
  TTagVector = class(TObjectVector)
    { Case of Name does *not* matter (so don't bother converting it to 
      lowercase or something like that before using this method). 
      Returns nil if not found. 
      
      Maybe in the future it will use hashlist, for now it's not needed. }
    function FindByName(const Name: string): TTag;
  end;

  TTagManager = class
  private
    FTags: TTagVector;
    FConvertString: TStringConverter;
    FAbbreviations: TStringList;
    FOnMessage: TPasDocMessageEvent;
    FParagraph: string;
    FSpace: string;
    FShortDash, FEnDash, FEmDash: string;
    FURLLink: TStringConverter;

    function DoConvertString(const s: string): string;
    function DoURLLink(const s: string): string;
    procedure Unabbreviate(var s: string);
    
    { This is underlying version of Execute.

      If EnclosingTag = nil then this is understood to be 
      toplevel of description, which means that all tags are allowed inside.
      
      If EnclosingTag <> nil then this is not toplevel.
      So toTopLevel tags are not allowed and other tags are allowed 
      on the basis of EnclosingTag.ContentAllowedInside. }
    function CoreExecute(const Description: string;
      EnclosingTag: TTag;
      WantFirstSentenceEnd: boolean;
      out FirstSentenceEnd: Integer): string; overload;

    function CoreExecute(const Description: string;
      EnclosingTag: TTag): string; overload;
  public
    constructor Create;
    destructor Destroy; override;

    { Call OnMessage (if assigned) with given params. }
    procedure DoMessage(const AVerbosity: Cardinal;
      const MessageType: TMessageType; const AMessage: string;
      const AArguments: array of const);

    { This will be used to print messages from within @link(Execute).

      Note that in this unit we essentialy "don't know"
      that parsed Description string is probably attached to some TPasItem.
      It's good that we don't know it (because it makes this class more flexible).
      But it also means that OnMessage that you assign here may want to add
      to passed AMessage something like + ' (Expanded_TPasItem_Name)',
      see e.g. TDocGenerator.DoMessageFromExpandDescription.
      Maybe in the future we will do some descendant of this class,
      like TTagManagerForPasItem. }
    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;

    { This will be inserted on paragraph marker (two consecutive newlines,
      see wiki page WritingDocumentation) in the text.
      This should specify how paragraphs are marked in particular
      output format, e.g. html generator may set this to '<p>'.
      
      Default value is ' ' (one space). }
    property Paragraph: string read FParagraph write FParagraph;

    { This will be inserted on each whitespace sequence (but not on 
      paragraph break). This is consistent with 
      [http://pasdoc.sipsolutions.net/WritingDocumentation]
      that clearly says that "amount of whitespace does not matter".
      
      Although in some pasdoc output formats amount of whitespace also
      does not matter (e.g. HTML and LaTeX) but in other (e.g. plain text)
      it matters, so such space compression is needed.
      In other output formats (no examples yet) it may need to be expressed
      by something else than simple space, that's why this property
      is exposed. 
      
      Default value is ' ' (one space). }
    property Space: string read FSpace write FSpace;
    
    { This will be inserted on @code(@@@-) in description,
      and on a normal single dash in description that is not a part
      of en-dash or em-dash.
      This should produce just a short dash.
      
      Default value is '@-'. 
      
      You will never get any '-' character to be converted by ConvertString. 
      Convertion of '-' is controlled solely by XxxDash properties of 
      tag manager.
      
      @seealso EnDash
      @seealso EmDash }
    property ShortDash: string read FShortDash write FShortDash;

    { This will be inserted on @code(@-@-) in description.
      This should produce en-dash (as in LaTeX).
      Default value is '@-@-'. }
    property EnDash: string read FEnDash write FEnDash;

    { This will be inserted on @code(@-@-@-) in description.
      This should produce em-dash (as in LaTeX).
      Default value is '@-@-@-'. }
    property EmDash: string read FEmDash write FEmDash;
    
    { This will be called from @link(Execute) when URL will be found
      in Description. Note that passed here URL will *not* be processed by
      @link(ConvertString). 
      
      This tells what to put in result on URL.
      If this is not assigned, then ConvertString(URL) will be appended
      to Result in @link(Execute). }
    property URLLink: TStringConverter read FURLLink write FURLLink;

    { This method is the very essence of this class and this unit.
      It expands Description, which means that it processes Description
      (text supplied by user in some comment in parsed unit)
      into something ready to be included in output documentation.
      This means that this handles parsing @-tags, inserting
      paragraph markers, recognizing URLs in Description and
      correctly translating it, and translating rest of the "normal" text
      via ConvertString.
      
      If WantFirstSentenceEnd then we will look for '.' char 
      followed by any whitespace in Description. 
      Moreover, this '.' must be outside of any @-tags
      parameter. Under FirstSentenceEnd we will return the number
      of beginning characters *in the output string* that will
      include correspong '.' character (note that this definition
      takes into account that ConvertString may translate '.' into
      something longer).
      If no such character exists in Description, FirstSentenceEnd will
      be set to Length(Result), so the whole Description will be treated
      as it's first sentence.
      
      If WantFirstSentenceEnd, FirstSentenceEnd will not be set. }
    function Execute(const Description: string;
      WantFirstSentenceEnd: boolean;
      out FirstSentenceEnd: Integer): string; overload;
      
    { This is equivalent to Execute(Description, false, Dummy) }
    function Execute(const Description: string): string; overload;

    property ConvertString: TStringConverter 
      read FConvertString write FConvertString;
    property Abbreviations: TStringList read FAbbreviations write FAbbreviations;
  end;

implementation

uses Utils;

{ TTag ------------------------------------------------------------  }

constructor TTag.Create(ATagManager: TTagManager;
  const AName: string; AOnExecute: TTagExecuteEvent;
  const ATagOptions: TTagOptions;
  const AContentAllowedInside: TContentAllowedInside);
begin
  inherited Create;
  FName := LowerCase(AName);
  FOnExecute := AOnExecute;
  FTagOptions := ATagOptions;
  FContentAllowedInside := AContentAllowedInside;
  
  FTagManager := ATagManager;
  if TagManager <> nil then
    TagManager.FTags.Add(Self);
end;

procedure TTag.Execute(EnclosingTag: TTag; const TagParameter: string;
  var ReplaceStr: string);
begin
  if Assigned(OnExecute) then
    OnExecute(Self, EnclosingTag, TagParameter, ReplaceStr);
end;

{ TTagVector ------------------------------------------------------------ }

function TTagVector.FindByName(const Name: string): TTag;
var 
  i: Integer;
  NameLower: string;
begin
  NameLower := LowerCase(Name);
  for i := 0 to Count - 1 do 
  begin
    Result := TTag(Items[i]);
    if Result.Name = NameLower then Exit;
  end;
  Result := nil;
end;

{ TTagManager ------------------------------------------------------------ }

constructor TTagManager.Create;
begin
  inherited Create;
  FTags := TTagVector.Create(true);
  FParagraph := ' ';
  FSpace := ' ';
  FShortDash := '-';
  FEnDash := '--';
  FEmDash := '---';
end;

destructor TTagManager.Destroy;
begin
  FreeAndNil(FTags);
  inherited;
end;

function TTagManager.DoConvertString(const s: string): string;
begin
  if Assigned(FConvertString) then
    Result := FConvertString(s)
  else
    Result := s;
end;

function TTagManager.DoURLLink(const s: string): string;
begin
  if Assigned(FURLLink) then
    Result := FURLLink(s)
  else
    Result := DoConvertString(s);
end;

procedure TTagManager.Unabbreviate(var s: string);
var
  idx: Integer;
begin
  if Assigned(Abbreviations) then begin
    idx := Abbreviations.IndexOfName(s);
    if idx>=0 then begin
      s := Abbreviations.Values[s];
    end;
  end;
end;

procedure TTagManager.DoMessage(const AVerbosity: Cardinal; const
  MessageType: TMessageType; const AMessage: string;
  const AArguments: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

function TTagManager.CoreExecute(const Description: string;
  EnclosingTag: TTag;
  WantFirstSentenceEnd: boolean;
  out FirstSentenceEnd: Integer): string;
var
  { This is the position of next char in Description to work with,
    i.e. first FOffset-1 chars in Description are considered "done"
    ("done" means that their converted version is appended to Result) }
  FOffset: Integer;

  { This checks if some tag starts at Description[FOffset + 1].
    If yes then it returns true and sets
    -- Tag to given tag object
    -- Parameters to params for this tag (text specified between '(' ')',
       parsed to the matching parenthesis)
    -- OffsetEnd to the index of *next* character in Description right
       after this tag (including it's parameters, if there were any)

    Note that it may also change it's out parameters even when it returns
    false; this doesn't harm anything for now, so I don't think there's
    a reason to correct this for now.

    In case some string looking as tag name (A-Za-z*) is here,
    but it's not a name of any existing tag,
    it not only returns false but also emits a warning for user. }
  function FindTag(out Tag: TTag;
    out Parameters: string; out OffsetEnd: Integer): Boolean;
  var
    i: Integer;
    BracketCount: integer;
    TagName: string;
  begin
    Result := False;
    Parameters := '';
    i := FOffset + 1;

    while (i <= Length(Description)) and
          (Description[i] in ['A'..'Z', 'a'..'z']) do
      Inc(i);

    if i = FOffset + 1 then Exit; { exit with false }

    TagName := Copy(Description, FOffset + 1, i - FOffset - 1);
    Tag := FTags.FindByName(TagName);
    OffsetEnd := i;

    if Tag = nil then
    begin
      DoMessage(1, mtWarning, 'Unknown tag name "%s"', [TagName]);
      Exit;
    end;
    
    Result := true;

    { OK, we found the correct tag, Tag variable is already set.
      Now lets get the parameters, setting Parameters and OffsetEnd. }

    if (i <= Length(Description)) and (Description[i] = '(') then
    begin
      { Read Parameters to a matching parenthesis.
        Note that we didn't check here whether
        toParameterRequired in Tag.TagOptions.
        Caller of FindTag will give a warning for user if it will
        receive some Parameters <> '' while
        toParameterRequired is *not* in Tag.TagOptions }
      Inc(i);
      BracketCount := 1;
      repeat
        case Description[i] of
          '(': Inc(BracketCount);
          ')': Dec(BracketCount);
        end;
        Inc(i);
      until (i > Length(Description)) or (BracketCount = 0);
      if (BracketCount = 0) then begin
        Parameters := Copy(Description, OffsetEnd + 1, i - OffsetEnd - 2);
        OffsetEnd := i;
      end else
        DoMessage(1, mtWarning,
          'No matching closing parenthesis for tag "%s"', [TagName]);
    end else
    if toParameterRequired in Tag.TagOptions then
    begin
      { Read Parameters to the end of Description or newline. }
      while (i <= Length(Description)) and
            (not (Description[i] in [#10, #13])) do
        Inc(i);
      Parameters := Trim(Copy(Description, OffsetEnd, i - OffsetEnd));
      OffsetEnd := i;
    end;
  end;

  { This checks whether we are looking (i.e. Description[FOffset] 
    starts with) at a pargraph marker
    (i.e. newline + 
          optional whitespace + 
          newline + 
          some more optional whitespaces and newlines)
    and if it is so, returns true and sets OffsetEnd to the next
    index in Description after this paragraph marker. }
  function FindParagraph(out OffsetEnd: Integer): boolean;
  var i: Integer;
  begin
    Result := false;
    
    i := FOffset;
    while SCharIs(Description, i, WhiteSpaceNotNL) do Inc(i);
    if not SCharIs(Description, i, WhiteSpaceNL) then Exit;
    { In case newline is two-characters wide, read it to the end
      (to not accidentally take #13#10 as two newlines.) }
    Inc(i);
    if (i <= Length(Description)) and
       ( ((Description[i-1] = #10) and (Description[i] = #13)) or
         ((Description[i-1] = #13) and (Description[i] = #10))
       ) then
      Inc(i);
    while SCharIs(Description, i, WhiteSpaceNotNL) do Inc(i);
    if not SCharIs(Description, i, WhiteSpaceNL) then Exit;
    
    { OK, so we found 2nd newline. So we got paragraph marker.
      Now read it to the end. }
    Result := true;
    while SCharIs(Description, i, WhiteSpace) do Inc(i);
    OffsetEnd := i;
  end;

  { This checks whether we are looking (i.e. Description[FOffset] 
    starts with) at some whitespace.
    If true, then it also sets OffsetEnd to next index after whitespace. }
  function FindWhitespace(out OffsetEnd: Integer): boolean;
  begin
    Result := SCharIs(Description, FOffset, WhiteSpace);
    if Result then
    begin
      OffsetEnd := FOffset + 1;
      while SCharIs(Description, OffsetEnd, WhiteSpace) do Inc(OffsetEnd);
    end;
  end;

  { Checks does Description[FOffset] may be a beginning of some URL.
    (xxx://xxxx/.../).
      
    If yes, returns true and sets OffsetEnd to the next
    index in Description after this URL.
    
    For your comfort, returns also URL (this is *always*
    Copy(Description, FOffset, OffsetEnd - FOffset)). }
  function FindURL(out OffsetEnd: Integer; out URL: string): boolean;

  { Here's how it works, and what is the meaning of constants below:

    Include all continuous AlphaNum chars.
    Then must be '://'.
    Include all continuous FullLinkChars and HalfLinkChars chars after '://'
    but then strip all HalfLinkChars from the end.

    This means that HalfLinkChars are allowed in the middle of URL,
    but only as long as there is some char after FullLinkChars
    but not at the end.
  }

  const
    AlphaNum      = ['A'..'Z', 'a'..'z', '0'..'9'];
    FullLinkChars = AlphaNum + ['_', '%', '/', '#', '~', '@'];
    HalfLinkChars = ['.', ',', '-', ':', ';', '?', '=', '&'];  
    URLMiddle = '://';
  var
    i: Integer;
  begin
    Result := False;
    
    i := FOffset;    
    while SCharIs(Description, i, AlphaNum) do Inc(i);
    if not (Copy(Description, i, Length(URLMiddle)) = URLMiddle) then Exit;
    
    Result := true;
    i := i + Length(URLMiddle);
    while SCharIs(Description, i, FullLinkChars + HalfLinkChars) do Inc(i);
    Dec(i);
    while (Description[i] in HalfLinkChars) do Dec(i);
    Inc(i);
    OffsetEnd := i;
    
    URL := Copy(Description, FOffset, OffsetEnd - FOffset);
  end;
  
  function FindFirstSentenceEnd: boolean;
  begin
    Result := (Description[FOffset] = '.') and 
      SCharIs(Description, FOffset + 1, WhiteSpace);
  end;
  
  function IsNormalTextAllowed: boolean;
  begin
    Result := (EnclosingTag = nil) or
      (aiNormalText in EnclosingTag.ContentAllowedInside);
  end;
  
  procedure CheckNormalTextAllowed(const NormalText: string);
  begin
    if not IsNormalTextAllowed then
      DoMessage(1, mtWarning,
        'Such content, "%s", is not allowed '+
        'directly within the tag @%s', [NormalText, EnclosingTag.Name]);
  end;

var
  { Always ConvertBeginOffset <= FOffset. 
    Description[ConvertBeginOffset ... FOffset - 1] 
    is the string that should be filtered by DoConvertString. }
  ConvertBeginOffset: Integer;

  { This function increases ConvertBeginOffset to FOffset,
    appending converted version of
    Description[ConvertBeginOffset ... FOffset - 1]
    to Result. }
  procedure DoConvert;
  var
    ToAppend: string;
  begin
    ToAppend := Copy(Description, ConvertBeginOffset, 
      FOffset - ConvertBeginOffset);
    if ToAppend <> '' then
    begin
      CheckNormalTextAllowed(ToAppend);
      Result := Result + DoConvertString(ToAppend);
      ConvertBeginOffset := FOffset;
    end;
  end;

var
  ReplaceStr: string;
  Params: string;
  OffsetEnd: Integer;
  FoundTag: TTag;
  URL: string;
begin
  Result := '';
  FOffset := 1;
  ConvertBeginOffset := 1;
  
  if WantFirstSentenceEnd then
    FirstSentenceEnd := 0;
  
  { Description[FOffset] is the next char that must be processed
    (we're "looking at it" right now). }

  while FOffset <= Length(Description) do
  begin
    if (Description[FOffset] = '@') and
       FindTag(FoundTag, Params, OffsetEnd) then
    begin
      DoConvert;
      
      { Check is it allowed for this tag to be here }
      if EnclosingTag <> nil then
      begin
        { To which category Tag belongs ?
          As outlined in comments at TTagsAllowedIndideOption,
          there are three groups: toplevel tag, self tag, and other. }
        if toTopLevel in FoundTag.TagOptions then
        begin
          { toplevel tags are never allowed here }
          DoMessage(1, mtWarning,
            'The tag "%s" cannot be embedded within other tags', [FoundTag.Name]);
        end else
        if FoundTag = EnclosingTag then
        begin
          if not (aiSelfTag in EnclosingTag.ContentAllowedInside) then
            DoMessage(1, mtWarning,
              'The tag "%s" cannot be embedded within itself', [FoundTag.Name]);
        end else
        if not (aiOtherTags in EnclosingTag.ContentAllowedInside) then
        begin
          DoMessage(1, mtWarning,
            'The tag "%s" cannot contain other tags', [EnclosingTag.Name]);
        end;
      end;
      
      { Process Params }
      if Params <> '' then
      begin
        if toParameterRequired in FoundTag.TagOptions then
        begin
          Unabbreviate(Params);
          if toRecursiveTags in FoundTag.TagOptions then
            { recursively expand Params }
            Params := CoreExecute(Params, FoundTag);
        end else
        begin
          { Note that in this case we ignore whether
            toRecursiveTags is in Tag.TagOptions,
            we always behave like toRecursiveTags was not included.

            This is reported as a serious warning,
            because tag handler procedure will probably ignore
            passed value of Params and will set ReplaceStr to something
            unrelated to Params. This means that user input is completely
            discarded. So user should really correct it.

            I didn't mark this as an mtError only because some sensible
            output will be generated anyway. }
          DoMessage(1, mtWarning,
            'Tag "%s" is not allowed to have any parameters', [FoundTag.Name]);
        end;
        ReplaceStr := DoConvertString('@(' + FoundTag.Name) + Params + ConvertString(')');
      end else
        ReplaceStr := DoConvertString('@' + FoundTag.Name);
        
      { execute tag handler }
      FoundTag.Execute(EnclosingTag, Params, ReplaceStr);

      Result := Result + ReplaceStr;
      FOffset := OffsetEnd;
      
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@@' then
    begin
      DoConvert;
      
      { convert '@@' to '@' }
      CheckNormalTextAllowed('@@');
      Result := Result + '@';
      FOffset := FOffset + 2;
      
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@-' then
    begin
      DoConvert;
      
      { convert '@-' to ShortDash }
      CheckNormalTextAllowed('@-');
      Result := Result + ShortDash;
      FOffset := FOffset + 2;
      
      ConvertBeginOffset := FOffset;
    end else
    { Note that we must scan for '---' in Description before scanning for '--'. }
    if Copy(Description, FOffset, 3) = '---' then
    begin
      DoConvert;
      
      { convert '---' to EmDash }
      CheckNormalTextAllowed('---');
      Result := Result + EmDash;
      FOffset := FOffset + 3;
      
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '--' then
    begin
      DoConvert;
      
      { convert '--' to EnDash }
      CheckNormalTextAllowed('--');
      Result := Result + EnDash;
      FOffset := FOffset + 2;
      
      ConvertBeginOffset := FOffset;
    end else
    if Description[FOffset] = '-' then
    begin
      DoConvert;
      
      { So '-' is just a normal ShortDash }
      CheckNormalTextAllowed('-');
      Result := Result + ShortDash;
      FOffset := FOffset + 1;      
      
      ConvertBeginOffset := FOffset;
    end else
    if FindParagraph(OffsetEnd) then
    begin
      DoConvert;

      { If normal text is allowed then append Paragraph to Result.
        Otherwise just ignore any whitespace in Description. }
      if IsNormalTextAllowed then
        Result := Result + Paragraph;
      FOffset := OffsetEnd;
      
      ConvertBeginOffset := FOffset;
    end else
    { FindWhitespace must be checked after FindParagraph,
      otherwise we would take paragraph as just some whitespace. }
    if FindWhitespace(OffsetEnd) then
    begin
      DoConvert;
      
      { If normal text is allowed then append Space to Result.
        Otherwise just ignore any whitespace in Description. }
      if IsNormalTextAllowed then
        Result := Result + Space;
      FOffset := OffsetEnd;
      
      ConvertBeginOffset := FOffset;
    end else
    if FindURL(OffsetEnd, URL) then
    begin
      DoConvert;

      CheckNormalTextAllowed(URL);
      Result := Result + DoURLLink(URL);
      FOffset := OffsetEnd;
      
      ConvertBeginOffset := FOffset;
    end else
    if WantFirstSentenceEnd and
       (FirstSentenceEnd = 0) and
       FindFirstSentenceEnd then
    begin
      DoConvert;
      
      CheckNormalTextAllowed('.');
      Result := Result + ConvertString('.');
      FirstSentenceEnd := Length(Result);
      Inc(FOffset);
      
      ConvertBeginOffset := FOffset;
    end else
      Inc(FOffset);
  end;

  DoConvert;

  if WantFirstSentenceEnd and (FirstSentenceEnd = 0) then
    FirstSentenceEnd := Length(Result);

  { Only for testing:
  Writeln('----');
  Writeln('Description was "', Description, '"');
  Writeln('Result is "', Result, '"');
  Writeln('----');}
end;

function TTagManager.CoreExecute(const Description: string;
  EnclosingTag: TTag): string;
var Dummy: Integer;
begin
  Result := CoreExecute(Description, EnclosingTag, false, Dummy);
end;

function TTagManager.Execute(const Description: string;
  WantFirstSentenceEnd: boolean;
  out FirstSentenceEnd: Integer): string;
begin
  Result := CoreExecute(Description, nil, 
    WantFirstSentenceEnd, FirstSentenceEnd);
end;

function TTagManager.Execute(const Description: string): string; 
var Dummy: Integer;
begin
  Result := Execute(Description, false, Dummy);
end;

end.
