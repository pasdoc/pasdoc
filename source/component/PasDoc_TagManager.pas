{
  Copyright 1998-2014 PasDoc developers.

  This file is part of "PasDoc".

  "PasDoc" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "PasDoc" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "PasDoc"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ @abstract(Collects information about available @@-tags and can parse text
  with tags.) }
unit PasDoc_TagManager;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Types,
  PasDoc_ObjectVector;

type
  TTagManager = class;
  TTag = class;

  { @seealso TTag.Execute }
  TTagExecuteEvent = procedure(ThisTag: TTag; var ThisTagData: TObject;
    EnclosingTag: TTag; var EnclosingTagData: TObject;
    const TagParameter: string; var ReplaceStr: string) of object;

  { @seealso TTag.AllowedInside }
  TTagAllowedInsideEvent = procedure(
    ThisTag: TTag; EnclosingTag: TTag; var Allowed: boolean) of object;

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
      that we will handle paragraphs inside parameters etc. ---
      all that does @link(TTagManager.Execute).

      If toParameterRequired is not present in TTagOptions then
      it's not important whether you included toRecursiveTags.

      It's useful for some tags to include toParameterRequired
      without including toRecursiveTags, e.g. @@longcode or @@html,
      that want to get their parameters "verbatim", not processed. 
      
      @bold(If toRecursiveTags is not included in tag options:)
      Then @italic(everything) is allowed within parameter of this tag,
      but nothing is interpreted. E.g. you can freely use @@ char,
      and even write various @@-tags inside @@html tag --- this doesn't
      matter, because @@-tags will not be interpreted (they will
      not be even searched !) inside @@html tag. In other words,
      @@ character means literally "@@" inside @@html, nothing more.
      The only exception are double @@@@, @@( and @@): we still treat them
      specially, to allow escaping the default parenthesis matching rules.
      Unless toRecursiveTagsManually is present. }
    toRecursiveTags,
    
    { Use this, instead of toRecursiveTags, if the implementation of your
      tag calls (always!) TagManager.CoreExecute on given TagParameter.
      This means that your tag is expanded recursively (it handles @-tags inside),
      but you do it manually (instead of allowing toRecursiveTags to do the job).
      In this case, TagParameter given will be really absolutely unmodified
      (even the special @@@@, @@( and @@) will not be handled),
      because we know that it will be handled later by special CoreExecute call.

      Never use both flags toRecursiveTags and toRecursiveTagsManually. }
    toRecursiveTagsManually,

    { This is meaningful only if toRecursiveTags is included.
      Then toAllowOtherTagsInsideByDefault determines
      are other tags allowed by the default implementation
      of @link(TTag.AllowedInside). }
    toAllowOtherTagsInsideByDefault,
    
    { This is meaningful only if toRecursiveTags is included.
      Then @name says that normal text is allowed
      inside parameter of this tag.
      @italic("Normal text") is anything except other @@-tags:
      normal text, paragraph breaks, various dashes, URLs, 
      and literal @@ character (expressed by @@@@ in descriptions).

      If @name will not be included,
      then normal text (not enclosed within other @@-tags) will
      not be allowed inside. Only whitespace will be allowed, and 
      it will be ignored anyway (i.e. will not be passed to
      ConvertString, empty line will not produce any Paragraph etc.).
      This is useful for tags like @@orderedList that should only contain
      other @@item tags inside. }
    toAllowNormalTextInside,
    
    { This is useful for tags like @@raises and @@param that treat
      1st word of their descriptions very specially
      (where "what exactly is the 1st word" is defined by the
      @link(ExtractFirstWord) function). This tells pasdoc to
      leave the beginning of tag parameter (the first word and
      the eventual whitespace before it) as it is in the parameter.
      Don't search there for @@-tags, URLs, @-- or other special dashes,
      don't insert paragraphs, don't try to auto-link it.
      
      This is meaningful only if toRecursiveTags is included
      (otherwise the whole tag parameters are always preserved "verbatim").
      
      TODO: in the future TTagExecuteEvent should just get this
      "first word" as a separate parameter, separated from TagParameters.
      Also, this word should not be converted by ConvertString. }
    toFirstWordVerbatim);

  TTagOptions = set of TTagOption;

  TTag = class
  private
    FOnPreExecute: TTagExecuteEvent;
    FOnExecute: TTagExecuteEvent;
    FTagOptions: TTagOptions;
    FName: string;
    FTagManager: TTagManager;
    FOnAllowedInside: TTagAllowedInsideEvent;
  public
    { Note that AName will be converted to lowercase before assigning 
      to Name. }
    constructor Create(ATagManager: TTagManager;
      const AName: string;
      AOnPreExecute: TTagExecuteEvent;
      AOnExecute: TTagExecuteEvent;
      const ATagOptions: TTagOptions);

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
      @longcode# NewTagParameter := TagManager.Execute(TagParameter, ...) #
      and this way you have explicitly recursively expanded the tag.

      Scenario above is actually used in implementation of @@noAutoLink
      tag. There I call TagManager.Execute with parameter
      @code(AutoLink) set to false thus preventing auto-linking 
      inside text within @@noAutoLink. }
    property TagManager: TTagManager read FTagManager;
    
    { Name of the tag, that must be specified by user after the "@@" sign.
      Value of this property must always be lowercase. }
    property Name: string read FName write FName;

    property OnPreExecute: TTagExecuteEvent 
      read FOnPreExecute write FOnPreExecute;
       
    property OnExecute: TTagExecuteEvent 
      read FOnExecute write FOnExecute;

    { This is completely analogous to @link(Execute) but used when
      @link(TTagManager.PreExecute) is @true. 
      In this class this simply calls @link(OnPreExecute). }
    procedure PreExecute(var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string); virtual;

    { This will be used to do main work when this
      @@-tag occured in description.

      EnclosingTag parameter specifies enclosing tag. This
      is useful for tags that must behave differently in different
      contexts, e.g. in plain-text output @@item tag will behave
      differently inside @@orderedList and @@unorderedList.
      EnclosingTag is nil when the tag occured at top level of the
      description.
      
      ThisTagData and EnclosingTagData form a mechanism to pass
      arbitraty data between child tags enclosed within one
      parent tag. Example uses:
      
      @unorderedList(
        @item(This is the way for multiple @@item tags
          inside @@orderedList tag to count themselves (to provide
          list item numbers, for pasdoc output formats that can't
          automatically number list items).)
          
        @item(This is the way for 
          @@itemSpacing tag to communicate with enclosing
          @@orderedList tag to specify list style. )
      
        @item(And this is the way for @@cell tags to be collected
          inside rows data and then @@rows tags to be collected
          inside table data. Thanks to such collecting 
          @link(TDocGenerator.FormatTable) receives at once all
          information about given table, and can use it to format
          table.)
      )
      
      How does this XxxTagData mechanism work:
      
      When we start parsing parameter of some tag with
      toRecursiveTags, we create a new pointer inited to 
      @link(CreateOccurenceData).
      When @@-tags occur inside this parameter, we pass them 
      this pointer as EnclosingTagData (this way all @@-tags
      with the same parent can use this pointer to communicate
      with each other). At the end, when parameter was parsed,
      we call given tag's Execute method passing the resulting 
      pointer as ThisTagData (this way @@-tags with the same parent
      can use this pointer to pass some data to their parent).

      In this class this method simply calls @link(OnExecute) 
      (if assigned). }
    procedure Execute(var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string); virtual;
      
    property OnAllowedInside: TTagAllowedInsideEvent
      read FOnAllowedInside write FOnAllowedInside;
      
    { This will be checked always when this tag occurs within description.
      Given EnclosingTag is enclosing tag, nil if we're in top level.
      If this returns false then this tag will not be allowed inside
      EnclosingTag.
      
      In this class this method 
      @orderedList(
        @item(
          Assumes that Result = true if we're at top level
          or EnclosingTag.TagOptions contains
          toAllowOtherTagsInsideByDefault.
          Else it assumes Result = false.)
        @item(
          Then it calls @link(OnAllowedInside
            OnAllowedInside(Self, EnclosingTag, Result)) 
          (if OnAllowedInside is assigned).)
      ) }
    function AllowedInside(EnclosingTag: TTag): boolean; virtual;
    
    { In this class this simply returns @nil. }
    function CreateOccurenceData: TObject; virtual;
    
    { In this class this simply does @code(Value.Free). }
    procedure DestroyOccurenceData(Value: TObject); virtual;
  end;
  
  TTopLevelTag = class(TTag)
    { This returns just @code(EnclosingTag = nil).
    
      Which means that this tag is allowed only at top level of
      description, never inside parameter of some tag. }
    function AllowedInside(EnclosingTag: TTag): boolean; override;
  end;
  
  TNonSelfTag = class(TTag)
    { This returns just @code(inherited and (EnclosingTag <> Self)).
    
      Which means that (assuming that @link(OnAllowedInside) 
      is not assigned) this tag is allowed at top level of
      description and inside parameter of any tag
      @italic(but not within itself and not within tags 
      without toAllowOtherTagsInsideByDefault).
      
      This is currently not used by any tag. }
    function AllowedInside(EnclosingTag: TTag): boolean; override;
  end;

  { All Items of this list must be non-nil TTag objects. }
  TTagVector = class(TObjectVector)
    { Case of Name does @italic(not) matter (so don't bother converting it to 
      lowercase or something like that before using this method). 
      Returns nil if not found. 
      
      Maybe in the future it will use hashlist, for now it's not needed. }
    function FindByName(const Name: string): TTag;
  end;
  
  TTryAutoLinkEvent = procedure(TagManager: TTagManager;
    const QualifiedIdentifier: TNameParts;
    out QualifiedIdentifierReplacement: string;
    var AutoLinked: boolean) of object;

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
    FOnTryAutoLink: TTryAutoLinkEvent;
    FPreExecute: boolean;

    function DoConvertString(const s: string): string;
    function DoURLLink(const s: string): string;
    procedure Unabbreviate(var s: string);
      
    function TryAutoLink(const QualifiedIdentifier: TNameParts;
      out QualifiedIdentifierReplacement: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Call OnMessage (if assigned) with given params. }
    procedure DoMessage(const AVerbosity: Cardinal;
      const MessageType: TPasDocMessageType; const AMessage: string;
      const AArguments: array of const);

    { Call @link(DoMessage) only if @link(PreExecute) is @false. }
    procedure DoMessageNonPre(const AVerbosity: Cardinal;
      const MessageType: TPasDocMessageType; const AMessage: string;
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
      
      Default value is '-'. 
      
      You will never get any '-' character to be converted by ConvertString. 
      Convertion of '-' is controlled solely by XxxDash properties of 
      tag manager.
      
      @seealso EnDash
      @seealso EmDash }
    property ShortDash: string read FShortDash write FShortDash;

    { This will be inserted on @code(@--) in description.
      This should produce en-dash (as in LaTeX).
      Default value is '@--'. }
    property EnDash: string read FEnDash write FEnDash;

    { This will be inserted on @code(@-@--) in description.
      This should produce em-dash (as in LaTeX).
      Default value is '@-@--'. }
    property EmDash: string read FEmDash write FEmDash;
    
    { This will be called from @link(Execute) when URL will be found
      in Description. Note that passed here URL will @italic(not) be processed by
      @link(ConvertString). 
      
      This tells what to put in result on URL.
      If this is not assigned, then ConvertString(URL) will be appended
      to Result in @link(Execute). }
    property URLLink: TStringConverter read FURLLink write FURLLink;

    { This should check does QualifiedIdentifier looks like a name
      of some existing identifier. If yes, sets AutoLinked to true and
      sets QualifiedIdentifierReplacement to a link to
      QualifiedIdentifier (QualifiedIdentifierReplacement should be 
      ready to be put in final documentation, i.e. already in the 
      final output format). By default AutoLinked is false. }
    property OnTryAutoLink: TTryAutoLinkEvent
      read FOnTryAutoLink write FOnTryAutoLink;

    { This method is the very essence of this class and this unit.
      It expands Description, which means that it processes Description
      (text supplied by user in some comment in parsed unit)
      into something ready to be included in output documentation.
      This means that this handles parsing @@-tags, inserting
      paragraph markers, recognizing URLs in Description and
      correctly translating it, and translating rest of the "normal" text
      via ConvertString.
      
      If WantFirstSentenceEnd then we will look for '.' char 
      followed by any whitespace in Description. 
      Moreover, this '.' must be outside of any @@-tags
      parameter. Under FirstSentenceEnd we will return the number
      of beginning characters @italic(in the output string) that will
      include correspong '.' character (note that this definition
      takes into account that ConvertString may translate '.' into
      something longer).
      If no such character exists in Description, FirstSentenceEnd will
      be set to Length(Result), so the whole Description will be treated
      as it's first sentence.
      
      If WantFirstSentenceEnd, FirstSentenceEnd will not be set. }
    function Execute(const Description: string;
      AutoLink: boolean;
      WantFirstSentenceEnd: boolean;
      out FirstSentenceEnd: Integer): string; overload;
      
    { This is equivalent to Execute(Description, AutoLink, false, Dummy) }
    function Execute(const Description: string;
      AutoLink: boolean): string; overload;

    { This is the underlying version of Execute. Use with caution!

      If EnclosingTag = nil then this is understood to be 
      toplevel of description, which means that all tags are allowed inside.
      
      If EnclosingTag <> nil then this is not toplevel. 
      
      EnclosingTagData returns collected data for given EnclosingTag.
      You should init it to EnclosingTag.CreateOccurenceData.
      It will be passed as EnclosingTagData to each of @@-tags 
      found inside Description. }
    function CoreExecute(const Description: string;
      AutoLink: boolean;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      WantFirstSentenceEnd: boolean;
      out FirstSentenceEnd: Integer): string; overload;

    function CoreExecute(const Description: string;
      AutoLink: boolean;
      EnclosingTag: TTag; var EnclosingTagData: TObject): string; overload;

    property ConvertString: TStringConverter 
      read FConvertString write FConvertString;
    property Abbreviations: TStringList read FAbbreviations write FAbbreviations;
    
    { When @name is @true, tag manager will work a little differently than usual:
    
      @unorderedList(
        @item(Instead of @link(TTag.Execute),
          @link(TTag.PreExecute) will be called.)
          
        @item(Various warnings will @italic(not) be reported.
        
          Assumption is that you will later process the same text
          with @name set to @false to get all the warnings.)
          
        @item(AutoLink will not be used (like it was always false).
          Also the result of @link(Execute) will be pretty much
          random and meaningless (so you should ignore it).
          Also this means that the TagParameter for tags with
          toRecursiveTags should be ignored, because it will be
          something incorrect. This means that only tags
          without toRecursiveTags should actually use
          TagParameter in their OnPreExecute handlers.
        
          Assumption is that you actually don't care about the
          result of @link(Execute) methods,
          and you will later process the same text
          with @name set to @false to get the proper output.
          
          The goal is to make execution with PreExecute set to @true
          as fast as possible.)
      ) }
    property PreExecute: boolean
      read FPreExecute write FPreExecute;
  end;

implementation

uses PasDoc_Utils, StrUtils;

{ TTag ------------------------------------------------------------  }

constructor TTag.Create(ATagManager: TTagManager;
  const AName: string; 
  AOnPreExecute: TTagExecuteEvent;
  AOnExecute: TTagExecuteEvent;
  const ATagOptions: TTagOptions);
begin
  inherited Create;
  FName := LowerCase(AName);
  FOnPreExecute := AOnPreExecute;
  FOnExecute := AOnExecute;
  FTagOptions := ATagOptions;
  
  FTagManager := ATagManager;
  if TagManager <> nil then
    TagManager.FTags.Add(Self);
end;

procedure TTag.PreExecute(var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if Assigned(OnPreExecute) then
    OnPreExecute(Self, ThisTagData, EnclosingTag, EnclosingTagData,
      TagParameter, ReplaceStr);
end;

procedure TTag.Execute(var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if Assigned(OnExecute) then
    OnExecute(Self, ThisTagData, EnclosingTag, EnclosingTagData,
      TagParameter, ReplaceStr);
end;

function TTag.AllowedInside(EnclosingTag: TTag): boolean;
begin
  Result := (EnclosingTag = nil) or
    (toAllowOtherTagsInsideByDefault in EnclosingTag.TagOptions);

  if Assigned(OnAllowedInside) then
    OnAllowedInside(Self, EnclosingTag, Result);
end;

function TTag.CreateOccurenceData: TObject;
begin
  Result := nil;
end;

procedure TTag.DestroyOccurenceData(Value: TObject);
begin
  Value.Free;
end;

{ TTopLevelTag ---------------------------------------------------------- }

function TTopLevelTag.AllowedInside(EnclosingTag: TTag): boolean;
begin
  Result := EnclosingTag = nil;
end;

{ TNonSelfTag ----------------------------------------------------------- }

function TNonSelfTag.AllowedInside(EnclosingTag: TTag): boolean;
begin
  Result := inherited AllowedInside(EnclosingTag) and (EnclosingTag <> Self);
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
  MessageType: TPasDocMessageType; const AMessage: string;
  const AArguments: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

procedure TTagManager.DoMessageNonPre(const AVerbosity: Cardinal;
  const MessageType: TPasDocMessageType; const AMessage: string;
  const AArguments: array of const);
begin
  if not PreExecute then
    DoMessage(AVerbosity, MessageType, AMessage, AArguments);
end;

function TTagManager.TryAutoLink(const QualifiedIdentifier: TNameParts;
  out QualifiedIdentifierReplacement: string): boolean;
begin
  Result := false;
  
  if Assigned(OnTryAutoLink) then
    OnTryAutoLink(Self, QualifiedIdentifier, 
      QualifiedIdentifierReplacement, Result);
  
  if Result then
    DoMessage(3, pmtInformation, 'Automatically linked identifier "%s"',
      [GlueNameParts(QualifiedIdentifier)]);
end;

function TTagManager.CoreExecute(const Description: string;
  AutoLink: boolean;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
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
          IsCharInSet(Description[i], ['A'..'Z', 'a'..'z']) do
      Inc(i);

    if i = FOffset + 1 then Exit; { exit with false }

    TagName := Copy(Description, FOffset + 1, i - FOffset - 1);
    Tag := FTags.FindByName(TagName);
    OffsetEnd := i;

    if Tag = nil then
    begin
      DoMessageNonPre(1, pmtWarning, 'Unknown tag name "%s"', [TagName]);
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
      while not ((i > Length(Description)) or (BracketCount = 0)) do
      begin
        case Description[i] of
          '@':
            { Inc(I) here means that we will skip to next character
              when we will see @@, @( or @).
              This means that @( and @) will correctly *not* change
              BracketCount. And @@ will be properly avoided,
              so that e.g. "@@(" will correctly increase BracketCount
              (because "@@(" means one "at" character and then normal
              opening paren). }
            Inc(I);
          '(': Inc(BracketCount);
          ')': Dec(BracketCount);
        end;
        Inc(i);
      end;
      if (BracketCount = 0) then begin
        Parameters := Copy(Description, OffsetEnd + 1, i - OffsetEnd - 2);
        OffsetEnd := i;
      end else
        DoMessageNonPre(1, pmtWarning,
          'No matching closing parenthesis for tag "%s"', [TagName]);
    end else
    if toParameterRequired in Tag.TagOptions then
    begin
      { Read Parameters to the end of Description or newline. }
      while (i <= Length(Description)) and
            (not IsCharInSet(Description[i], [#10, #13])) do
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
    while IsCharInSet(Description[i], HalfLinkChars) do Dec(i);
    Inc(i);
    OffsetEnd := i;
    
    URL := Copy(Description, FOffset, OffsetEnd - FOffset);
  end;

  { Checks does Description[FOffset] may be a beginning of some 
    qualified identifier (identifier is [A-Za-z_]([A-Za-z_0-9])*,
    qualified identifier is a sequence of identifiers delimited
    by dots).

    If yes, returns true and sets OffsetEnd to the next
    index in Description after this qualified ident.
    
    For your comfort, returns also QualifiedIdentifier 
    (this is *always* equal to SplitNameParts(
    Copy(Description, FOffset, OffsetEnd - FOffset))). }
  function FindQualifiedIdentifier(out OffsetEnd: Integer; 
    out QualifiedIdentifier: TNameParts): boolean;
  const
    FirstIdentChar = ['a'..'z', 'A'..'Z', '_'];
    NonFirstIdentChar = FirstIdentChar + ['0'..'9'];
    AnyQualifiedIdentChar = NonFirstIdentChar + ['.'];
  var
    NamePartBegin: Integer;
  begin
    Result := 
      ( (FOffset = 1) or
        not IsCharInSet(Description[FOffset - 1], AnyQualifiedIdentChar) ) and
      SCharIs(Description, FOffset, FirstIdentChar);
    
    if Result then
    begin
      NamePartBegin := FOffset;
      OffsetEnd := FOffset + 1;
      SetLength(QualifiedIdentifier, 0);

      repeat
        { skip a sequence of NonFirstIdentChar characters }
        while SCharIs(Description, OffsetEnd, NonFirstIdentChar) do 
          Inc(OffsetEnd);
          
        if Length(QualifiedIdentifier) = MaxNameParts then
        begin
          { I can't add new item to QualifiedIdentifier.
            So Result is false. }
          Result := false;
          Exit;
        end;
        
        { Append next part to QualifiedIdentifier }
        SetLength(QualifiedIdentifier, Length(QualifiedIdentifier) + 1);
        QualifiedIdentifier[Length(QualifiedIdentifier) - 1] :=
          Copy(Description, NamePartBegin, OffsetEnd - NamePartBegin);
          
        if SCharIs(Description, OffsetEnd, '.') and
           SCharIs(Description, OffsetEnd + 1, FirstIdentChar) then
        begin
          NamePartBegin := OffsetEnd + 1;
          { skip the dot and skip FirstIdentChar character }
          Inc(OffsetEnd, 2); 
        end else
          break;
      until false;
    end;
  end;
  
  function FindFirstSentenceEnd: boolean;
  begin
    Result := (Description[FOffset] = '.') and 
      SCharIs(Description, FOffset + 1, WhiteSpace);
  end;
  
  function IsNormalTextAllowed: boolean;
  begin
    Result := (EnclosingTag = nil) or
      (toAllowNormalTextInside in EnclosingTag.TagOptions);
  end;
  
  function CheckNormalTextAllowed(const NormalText: string): boolean;
  begin
    Result := IsNormalTextAllowed;
    if not Result then
      DoMessageNonPre(1, pmtWarning,
        'Such content, "%s", is not allowed '+
        'directly within the tag @%s', [NormalText, EnclosingTag.Name]);
  end;

  { Strip initial @ from @( and @). Do not touch other @ occurences.
    This is only used for tags without toRecursiveTags
    (for toRecursiveTags, the recursive call to CoreExecute
    will already handle it). }
  function HandleAtChar(const S: string): string;
  var
    PosAt, HandledCount: Integer;
  begin
    Result := '';
    HandledCount := 0;
    while HandledCount < Length(S) do
    begin
      PosAt := PosEx('@', S, HandledCount + 1);
      if PosAt = 0 then
      begin
        Result := Result + SEnding(S, HandledCount + 1);
        HandledCount := Length(S);
      end else
      if SCharIs(S, PosAt + 1, ['(', ')', '@']) then
      begin
        { strip @, add the next ( or ) or @ }
        Result := Result + Copy(S, HandledCount + 1, PosAt - HandledCount - 1) +
          S[PosAt + 1];
        HandledCount := PosAt + 1;
      end else
      begin
        { do not strip @ }
        Result := Result + Copy(S, HandledCount + 1, PosAt - HandledCount);
        HandledCount := PosAt;
      end;
    end;
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
      if (not PreExecute) and
         CheckNormalTextAllowed(ToAppend) then
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
  FoundTagData: TObject;
  QualifiedIdentifier: TNameParts;
  QualifiedIdentifierReplacement: string;
begin
  Result := '';
  FOffset := 1;
  ConvertBeginOffset := 1;
  
  if (EnclosingTag <> nil) and
     (toFirstWordVerbatim in EnclosingTag.TagOptions) then
  begin
    { Skip the first word in Description }
    while SCharIs(Description, FOffset, WhiteSpace) do Inc(FOffset);
    while SCharIs(Description, FOffset, AllChars - WhiteSpace) do Inc(FOffset);
  end;
  
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
      if not FoundTag.AllowedInside(EnclosingTag) then
      begin
        if EnclosingTag = nil then
          DoMessageNonPre(1, pmtWarning, 'The tag "@%s" cannot be used at the ' +
            'top level of description, it must be used within some other @-tag', 
            [FoundTag.Name]) else
          DoMessageNonPre(1, pmtWarning, 'The tag "@%s" cannot be used inside ' +
            'parameter of tag "@%s"', [FoundTag.Name, EnclosingTag.Name]);
            
        { Assign dummy value for ReplaceStr.
        
          We can't proceed with normal recursive expanding and
          calling FoundTag.[Pre]Execute, because tag methods
          (and callbacks, like TTag.On[Pre]Execute) may assume that the tag
          is always enclosed only within allowed tags
          (so e.g. EnclosingTag and EnclosingTagData values for
          On[Pre]Execute are of appropriate classes etc.) }
        ReplaceStr := '';
      end else
      begin
        FoundTagData := FoundTag.CreateOccurenceData;
        try
          { Process Params }
          if Params <> '' then
          begin
            if toParameterRequired in FoundTag.TagOptions then
            begin
              Unabbreviate(Params);
              if toRecursiveTags in FoundTag.TagOptions then
                { recursively expand Params }
                Params := CoreExecute(Params, AutoLink, FoundTag, FoundTagData) else
              if not (toRecursiveTagsManually in FoundTag.TagOptions) then
                Params := HandleAtChar(Params);
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
              DoMessageNonPre(1, pmtWarning,
                'Tag "%s" is not allowed to have any parameters', [FoundTag.Name]);
            end;
            ReplaceStr := DoConvertString('@(' + FoundTag.Name) + Params + ConvertString(')');
          end else
            ReplaceStr := DoConvertString('@' + FoundTag.Name);

          { execute tag handler }
          if PreExecute then
            FoundTag.PreExecute(FoundTagData, EnclosingTag, EnclosingTagData,
              Params, ReplaceStr) else
            FoundTag.Execute(FoundTagData, EnclosingTag, EnclosingTagData,
              Params, ReplaceStr);

        finally 
          FoundTag.DestroyOccurenceData(FoundTagData) 
        end;
      end;

      Result := Result + ReplaceStr;
      
      FOffset := OffsetEnd;
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@(' then
    begin
      DoConvert;
      
      { convert '@(' to '(' }
      if CheckNormalTextAllowed('@(') then
        Result := Result + '(';
        
      FOffset := FOffset + 2;
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@)' then
    begin
      DoConvert;
      
      { convert '@)' to '(' }
      if CheckNormalTextAllowed('@)') then
        Result := Result + ')';
        
      FOffset := FOffset + 2;
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@@' then
    begin
      DoConvert;
      
      { convert '@@' to '@' }
      if CheckNormalTextAllowed('@@') then
        Result := Result + '@';
        
      FOffset := FOffset + 2;
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '@-' then
    begin
      DoConvert;
      
      { convert '@-' to ShortDash }
      if CheckNormalTextAllowed('@-') then
        Result := Result + ShortDash;
      
      FOffset := FOffset + 2;
      ConvertBeginOffset := FOffset;
    end else
    { Note that we must scan for '---' in Description before scanning for '--'. }
    if Copy(Description, FOffset, 3) = '---' then
    begin
      DoConvert;
      
      { convert '---' to EmDash }
      if CheckNormalTextAllowed('---') then
        Result := Result + EmDash;
      
      FOffset := FOffset + 3;      
      ConvertBeginOffset := FOffset;
    end else
    if Copy(Description, FOffset, 2) = '--' then
    begin
      DoConvert;
      
      { convert '--' to EnDash }
      if CheckNormalTextAllowed('--') then
        Result := Result + EnDash;
      
      FOffset := FOffset + 2;      
      ConvertBeginOffset := FOffset;
    end else
    if Description[FOffset] = '-' then
    begin
      DoConvert;
      
      { So '-' is just a normal ShortDash }
      if CheckNormalTextAllowed('-') then
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
    if (not PreExecute) and 
       AutoLink and
       FindQualifiedIdentifier(OffsetEnd, QualifiedIdentifier) and
       TryAutoLink(QualifiedIdentifier, QualifiedIdentifierReplacement) then
    begin
      DoConvert;

      if CheckNormalTextAllowed(GlueNameParts(QualifiedIdentifier)) then
        Result := Result + QualifiedIdentifierReplacement;
        
      FOffset := OffsetEnd;
      ConvertBeginOffset := FOffset;
    end else
    if FindURL(OffsetEnd, URL) then
    begin
      DoConvert;

      if CheckNormalTextAllowed(URL) then
        Result := Result + DoURLLink(URL);
        
      FOffset := OffsetEnd;
      ConvertBeginOffset := FOffset;
    end else
    if WantFirstSentenceEnd and
       (FirstSentenceEnd = 0) and
       FindFirstSentenceEnd then
    begin
      DoConvert;
      
      if CheckNormalTextAllowed('.') then
      begin
        Result := Result + ConvertString('.');
        FirstSentenceEnd := Length(Result);
      end;
      
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
  AutoLink: boolean;
  EnclosingTag: TTag; var EnclosingTagData: TObject): string;
var Dummy: Integer;
begin
  Result := CoreExecute(Description, AutoLink,
    EnclosingTag, EnclosingTagData,
    false, Dummy);
end;

function TTagManager.Execute(const Description: string;
  AutoLink: boolean;
  WantFirstSentenceEnd: boolean;
  out FirstSentenceEnd: Integer): string;
var 
  EnclosingTagData: TObject;
begin
  EnclosingTagData := nil;
  Result := CoreExecute(Description, AutoLink,
    nil, EnclosingTagData,
    WantFirstSentenceEnd, FirstSentenceEnd);
  { Just ignore resulting EnclosingTagData }
end;

function TTagManager.Execute(const Description: string;
  AutoLink: boolean): string; 
var 
  Dummy: Integer;
begin
  Result := Execute(Description, AutoLink, false, Dummy);
end;

end.
