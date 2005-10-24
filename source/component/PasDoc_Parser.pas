{ @abstract(provides all the parsing functionality of pasdoc)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @cvs($Date$)

  Parsing implements most of the functionality of the pasdoc program.

  It provides the @link(TParser) object, which scans the command line parameters
  for file names and switches and then starts collecting information from those
  files, issueing warnings to standard out if necessary. }

unit PasDoc_Parser;

{$I DEFINES.INC}

interface

uses
  Classes,
  PasDoc_Types,
  PasDoc_Items,
  PasDoc_Scanner,
  PasDoc_Tokenizer,
  StringVector;

type
  { Parser class that will process a complete unit file and all of its
    include files, regarding directives.
    When creating this object constructor @link(Create) takes as an argument 
    an input stream and a list of directives.
    Parsing work is done by calling @link(ParseUnit) method.
    If no errors appear, should return a @link(TPasUnit) object with
    all information on the unit. Else exception is raised.
    
    Things that parser inits in items it returns:
    
    @unorderedList(
      @item(Of every TPasItem :
        Name, RawDescription, Visibility, IsDeprecated, IsPlatformSpecific, 
        IsLibrarySpecific, FullDeclararation (note: for now not all items
        get sensible FullDeclararation, but the intention is to improve this
        over time; see @link(TPasItem.FullDeclaration) to know where 
        FullDeclararation is available now).

        Note to IsDeprecated: parser inits it basing on hint directive
        "deprecated" presence in source file; it doesn't handle the fact 
        that @@deprecated tag may be specified inside RawDescription.

        Note to RawDescription: parser inits them from user's comments
        that preceded given item in source file.
        It doesn't handle the fact that @@member and @@value tags
        may also assign RawDescription for some item.)
      
      @item Of TPasCio: Ancestors, Fields, Methods, Properties, MyType.
      
      @item Of TPasEnum: Members.
      
      @item Of TPasMethod: What.
      
      @item Of TPasVarConst: FullDeclaration.
      
      @item(Of TPasProperty: IndexDecl, FullDeclaration.
        PropType (only if was specified in property declaration).
        It was intended that parser will also set Default,
        NoDefault, StoredId, DefaultId, Reader, Writer attributes, 
        but it's still not implemented.)
      
      @item(Of TPasUnit; UsesUnits, Types, Variables, CIOs, Constants,
        FuncsProcs.)
    )
    
    It doesn't init other values. 
    E.g. AbstractDescription or DetailedDescription of TPasItem 
    should be inited while expanding this item's tags.
    E.g. SourceFileDateTime and SourceFileName of TPasUnit must
    be set by other means. }
  TParser = class
  private
    FImplicitVisibility: TImplicitVisibility;

    { Last comment found in input.
      This only takes into account normal comments, i.e. not back-comments.
      Modified by @link(GetLastComment) and @link(PeekNextToken)
      (and consequently by all @link(PeekNextToken) and @link(GetNextToken)
      versions). 
      
      LastCommentContent is only the comment content, with comment braces
      and markers already stripped. }
    IsLastComment: boolean;
    LastCommentWasCStyle: boolean;
    LastCommentContent: string;
    
    { The underlying scanner object. }
    Scanner: TScanner;
    
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FCommentMarkers: TStringList;
    FMarkersOptional: boolean;
    FShowVisibilities: TVisibilities;
    
    { These are the items that the next "back-comment"
      (the comment starting with "<", see
      [http://pasdoc.sipsolutions.net/WhereToPlaceComments]
      section "Placing comments after the item") will apply to. }
    ItemsForNextBackComment: TPasItems;

    procedure DoError(const AMessage: string; 
      const AArguments: array of const);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    { Checks if T.MyType is ATokenType, if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ATokenType: TTokenType); overload;
    
    { Checks if T.IsSymbol(ASymbolType), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ASymbolType: TSymbolType); overload;
    
    { Checks if T.IsKeyWord(AKeyWord), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; AKeyWord: TKeyWord); overload;

    { If not IsLastComment, then returns empty string,
      otherwise returns LastCommentContent and sets IsLastComment to false. }
    function GetLastComment: String;

    { Reads tokens and throws them away as long as they are either whitespace
      or comments.
      
      Sets WhitespaceCollector to all the whitespace that was skipped.
      (Does @italic(not) append them to WhitespaceCollector,
      it @italic(sets) WhitespaceCollector to them, deleting previous
      WhitespaceCollector value.)
      
      Comments are collected to [Is]LastCommentXxx properties, so that you can
      use GetLastComment.

      Returns non-white token that was found.
      This token is equal to @code(Scanner.PeekToken).
      Note that this token was @italic(peeked)
      from the stream, i.e. the caller is still responsible for doing 
      @code(Scanner.ConsumeToken).
      Calling this method twice in a row will return the same thing.
      
      Always returns something non-nil (will raise exception in case
      of problems, e.g. when stream ended). }
    function PeekNextToken(out WhitespaceCollector: string): TToken; overload;
    
    { Same thing as PeekNextToken(Dummy) }
    function PeekNextToken: TToken; overload;
        
    { Just like @link(PeekNextToken), but returned token is already consumed.
      Next call to @name will return next token. }
    function GetNextToken(out WhitespaceCollector: string): TToken; overload;
    
    { Just like @link(PeekNextToken), but returned token is already consumed.
      Moreover, whitespace collected is appended to 
      WhitespaceCollectorItem.FullDeclaration
      (does not delete previous WhitespaceCollectorItem.FullDeclaration value, 
      it only appends to it). }
    function GetNextToken(WhitespaceCollectorItem: TPasItem): TToken; overload;
    
    function GetNextToken: TToken; overload;
    
    { This does @link(GetNextToken), then checks is it a ATokenType
      (using @link(CheckToken)), then frees the token.
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(ATokenType: TTokenType): string; overload;
    
    { This does @link(GetNextToken), then checks is it a symbol with 
      ASymbolType (using @link(CheckToken)), then frees the token. 
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(ASymbolType: TSymbolType): string; overload;
    
    { This does @link(GetNextToken), then checks is it a keyword with 
      AKeyWord (using @link(CheckToken)), then frees the token. 
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(AKeyWord: TKeyWord): string; overload;
    
    { Parses a constructor, a destructor, a function or a procedure.
      Resulting @link(TPasMethod) item will be returned in M.
      
      ClassKeywordString contains the keyword 'class'
      in the exact spelling as it was found in input,
      for class methods. Else it contains ''.
      
      KeyWordString contains the keyword constructor, destructor, 
      function or procedure in the exact spelling as it was found in input.
      You can specify KeyWordString = '', this way you avoid including
      such keyword at the beginning of returned M.FullDeclaration.
      
      Key contains one of the KEY_xxx constants for the What field of the
      resulting method object. Yes, this is the keyword that should correspond 
      to KeyWordString.
      
      D may contain a description or nil. }
    procedure ParseCDFP(out M: TPasMethod; 
      const ClassKeywordString: string;
      const KeyWordString: string; Key: TKeyWord;
      d: string; const NeedName: boolean);
      
    { Parses a class, an interface or an object.
      U is the unit this item will be added to on success.
      N is the name of this item.
      CIOType describes if item is class, interface or object.
      D may contain a description or nil. }
    procedure ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
      TCIOType; d: string; const IsInRecordCase: boolean);
    
    procedure ParseRecordCase(const R: TPasCio; const SubCase: boolean);
    procedure ParseConstant(const U: TPasUnit);
    procedure ParseInterfaceSection(const U: TPasUnit);
    procedure ParseProperty(out p: TPasProperty);
    procedure ParseType(const U: TPasUnit);
    
    { This assumes that you just read left parenthesis starting
      an enumerated type. It finishes parsing of TPasEnum,
      returning is as P. }
    procedure ParseEnum(out p: TPasEnum; const Name, RawDescription: string);

    procedure ParseUses(const U: TPasUnit);
    
    { This parses the sequence of identifiers separated by commas
      and ended by symbol FinalSymbol. More specifically in EBNF it parses
        TOK_IDENTIFIER (SYM_COMMA TOK_IDENTIFIER)+ FinalSymbol
      FinalSymbol must be something else than SYM_COMMA.
      After executing this, next token (returned by GetNextToken and PeekNextToken)
      will point to the next token right after FinalSymbol.
      All found identifiers will be appended to Names. 
      
      If RawDescriptions <> nil then this will also get
      all comments documenting the identifiers in Names
      (it will append the same number of items to 
      RawDescriptions as it appended to Names).
      The strategy how comments are assigned to item in this case is
      described on [http://pasdoc.sipsolutions.net/WhereToPlaceComments]
      (see section "Multiple fields/variables in one declaration"). }
    procedure ParseCommaSeparatedIdentifiers(Names: TStrings;
      FinalSymbol: TSymbolType; RawDescriptions: TStringList);
    
    procedure ParseVariables(const U: TPasUnit);
    
    { Parse one variables or fields clause 
      ("one clause" is something like 
        NAME1, NAME2, ... : TYPE;
      i.e. a list of variables/fields sharing one type declaration.)
      
      @param(Items If Items <> nil then it adds parsed variables/fields to Items.)
      @param(Visibility will be assigned to Visibility of 
        each variable/field instance.) 
      @param(IsInRecordCase indicates if we're within record's case.
        It's relevant only if OfObject is true.) }
    procedure ParseFieldsVariables(Items: TPasItems; 
      OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean);
    
    { Read all tokens until you find a semicolon at brace-level 0 and
      end-level (between "record" and "end" keywords) also 0.

      Alternatively, also stops before reading "end" without beginning
      "record" (so it can handle some cases where declaration doesn't end
      with semicolon).
      
      Alternatively, only if IsInRecordCase, also stops before reading
      ')' without matching '('. That's because fields' declarations
      inside record case may be terminated by just ')' indicating
      that this case clause terminates, without a semicolon.

      If you pass Item <> nil then all read data will be 
      appended to Item.FullDeclaration. Also Item.IsLibrarySpecific,
      Item.IsPlatformSpecific and Item.IsDeprecated will be set to true
      if appropriate hint directive will occur in source file. }    
    procedure SkipDeclaration(const Item: TPasItem; IsInRecordCase: boolean);
    
    procedure SetCommentMarkers(const Value: TStringList);
    
    { Skips all whitespace and comments and while it sees some hint directive
      (platform, library, deprecated) it consumes it, sets appropriate
      property of Item (IsPlatformSpecific, IsLibrarySpecific or IsDeprecated)
      to true and goes further.
      
      Stops when PeekNextToken returns some non-whitespace non-comment 
      non-hint-directive token. }
    procedure ParseHintDirectives(Item: TPasItem);
  public
    { Create a parser, initialize the scanner with input stream S.
      All strings in SD are defined compiler directives. }
    constructor Create(
      const InputStream: TStream;
      const Directives: TStringVector;
      const IncludeFilePaths: TStringVector;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName: string;
      const AHandleMacros: boolean);
      
    { Release all dynamically allocated memory. }
    destructor Destroy; override;
    
    procedure ParseUnit(var U: TPasUnit);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkersOptional: boolean read fMarkersOptional write fMarkersOptional;
    property ShowVisibilities: TVisibilities 
      read FShowVisibilities write FShowVisibilities;
      
    { See command-line option @--implicit-visibility documentation at
      [http://pasdoc.sipsolutions.net/ImplicitVisibilityOption] }
    property ImplicitVisibility: TImplicitVisibility
      read FImplicitVisibility write FImplicitVisibility;
  end;

implementation

uses
  SysUtils,
  Utils;

{ ---------------------------------------------------------------------------- }
{ TParser }
{ ---------------------------------------------------------------------------- }

constructor TParser.Create(
  const InputStream: TStream;
  const Directives: TStringVector;
  const IncludeFilePaths: TStringVector;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName: string;
  const AHandleMacros: boolean);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  Scanner := TScanner.Create(InputStream, OnMessageEvent, 
    VerbosityLevel, AStreamName, AHandleMacros);
  Scanner.AddSymbols(Directives);
  Scanner.IncludeFilePaths := IncludeFilePaths;
  FCommentMarkers := TStringlist.Create;
  ItemsForNextBackComment := TPasItems.Create(false);
end;

{ ---------------------------------------------------------------------------- }

destructor TParser.Destroy;
begin
  FCommentMarkers.Free;
  Scanner.Free;
  ItemsForNextBackComment.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoError(const AMessage: string; 
  const AArguments: array of const);
begin
  raise EPasDoc.Create(Scanner.GetStreamInfo + ': ' + AMessage, AArguments, 1);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity <= FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

const
  SExpectedButFound = '%s expected but %s found';

procedure TParser.CheckToken(T: TToken; ATokenType: TTokenType);
begin
  if T.MyType <> ATokenType then
    DoError(SExpectedButFound, 
      [TOKEN_TYPE_NAMES[ATokenType], T.Description]);
end;

procedure TParser.CheckToken(T: TToken; ASymbolType: TSymbolType);
begin
  if not T.IsSymbol(ASymbolType) then
    DoError(SExpectedButFound, 
      [Format('symbol "%s"', [SymbolNames[ASymbolType]]), T.Description]);
end;

procedure TParser.CheckToken(T: TToken; AKeyWord: TKeyWord);
begin
  if not T.IsKeyWord(AKeyWord) then
    DoError(SExpectedButFound, 
      [Format('reserved word "%s"', [LowerCase(KeyWordArray[AKeyWord])]), 
      T.Description]);
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetLastComment: string;
begin
  if IsLastComment then
  begin
    Result := LastCommentContent;
    IsLastComment := false;
  end else
    Result := '';
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetNextToken(out WhitespaceCollector: string): TToken;
begin
  Result := PeekNextToken(WhitespaceCollector);
  Scanner.ConsumeToken;
end;

function TParser.GetNextToken(WhitespaceCollectorItem: TPasItem): TToken;
var
  WhitespaceCollector: string;
begin
  Result := GetNextToken(WhitespaceCollector);
  WhitespaceCollectorItem.FullDeclaration := 
    WhitespaceCollectorItem.FullDeclaration + WhitespaceCollector;
end;

function TParser.GetNextToken: TToken;
var
  LDummy: string;
begin
  Result := GetNextToken(LDummy);
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetAndCheckNextToken(ATokenType: TTokenType): string; 
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, ATokenType);
    Result := T.Data;
  finally 
    T.Free;
  end;
end;

function TParser.GetAndCheckNextToken(ASymbolType: TSymbolType): string;
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, ASymbolType);
    Result := T.Data;
  finally 
    T.Free;
  end;
end;

function TParser.GetAndCheckNextToken(AKeyWord: TKeyWord): string;
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, AKeyWord);
    Result := T.Data;
  finally 
    T.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCDFP(out M: TPasMethod; 
  const ClassKeywordString: string;
  const KeyWordString: string; Key: TKeyword; 
  d: string; const NeedName: boolean);
var
  IsSemicolon: Boolean;
  t: TToken;
  level: Integer;
  InvalidType: boolean;
begin
  M := TPasMethod.Create;
  M.RawDescription := d;
  t := nil;
  case Key of
    KEY_CONSTRUCTOR: M.What := METHOD_CONSTRUCTOR;
    KEY_DESTRUCTOR:  M.What := METHOD_DESTRUCTOR;
    KEY_FUNCTION:    M.What := METHOD_FUNCTION;
    KEY_PROCEDURE:   M.What := METHOD_PROCEDURE;
    KEY_OPERATOR:    M.What := METHOD_OPERATOR;
  else
    DoError('Expected keyword starting some method/procedure', []);
  end;
  
  if ClassKeyWordString <> '' then
    M.FullDeclaration :=  ClassKeyWordString + ' ';
  M.FullDeclaration := M.FullDeclaration + KeyWordString;

  { next non-wc token must be the name }
  if NeedName then
  begin
    try
      t := GetNextToken;
    except
      M.Free;
      raise;
    end;
    if (Key = KEY_OPERATOR) then
    begin
      InvalidType := (t.MyType <> TOK_SYMBOL);
    end
    else
    begin
      InvalidType := (t.MyType <> TOK_IDENTIFIER);
    end;

    if InvalidType then begin
      M.Free;
      try
        DoError('Unexpected token %s', [T.Description]);
      finally
        FreeAndNil(t);
      end;
    end;
    M.Name := t.Data;
    DoMessage(5, mtInformation, 'Parsing %s "%s"',
      [LowerCase(KeyWordArray[Key]), M.Name]);
    M.FullDeclaration := M.FullDeclaration + ' ' + M.Name;
    FreeAndNil(t);
  end;

  { copy tokens until first semicolon with parenthesis level zero }
  level := 0;
  repeat
    t := Scanner.GetToken;
    if not (t.MyType in TokenCommentTypes) then
      case t.MyType of
        TOK_WHITESPACE:
          begin
            { add exactly *one space* at the end of M.FullDeclaration }
            if Length(M.FullDeclaration) > 0 then begin
              if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
                M.FullDeclaration := M.FullDeclaration + ' ';
            end;
          end
        else
          M.FullDeclaration := M.FullDeclaration + t.Data;
      end;
    if t.IsSymbol(SYM_LEFT_PARENTHESIS) then Inc(level);
    if t.IsSymbol(SYM_RIGHT_PARENTHESIS) then Dec(level);
    IsSemicolon := t.IsSymbol(SYM_SEMICOLON);
    FreeAndNil(t);
  until IsSemicolon and (Level = 0);
  
  { first get non-WC token - if it is not an identifier in SD_SET put it back
    into stream and leave; otherwise copy tokens until semicolon }
  repeat
    FreeAndNil(t);
    try
      t := GetNextToken;
    except
      M.Free;
      raise;
    end;
    
    if t.MyType = TOK_IDENTIFIER then
    begin
      case t.Info.StandardDirective of
        SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
          SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE, SD_INLINE,
          SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
          SD_VARARGS:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);

            try
              t := GetNextToken;
            except
              M.Free;
              raise
            end;
          end;

        { * External declarations might be followed by a string constant.
          * Messages are followed by an integer constant between 1 and 49151 which
            specifies the message ID. }
        SD_EXTERNAL, SD_MESSAGE, SD_NAME:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);

            // Keep on reading up to the next semicolon or declaration
            repeat
              try
                t := GetNextToken;
              except
                M.Free;
                raise;
              end;

              if t.IsSymbol(SYM_SEMICOLON) then begin
                Break
              end else begin
                if t.MyType = TOK_IDENTIFIER then 
                  case t.Info.StandardDirective of
                    SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT, SD_EXTERNAL,
                      SD_FAR, SD_FORWARD, SD_NAME, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
                      SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL:
                      begin
                        // FScanner.UnGetToken(t);
                        Break;
                      end;
                  end;

                M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
                FreeAndNil(t);
              end;
            until False;
          end;
        SD_DEPRECATED: begin
          M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
          M.IsDeprecated := True;
          FreeAndNil(t);
          t := GetNextToken;
        end;
        SD_PLATFORM: begin
          M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
          M.IsPlatformSpecific := True;
          FreeAndNil(t);
          t := GetNextToken;
        end;
      else
        begin
          Scanner.UnGetToken(t);
          Break;
        end;
      end; // case
    end else
    if t.MyType = TOK_KEYWORD then 
    begin
      case t.Info.KeyWord of 
        KEY_INLINE: begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(t);
            t := GetNextToken;
          end;
        KEY_LIBRARY:
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            M.IsLibrarySpecific := True;
            FreeAndNil(t);
            t := GetNextToken;
          end;
        else 
          begin
            Scanner.UnGetToken(t);
            Break;        
          end;
      end;
    end else
    begin
      Scanner.UnGetToken(t);
      Break;
    end;

    { Apparently, the Delphi compiler does NOT enforce that
      directives must be separated and be terminated by a semicolon,
      even though Delphi help consistently uses them consistently.
      However, we take the compiler as a reference and try to mimic its behaviour. }
    { Is current token a semicolon? }
    if t.IsSymbol(SYM_SEMICOLON) then
      M.FullDeclaration := M.FullDeclaration + ';'
    else begin
      M.FullDeclaration := M.FullDeclaration + ' ';
      Scanner.UnGetToken(t);
    end;

  until False;
  
  ItemsForNextBackComment.ClearAndAdd(M);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
  TCIOType; d: string; const IsInRecordCase: boolean);

  { Parse fields clause, i.e. something like
      NAME1, NAME2, ... : TYPE;
    If AddToFields then adds parsed fields to i.Fields.
    Visibility of created fields is set to given Visibility parameter. }
  procedure ParseFields(i: TPasCio; AddToFields: boolean; 
    Visibility: TVisibility);
  var Items: TPasItems;
  begin
    if AddToFields then
      Items := i.Fields else
      Items := nil;
      
    { Note: 4th arg for ParseFieldsVariables is always "false",
      not "IsInRecordCase". That's because even if declaration
      of this CIO is within a record case, then we want to
      see record's terminating "end" keyword anyway.
      So it doesn't matter here whether our IsInRecordCase 
      parameter is true. }
    ParseFieldsVariables(Items, true, Visibility, false);
  end;

var
  Finished: Boolean;
  i: TPasCio;
  M: TPasMethod;
  p: TPasProperty;
  s: string;
  Visibility: TVisibility;
  t: TToken;
  ClassKeyWordString: string;
begin
  t := nil;
  DoMessage(5, mtInformation, 'Parsing class/interface/object "%s"', [CioName]);
  i := nil;
  try
    t := GetNextToken;

    { Test for forward class definition here:
        class MyClass = class;
      with no ancestor or class members listed after the word class. }
    if t.IsSymbol(SYM_SEMICOLON) then begin
      // No error, continue the parsing.
      t.Free;
      Exit;
    end;

    i := TPasCio.Create;
    try
      i.Name := CioName;
      i.RawDescription := d;
      i.MyType := CIOType;

      { get all ancestors; remember, this could look like
        TNewClass = class ( Classes.TClass, MyClasses.TFunkyClass, MoreClasses.YAC) ... end;
        All class ancestors are supposed to be included in the docs!
      }
      { TODO -otwm :
        That's not quite true since multiple inheritance is not supported by 
         Delphi/Kylix or FPC. Every entry but the first must be an interface. }
      if t.IsSymbol(SYM_LEFT_PARENTHESIS) then begin
          { optional ancestor introduced by ( }
        FreeAndNil(t);
        Finished := False;
        { outer repeat loop: one ancestor per pass }
        repeat
          FreeAndNil(t);
          t := GetNextToken;

          if t.MyType = TOK_IDENTIFIER then begin { an ancestor }
            s := t.Data;
                { inner repeat loop: one part of the ancestor per name }
            repeat
              FreeAndNil(t);
              t := Scanner.GetToken;
              if not t.IsSymbol(SYM_PERIOD) then begin
                Scanner.UnGetToken(t);
                t := nil;
                Break; { leave inner repeat loop }
              end;
              FreeAndNil(t);
              s := s + '.';
              t := Scanner.GetToken;
              if t.MyType <> TOK_IDENTIFIER then
                DoError('Expected class, object or interface in ancestor' +
                  ' declaration', []);

              s := s + t.Data;
            until False;
            i.Ancestors.Add(s);
          end else begin
            if (t.IsSymbol(SYM_COMMA)) then
                { comma, separating two ancestors } begin
              FreeAndNil(t);
            end else 
            begin
              try
                CheckToken(T, SYM_RIGHT_PARENTHESIS);
                Finished := true;
              finally
                FreeAndNil(t);
              end;
            end;
          end;
        until Finished;
      end else begin
        Scanner.UnGetToken(t);
        case i.MyType of
          CIO_CLASS: begin
            if not SameText(i.Name, 'tobject') then begin
              i.Ancestors.Add('TObject');
            end;
          end;
          CIO_SPINTERFACE: begin
            if not SameText(i.Name, 'idispinterface') then begin
              i.Ancestors.Add('IDispInterface');
            end;
          end;
          CIO_INTERFACE: begin
            if not SameText(i.Name, 'iinterface') then begin
              i.Ancestors.Add('IInterface');
            end;
          end;
          CIO_OBJECT: begin
            if not SameText(i.Name, 'tobject') then begin
              i.Ancestors.Add('TObject');
            end;
          end;
        end;
      end;
      t := GetNextToken;

      if (t.IsSymbol(SYM_LEFT_BRACKET)) then begin
        FreeAndNil(t);

        { for the time being, we throw away the ID itself }
        t := GetNextToken;
        if (t.MyType <> TOK_STRING) and (t.MyType <> TOK_IDENTIFIER) then
          DoError('Literal String or identifier as interface ID expected', []);
        FreeAndNil(t);

        t := GetNextToken;
        CheckToken(T, SYM_RIGHT_BRACKET);
      end else begin
        Scanner.UnGetToken(t);
      end;

      { Visibility of members at the beginning of a class declaration
        that don't have a specified visibility is controlled
        by ImplicitVisibility value. }
      case ImplicitVisibility of
        ivPublic:
          if Scanner.SwitchOptions['M'] then 
            Visibility := viPublished else 
            Visibility := viPublic;
        ivPublished:
          Visibility := viPublished;
        ivImplicit:
          Visibility := viImplicit;
        else raise EInternalError.Create('ImplicitVisibility = ??');
      end;

      { now collect methods, fields and properties }

      { This is needed to include ClassKeyWordString in 
        class methods declarations. }
      ClassKeyWordString := '';

      Finished := False;
      repeat
        FreeAndNil(t);
        try
          t := GetNextToken;
        except
          i.Free;
          raise;
        end;
        if (t.IsSymbol(SYM_SEMICOLON)) then 
        begin
          { A declaration of type "name = class(ancestor);" }
          Scanner.UnGetToken(T);
          Finished := True;
        end
        else
          if (t.MyType = TOK_KEYWORD) then
            case t.Info.KeyWord of
              KEY_CLASS: ClassKeyWordString := t.Data;
              KEY_CONSTRUCTOR,
              KEY_DESTRUCTOR,
              KEY_FUNCTION,
              KEY_PROCEDURE: 
                begin
                  d := GetLastComment;
                  try
                    ParseCDFP(M, ClassKeyWordString, 
                      t.Data, t.Info.KeyWord, d, True);
                  except
                    i.Free;
                    FreeAndNil(t);
                    raise;
                  end;
                  ClassKeyWordString := '';
                  M.Visibility := Visibility;
                  if Visibility in ShowVisibilities then begin
                    i.Methods.Add(M);
                  end
                  else
                  begin
                    M.Free;
                  end;
                end;
              KEY_END: Finished := True;
              KEY_PROPERTY: begin
                  ParseProperty(p);
                  p.Visibility := Visibility;
                  if Visibility in ShowVisibilities then begin
                    i.Properties.Add(p);
                  end
                  else
                  begin
                    FreeAndNil(p);
                  end;
                end;
              KEY_CASE: begin
                  try
                    ParseRecordCase(i, false);
                  except
                    FreeAndNil(t);
                    i.Free;
                    raise;
                  end;
                end;
            else begin
                i.Free;
                try
                  DoError('Unexpected %s', [T.Description]);
                finally
                  FreeAndNil(t);
                end;
              end;
            end
          else
            if (t.MyType = TOK_IDENTIFIER) then
            begin
              case t.Info.StandardDirective of
                SD_DEFAULT: 
                  begin
                    { Note: 2nd arg for SkipDeclaration is always "false",
                      not "IsInRecordCase". That's because even if declaration
                      of this CIO is within a record case, then we want to
                      see record's terminating "end" keyword anyway.
                      So it doesn't matter here whether our IsInRecordCase
                      parameter is true. }
                    SkipDeclaration(nil, false);
                    DoMessage(5, mtInformation, 'Skipped default property keyword.', []);
                  end;
                SD_PUBLIC:    Visibility := viPublic;
                SD_PUBLISHED: Visibility := viPublished;
                SD_PRIVATE:   Visibility := viPrivate;
                SD_PROTECTED: Visibility := viProtected;
                SD_AUTOMATED: Visibility := viAutomated;
                else
                  begin
                    Scanner.UnGetToken(T);
                    ParseFields(i, Visibility in ShowVisibilities, Visibility);
                  end;
              end;
            end;
        FreeAndNil(t);
      until Finished;

      ParseHintDirectives(i);

      t := GetNextToken;
      try
        if not t.IsSymbol(SYM_SEMICOLON) then
        begin
          if IsInRecordCase then
          begin
            if t.IsSymbol(SYM_RIGHT_PARENTHESIS) then 
              Scanner.UnGetToken(t) else 
            begin
              i.Free;
              DoError('Unexpected symbol at end of sub-record', []);
            end;
          end else 
          begin
            i.Free;
            DoError('Semicolon at the end of Class / Object / Interface' +
              ' / Record expected', []);
          end;
        end;
      finally
        FreeAndNil(t);
      end;

      if Assigned(U) then 
      begin
        U.AddCIO(I);
        ItemsForNextBackComment.ClearAndAdd(I);
      end;
    
    finally
      if not Assigned(U) then FreeAndNil(I);
    end;
  except
    t.Free;
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseConstant(const U: TPasUnit);
var
  i: TPasConstant;
begin
  i := TPasConstant.Create;
  i.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
  DoMessage(5, mtInformation, 'Parsing constant %s', [i.Name]);
  i.RawDescription := GetLastComment;
  i.FullDeclaration := i.Name;
  SkipDeclaration(i, false);
  U.AddConstant(i);
  ItemsForNextBackComment.ClearAndAdd(I);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseEnum(out p: TPasEnum;
  const Name, RawDescription: string);
var
  t: TToken;
  item: TPasItem;
begin
  t := nil;
  p := TPasEnum.Create;
  p.Name := Name;
  p.RawDescription := RawDescription;
  p.FullDeclaration := Name + ' = (...);'; 
  ItemsForNextBackComment.ClearAndAdd(P);

  t := GetNextToken;
  while not t.IsSymbol(SYM_RIGHT_PARENTHESIS) do begin
    if t.MyType = TOK_IDENTIFIER then begin
      item := TPasItem.Create;
      item.Name := t.Data;
      item.RawDescription := GetLastComment;
      p.Members.Add(item);
      ItemsForNextBackComment.ClearAndAdd(Item);
    end;
    if t.IsSymbol(SYM_EQUAL) then begin
      FreeAndNil(t);
      t := GetNextToken;
    end;
    FreeAndNil(t);
    t := GetNextToken;
  end;
  FreeAndNil(t);
  
  GetAndCheckNextToken(SYM_SEMICOLON);
end;

procedure TParser.ParseInterfaceSection(const U: TPasUnit);
const
  MODE_UNDEFINED = 0;
  MODE_CONST = 1;
  MODE_TYPE = 2;
  MODE_VAR = 3;
var
  d: string;
  Finished: Boolean;
  Mode: Integer;
  M: TPasMethod;
  t: TToken;
begin
  DoMessage(4, mtInformation, 'Entering interface section of unit %s',[U.Name]);
  Finished := False;
  Mode := MODE_UNDEFINED;

  repeat
    t := GetNextToken;

    try
      case t.MyType of
        TOK_IDENTIFIER: 
          begin
            case Mode of
              MODE_CONST:
                begin
                  Scanner.UnGetToken(T);
                  ParseConstant(U);
                end;
              MODE_TYPE:
                begin
                  Scanner.UnGetToken(T);
                  ParseType(U);
                end;
              MODE_VAR:
                begin
                  Scanner.UnGetToken(T);
                  ParseVariables(U);
                end;
            else
              DoError('Unexpected %s', [T.Description]);
            end;
          end;
        TOK_KEYWORD: begin
            case t.Info.KeyWord of
              KEY_RESOURCESTRING,
                KEY_CONST:
                Mode := MODE_CONST;
              KEY_OPERATOR: begin
                  d := GetLastComment;
                  ParseCDFP(M, '', t.Data, t.Info.KeyWord, d, True);
                  u.FuncsProcs.Add(M);
                  Mode := MODE_UNDEFINED;
                end;
              KEY_FUNCTION,
                KEY_PROCEDURE: begin
                  d := GetLastComment;
                  ParseCDFP(M, '', t.Data, t.Info.KeyWord, d, True);
                  u.FuncsProcs.Add(M);
                  Mode := MODE_UNDEFINED;
                end;
              KEY_IMPLEMENTATION:
                Finished := True;
              KEY_TYPE:
                Mode := MODE_TYPE;
              KEY_USES:
                ParseUses(U);
              KEY_THREADVAR,
                KEY_VAR:
                Mode := MODE_VAR;
            else
              DoError('Unexpected %s', [T.Description]);
            end;
          end;
      end;
    finally
      FreeAndNil(t);
    end;
  until Finished;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseProperty(out p: TPasProperty);
var
  Finished: Boolean;
  t: TToken;
begin
  p := TPasProperty.Create;
  p.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
  DoMessage(5, mtInformation, 'Parsing property %s', [p.Name]);
  p.IndexDecl := '';
  p.Proptype := '';
  p.FullDeclaration := 'property ' + p.Name;
  p.RawDescription := GetLastComment;
  ItemsForNextBackComment.ClearAndAdd(P);
  
  { Is this only a redeclaration of property from ancestor
    (to e.g. change it's visibility) }
  t := GetNextToken(P);
  if t.IsSymbol(SYM_SEMICOLON) then
  begin
    p.FullDeclaration := p.FullDeclaration + ';';
    FreeAndNil(t);
    Exit;
  end;

  { get index }
  if t.IsSymbol(SYM_LEFT_BRACKET) then
  begin
    FreeAndNil(t);
    p.IndexDecl := '[';
    p.FullDeclaration := p.FullDeclaration + '[';
    repeat
      t := Scanner.GetToken;

      if not (t.MyType in TokenCommentTypes + [TOK_DIRECTIVE]) then
      begin
        p.IndexDecl := p.IndexDecl + t.Data;
        p.FullDeclaration := p.FullDeclaration + t.Data;
      end;
      Finished := t.IsSymbol(SYM_RIGHT_BRACKET);
      FreeAndNil(t);
    until Finished;

    t := GetNextToken;
  end;

  { now if there is a colon, it is followed by the type }
  if t.IsSymbol(SYM_COLON) then 
  begin
    FreeAndNil(t);
    
    { get property type }
    t := GetNextToken;
    if (t.MyType <> TOK_IDENTIFIER) and (t.MyType <> TOK_KEYWORD) then
      DoError('Identifier or keyword expected but %s found', [T.Description]);

    p.Proptype := t.Data;
    FreeAndNil(t);
    p.FullDeclaration := p.FullDeclaration + ': ' + p.Proptype;
  end else
  begin
    p.FullDeclaration := p.FullDeclaration + t.Data;
    FreeAndNil(t);
  end;
  
  { read the rest of declaration }
  SkipDeclaration(P, false);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseRecordCase(const R: TPasCio;
  const SubCase: boolean);
var
  t1: TToken;
  LNeedId: boolean;
  P: TPasItem;
begin
  t1 := GetNextToken;
  try
    CheckToken(T1, TOK_IDENTIFIER);
    
    if PeekNextToken.IsSymbol(SYM_COLON) then
    begin
      { Then we have "case FieldName: FieldType of" }
      
      { consume and free the colon token }
      GetNextToken.Free; 

      P := TPasItem.Create;
      p.Name := T1.Data;
      p.RawDescription := GetLastComment;
      p.FullDeclaration := p.Name + ': ' + GetAndCheckNextToken(TOK_IDENTIFIER);
      ItemsForNextBackComment.ClearAndAdd(P);
      R.Fields.Add(p);
    end;
  finally 
    FreeAndNil(t1);
  end;
  
  GetAndCheckNextToken(KEY_OF);
  
  t1 := GetNextToken;
  LNeedId := True;
  repeat
    while true do begin
      case t1.MyType of
        TOK_SYMBOL: begin
            case t1.Info.SymbolType of
              SYM_COLON: break;
              SYM_COMMA: LNeedId := True;
            end;
          end;
        TOK_IDENTIFIER,
        TOK_NUMBER: 
          if not LNeedId then
          try
            DoError('Unexpected %s', [T1.Description]);
          finally
            FreeAndNil(t1);
          end;
        else begin
          try
            DoError('Unexpected %s', [T1.Description]);
          finally
            FreeAndNil(t1);
          end;
        end;
      end;
      FreeAndNil(t1);
      t1 := GetNextToken;
    end;
    // read all identifiers before colon

    FreeAndNil(t1);
    
    GetAndCheckNextToken(SYM_LEFT_PARENTHESIS);
    
    while not PeekNextToken.IsSymbol(SYM_RIGHT_PARENTHESIS) do 
    begin
      if PeekNextToken.IsKeyWord(KEY_CASE) then
      begin
        GetNextToken.Free; { consume and free "case" token }
        ParseRecordCase(R, true);
      end else
        ParseFieldsVariables(R.Fields, true, viPublic, true);
    end;
    
    GetNextToken.Free; // free ')' token
    
    t1 := GetNextToken;
    if t1.IsSymbol(SYM_SEMICOLON) then
    begin
      FreeAndNil(t1);
      t1 := GetNextToken;
    end;
    
  until t1.IsKeyWord(KEY_END) or
    (SubCase and t1.IsSymbol(SYM_RIGHT_PARENTHESIS));
    
  Scanner.UnGetToken(t1);
end;

procedure TParser.ParseType(const U: TPasUnit);
var
  d: string;
  NormalType: TPasType;
  TypeName: string;
  LCollected, LTemp: string;
  MethodType: TPasMethod;
  EnumType: TPasEnum;
  T: TToken;
begin
  TypeName := GetAndCheckNextToken(TOK_IDENTIFIER);
  DoMessage(5, mtInformation, 'Parsing type "%s"', [TypeName]);
  
  d := GetLastComment;
  t := GetNextToken(LCollected);

  if (not t.IsSymbol(SYM_EQUAL)) then begin
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
      FreeAndNil(t);
      t := nil;
      Exit;
    end;
    FreeAndNil(t);
    DoError('Symbol "=" expected', []);
  end;
  LCollected := TypeName + LCollected + t.Data;
  FreeAndNil(t);

  t := GetNextToken(LTemp);
  LCollected := LCollected + LTemp + t.Data;

  if (t.MyType = TOK_KEYWORD) then
    case t.Info.KeyWord of
      KEY_CLASS: begin
          FreeAndNil(t);
          t := GetNextToken(LTemp);
          LCollected := LCollected + LTemp + t.Data;
          if t.IsKeyWord(KEY_OF) then 
          begin
            { include "identifier = class of something;" as standard type }
          end else begin
            Scanner.UnGetToken(t);
            t := nil;
            ParseCIO(U, TypeName, CIO_CLASS, d, False);
            Exit;
          end;
        end;
      KEY_DISPINTERFACE: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_SPINTERFACE, d, False);
          Exit;
        end;
      KEY_INTERFACE: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_INTERFACE, d, False);
          Exit;
        end;
      KEY_OBJECT: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_OBJECT, d, False);
          Exit;
        end;
      KEY_RECORD: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_RECORD, d, False);
          Exit;
        end;
      KEY_PACKED: begin
          FreeAndNil(t);
          t := GetNextToken(LTemp);
          LCollected := LCollected + LTemp + t.Data;
          if t.IsKeyWord(KEY_RECORD) then 
          begin
            FreeAndNil(t);
            ParseCIO(U, TypeName, CIO_PACKEDRECORD, d, False);
            exit;
          end;
        end;
    end;
  if Assigned(t) then begin
    if (t.MyType = TOK_KEYWORD) then begin
      if t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then 
      begin
        ParseCDFP(MethodType, '', t.Data, t.Info.KeyWord, d, False);
        MethodType.Name := TypeName;
        MethodType.FullDeclaration := 
          TypeName + ' = ' + MethodType.FullDeclaration;
        U.AddType(MethodType);
        FreeAndNil(t);
        exit;
      end;
    end;
    if t.IsSymbol(SYM_LEFT_PARENTHESIS) then 
    begin
      ParseEnum(EnumType, TypeName, d);
      U.AddType(EnumType);
      FreeAndNil(t);
      Exit;
    end;
    SetLength(LCollected, Length(LCollected)-Length(t.Data));
    Scanner.UnGetToken(t);
  end;
  FreeAndNil(t);

  NormalType := TPasType.Create;
  NormalType.FullDeclaration := LCollected;
  SkipDeclaration(NormalType, false);
  NormalType.Name := TypeName;
  NormalType.RawDescription := d;
  U.AddType(NormalType);
  ItemsForNextBackComment.ClearAndAdd(NormalType);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUnit(var U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_UNIT);

  U := TPasUnit.Create;
  try
    U.RawDescription := GetLastComment;

    { get unit name identifier }
    U.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
    
    ItemsForNextBackComment.ClearAndAdd(U);
    
    ParseHintDirectives(U);
    
    { skip semicolon }
    GetAndCheckNextToken(SYM_SEMICOLON);
    
    { get 'interface' keyword }
    GetAndCheckNextToken(KEY_INTERFACE);
    
    { now parse the interface section of that unit }
    ParseInterfaceSection(U);
  except
    FreeAndNil(U);
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUses(const U: TPasUnit);
begin
  ParseCommaSeparatedIdentifiers(U.UsesUnits, SYM_SEMICOLON, nil);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCommaSeparatedIdentifiers(Names: TStrings;
  FinalSymbol: TSymbolType; RawDescriptions: TStringList);
var
  T: TToken;
  FirstIdentifier: boolean;
begin
  FirstIdentifier := true;
  
  repeat
    Names.Append(GetAndCheckNextToken(TOK_IDENTIFIER));
    
    { Now we modify FirstIdentifier and append item to RawDescriptions }
    if FirstIdentifier then
    begin
      FirstIdentifier := false;
      if RawDescriptions <> nil then
        RawDescriptions.Append(GetLastComment);
    end else
    if RawDescriptions <> nil then
    begin
      if IsLastComment then
        RawDescriptions.Append(GetLastComment) else
        RawDescriptions.Append(RawDescriptions[RawDescriptions.Count - 1]);
    end;
    
    T := GetNextToken;
    try
      if (T.MyType <> TOK_SYMBOL) or
        ( (T.Info.SymbolType <> SYM_COMMA) and
          (T.Info.SymbolType <> FinalSymbol) ) then
        DoError('One of symbols "," or "%s" expected', [SymbolNames[FinalSymbol]]);
      if T.Info.SymbolType = FinalSymbol then
        break;
    finally
      FreeAndNil(T);
    end;
  until false;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseVariables(const U: TPasUnit);
begin
  ParseFieldsVariables(U.Variables, false, viPublished, false);
end;

procedure TParser.ParseFieldsVariables(Items: TPasItems; 
  OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean);
var
  Finished: Boolean;
  NewItem: TPasFieldVariable;
  ItemCollector: TPasFieldVariable;
  m: TPasMethod;
  t, ttemp: TToken;
  FirstCheck: boolean;
  NewItemNames: TStringList;
  I: Integer;
  RawDescriptions: TStringList;
begin
  NewItemNames := nil;
  RawDescriptions := nil;
  try
    NewItemNames := TStringList.Create;
    RawDescriptions := TStringList.Create;
    
    ParseCommaSeparatedIdentifiers(NewItemNames, SYM_COLON, RawDescriptions);
    
    ItemCollector := TPasFieldVariable.Create;
    try
      ItemCollector.FullDeclaration := ':';
      
      t := GetNextToken(ItemCollector);
      ItemCollector.FullDeclaration := ItemCollector.FullDeclaration + t.Data;
      if (t.MyType = TOK_KEYWORD) and 
         (t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE]) then 
      begin
        { KeyWordString for ParseCDFP below is '', because we already included
          t.Data inside ItemCollector.FullDeclaration. 
          If KeyWordString would be t.Data, then we would incorrectly
          append t.Data twice to ItemCollector.FullDeclaration
          when appending m.FullDeclaration to ItemCollector.FullDeclaration. }
        ParseCDFP(M, '', '', t.Info.KeyWord, '', false);
        ItemCollector.FullDeclaration := 
          ItemCollector.FullDeclaration + M.FullDeclaration;
        M.Free;
        FreeAndNil(t);
        t := GetNextToken(ItemCollector);

        if t.IsSymbol(SYM_EQUAL) then
        begin
          ItemCollector.FullDeclaration := 
            ItemCollector.FullDeclaration + t.Data;
          SkipDeclaration(ItemCollector, IsInRecordCase);
        end else 
        begin
          Scanner.UnGetToken(t);
        end;
      end else
      if t.IsKeyWord(KEY_RECORD) then
      begin
        ParseCIO(nil, '', CIO_RECORD, '', IsInRecordCase);
      end else
      if t.IsKeyWord(KEY_PACKED) then
      begin 
        FreeAndNil(t);
        t := GetNextToken;
        if t.IsKeyWord(KEY_RECORD) then 
        begin
          ParseCIO(nil, '', CIO_PACKEDRECORD, '', IsInRecordCase);
        end else 
        begin
          SkipDeclaration(ItemCollector, IsInRecordCase);
        end;      
      end else
      begin
        SkipDeclaration(ItemCollector, IsInRecordCase);
      end;

      if not OfObject then
      begin
        // The following section allows PasDoc to parse variable modifiers in FPC.
        // See: http://www.freepascal.org/docs-html/ref/refse19.html
        Finished := False;
        FirstCheck := True;
        repeat
          ttemp := GetNextToken;

          if FirstCheck then
          begin
            // If the first non-white character token after the semicolon
            // is "cvar", "export', "external", or "public", there is
            // a variable modifier present.

            // This does not take into account the "absolute" modifier
            // (which is not preceeded by a semicolon).
            FirstCheck := False;
            if (ttemp.MyType = TOK_IDENTIFIER) and 
               (ttemp.Info.StandardDirective in 
                 [SD_CVAR, SD_EXPORT, SD_EXTERNAL, SD_PUBLIC]) then 
            begin
              ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ' ' + ttemp.Data;
              FreeAndNil(ttemp)
            end
            else
            begin
              Finished := True;
              Scanner.UnGetToken(ttemp);
            end;
            while not Finished do
            begin
              ttemp := GetNextToken;
              if ttemp.IsSymbol(SYM_SEMICOLON) then
              begin
                Finished := True;
                FirstCheck := False;
                ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ttemp.Data;
              end
              else
              begin
                ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ' ' + ttemp.Data;
              end;
              FreeAndNil(ttemp)
            end;
          end;
        until Finished and not FirstCheck;
      end;

      if Items <> nil then
      begin
        ItemsForNextBackComment.Clear;
        for I := 0 to NewItemNames.Count - 1 do
        begin
          NewItem := TPasFieldVariable.Create;
          NewItem.Name := NewItemNames[i];
          NewItem.Visibility := Visibility;
          NewItem.RawDescription := RawDescriptions[i];
          NewItem.FullDeclaration := NewItem.Name + ItemCollector.FullDeclaration;
          NewItem.IsDeprecated := ItemCollector.IsDeprecated;
          NewItem.IsPlatformSpecific := ItemCollector.IsPlatformSpecific;
          NewItem.IsLibrarySpecific := ItemCollector.IsLibrarySpecific;
          Items.Add(NewItem);
          ItemsForNextBackComment.Add(NewItem);
        end;
      end;
    finally
      ItemCollector.Free;
      FreeAndNil(t);
    end;
  finally 
    NewItemNames.Free;
    RawDescriptions.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.SetCommentMarkers(const Value: TStringList);
begin
  FCommentMarkers.Assign(Value);
end;

procedure TParser.SkipDeclaration(const Item: TPasItem; IsInRecordCase: boolean);
var
  EndLevel: Integer;
  IsSemicolon: Boolean;
  PLevel: Integer;
  t: TToken;
  WhitespaceCollector: string;
begin
  EndLevel := 0;
  PLevel := 0;
  repeat
    t := GetNextToken(WhitespaceCollector);

    if Assigned(Item) then begin
      Item.FullDeclaration := Item.FullDeclaration + WhitespaceCollector;
    end;
    case t.MyType of
      TOK_SYMBOL:
        case t.Info.SymbolType of
          SYM_LEFT_PARENTHESIS: Inc(PLevel);
          SYM_RIGHT_PARENTHESIS: Dec(PLevel);
        end;
      TOK_KEYWORD:
        case t.Info.KeyWord of
          KEY_END: Dec(EndLevel);
          KEY_RECORD: Inc(EndLevel);
          KEY_LIBRARY: 
            if Assigned(Item) then Item.IsLibrarySpecific := true;
        end;
      TOK_IDENTIFIER:
        case t.Info.StandardDirective of
          SD_PLATFORM: 
            if Assigned(Item) then Item.IsPlatformSpecific := true;
          SD_DEPRECATED: 
            if Assigned(Item) then Item.IsDeprecated := true;
        end;
    end;
    IsSemicolon := t.IsSymbol(SYM_SEMICOLON);
    
    { Reason for "EndLevel < 0" condition:
        Within records et al. the last declaration need not be terminated by ;
      Reason for "(PLevel < 0) and IsInRecordCase" condition:
        See autodoc of SkipDeclaration in TParser interface. }
    if (EndLevel < 0) or
       ( (PLevel < 0) and IsInRecordCase ) then 
    begin
      Scanner.UnGetToken(t);
      Exit;
    end;
    
    if Assigned(Item) then Item.FullDeclaration := Item.FullDeclaration + t.Data;
    
    FreeAndNil(t);
  until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
end;

{ ---------------------------------------------------------------------------- }

function TParser.PeekNextToken(out WhitespaceCollector: string): TToken;

  { Extracts the documentation comment from T.CommentContent to DocComment.
    Always T.MyType must be within TokenCommentTypes.

    T must not be nil.

    The comment is intended to be a "documentation comment",
    i.e. we intend to put it inside output documentation.
    So comment markers, if present, 
    are removed from the beginning and end of the data.
    Also, if comment markers were required but were not present,
    then CommentContent is an empty string.

    Also back-comment marker, the '<', is removed, if exists,
    and BackComment is set to @true. Otherwise BackComment is @false. }
  procedure ExtractDocComment(
    const t: TToken; out DocComment: string; out BackComment: boolean);
  const
    BackCommentMarker = '<';
  var
    i: integer;
    Marker: string;
    WasMarker: boolean;
  begin
    BackComment := false;
    DocComment := t.CommentContent;

    if CommentMarkers.Count <> 0 then
    begin
      WasMarker := false;
      for i := 0 to CommentMarkers.Count - 1 do 
      begin
        Marker := CommentMarkers[i];
        if IsPrefix(Marker, DocComment) then
        begin
          Delete(DocComment, 1, Length(Marker));
          WasMarker := true;
          Break;
        end;
      end;

      if (not MarkersOptional) and (not WasMarker) then
      begin
        DocComment := '';
        Exit;
      end;
    end;

    if SCharIs(DocComment, 1, BackCommentMarker) then
    begin
      BackComment := true;
      Delete(DocComment, 1, Length(BackCommentMarker));
    end;
  end;    

var
  T: TToken;
  TBackComment, TIsCStyle: boolean;
  TDocComment: string;
begin
  Result := nil;
  t := nil;
  WhitespaceCollector := '';
  repeat
    t := Scanner.PeekToken;
    if t.MyType in TokenCommentTypes then
    begin
      Scanner.ConsumeToken;

      { Get info from T }
      ExtractDocComment(T, TDocComment, TBackComment);
      TIsCStyle := t.MyType = TOK_COMMENT_CSTYLE;
      FreeAndNil(T);
      
      if TBackComment then
      begin
        { TODO -- this remains to be written to properly implement back-comments }
      end else
      if IsLastComment and LastCommentWasCStyle and TIsCStyle then
      begin
        { If there are several //-style comments in a row, combine them }
        LastCommentContent := LastCommentContent + LineEnding + TDocComment;
      end else
      begin
        { This is a normal comment, so fill [Is]LastCommentXxx properties }
        IsLastComment := true;
        LastCommentWasCStyle := TIsCStyle;
        LastCommentContent := TDocComment;
      end;
    end else
    begin
      case t.MyType of
        TOK_WHITESPACE: 
          begin
            Scanner.ConsumeToken;
            WhitespaceCollector := WhitespaceCollector + t.Data;
            FreeAndNil(t);
          end;
        else 
          begin
            Result := t;
            break;
          end;
      end;
    end;
  until False;
end;

function TParser.PeekNextToken: TToken; 
var 
  Dummy: string;
begin
  Result := PeekNextToken(Dummy);
end;

{ ------------------------------------------------------------ }

procedure TParser.ParseHintDirectives(Item: TPasItem);
var
  t: TToken;
begin
  repeat
    t := PeekNextToken;
    
    if t.IsStandardDirective(SD_PLATFORM) then
    begin
      Scanner.ConsumeToken;
      Item.IsPlatformSpecific := true;
      FreeAndNil(t);
    end else
    if t.IsStandardDirective(SD_DEPRECATED) then
    begin
      Scanner.ConsumeToken;
      Item.IsDeprecated := true;
      FreeAndNil(t);
    end else
    if t.IsKeyWord(KEY_LIBRARY) then
    begin
      Scanner.ConsumeToken;
      Item.IsLibrarySpecific := true;
      FreeAndNil(t);
    end else
      break;
  until false;
end;

end.
