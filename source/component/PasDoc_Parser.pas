{ @abstract(provides all the parsing functionality of pasdoc)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)
  @cvs($Date$)

  Parsing implements most of the functionality of the pasdoc program.

  It provides the @link(TParser) object, which scans the command line parameters
  for file names and switches and then starts collecting information from those
  files, issueing warnings to standard out if necessary. }

unit PasDoc_Parser;

{$I pasdoc_defines.inc}

interface

uses
  Classes,
  PasDoc_Types,
  PasDoc_Items,
  PasDoc_Scanner,
  PasDoc_Tokenizer,
  PasDoc_StringVector;

type
  // @name stores a series of @link(TRawDescriptionInfo TRawDescriptionInfos).
  // It is modelled after TStringList but has only the minimum number
  // of methods required for use in PasDoc.
  TRawDescriptionInfoList = class(TObject)
  private
    // @name holds the @link(TRawDescriptionInfo TRawDescriptionInfos) in @classname
    FItems: array of TRawDescriptionInfo;
    // @name holds the number of items currently stored in @classname.
    // @seealso(Count).
    FCount: integer;
    // @name is the read specifier for @link(Items)
    function GetItems(Index: integer): TRawDescriptionInfo;
    // @name expands the capacity of @link(FItems).
    procedure Grow;
  public
    // @name adds a new @link(TRawDescriptionInfo) to @classname.
    function Append(Comment: TRawDescriptionInfo): integer;
    // @name is the number of @link(TRawDescriptionInfo TRawDescriptionInfos) in
    // @classname.
    property Count: integer read FCount;
    Constructor Create;
    // @name provides read access to the 
    // @link(TRawDescriptionInfo TRawDescriptionInfos) in @classname.
    property Items[Index: integer]: TRawDescriptionInfo read GetItems; default;
  end;

  { Parser class that will process a complete unit file and all of its
    include files, regarding directives.
    When creating this object constructor @link(Create) takes as an argument 
    an input stream and a list of directives.
    Parsing work is done by calling @link(ParseUnitOrProgram) method.
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
      
      @item Of TPasEnum: Members, FullDeclararation.
      
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
    LastCommentInfo: TRawDescriptionInfo;
    
    { The underlying scanner object. }
    Scanner: TScanner;
    
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FCommentMarkers: TStringList;
    FMarkersOptional: boolean;
    FIgnoreLeading: string;
    FShowVisibilities: TVisibilities;
    
    { These are the items that the next "back-comment"
      (the comment starting with "<", see
      [http://pasdoc.sipsolutions.net/WhereToPlaceComments]
      section "Placing comments after the item") will apply to. }
    ItemsForNextBackComment: TPasItems;
    
    { Returns @link(TMethodType) value for corresponding @link(TKeyWord) value.
      If given KeyWord has no corresponding @link(TMethodType) value,
      raises @link(EInternalError). }
    function KeyWordToMethodType(KeyWord: TKeyWord): TMethodType;

    procedure DoError(const AMessage: string; 
      const AArguments: array of const);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);

    { Checks if T.MyType is ATokenType, if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ATokenType: TTokenType); overload;
    
    { Checks if T.IsSymbol(ASymbolType), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ASymbolType: TSymbolType); overload;
    
    { Checks if T.IsKeyWord(AKeyWord), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; AKeyWord: TKeyWord); overload;

    { If not IsLastComment, then returns @link(EmptyRawDescriptionInfo)
      otherwise returns LastCommentInfo and sets IsLastComment to false. }
    function GetLastComment: TRawDescriptionInfo;

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
    { This is an overload for parsing a unit name that may contain one or
      more dots ('.')
      @param AIsUnitName is a dummy parameter to allow overloading that is assumed to be true }
    function GetAndCheckNextToken(ATokenType: TTokenType; AIsUnitname: boolean): string; overload;

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
    
    { Parses a constructor, a destructor, a function or a procedure
      or an operator (for FPC).
      Resulting @link(TPasMethod) item will be returned in M.
      
      ClassKeywordString contains the keyword 'class'
      in the exact spelling as it was found in input,
      for class methods. Else it contains ''.
      
      MethodTypeString contains the keyword 'constructor', 'destructor', 
      'function' or 'procedure' or standard directive 'operator' 
      in the exact spelling as it was found in input.
      You can specify MethodTypeString = '', this way you avoid including
      such keyword at the beginning of returned M.FullDeclaration.
      
      MethodType is used for the What field of the resulting TPasMethod. 
      This should correspond to MethodTypeString.
      
      D may contain a description or nil. }
    procedure ParseCDFP(out M: TPasMethod; 
      const ClassKeywordString: string;
      const MethodTypeString: string; MethodType: TMethodType;
      const RawDescriptionInfo: TRawDescriptionInfo;
      const NeedName: boolean; InitItemsForNextBackComment: boolean);
      
    { Parses a class, an interface or an object.
      U is the unit this item will be added to on success.
      N is the name of this item.
      CIOType describes if item is class, interface or object.
      D may contain a description or nil. }
    procedure ParseCIO(const U: TPasUnit; 
      const CioName: string; CIOType: TCIOType; 
      const RawDescriptionInfo: TRawDescriptionInfo;
      const IsInRecordCase: boolean);
    
    procedure ParseRecordCase(const R: TPasCio; const SubCase: boolean);
    procedure ParseConstant(const U: TPasUnit);
    procedure ParseInterfaceSection(const U: TPasUnit);
    procedure ParseProperty(out p: TPasProperty);
    procedure ParseType(const U: TPasUnit);
    
    { This assumes that you just read left parenthesis starting
      an enumerated type. It finishes parsing of TPasEnum,
      returning is as P. }
    procedure ParseEnum(out p: TPasEnum; const Name: string;
      const RawDescriptionInfo: TRawDescriptionInfo);

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
      FinalSymbol: TSymbolType; 
      RawDescriptions: TRawDescriptionInfoList);
    
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
      OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean;
      const ClassKeyWordString: string = '');
    
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
    
    procedure ParseUnit(U: TPasUnit);
    procedure ParseProgram(U: TPasUnit);
    procedure ParseProgramOrLibraryUses(U: TPasUnit);
    procedure ParseLibrary(U: TPasUnit);
  public
    { Create a parser, initialize the scanner with input stream S.
      All strings in SD are defined compiler directives. }
    constructor Create(
      const InputStream: TStream;
      const Directives: TStringVector;
      const IncludeFilePaths: TStringVector;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName, AStreamPath: string;
      const AHandleMacros: boolean);
      
    { Release all dynamically allocated memory. }
    destructor Destroy; override;
    
    { This does the real parsing work, creating U unit and parsing
      InputStream and filling all U properties. }
    procedure ParseUnitOrProgram(var U: TPasUnit);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkersOptional: boolean read fMarkersOptional write fMarkersOptional;
    property IgnoreLeading: string read FIgnoreLeading write FIgnoreLeading;
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
  PasDoc_Utils;

{ ---------------------------------------------------------------------------- }
{ TParser }
{ ---------------------------------------------------------------------------- }

constructor TParser.Create(
  const InputStream: TStream;
  const Directives: TStringVector;
  const IncludeFilePaths: TStringVector;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName, AStreamPath: string;
  const AHandleMacros: boolean);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  Scanner := TScanner.Create(InputStream, OnMessageEvent, 
    VerbosityLevel, AStreamName, AStreamPath, AHandleMacros);
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

function TParser.KeyWordToMethodType(KeyWord: TKeyWord): TMethodType;
begin
  case KeyWord of
    KEY_CONSTRUCTOR: Result := METHOD_CONSTRUCTOR;
    KEY_DESTRUCTOR:  Result := METHOD_DESTRUCTOR;
    KEY_FUNCTION:    Result := METHOD_FUNCTION;
    KEY_PROCEDURE:   Result := METHOD_PROCEDURE;
  else
    raise EInternalError.Create('KeyWordToMethodType: invalid keyword');
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoError(const AMessage: string; 
  const AArguments: array of const);
begin
  raise EPasDoc.Create(Scanner.GetStreamInfo + ': ' + AMessage, AArguments, 1);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
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

function TParser.GetLastComment: TRawDescriptionInfo;
begin
  if IsLastComment then
  begin
    Result := LastCommentInfo;
    IsLastComment := false;
  end else
    Result := EmptyRawDescriptionInfo;
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

function TParser.GetAndCheckNextToken(ATokenType: TTokenType; AIsUnitname: boolean): string;
var
  T: TToken;
  s: string;
begin
  // note: the Value of AIsUnitName is ignored, assume it is true
  Result := '';
  while true do begin
    T := GetNextToken;
    try
      CheckToken(T, ATokenType);
      Result := Result + T.Data;
    finally
      T.Free;
    end;
    t := PeekNextToken(s);
    if (s = '') and t.IsSymbol(SYM_PERIOD) then begin
      Result := Result + '.';
      Scanner.ConsumeToken;
      t.Free;
    end else
      exit;
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
  const MethodTypeString: string; MethodType: TMethodType;
  const RawDescriptionInfo: TRawDescriptionInfo;
  const NeedName: boolean; InitItemsForNextBackComment: boolean);
  
  { Reads tokens (adding them to M.FullDeclaration) until a semicolon
    (on parenthesis level zero) is found (this final semicolon 
    is also read and appended to M.FullDeclaration). }
  procedure ReadTokensUntilSemicolon;
  var
    T: TToken;
    Level: Integer;
    IsSemicolon: Boolean;
  begin
    Level := 0;
    repeat
      T := Scanner.GetToken;
      try
        if T.MyType = TOK_WHITESPACE then
        begin
          { add exactly *one space* at the end of M.FullDeclaration }
          if Length(M.FullDeclaration) > 0 then 
          begin
            if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
              M.FullDeclaration := M.FullDeclaration + ' ';
          end;
        end else
        if not (T.MyType in TokenCommentTypes) then
          M.FullDeclaration := M.FullDeclaration + T.Data;

        if T.IsSymbol(SYM_LEFT_PARENTHESIS) then Inc(level);
        if T.IsSymbol(SYM_RIGHT_PARENTHESIS) then Dec(level);
        IsSemicolon := T.IsSymbol(SYM_SEMICOLON);
      finally
        FreeAndNil(T);
      end;
    until IsSemicolon and (Level = 0);
  end;
  
var
  t: TToken;
  InvalidType: boolean;
begin
  M := TPasMethod.Create;
  M.RawDescriptionInfo^ := RawDescriptionInfo;
  if InitItemsForNextBackComment then
    ItemsForNextBackComment.ClearAndAdd(M);
    
  t := nil;
  M.What := MethodType;
  
  if ClassKeyWordString <> '' then
    M.FullDeclaration :=  ClassKeyWordString + ' ';
  M.FullDeclaration := M.FullDeclaration + MethodTypeString;

  { next non-wc token must be the name }
  if NeedName then
  begin
    try
      t := GetNextToken;
    except
      M.Free;
      raise;
    end;
    if (MethodType = METHOD_OPERATOR) then
    begin
      { In FPC operators "or", "and", "xor" (expressed as keywords) can be
        overloaded, also symbolic operators like "+", "*" etc..
        In Delphi 2006+ "operator" is followed by identifiers like
        "Implicit", "Explicit" or "LogicalNot",  "BitwiseAnd" etc.. }
      InvalidType := (t.MyType <> TOK_IDENTIFIER) and
                     (t.MyType <> TOK_SYMBOL) and (t.MyType <> TOK_KEYWORD);
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
    DoMessage(5, pmtInformation, 'Parsing %s "%s"',
      [MethodTypeToString(MethodType), M.Name]);
    M.FullDeclaration := M.FullDeclaration + ' ' + M.Name;
    FreeAndNil(t);
  end;
  
  ReadTokensUntilSemicolon;

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
        SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STATIC,
        SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
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
            specifies the message ID.
          * Deprecated might be followed by a string constant since D2010. }
        SD_EXTERNAL, SD_MESSAGE, SD_NAME, SD_DEPRECATED:
          begin
            M.IsDeprecated := t.Info.StandardDirective = SD_DEPRECATED;
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
                { Some directives mean that the next part starts immediately,
                  without a semicolon between. For example,
                  "deprecated platform" may be placed after a procedure
                  (without a semicolon after "deprecated"). }
                if t.MyType = TOK_IDENTIFIER then 
                  case t.Info.StandardDirective of
                    SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
                    SD_EXTERNAL,
                    SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
                    SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STATIC,
                    SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
                    SD_DEPRECATED, SD_PLATFORM:
                      begin
                        Scanner.UnGetToken(t);
                        Break;
                      end;
                  end;

                M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
                FreeAndNil(t);
              end;
            until False;
          end;
        SD_PLATFORM: 
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            M.IsPlatformSpecific := True;
            FreeAndNil(t);
            t := GetNextToken;
          end;
        SD_DISPID: 
          begin
            M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
            FreeAndNil(T);
            
            ReadTokensUntilSemicolon;
            
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

    { Directives don't have to be separated and be terminated by a semicolon.
      This is known at least for combination "deprecated platform".
      Apparently, some Delphi versions also allowed not using semicolon
      at other places (and we have to mimic compiler behavior, not only
      documented behavior, since in practice people (over)use everything
      that compiler allows).
      
      So, check is current token a semicolon and append to FullDeclaration.
      Note that T may be nil now (e.g. because we used UnGetToken last). }
    if (t <> nil) and t.IsSymbol(SYM_SEMICOLON) then
      M.FullDeclaration := M.FullDeclaration + ';'
    else begin
      M.FullDeclaration := M.FullDeclaration + ' ';
      if t <> nil then Scanner.UnGetToken(t);
    end;

  until False;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCIO(const U: TPasUnit; 
  const CioName: string; CIOType: TCIOType; 
  const RawDescriptionInfo: TRawDescriptionInfo;
  const IsInRecordCase: boolean);

  { Parse fields clause, i.e. something like
      NAME1, NAME2, ... : TYPE;
    If AddToFields then adds parsed fields to i.Fields.
    Visibility of created fields is set to given Visibility parameter. }
  procedure ParseFields(i: TPasCio; AddToFields: boolean; 
    Visibility: TVisibility; const ClassKeyWordString: string);
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
    ParseFieldsVariables(Items, true, Visibility, false, ClassKeyWordString);
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
  StrictVisibility: boolean;
begin
  StrictVisibility := False;
  t := nil;
  DoMessage(5, pmtInformation, 'Parsing class/interface/object "%s"', [CioName]);
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
      i.RawDescriptionInfo^ := RawDescriptionInfo;
      i.MyType := CIOType;

      if (CIOType in [ CIO_CLASS, CIO_PACKEDCLASS ] ) and 
        (t.MyType = TOK_IDENTIFIER) and
        (t.Info.StandardDirective in [SD_ABSTRACT, SD_SEALED]) then
      begin
        if t.Info.StandardDirective = SD_ABSTRACT then
        begin
          i.ClassDirective := CT_ABSTRACT;
        end
        else
        begin
          i.ClassDirective := CT_SEALED;
        end;
        FreeAndNil(t);
        t := GetNextToken;
      end;
      
      { This allows to write back-comment for class declarations like
          TMyClass = class(TMyAncestor) //< back comment
      }
      ItemsForNextBackComment.ClearAndAdd(I);

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
          CIO_CLASS, CIO_PACKEDCLASS: begin
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
          CIO_OBJECT, CIO_PACKEDOBJECT: begin
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

      if I.MyType in [ CIO_CLASS, CIO_PACKEDCLASS ] then
      begin
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
      end else
      begin
        { Everything besides a class always starts with visibility "public". }
        Visibility := viPublic;
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
        else if T.Info.StandardDirective = SD_OPERATOR then
        begin
          { Same code as for KEY_CONSTRUCTOR, KEY_DESTRUCTOR,
            KEY_FUNCTION, KEY_PROCEDURE below, something to be optimized. }

          try
            ParseCDFP(M, ClassKeyWordString, t.Data, METHOD_OPERATOR,
              GetLastComment, true, true);
          except
            i.Free;
            FreeAndNil(t);
            raise;
          end;
          ClassKeyWordString := '';

          if Visibility in ShowVisibilities then
          begin
            M.Visibility := Visibility;
            i.Methods.Add(M);
          end
          else begin
            ItemsForNextBackComment.Clear;
            FreeAndNil(M);
          end;
        end
        else if (t.MyType = TOK_KEYWORD) then
          begin
            if StrictVisibility then
            begin
              DoError('"strict" found in an unexpected location', []);
            end;
            case t.Info.KeyWord of
              KEY_VAR:
                begin
                  ClassKeyWordString := Trim(ClassKeyWordString + ' ' + t.Data);
                end;
              KEY_CLASS: ClassKeyWordString := t.Data;
              KEY_CONSTRUCTOR,
              KEY_DESTRUCTOR,
              KEY_FUNCTION,
              KEY_PROCEDURE: 
                begin
                  try
                    ParseCDFP(M, ClassKeyWordString, 
                      t.Data, KeyWordToMethodType(t.Info.KeyWord),
                      GetLastComment, true, true);
                  except
                    i.Free;
                    FreeAndNil(t);
                    raise;
                  end;
                  ClassKeyWordString := '';
                  
                  if Visibility in ShowVisibilities then
                  begin
                    M.Visibility := Visibility;
                    i.Methods.Add(M);
                  end else
                  begin
                    ItemsForNextBackComment.Clear;
                    FreeAndNil(M);
                  end;
                end;
              KEY_END: Finished := True;
              KEY_PROPERTY: 
                begin
                  ParseProperty(p);
                  
                  if Visibility in ShowVisibilities then 
                  begin
                    p.Visibility := Visibility;
                    i.Properties.Add(p);
                  end else
                  begin
                    ItemsForNextBackComment.Clear;
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
          end
          else
            if (t.MyType = TOK_IDENTIFIER) then
            begin

              case t.Info.StandardDirective of
                SD_DEFAULT: 
                  begin
                    if StrictVisibility then
                    begin
                      DoError('"strict" found in an unexpected location', []);
                    end;
                    { Note: 2nd arg for SkipDeclaration is always "false",
                      not "IsInRecordCase". That's because even if declaration
                      of this CIO is within a record case, then we want to
                      see record's terminating "end" keyword anyway.
                      So it doesn't matter here whether our IsInRecordCase
                      parameter is true. }
                    SkipDeclaration(nil, false);
                    DoMessage(5, pmtInformation, 'Skipped default property keyword.', []);
                  end;
                SD_PUBLIC:
                  begin
                    if StrictVisibility then
                    begin
                      DoError('"strict" found in an unexpected location', []);
                    end;
                    Visibility := viPublic;
                  end;
                SD_PUBLISHED:
                  begin
                    if StrictVisibility then
                    begin
                      DoError('"strict" found in an unexpected location', []);
                    end;
                    Visibility := viPublished;
                  end;
                SD_PRIVATE:
                  begin
                    if StrictVisibility then
                    begin
                      StrictVisibility := False;
                      Visibility := viStrictPrivate;
                    end
                    else
                    begin
                      Visibility := viPrivate;
                    end;
                  end;
                SD_PROTECTED:
                  begin
                    if StrictVisibility then
                    begin
                      StrictVisibility := False;
                      Visibility := viStrictProtected;
                    end
                    else
                    begin
                      Visibility := viProtected;
                    end;
                  end;
                SD_AUTOMATED:
                  begin
                    Visibility := viAutomated;
                  end;
                SD_STRICT: StrictVisibility := True;
                else
                  begin
                    Scanner.UnGetToken(T);
                    ParseFields(i, Visibility in ShowVisibilities, Visibility,
                      ClassKeyWordString);
                    if not (Visibility in ShowVisibilities) then
                      ItemsForNextBackComment.Clear;
                    ClassKeyWordString := '';
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
  t: TToken;
  WhitespaceCollector: string;

  { Same as SkipDeclaration() except that it returns TRUE if we hit a  }
  { procedural constant, it might be followed by a calling convention  }
  function LocalSkipDeclaration: Boolean;
  var
    EndLevel, PLevel: Integer;
    IsSemicolon: Boolean;
  begin
    EndLevel := 0;
    PLevel := 0;
    Result := False;
    {AG}
    repeat
      t := GetNextToken(WhitespaceCollector);

      i.FullDeclaration := i.FullDeclaration + WhitespaceCollector;
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
              i.IsLibrarySpecific := true;
            KEY_FUNCTION,
            KEY_PROCEDURE:
              Result := True;
        end;
      TOK_IDENTIFIER:
        case t.Info.StandardDirective of
          SD_PLATFORM:
            i.IsPlatformSpecific := true;
          SD_DEPRECATED:
            i.IsDeprecated := true;
        end;
      end;
      IsSemicolon := t.IsSymbol(SYM_SEMICOLON);

      { Reason for "EndLevel < 0" condition:
          Within records et al. the last declaration need not be terminated by ;
        Reason for "(PLevel < 0) and IsInRecordCase" condition:
          See autodoc of SkipDeclaration in TParser interface. }
      if (EndLevel < 0) {or
        ( (PLevel < 0) and IsInRecordCase )} then
      begin
        Scanner.UnGetToken(t);
        Exit;
      end;
      i.FullDeclaration := i.FullDeclaration + t.Data;
      t.Free;
    until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
  end;
  {/AG}

begin
  i := TPasConstant.Create;
  i.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
  DoMessage(5, pmtInformation, 'Parsing constant %s', [i.Name]);
  i.RawDescriptionInfo^ := GetLastComment;
  i.FullDeclaration := i.Name;
  {AG}
  if LocalSkipDeclaration then
  begin
    { Check for following calling conventions }
    t := GetNextToken(WhitespaceCollector);
    case t.Info.StandardDirective of
      SD_CDECL, SD_STDCALL, SD_PASCAL, SD_REGISTER, SD_SAFECALL:
        begin
          i.FullDeclaration := i.FullDeclaration + WhitespaceCollector;
          i.FullDeclaration := i.FullDeclaration + t.Data;
          t.Free;
          SkipDeclaration(i, false);
        end;
    else
      Scanner.UnGetToken(t);
    end;
  end;
  //SkipDeclaration(i, false);
  {/AG}

  U.AddConstant(i);
  ItemsForNextBackComment.ClearAndAdd(I);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseEnum(out p: TPasEnum;
  const Name: string; const RawDescriptionInfo: TRawDescriptionInfo);
var
  T: TToken;
  Item: TPasItem;
  ParenLevel: Integer;
begin
  t := nil;
  p := TPasEnum.Create;
  p.Name := Name;
  p.RawDescriptionInfo^ := RawDescriptionInfo;
  p.FullDeclaration := Name + ' = (...);'; 
  ItemsForNextBackComment.ClearAndAdd(P);

  T := GetNextToken;
  while not T.IsSymbol(SYM_RIGHT_PARENTHESIS) do
  begin
    CheckToken(T, TOK_IDENTIFIER);
    Item := TPasItem.Create;
    Item.Name := T.Data;
    Item.RawDescriptionInfo^ := GetLastComment;
    Item.FullDeclaration := Item.Name;
    p.Members.Add(Item);
    ItemsForNextBackComment.ClearAndAdd(Item);
    FreeAndNil(T);

    T := GetNextToken;
    if T.IsSymbol(SYM_EQUAL) or T.IsSymbol(SYM_ASSIGN) then
    begin
      Item.FullDeclaration := Item.FullDeclaration + ' ' + T.Data + ' ';
      FreeAndNil(T);
      
      { Now read tokens until comma or right paren (but only on ParenLevel = 0). }
      ParenLevel := 0;
      repeat
        T := GetNextToken;
        
        if (ParenLevel = 0) and
           (T.IsSymbol(SYM_COMMA) or T.IsSymbol(SYM_RIGHT_PARENTHESIS)) then
          Break;
          
        if T.MyType = TOK_SYMBOL then
          case T.Info.SymbolType of
            SYM_LEFT_PARENTHESIS, SYM_LEFT_BRACKET: Inc(ParenLevel);
            SYM_RIGHT_PARENTHESIS, SYM_RIGHT_BRACKET: Dec(ParenLevel);
          end;
            
        Item.FullDeclaration := Item.FullDeclaration + T.Data;
        FreeAndNil(T);
      until false;
    end;
    
    if T.IsSymbol(SYM_COMMA) then
    begin
      FreeAndNil(T);
      T := GetNextToken;
    end;
  end;
  FreeAndNil(T);
  
  GetAndCheckNextToken(SYM_SEMICOLON);
end;

procedure TParser.ParseInterfaceSection(const U: TPasUnit);
const
  MODE_UNDEFINED = 0;
  MODE_CONST = 1;
  MODE_TYPE = 2;
  MODE_VAR = 3;
var
  Finished: Boolean;
  Mode: Integer;
  M: TPasMethod;
  t: TToken;
  PropertyParsed: TPasProperty;
begin
  DoMessage(4, pmtInformation, 'Entering interface section of unit %s',[U.Name]);
  Finished := False;
  Mode := MODE_UNDEFINED;

  repeat
    t := GetNextToken;

    try
      case t.MyType of
        TOK_IDENTIFIER:
          if T.Info.StandardDirective = SD_OPERATOR then
          begin
            ParseCDFP(M, '', t.Data, METHOD_OPERATOR,
              GetLastComment, true, true);
            u.FuncsProcs.Add(M);
            Mode := MODE_UNDEFINED;
          end else
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
              KEY_FUNCTION, KEY_PROCEDURE: 
                begin
                  ParseCDFP(M, '', t.Data, KeyWordToMethodType(t.Info.KeyWord), 
                    GetLastComment, true, true);
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
              KEY_PROPERTY:
                begin
                  ParseProperty(PropertyParsed);
                  U.Variables.Add(PropertyParsed);
                  Mode := MODE_UNDEFINED;
                end;
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
  DoMessage(5, pmtInformation, 'Parsing property %s', [p.Name]);
  p.IndexDecl := '';
  p.Proptype := '';
  p.FullDeclaration := 'property ' + p.Name;
  p.RawDescriptionInfo^ := GetLastComment;
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
      p.RawDescriptionInfo^ := GetLastComment;
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
        TOK_KEYWORD:
        begin
          if not (t1.Info.KeyWord in [KEY_OR, KEY_AND]) then
          begin
            try
              DoError('Unexpected %s', [T1.Description]);
            finally
              FreeAndNil(t1);
            end;
          end;
        end
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
  RawDescriptionInfo: TRawDescriptionInfo;
  NormalType: TPasType;
  TypeName: string;
  LCollected, LTemp: string;
  MethodType: TPasMethod;
  EnumType: TPasEnum;
  T: TToken;
begin
  TypeName := GetAndCheckNextToken(TOK_IDENTIFIER);
  DoMessage(5, pmtInformation, 'Parsing type "%s"', [TypeName]);
  
  RawDescriptionInfo := GetLastComment;
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
            ParseCIO(U, TypeName, CIO_CLASS, 
              RawDescriptionInfo, False);
            Exit;
          end;
        end;
      KEY_DISPINTERFACE: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_SPINTERFACE, 
            RawDescriptionInfo, False);
          Exit;
        end;
      KEY_INTERFACE: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_INTERFACE, 
            RawDescriptionInfo, False);
          Exit;
        end;
      KEY_OBJECT: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_OBJECT, 
            RawDescriptionInfo, False);
          Exit;
        end;
      KEY_RECORD: begin
          FreeAndNil(t);
          ParseCIO(U, TypeName, CIO_RECORD, 
            RawDescriptionInfo, False);
          Exit;
        end;
      KEY_PACKED: begin
          FreeAndNil(t);
          t := GetNextToken(LTemp);
          LCollected := LCollected + LTemp + t.Data;
          if t.IsKeyWord(KEY_RECORD) then 
          begin
            FreeAndNil(t);
            ParseCIO(U, TypeName, CIO_PACKEDRECORD, 
              RawDescriptionInfo, False);
            exit;
          end else if t.IsKeyWord(KEY_OBJECT) then
          begin
            FreeAndNil(t);
            ParseCIO(U, TypeName, CIO_PACKEDOBJECT, 
              RawDescriptionInfo, False);
            Exit;
          end else if t.IsKeyWord(KEY_CLASS) then
          begin
            // no check for "of", no packed classpointers allowed
            FreeAndNil(t);
            ParseCIO(U, TypeName, CIO_PACKEDCLASS, 
              RawDescriptionInfo, False);
            Exit;
          end;
        end;
    end;
  if Assigned(t) then begin
    if (t.MyType = TOK_KEYWORD) then begin
      if t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then 
      begin
        ParseCDFP(MethodType, '', t.Data, KeyWordToMethodType(t.Info.KeyWord), 
          RawDescriptionInfo, false, true);
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
      ParseEnum(EnumType, TypeName, RawDescriptionInfo);
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
  NormalType.RawDescriptionInfo^ := RawDescriptionInfo;
  U.AddType(NormalType);
  ItemsForNextBackComment.ClearAndAdd(NormalType);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUnit(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_UNIT);
  U.RawDescriptionInfo^ := GetLastComment;

  { get unit name identifier }
  U.Name := GetAndCheckNextToken(TOK_IDENTIFIER, true);

  ItemsForNextBackComment.ClearAndAdd(U);

  ParseHintDirectives(U);

  { skip semicolon }
  GetAndCheckNextToken(SYM_SEMICOLON);

  { get 'interface' keyword }
  GetAndCheckNextToken(KEY_INTERFACE);

  { now parse the interface section of that unit }
  ParseInterfaceSection(U);
end;

{ ---------------------------------------------------------------------------- }
procedure TParser.ParseProgramOrLibraryUses(U: TPasUnit);
var
  T: TToken;
begin
  U.RawDescriptionInfo^ := GetLastComment;

  { get program/library name identifier }
  U.Name := GetAndCheckNextToken(TOK_IDENTIFIER);

  ItemsForNextBackComment.ClearAndAdd(U);

  ParseHintDirectives(U);

  { skip semicolon }
  GetAndCheckNextToken(SYM_SEMICOLON);

  T := GetNextToken;
  try
    if T.IsKeyWord(KEY_USES) then
      ParseUses(U);
  finally
    FreeAndNil(T);
  end;
end;

procedure TParser.ParseProgram(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_PROGRAM);
  ParseProgramOrLibraryUses(U);
end;

procedure TParser.ParseLibrary(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_LIBRARY);
  ParseProgramOrLibraryUses(U);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUnitOrProgram(var U: TPasUnit);
var
  t: TToken;
begin
  U := TPasUnit.Create;
  try
    t := PeekNextToken;
    U.IsUnit := t.IsKeyWord(KEY_UNIT);
    if U.IsUnit then
      ParseUnit(U) else
      begin
        U.IsProgram := t.IsKeyWord(KEY_PROGRAM);
        if U.IsProgram  then
        begin
          ParseProgram(U);
        end
        else
        begin
          ParseLibrary(U);
        end;
      end;
  except
    FreeAndNil(U);
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUses(const U: TPasUnit);
var
  T: TToken;
begin
  { Parsing uses clause clears the comment, otherwise 
    - normal comments before "uses" clause would be assigned to normal unit
      items (like a procedure), which is quite unexpected
      (see ok_comment_over_uses_clause.pas testcase).
    - analogously, back comments after "uses" clause would be assigned to the unit
      description (see ok_comment_over_uses_clause_2.pas testcase).
  }
  IsLastComment := false;
  ItemsForNextBackComment.Clear;
  
  repeat
    U.UsesUnits.Append(GetAndCheckNextToken(TOK_IDENTIFIER, true));
    
    T := GetNextToken;
    try
      if T.IsKeyWord(KEY_IN) then
      begin
        FreeAndNil(T);
        
        { Below we just ignore the value of next string token.
        
          We can do this -- because PasDoc (at least for now) 
          does not recursively parse units on "uses" clause. 
          So we are not interested in the value of
          given string (which should be a file-name (usually relative,
          but absolute is also allowed AFAIK) with given unit.)
          
          If we will ever want to implement such "recursive parsing
          of units" in PasDoc, we will have to fix this to 
          *not* ignore value of token below. }
        GetAndCheckNextToken(TOK_STRING);
        
        T := GetNextToken;
      end;
    
      if T.IsSymbol(SYM_COMMA) then Continue else
      if T.IsSymbol(SYM_SEMICOLON) then Break else
        DoError('One of "," or ";" expected', []);
    finally
      FreeAndNil(T);
    end;
  until false;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCommaSeparatedIdentifiers(Names: TStrings;
  FinalSymbol: TSymbolType; 
  RawDescriptions: TRawDescriptionInfoList);
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
  OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean;
  const ClassKeyWordString: string = '');
  
  // The section allows PasDoc to parse variable modifiers in FPC.
  // See: http://www.freepascal.org/docs-html/ref/refse19.html
  // This consumes some tokens and appends to ItemCollector.FullDeclaration.
  procedure ParseVariableModifiers(ItemCollector: TPasFieldVariable);
  var
    Finished: Boolean;
    FirstCheck: boolean;
    ttemp: TToken;
  begin
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
  
var
  NewItem: TPasFieldVariable;
  ItemCollector: TPasFieldVariable;
  m: TPasMethod;
  t: TToken;
  NewItemNames: TStringList;
  I: Integer;
  RawDescriptions: TRawDescriptionInfoList;
  NewItems: TPasItems;
begin
  NewItemNames := nil;
  RawDescriptions := nil;
  NewItems := nil;
  try
    NewItemNames := TStringList.Create;
    RawDescriptions := TRawDescriptionInfoList.Create;
    NewItems := TPasItems.Create(false);
    
    ParseCommaSeparatedIdentifiers(NewItemNames, SYM_COLON, RawDescriptions);
    
    ItemCollector := TPasFieldVariable.Create;
    try
      ItemCollector.FullDeclaration := ':';
      
      t := GetNextToken(ItemCollector);

      { If symnbol is "(", we will later unget it and read it once again.
        This way SkipDeclaration can read type declaration up to the matching
        parenthesis, which is needed to handle fiels/var declarations with
        inline enumareted type, like
          var MyVar: (One, Two);
      }
      if not t.IsSymbol(SYM_LEFT_PARENTHESIS) then
      begin
        ItemCollector.FullDeclaration := ItemCollector.FullDeclaration + t.Data;
      end;

      if (t.MyType = TOK_KEYWORD) and
         (t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE]) then
      begin
        { MethodTypeString for ParseCDFP below is '', because we already included
          t.Data inside ItemCollector.FullDeclaration.
          If MethodTypeString would be t.Data, then we would incorrectly
          append t.Data twice to ItemCollector.FullDeclaration
          when appending m.FullDeclaration to ItemCollector.FullDeclaration.

          Note that param InitItemsForNextBackComment for ParseCDFP
          below is false. We will free M in the near time, and we don't
          want M to grab back-comment intended for our fields. }
        ParseCDFP(M, '', '', KeyWordToMethodType(t.Info.KeyWord),
          EmptyRawDescriptionInfo, false, false);
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
        ParseCIO(nil, '', CIO_RECORD, EmptyRawDescriptionInfo, IsInRecordCase);
      end else
      if t.IsKeyWord(KEY_PACKED) then
      begin 
        FreeAndNil(t);
        t := GetNextToken;
        if t.IsKeyWord(KEY_RECORD) then 
        begin
          ParseCIO(nil, '', CIO_PACKEDRECORD, EmptyRawDescriptionInfo, IsInRecordCase);
        end else 
        begin
          SkipDeclaration(ItemCollector, IsInRecordCase);
        end;      
      end else
      begin
        if t.IsSymbol(SYM_LEFT_PARENTHESIS) then
        begin
          Scanner.UnGetToken(t);
        end;
        SkipDeclaration(ItemCollector, IsInRecordCase);
      end;

      { Create and add (to Items and ItemsForNextBackComment and NewItems) 
        new items now.
        We must do it, because we want to init ItemsForNextBackComment *now*,
        not later (after parsing variable modifiers).
        Otherwise we could accidentaly "miss"
        some back-comment while searching for variable modifiers. 
        
        Note that when parsing variable modifiers, Get/PeekNextToken
        inside may actually use ItemsForNextBackComment and clear it,
        that's why we can't count on ItemsForNextBackComment to hold
        all our new items and we have to use NewItems. }
      if Items <> nil then
      begin
        ItemsForNextBackComment.Clear;
        for I := 0 to NewItemNames.Count - 1 do
        begin
          NewItem := TPasFieldVariable.Create;
          NewItem.Name := NewItemNames[I];
          NewItem.RawDescriptionInfo^ := RawDescriptions[I];
          NewItem.Visibility := Visibility;
          Items.Add(NewItem);
          NewItems.Add(NewItem);
          ItemsForNextBackComment.Add(NewItem);
        end;
      end;

      if not OfObject then
        ParseVariableModifiers(ItemCollector);

      { Now, when whole parsing work is finished, finish initializing NewItems. }
      if Items <> nil then
      begin
        for I := 0 to NewItems.Count - 1 do
        begin
          NewItem := NewItems[I] as TPasFieldVariable;
          NewItem.FullDeclaration := NewItem.Name + ItemCollector.FullDeclaration;
          if ClassKeyWordString <> '' then
          begin
            NewItem.FullDeclaration := ClassKeyWordString
              + ' ' + NewItem.FullDeclaration;
          end;
          NewItem.IsDeprecated := ItemCollector.IsDeprecated;
          NewItem.IsPlatformSpecific := ItemCollector.IsPlatformSpecific;
          NewItem.IsLibrarySpecific := ItemCollector.IsLibrarySpecific;
        end;
      end;
    finally
      ItemCollector.Free;
      FreeAndNil(t);
    end;
  finally 
    NewItemNames.Free;
    RawDescriptions.Free;
    NewItems.Free;
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

  { Returns the offset of the next line in string S after position Index }
  function FindNextLine(const S: string; Index: integer): integer;
  begin
    while SCharIs(S, Index, AllChars - WhiteSpaceNL) do Inc(Index);
    while SCharIs(S, Index, WhiteSpaceNL) do Inc(Index);
    if (Index > Length(S)) then
      Result := 0
    else
      Result := Index;
  end;

  { Checks whether string S starts with sub-string SubS at Index.
    Returns true is S starts with SubS, false otherwise. }
  function SStartsWith(const S: string; Index: integer; const SubS: string): boolean;
  var
    I: integer;
  begin
    Result := false;

    if Length(S) < Length(SubS) + Index - 1 then
      Exit;

    for I := 0 to Length(SubS)-1 do
    begin
      if S[Index + I] <> SubS[I+1] then
        Exit;
    end;
      
    Result := true;
  end;
  
  { Extracts the documentation comment from T.CommentContent
    (and some other T properties needed for TRawDescriptionInfo)
    to CommentInfo. Always T.MyType must be within TokenCommentTypes.

    T must not be nil.

    The comment is intended to be a "documentation comment",
    i.e. we intend to put it inside output documentation.
    So comment markers, if present, 
    are removed from the beginning and end of the data.
    Also, if comment markers were required but were not present,
    then CommentInfo.Content is an empty string.

    Also back-comment marker, the '<', is removed, if exists,
    and BackComment is set to @true. Otherwise BackComment is @false. }
  procedure ExtractDocComment(
    const t: TToken; out CommentInfo: TRawDescriptionInfo; 
    out BackComment: boolean);
  const
    BackCommentMarker = '<';
  var
    i: integer;
    Marker: string;
    WasMarker: boolean;
    CurPos: integer;
  begin
    BackComment := false;
    CommentInfo.Content := T.CommentContent;
    CommentInfo.StreamName := T.StreamName;
    CommentInfo.BeginPosition := T.BeginPosition;
    CommentInfo.EndPosition := T.EndPosition;

    if CommentMarkers.Count <> 0 then
    begin
      WasMarker := false;
      for i := 0 to CommentMarkers.Count - 1 do 
      begin
        Marker := CommentMarkers[i];
        if IsPrefix(Marker, CommentInfo.Content) then
        begin
          Delete(CommentInfo.Content, 1, Length(Marker));
          WasMarker := true;
          Break;
        end;
      end;

      if (not MarkersOptional) and (not WasMarker) then
      begin
        CommentInfo.Content := '';
        Exit;
      end;
    end;

    if SCharIs(CommentInfo.Content, 1, BackCommentMarker) then
    begin
      BackComment := true;
      Delete(CommentInfo.Content, 1, Length(BackCommentMarker));
    end;

    { Replace leading characters (Nothing to do for single-line (//) comments) }
    if (IgnoreLeading <> '') and
       (T.MyType in [TOK_COMMENT_PAS, TOK_COMMENT_EXT]) then
    begin
      CurPos := 1;
      repeat
        while SCharIs(CommentInfo.Content, CurPos, WhiteSpace) do Inc(CurPos);
        while SStartsWith(CommentInfo.Content, CurPos, IgnoreLeading) do
          Delete(CommentInfo.Content, CurPos, Length(IgnoreLeading));
        CurPos := FindNextLine(CommentInfo.Content, CurPos);
      until CurPos = 0;
    end;
  end;

var
  T: TToken;
  TBackComment, TIsCStyle: boolean;
  TCommentInfo: TRawDescriptionInfo;
  i: Integer;
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
      ExtractDocComment(T, TCommentInfo, TBackComment);
      TIsCStyle := t.MyType = TOK_COMMENT_CSTYLE;
      FreeAndNil(T);
      
      if TBackComment then
      begin
        if ItemsForNextBackComment.Count = 0 then
          DoMessage(1, pmtWarning,
            '%s: This is a back-comment (comment starting with "<") ' +
            'but there is no item declared right before it: "%s"', 
            [Scanner.GetStreamInfo, TCommentInfo.Content]);

        for i := 0 to ItemsForNextBackComment.Count - 1 do
        begin
          if ItemsForNextBackComment.PasItemAt[i].RawDescription <> '' then
            DoMessage(1, pmtWarning,
              '%s: Item %s already has one description, now it''s ' +
              'overriden by back-comment (comment starting with "<"): "%s"',
              [ Scanner.GetStreamInfo,
                ItemsForNextBackComment.PasItemAt[i].QualifiedName,
                TCommentInfo.Content]);
              
          ItemsForNextBackComment.PasItemAt[i].RawDescriptionInfo^ := TCommentInfo;
        end;
        
        ItemsForNextBackComment.Clear;
      end else
      if IsLastComment and LastCommentWasCStyle and TIsCStyle then
      begin
        { If there are several //-style comments in a row, combine them }
        LastCommentInfo.Content := LastCommentInfo.Content + 
          LineEnding + TCommentInfo.Content;
        if LastCommentInfo.StreamName = TCommentInfo.StreamName then
          LastCommentInfo.EndPosition := TCommentInfo.EndPosition else
          // ' ' is used to indicate that there is no
          // single stream containing the entire comment.
          LastCommentInfo.StreamName := ' ';
      end else
      begin
        { This is a normal comment, so fill [Is]LastCommentXxx properties }
        IsLastComment := true;
        LastCommentWasCStyle := TIsCStyle;
        LastCommentInfo := TCommentInfo;
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

{ TRawDescriptionInfoList --------------------------------------------------------------- }

function TRawDescriptionInfoList.GetItems(Index: integer): TRawDescriptionInfo;
begin
  { FItems is a dynarray, so compiler will automatically
    add appropriate range checks here in $R+ mode.
    So no need to explicitly check Index for validity here. }
  Result := FItems[Index];
end;

procedure TRawDescriptionInfoList.Grow;
var
  Delta: integer;
begin
  if Length(FItems) < 16 then begin
    Delta := 4;
  end
  else begin
    Delta := Length(FItems) div 4;
  end;
  SetLength(FItems, Length(FItems) + Delta);
end;

function TRawDescriptionInfoList.Append(Comment: TRawDescriptionInfo): integer;
begin
  if Length(FItems) = Count then Grow;
  FItems[Count] := Comment;
  result := Count;
  Inc(FCount);
end;

constructor TRawDescriptionInfoList.Create;
begin
  inherited;
  SetLength(FItems, 4);
  FCount := 0;
end;

end.