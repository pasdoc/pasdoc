{ @abstract(provides all the parsing functionality of pasdoc)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))

  Parsing implements most of the functionality of the pasdoc program.

  <P>It provides the @link(TParser) object, which scans the command line parameters
  for file names and switches and then starts collecting information from those
  files, issueing warnings to standard out if necessary. }

unit PasDoc_Parser;

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
    Will normally be created used @link(Init) with an input stream
    and a list of directives, then does the parsing work when its
    @link(ParseUnit) method is called.
    If no errors appear, should return a @link(TPasUnit) object with
    all information on the unit.
    Otherwise a description of the error should be found in
    @link(ErrorMessage). }
  TParser = class
    { Last comment found in input or nil if no comment available.
    Will be modified by @link(GetLastComment). }
    LastCommentToken: TToken;
    { The underlying scanner object. }
    Scanner: TScanner;
  private
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FStarOnly: boolean;
  protected
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    { Will write last comment that was found in input to T. If there was none,
      T will be set to nil. }
    function GetLastComment: string;
    { Get next token T from scanner that is neither whitespace nor comment.
      Return true on success. }
    function GetNextNonWCToken(var t: TToken): Boolean;
    function ParseArguments(var a: string): Boolean;
    { Parses a constructor, a destructor, a function or a procedure.
      Resulting PMethod item will be returned in M.
      CS may contain the 'class' keyword - its exact spelling is taken from
      this variable.
      CDFP contains the keyword constructor, destructor, function or procedure
      in the exact spelling as it was found in input.
      Key contains one of the KEY_xxx constants for the What field of the
      resulting method object.
      D may contain a description or nil. }
    function ParseCDFP(var M: TPasMethod; CS, CDFPS: string; Key: TKeyWord;
      d: string; const NeedName: boolean): Boolean;
    { Parses a class, an interface or an object.
      U is the unit this item will be added to on success.
      N is the name of this item.
      CIOType describes if item is class, interface or object.
      D may contain a description or nil. }
    function ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
      TCIOType; d: string): Boolean;
    { }
    function ParseRecordCase(const R: TPasCio; const SubCase: boolean = false): boolean;
    function ParseConstant(const U: TPasUnit; t: TToken): Boolean;
    function ParseInterfaceSection(const U: TPasUnit): Boolean;
    function ParseProperty(var p: TPasProperty): Boolean;
    function ParseType(const U: TPasUnit; var t: TToken): Boolean;
    function ParseUses(const U: TPasUnit): Boolean;
    function ParseVariables(const U: TPasUnit; var t: TToken): Boolean;
    function SkipDeclaration: Boolean;
    { Reads tokens and throws them away as long as they are either whitespace
      or comments. Returns true on success, false if there were any errors. }
    function SkipWhitespaceAndComments: Boolean;
  public
    { Create a parser, initialize the scanner with input stream S.
      All strings in SD are defined compiler directives. }
    constructor Create(
      const InputStream: TStream;
      const Directives: TStringVector;
      const IncludeFilePaths: TStringVector;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal);
    { Release all dynamically allocated memory. }
    destructor Destroy; override;
    function ParseUnit(out U: TPasUnit): Boolean;

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property StarStyleOnly: boolean read FStarOnly write FStarOnly default
      False;
  end;

implementation

uses
  SysUtils,

  Utils;

{ ---------------------------------------------------------------------------- }
{ TParser
{ ---------------------------------------------------------------------------- }

constructor TParser.Create(
  const InputStream: TStream;
  const Directives: TStringVector;
  const IncludeFilePaths: TStringVector;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  Scanner := TScanner.Create(InputStream, OnMessageEvent, VerbosityLevel);
  Scanner.AddDirectives(Directives);
  Scanner.IncludeFilePaths := IncludeFilePaths;
  StarStyleOnly := True;
end;

{ ---------------------------------------------------------------------------- }

destructor TParser.Destroy;
begin
  Scanner.Free;
  LastCommentToken.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Integer);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity < FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetLastComment: string;
var
  l: Integer;
  LBSComment: Integer;
begin
  LBSComment := 0;
  if Assigned(LastCommentToken) then begin
    Result := LastCommentToken.Data;
    LastCommentToken.Free;
    LastCommentToken := nil;

      { remove comment characters here }
    l := Length(Result);

    if (l > 0) and (Result[1] = '{') then begin
      Delete(Result, 1, 1);
      Dec(l);
    end;

    if (l > 0) and (Result[l] = '}') then begin
      Delete(Result, Length(Result), 1);
      Dec(l);
    end;

    if (l > 1) and (Result[1] = '/') and (Result[2] = '/') then begin
      Delete(Result, 1, 2);
      Exit;
    end;

    if (l > 1) and (Result[1] = '(') and (Result[2] = '*') then begin
      Delete(Result, 1, 2);
      Dec(l, 2);
      LBSComment := 1;
    end;

    if (l > 1) and (Result[l - 2] = '*') and (Result[l - 1] = ')') then
      Delete(Result, l - 1, 2);
  end
  else
    Result := '';

  if StarStyleOnly then begin
    if (Length(Result) < 2-LBSComment) or (Result[1] <> '*') or ((Result[2] <> '*') and (LBSComment=0)) then
      begin
      Result := '';
      exit;
    end else begin
      Delete(Result, 1, 2-LBSComment);
    end;
  end else begin
    if (Length(Result) >= 2-LBSComment) and (Result[1] = '*') and ((Result[2] = '*') or (LBSComment=1)) then
      begin
      Delete(Result, 1, 2-LBSComment);
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetNextNonWCToken(var t: TToken): Boolean;
begin
  Assert(t=nil);
  if SkipWhitespaceAndComments then begin
    Result := Scanner.GetToken(t)
  end else begin
    Result := False;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseArguments(var a: string): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  Finished := False;
  a := '';
  repeat
    if Scanner.GetToken(t) then begin
      if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
        SYM_RIGHT_PARENTHESIS) then begin
        Finished := True
      end else begin
        if (t.MyType = TOK_SYMBOL) and
          ((t.Info.SymbolType = SYM_COLON) or
          (t.Info.SymbolType = SYM_COMMA) or
          (t.Info.SymbolType = SYM_SEMICOLON)) then begin
          if (Length(a) > 0) and (a[Length(a)] = ' ') then
            SetLength(a, Length(a) - 1);
          a := a + t.Data;
        end
        else if (t.MyType = TOK_WHITESPACE) then begin
          if (Length(a) > 0) and (a[Length(a)] <> ' ') then a := a + ' ';
        end
        else if (t.MyType = TOK_COMMENT) or (t.MyType = TOK_RECTIVE) then
          begin
              { ignore }
        end
        else { otherwise copy }
          a := a + t.Data;
      end;
      FreeAndNil(t);
    end;
  until Finished;
  ParseArguments := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseCDFP(var M: TPasMethod; CS, CDFPS: string; Key:
  TKeyword; d: string; const NeedName: boolean): Boolean;
const
  SDSet: set of TStandardDirective =
  [SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
    SD_EXTERNAL, SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD,
    SD_OVERRIDE, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
    SD_DEPRECATED];
var
  Finished: Boolean;
  IsSemicolon: Boolean;
  pl: TStandardDirective;
  t: TToken;
  level: Integer;
begin
  Result := False;
  M := TPasMethod.Create;
  M.Description := d;
  t := nil;
  case Key of
    KEY_CONSTRUCTOR:
      M.What := METHOD_CONSTRUCTOR;
    KEY_DESTRUCTOR:
      M.What := METHOD_DESTRUCTOR;
    KEY_FUNCTION, KEY_PROCEDURE:
      M.What := METHOD_FUNCTION_PROCEDURE;
  else
    DoError('FATAL ERROR: CDFP got invalid key.', [], 1);
  end;

  { next non-wc token must be the name }
  if NeedName then begin
    if (not GetNextNonWCToken(t)) then begin
      M.Free;
      DoError('Could not get next non white space token', []);
    end;
    if (t.MyType <> TOK_IDENTIFIER) then begin
      M.Free;
      FreeAndNil(t);
      DoError('Could not get next identifier', []);
    end;
    if (Length(CS) > 0) then CS := CS + ' ';
    M.Name := t.Data;
    DoMessage(5, mtInformation, 'Parsing %s %s', [CDFPS, M.Name]);
    M.FullDeclaration := CS + CDFPS + ' ' + M.Name;
    FreeAndNil(t);
  end;

  { copy tokens until first semicolon with parenthesis level zero }
  level := 0;
  repeat
    if (not Scanner.GetToken(t)) then begin
      M.Free;
      DoError('Could not get next token', []);
    end;
    if (t.MyType = TOK_COMMENT) then
    else if (t.MyType = TOK_WHITESPACE) then begin
      if Length(M.FullDeclaration) > 0 then begin
        if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
          M.FullDeclaration := M.FullDeclaration + ' ';
      end;
    end
    else begin
      M.FullDeclaration := M.FullDeclaration + t.Data;
    end;
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_LEFT_PARENTHESIS)
      then Inc(level);
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_RIGHT_PARENTHESIS)
      then Dec(level);
    IsSemicolon := (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
      SYM_SEMICOLON);
    FreeAndNil(t);
  until IsSemicolon and (Level = 0);

  { first get non-WC token - if it is not an identifier in SD_SET put it back
    into stream and leave; otherwise copy tokens until semicolon }
  Finished := False;
  repeat
    FreeAndNil(t);
    if (not GetNextNonWCToken(t)) then begin
      M.Free;
      DoError('Could not get next non white space token', []);
    end;
    if (t.MyType <> TOK_IDENTIFIER) then begin
      Scanner.UnGetToken(t);
      Break;
    end;
    CS := t.Data;
    pl := StandardDirectiveByName(CS);
    if (pl = SD_INVALIDSTANDARDDIRECTIVE) or (not (pl in SDSet)) then begin
      Scanner.UnGetToken(t);
      Break;
    end;
    if (pl = SD_DEPRECATED) then begin
      M.IsDeprecated := True;
    end;
    M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
    FreeAndNil(t);

    { Apparently, the Delphi compiler does NOT enforce that
      directives must be separated and be terminated by a semicolon,
      even though Delphi help consistently uses them consistently.
      However, we take the compiler as a reference and try to mimic its behaviour. }
    if (not GetNextNonWCToken(t)) then begin
      M.Free;
      Exit;
    end;
    { Is current token a semicolon? }
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_SEMICOLON) then
      M.FullDeclaration := M.FullDeclaration + ';'
    else begin
      M.FullDeclaration := M.FullDeclaration + ' ';
      Scanner.UnGetToken(t);
    end;

  until Finished;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseCIO(const U: TPasUnit; const CioName: string; CIOType:
  TCIOType; d: string): Boolean;
var
  CS: string;
  CSFound: Boolean;
  f: TPasItem;
  Finished: Boolean;
  i: TPasCio;
  Ind: TStandardDirective;
  M: TPasMethod;
  p: TPasProperty;
  s: string;
  State: TAccessibility;
  t: TToken;
begin
  t := nil;
  Result := False;
  DoMessage(5, mtInformation, 'Parsing class/interface/object "%s"',
    [CioName]);
  if not GetNextNonWCToken(t) then Exit;

  { Test for forward class definition here:
      class MyClass = class;
    with no ancestor or class members listed after the word class. }
  if t.IsSymbol(SYM_SEMICOLON) then begin
    Result := True; // No error, continue the parsing.
    Exit;
  end;

  i := TPasCio.Create;
  i.Name := CioName;
  i.Description := d;
  i.MyType := CIOType;
  { get all ancestors; remember, this could look like
    TNewClass = class ( Classes.TClass, MyClasses.TFunkyClass, MoreClasses.YAC) ... end;
    All class ancestors are supposed to be included in the docs!
  }
  if t.IsSymbol(SYM_LEFT_PARENTHESIS) then begin
      { optional ancestor introduced by ( }
    FreeAndNil(t);
    Finished := False;
    i.Ancestors := NewStringVector;
      { outer repeat loop: one ancestor per pass }
    repeat
      FreeAndNil(t);
      if not GetNextNonWCToken(t) then Exit;
      if t.MyType = TOK_IDENTIFIER then begin { an ancestor }
        s := t.Data;
        FreeAndNil(t);
            { inner repeat loop: one part of the ancestor per name }
        repeat
          if not Scanner.GetToken(t) then begin
            Exit;
          end;
          if not t.IsSymbol(SYM_PERIOD) then begin
            Scanner.UnGetToken(t);
            t := nil;
            Break; { leave inner repeat loop }
          end;
          FreeAndNil(t);
          s := s + '.';
          if not Scanner.GetToken(t) or (t.MyType <> TOK_IDENTIFIER) then
            DoError('%s: expected class, object or interface in ancestor declaration.', [Scanner.GetStreamInfo]);

          s := s + t.Data;
        until False;
        i.Ancestors.Add(s);
      end else begin
        if (t.IsSymbol(SYM_COMMA)) then
            { comma, separating two ancestors } begin
          FreeAndNil(t);
        end else begin
          Finished := t.IsSymbol(SYM_RIGHT_PARENTHESIS);
          FreeAndNil(t);
          if not Finished then
            DoError('%s: Error - ")" expected.', [Scanner.GetStreamInfo]);
        end;
      end;
    until Finished;
  end else begin
    if (t.IsSymbol(SYM_LEFT_BRACKET)) then begin
      FreeAndNil(t);
      { for the time being, we throw away the ID itself }
      if (not GetNextNonWCToken(t)) then begin
        Exit;
      end;
      if (t.MyType <> TOK_STRING) then
        DoError('%s: Error - literal String as interface ID expected.',
          [Scanner.GetStreamInfo]);
      if not GetNextNonWCToken(t) then begin
        Exit;
      end;
      if not t.IsSymbol(SYM_RIGHT_BRACKET) then
        DoError('%s: Error - "]" expected.', [Scanner.GetStreamInfo]);
    end else begin
      Scanner.UnGetToken(t);
      if i.MyType = CIO_class then begin
        i.Ancestors := NewStringVector;
        i.Ancestors.Add('TObject');
      end;
    end;
  end;
  { now collect methods, fields and properties }
  CS := '';
  State := STATE_PUBLIC;
  Finished := False;
  repeat
    CSFound := False;
    if not GetNextNonWCToken(t) then begin
      i.Free;
      Exit;
    end;
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
        { A forward declaration of type "name = class(ancestor);" }
      FreeAndNil(t);
      U.AddCIO(i);
      Result := True;
      Exit;
    end
    else
      if (t.MyType = TOK_RESERVED) then
        case t.Info.ReservedKey of
          KEY_CLASS: begin
              CS := t.Data;
              CSFound := True;
            end;
          KEY_CONSTRUCTOR,
            KEY_DESTRUCTOR,
            KEY_FUNCTION,
            KEY_PROCEDURE: begin
              d := GetLastComment;
              if (not ParseCDFP(M, CS, t.Data, t.Info.ReservedKey, d, True))
                then begin
                i.Free;
                FreeAndNil(t);
                Exit;
              end;
              M.State := State;
              M.InsertMethod(M, i.Methods);
            end;
          KEY_END: Finished := True;
          KEY_PROPERTY: begin
              if (not ParseProperty(p)) then begin
                FreeAndNil(t);
                Exit;
              end;
              p.State := State;
              p.InsertProperty(p, i.Properties);
            end;
          KEY_CASE: begin
              if not ParseRecordCase(i) then begin
                FreeAndNil(t);
                Exit;
              end;
            end;
        else begin
            i.Free;
            try
              DoError('%s: Error, unexpected reserved keyword "%s".',
                [Scanner.GetStreamInfo, KeyWordArray[t.Info.ReservedKey]]);
            finally
              FreeAndNil(t);
            end;
          end;
        end
      else
        if (t.MyType = TOK_IDENTIFIER) then begin
          CS := t.Data;
          Ind := StandardDirectiveByName(CS);
          case Ind of
            SD_DEFAULT: begin
                if not SkipDeclaration then
                  DoError('%s: Could not skip declaration after default property.', [Scanner.GetStreamInfo]);
                DoMessage(5, mtInformation,
                  'Skipped default property keyword.', []);
              end;
            SD_PUBLIC: State := STATE_PUBLIC;
            SD_PUBLISHED: State := STATE_PUBLISHED;
            SD_PRIVATE: State := STATE_PRIVATE;
            SD_PROTECTED: State := STATE_PROTECTED;
            SD_AUTOMATED: State := STATE_PUBLIC;
          else
            Ind := SD_INVALIDSTANDARDDIRECTIVE;
          end;
          if (Ind = SD_INVALIDSTANDARDDIRECTIVE) then begin
            f := TPasItem.Create;
            f.Name := t.Data;
            f.State := State;
            f.Description := GetLastComment;
            if not SkipDeclaration then begin
              f.Free;
              Exit;
            end;

            f.InsertItem(f, i.Fields);
          end;
        end;
    if (not CSFound) then CS := '';
    FreeAndNil(t);
  until Finished;
  if (not GetNextNonWCToken(t)) or (not t.IsSymbol(SYM_SEMICOLON)) then begin
    i.Free;
    DoError('%s: Semicolon at the end of Class/Object/Interface expected.',
      [Scanner.GetStreamInfo]);
  end;
  FreeAndNil(t);
  U.AddCIO(i);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseConstant(const U: TPasUnit; t: TToken): Boolean;
var
  i: TPasItem;
begin
  Result := False;
  i := TPasItem.Create;
  i.Name := t.Data;
  DoMessage(5, mtInformation, 'Parsing constant %s.', [i.Name]);
  i.Description := GetLastComment;
  if SkipDeclaration then begin
    U.AddConstant(i);
    Result := True;
  end
  else
    DoError('Could not skip declaration of constant "%s".', [i.Name]);
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseInterfaceSection(const U: TPasUnit): Boolean;
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
  Result := False;
  Finished := False;
  Mode := MODE_UNDEFINED;
  t := nil;
  repeat
    if not GetNextNonWCToken(t) then
      DoError('Could not get next non-whitespace, non-comment token in file %s', [Scanner.GetStreamInfo]);

    case t.MyType of
      TOK_IDENTIFIER: begin
          // s := t.Data;
          case Mode of
            MODE_CONST:
              if (not ParseConstant(U, t)) then Exit;
            MODE_TYPE:
              if (not ParseType(U, t)) then Exit;
            MODE_VAR:
              if (not ParseVariables(U, t)) then Exit;
          else
            DoError('%s: Error, unexpected identifier "%s".',
              [Scanner.GetStreamInfo, t.Data]);
          end;
        end;
      TOK_RESERVED: begin
          case t.Info.ReservedKey of
            KEY_RESOURCESTRING,
              KEY_CONST:
              Mode := MODE_CONST;
            KEY_FUNCTION,
              KEY_PROCEDURE: begin
                d := GetLastComment;
                if (not ParseCDFP(M, '', t.Data, t.Info.ReservedKey, d, True))
                  then begin
                  Exit;
                end;
                M.InsertMethod(M, U.FuncsProcs);
                Mode := MODE_UNDEFINED;
              end;
            KEY_IMPLEMENTATION:
              Finished := True;
            KEY_TYPE:
              Mode := MODE_TYPE;
            KEY_USES:
              if not ParseUses(U) then Exit;
            KEY_THREADVAR,
              KEY_VAR:
              Mode := MODE_VAR;
          else
            DoError('%s: Unexpected keyword %s.', [Scanner.GetStreamInfo,
              t.Data]);
          end;
        end;
    end;
    if Assigned(t) then FreeAndNil(t);
  until Finished;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseProperty(var p: TPasProperty): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  t := nil;
  ParseProperty := False;
  if (not GetNextNonWCToken(t)) then Exit;
  if (t.MyType <> TOK_IDENTIFIER) then begin
    FreeAndNil(t);
    DoError('%s: expected identifier as property name.',
      [Scanner.GetStreamInfo]);
  end;
  p := TPasProperty.Create;
  p.Name := t.Data;
  DoMessage(5, mtInformation, 'Parsing property %s', [p.Name]);
  p.IndexDecl := '';
  p.Proptype := '';
  FreeAndNil(t);
  p.Description := GetLastComment;
  if (not GetNextNonWCToken(t)) then Exit;
  { get index }
  if (t.IsSymbol(SYM_LEFT_BRACKET)) then begin
    FreeAndNil(t);
    p.IndexDecl := '[';
    repeat
      if not Scanner.GetToken(t) then
        DoError('Error, could not parse property in file %s',
          [Scanner.GetStreamInfo]);

      if (t.MyType <> TOK_COMMENT) and (t.MyType <> TOK_RECTIVE) then
        p.IndexDecl := p.IndexDecl + t.Data;
      Finished := t.IsSymbol(SYM_RIGHT_BRACKET);
      FreeAndNil(t);
    until Finished;
      { next nonwc token should be the colon }
    if (not GetNextNonWCToken(t)) then
      Exit;
  end
  else begin
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
      p.FullDeclaration := p.Name + ';';
      FreeAndNil(t);
      ParseProperty := True;
      Exit;
    end;
  end;

  { now if there is a colon, it is followed by the type }
  if t.IsSymbol(SYM_COLON) then begin
      { get property type }
    FreeAndNil(t);
    if (not GetNextNonWCToken(t)) then Exit;
    if (t.MyType <> TOK_IDENTIFIER) and (t.MyType <> TOK_RESERVED) then
      DoError('Identifier expected, found %s in file %s',
        [TokenTypeNames[t.MyType], Scanner.GetStreamInfo]);

    p.Proptype := t.Data;
    FreeAndNil(t);
    p.FullDeclaration := p.Name + p.IndexDecl + ': ' + p.Proptype + ';';
  end
  else
    p.FullDeclaration := p.Name + ';';

  { simply skipping the rest of declaration }
  if not SkipDeclaration then
    DoError('Could not skip rest of declaration in file %s',
      [Scanner.GetStreamInfo]);

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

{
  TYPENAME =
    class of ... ;               => "normal" type
    class ( ANCESTOR<S> )        => class
          ANYTHING               => class
    object ( ) end ;                 => object
          ANYTHING
    interface end ;              => interface
}

function TParser.ParseRecordCase(const R: TPasCio;
  const SubCase: boolean): boolean;
var
  t1, t2: TToken;
  P: TPasItem;
  LLastWasComma: boolean;
begin
  Result := True;
  t1:=nil; t2:=nil;
  GetNextNonWCToken(t1);
  if t1.MyType <> TOK_IDENTIFIER then begin
    Result := False;
    FreeAndNil(t1);
  end else begin
    GetNextNonWCToken(t2);
    if (t2.MyType = TOK_SYMBOL) and (t2.Info.SymbolType = SYM_COLON) then begin
      // case x:Type of
      FreeAndNil(t2); // colon
      GetNextNonWCToken(t2);
      P := TPasItem.Create;
      p.Name := t1.Data;
      p.Description := GetLastComment;
      p.InsertItem(p, R.Fields);
    end else begin
      // case Type of
      Scanner.UnGetToken(t2);
    end;
    FreeAndNil(t2);
    FreeAndNil(t1);
    GetNextNonWCToken(t1);
    if (t1.MyType <> TOK_RESERVED) or (t1.Info.ReservedKey <> KEY_OF) then begin
      FreeAndNil(t1);
      DoError('OF expected',[],1);
    end;
    FreeAndNil(t1);
    GetNextNonWCToken(t1);
    repeat
      FreeAndNil(t1);
      GetNextNonWCToken(t1);
      if (t1.MyType <> TOK_SYMBOL) or (t1.Info.SymbolType <> SYM_COLON) then begin
        FreeAndNil(t1);
        DoError(': expected', [], 1);
      end;
      FreeAndNil(t1);
      GetNextNonWCToken(t1);
      if (t1.MyType <> TOK_SYMBOL) or (t1.Info.SymbolType <> SYM_LEFT_PARENTHESIS) then begin
        FreeAndNil(t1);
        DoError('( expected', [], 1);
      end;
      FreeAndNil(t1);
      GetNextNonWCToken(t1);
      while (t1.MyType <> TOK_SYMbol) or (T1.Info.SymbolType <> SYM_RIGHT_PARENTHESIS) do begin
        if t1.MyType = TOK_IDENTIFIER then begin
          P := TPasItem.Create;
          p.Description := GetLastComment;
          P.Name:=t1.Data;
          p.InsertItem(p, R.Fields);
          FreeAndNil(t1);
          GetNextNonWCToken(t1);
          LLastWasComma := false;
          while (t1.MyType <> TOK_SYMBOL) OR ((t1.Info.SymbolType <> SYM_SEMICOLON) and (t1.Info.SymbolType <> SYM_RIGHT_PARENTHESIS)) do begin
            if (t1.MyType = TOK_IDENTIFIER) and LLastWasComma then begin
              p := TPasItem.Create;
              p.Description := GetLastComment;
              p.Name := t1.data;
              p.InsertItem(p, R.Fields);
            end;
            LLastWasComma := false;
            if (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_COMMA) then begin
              LLastWasComma := True;
            end;
            FreeAndNil(t1);
            GetNextNonWCToken(t1);
          end;
          if t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS then begin
            Scanner.UnGetToken(t1);
          end;
        end else begin
          if (t1.MyType = TOK_RESERVED) and (t1.Info.ReservedKey = KEY_CASE) then begin
            ParseRecordCase(R, true);
          end else begin
            FreeAndNil(t1);
            DoError('Invalid keyword found',[],1);
          end;
        end;
        FreeAndNil(t1); // free token
        GetNextNonWCToken(t1);
      end;
      FreeAndNil(t1); // free ')' token
      GetNextNonWCToken(t1); // next
      if (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_SEMICOLON) then begin
        FreeAndNil(t1);
        GetNextNonWCToken(t1);
      end;
      if (t1.MyType = TOK_RESERVED) and (t1.Info.ReservedKey = KEY_END) then break;
      if subcase and (t1.MyType = TOK_SYMBOL) and (t1.Info.SymbolType = SYM_RIGHT_PARENTHESIS) then break;
    until false;
    Scanner.UnGetToken(t1);
  end;
end;

function TParser.ParseType(const U: TPasUnit; var t: TToken): Boolean;
var
  d: string;
  i: TPasItem;
  n: string;
begin
  Result := False;
  n := t.Data;
  DoMessage(5, mtInformation, 'Parsing type "%s"', [n]);
  FreeAndNil(t);
  d := GetLastComment;
  if (not GetNextNonWCToken(t)) then
    Exit;

  if (not t.IsSymbol(SYM_EQUAL)) then begin
    if (t.IsSymbol(SYM_SEMICOLON)) then begin
      FreeAndNil(t);
      t := nil;
      Result := True;
      Exit;
    end;
    FreeAndNil(t);
    DoError('"=" expected in file %s', [Scanner.GetStreamInfo]);
  end;

  FreeAndNil(t);
  if (not GetNextNonWCToken(t)) then
    Exit;

  if (t.MyType = TOK_RESERVED) then
    case t.Info.ReservedKey of
      KEY_CLASS: begin
          FreeAndNil(t);
          if (not GetNextNonWCToken(t)) then Exit;
          if (t.MyType = TOK_RESERVED) and (t.Info.ReservedKey = KEY_OF) then
            begin
              { include "identifier = class of something;" as standard type }
          end else begin
            Scanner.UnGetToken(t);
            t := nil;
            if not ParseCIO(U, n, CIO_CLASS, d) then Exit;
            Result := True;
            Exit;
          end;
        end;
      KEY_SPINTERFACE: begin
          if not ParseCIO(U, n, CIO_SPINTERFACE, d) then Exit;
          FreeAndNil(t);
          t := nil;
          Result := True;
          Exit;
        end;
      KEY_INTERFACE: begin
          if not ParseCIO(U, n, CIO_INTERFACE, d) then Exit;
          FreeAndNil(t);
          t := nil;
          Result := True;
          Exit;
        end;
      KEY_OBJECT: begin
          if not ParseCIO(U, n, CIO_OBJECT, d) then Exit;
          FreeAndNil(t);
          t := nil;
          Result := True;
          Exit;
        end;
      KEY_RECORD: begin
          if not ParseCIO(U, n, CIO_RECORD, d) then Exit;
          FreeAndNil(t);
          t := nil;
          Result := True;
          Exit;
        end;
    end;

  Scanner.UnGetToken(t);

  t := nil; { so that calling function will not try to dispose of it }
  if (not SkipDeclaration) then
    Exit;

  i := TPasItem.Create;
  i.Name := n;
  i.Description := d;
  U.AddType(i);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseUnit(out U: TPasUnit): Boolean;
var
  t: TToken;
begin
  t := nil;
  Result := False;
  { get 'unit' keyword }
  if not GetNextNonWCToken(t) then Exit;
  if (t.MyType <> TOK_RESERVED) or (t.Info.ReservedKey <> KEY_UNIT) then
    DoError(Scanner.GetStreamInfo + ': keyword "unit" expected.', []);
  
  FreeAndNil(t);

  U := TPasUnit.Create;
  U.Description := GetLastComment;
  if not GetNextNonWCToken(t) then Exit;

  { get unit name identifier }
  if t.MyType <> TOK_IDENTIFIER then
    DoError(Scanner.GetStreamInfo + ': identifier (unit name) expected.',
      []);
  U.Name := t.Data;
  FreeAndNil(t);
  { skip semicolon }
  if not GetNextNonWCToken(t) then Exit;
  if not t.IsSymbol(SYM_SEMICOLON) then
    DoError(Scanner.GetStreamInfo + ': semicolon expected.', []);
  FreeAndNil(t);
  if not GetNextNonWCToken(t) then Exit;

  { get 'interface' keyword }
  if (t.MyType <> TOK_RESERVED) or (t.Info.ReservedKey <> KEY_INTERFACE) then
    DoError(Scanner.GetStreamInfo + ': keyword "INTERFACE" expected.', []);
  { now parse the interface section of that unit }
  Result := ParseInterfaceSection(U);
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseUses(const U: TPasUnit): Boolean;
var
  Finished: Boolean;
  t: TToken;
begin
  t := nil;
  Result := False;

  if U.UsesUnits = nil then U.UsesUnits := NewStringVector;

  repeat
    if not GetNextNonWCToken(t) then Exit;
    if t.MyType <> TOK_IDENTIFIER then
      DoError('%s: Error, unit name expected (found %s, %s.',
        [Scanner.GetStreamInfo, t.GetTypeName, t.Data]);
    U.UsesUnits.Add(t.Data);
    FreeAndNil(t);
    if not GetNextNonWCToken(t) then Exit;
    if (t.MyType <> TOK_SYMBOL) and
      (t.Info.SymbolType <> SYM_COMMA) and
      (t.Info.SymbolType <> SYM_SEMICOLON) then
      DoError('%s: Error, comma or semicolon expected.',
        [Scanner.GetStreamInfo]);

    Finished := t.Info.SymbolType = SYM_SEMICOLON;
    FreeAndNil(t);
  until Finished;

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.ParseVariables(const U: TPasUnit; var t: TToken): Boolean;
var
  Finished: Boolean;
  FirstLoop: Boolean;
  i: TPasItem;
  m: TPasMethod;
begin
  Result := False;
  FirstLoop := True;
  repeat
    i := TPasItem.Create;
    if FirstLoop then begin
      i.Name := t.Data;
      FirstLoop := False;
    end
    else begin
      if (not GetNextNonWCToken(t)) then Exit;
      if (t.MyType <> TOK_IDENTIFIER) then
        DoError('%s: Identifier expected.', [Scanner.GetStreamInfo]);
      i.Name := t.Data;
    end;
    i.Description := GetLastComment;
    U.AddVariable(i);
    FreeAndNil(t);
    if (not GetNextNonWCToken(t)) then Exit;
    if (t.MyType <> TOK_SYMBOL) or
      ((t.Info.SymbolType <> SYM_COMMA) and
      (t.Info.SymbolType <> SYM_COLON)) then
      DoError('%s: Expected comma or colon in var declaration.',
        [Scanner.GetStreamInfo]);

    Finished := (t.Info.SymbolType = SYM_COLON);
    FreeAndNil(t);
    t := nil;
  until Finished;
  GetNextNonWCToken(t);
  if (t.MyType = TOK_RESERVED) and (t.Info.ReservedKey in [KEY_FUNCTION,
    KEY_PROCEDURE]) then begin
    ParseCDFP(m, '', t.Data, t.Info.ReservedKey, '', False);
    m.Free;
    FreeAndNil(t);
    GetNextNonWCToken(t);
    if (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType = SYM_EQUAL) then begin
      SkipDeclaration;
    end else begin
      Scanner.UnGetToken(t);
    end;
  end else begin
    if not SkipDeclaration then Exit;
  end;

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.SkipDeclaration: Boolean;
var
  EndLevel: Integer;
  IsSemicolon: Boolean;
  PLevel: Integer;
  t: TToken;
begin
  Result := False;
  EndLevel := 0;
  PLevel := 0;
  t := nil;
  repeat
    if not GetNextNonWCToken(t) then Exit;
    case t.MyType of
      TOK_SYMBOL:
        case t.Info.SymbolType of
          SYM_LEFT_PARENTHESIS: Inc(PLevel);
          SYM_RIGHT_PARENTHESIS: Dec(PLevel);
        end;
      TOK_RESERVED:
        case t.Info.ReservedKey of
          KEY_END: Dec(EndLevel);
          KEY_RECORD: Inc(EndLevel);
        end;
    end;
    IsSemicolon := (t.MyType = TOK_SYMBOL) and (t.Info.SymbolType =
      SYM_SEMICOLON);
    FreeAndNil(t);
  until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TParser.SkipWhitespaceAndComments: Boolean;
var
  t: TToken;
begin
  Result := False;
  t := nil;
  repeat
    if not Scanner.PeekToken(t) then break;
    if t.MyType = TOK_WHITESPACE then begin
      Scanner.ConsumeToken;
      FreeAndNil(t);
    end else begin
      if t.MyType = TOK_COMMENT then begin
        Scanner.ConsumeToken;
        if Assigned(LastCommentToken) then LastCommentToken.Free;
        LastCommentToken := t;
        t := nil;
      end else begin
        Result := True;
        break;
      end;
    end;
  until False;
end;

end.
