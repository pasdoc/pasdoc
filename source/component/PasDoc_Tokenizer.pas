{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michalis Kamburelis)
  @abstract(Provides simplified Pascal tokenizer.)

The @link(TTokenizer) object creates @link(TToken) objects (tokens) for the
Pascal programming language from a character input stream.

The @link(PasDoc_Scanner) unit does the same (it actually uses this unit's
tokenizer), with the exception that it evaluates compiler directives,
which are comments that start with a dollar sign. }

unit PasDoc_Tokenizer;

{$I DEFINES.INC}

interface

uses
  Utils,
  PasDoc_Types,
  Classes;

type
  { enumeration type that provides all types of tokens; each token's name
    starts with TOK_.
    
    TOK_DIRECTIVE is a compiler directive (like $ifdef, $define). 
    
    Note that tokenizer is not able to tell whether you used
    standard directive (e.g. 'Register') as an identifier
    (e.g. you're declaring procedure named 'Register')
    or as a real standard directive (e.g. a calling specifier 'register').
    So there is @italic(no) value like TOK_STANDARD_DIRECTIVE here,
    standard directives are always reported as TOK_IDENTIFIER.
    You can check TToken.Info.StandardDirective to know whether
    this identifier is @italic(maybe) used as real standard directive. }
  TTokenType = (TOK_WHITESPACE, TOK_COMMENT_PAS, TOK_COMMENT_EXT, 
                TOK_COMMENT_CSTYLE, TOK_IDENTIFIER, TOK_NUMBER, 
                TOK_STRING, TOK_SYMBOL, TOK_DIRECTIVE, TOK_KEYWORD);

type
  TKeyword = (
    KEY_INVALIDKEYWORD,
    KEY_AND,
    KEY_ARRAY,
    KEY_AS,
    KEY_ASM,
    KEY_BEGIN,
    KEY_CASE,
    KEY_CLASS,
    KEY_CONST,
    KEY_CONSTRUCTOR,
    KEY_DESTRUCTOR,
    KEY_DISPINTERFACE,
    KEY_DIV,
    KEY_DO,
    KEY_DOWNTO,
    KEY_ELSE,
    KEY_END,
    KEY_EXCEPT,
    KEY_EXPORTS,
    KEY_FILE,
    KEY_FINALIZATION,
    KEY_FINALLY,
    KEY_FOR,
    KEY_FUNCTION,
    KEY_GOTO,
    KEY_IF,
    KEY_IMPLEMENTATION,
    KEY_IN,
    KEY_INHERITED,
    KEY_INITIALIZATION,
    KEY_INLINE,
    KEY_INTERFACE,
    KEY_IS,
    KEY_LABEL,
    KEY_LIBRARY,
    KEY_MOD,
    KEY_NIL,
    KEY_NOT,
    KEY_OBJECT,
    KEY_OF,
    KEY_ON,
    KEY_OR,
    KEY_OPERATOR,
    KEY_PACKED,
    KEY_PROCEDURE,
    KEY_PROGRAM,
    KEY_PROPERTY,
    KEY_RAISE,
    KEY_RECORD,
    KEY_REPEAT,
    KEY_RESOURCESTRING,
    KEY_SET,
    KEY_SHL,
    KEY_SHR,
    KEY_STRING,
    KEY_THEN,
    KEY_THREADVAR,
    KEY_TO,
    KEY_TRY,
    KEY_TYPE,
    KEY_UNIT,
    KEY_UNTIL,
    KEY_USES,
    KEY_VAR,
    KEY_WHILE,
    KEY_WITH,
    KEY_XOR);

  TStandardDirective = (
    SD_INVALIDSTANDARDDIRECTIVE,
    SD_ABSOLUTE,
    SD_ABSTRACT,
    SD_APIENTRY,
    SD_ASSEMBLER,
    SD_AUTOMATED,
    SD_CDECL,
    SD_CVAR,
    SD_DEFAULT,
    SD_SPID,
    SD_DYNAMIC,
    SD_EXPORT,
    SD_EXTERNAL,
    SD_FAR,
    SD_FORWARD,
    SD_INDEX,
    SD_INLINE,
    SD_MESSAGE,
    SD_NAME,
    SD_NEAR,
    SD_NODEFAULT,
    SD_OUT,
    SD_OVERLOAD,
    SD_OVERRIDE,
    SD_PASCAL,
    SD_PRIVATE,
    SD_PROTECTED,
    SD_PUBLIC,
    SD_PUBLISHED,
    SD_READ,
    SD_REGISTER,
    SD_REINTRODUCE,
    SD_RESIDENT,
    SD_STDCALL,
    SD_STORED,
    SD_VIRTUAL,
    SD_WRITE,
    SD_DEPRECATED,
    SD_SAFECALL,
    SD_PLATFORM,
    SD_VARARGS);

const
  { Names of the token types. All start with lower letter.
    They should somehow describe (in a few short words) given 
    TTokenType. }
  TOKEN_TYPE_NAMES: array[TTokenType] of string =
  ( 'whitespace', 'comment ((**)-style)', 'comment ({}-style)', 
    'comment (//-style)', 'identifier', 'number', 'string', 'symbol', 
    'directive', 'reserved word');

  TokenCommentTypes: set of TTokenType = 
  [ TOK_COMMENT_PAS, TOK_COMMENT_EXT, TOK_COMMENT_CSTYLE ];

type
  { enumeration type that provides all types of symbols; each
    symbol's name starts with SYM_ }
  TSymbolType = (SYM_PLUS, SYM_MINUS, SYM_ASTERISK, SYM_SLASH, SYM_EQUAL,
    SYM_LESS_THAN, SYM_LESS_THAN_EQUAL, SYM_GREATER_THAN,
    SYM_GREATER_THAN_EQUAL, SYM_LEFT_BRACKET, SYM_RIGHT_BRACKET,
    SYM_COMMA, SYM_LEFT_PARENTHESIS, SYM_RIGHT_PARENTHESIS, SYM_COLON,
    SYM_SEMICOLON, SYM_ROOF, SYM_PERIOD, SYM_AT, 
    SYM_DOLLAR, SYM_ASSIGN, SYM_RANGE, SYM_POWER,
    { SYM_BACKSLASH may occur when writing char constant "^\",
      see ../../tests/ok_caret_character.pas }
    SYM_BACKSLASH);

const
  { Symbols as strings. They can be useful to have some mapping
    TSymbolType -> string, but remember that actually some symbols
    in tokenizer have multiple possible representations,
    e.g. "right bracket" is usually given as "]" but can also 
    be written as ".)". }
  SymbolNames: array[TSymbolType] of string = 
  ( '+', '-', '*', '/', '=', '<', '<=', '>', '>=', '[', ']', ',',
    '(', ')', ':', ';', '^', '.', '@', '$', ':=', '..', '**', '\' );

type
  { Stores the exact type and additional information on one token. }
  TToken = class(TObject)
  private
    FEndPosition: Int64;
    FBeginPosition: Int64;
    FStreamName: string;
  public
    { the exact character representation of this token as it was found in the
      input file }
    Data: string;
    
    { the type of this token as @link(TTokenType) }
    MyType: TTokenType;
    
    { additional information on this token as a variant record depending 
      on the token's MyType }
    Info: record
      case TTokenType of
        TOK_SYMBOL: 
          (SymbolType: TSymbolType);
        TOK_KEYWORD: 
          (KeyWord: TKeyWord);
        TOK_IDENTIFIER: 
          (StandardDirective: TStandardDirective);
    end;

    { Contents of a comment token.
      This is defined only when MyType is in TokenCommentTypes
      or is TOK_DIRECTIVE.
      This is the text within the comment @italic(without) comment delimiters. 
      For TOK_DIRECTIVE you can safely assume that CommentContent[1] = '$'. }
    CommentContent: string;
    
    { Create a token of and assign the argument token type to @link(MyType) }
    constructor Create(const TT: TTokenType);
    function GetTypeName: string;
    
    { Does @link(MyType) is TOK_SYMBOL and Info.SymbolType is ASymbolType ? }
    function IsSymbol(const ASymbolType: TSymbolType): Boolean;
    
    { Does @link(MyType) is TOK_KEYWORD and Info.KeyWord is AKeyWord ? }
    function IsKeyWord(const AKeyWord: TKeyWord): Boolean;
    
    { Does @link(MyType) is TOK_IDENTIFIER and Info.StandardDirective is
      AStandardDirective ? }
    function IsStandardDirective(
      const AStandardDirective: TStandardDirective): Boolean;
      
    { Few words long description of this token.
      Describes MyType and Data (for those tokens that tend to have short Data).
      Starts with lower letter. }
    function Description: string;
    
    // @name is the name of the TStream from which this @classname was read.
    // It is currently used to set @link(TRawDescriptionInfo.StreamName).
    property StreamName: string read FStreamName;
    
    // @name is the position in the stream of the start of the token.
    // It is currently used to set @link(TRawDescriptionInfo.BeginPosition).
    property BeginPosition: Int64 read FBeginPosition;
    
    // @name is the position in the stream of the character immediately
    // after the end of the token.
    // It is currently used to set @link(TRawDescriptionInfo.EndPosition).
    property EndPosition: Int64 read FEndPosition;
  end;

  { @abstract(Converts an input TStream to a sequence of @link(TToken) objects.) }
  TTokenizer = class(TObject)
  private
    function StreamPosition: Int64;
  protected
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    { if @link(IsCharBuffered) is true, this field contains the buffered
      character }
    BufferedChar: Char;
    { true if end of stream @link(Stream) has been reached, false otherwise }
    EOS: Boolean;
    { if this is true, @link(BufferedChar) contains a buffered character;
      the next call to @link(GetChar) or @link(PeekChar) will return this
      character, not the next in the associated stream @link(Stream) }
    IsCharBuffered: Boolean;
    { current row in stream @link(Stream); useful when giving error messages }
    Row: Integer;
    { the input stream this tokenizer is working on }
    Stream: TStream;
    FStreamName: string;
    
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    procedure CheckForDirective(const t: TToken);
    procedure ConsumeChar;
    
    function CreateSymbolToken(const st: TSymbolType; const s: string): 
      TToken; overload;
      
    { Uses default symbol representation, from SymbolNames[st] }
    function CreateSymbolToken(const st: TSymbolType): TToken; overload;
    
    function GetChar(out c: Char): Boolean;
    function PeekChar(out c: Char): Boolean;
    function ReadCommentType1: TToken;
    function ReadCommentType2: TToken;
    function ReadCommentType3: TToken;
    function ReadLiteralString(var t: TToken): Boolean;
    function ReadToken(c: Char; const s: TCharSet; const TT: TTokenType; var
      t: TToken): Boolean;

  public
    { Creates a TTokenizer and associates it with given input TStream. 
      Note that AStream will be freed when this object will be freed. }
    constructor Create(
      const AStream: TStream;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName: string);
    { Releases all dynamically allocated memory. }
    destructor Destroy; override;
    function HasData: Boolean;
    function GetStreamInfo: string;
    function GetToken: TToken;
    { Skips all chars until it encounters either $ELSE or $ENDIF compiler defines. }
    function SkipUntilCompilerDirective: TToken;
    
    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property Verbosity: Cardinal read FVerbosity write FVerbosity;
    property StreamName: string read FStreamName;
  end;

const
  { all Object Pascal keywords }
  KeyWordArray: array[Low(TKeyword)..High(TKeyword)] of string =
  ('x', // lowercase never matches
    'AND', 'ARRAY', 'AS', 'ASM', 'BEGIN', 'CASE', 'CLASS', 'CONST',
    'CONSTRUCTOR', 'DESTRUCTOR', 'DISPINTERFACE', 'DIV',  'DO', 'DOWNTO',
    'ELSE', 'END', 'EXCEPT', 'EXPORTS', 'FILE', 'FINALIZATION',
    'FINALLY', 'FOR', 'FUNCTION', 'GOTO', 'IF', 'IMPLEMENTATION',
    'IN', 'INHERITED', 'INITIALIZATION', 'INLINE', 'INTERFACE',
    'IS', 'LABEL', 'LIBRARY', 'MOD', 'NIL', 'NOT', 'OBJECT', 'OF',
    'ON', 'OR', 'OPERATOR', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING', 'SET', 'SHL',
    'SHR', 'STRING', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES', 'VAR', 'WHILE', 'WITH', 'XOR');

  { Object Pascal directives }
  StandardDirectiveArray:
    array[Low(TStandardDirective)..High(TStandardDirective)] of PChar =
  ('x', // lowercase letters never match
    'ABSOLUTE', 'ABSTRACT', 'APIENTRY', 'ASSEMBLER', 'AUTOMATED',
    'CDECL', 'CVAR', 'DEFAULT', 'SPID', 'DYNAMIC', 'EXPORT', 'EXTERNAL',
    'FAR', 'FORWARD', 'INDEX', 'INLINE', 'MESSAGE', 'NAME', 'NEAR',
    'NODEFAULT', 'OUT', 'OVERLOAD', 'OVERRIDE', 'PASCAL', 'PRIVATE',
    'PROTECTED', 'PUBLIC', 'PUBLISHED', 'READ', 'REGISTER',
    'REINTRODUCE', 'RESIDENT', 'STDCALL', 'STORED', 'VIRTUAL',
    'WRITE', 'DEPRECATED', 'SAFECALL', 'PLATFORM', 'VARARGS');

{ Checks is Name (case ignored) some Pascal keyword.
  Returns SD_INVALIDSTANDARDDIRECTIVE if not. }
function StandardDirectiveByName(const Name: string): TStandardDirective;

{ Checks is Name (case ignored) some Pascal standard directive.
  Returns KEY_INVALIDKEYWORD if not. }
function KeyWordByName(const Name: string): TKeyword;

implementation

uses
  SysUtils;

function KeyWordByName(const Name: string): TKeyword;
var
  LName: string;
  i: TKeyword;
begin
  LName := UpperCase(Name);
  Result := KEY_INVALIDKEYWORD;
  for i := Low(TKeyword) to High(TKeyword) do begin
    if LName = KeyWordArray[i] then begin
      Result := i;
      break;
    end;
  end;
end;

function StandardDirectiveByName(const Name: string): TStandardDirective;
var
  LName: string;
  i: TStandardDirective;
begin
  LName := UpperCase(Name);
  Result := SD_INVALIDSTANDARDDIRECTIVE;
  for i := Low(TStandardDirective) to High(TStandardDirective) do begin
    if LName = StandardDirectiveArray[i] then begin
      Result := i;
      break;
    end;
  end;
end;

const
  Whitespace = [#9, #10, #13, ' '];
  Letters = ['A'..'Z', 'a'..'z'];
  DecimalDigits = ['0'..'9'];
  HexadecimalDigits = DecimalDigits + ['A'..'F', 'a'..'f'];
  IdentifierStart = ['_'] + Letters;
  IdentifierOther = IdentifierStart + DecimalDigits;
  CharOther = HexadecimalDigits + ['$'];
  NumberStart = DecimalDigits + ['$'];
  NumberOther = HexadecimalDigits + ['.', '+', '-'];
  QuoteChar = '''';
  NUM_SINGLE_CHAR_SYMBOLS = 10;
  SingleCharSymbols: array[0..NUM_SINGLE_CHAR_SYMBOLS - 1] of
  record
    c: Char;
    s: TSymbolType;
  end =
  ((c: ';'; s: SYM_SEMICOLON),
    (c: ','; s: SYM_COMMA),
    (c: '['; s: SYM_LEFT_BRACKET),
    (c: ']'; s: SYM_RIGHT_BRACKET),
    (c: '+'; s: SYM_PLUS),
    (c: '-'; s: SYM_MINUS),
    (c: '*'; s: SYM_ASTERISK),
    (c: '='; s: SYM_EQUAL),
    (c: '^'; s: SYM_ROOF),
    (c: '@'; s: SYM_AT));

{ ---------------------------------------------------------------------------- }
{ TToken }
{ ---------------------------------------------------------------------------- }

constructor TToken.Create(const TT: TTokenType);
begin
  inherited Create;
  MyType := TT;
end;

{ ---------------------------------------------------------------------------- }

function TToken.GetTypeName: string;
begin
  GetTypeName := TOKEN_TYPE_NAMES[MyType];
end;

{ ---------------------------------------------------------------------------- }

function TToken.IsSymbol(const ASymbolType: TSymbolType): Boolean;
begin
  Result := (MyType = TOK_SYMBOL) and (Info.SymbolType = ASymbolType);
end;

function TToken.IsKeyWord(const AKeyWord: TKeyWord): Boolean;
begin
  Result := (MyType = TOK_KEYWORD) and (Info.KeyWord = AKeyWord);
end;

function TToken.IsStandardDirective(
  const AStandardDirective: TStandardDirective): Boolean;
begin
  Result := (MyType = TOK_IDENTIFIER) and 
    (Info.StandardDirective = AStandardDirective);
end;

function TToken.Description: string;
begin
  Result := TOKEN_TYPE_NAMES[MyType];
  if MyType in [TOK_SYMBOL, TOK_KEYWORD, TOK_IDENTIFIER] then
    Result := Result + ' "' + Data + '"';
end;

{ ---------------------------------------------------------------------------- }
{ TTokenizer }
{ ---------------------------------------------------------------------------- }

constructor TTokenizer.Create(
  const AStream: TStream;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName: string);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;
  Row := 1;
  Stream := AStream;
  FStreamName := AStreamName;
end;

{ ---------------------------------------------------------------------------- }

destructor TTokenizer.Destroy;
begin
  Stream.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.CheckForDirective(const t: TToken);
begin
  if SCharIs(T.CommentContent, 1, '$') then
    t.MyType := TOK_DIRECTIVE;
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.ConsumeChar;
begin
  IsCharBuffered := False;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.CreateSymbolToken(const st: TSymbolType; 
  const s: string): TToken;
begin
  Result := TToken.Create(TOK_SYMBOL);
  with Result do begin
    Info.SymbolType := st;
    Data := s;
  end;
end;

function TTokenizer.CreateSymbolToken(const st: TSymbolType): TToken;
begin
  Result := CreateSymbolToken(st, SymbolNames[st]);
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.DoError(const AMessage: string; const AArguments: array
  of const; const AExitCode: Word);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity < FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.GetChar(out c: Char): Boolean;
begin
  if IsCharBuffered then begin
    c := BufferedChar;
    IsCharBuffered := False;
    Result := True;
  end
  else begin
    Result := Stream.Position < Stream.Size;
    if Result then begin
      Stream.Read(c, 1);
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.GetStreamInfo: string;
begin
  GetStreamInfo := FStreamName + '(' + IntToStr(Row) + ')';
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.HasData: Boolean;
begin
  HasData := IsCharBuffered or (Stream.Position < Stream.Size);
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.StreamPosition: Int64;
begin
  if IsCharBuffered then
    Result := Stream.Position - 1 else
    Result := Stream.Position;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.GetToken: TToken;
var
  c: Char;
  MaybeKeyword: TKeyword;
  s: string;
  J: Integer;
  BeginPosition: integer;
begin
  Result := nil;
  try
    BeginPosition := StreamPosition;

    if not GetChar(c) then
      DoError('Error: tokenizer: could not read character', [], 0);
    
    if c in Whitespace then
    begin
      if ReadToken(c, Whitespace, TOK_WHITESPACE, Result) then
          { after successful reading all whitespace characters, update
            internal row counter to be able to state current row on errors;
            TODO: will fail on Mac files (row is 13) }
        Inc(Row, StrCountCharA(Result.Data, #10))
      else
        DoError('Error: tokenizer: could not read character', [], 0);
    end else
    if c in IdentifierStart then 
    begin
      if ReadToken(c, IdentifierOther, TOK_IDENTIFIER, Result) then 
      begin
        s := Result.Data;
        { check if identifier is a keyword }
        MaybeKeyword := KeyWordByName(s);
        if (MaybeKeyword <> KEY_INVALIDKEYWORD) then
        begin
          Result.MyType := TOK_KEYWORD;
          Result.Info.KeyWord := MaybeKeyword;
        end else
        begin
          { calculate Result.Info.StandardDirective }
          Result.Info.StandardDirective := StandardDirectiveByName(s);
        end;
      end;
    end else
    if c in NumberStart then
      ReadToken(c, NumberOther, TOK_NUMBER, Result)
    else
      case c of
        QuoteChar:
          ReadLiteralString(Result);
        '#':
          ReadToken(c, CharOther, TOK_STRING, Result);
        '{': begin
            Result := ReadCommentType1;
            CheckForDirective(Result);
          end;
        '(': begin
            c := ' ';
            if HasData and not PeekChar(c) then DoError('Error: tokenizer: could not read character.', [], 0);
            case c of
              '*': begin
                  ConsumeChar;
                  Result := ReadCommentType2;
                  CheckForDirective(Result);
                end;
              '.': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_LEFT_BRACKET, '(.');
                end;
            else
              Result := CreateSymbolToken(SYM_LEFT_PARENTHESIS);
            end;
          end;
        ')': begin
            c := ' ';
            Result := CreateSymbolToken(SYM_RIGHT_PARENTHESIS);
          end;
        '.': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '.': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_RANGE);
                end;
              ')': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_RIGHT_BRACKET, '.)');
                end;
            else
              Result := CreateSymbolToken(SYM_PERIOD);
            end;
          end;
        '/': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '/': begin
                  ConsumeChar;
                  Result := ReadCommentType3;
                end;
            else
              Result := CreateSymbolToken(SYM_SLASH);
            end;
          end;
        ':': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '=': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_ASSIGN);
                end;
            else
              Result := CreateSymbolToken(SYM_COLON);
            end;
          end;
        '<': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '=': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_LESS_THAN_EQUAL);
                end;
            else
              Result := CreateSymbolToken(SYM_LESS_THAN);
            end;
          end;
        '>': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '=': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_GREATER_THAN_EQUAL);
                end;
            else
              Result := CreateSymbolToken(SYM_GREATER_THAN);
            end;
          end;
        '*': begin
            c := ' ';
            if HasData and (not PeekChar(c)) then Exit;
            case c of
              '*': begin
                  ConsumeChar;
                  Result := CreateSymbolToken(SYM_POWER);
                end;
            else
              Result := CreateSymbolToken(SYM_ASTERISK);
            end;
          end;
        '\': Result := CreateSymbolToken(SYM_BACKSLASH);
      else begin
          for J := 0 to NUM_SINGLE_CHAR_SYMBOLS - 1 do begin
            if (c = SingleCharSymbols[J].c) then begin
              Result := CreateSymbolToken(SingleCharSymbols[J].s, c);
              exit;
            end;
          end;
          DoError('Error: Invalid character in Pascal input stream.', [], 0);
        end;
      end;
  finally
    if Result <> nil then
    begin
      Result.FStreamName := StreamName;
      Result.FBeginPosition := BeginPosition;
      Result.FEndPosition := StreamPosition;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.PeekChar(out c: Char): Boolean;
begin
  if IsCharBuffered then begin
    c := BufferedChar;
    Result := True;
  end
  else begin
    if Stream.Position < Stream.Size then begin
      Stream.Read(c, 1);
      BufferedChar := c;
      IsCharBuffered := True;
      Result := True;
    end
    else begin
      EOS := True;
      PeekChar := False;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadCommentType1: TToken;
var
  c: Char;
begin
  Result := TToken.Create(TOK_COMMENT_EXT);
  with Result do 
  begin
    CommentContent := '';
    repeat
      if not HasData or not GetChar(c) then Exit;
      if c = #10 then Inc(Row);
      CommentContent := CommentContent + c; // TODO: Speed up!
    until c = '}';
    
    Data := '{' + CommentContent;
    (* Remove last '}' from CommentContent *)
    SetLength(CommentContent, Length(CommentContent) - 1);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadCommentType2: TToken;
var
  c: Char;
begin
  Result := TToken.Create(TOK_COMMENT_PAS);
  Result.CommentContent := '';
  if not HasData or not GetChar(c) then Exit;
  repeat
    Result.CommentContent := Result.CommentContent + c;

    if c <> '*' then begin
      if c = #10 then Inc(Row);
      if not HasData or not GetChar(c) then Exit;
    end else begin
      if not HasData or not GetChar(c) then Exit;
      if c = ')' then
        begin
          ConsumeChar;
          Result.Data := '(*' + Result.CommentContent + ')';
          { Remove last '*' from Result.CommentContent }
          SetLength(Result.CommentContent, Length(Result.CommentContent) - 1);
          Break;
        end;
    end;
  until False;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadCommentType3: TToken;
var
  c: Char;
begin
  Result := TToken.Create(TOK_COMMENT_CSTYLE);
  with Result do 
  begin
    CommentContent := '';
    while HasData and GetChar(c) and not (c in [#10, #13]) do
      CommentContent := CommentContent + c;
    Data := '//' + CommentContent;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadLiteralString(var t: TToken): Boolean;

  procedure ReleaseToken;
  begin
    t.Free;
    t := nil;
  end;

var
  c: Char;
  Finished: Boolean;
begin
  Finished := False;

  t := TToken.Create(TOK_STRING);
  t.Data := '''';

  repeat
    if not (Stream.Position < Stream.Size) then begin
      ReleaseToken;
      DoError('Error: tokenizer: unexpected end of stream.', [], 0);
    end;
    if not GetChar(c) then begin
      ReleaseToken;
      DoError('Error: tokenizer: could not read character.', [], 0);
    end;
    if c = QuoteChar then begin
      if not PeekChar(c) then begin
        ReleaseToken;
        DoError('Error: tokenizer: could not peek character.', [], 0)
      end;
      if c = QuoteChar then { escaped single quote within string } begin
        ConsumeChar;
        t.Data := t.Data + QuoteChar;
      end
      else { end of string } begin
        Finished := True;
      end;
      t.Data := t.Data + QuoteChar;
    end
    else begin
      t.Data := t.Data + c;
    end;
  until Finished;
  ReadLiteralString := True;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadToken(c: Char; const s: TCharSet; const TT:
  TTokenType; var t: TToken): Boolean;
begin
  Assert(t=nil);
  Result := False;

  t := TToken.Create(TT);
  t.Data := c;
  repeat
    if not PeekChar(c) then begin
      if EOS then
        Result := True
      else begin
        t.Free;
        t := nil;
      end;
      break;
    end;
    if (c in s) then begin
      t.Data := t.Data + c;
      ConsumeChar;
    end else begin
      Result := True;
      break;
    end;
  until False;
  if Result then begin
    Assert(Assigned(t));
  end else begin
    Assert(not Assigned(t));
  end;
end;

function TTokenizer.SkipUntilCompilerDirective: TToken;
var
  c: Char;
begin
  Result := nil;
  repeat
    if GetChar(c) then
      case c of
        '{':
          begin
            Result := ReadCommentType1;
            CheckForDirective(Result);
            if Result.MyType = TOK_DIRECTIVE then break;
            FreeAndNil(Result);
          end;
        '(':
          begin
            GetChar(c);
            if c = '*' then begin
              Result := ReadCommentType2;
              CheckForDirective(Result);
              if Result.MyType = TOK_DIRECTIVE then break;
              FreeAndNil(Result);
            end;
          end;
        #10: Inc(Row);
      end
    else
      DoError('Could not read character from %s', [GetStreamInfo], 0);
  until False;
end;

end.
