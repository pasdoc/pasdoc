{
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @abstract(Provides simplified Pascal tokenizer.)

The @link(TTokenizer) object creates @link(TToken) objects (tokens) for the
Pascal programming language from a character input stream.

<P>The @link(Scanning) unit does the same (it actually uses this unit's
tokenizer), with the exception that it evaluates compiler directives,
which are comments that start with a dollar sign. }

unit PasDoc_Tokenizer;

interface

uses
  StringVector,
  Utils,
  PasDoc_Types,
  Classes;

type
  { enumeration type that provides all types of tokens; each token's name
    starts with TOK_ }
  TTokenType = (TOK_WHITESPACE, TOK_COMMENT, TOK_IDENTIFIER,
    TOK_NUMBER, TOK_STRING, TOK_SYMBOL, TOK_DIRECTIVE, TOK_RESERVED);

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
    KEY_SPINTERFACE,
    KEY_V,
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

const
  { Names of the token types }
  TOKEN_TYPE_NAMES: array[TTokenType] of string =
  ('Whitespace', 'Comment', 'Identifier', 'Number', 'String',
    'Symbol', 'Directive', 'Reserved');

type
  { }
  TCharSet = set of Char;
  { enumeration type that provides all types of symbols; each
    symbol's name starts with SYM_ }
  TSymbolType = (SYM_PLUS, SYM_MINUS, SYM_ASTERISK, SYM_SLASH, SYM_EQUAL,
    SYM_LESS_THAN, SYM_LESS_THAN_EQUAL, SYM_GREATER_THAN,
    SYM_GREATER_THAN_EQUAL, SYM_LEFT_BRACKET, SYM_RIGHT_BRACKET,
    SYM_COMMA, SYM_LEFT_PARENTHESIS, SYM_RIGHT_PARENTHESIS, SYM_COLON,
    SYM_SEMICOLON, SYM_ROOF, SYM_PERIOD, SYM_AT, SYM_LEFT_BRACE,
    SYM_RIGHT_BRACE, SYM_DOLLAR, SYM_NUMBER, SYM_ASSIGN, SYM_RANGE);

  { Stores the exact type and additional information on one token.
    Additionally, @link(Data) stores the array of characters }
  TToken = class(TObject)
    { the exact character representation of this token as it was found in the
      input file }
    Data: string;
    { additional information on this token as a record of variant fields }
    Info: record
      case Integer of
        0: (WhitespaceRows: Integer);
        2: (SymbolType: TSymbolType);
        3: (ReservedKey: TKeyWord);
    end;
    { the type of this token as @link(TTokenType) }
    MyType: TTokenType;
    { Create a token of and assign the argument token type to @link(MyType) }
    constructor Create(const TT: TTokenType);
    function GetTypeName: string;
    { Returns if argument ST equals @link(MyType) }
    function IsSymbol(const st: TSymbolType): Boolean;
  end;

  { @abstract(Converts a @link(TInputStream) to a sequence of @link(TToken) objects.) }
  TTokenizer = class(TObject)
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
      const; const AExitCode: Integer);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    procedure CheckForDirective(const t: TToken);
    procedure ConsumeChar;
    function CreateSymbolToken(const st: TSymbolType; const s: string): TToken;
    function GetChar(var c: Char): Boolean;
    function PeekChar(var c: Char): Boolean;
    function ReadCommentType1: TToken;
    function ReadCommentType2: TToken;
    function ReadCommentType3: TToken;
    function ReadLiteralString(var t: TToken): Boolean;
    function ReadToken(c: Char; const s: TCharSet; const TT: TTokenType; var
      t: TToken): Boolean;

  public
    { Creates a TTokenizer and associates it with given @link(TInputStream). }
    constructor Create(
      const s: TStream;
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

//const
//  {: names of the token types }
//  TokenTypeNames: array[TOK_WHITESPACE..TOK_RESERVED] of string =
//  ('whitespace', 'comment', 'identifier', 'number', 'string',
//    'symbol', 'directive', 'reserved');

const
  { all Object Pascal keywords }
  KeyWordArray: array[Low(TKeyword)..High(TKeyword)] of string =
  ('x', // lowercase never matches
    'AND', 'ARRAY', 'AS', 'ASM', 'BEGIN', 'CASE', 'CLASS', 'CONST',
    'CONSTRUCTOR', 'DESTRUCTOR', 'SPINTERFACE', 'V', 'DO', 'DOWNTO',
    'ELSE', 'END', 'EXCEPT', 'EXPORTS', 'FILE', 'FINALIZATION',
    'FINALLY', 'FOR', 'FUNCTION', 'GOTO', 'IF', 'IMPLEMENTATION',
    'IN', 'INHERITED', 'INITIALIZATION', 'INLINE', 'INTERFACE',
    'IS', 'LABEL', 'LIBRARY', 'MOD', 'NIL', 'NOT', 'OBJECT', 'OF',
    'ON', 'OR', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING', 'SET', 'SHL',
    'SHR', 'STRING', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES', 'VAR', 'WHILE', 'WITH', 'XOR');

type
  TStandardDirective = (
    SD_INVALIDSTANDARDDIRECTIVE,
    SD_ABSOLUTE,
    SD_ABSTRACT,
    SD_APIENTRY,
    SD_ASSEMBLER,
    SD_AUTOMATED,
    SD_CDECL,
    SD_DEFAULT,
    SD_SPID,
    SD_DYNAMIC,
    SD_EXPORT,
    SD_EXTERNAL,
    SD_FAR,
    SD_FORWARD,
    SD_INDEX,
    SD_MESSAGE,
    SD_NAME,
    SD_NEAR,
    SD_NODEFAULT,
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
  { Object Pascal directives }
  StandardDirectiveArray:
    array[Low(TStandardDirective)..High(TStandardDirective)] of PChar =
  ('x', // lowercase letters never match
    'ABSOLUTE', 'ABSTRACT', 'APIENTRY', 'ASSEMBLER', 'AUTOMATED',
    'CDECL', 'DEFAULT', 'SPID', 'DYNAMIC', 'EXPORT', 'EXTERNAL',
    'FAR', 'FORWARD', 'INDEX', 'MESSAGE', 'NAME', 'NEAR',
    'NODEFAULT', 'OVERLOAD', 'OVERRIDE', 'PASCAL', 'PRIVATE',
    'PROTECTED', 'PUBLIC', 'PUBLISHED', 'READ', 'REGISTER',
    'REINTRODUCE', 'RESIDENT', 'STDCALL', 'STORED', 'VIRTUAL',
    'WRITE', 'DEPRECATED', 'SAFECALL', 'PLATFORM', 'VARARGS');

function StandardDirectiveByName(const Name: string): TStandardDirective;
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
  { TToken
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

function TToken.IsSymbol(const st: TSymbolType): Boolean;
begin
  IsSymbol := (MyType = TOK_SYMBOL) and (Info.SymbolType = st);
end;

{ ---------------------------------------------------------------------------- }
{ TTokenizer
{ ---------------------------------------------------------------------------- }

constructor TTokenizer.Create(
  const s: TStream;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName: string);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;
  Row := 1;
  Stream := s;
  FStreamName := AStreamName;
end;

{ ---------------------------------------------------------------------------- }

destructor TTokenizer.Destroy;
begin
  if Assigned(Stream) then Stream.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.CheckForDirective(const t: TToken);
var
  l: Cardinal;
begin
  l := Length(t.Data);
  if ((l >= 2) and (t.Data[1] = '{') and (t.Data[2] = '$')) or
    ((l >= 3) and (t.Data[1] = '(') and (t.Data[2] = '*') and (t.Data[3] = '$')) then
    t.MyType := TOK_DIRECTIVE;
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.ConsumeChar;
begin
  IsCharBuffered := False;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.CreateSymbolToken(const st: TSymbolType; const s:
  string): TToken;
begin
  Result := TToken.Create(TOK_SYMBOL);
  with Result do begin
    Info.SymbolType := st;
    Data := s;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.DoError(const AMessage: string; const AArguments: array
  of const; const AExitCode: Integer);
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

function TTokenizer.GetChar(var c: Char): Boolean;
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

function TTokenizer.GetToken: TToken;
var
  c: Char;
  I: TKeyword;
  s: string;
  J: Integer;
begin
  Result := nil;
  if GetChar(c) then
    if c in Whitespace then begin
      if ReadToken(c, Whitespace, TOK_WHITESPACE, Result) then
          { after successful reading all whitespace characters, update
            internal row counter to be able to state current row on errors;
            caveat: will fail on Mac files (row is 13) }
        Inc(Row, StrCountCharA(Result.Data, #10))
      else
        DoError('Error: tokenizer: could not read character.', [], 0);
    end
    else
      if c in IdentifierStart then begin
        if ReadToken(c, IdentifierOther, TOK_IDENTIFIER, Result) then begin
              { check if identifier is a reserved identifier }
          s := Result.Data;
          I := KeyWordByName(s);
          if (I <> KEY_INVALIDKEYWORD) then begin
            Result.MyType := TOK_RESERVED;
            Result.Info.ReservedKey := I;
          end;
        end
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
                  Result := CreateSymbolToken(SYM_LEFT_PARENTHESIS, '(');
                end;
              end;
            ')': begin
                c := ' ';
                Result := CreateSymbolToken(SYM_RIGHT_PARENTHESIS, ')');
              end;
            '.': begin
                c := ' ';
                if HasData and (not PeekChar(c)) then Exit;
                case c of
                  '.': begin
                      ConsumeChar;
                      Result := CreateSymbolToken(SYM_RANGE, '..');
                    end;
                  ')': begin
                      ConsumeChar;
                      Result := CreateSymbolToken(SYM_RIGHT_BRACKET, '.)');
                    end;
                else
                  Result := CreateSymbolToken(SYM_PERIOD, '.');
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
                  Result := CreateSymbolToken(SYM_SLASH, '/');
                end;
              end;
            ':': begin
                c := ' ';
                if HasData and (not PeekChar(c)) then Exit;
                case c of
                  '=': begin
                      ConsumeChar;
                      Result := CreateSymbolToken(SYM_ASSIGN, ':=');
                    end;
                else
                  Result := CreateSymbolToken(SYM_COLON, ':');
                end;
              end;
            '<': begin
                c := ' ';
                if HasData and (not PeekChar(c)) then Exit;
                case c of
                  '=': begin
                      ConsumeChar;
                      Result := CreateSymbolToken(SYM_LESS_THAN_EQUAL, '<=');
                    end;
                else
                  Result := CreateSymbolToken(SYM_LESS_THAN, '<');
                end;
              end;
            '>': begin
                c := ' ';
                if HasData and (not PeekChar(c)) then Exit;
                case c of
                  '=': begin
                      ConsumeChar;
                      Result := CreateSymbolToken(SYM_GREATER_THAN_EQUAL,
                        '<=');
                    end;
                else
                  Result := CreateSymbolToken(SYM_GREATER_THAN, '<');
                end;
              end;
          else begin
              for J := 0 to NUM_SINGLE_CHAR_SYMBOLS - 1 do begin
                if (c = SingleCharSymbols[J].c) then begin
                  Result := CreateSymbolToken(SingleCharSymbols[J].s, c);
                  exit;
                end;
              end;
              DoError('Error: Invalid character in Pascal input stream.', [], 0);
            end;
          end
      else
        DoError('Error: tokenizer: could not read character.', [], 0);
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.PeekChar(var c: Char): Boolean;
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
  Result := TToken.Create(TOK_COMMENT);
  with Result do begin
    Data := '{';
    repeat
      if not HasData or not GetChar(c) then Exit;
      if c = #10 then Inc(Row);
      Data := Data + c; // TODO: Speed up!
    until c = '}';
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadCommentType2: TToken;
var
  c: Char;
begin
  Result := TToken.Create(TOK_COMMENT);
  Result.Data := '(*';
  if not HasData or not GetChar(c) then Exit;
  repeat
    Result.Data := Result.Data + c;
 
    if c <> '*' then begin
      if c = #10 then Inc(Row);
      if not HasData or not GetChar(c) then Exit;
    end else begin
      if not HasData or not GetChar(c) then Exit;
      if c = ')' then
        begin
          ConsumeChar;
          Result.Data := Result.Data + c;
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
  Result := TToken.Create(TOK_COMMENT);
  with Result do begin
    Data := '//';
    while HasData and GetChar(c) and not (c in [#10, #13]) do
      Data := Data + c;
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
      end
    else
      DoError('Could not read character from %s', [GetStreamInfo], 0);
  until False;
end;

end.
