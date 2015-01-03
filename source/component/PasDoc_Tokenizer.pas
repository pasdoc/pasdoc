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

{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)
  @abstract(Simple Pascal tokenizer.)

The @link(TTokenizer) object creates @link(TToken) objects (tokens) for the
Pascal programming language from a character input stream.

The @link(PasDoc_Scanner) unit does the same (it actually uses this unit's
tokenizer), with the exception that it evaluates compiler directives,
which are comments that start with a dollar sign. }

unit PasDoc_Tokenizer;

{$I pasdoc_defines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  PasDoc_Utils,
  PasDoc_Types,
  PasDoc_StreamUtils;

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
                TOK_COMMENT_HELPINSIGHT,
                TOK_COMMENT_CSTYLE, TOK_IDENTIFIER, TOK_NUMBER, 
                TOK_STRING, TOK_SYMBOL, TOK_DIRECTIVE, TOK_KEYWORD,
                TOK_ATT_ASSEMBLER_REGISTER);

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
    SD_DISPID,
    SD_DYNAMIC,
    SD_EXPORT,
    SD_EXTERNAL,
    SD_FAR,
    SD_FORWARD,
    SD_GENERIC,
    SD_HELPER,
    SD_INDEX,
    SD_INLINE,
    SD_MESSAGE,
    SD_NAME,
    SD_NEAR,
    SD_NODEFAULT,
    SD_OPERATOR,
    SD_OUT,
    SD_OVERLOAD,
    SD_OVERRIDE,
    SD_PASCAL,
    SD_PRIVATE,
    SD_PROTECTED,
    SD_PUBLIC,
    SD_PUBLISHED,
    SD_READ,
    SD_REFERENCE,
    SD_REGISTER,
    SD_REINTRODUCE,
    SD_RESIDENT,
    SD_SEALED,
    SD_SPECIALIZE,
    SD_STATIC,
    SD_STDCALL,
    SD_STORED,
    SD_STRICT,
    SD_VIRTUAL,
    SD_WRITE,
    SD_DEPRECATED,
    SD_SAFECALL,
    SD_PLATFORM,
    SD_VARARGS,
    SD_FINAL);

const
  { Names of the token types. All start with lower letter.
    They should somehow describe (in a few short words) given 
    TTokenType. }
  TOKEN_TYPE_NAMES: array[TTokenType] of string =
  ( 'whitespace', 'comment ((**)-style)', 'comment ({}-style)', 
    'comment (///-style)',
    'comment (//-style)', 'identifier', 'number', 'string', 'symbol', 
    'directive', 'reserved word', 'AT&T assembler register name');

  TokenCommentTypes: set of TTokenType = 
  [ TOK_COMMENT_PAS, TOK_COMMENT_EXT,
  TOK_COMMENT_HELPINSIGHT,
  TOK_COMMENT_CSTYLE ];

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
    
    { Contents of the string token, that is: the value of the string literal.
      D only when MyType is TOK_STRING. }
    StringContent: string;
    
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
    FBufferedCharSize : Integer;
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
    FStreamPath: string;
    
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);

    procedure CheckForDirective(const t: TToken);
    procedure ConsumeChar;
    
    function CreateSymbolToken(const st: TSymbolType; const s: string): 
      TToken; overload;
      
    { Uses default symbol representation, from SymbolNames[st] }
    function CreateSymbolToken(const st: TSymbolType): TToken; overload;
{$IFDEF STRING_UNICODE}
    { Returns source codepoint size in bytes on success or 0 on failure. }
    { Supports ANSI, UTF-8, UCS2 and UCS2 big endian sources.            }
    { Note that only Unicode codepoints from the BMP are supported.      }    
    function GetChar(out c: WideChar): Integer;
{$ELSE}
    { Returns 1 on success or 0 on failure }
    function GetChar(out c: AnsiChar): Integer;
{$ENDIF}
    function PeekChar(out c: Char): Boolean;
    function ReadCommentType1: TToken;
    function ReadCommentType2: TToken;
    function ReadCommentType3: TToken;
    function ReadAttAssemblerRegister: TToken;
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
      const AStreamName, AStreamPath: string);
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
    
    { This is the path where the underlying file of this stream is located.
    
      It may be an absolute path or a relative path. Relative paths
      are always resolved vs pasdoc current directory.
      This way user can give relative paths in command-line
      when writing Pascal source filenames to parse.
    
      In particular, this may be '' to indicate current dir.
      
      It's always specified like it was processed by
      IncludeTrailingPathDelimiter, so it has trailing PathDelim
      included (unless it was '', in which case it remains empty). }
    property StreamPath: string read FStreamPath;
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
    'ON', 'OR', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING', 'SET', 'SHL',
    'SHR', 'STRING', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES', 'VAR', 'WHILE', 'WITH', 'XOR');

  { Object Pascal directives }
  StandardDirectiveArray:
    array[Low(TStandardDirective)..High(TStandardDirective)] of PChar =
  ('x', // lowercase letters never match
    'ABSOLUTE', 'ABSTRACT', 'APIENTRY', 'ASSEMBLER', 'AUTOMATED',
    'CDECL', 'CVAR', 'DEFAULT', 'DISPID', 'DYNAMIC', 'EXPORT', 'EXTERNAL',
    'FAR', 'FORWARD', 'GENERIC', 'HELPER', 'INDEX', 'INLINE', 'MESSAGE', 'NAME', 'NEAR',
    'NODEFAULT', 'OPERATOR', 'OUT', 'OVERLOAD', 'OVERRIDE', 'PASCAL', 'PRIVATE',
    'PROTECTED', 'PUBLIC', 'PUBLISHED', 'READ', 'REFERENCE', 'REGISTER',
    'REINTRODUCE', 'RESIDENT', 'SEALED', 'SPECIALIZE', 'STATIC',
    'STDCALL', 'STORED', 'STRICT', 'VIRTUAL',
    'WRITE', 'DEPRECATED', 'SAFECALL', 'PLATFORM', 'VARARGS', 'FINAL');

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
  const AStreamName, AStreamPath: string);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;
  Row := 1;
  Stream := AStream;
  FStreamName := AStreamName;
  FStreamPath := AStreamPath;
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
  raise EPasDoc.Create(AMessage + Format(' (at %s)', [GetStreamInfo]),
    AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TTokenizer.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity < FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }
{$IFDEF STRING_UNICODE}
function TTokenizer.GetChar(out c: WideChar): Integer;
const
  LDefaultFailChar = '?';
var
  Buf : array [0..7] of Byte;
  LInt: Integer;
begin
  if IsCharBuffered then
  begin
    c := BufferedChar;
    IsCharBuffered := False;
    Result := FBufferedCharSize;
  end
  else begin // Actually only UCS2 and UCS2Be
    case TStreamReader(Stream).CurrentCodePage of
        CP_UTF16    :
          begin
            Result := Stream.Read(c, 2);            
            Exit;
          end;
        CP_UTF16BE  :
          begin
            Result := Stream.Read(c, 2);            
            Swap16Buf(@c, @c, 1);
            Exit;
          end;
    end; // case

    { MBCS text }
    Result := 0;
    Buf[0] := 0;

    Result := Stream.Read(Buf[Result], 1);
    if Result = 0 then
      Exit;
    if TStreamReader(Stream).CurrentCodePage = CP_UTF8 then
    begin
      LInt := Utf8Size(Buf[0]); // Read number of bytes
      if LInt > 1 then
      begin
        Inc(Result, Stream.Read(Buf[Result], LInt -1));
        if Result <> LInt then
        begin
          c := LDefaultFailChar;    // return the default fail char.
          Exit;
        end;
      end;
    end
    else begin
      { Only DBCS have constant LeadBytes so we actually do not support }
      { some rarely used MBCS, such as euc-jp or UTF-7, with a maximum  }
      { codepoint size > 2 bytes.                                 }{ AG }
      if AnsiChar(Buf[0]) in TStreamReader(Stream).LeadBytes then
      begin
        if Stream.Read(Buf[Result], 1) = 1 then
          Inc(Result)
        else begin
          Result := 0;
          Exit;
        end;
      end
    end;
    if (Result = 1) and (Buf[0] < 128) then
      c := WideChar(Buf[0]) // Plain ASCII, no need to call MbToWc (speed)
    else
    if MultiByteToWideChar(TStreamReader(Stream).CurrentCodePage,
                           0, @Buf[0], Result, @c, 1) <> 1 then
        c := LDefaultFailChar; // return the default fail char.
  end;
end;

{$ELSE}

function TTokenizer.GetChar(out c: AnsiChar): Integer;
begin
  if IsCharBuffered then
  begin
    c := BufferedChar;
    IsCharBuffered := False;
    Result := FBufferedCharSize;
  end
  else
    Result := Stream.Read(c, 1);
end;
{$ENDIF}
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
    Result := Stream.Position - FBufferedCharSize
  else
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
  BeginPosition := StreamPosition; //used in finally
  try
    if GetChar(c) = 0 then
      DoError('Tokenizer: could not read character', [], 0);

    if IsCharInSet(c, Whitespace) then
    begin
      if ReadToken(c, Whitespace, TOK_WHITESPACE, Result) then
          { after successful reading all whitespace characters, update
            internal row counter to be able to state current row on errors;
            TODO: will fail on Mac files (row is 13) }
        Inc(Row, StrCountCharA(Result.Data, #10))
      else
        DoError('Tokenizer: could not read character', [], 0);
    end else
    if IsCharInSet(c, IdentifierStart) then
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
    if IsCharInSet(c, NumberStart) then
      ReadToken(c, NumberOther, TOK_NUMBER, Result)
    else
      case c of
        QuoteChar:
          ReadLiteralString(Result);
        '#':
          if ReadToken(c, CharOther, TOK_STRING, Result) then
          begin
            try
              { Note that StrToInt automatically handles hex characters when 
                number starts from $. So below will automatically work for them. }
              Result.StringContent := Chr(StrToInt(SEnding(Result.Data, 2)));
            except 
              { In case of EConvertError, make a warning and continue.
                Result.StringContent will remain empty, which isn't a real problem. }
              on E: EConvertError do
                DoMessage(2, pmtWarning, 'Cannot convert string character code to int: %s', [Result.Data]);
            end;
          end;
        '{': begin
            Result := ReadCommentType1;
            CheckForDirective(Result);
          end;
        '(': begin
            c := ' ';
            if HasData and not PeekChar(c) then
              DoError('Tokenizer: could not read character', [], 0);
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
        '%': Result := ReadAttAssemblerRegister;
      else begin
          for J := 0 to NUM_SINGLE_CHAR_SYMBOLS - 1 do begin
            if (c = SingleCharSymbols[J].c) then begin
              Result := CreateSymbolToken(SingleCharSymbols[J].s, c);
              exit;
            end;
          end;
          DoError('Invalid character (code %d) in Pascal input stream', [Ord(C)], 0);
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
    FBufferedCharSize := GetChar(c);
    if FBufferedCharSize > 0 then
    begin
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
      if not HasData or (GetChar(c) = 0) then Exit;
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
  if not HasData or (GetChar(c) = 0) then Exit;
  repeat
    Result.CommentContent := Result.CommentContent + c;

    if c <> '*' then begin
      if c = #10 then Inc(Row);
      if not HasData or (GetChar(c) = 0) then Exit;
    end else begin
      if not HasData or (GetChar(c) = 0) then Exit;
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
  pos: Integer;
  Prefix: string;
begin
  Result := TToken.Create(TOK_COMMENT_CSTYLE);
  with Result do
  begin
    CommentContent := '';
    pos := 0;

    Prefix := '//';
    while HasData and (GetChar(c) > 0) do
    begin
      case c of
        #10: begin Inc(Row); break end;
        #13: break;
        else if (c = '/') and (pos = 0) then
          begin
            MyType := TOK_COMMENT_HELPINSIGHT;
            Prefix := '///';
          end else
            CommentContent := CommentContent + c;
      end;
      Inc(pos);
    end;

    Data := Prefix + CommentContent;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TTokenizer.ReadAttAssemblerRegister: TToken;
var
  C: char;
begin
  Result := TToken.Create(TOK_ATT_ASSEMBLER_REGISTER);
  
  Result.Data := '%';
  repeat
    if (not HasData) or (not PeekChar(C)) then Exit;
    if IsCharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9']) then
    begin
      GetChar(C);
      Result.Data := Result.Data + C;
    end else
      Break;
  until false;
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
      DoError('Tokenizer: unexpected end of stream', [], 0);
    end;
    if GetChar(c) = 0 then begin
      ReleaseToken;
      DoError('Tokenizer: could not read character', [], 0);
    end;
    if c = QuoteChar then begin
      if not PeekChar(c) then begin
        ReleaseToken;
        DoError('Tokenizer: could not peek character', [], 0)
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
    { Note that, because of logic above, this will append only ONE apostrophe 
      when reading two apostrophes in source code.
      Checking Finished prevents adding the ending apostrophe. }
    if not Finished then
      T.StringContent := T.StringContent + c;
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
    if IsCharInSet(c, s) then begin
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
    if GetChar(c) > 0 then
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
            if PeekChar(c) and (c = '*') then
            begin
              ConsumeChar;
              Result := ReadCommentType2;
              CheckForDirective(Result);
              if Result.MyType = TOK_DIRECTIVE then break;
              FreeAndNil(Result);
            end;

            (* If C was not a '*', then we don't consume it here.
               This is important, because C could be #10 (indicates
               newline, so we must Inc(Row)) or even '{' (which could
               indicate compiler directive). And sequences like
               '('#10 and '({$ifdef ...' should work, see
               ../../tests/error_line_number_3.pas and
               ../../tests/ok_not_defined_omit.pas *)

          end;
        #10: Inc(Row);
      end
    else
      DoError('Could not read character', [], 0);
  until False;
end;

end.
