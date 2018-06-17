{
  Copyright 1998-2018 PasDoc developers.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)

  @abstract(Simple Pascal scanner.)

  The scanner object @link(TScanner) returns tokens from a Pascal language
  character input stream. It uses the @link(PasDoc_Tokenizer) unit to get tokens,
  regarding conditional directives that might lead to including another files
  or will add or delete conditional symbols. Also handles FPC macros
  (when HandleMacros is true). So, this scanner is a combined
  tokenizer and pre-processor. }

unit PasDoc_Scanner;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Types,
  PasDoc_Tokenizer,
  PasDoc_StringVector,
  PasDoc_StreamUtils,
  PasDoc_StringPairVector;

const
  { maximum number of streams we can recurse into; first one is the unit
    stream, any other stream an include file; current value is 32, increase
    this if you have more include files recursively including others }
  MAX_TOKENIZERS = 32;

type
  { subrange type that has the 26 lower case letters from a to z }
  TUpperCaseLetter = 'A'..'Z';
  { an array of boolean values, index type is @link(TUpperCaseLetter) }
  TSwitchOptions = array[TUpperCaseLetter] of Boolean;

  ETokenizerStreamEnd = class(EPasDoc);
  EInvalidIfCondition = class(EPasDoc);

  { All directives a scanner is going to regard. }
  TDirectiveType = (DT_UNKNOWN, DT_DEFINE, DT_ELSE, DT_ENDIF, DT_IFDEF,
    DT_IFNDEF, DT_IFOPT, DT_INCLUDE_FILE, DT_UNDEF, DT_INCLUDE_FILE_2,
    DT_IF, DT_ELSEIF, DT_IFEND);

  { This class scans one unit using one or more @link(TTokenizer) objects
    to scan the unit and all nested include files. }
  TScanner = class(TObject)
  private
    FCurrentTokenizer: Integer;
    FDirectiveLevel: Integer;
    FTokenizers: array[0..MAX_TOKENIZERS - 1] of TTokenizer;
    FSwitchOptions: TSwitchOptions;
    FBufferedToken: TToken;

    { For each symbol:
        Name is the unique Name,
        Value is the string to be expanded into (in case of a macro),
        Data is SymbolIsMacro or SymbolIsNotMacro to say if this is a macro
          (i.e. should it be expanded).

      Note the important fact: we can't use Value <> '' to decide
      if symbol is a macro. A non-macro symbol is something
      different than a macro that expands to nothing. }
    FSymbols: TStringPairVector;

    FIncludeFilePaths: TStringVector;
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FHandleMacros: boolean;

    { Removes symbol Name from the internal list of symbols.
      If Name was not in that list, nothing is done. }
    procedure DeleteSymbol(const Name: string);

    { Returns if a given symbol Name is defined at the moment. }
    function IsSymbolDefined(const Name: string): Boolean;

    function IsSwitchDefined(n: string): Boolean;

    { This creates and adds new Tokenizer to FTokenizers list and makes
      it the current tokenizer. It also checks MAX_TOKENIZERS limit.
      After calling this procedure, don't free Stream -- it will be
      owned by created Tokenizer, and created Tokenizer will be managed
      as part of FTokenizers list. }
    procedure OpenNewTokenizer(Stream: TStream;
      const StreamName, StreamPath: string);

    procedure OpenIncludeFile(n: string);

    { Skip (discard all the tokens) until $else, $elseif, $endif, $ifend is found.
      Returns the directive that stopped the scanning,
      it is always one of [DT_ELSE, DT_ELSEIF, DT_ENDIF, DT_IFEND].

      It consumes all the tokens from FTokenizers[FCurrentTokenizer],
      including the final one ($ifend and so on).
      If final token is $elseif and you used overloaded version with ElseifToken,
      then the final token is returned.

      Note that for pasdoc, $endif and $ifend directives are always exactly
      equivalent and interchangeable. For Delphi, it used to be that
      $if / $elseif had to be terminated with $ifend
      (to be able to nest $if...$ifend within $ifdef...$endif
      on older Delphi versions that don't support $if,
      see Borland Delphi docs about this).
      For FPC, $endif is valid terminator for $if. }
    function SkipUntilElseOrEndif(out ElseifToken: TToken): TDirectiveType; overload;
    function SkipUntilElseOrEndif: TDirectiveType; overload;

    { Skip until $endif, $ifend.
      Consumes all the tokens from FTokenizers[FCurrentTokenizer]
      including the final one. }
    procedure SkipUntilEndif;

    procedure ResolveSwitchDirectives(const Comment: String);

    procedure SetIncludeFilePaths(Value: TStringVector);

    { Calculate boolean condition, like the one allowed at $if and $elseif. }
    function IfCondition(const Condition: String): Boolean;
  protected
    procedure DoError(const AMessage: string;
      const AArguments: array of const);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);
  public
    { Creates a TScanner object that scans the given input stream.

      Note that the stream S will be freed by this object
      (at destruction or when we will read all it's tokens),
      so after creating TScanner you should leave the stream
      to be managed completely by this TScanner. }
    constructor Create(
      const s: TStream;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName, AStreamPath: string;
      const AHandleMacros: boolean);
    destructor Destroy; override;

    { Adds Name to the list of symbols (as a normal symbol, not macro). }
    procedure AddSymbol(const Name: string);

    { Adds all symbols in the NewSymbols collection by calling
      @link(AddSymbol) for each of the strings in that collection. }
    procedure AddSymbols(const NewSymbols: TStringVector);

    { Adds Name as a symbol that is a macro, that expands to Value. }
    procedure AddMacro(const Name, Value: string);

    { Gets next token and throws it away. }
    procedure ConsumeToken;

    { Returns next token.
      Always non-nil (will raise exception in case of any problem). }
    function GetToken: TToken;

    { Returns the name of the file that is currently processed and the line
      number. Good for meaningful error messages. }
    function GetStreamInfo: string;

    { Paths to search for include files.
      When you assign something to this property
      it causes Assign(Value) call, not a real reference copy. }
    property IncludeFilePaths: TStringVector read FIncludeFilePaths
      write SetIncludeFilePaths;
    function PeekToken: TToken;

    { Place T in the buffer. Next time you will call GetToken you will
      get T. This also sets T to nil (because you shouldn't free T
      anymore after ungetting it). Note that the buffer has room only
      for 1 token, so you have to make sure that you will never unget
      more than two tokens. Practically, always call UnGetToken right
      after some GetToken. }
    procedure UnGetToken(var t: TToken);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property Verbosity: Cardinal read FVerbosity write FVerbosity;
    property SwitchOptions: TSwitchOptions read FSwitchOptions;

    property HandleMacros: boolean read FHandleMacros;
  end;

implementation

uses PasDoc_Utils;

const
  DirectiveNames: array[DT_DEFINE..High(TDirectiveType)] of string =
  ( 'DEFINE', 'ELSE', 'ENDIF', 'IFDEF', 'IFNDEF', 'IFOPT', 'I', 'UNDEF',
    'INCLUDE', 'IF', 'ELSEIF', 'IFEND' );

  SymbolIsNotMacro = nil;
  SymbolIsMacro = Pointer(1);

{ ---------------------------------------------------------------------------- }

(*Assumes that CommentContent is taken from a Token.CommentContent where
  Token.MyType was TOK_DIRECTIVE.

  Extracts DirectiveName and DirectiveParam from CommentContent.
  DirectiveName is the thing right after $ sign, uppercased.
  DirectiveParam (in two versions: Black and White) is what followed
  after DirectiveName.

  E.g. for CommentContent = {$define My_Symbol} we get
  DirectiveName = 'DEFINE' and
  DirectiveParamBlack = 'My_Symbol'
  (and DirectiveParamWhite also = 'My_Symbol').

  We get two versions of DirectiveParam:
  @orderedList(
    @item(DirectiveParamBlack is what followed DirectiveName and
      ended at the 1st whitespace. So DirectiveParamBlack
      never contains any white char.)

    @item(DirectiveParamWhite is what followed DirectiveName and
      ended at end of CommentContent. So DirectiveParamWhite
      may contain white characters.)
  )

  So DirectiveParamBlack is always a prefix of DirectiveParamWhite.

  Some directives use DirectiveParamBlack and some use
  DirectiveParamWhite, that's why we return both.
  E.g. {$ifdef foo bar xyz} is equivalent to {$ifdef foo}
  (bar and xyz are simply ignored by Pascal compilers).
  When using FPC and macro is off, or when using other compilers,
  {$define foo := bar xyz} means just {$define foo}.
  So you use DirectiveParamBlack.
  However when using FPC and macro is on,
  then {$define foo := bar xyz} means "define foo as a macro
  that expands to ``bar xyz'', so in this case you will need to use
  DirectiveParamWhite.
*)
function SplitDirective(const CommentContent: string;
  out DirectiveName, DirectiveParamBlack, DirectiveParamWhite: string): Boolean;
var
  i: Integer;
  l: Integer;
begin
  Result := False;
  DirectiveName := '';
  DirectiveParamBlack := '';
  DirectiveParamWhite := '';

  l := Length(CommentContent);

  { skip dollar sign from CommentContent }
  i := 2;
  if i > l then Exit;

  { get directive name }
  while (i <= l) and (CommentContent[i] <> ' ') do
  begin
    DirectiveName := DirectiveName + UpCase(CommentContent[i]);
    Inc(i);
  end;
  Result := True;

  { skip spaces }
  while (i <= l) and (CommentContent[i] = ' ') do
    Inc(i);
  if i > l then Exit;

  { get parameters - no conversion to uppercase here, it could be an include
    file name whose name need not be changed (platform.inc <> PLATFORM.INC) }
  while (i <= l) and (CommentContent[i] <> ' ') do
  begin
    DirectiveParamBlack := DirectiveParamBlack + CommentContent[i];
    Inc(i);
  end;

  DirectiveParamWhite := DirectiveParamBlack;
  while (i <= l) do
  begin
    DirectiveParamWhite := DirectiveParamWhite + CommentContent[i];
    Inc(i);
  end;
end;

{ First, splits CommentContent like SplitDirective.

  Then returns true and sets Dt to appropriate directive type,
  if DirectiveName was something known (see array DirectiveNames).
  Else returns false. }
function IdentifyDirective(const CommentContent: string;
  out dt: TDirectiveType;
  out DirectiveName, DirectiveParamBlack, DirectiveParamWhite: string): Boolean;
var
  i: TDirectiveType;
begin
  Result := false;
  if SplitDirective(CommentContent,
    DirectiveName, DirectiveParamBlack, DirectiveParamWhite) then
  begin
    for i := DT_DEFINE to High(TDirectiveType) do
    begin
      if UpperCase(DirectiveName) = DirectiveNames[i] then
      begin
        dt := i;
        Result := True;
        break;
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TScanner }
{ ---------------------------------------------------------------------------- }

constructor TScanner.Create(
  const s: TStream;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName, AStreamPath: string;
  const AHandleMacros: boolean);
var
  c: TUpperCaseLetter;
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;
  FHandleMacros := AHandleMacros;

  { Set default switch directives (according to the Delphi 4 Help). }
  for c := Low(SwitchOptions) to High(SwitchOptions) do
    FSwitchOptions[c] := False;
  FSwitchOptions['A'] := True;
  FSwitchOptions['C'] := True;
  FSwitchOptions['D'] := True;
  FSwitchOptions['G'] := True;
  FSwitchOptions['H'] := True;
  FSwitchOptions['I'] := True;
  FSwitchOptions['J'] := True;
  FSwitchOptions['L'] := True;
  FSwitchOptions['P'] := True;
  FSwitchOptions['O'] := True;
  FSwitchOptions['V'] := True;
  FSwitchOptions['X'] := True;

  FSymbols := TStringPairVector.Create(true);

  FTokenizers[0] := TTokenizer.Create(s, OnMessageEvent, VerbosityLevel,
    AStreamName, AStreamPath);
  FCurrentTokenizer := 0;
  FBufferedToken := nil;

  FIncludeFilePaths := TStringVector.Create;
end;

{ ---------------------------------------------------------------------------- }

destructor TScanner.Destroy;
var
  i: Integer;
begin
  FSymbols.Free;

  for i := 0 to FCurrentTokenizer do begin
    FTokenizers[i].Free;
  end;

  FBufferedToken.Free;

  FIncludeFilePaths.Free;

  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.SetIncludeFilePaths(Value: TStringVector);
begin
  FIncludeFilePaths.Assign(Value);
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.AddSymbol(const Name: string);
begin
  if not IsSymbolDefined(Name) then
  begin
    DoMessage(6, pmtInformation, 'Symbol "%s" defined', [Name]);
    FSymbols.Add(TStringPair.Create(Name, '', SymbolIsNotMacro));
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.AddSymbols(const NewSymbols: TStringVector);
var
  i: Integer;
begin
  if NewSymbols <> nil then
    for i := 0 to NewSymbols.Count - 1 do
      AddSymbol(NewSymbols[i])
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.AddMacro(const Name, Value: string);
var
  i: Integer;
begin
  i := FSymbols.FindName(Name);
  if i = -1 then
  begin
    DoMessage(6, pmtInformation, 'Macro "%s" defined as "%s"', [Name, Value]);
    FSymbols.Add(TStringPair.Create(Name, Value, SymbolIsMacro));
  end else
  begin
    DoMessage(6, pmtInformation, 'Macro "%s" RE-defined as "%s"', [Name, Value]);
    { Redefine macro in this case. }
    FSymbols.Items[i].Value := Value;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.ConsumeToken;
begin
  FBufferedToken := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DeleteSymbol(const Name: string);
begin
  FSymbols.DeleteName(Name);
end;

{ ---------------------------------------------------------------------------- }

function TScanner.GetStreamInfo: string;
begin
  if (FCurrentTokenizer >= 0) then begin
    Result := FTokenizers[FCurrentTokenizer].GetStreamInfo
  end else begin
    Result := '';
  end;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.GetToken: TToken;

  { Call this when you get $define directive }
  procedure HandleDefineDirective(
    const DirectiveParamBlack, DirectiveParamWhite: string);
  var
    i: Integer;
    SymbolName: string;
  begin
    if not HandleMacros then
      AddSymbol(DirectiveParamBlack) else
    begin
      i := 1;
      while SCharIs(DirectiveParamWhite, i, ['a'..'z', 'A'..'Z', '1'..'9', '_']) do
        Inc(i);

      SymbolName := Copy(DirectiveParamWhite, 1, i - 1);

      while SCharIs(DirectiveParamWhite, i, WhiteSpace) do
        Inc(i);

      if Copy(DirectiveParamWhite, i, 2) = ':=' then
        AddMacro(SymbolName, Copy(DirectiveParamWhite, i + 2, MaxInt)) else
        AddSymbol(SymbolName);
    end;
  end;

  { If T is an identifier that expands to a macro, then it handles it
    (i.e. opens a new tokenizer that expands a macro) and returns true.
    Else returns false. }
  function ExpandMacro(T: TToken): boolean;
  var
    SymbolIndex: Integer;
  begin
    Result := T.MyType = TOK_IDENTIFIER;
    if Result then
    begin
      SymbolIndex := FSymbols.FindName(T.Data);
      Result := (SymbolIndex <> -1) and
         (FSymbols[SymbolIndex].Data = SymbolIsMacro);
      if Result then
        OpenNewTokenizer(TStringStream.Create(
          FSymbols[SymbolIndex].Value),
          '<' + FSymbols[SymbolIndex].Name + ' macro>',
          { Expanded macro text inherits current StreamPath }
          FTokenizers[FCurrentTokenizer].StreamPath);
    end;
  end;

  { Call this on $ifdef, $ifndef, $ifopt, $if directives.
    This "consumes" all the tokens up to the "active" (with satisfied
    condition) block of $ifdef / $if / $else / $elseif sequence.
    So the outside code can just pass-through the remaining tokens,
    and ignore everything after any $else or $elseif,
    up to the matching $endif / $ifend.

    @param(IsTrue says if condition is true.))
    @param(DirectiveName is used for debug messages.)
    @param(DirectiveParam is also used for debug messages.) }
  procedure HandleIfDirective(IsTrue: boolean;
    const DirectiveName, DirectiveParam: string);

    function ExtractElseifCondition(const ElseifToken: TToken): String;
    var
      DT: TDirectiveType;
      DirectiveName, DirectiveParamBlack, DirectiveParamWhite: String;
    begin
      Assert(ElseifToken.MyType = TOK_DIRECTIVE);
      if not IdentifyDirective(ElseifToken.CommentContent, DT,
        DirectiveName, DirectiveParamBlack, DirectiveParamWhite) then
        DoError('IdentifyDirective returned false, but we know it should be $elseif', []);
      Assert(DT = DT_ELSEIF);
      Result := DirectiveParamWhite;
    end;

  var
    ElseifToken: TToken;
    EndingDirective: TDirectiveType;
    ElseifCondition: String;
  begin
    DoMessage(6, pmtInformation,
      '$%s encountered (%s), condition is %s, level %d',
      [DirectiveName, DirectiveParam, BoolToStr(IsTrue), FDirectiveLevel]);
    if IsTrue then
    begin
      Inc(FDirectiveLevel);
    end else
    begin
      { The implementation outside assumes that HandleIfDirective
        always "consumes" all the tokens up to the "active" (with satisfied
        condition) block of $ifdef / $if / $else / $elseif sequence.
        This way the implementation outside can just pass through all tokens,
        and "skip until endif" when it sees $else / $elseif.

        This gets complicated in case of $elseif: we must now find the first
        $elseif that has satisfied condition, and then break. }

      repeat
        EndingDirective := SkipUntilElseOrEndif(ElseifToken);
        case EndingDirective of
          DT_ELSE:
            begin
              Inc(FDirectiveLevel);
              Break;
            end;
          DT_ELSEIF:
            begin
              ElseifCondition := ExtractElseifCondition(ElseifToken);
              if IfCondition(ElseifCondition) then
              begin
                Inc(FDirectiveLevel);
                Break;
              end; // otherwise do nothing, let the loop continue
            end;
          else
            { Then it ended with endif / ifend, in which case we just exit
              the loop without touching the FDirectiveLevel. }
            Break;
        end;
      until false;
    end;
  end;

var
  dt: TDirectiveType;
  Finished: Boolean;
  DirectiveName, DirectiveParamBlack, DirectiveParamWhite: string;
begin
  if Assigned(FBufferedToken) then
  begin
    { we have a token buffered, we'll return this one }
    Result := FBufferedToken;
    FBufferedToken := nil;
    Exit;
  end;

  Finished := False;
  repeat
    { check if we have a tokenizer left }
    if (FCurrentTokenizer = -1) then
      raise ETokenizerStreamEnd.Create('Unexpected end of stream', [], 1);

    if FTokenizers[FCurrentTokenizer].HasData then
    begin
      { get next token from tokenizer }
      Result := FTokenizers[FCurrentTokenizer].GetToken;
      try
        { if token is a directive, then we handle it }
        if Result.MyType = TOK_DIRECTIVE then
        begin
          if IdentifyDirective(Result.CommentContent, dt,
            DirectiveName, DirectiveParamBlack, DirectiveParamWhite) then
          begin
            case dt of
              DT_DEFINE:
                HandleDefineDirective(DirectiveParamBlack, DirectiveParamWhite);
              DT_ELSE:
                begin
                  DoMessage(5, pmtInformation, '$ELSE encountered', []);

                  { We encountered $else that should not be entered further
                    (after $ifdef / $if or such with a satisfied condition).

                    Note: An $else that should be entered is always consumed
                    inside HandleIfDirective when the $ifxxx is false.

                    So the only thing we can do is to skip to $endif now. }
                  if FDirectiveLevel <= 0 then
                    DoError(GetStreamInfo + ': unexpected $ELSE directive, without $IFDEF / $IF beginning', [])
                  else
                  if SkipUntilElseOrEndif in [DT_ELSE, DT_ELSEIF] then
                    DoError(GetStreamInfo + ': unexpected $ELSE / $ELSEIF directive, expected $ENDIF / $IFEND now', [])
                  else
                    Dec(FDirectiveLevel);
                end;
              DT_ELSEIF:
                begin
                  DoMessage(5, pmtInformation, '$ELSEIF encountered', []);

                  { We encountered $elseif that should not be entered further
                    (after $ifdef / $if or even previous $elseif
                    with a satisfied condition).
                    So we skip to $endif now. }

                  if FDirectiveLevel <= 0 then
                    DoError(GetStreamInfo + ': unexpected $ELSEIF directive, without $IFDEF / $IF beginning', [])
                  else
                  begin
                    SkipUntilEndif;
                    Dec(FDirectiveLevel);
                  end;
                end;
              DT_ENDIF, DT_IFEND:
                begin
                  DoMessage(5, pmtInformation, '$%s encountered', [DirectiveName]);
                  if (FDirectiveLevel > 0) then
                  begin
                    Dec(FDirectiveLevel);
                    DoMessage(6, pmtInformation, 'FDirectiveLevel = ' + IntToStr(FDirectiveLevel), []);
                  end else
                    DoError(GetStreamInfo + ': unexpected $%s directive', [DirectiveName]);
                end;
              DT_IFDEF: HandleIfDirective(IsSymbolDefined(DirectiveParamBlack),
                'IFDEF', DirectiveParamBlack);
              DT_IFNDEF: HandleIfDirective(not IsSymbolDefined(DirectiveParamBlack),
                'IFNDEF', DirectiveParamBlack);
              DT_IFOPT: HandleIfDirective(IsSwitchDefined(DirectiveParamBlack),
                'IFOPT', DirectiveParamBlack);
              DT_IF: HandleIfDirective(IfCondition(DirectiveParamWhite),
                'IF', DirectiveParamWhite);
              DT_INCLUDE_FILE, DT_INCLUDE_FILE_2:
                begin
                  if (Length(DirectiveParamBlack) >= 2) and
                     (DirectiveParamBlack[1] = '%') and
                     (DirectiveParamBlack[Length(DirectiveParamBlack)] = '%') then
                  begin
                    (* Then this is FPC's feature, see
                      "$I or $INCLUDE : Include compiler info" on
                      [http://www.freepascal.org/docs-html/prog/progsu30.html].

                      Unlike FPC, PasDoc will not expand the %variable%
                      (for reasoning, see comments in
                      ../../tests/ok_include_environment.pas file).
                      We change Result to say that it's a string literal
                      (but we leave Result.Data as it is, to show exact
                      info to the user). We do *not* want to enclose it in
                      quotes, because then real string literal '{$I %DATE%}'
                      wouldn't be different than using {$I %DATE%} feature. *)

                    Result.MyType := TOK_STRING;
                    Break;
                  end else
                  begin
                    OpenIncludeFile(DirectiveParamBlack);
                  end;
                end;
              DT_UNDEF:
                begin
                  DoMessage(6, pmtInformation, 'UNDEF encountered (%s)', [DirectiveParamBlack]);
                  DeleteSymbol(DirectiveParamBlack);
                end;
            end;
          end else
          begin
            ResolveSwitchDirectives(Result.Data);
          end;

          FreeAndNil(Result);
        end else
        if ExpandMacro(Result) then
        begin
          FreeAndNil(Result);
        end else
        begin
          { If the token is not a directive, and not an identifier that expands
            to a macro, then we just return it. }
          Finished := True;
        end;

      except
        FreeAndNil(Result);
        raise;
      end;

    end else
    begin
      DoMessage(5, pmtInformation, 'Closing file "%s"',
        [FTokenizers[FCurrentTokenizer].GetStreamInfo]);
      FTokenizers[FCurrentTokenizer].Free;
      FTokenizers[FCurrentTokenizer] := nil;
      Dec(FCurrentTokenizer);
    end;
  until Finished;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.IsSymbolDefined(const Name: string): Boolean;
begin
  Result := FSymbols.FindName(Name) <> -1;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.IsSwitchDefined(N: string): Boolean;
begin
  { We expect a length 2 AnsiString like 'I+' or 'A-', first character a letter,
    second character plus or minus }
  if Length(N) >= 2 then
    begin;
      if (N[1] >= 'a') and (N[1] <= 'z') then
        N[1] := Char(Ord(N[1]) - 32);
      if (N[1] >= 'A') and (N[1] <= 'Z') and ((N[2] = '-') or (N[2] = '+')) then
        begin
          Result := FSwitchOptions[N[1]] = (N[2] = '+');
          Exit;
        end;
    end;

  DoMessage(2, pmtInformation, GetStreamInfo + ': Invalid $IFOPT parameter (%s).', [N]);
  Result := False;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.OpenNewTokenizer(Stream: TStream;
  const StreamName, StreamPath: string);
var
  Tokenizer: TTokenizer;
begin
  { check if maximum number of FTokenizers has been reached }
  if FCurrentTokenizer = MAX_TOKENIZERS - 1 then
  begin
    Stream.Free;
    DoError('%s: Maximum level of recursion (%d) reached when trying to ' +
      'create new tokenizer "%s" (Probably you have recursive file inclusion ' +
      '(with $include directive) or macro expansion)',
      [GetStreamInfo, MAX_TOKENIZERS, StreamName]);
  end;

  Tokenizer := TTokenizer.Create(Stream, FOnMessage, FVerbosity,
    StreamName, StreamPath);

  { add new tokenizer }
  Inc(FCurrentTokenizer);
  FTokenizers[FCurrentTokenizer] := Tokenizer;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.OpenIncludeFile(n: string);
var
  NLowerCase: string;
  UseLowerCase: boolean;

  { Check for availability of file N inside given Path
    (that must be like after IncludeTrailingPathDelimiter --- either
    '' or ends with PathDelim).
    It yes, then returns @true and opens new tokenizer with
    appropriate stream, else returns false.

    Check both N and NLowerCase
    (on case-sensitive system, filename may be written in exact
    case (like for Kylix) or lowercase (like for FPC 1.0.x),
    FPC >= 2.x accepts both). }
  function TryOpen(const Path: string): boolean;
  var
    Name: string;
  begin
    Name := Path + N;
    DoMessage(5, pmtInformation, 'Trying to open include file "%s"...', [Name]);
    Result := FileExists(Name);

    if (not Result) and UseLowerCase then
    begin
      Name := Path + NLowerCase;
      DoMessage(5, pmtInformation, 'Trying to open include file "%s" (lowercased)...', [Name]);
      Result := FileExists(Name);
    end;

    if Result then
      { create new tokenizer with stream }
    {$IFDEF STRING_UNICODE}
      OpenNewTokenizer(TStreamReader.Create(Name),
        Name, ExtractFilePath(Name));
    {$ELSE}
    {$IFDEF USE_BUFFERED_STREAM}
      OpenNewTokenizer(TBufferedStream.Create(Name, fmOpenRead or fmShareDenyWrite),
        Name, ExtractFilePath(Name));
    {$ELSE}
      OpenNewTokenizer(TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite),
        Name, ExtractFilePath(Name));
    {$ENDIF}
    {$ENDIF}
  end;

  function TryOpenIncludeFilePaths: boolean;
  var
    I: Integer;
  begin
    for I := 0 to IncludeFilePaths.Count - 1 do
      if IncludeFilePaths[I] <> '' then
      begin
        Result := TryOpen(IncludeFilePaths[I]);
        if Result then Exit;
      end;

    Result := false;
  end;

begin
  if (Length(N) > 2) and (N[1] = '''') and (N[Length(N)] = '''') then
    N := Copy(N, 2, Length(N) - 2);

  NLowerCase := LowerCase(N);
  { If NLowerCase = N, avoid calling FileExists twice (as FileExists
    may be costly when generating large docs from many files) }
  UseLowerCase := NLowerCase <> N;

  if not TryOpen(FTokenizers[FCurrentTokenizer].StreamPath) then
    if not TryOpenIncludeFilePaths then
      if not TryOpen('') then
        DoError('%s: could not open include file %s', [GetStreamInfo, n]);
end;

{ ---------------------------------------------------------------------------- }

function TScanner.PeekToken: TToken;
begin
  FBufferedToken := GetToken;
  Result := FBufferedToken;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.SkipUntilElseOrEndif(out ElseifToken: TToken): TDirectiveType;
var
  dt: TDirectiveType;
  Level: Integer;
  DirectiveName, DirectiveParamBlack, DirectiveParamWhite: string;
  t: TToken;
  Stop: Boolean;
begin
  Level := 1;
  ElseifToken := nil;
  repeat
    t := FTokenizers[FCurrentTokenizer].SkipUntilCompilerDirective;
    if t = nil then begin
      DoError('Unexpected end of code when looking for matching $ELSE / $ELSEIF / $ENDIF / $IFEND', []);
    end;

    if (t.MyType = TOK_DIRECTIVE) then begin
      if IdentifyDirective(t.CommentContent,
        dt, DirectiveName, DirectiveParamBlack, DirectiveParamWhite) then
      begin
        DoMessage(6, pmtInformation, 'SkipUntilElseOrFound: encountered directive %s', [DirectiveNames[dt]]);
        case dt of
          DT_IFDEF, DT_IFNDEF, DT_IFOPT, DT_IF: Inc(Level);
          DT_ELSE, DT_ELSEIF:
            { RJ: We must jump over all nested $IFDEFs until its $ENDIF is
              encountered, ignoring all $ELSEs. We must therefore not
              decrement Level at $ELSE if it is part of such a nested $IFDEF.
              $ELSE must decrement Level only for the initial $IFDEF.
              That's why we test Level for 1 (initial $IFDEF) here. }
            if Level = 1 then Dec(Level);
          DT_ENDIF, DT_IFEND: Dec(Level);
        end;
      end;
    end;
    Stop := (Level = 0) and
      (T.MyType = TOK_DIRECTIVE) and
      (dt in [DT_ELSE, DT_ELSEIF, DT_ENDIF, DT_IFEND]);
    if Stop and (dt = DT_ELSEIF) then
    begin
      ElseifToken := T;
      T := nil;
    end;
    FreeAndNil(T);
  until Stop;
  Result := dt;
  DoMessage(6, pmtInformation, 'Skipped code, last directive is %s', [DirectiveNames[dt]]);
end;

function TScanner.SkipUntilElseOrEndif: TDirectiveType;
var
  ElseifToken: TToken;
begin
  Result := SkipUntilElseOrEndif(ElseifToken);
  FreeAndNil(ElseifToken);
end;

procedure TScanner.SkipUntilEndif;
var
  EndingDirective: TDirectiveType;
begin
  repeat
    EndingDirective := SkipUntilElseOrEndif;
  until EndingDirective in [DT_ENDIF, DT_IFEND];
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.UnGetToken(var t: TToken);
begin
  if Assigned(FBufferedToken) then
    DoError('%s: Cannot UnGet more than one token in TScanner.',
      [GetStreamInfo]);

  FBufferedToken := t;
  t := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DoError(const AMessage: string;
  const AArguments: array of const);
begin
  raise EPasDoc.Create(AMessage, AArguments, 1);
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.ResolveSwitchDirectives(const Comment: String);
var
  p: PChar;
  l: Cardinal;
  c: Char;

  procedure SkipWhiteSpace;
  begin
    while (l > 0) and (p^ <= #32) do
      begin
        Inc(p);
        Dec(l);
      end;
  end;

begin
  p := Pointer(Comment);
  l := Length(Comment);

  if l < 4 then Exit;
  case p^ of
    '{':
      begin
        if p[1] <> '$' then Exit;
        Inc(p, 2);
        Dec(l, 2);
      end;
    '(':
      begin
        if (p[1] <> '*') or (p[2] <> '$') then Exit;
        Inc(p, 3);
        Dec(l, 3);
      end;
  else
    Exit;
  end;

  repeat
    SkipWhiteSpace;
    if l < 3 then Exit;

    c := p^;
    if IsCharInSet(c, ['a'..'z']) then
      Dec(c, 32);

    if not IsCharInSet(c, ['A'..'Z']) or not IsCharInSet(p[1], ['-', '+']) then
      Exit;

    FSwitchOptions[c] := p[1] = '+';
    Inc(p, 2);
    Dec(l, 2);

    SkipWhiteSpace;

    // Skip comma
    if (l = 0) or (p^ <> ',') then Exit;
    Inc(p);
    Dec(l);
  until False;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.IfCondition(const Condition: String): Boolean;
var
  Tokenizer: TTokenizer;

  { Get next token that is not whitespace from Tokenizer.
    Return @nil if end of stream. }
  function NextToken: TToken;
  begin
    repeat
      Result := Tokenizer.GetToken(true);
    until (Result = nil) or (Result.MyType <> TOK_WHITESPACE);
  end;

  (* Consume tokens after "defined.
     We handle two forms:
       {$IF DEFINED(MySym)}
     or
       {$IF DEFINED MySym}
  *)
  function ParseDefinedFunctionParameter: Boolean;
  var
    T: TToken;
    SymbolName: string;
  begin
    T := NextToken;
    if T.IsSymbol(SYM_LEFT_PARENTHESIS) then
    begin
      FreeAndNil(T);

      T := NextToken;
      if T.MyType <> TOK_IDENTIFIER then
        raise EInvalidIfCondition.CreateFmt('Expected identifier (symbol name), got %s', [T.Description]);
      SymbolName := T.Data;
      FreeAndNil(T);

      T := NextToken;
      if not T.IsSymbol(SYM_RIGHT_PARENTHESIS) then
        raise EInvalidIfCondition.CreateFmt('Expected ")", got %s', [T.Description]);
      FreeAndNil(T);
    end else
    if T.MyType = TOK_IDENTIFIER then
    begin
      SymbolName := T.Data;
      FreeAndNil(T);
    end else
      raise EInvalidIfCondition.CreateFmt('Expected "(" or symbol name, got %s', [T.Description]);

    Result := IsSymbolDefined(SymbolName);
  end;

  function ParseOptionFunctionParameter: String;
  var
    T: TToken;
  begin
    T := NextToken;
    if not T.IsSymbol(SYM_LEFT_PARENTHESIS) then
      raise EInvalidIfCondition.CreateFmt('Expected "(", got %s', [T.Description]);
    FreeAndNil(T);

    T := NextToken;
    if T.MyType <> TOK_IDENTIFIER then
      raise EInvalidIfCondition.CreateFmt('Expected identifier (option name), got %s', [T.Description]);
    Result := T.Data;
    FreeAndNil(T);

    T := NextToken;
    if T.MyType <> TOK_SYMBOL then
      raise EInvalidIfCondition.CreateFmt('Expected symbol (+ or -), got %s', [T.Description]);
    Result := Result + T.Data;
    FreeAndNil(T);

    T := NextToken;
    if not T.IsSymbol(SYM_RIGHT_PARENTHESIS) then
      raise EInvalidIfCondition.CreateFmt('Expected ")", got %s', [T.Description]);
    FreeAndNil(T);
  end;

(*
  function ParseSimpleFunctionParameter: String;
  var
    T: TToken;
  begin
    T := NextToken;
    if not T.IsSymbol(SYM_LEFT_PARENTHESIS) then
      raise EInvalidIfCondition.CreateFmt('Expected "(", got %s', [T.Description]);
    FreeAndNil(T);

    T := NextToken;
    if T.MyType <> TOK_IDENTIFIER then
      raise EInvalidIfCondition.CreateFmt('Expected identifier (function parameter), got %s', [T.Description]);
    Result := T.Data;
    FreeAndNil(T);

    T := NextToken;
    if not T.IsSymbol(SYM_RIGHT_PARENTHESIS) then
      raise EInvalidIfCondition.CreateFmt('Expected ")", got %s', [T.Description]);
    FreeAndNil(T);
  end;
*)

  { Consume tokens constituting a function, like "defined(xxx)".
    See https://freepascal.org/docs-html/current/prog/progsu127.html . }
  function ParseFunction: Boolean;
  var
    T: TToken;
    Identifier: String;
  begin
    T := NextToken;
    try
      { if it's a number, than anything <> 0 results in true. }
      if T.MyType = TOK_NUMBER then
        Exit(StrToInt(T.Data) <> 0);

      if T.MyType <> TOK_IDENTIFIER then
        raise EInvalidIfCondition.CreateFmt('Expected identifier (function name), got %s', [T.Description]);
      Identifier := LowerCase(T.Data);
    finally FreeAndNil(T) end;

    if Identifier = 'false' then
      Result := false
    else
    if Identifier = 'true' then
      Result := true
    else
    if Identifier = 'defined' then
      Result := ParseDefinedFunctionParameter
    else
    if Identifier = 'undefined' then
      // just negate the result defined(xxx) would have
      Result := not ParseDefinedFunctionParameter
    else
    if Identifier = 'option' then
      Result := IsSwitchDefined(ParseOptionFunctionParameter)
    else
    if Identifier = 'sizeof' then
      raise EInvalidIfCondition.Create('Evaluating "sizeof" function for $if / $elseif not implemented', [])
    else
    if Identifier = 'declared' then
      raise EInvalidIfCondition.Create('Evaluating "declared" function for $if / $elseif not implemented', [])
    else
      raise EInvalidIfCondition.CreateFmt('Unknown function "%s" in $if / $elseif', [Identifier]);
  end;

  { Consume tokens constituting a function
    or a function with "not" at the beginning, like "not defined(xxx)". }
  function ParseFunctionNegated: Boolean;
  var
    T: TToken;
  begin
    T := NextToken;
    if T.IsKeyWord(KEY_NOT) then
    begin
      FreeAndNil(T);
      Result := not ParseFunction;
    end else
    begin
      Tokenizer.UnGetToken(T);
      Result := ParseFunction;
    end;
  end;

  { Consume tokens constituting an expression, like "defined(xxx) or defined(yyy)".
    See https://freepascal.org/docs-html/current/prog/progsu127.html . }
  function ParseExpression: Boolean;
  var
    T: TToken;
  begin
    Result := ParseFunctionNegated;
    T := NextToken;
    if T <> nil then
    begin
      try
        if T.IsKeyWord(KEY_AND) then
          Result := Result and ParseExpression
        else
        if T.IsKeyWord(KEY_OR) then
          Result := Result and ParseExpression
        else
        if T.IsSymbol(SYM_EQUAL) then
          Result := Result = ParseExpression
        else
          raise EInvalidIfCondition.Create('Cannot handle opertator function "%s" in $if / $elseif', [T.Description]);
      finally FreeAndNil(T) end;
    end;
  end;

begin
  Tokenizer := TTokenizer.Create(TStringStream.Create(Condition),
    FOnMessage, FVerbosity, '$if / $elseif condition', '');
  try
    try
      Result := ParseExpression;
    except
      on E: EInvalidIfCondition do
      begin
        DoMessage(2, pmtWarning,
          'Cannot evaluate this $if / $elseif condition, assuming that "%s" is true. Error message is: %s', [Condition, E.Message]);
        Result := true;
      end;
    end;

    { To test NextToken and tokenizer:
    repeat
      T := NextToken;
      if T = nil then Break;
      Writeln('Got token ', T.Description);
      FreeAndNil(T);
    until false;
    }
  finally FreeAndNil(Tokenizer) end;
end;

end.
