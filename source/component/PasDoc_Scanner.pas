{
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))

  @abstract(Provides a simplified Pascal scanner.)
  
  The scanner object @link(TScanner) returns tokens from a Pascal language
  character input stream. It uses the @link(Tokenizi) unit to get tokens,
  regarding conditional directives that might lead to including another files
  or will add or delete conditional directives. So, this scanner is a combined
  tokenizer and pre-processor. }

unit PasDoc_Scanner;

interface

uses
  SysUtils,
  Classes,
  PasDoc_Types,
  PasDoc_Tokenizer,
  StringVector;

const
  { maximum number of streams we can recurse into; first one is the unit
    stream, any other stream an include file; current value is 32, increase
    this if you have more include files recursively including others }
  MAX_TOKENIZERS = 32;

type
  { subrange type that has the 26 lower case letters from a to z }
  TLowerCaseLetter = 'a'..'z';
  { an array of boolean values, index type is @link(TLowerCaseLetter) }
  TSwitchOptions = array[TLowerCaseLetter] of Boolean;

  { This class scans one unit using one or more @link(TTokenizer) objects
    to scan the unit and all nested include files. }
  TScanner = class(TObject)
    CT: Integer;
    DirectiveLevel: Integer;
    ErrorMessage: string;
    SwitchOptions: TSwitchOptions;
    Tokenizers: array[0..MAX_TOKENIZERS - 1] of TTokenizer;

  private
    FBufferedToken: TToken;
    FDirectives: TStringVector;
    FIncludeFilePaths: TStringVector;
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;

    { Adds parameter String to the list of directives.
      Returns true on success, false on failure (e.g. in case that there
      wasn't enough memory. }
    procedure AddDirective(const n: string);
    { Removes directive N from the internal list of directives.
      If N was not in that list, nothing is done. }
    procedure DeleteDirective(n: string);
    { Returns if a given directive N is defined at the moment. }
    function IsDirectiveDefined(const n: string): Boolean;
    function IsSwitchDefined(n: string): Boolean;
    function OpenIncludeFile(const n: string): Boolean;
    function SkipUntilElseOrEndif(out FoundElse: Boolean): Boolean;
  protected
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);
  public
    { Creates a TScanner object that scans the given input stream. }
    constructor Create(
      const s: TStream;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal);
    destructor Destroy; override;

    { Adds all directives in the parameter String collection by calling
      @link(AddDirective) for each of the strings in that collection. }
    procedure AddDirectives(const DL: TStringVector);
    { Gets next token and throws it away. }
    procedure ConsumeToken;

    { Returns next token as parameter. Returns true on success, false on error. }
    function GetToken(var t: TToken): Boolean;
    { Returns the name of the file that is currently processed and the line
      number. Good for meaningful error messages. }
    function GetStreamInfo: string;
    property IncludeFilePaths: TStringVector read FIncludeFilePaths write
      FIncludeFilePaths;
    function PeekToken(var t: TToken): Boolean;
    procedure UnGetToken(var t: TToken);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property Verbosity: Cardinal read FVerbosity write FVerbosity;
  end;

implementation

uses
  Utils;

type
  { all directives a scanner is going to regard }
  TDirectiveType = (DT_DEFINE, DT_ELSE, DT_ENDIF, DT_IFDEF, DT_IFNDEF,
    DT_IFOPT, DT_INCLUDE_FILE, DT_UNDEF, DT_UNKNOWN);

const
  NUM_RECTIVES = 8;
  DirectiveNames: array[DT_DEFINE..DT_UNDEF] of string[6] =
  ('DEFINE', 'ELSE', 'ENDIF', 'IFDEF', 'IFNDEF', 'IFOPT', 'I', 'UNDEF');

  { this function recognizes only those directives we'll need for the scanner }

function SplitDirective(const t: string; var DirectiveName, Params: string):
  Boolean;
var
  i: Integer;
  l: Integer;
begin
  Result := False;
  i := 0;
  DirectiveName := '';
  Params := '';
  { find dollar sign }
  l := Length(t);
  while (i <= l) and (t[i] <> '$') do
    Inc(i);
  if i > l then Exit;
  Inc(i);

  { get directive name }
  while (i <= l) and not (t[i] in [#32, '*', '}']) do begin
    DirectiveName := DirectiveName + UpCase(t[i]);
    Inc(i);
  end;
  Result := True;
  if t[i] in ['*', '}'] then Exit;

  { skip spaces }
  while (i <= l) and (t[i] = ' ') do
    Inc(i);
  if i > l then Exit;

  { get parameters - no conversion to uppercase here, it could be an include
    file name whose name need not be changed (platform.inc <> PLATFORM.INC) }
  while (i <= l) and not (t[i] in [#32, '*', '}']) do begin
    Params := Params + t[i];
    Inc(i);
  end;
end;

{ ---------------------------------------------------------------------------- }

function IdentifyDirective(const t: string; out dt: TDirectiveType; var p:
  string): Boolean;
var
  i: TDirectiveType;
  s: string;
begin
  Result := False;
  if not SplitDirective(t, s, p) then Exit;
  s := LowerCase(s);
  for i := DT_DEFINE to DT_UNDEF do
    if s = LowerCase(DirectiveNames[i]) then begin
      dt := i;
      Result := True;
      Exit;
    end;
end;

{ ---------------------------------------------------------------------------- }
{ TScanner
{ ---------------------------------------------------------------------------- }

constructor TScanner.Create(
  const s: TStream;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal);
var
  c: TLowerCaseLetter;
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  for c := Low(SwitchOptions) to High(SwitchOptions) do
    SwitchOptions[c] := False;

  Tokenizers[0] := TTokenizer.Create(s, OnMessageEvent, VerbosityLevel);

  FBufferedToken := nil;
end;

{ ---------------------------------------------------------------------------- }

destructor TScanner.Destroy;
var
  i: Integer;
begin
  FDirectives.Free;

  for i := 0 to CT do
    Tokenizers[i].Free;

  FBufferedToken.Free;
  
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.AddDirective(const n: string);
begin
  if not IsDirectiveDefined(n) then begin
    if FDirectives = nil then FDirectives := NewStringVector;
    FDirectives.Add(n);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.AddDirectives(const DL: TStringVector);
var
  i: Integer;
begin
  if DL <> nil then
    for i := 0 to DL.Count - 1 do
      AddDirective(DL[i])
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.ConsumeToken;
begin
  FBufferedToken := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DeleteDirective(n: string);
begin
  if FDirectives <> nil then begin
    FDirectives.RemoveAllNamesCI(n);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.GetStreamInfo: string;
begin
  if (CT >= 0) then
    Result := Tokenizers[CT].GetStreamInfo
  else
    Result := '';
end;

{ ---------------------------------------------------------------------------- }

function TScanner.GetToken(var t: TToken): Boolean;
var
  dt: TDirectiveType;
  Finished: Boolean;
  FoundElse: Boolean;
  p: string;
begin
  Assert(t = nil);
  if Assigned(FBufferedToken) then begin
      { we have a token buffered, we'll return this one }
    t := FBufferedToken;
    FBufferedToken := nil;
    Result := True;
    Exit;
  end;
  Result := False;
  Finished := False;
  repeat
    { check if we have a tokenizer left }
    if (CT = -1) then
      DoError('End of stream reached while trying to get next token.', []);

    if Tokenizers[CT].HasData then begin
        { get next token from tokenizer }
      t := Tokenizers[CT].GetToken;
        { check if token is a directive }
      if (t.MyType = TOK_RECTIVE) then begin
        if not IdentifyDirective(t.Data, dt, p) then begin
          t.Free;
          Continue;
        end;
        case dt of
          DT_DEFINE: begin
              DoMessage(6, mtInformation, 'DEFINE encountered (' + p + ')',
                []);
              AddDirective(p);
            end;
          DT_ELSE: begin
              DoMessage(5, mtInformation, 'ELSE encountered', []);
              if (DirectiveLevel > 0) then begin
                      // RJ Dec(DirectiveLevel);
                if not SkipUntilElseOrEndif(FoundElse) then Exit;
                if not FoundElse then Dec(DirectiveLevel); // RJ
              end
              else begin
                ErrorMessage := GetStreamInfo +
                  ': unexpected $ELSE directive.';
                Exit;
              end;
            end;
          DT_ENDIF: begin
              DoMessage(5, mtInformation, 'ENDIF encountered', []);
              if (DirectiveLevel > 0) then begin
                Dec(DirectiveLevel);
                DoMessage(6, mtInformation, 'DirectiveLevel = ' +
                  IntToStr(DirectiveLevel), []);
              end
              else begin
                ErrorMessage := GetStreamInfo +
                  ': unexpected $ENDIF directive.';
                Exit;
              end;
            end;
          DT_IFDEF: begin
              if IsDirectiveDefined(p) then begin
                Inc(DirectiveLevel);
                DoMessage(6, mtInformation,
                  'IFDEF encountered (%s), defined, level %d', [p,
                  DirectiveLevel]);
              end
              else begin
                DoMessage(6, mtInformation,
                  'IFDEF encountered (%s), not defined, level %d', [p,
                  DirectiveLevel]);
                if not SkipUntilElseOrEndif(FoundElse) then Exit;
                if FoundElse then
                  Inc(DirectiveLevel);
              end;
            end;
          DT_IFNDEF: begin
              if not IsDirectiveDefined(p) then begin
                Inc(DirectiveLevel);
                DoMessage(6, mtInformation,
                  'IFNDEF encountered (%s), not defined, level %d', [p,
                  DirectiveLevel]);
              end
              else begin
                DoMessage(6, mtInformation,
                  'IFNDEF encountered (%s), defined, level %d', [p,
                  DirectiveLevel]);
                if not SkipUntilElseOrEndif(FoundElse) then Exit;
                if FoundElse then
                  Inc(DirectiveLevel);
              end;
            end;
          DT_IFOPT: begin
              if (not IsSwitchDefined(p)) then begin
                if (not SkipUntilElseOrEndif(FoundElse)) then Exit;
                if FoundElse then Inc(DirectiveLevel);
              end
              else
                Inc(DirectiveLevel);
            end;
          DT_INCLUDE_FILE:
            if not OpenIncludeFile(p) then
              DoError(GetStreamInfo + ': Error, could not open include file "'
                + p + '"', []);
          DT_UNDEF: begin
              DoMessage(6, mtInformation, 'UNDEF encountered (%s)', [p]);
              DeleteDirective(p);
            end;
        end;
      end;
      if t.MyType = TOK_RECTIVE then begin
        t.Free;
        t := nil;
      end else begin
        Finished := True;
      end;
    end else begin
      DoMessage(5, mtInformation, 'Closing file "%s"',
        [Tokenizers[CT].GetStreamInfo]);
      Tokenizers[CT].Free;
      Tokenizers[CT] := nil;
      Dec(CT);
    end;
  until Finished;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.IsDirectiveDefined(const n: string): Boolean;
begin
  Result := (FDirectives <> nil) and FDirectives.ExistsNameCI(n);
end;

{ ---------------------------------------------------------------------------- }

function TScanner.IsSwitchDefined(n: string): Boolean;
var
  b1: Boolean;
  b2: Boolean;
  l: TLowerCaseLetter;
begin
  { we expect a length 2 String like 'I+' or 'A-', first character a letter,
    second character plus or minus }
  if ((n[1] >= 'A') and (n[1] <= 'Z')) then n[1] := Chr(Ord(n[1]) + 32);
  if (Length(n) < 2) or
    ((n[1] < 'a') and (n[1] > 'z')) or
    ((n[2] <> '-') and (n[2] <> '+')) then begin
    DoMessage(2, mtInformation, GetStreamInfo +
      ': Warning - invalid $ifopt parameters.', []);
    Result := False;
  end
  else begin
      { look up switch from current table }
    l := TLowerCaseLetter(Ord(n[1]) - Ord('a'));
    b1 := SwitchOptions[l];
      { get status from parameter }
    b2 := (n[2] = '+');
    Result := (b1 = b2);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.OpenIncludeFile(const n: string): Boolean;
var
  i: Integer;
  Name: string;
  NumAttempts: Integer;
  p: string;
  s: TStream;
begin
  { check if maximum number of tokenizers has been reached }
  if CT = MAX_TOKENIZERS - 1 then begin
    DoError('%s: maximum number of tokenizers reached.', [GetStreamInfo]);
  end;

  { determine how many names we can check; number is 1 + IncludeFilePaths.Count }
  NumAttempts := 1;

  if IncludeFilePaths <> nil then
    Inc(NumAttempts, IncludeFilePaths.Count);

  s := nil;
  { loop until we have checked all names or one attempt was successful }
  for i := 0 to NumAttempts - 1 do begin
    if i = 0 then
      Name := n
    else begin
      p := IncludeFilePaths[i - 1];
      if p <> '' then
        Name := p + n
      else
        Continue; { next loop iteration }
    end;
    DoMessage(5, mtInformation, 'Trying to open include file "%s"...', [Name]);
    try
      if FileExists(Name) then begin
        s := TFileStream.Create(Name, fmOpenRead);
      end;
    except
      on EFileStreamError do ;
    end;
    if Assigned(s) then Break;
  end;

  { if we still don't have a valid open stream we failed }
  if not Assigned(s) then begin
    DoError('%s: could not open include file %s', [GetStreamInfo, n]);
  end;

  { create new tokenizer with stream }
  Inc(CT);
  Tokenizers[CT] := TTokenizer.Create(s, FOnMessage, FVerbosity);

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.PeekToken(var t: TToken): Boolean;
begin
  if Assigned(FBufferedToken) then begin
    t := FBufferedToken;
    Result := True;
  end else begin
    if GetToken(t) then begin
      FBufferedToken := t;
      Result := True;
    end else begin
      Result := False;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TScanner.SkipUntilElseOrEndif(out FoundElse: Boolean): Boolean;
var
  dt: TDirectiveType;
  Level: Integer;
  p: string;
  t: TToken;
  TT: TTokenType;
begin
//  Result := False; // no hint
  Level := 1;
  repeat
    t := Tokenizers[CT].GetToken;
    if t = nil then
      DoError('SkipUntilElseOrEndif GetToken', []);

    {writeln('token ', t.Data^.GetString);}
    if (t.MyType = TOK_RECTIVE) then begin
      if IdentifyDirective(t.Data, dt, p) then begin
        DoMessage(6, mtInformation,
          'SkipUntilElseOrFound: encountered directive %s',
          [DirectiveNames[dt]]);
        case dt of
          DT_IFDEF,
            DT_IFNDEF,
            DT_IFOPT: Inc(Level);
          DT_ELSE:
                { RJ: We must jump over all nested $IFDEFs until its $ENDIF is
                  encountered, ignoring all $ELSEs. We must therefore not
                  decrement Level at $ELSE if it is part of such a nested $IFDEF.
                  $ELSE must decrement Level only for the initial $IFDEF.
                  That's why whe test Level for 1 (initial $IFDEF) here. }
            if Level = 1 then Dec(Level);
          DT_ENDIF: Dec(Level);
        end;
      end;
    end;
    TT := t.MyType;
    t.Free;
  until (Level = 0) and (TT = TOK_RECTIVE) and ((dt = DT_ELSE) or (dt =
    DT_ENDIF));
  FoundElse := (dt = DT_ELSE);
  if FoundElse then
    p := 'ELSE'
  else
    p := 'ENDIF';
  DoMessage(6, mtInformation, 'Skipped code, last token ' + p, []);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.UnGetToken(var t: TToken);
begin
  if Assigned(FBufferedToken) then
    DoError('%s: FATAL ERROR - CANNOT UNGET MORE THAN ONE TOKEN.',
      [GetStreamInfo]);

  FBufferedToken := t;
  t := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Integer);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TScanner.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity < FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

end.
