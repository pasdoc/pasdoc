unit OptionParser;

interface
uses
  Classes;
  
type
  TOption = class
  protected
    FShort: char;
    FLong: string;
    FShortSens: boolean;
    FLongSens: boolean;
    FExplanation: string;
    FWasSpecified: boolean;
    function ParseOption(const AWords: TStrings): boolean; virtual; abstract;
  public
    constructor Create(const AShort:char; const ALong: string = ''; const AShortCaseSensitive: boolean = True; const ALongCaseSensitive: boolean = false); virtual;

    function GetOptionWidth: Integer;
    procedure WriteExplanation(const AOptWidth: Integer);

    property ShortForm: char read FShort write FShort;
    property LongForm: string read FLong write FLong;
    property ShortCaseSensitive: boolean read FShortSens write FShortSens;
    property LongCaseSensitive: boolean read FLongSens write FLongSens;
    property WasSpecified: boolean read FWasSpecified;
    property Explanation: string read FExplanation write FExplanation;
  end;

  TBoolOption = class(TOption)
  protected
    FTurnedOn: boolean;
    function ParseOption(const AWords: TStrings): boolean; override;
  public
    property TurnedOn: boolean read FTurnedOn;
  end;

  TValueOption = class(TOption)
  protected
    function CheckValue(const AString: String): boolean; virtual; abstract;
    function ParseOption(const AWords: TStrings): boolean; override;
  end;

  TIntegerOption = class(TValueOption)
  protected
    FValue: Integer;
    function CheckValue(const AString: String): boolean; override;
  public
    property Value: Integer read FValue write FValue;
  end;

  TStringOption = class(TValueOption)
  protected
    FValue: String;
    function CheckValue(const AString: String): boolean; override;
  public
    property Value: String read FValue write FValue;
  end;

  TStringOptionList = class(TValueOption)
  protected
    FValues: TStringList;
    function CheckValue(const AString: String): Boolean; override;
  public
    property Values: TStringList read FValues;
    constructor Create(const AShort: Char; const ALong: String = '';
      const AShortCaseSensitive: Boolean = True;
      const ALongCaseSensitive: Boolean = False); override;
    destructor Destroy; override;
  end;

  TOptionParser = class
  protected
    FParams: TStringList;
    FOptions: TList;
    FLeftList: TStringList;
    function GetOptionsCount: Integer;
    function GetOption(const AIndex: Integer): TOption;
  public
    constructor Create; overload; virtual;
    constructor Create(const AParams: TStrings); overload; virtual;
    destructor Destroy; override;
    function AddOption(const AOption: TOption): TOption;
    procedure ParseOptions;

    procedure WriteExplanations;

    property LeftList: TStringList read FLeftList;
    property OptionsCount: Integer read GetOptionsCount;
    property Options[const AIndex: Integer]: TOption read GetOption;
  end;

implementation
uses
  SysUtils;

const
  ShortOptionChar = '-';
  LongOptionString = '--';

{ TOptionParser }

constructor TOptionParser.Create;
begin
  Create(nil);
end;

function TOptionParser.AddOption(const AOption: TOption): TOption;
begin
  FOptions.Add(AOption);
  Result := AOption;
end;

constructor TOptionParser.Create(const AParams: TStrings);
var
  i: Integer;
begin
  inherited Create;
  FParams := TStringList.Create;
  if Assigned(AParams) then begin
    FParams.Assign(AParams);
  end else begin
    for i := 1 to ParamCount do begin
      FParams.Add(ParamStr(i));
    end;
  end;

  FLeftList := TStringList.Create;
  FOptions := TList.Create;
end;

destructor TOptionParser.Destroy;
var
  i: Integer;
begin
  for i := FOptions.Count-1 downto 0 do begin
    TOption(FOptions[i]).Free;
  end;
  FLeftList.Free;
  FParams.Free;
  FOptions.Free;
  inherited;
end;

procedure TOptionParser.ParseOptions;
var
  LCopyList: TStringList;
  i: Integer;
  LFoundSomething: boolean;
begin
  LCopyList := TStringList.Create;
  LCopyList.Assign(FParams);
  FLeftList.Clear;
  try
    while LCopyList.Count > 0 do begin
      LFoundSomething := false;
      for i := 0 to FOptions.Count-1 do begin
        if TOption(FOptions[i]).ParseOption(LCopyList) then begin
          LFoundSomething := true;
          break;
        end;
      end;
      if not LFoundSomething then begin
        FLeftList.Add(LCopyList[0]);
        LCopyList.Delete(0);
      end;
    end;
  finally
    LCopyList.Free;
  end;
end;

function TOptionParser.GetOptionsCount: Integer;
begin
  Result := FOptions.Count;
end;

function TOptionParser.GetOption(const AIndex: Integer): TOption;
begin
  Result := TOption(FOptions[AIndex]);
end;

procedure TOptionParser.WriteExplanations;
  function Max(const A,B: Integer): Integer;
  begin
    if A>B then Result := A else Result := B;
  end;

var
  i: Integer;
  LMaxWidth: Integer;
begin
  LMaxWidth := 0;
  for i := 0 to OptionsCount-1 do begin
    LMaxWidth := Max(LMaxWidth, Options[i].GetOptionWidth);
  end;
  for i := 0 to OptionsCount-1 do begin
    Options[i].WriteExplanation(LMaxWidth);
  end;
end;

{ TOption }

constructor TOption.Create(const AShort: char; const ALong: string;
  const AShortCaseSensitive, ALongCaseSensitive: boolean);
begin
  inherited Create;
  FShort := AShort;
  FLong := ALong;
  FShortSens := AShortCaseSensitive;
  FLongSens := ALongCaseSensitive;
end;

function TOption.GetOptionWidth: Integer;
begin
  Result := 0;
  if ShortForm<>#0 then begin
    Inc(Result, 4); // "-x, "
  end;
  if Length(LongForm)>0 then begin
    Inc(Result, Length(LongForm)+Length(LongOptionString));
  end else begin
    Dec(Result, 2);
  end;
end;

procedure TOption.WriteExplanation(const AOptWidth: Integer);
  procedure WriteBlank(const ANumber: Integer);
  var
    j: Integer;
  begin
    for j := ANumber-1 downto 0 do begin
      Write(' ');
    end;
  end;
  
var
  LLines: TStringList;
  i: Integer;
  LWritten: Integer;
begin
  Write('  ');
  LWritten := 2;
  if ShortForm <> #0 then begin
    Write(ShortOptionChar, ShortForm);
    Inc(LWritten, 2);
    if Length(LongForm)>0  then begin
      Write(', ');
      Inc(LWritten, 2);
    end;
  end;
  if Length(LongForm)>0 then begin
    Write(LongOptionString, LongForm);
    Inc(LWritten, Length(LongOptionString) + Length(LongForm));
  end;
  Write(' ');
  Inc(LWritten, 1);
  LLines := TStringList.Create;
  LLines.Text := WrapText(Explanation, 77 - AOptWidth);
  for i := 0 to LLines.Count-1 do begin
    if Length(LLines[i]) > 0 then begin
      // WrapText has a bug...
      if i = 0 then begin
        WriteBlank(AOptWidth + 4 - LWritten);
      end else begin
        WriteBlank(AOptWidth + 4);
      end;
      WriteLn(LLines[i]);
    end;
  end;
  LLines.Free;
end;

{ TBoolOption }

function TBoolOption.ParseOption(const AWords: TStrings): boolean;
begin
  Result := False;
  if ShortForm <> #0 then begin
    if AWords[0] = ShortOptionChar+ShortForm then begin
      FTurnedOn := True;
      Result := True;
      AWords.Delete(0);
      FWasSpecified := True;
    end else begin
      if (not ShortCaseSensitive) and (LowerCase(AWords[0]) = ShortOptionChar+LowerCase(ShortForm)) then begin
        FTurnedOn := True;
        Result := True;
        AWords.Delete(0);
        FWasSpecified := True;
      end;
    end;
  end;
  
  if (not Result) and (Length(LongForm) > 0) then begin
    if AWords[0] = LongOptionString+LongForm then begin
      FTurnedOn := True;
      Result := True;
      AWords.Delete(0);
      FWasSpecified := True;
    end else begin
      if (not LongCaseSensitive) and (LowerCase(AWords[0]) = LongOptionString+LowerCase(LongForm)) then begin
        FTurnedOn := True;
        Result := True;
        AWords.Delete(0);
        FWasSpecified := True;
      end;
    end;
  end;
end;

{ TValueOption }

function TValueOption.ParseOption(const AWords: TStrings): boolean;
var
  LValue: string;
begin
  Result := False;
  if ShortForm <> #0 then begin
    if (Copy(AWords[0],1,Length(ShortOptionChar+ShortForm)) = ShortOptionChar+ShortForm)
      OR ((not ShortCaseSensitive) and (LowerCase(Copy(AWords[0],1,Length(ShortOptionChar+ShortForm))) = ShortOptionChar+LowerCase(ShortForm))) then begin
      LValue := Copy(AWords[0], Length(ShortOptionChar+ShortForm)+1, MaxInt);
      if LValue = '' then begin
        if AWords.Count>1 then begin
          LValue := AWords[1];
          if CheckValue(LValue) then begin
            Result := True;
            AWords.Delete(0);
            AWords.Delete(0);
          end else begin
            Result := CheckValue('');
            if Result then AWords.Delete(0);
          end;
        end else begin
          Result := CheckValue(LValue);
          if Result then AWords.Delete(0);
        end;
      end else begin
        Result := CheckValue(LValue);
        if Result then AWords.Delete(0);
      end;
    end;
  end;
  if Result then FWasSpecified := True;
  if (not Result) and (Length(LongForm) > 0) then begin
    if (Copy(AWords[0],1,Length(LongOptionString+LongForm)) = LongOptionString+LongForm)
      OR ((not LongCaseSensitive) AND (LowerCase(Copy(AWords[0],1,Length(LongOptionString+LongForm))) = LongOptionString+LowerCase(LongForm))) then begin
      if Length(AWords[0]) = Length(LongOptionString+LongForm) then begin
        if AWords.Count>1 then begin
          LValue := AWords[1];
        end else begin
          LValue := '';
        end;
        Result := CheckValue(LValue);
        if Result then begin
          AWords.Delete(0);
          if AWords.Count>0 then AWords.Delete(0);
        end;
      end else begin
        if Copy(AWords[0], Length(LongOptionString+LongForm)+1, 1) = '=' then begin
          LValue := Copy(AWords[0], Length(LongOptionString+LongForm)+2, MaxInt);
          Result := CheckValue(LValue);
          if Result then AWords.Delete(0);
        end;
      end;
    end;
  end;
  if Result then FWasSpecified := True;
end;

{ TIntegerOption }

function TIntegerOption.CheckValue(const AString: String): boolean;
var
  LValue: Integer;
begin
  Result := TryStrToInt(AString, LValue);
  if Result then FValue := LValue;
end;

{ TStringOption }

function TStringOption.CheckValue(const AString: String): boolean;
begin
  FValue := AString;
  Result := True;
end;

{ TStringOptionList }

function TStringOptionList.CheckValue(const AString: String): Boolean;
begin
  Result := True;
  FValues.Add(AString);
end;

constructor TStringOptionList.Create(const AShort: Char;
  const ALong: String; const AShortCaseSensitive,
  ALongCaseSensitive: Boolean);
begin
  inherited;
  FValues := TStringList.Create;
end;

destructor TStringOptionList.Destroy;
begin
  FValues.Free;
  inherited;
end;

end.
