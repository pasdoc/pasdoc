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

{ @abstract(The @name unit --- easing command line parsing)
  @author(Johannes Berg <johannes@sipsolutions.de>)

  To use this unit, create an object of @link(TOptionParser) and add options to
  it, each option descends from @link(TOption).
  Then, call your object's @link(TOptionParser.ParseOptions)
  method and options are parsed.
  After parsing, examine your option objects. }
unit PasDoc_OptionParser;

{$I pasdoc_defines.inc}

interface
uses
  Classes
{$IFDEF USE_VARIANTS},
  Variants
{$ENDIF};

const
  { default short option character used }
  DefShortOptionChar = '-';
  { default long option string used }
  DefLongOptionString = '--';

type
  TOptionParser = class;
  { @abstract(abstract base class for options)
    This class implements all the basic functionality and provides
    abstract methods for the @link(TOptionParser) class to call, which are
    overridden by descendants.
    It also provides function to write the explanation.
  }
  TOption = class
  protected
    FShort: char;
    FLong: string;
    FShortSens: boolean;
    FLongSens: boolean;
    FExplanation: string;
    FWasSpecified: boolean;
    FParser: TOptionParser;
    function ParseOption(const AWords: TStrings): boolean; virtual; abstract;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;
{$ENDIF}
  public
    { Create a new Option.
      Set AShort to #0 in order to have no short option.
      Technically you can set ALong to '' to have no long option,
      but in practive *every* option should have long form.
      Don't override this in descendants (this always simply calls
      CreateEx). Override only CreateEx. }
    constructor Create(const AShort:char; const ALong: string);

    constructor CreateEx(const AShort:char; const ALong: string;
      const AShortCaseSensitive, ALongCaseSensitive: boolean); virtual;

    { returns the width of the string "-s, @--long-option" where s is the short option.
      Removes non-existant options (longoption = '' or shortoption = #0) }
    function GetOptionWidth: Integer;
    { writes the wrapped explanation including option format,
      AOptWidth determines how much it is indented & wrapped }
    procedure WriteExplanation(const AOptWidth: Integer);
    { Short form of the option --- single character --- if #0 then not used }
    property ShortForm: char read FShort write FShort;
    { long form of the option --- string --- if empty, then not used }
    property LongForm: string read FLong write FLong;
    { specified whether the short form should be case sensitive or not }
    property ShortCaseSensitive: boolean read FShortSens write FShortSens;
    { specifies whether the long form should be case sensitive or not }
    property LongCaseSensitive: boolean read FLongSens write FLongSens;
    { signifies if the option was specified at least once }
    property WasSpecified: boolean read FWasSpecified;
    { explanation for the option, see also @link(WriteExplanation) }
    property Explanation: string read FExplanation write FExplanation;
{$IFDEF USE_VARIANTS}
    { Value as Variant --- for easier access through the @link(TOptionParser.ByName) property }
    property Value: Variant read GetValue write SetValue;
{$ENDIF}
  end;

  { @abstract(simple boolean option)
    turned off when not specified,
    turned on when specified. Cannot handle @--option=false et al. }
  TBoolOption = class(TOption)
  protected
    function ParseOption(const AWords: TStrings): boolean; override;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
{$ENDIF}
  public
    property TurnedOn: boolean read FWasSpecified;
  end;

  { @abstract(base class for all options that values)
    base class for all options that take one or more values
    of the form @--option=value or @--option value etc }
  TValueOption = class(TOption)
  protected
    function CheckValue(const AString: String): boolean; virtual; abstract;
    function ParseOption(const AWords: TStrings): boolean; override;
  end;

  { @abstract(Integer option)
    accepts only integers }
  TIntegerOption = class(TValueOption)
  protected
    FValue: Integer;
    function CheckValue(const AString: String): boolean; override;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
{$ENDIF}
  public
    property Value: Integer read FValue write FValue;
  end;

  { @abstract(String option)
    accepts a single string }
  TStringOption = class(TValueOption)
  protected
    FValue: String;
    function CheckValue(const AString: String): boolean; override;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
{$ENDIF}
  public
    property Value: String read FValue write FValue;
  end;

  { @abstract(stringlist option)
    accepts multiple strings and collates them
    even if the option itself is specified more than one time }
  TStringOptionList = class(TValueOption)
  protected
    FValues: TStringList;
    function CheckValue(const AString: String): Boolean; override;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
{$ENDIF}
  public
    property Values: TStringList read FValues;
    constructor CreateEx(const AShort: Char; const ALong: String;
      const AShortCaseSensitive, ALongCaseSensitive: Boolean); override;
    destructor Destroy; override;
  end;

  { @abstract(pathlist option)
    accepts multiple strings paths and collates them
    even if the option itself is specified more than one time.
    Paths in a single option can be separated by the
    DirectorySeparator }
  TPathListOption = class(TStringOptionList)
    function CheckValue(const AString: String): Boolean; override;
  end;

  { @abstract(useful for making a choice of things)
    Values must not
    have a + or - sign as the last character as that can be used
    to add/remove items from the default set, specifying items without
    +/- at the end clears the default and uses only specified items }
  TSetOption = class(TValueOption)
  protected
    FPossibleValues,
    FValues: TStringList;
    function GetPossibleValues: string;
    procedure SetPossibleValues(const Value: string);
    function CheckValue(const AString: String): Boolean; override;
{$IFDEF USE_VARIANTS}
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
{$ENDIF}
    function GetValues: string;
    procedure SetValues(const Value: string);
  public
    property PossibleValues: string read GetPossibleValues write SetPossibleValues;
    constructor CreateEx(const AShort: Char; const ALong: String;
      const AShortCaseSensitive, ALongCaseSensitive: Boolean); override;
    destructor Destroy; override;

    function HasValue(const AValue: string): boolean;

    property Values: string read GetValues write SetValues;
  end;

  { @abstract(OptionParser --- instantiate one of these for commandline parsing)
    This class is the main parsing class, although a lot of parsing is handled
    by @link(TOption) and its descendants instead. }
  TOptionParser = class
  protected
    FParams: TStringList;
    FOptions: TList;
    FLeftList: TStringList;
    FShortOptionChar: Char;
    FLongOptionString: string;
    function GetOption(const AIndex: Integer): TOption;
    function GetOptionsCount: Integer;
    function GetOptionByLongName(const AName: string): TOption;
    function GetOptionByShortname(const AName: char): TOption;
  public
    { Create without any options --- this will parse the current command line }
    constructor Create; virtual;
    { Create with parameters to be used instead of command line }
    constructor CreateParams(const AParams: TStrings); virtual;
    { destroy the option parser object and all associated @link(TOption) objects }
    destructor Destroy; override;
    { Add a @link(TOption) descendant to be included in parsing the command line }
    function AddOption(const AOption: TOption): TOption;
    { Parse the specified command line, see also @link(Create) }
    procedure ParseOptions;
    { output explanations for all options to stdout, will nicely format the
      output and wrap explanations }
    procedure WriteExplanations;
    { This StringList contains all the items from the command line that could
      not be parsed. Includes options that didn't accept their value and
      non-options like filenames specified on the command line }
    property LeftList: TStringList read FLeftList;
    { The number of option objects that were added to this parser }
    property OptionsCount: Integer read GetOptionsCount;
    { retrieve an option by index --- you can use this and @link(OptionsCount)
      to iterate through the options that this parser owns }
    property Options[const AIndex: Integer]: TOption read GetOption;
    { retrieve an option by its long form. Case sensitivity of the options
      is taken into account! }
    property ByName[const AName: string]: TOption read GetOptionByLongName;
    { retrieve an option by its short form. Case sensitivity of the options
      is taken into account! }
    property ByShortName[const AName: char]: TOption read GetOptionByShortname;
    { introductory character to be used for short options }
    property ShortOptionStart: Char read FShortOptionChar write FShortOptionChar default DefShortOptionChar;
    { introductory string to be used for long options }
    property LongOptionStart: String read FLongOptionString write FLongOptionString;
  end;

implementation
uses
  SysUtils;

function TryStrToInt(const AString: string; var AValue: Integer): Boolean;
var
  LError: Integer;
begin
  Val(AString, AValue, LError);
  Result := LError = 0;
end;

{ TOptionParser }

constructor TOptionParser.Create;
begin
  CreateParams(nil);
end;

function TOptionParser.AddOption(const AOption: TOption): TOption;
begin
  FOptions.Add(AOption);
  Result := AOption;
  AOption.FParser := Self;
end;

constructor TOptionParser.CreateParams(const AParams: TStrings);
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

  FLongOptionString := DefLongOptionString;
  FShortOptionChar := DefShortOptionChar;
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

function TOptionParser.GetOptionByLongName(const AName: string): TOption;
var
  i: Integer;
begin
  Result := nil;
  for i := GetOptionsCount-1 downto 0 do begin
    if  (Options[i].LongForm = AName)
        OR (Options[i].LongCaseSensitive AND (LowerCase(Options[i].LongForm) = LowerCase(AName))) then begin
      Result := Options[i];
      break;
    end;
  end;
end;

function TOptionParser.GetOptionByShortname(const AName: char): TOption;
var
  i: Integer;
begin
  Result := nil;
  for i := GetOptionsCount-1 downto 0 do begin
    if  (Options[i].ShortForm = AName)
        OR (Options[i].LongCaseSensitive AND (LowerCase(Options[i].ShortForm) = LowerCase(AName))) then begin
      Result := Options[i];
      break;
    end;
  end;
end;

{ TOption }

constructor TOption.CreateEx(const AShort: char; const ALong: string;
  const AShortCaseSensitive, ALongCaseSensitive: boolean);
begin
  inherited Create;
  FShort := AShort;
  FLong := ALong;
  FShortSens := AShortCaseSensitive;
  FLongSens := ALongCaseSensitive;
end;

constructor TOption.Create(const AShort: char; const ALong: string);
begin
  CreateEx(AShort, ALong, True, False);
end;

function TOption.GetOptionWidth: Integer;
begin
  Result := 0;
  if ShortForm<>#0 then begin
    Inc(Result, 4); // "-x, "
  end;
  if Length(LongForm)>0 then begin
    Inc(Result, Length(LongForm)+Length(FParser.LongOptionStart));
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
    Write(FParser.ShortOptionStart, ShortForm);
    Inc(LWritten, 2);
    if Length(LongForm)>0  then begin
      Write(', ');
      Inc(LWritten, 2);
    end;
  end;
  if Length(LongForm)>0 then begin
    Write(FParser.LongOptionStart, LongForm);
    Inc(LWritten, Length(FParser.LongOptionStart) + Length(LongForm));
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

{$IFDEF USE_VARIANTS}
function TBoolOption.GetValue: Variant;
begin
  Result := WasSpecified;
end;
{$ENDIF}

function TBoolOption.ParseOption(const AWords: TStrings): boolean;
begin
  Result := False;
  if ShortForm <> #0 then begin
    if AWords[0] = FParser.ShortOptionStart+ShortForm then begin
      Result := True;
      AWords.Delete(0);
      FWasSpecified := True;
    end else begin
      if (not ShortCaseSensitive) and (LowerCase(AWords[0]) = FParser.ShortOptionStart+LowerCase(ShortForm)) then begin
        Result := True;
        AWords.Delete(0);
        FWasSpecified := True;
      end;
    end;
  end;

  if (not Result) and (Length(LongForm) > 0) then begin
    if AWords[0] = FParser.LongOptionStart+LongForm then begin
      Result := True;
      AWords.Delete(0);
      FWasSpecified := True;
    end else begin
      if (not LongCaseSensitive) and (LowerCase(AWords[0]) = FParser.LongOptionStart+LowerCase(LongForm)) then begin
        Result := True;
        AWords.Delete(0);
        FWasSpecified := True;
      end;
    end;
  end;
end;

{$IFDEF USE_VARIANTS}
procedure TBoolOption.SetValue(const AValue: Variant);
begin
// do nothing, this option can either be specified or not
end;
{$ENDIF}

{ TValueOption }

function TValueOption.ParseOption(const AWords: TStrings): boolean;
var
  LValue: string;
begin
  Result := False;
  if ShortForm <> #0 then begin
    if (Copy(AWords[0],1,Length(FParser.ShortOptionStart+ShortForm)) = FParser.ShortOptionStart+ShortForm)
      OR ((not ShortCaseSensitive) and (LowerCase(Copy(AWords[0],1,Length(FParser.ShortOptionStart+ShortForm))) = FParser.ShortOptionStart+LowerCase(ShortForm))) then begin
      LValue := Copy(AWords[0], Length(FParser.ShortOptionStart+ShortForm)+1, MaxInt);
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
    if (Copy(AWords[0],1,Length(FParser.LongOptionStart+LongForm)) = FParser.LongOptionStart+LongForm)
      OR ((not LongCaseSensitive) AND (LowerCase(Copy(AWords[0],1,Length(FParser.LongOptionStart+LongForm))) = FParser.LongOptionStart+LowerCase(LongForm))) then begin
      if Length(AWords[0]) = Length(FParser.LongOptionStart+LongForm) then begin
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
        if Copy(AWords[0], Length(FParser.LongOptionStart+LongForm)+1, 1) = '=' then begin
          LValue := Copy(AWords[0], Length(FParser.LongOptionStart+LongForm)+2, MaxInt);
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

{$IFDEF USE_VARIANTS}
function TIntegerOption.GetValue: Variant;
begin
  Result := FValue;
end;
{$ENDIF}

{$IFDEF USE_VARIANTS}
procedure TIntegerOption.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;
{$ENDIF}

{ TStringOption }

function TStringOption.CheckValue(const AString: String): boolean;
begin
  FValue := AString;
  Result := True;
end;

{$IFDEF USE_VARIANTS}
function TStringOption.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TStringOption.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;
{$ENDIF}

{ TStringOptionList }

function TStringOptionList.CheckValue(const AString: String): Boolean;
begin
  Result := True;
  FValues.Add(AString);
end;

constructor TStringOptionList.CreateEx(const AShort: Char;
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

{$IFDEF USE_VARIANTS}
function TStringOptionList.GetValue: Variant;
begin
  Result := FValues.Text;
end;

procedure TStringOptionList.SetValue(const AValue: Variant);
begin
  FValues.Text := AValue;
end;
{$ENDIF}

{ TSetOption }

function TSetOption.CheckValue(const AString: String): Boolean;
var
  LList,
  LResult: TStringList;
  i: Integer;
  s: string;
  si: Integer;
  LCleared: boolean;
begin
  Result := True;
  LCleared := false;
  LList := TStringList.Create;
  LResult := TStringList.Create;
  try
    LList.Duplicates := dupIgnore;
    LList.CommaText := AString;
    LList.Sorted := True;
    LResult.Assign(FValues); // default values
    LResult.Duplicates := dupIgnore;
    LResult.Sorted := True;
    i := 0;
    while i < LList.Count do begin
      s := LList[i];
      if Length(s) = 0 then continue;
      case s[length(s)] of
        '-': begin
               SetLength(s, Length(s)-1);
               if FPossibleValues.IndexOf(s) >= 0 then begin
                 si := LResult.IndexOf(s);
                 if si>=0 then begin
                   LResult.Delete(si);
                 end;
               end else begin
                 Result := false;
                 break;
               end;
             end;
        '+': begin
               SetLength(s, Length(s)-1);
               if FPossibleValues.IndexOf(s) >= 0 then begin
                 LResult.Add(s);
               end else begin
                 Result := false;
                 break;
               end;
             end;
        else begin
               if FPossibleValues.IndexOf(s) >= 0 then begin
                 LResult.Add(s);
               end else begin
                 Result := false;
                 break;
               end;
               if not LCleared then begin
                 LCleared := True;
                 LResult.Clear;
                 i := -1; // restart from beginning
               end;
             end;
      end;
      Inc(i);
    end;
  finally
    LList.Free;
    FValues.Assign(LResult);
    LResult.Free;
  end;
end;

constructor TSetOption.CreateEx(const AShort: Char; const ALong: String;
  const AShortCaseSensitive, ALongCaseSensitive: Boolean);
begin
  inherited;
  FPossibleValues := TStringList.Create;
  FPossibleValues.Duplicates := dupIgnore;
  FPossibleValues.Sorted := True;
  FValues := TStringList.Create;
  FValues.Duplicates := dupIgnore;
  FValues.Sorted := True;
end;

destructor TSetOption.Destroy;
begin
  FPossibleValues.Free;
  FValues.Free;
  inherited;
end;

function TSetOption.GetPossibleValues: string;
begin
  Result := FPossibleValues.CommaText;
end;

{$IFDEF USE_VARIANTS}
function TSetOption.GetValue: Variant;
begin
  Result := FValues.CommaText;
end;
{$ENDIF}

function TSetOption.GetValues: string;
begin
  Result := FValues.CommaText;
end;

function TSetOption.HasValue(const AValue: string): boolean;
begin
  Result := FValues.IndexOf(AValue)>=0;
end;

procedure TSetOption.SetPossibleValues(const Value: string);
begin
  FPossibleValues.CommaText := Value;
end;

{$IFDEF USE_VARIANTS}
procedure TSetOption.SetValue(const AValue: Variant);
begin
  FValues.CommaText := AValue;
end;
{$ENDIF}

procedure TSetOption.SetValues(const Value: string);
begin
  FValues.CommaText := Value;
end;

{ TPathListOption }

{$IFNDEF DELPHI_6_UP}
{$IFDEF FPC}
const
  sLineBreak = LineEnding;
  PathSep    = PathSeparator;
{$ELSE}
{$IFNDEF KYLIX}
const
  sLineBreak = #13#10;
  PathSep    = ';';
{$ENDIF}
{$ENDIF}
{$ENDIF}

function TPathListOption.CheckValue(const AString: String): Boolean;
var
  LValues: TStringList;
begin
  Result := true;
  LValues := TStringList.Create;
  LValues.Text := StringReplace(AString, PathSep, sLineBreak, [rfReplaceAll]);
  FValues.AddStrings(LValues);
  LValues.Free;
end;

end.
