unit PasDoc_SortSettings;

{$I pasdoc_defines.inc}

interface

uses SysUtils;

type
  EInvalidSortSetting = class(Exception);

  TSortSetting = (
    { At unit (TPasUnit) level : } { }
    ssCIOs, ssConstants, ssFuncsProcs, ssTypes, ssVariables, ssUsesClauses,
    { At CIO (TPasCio) level : } { }
    ssRecordFields, ssNonRecordFields, ssMethods, ssProperties);
  TSortSettings = set of TSortSetting;

const
  AllSortSettings: TSortSettings = [Low(TSortSetting) .. High(TSortSetting)];
  
  { Must be lowercase.
    Used in @link(SortSettingsToName), @link(SortSettingFromName). }
  SortSettingNames: array[TSortSetting] of string = (
    'structures', 'constants', 'functions', 'types', 'variables', 'uses-clauses',
    'record-fields', 'non-record-fields', 'methods', 'properties' );

{ @raises(EInvalidSortSetting if ASortSettingName does not match 
  (case ignored) to any SortSettingNames.) }
function SortSettingFromName(const SortSettingName: string): TSortSetting;

{ Comma-separated list }
function SortSettingsToName(const SortSettings: TSortSettings): string;

implementation

function SortSettingFromName(const SortSettingName: string): TSortSetting;
var S: string;
begin
  S := LowerCase(SortSettingName);
  for Result := Low(Result) to High(Result) do
    if S = SortSettingNames[Result] then
      Exit;
  raise EInvalidSortSetting.CreateFmt('Invalid sort specifier "%s"', 
    [SortSettingName]);
end;

function SortSettingsToName(const SortSettings: TSortSettings): string;
var SS: TSortSetting;
begin
  Result := '';
  for SS := Low(SS) to High(SS) do
    if SS in SortSettings then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + SortSettingNames[SS];
    end;
end;

end.