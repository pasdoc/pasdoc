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

{ @abstract(Sorting settings types and names.)}
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