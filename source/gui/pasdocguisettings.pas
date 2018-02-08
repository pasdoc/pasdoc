{
  Copyright 1998-2018 PasDoc developers.

  This file is part of "pasdoc_gui".

  "pasdoc_gui" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "pasdoc_gui" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "pasdoc_gui"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ This unit provides @link(IniFile) variable that can be used to store
  some user configuration. It also declares some global variables
  that are part of user configuration and don't fit anywhere else,
  like WWWHelpServer.

  @author(Michalis Kamburelis) }


unit PasDocGuiSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;
  
var
  IniFile: TIniFile;

const
  DefaultWWWHelpServer = 'https://github.com/pasdoc/pasdoc/wiki/';

var
  WWWHelpServer: string;

implementation

var
  ConfigDir: string;
initialization
  { calculate ConfigDir }
  ConfigDir := GetAppConfigDir(false);
  if not ForceDirectories(ConfigDir) then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [ConfigDir]);
  ConfigDir := IncludeTrailingPathDelimiter(ConfigDir);

  { initialize IniFile }
  IniFile := TIniFile.Create(ConfigDir + 'prefs.ini');
  
  WWWHelpServer := IniFile.ReadString('Main', 'WWWHelpServer',
    DefaultWWWHelpServer);
finalization
  if IniFile <> nil then
  begin
    IniFile.WriteString('Main', 'WWWHelpServer', WWWHelpServer);
    
    IniFile.UpdateFile;
    FreeAndNil(IniFile);
  end;
end.

