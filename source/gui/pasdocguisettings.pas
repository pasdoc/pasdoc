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
  DefaultWWWHelpServer = 'http://pasdoc.sipsolutions.net/';

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

