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

{$ifdef UNIX}

{ Code below is copied from FPC 2.0.3 rtl/unix/sysutils.pp.

  It has fixed GetAppConfigDir and GetAppConfigFile, so that
  1. config file is stored inside hidden dir
  2. moreover it follows base-dir spec
  
  Michalis send this patch to FPC and it was applied during FPC 2.0.3
  development. So for FPC 2.0.0 and 2.0.2, we need the workaround below. }
  
Function GetHomeDir : String;

begin
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result);
end;
  
{ Follows base-dir spec, 
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
Function XdgConfigHome : String;
begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:=GetHomeDir + '.config/' 
  else
    Result:=IncludeTrailingPathDelimiter(Result);
end;

Function GetAppConfigDir(Global : Boolean) : String;

begin
  If Global then
    Result:=SysConfigDir
  else
    Result:=XdgConfigHome + ApplicationName;
end;

Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  if Global then
    begin
    Result:=IncludeTrailingPathDelimiter(SysConfigDir);
    if SubDir then
      Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
    Result:=Result+ApplicationName+ConfigExtension;
    end
  else
    begin
    if SubDir then
      begin
      Result:=IncludeTrailingPathDelimiter(GetAppConfigDir(False));
      Result:=Result+ApplicationName+ConfigExtension;
      end
    else
      begin
      Result:=XdgConfigHome + ApplicationName + ConfigExtension;
      end;
    end;
end;
{$endif}

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

