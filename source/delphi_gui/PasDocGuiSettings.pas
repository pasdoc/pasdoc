{ This unit provides @link(IniFile) variable that can be used to store
  some user configuration. It also declares some global variables
  that are part of user configuration and don't fit anywhere else,
  like WWWHelpServer.

  @author(Michalis Kamburelis) }


unit PasDocGuiSettings;

interface

{$IFDEF ConditionalExpressions}
  {$IF CompilerVersion >= 15}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 20}
    {$DEFINE STRING_UNICODE}
  {$IFEND}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
  IniFiles;
  
var
  IniFile: TIniFile;

const
  DefaultWWWHelpServer = 'http://pasdoc.sipsolutions.net/';

var
  WWWHelpServer: string;
  AutoLoadLastProject: Boolean = TRUE;

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

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
function GetCommonAppDataFolder(const SubPath: String): String;
var
    hSHFolderDLL: HMODULE;
    f_SHGetFolderPath: function(hwndOwner: HWND; nFolder: Integer;
        hToken: THandle; dwFlags: DWORD; pszPath: PChar): HRESULT; stdcall;
    Buf: array[0..MAX_PATH - 1] of Char;
const
    CSIDL_LOCAL_APPDATA = $001C;
    SHGFP_TYPE_CURRENT  = 0;
begin
    Result := '';
    hSHFolderDLL := LoadLibrary('shfolder.dll');
    if hSHFolderDLL = 0 then
        Exit;
    try
    {$IFDEF UNICODE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathW');
    {$ELSE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathA');
    {$ENDIF}
        if @f_SHGetFolderPath = nil then
            Exit;
        if Succeeded(f_SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0,
                                       SHGFP_TYPE_CURRENT, Buf)) then begin
            Result := ExpandFileName(Buf);
            Result := IncludeTrailingPathDelimiter(Result) + SubPath;
            try
                if not ForceDirectories(Result) then
                    Result := '';
            except
                Result := '';
            end;
        end;
    finally
        FreeLibrary(hSHFolderDLL);
    end;
end;

function GetAppConfigDir(Global: Boolean) : String;
begin
  Result := GetCommonAppDataFolder(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  if Result = '' then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [Result]);
end;

{$ENDIF}
{$ENDIF}

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
  AutoLoadLastProject := IniFile.ReadBool('Main', 'AutoLoadLastProject', TRUE);

finalization
  if IniFile <> nil then
  begin
    IniFile.WriteString('Main', 'WWWHelpServer', WWWHelpServer);
    IniFile.WriteBool('Main', 'AutoLoadLastProject', AutoLoadLastProject);
    IniFile.UpdateFile;
    FreeAndNil(IniFile);
  end;
end.

