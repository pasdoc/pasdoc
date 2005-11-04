{ @author(Michalis Kamburelis) }

unit WWWBrowserRunnerDM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, Process;

type

  { TWWWBrowserRunner }

  TWWWBrowserRunner = class(TDataModule)
    DocBrowserProcess: TProcess;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    BrowserCommand: string;
    procedure RunBrowser(const URL: string);
  end;

var
  WWWBrowserRunner: TWWWBrowserRunner;

const
  DefaultWWWBrowserCommand =
    {$ifdef WIN32} '' {$else} 'sh -c "$BROWSER %s"' {$endif};

implementation

uses {$ifdef WIN32} Windows, ShellAPI, {$endif} PasDocGuiSettings;

{ TWWWBrowserRunner }

procedure TWWWBrowserRunner.DataModuleCreate(Sender: TObject);
begin
  BrowserCommand := IniFile.ReadString('Main', 'WWWBrowserCommand',
    DefaultWWWBrowserCommand);
end;

procedure TWWWBrowserRunner.DataModuleDestroy(Sender: TObject);
begin
  IniFile.WriteString('Main', 'WWWBrowserCommand', BrowserCommand);
end;

procedure TWWWBrowserRunner.RunBrowser(const URL: string);

  {$ifdef WIN32}
  procedure ShellExecuteURL;
  var
    ExecInfo: TShellExecuteInfo;
  const
    OpenCommand = 'open';
  begin
    ExecInfo.cbSize := SizeOf(ExecInfo);
    ExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    ExecInfo.hWnd := 0;
    ExecInfo.lpVerb := PChar(OpenCommand);
    ExecInfo.lpFile := PChar(URL);
    ExecInfo.lpParameters := nil;
    ExecInfo.lpDirectory := nil;
    ExecInfo.nShow := SW_SHOWNORMAL;
    ShellExecuteEx( LPSHELLEXECUTEINFOA(@ExecInfo) );
  end;
  {$endif}

begin
  {$ifdef WIN32}
  if Trim(BrowserCommand) = '' then
  begin
    ShellExecuteURL;
    Exit;
  end;
  {$endif}
  
  DocBrowserProcess.CommandLine := Format(BrowserCommand, [URL]);
  DocBrowserProcess.Execute;
end;

initialization
  {$I wwwbrowserrunnerdm.lrs}
end.

