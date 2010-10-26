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
    {$ifdef MSWINDOWS} '' {$else} 'xdg-open "%s"' {$endif};

implementation

uses {$ifdef MSWINDOWS} Windows, ShellAPI, {$endif} PasDocGuiSettings;

{ TWWWBrowserRunner }

procedure TWWWBrowserRunner.DataModuleCreate(Sender: TObject);
begin
  BrowserCommand := IniFile.ReadString('Main', 'WWWBrowserCommand_V2',
    DefaultWWWBrowserCommand);
end;

procedure TWWWBrowserRunner.DataModuleDestroy(Sender: TObject);
begin
  IniFile.WriteString('Main', 'WWWBrowserCommand_V2', BrowserCommand);
end;

procedure TWWWBrowserRunner.RunBrowser(const URL: string);

  {$ifdef MSWINDOWS}
  procedure ShellExecuteURL;
  var
    ExecInfo: TShellExecuteInfo;
  const
    OpenCommand = 'open';
  begin
    ExecInfo.cbSize := SizeOf(ExecInfo);
    ExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    ExecInfo. {$ifdef VER2_2} hWnd {$else} Wnd {$endif} := 0;
    ExecInfo.lpVerb := PChar(OpenCommand);
    ExecInfo.lpFile := PChar(URL);
    ExecInfo.lpParameters := nil;
    ExecInfo.lpDirectory := nil;
    ExecInfo.nShow := SW_SHOWNORMAL;
    ShellExecuteEx( LPSHELLEXECUTEINFOA(@ExecInfo) );
  end;
  {$endif}

begin
  {$ifdef MSWINDOWS}
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

