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
    {$ifdef WIN32} 'explorer %s' {$else} 'sh -c "$BROWSER %s"' {$endif};

implementation

uses PasDocGuiSettings;

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
begin
  DocBrowserProcess.CommandLine := Format(BrowserCommand, [URL]);
  DocBrowserProcess.Execute;
end;

initialization
  {$I wwwbrowserrunnerdm.lrs}
end.

