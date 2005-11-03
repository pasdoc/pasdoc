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
  private
    { private declarations }
  public
    BrowserCommand: string;
    procedure RunBrowser(const URL: string);
  end;

var
  WWWBrowserRunner: TWWWBrowserRunner;

implementation

{ TWWWBrowserRunner }

procedure TWWWBrowserRunner.DataModuleCreate(Sender: TObject);
begin
  BrowserCommand :=
    {$ifdef WIN32} 'explorer %s' {$else} 'sh -c "$BROWSER %s"' {$endif};
end;

procedure TWWWBrowserRunner.RunBrowser(const URL: string);
begin
  DocBrowserProcess.CommandLine := Format(BrowserCommand, [URL]);
  DocBrowserProcess.Execute;
end;

initialization
  {$I wwwbrowserrunnerdm.lrs}
end.

