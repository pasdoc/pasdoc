{ This unit implements TPreferences form (run by TPreferences.Execute).

  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)}

unit PreferencesFrm;

interface

{$R *.dfm}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TPreferences }

  TPreferences = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnResetDefaults: TButton;
    EditWWWBrowserCommand: TEdit;
    EditWWWHelpServer: TEdit;
    LabelWWWBrowserCommand: TLabel;
    LabelWWWHelpServer: TLabel;
    cbLoadLastProject: TCheckBox;
    procedure BtnResetDefaultsClick(Sender: TObject);
  private
    { private declarations }
  public
    class procedure Execute;
  end; 

implementation

uses
  WWWBrowserRunnerDM, PasDocGuiSettings;

procedure TPreferences.BtnResetDefaultsClick(Sender: TObject);
begin
  EditWWWBrowserCommand.Text := DefaultWWWBrowserCommand;
  EditWWWHelpServer.Text := DefaultWWWHelpServer;
  cbLoadLastProject.Checked := TRUE;
end;

class procedure TPreferences.Execute;
var
  F: TPreferences;
begin
  F := TPreferences.Create(nil);
  try
    F.EditWWWBrowserCommand.Text := WWWBrowserRunner.BrowserCommand;
    F.EditWWWHelpServer.Text := WWWHelpServer;
    F.cbLoadLastProject.Checked := AutoLoadLastProject;
    if F.ShowModal = mrOK then
    begin
      WWWBrowserRunner.BrowserCommand := F.EditWWWBrowserCommand.Text;
      WWWHelpServer := F.EditWWWHelpServer.Text;
      AutoLoadLastProject := F.cbLoadLastProject.Checked;
    end;
  finally
    F.Free;
  end;
end;

end.

