{ This unit implements TPreferences form (run by TPreferences.Execute).

  @author(Michalis Kamburelis) }

unit PreferencesFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel;

type

  { TPreferences }

  TPreferences = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditWWWHelpServer: TEdit;
    LabelWWWHelpServer: TLabel;
  private
    { private declarations }
  public
    class procedure Execute;
  end; 

implementation

uses PasDocGuiSettings;

class procedure TPreferences.Execute;
var
  F: TPreferences;
begin
  F := TPreferences.Create(nil);
  try
    F.EditWWWHelpServer.Text := WWWHelpServer;
    if F.ShowModal = mrOK then
    begin
      WWWHelpServer := F.EditWWWHelpServer.Text;
    end;
  finally
    F.Free;
  end;
end;

initialization
  {$I preferencesfrm.lrs}
end.

