unit frmAboutUnit;

{$mode DELPHI}

interface

uses
  SysUtils, Classes, LResources, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    Memo1: TMemo;
    procedure frmAboutCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses PasDoc;

{ TfrmAbout }

procedure TfrmAbout.frmAboutCreate(Sender: TObject);
begin
  Memo1.Lines.Text :=
    'by Richard B. Winston (rbwinst@usgs.gov)' +LineEnding+
    LineEnding+
    'Conversion to Lazarus by Michalis Kamburelis.' +LineEnding+
    LineEnding+
    'This program makes use of PasDoc (http://pasdoc.sourceforge.net/). ' +
    'PasDoc is licensed under the GNU General Public License.' +LineEnding+
    LineEnding+
    'PasDoc version information:' +LineEnding+
    PASDOC_FULL_INFO;
end;

initialization
  {$I frmaboutunit.lrs}
end.
