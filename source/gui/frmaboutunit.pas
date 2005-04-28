{
  Copyright 2004-2005 Richard B. Winston, U.S. Geological Survey (USGS)
  Copyright 2005 Michalis Kamburelis

  This file is part of pasdoc_gui.

  pasdoc_gui is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  pasdoc_gui is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with pasdoc_gui; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @author(Michalis Kamburelis)
}

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
    'Original version Richard B. Winston (rbwinst@usgs.gov), ' +
    'U.S. Geological Survey (USGS)' + LineEnding +
    LineEnding +
    'Modifications copyright 2005 Michalis Kamburelis' + LineEnding +
    LineEnding +
    'Additional modifications by Richard B. Winston' + LineEnding +
    LineEnding +
    'pasdoc_gui and PasDoc component are free software. ' +
    'You are welcome to further modify and redistribute them on terms ' +
    'of GNU General Public License.' +LineEnding+
    LineEnding +
    'See http://pasdoc.sourceforge.net/' +LineEnding+
    LineEnding+
    'PasDoc version information:' +LineEnding+
    PASDOC_FULL_INFO;
end;

initialization
  {$I frmaboutunit.lrs}
end.
