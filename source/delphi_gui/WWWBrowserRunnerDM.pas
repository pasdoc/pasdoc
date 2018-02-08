{
  Copyright 1998-2018 PasDoc developers.

  This file is part of "pasdoc_gui".

  "pasdoc_gui" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "pasdoc_gui" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "pasdoc_gui"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>) }

unit WWWBrowserRunnerDM;

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
  Classes, SysUtils, Forms, Controls, Dialogs;

type

  { TWWWBrowserRunner }

  TWWWBrowserRunner = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    procedure RunBrowser(const URL: string);
  end;

var
  WWWBrowserRunner: TWWWBrowserRunner;

implementation

uses {$ifdef WIN32} Windows, ShellAPI, {$endif} PasDocGuiSettings;

{$R *.dfm}

{ TWWWBrowserRunner }

procedure TWWWBrowserRunner.DataModuleCreate(Sender: TObject);
begin
end;

procedure TWWWBrowserRunner.DataModuleDestroy(Sender: TObject);
begin
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
    ExecInfo.Wnd := 0; // <= Might be "ExecInfo.hWnd" in older FPC ?
    ExecInfo.lpVerb := PChar(OpenCommand);
    ExecInfo.lpFile := PChar(URL);
    ExecInfo.lpParameters := nil;
    ExecInfo.lpDirectory := nil;
    ExecInfo.nShow := SW_SHOWNORMAL;
    ShellExecuteEx(@ExecInfo);
  end;
  {$endif}

begin
  {$ifdef WIN32}
  ShellExecuteURL;
  {$endif}
end;

end.

