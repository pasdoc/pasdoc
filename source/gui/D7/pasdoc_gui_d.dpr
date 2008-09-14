program pasdoc_gui_d;

uses
  Forms,
  fAboutUnit in 'fAboutUnit.pas' {Form1},
  uShell in 'uShell.pas',
  frDirs in 'frDirs.pas' {DirList: TFrame},
  frDir in 'frDir.pas' {DirBox: TFrame},
  fDocMain in 'fDocMain.pas' {DocMain},
  test_tags in 'test_tags.pas';

{ $R pasdoc_gui.res}

begin
  Application.Initialize;
  Application.CreateForm(TDocMain, DocMain);
  Application.Run;
end.

