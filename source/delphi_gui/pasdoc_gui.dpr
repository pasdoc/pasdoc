program pasdoc_gui;

{$R 'pasdoc_gui_manifest.res' 'pasdoc_gui_manifest.rc'}

uses
  Forms,
  frmAboutUnit in 'frmAboutUnit.pas' {frmAbout},
  frmhelpgeneratorunit in 'frmhelpgeneratorunit.pas' {frmHelpGenerator},
  WWWBrowserRunnerDM in 'WWWBrowserRunnerDM.pas' {WWWBrowserRunner},
  PasDocGuiSettings in 'PasDocGuiSettings.pas',
  preferencesfrm in 'preferencesfrm.pas' {preferencesfrm};

{$R *.res}
  
begin
  Application.Initialize;
  Application.CreateForm(TfrmHelpGenerator, frmHelpGenerator);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TWWWBrowserRunner, WWWBrowserRunner);
  Application.Run;
end.

