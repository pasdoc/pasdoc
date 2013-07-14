program pasdoc_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, frmAboutUnit, frmHelpGeneratorUnit, pasdoc_package,
  HelpProcessor, PreferencesFrm, PasDocGuiSettings;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmHelpGenerator, frmHelpGenerator);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.

