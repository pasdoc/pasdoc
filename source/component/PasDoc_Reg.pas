{ @abstract(Registers the PasDoc components into the IDE. )
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de> }

unit PasDoc_Reg;

interface

{ Registers the PasDoc components into the IDE. }
procedure Register;

implementation

uses
  Classes,
  PasDoc,
  PasDoc_GenHtml,
  PasDoc_GenLatex;

procedure Register;
begin
  RegisterComponents('PasDoc', [TPasDoc, THTMLDocGenerator, TTexDocGenerator]);
end;

end.
