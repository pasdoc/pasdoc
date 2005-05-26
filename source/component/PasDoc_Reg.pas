{ @abstract(Registers the PasDoc components into the IDE. )
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Johannes Berg <johannes@sipsolutions.de>) 
  @author(Michalis Kamburelis)
  @cvs($Date$)  
}

unit PasDoc_Reg;

interface

{ Registers the PasDoc components into the IDE. }
procedure Register;

implementation

uses
  Classes,
  PasDoc,
  PasDoc_GenHtml,
  PasDoc_GenLatex,
  PasDoc_GenHtmlHelp;

procedure Register;
begin
  RegisterComponents('PasDoc', [TPasDoc, THTMLDocGenerator, TTexDocGenerator,
    THTMLHelpDocGenerator]);
end;

end.
