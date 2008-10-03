{ @abstract(Registers the PasDoc components into the IDE. )
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Johannes Berg <johannes@sipsolutions.de>) 
  @author(Michalis Kamburelis)
  @cvs($Date$)  
  
  TODO: We have some properties in TPasDoc and generators components that
  should be registered with filename editors.
}

unit PasDoc_Reg;

interface

{ Registers the PasDoc components into the IDE. }
procedure Register;

implementation

uses
  Classes,
  PasDoc_GenHtml,
  PasDoc_GenLatex,
  PasDoc_GenHtmlHelp,
  PasDoc_GenSimpleXML,
  PasDoc_GenFullXML,
  PasDoc_GenFullHtml,
  PasDoc_Base;

procedure Register;
begin
  RegisterComponents('PasDoc', [TPasDoc,
    THTMLDocGenerator, THTMLHelpDocGenerator, TFullHTMLDocGenerator,
    TTexDocGenerator,
    TSimpleXMLDocGenerator, TXMLDocGenerator
  ]);
end;

end.
