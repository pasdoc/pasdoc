{ @abstract(Registers the PasDoc components into the IDE. )
  @author(Ralf Junker (delphi@zeitungsjunge.de)) }

unit PasDoc_Reg;

interface

{ Registers the PasDoc components into the IDE. }
procedure Register;

implementation

uses
  Classes,
  PasDoc;

{ ---------------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('The Delphi Inspiration', [TPasDoc]);
end;

end.
