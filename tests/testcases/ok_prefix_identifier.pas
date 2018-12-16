{ Test parsing identifiers declared as &xxx, where "xxx" may be a reserved word.
  See
  @unorderedList(
    @item https://github.com/pasdoc/pasdoc/issues/28
    @item http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Fundamental_Syntactic_Elements_(Delphi)#Extended_Identifiers
  )
}
{$ifdef FPC} {$mode delphi} {$endif}

unit ok_prefix_identifier;

interface

uses
  Classes, SysUtils;

type
  TTest = class
  class var
    &begin : Integer;
    &var : Integer;
    // &123 : Integer; // not allowed, the thing after & must still be valid identifier
    // & xyz : Integer;  // not alllowed
    { }
    class constructor Create;
    class function &end : Integer;
  end;

implementation

class constructor TTest.Create;
begin
  &begin := 100;
end;

class function TTest.&end : Integer;
begin
  Result := TTest.&begin;
end;

end.
