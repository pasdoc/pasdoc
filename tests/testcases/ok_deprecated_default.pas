{ Test parsing property with deprecated directive and default specifier.
  See https://github.com/pasdoc/pasdoc/issues/138 }
unit ok_deprecated_default;

interface

type
  TMyClass = class
    function GetItems(const Index: Integer): Byte;
    procedure SetItems(const Index: Integer; const Value: Byte);
    property Items [const Index: Integer]: Byte read GetItems write SetItems; default;
      deprecated 'use instead X, Y fields';
  end;

implementation

function TMyClass.GetItems(const Index: Integer): Byte;
begin
  Result := 0;
end;

procedure TMyClass.SetItems(const Index: Integer; const Value: Byte);
begin
end;

end.
