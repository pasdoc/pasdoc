unit ok_property_decl;

interface

type
  TMyClass = class
  private
    function GetMyProperty(S: string): Integer;
  public
    property MyProperty[S: string]: Integer read GetMyProperty;

    { From https://github.com/pasdoc/pasdoc/issues/174 }
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;

    property MyPropertyNoDefault: Single nodefault;
    property MyPropertyDefaultExpression: Integer default 10 + 20;
    property MyPropertyStored: Single stored false;
    property MyPropertyStored2: Single stored GetStored;
  end;

implementation

end.
