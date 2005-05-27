unit ok_property_decl;

interface

type
  TMyClass = class
  private
    function GetMyProperty(S: string): Integer;
  public
    property MyProperty[S: string]: Integer read GetMyProperty;
  end;

implementation

end.