// Test for handling class functions in PasDoc
unit ok_class_function;

interface

type
  // @abstract(This class is designed to see if PasDoc can
  // parse class functions correctly.)
  TDummy = class(TObject)
  public
    // Test To see if PasDoc can handle this.
    Class Function MyFunction: integer;
    // Test To see if PasDoc can handle this.
    class procedure MyProcedure;
  end;

implementation

{ TDummy }

class function TDummy.MyFunction: integer;
begin

end;

class procedure TDummy.MyProcedure;
begin

end;

end.
 