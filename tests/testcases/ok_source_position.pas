{ @abstract(Test of @--show-source-position and @--source-url-pattern features.) }
unit ok_source_position;

interface

{ A simple constant. }
const MyConst = 42;

{ A simple type. }
type TMyType = Integer;

{ A simple variable. }
var MyVar: Integer;

{ A simple procedure. }
procedure MyProc;

{ A simple class. }
type
  TMyClass = class
    { A field. }
    MyField: Integer;
    { A method. }
    procedure MyMethod;
    { A property. }
    property MyProp: Integer read MyField;
  end;

  { An enumerated type. }
  TEnum = (
    { The first value. }
    eOne,
    { The second value. }
    eTwo,
    { The third value. }
    eThree
  );

implementation

procedure MyProc;
begin
end;

procedure TMyClass.MyMethod;
begin
end;

end.
