unit ok_array_parameters;

interface

procedure Foo(A: array of string; const B: array of Integer = 3; C: file of TObject = 5);

// @link(Foo(array of string, array of Integer, file of TObject))
procedure Bar;

implementation

end.