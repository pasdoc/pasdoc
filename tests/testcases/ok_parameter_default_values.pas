unit ok_parameter_default_values;

interface

procedure Foo(A: Integer; B: Integer = 3; C: Integer = 5);
procedure Bar(A, B: Integer; C: Integer = 3; D: string = '');
function Baz(A: Boolean = True): string;

implementation

end.