unit warning_no_params;

interface

{ Parens are missing here, generated docs will not be as expected.
  PasDoc should warn about this.

  @returns
    something }
function Foo1(A: Integer): Integer;

{ Parens are missing here, generated docs will not be as expected.
  PasDoc should warn about this.

  @param
    A means something }
function Foo2(A: Integer): Integer;

type
  EFoo = class(Exception);

{ Parens are missing here, generated docs will not be as expected.
  PasDoc should warn about this.

  @raises
    EFoo when something is bad }
function Foo3(A: Integer): Integer;

implementation

end.