unit anonymous_methods;

interface

type
  TSimpleProcedure = reference to procedure;
  TSimpleFunction = reference to function(x: string): Integer;

implementation

end.
