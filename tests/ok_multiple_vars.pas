unit ok_multiple_vars;

interface

type
  TMyClass = class
    { Docs for A and B }
    A, B: Integer;
    { Docs for A and B }
    C, D: procedure(A: Integer): boolean;  
  end;

var 
  { Docs for A and B }
  A, B: Integer;
  { Docs for A and B }
  C, D: procedure(A: Integer): boolean;

implementation

end.