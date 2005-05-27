unit ok_multiple_vars;

interface

type
  TMyClass = class
    { Docs for A and B }
    A, B: Integer;
    { Docs for C and D }
    C, D: procedure(A: Integer): boolean;  
  end;

var 
  { Docs for A and B }
  A, B: Integer;
  { Docs for C and D }
  C, D: procedure(A: Integer): boolean;

implementation

end.