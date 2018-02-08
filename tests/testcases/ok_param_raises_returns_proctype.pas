unit ok_param_raises_returns_proctype;

interface

type
  EFoo = class(Exception);
  
  { @param(A Description of param A)
    @raises(EFoo Description when EFoo is raised) }
  TMyProcedure = procedure(A: integer);
  
  { @param(A Description of param A)
    @returns(@true or @false)
    @raises(EFoo Description when EFoo is raised) }
  TMyMethod = function(A: integer): boolean of object;

implementation

end.