{ @abstract(Test that generics with types separated by semicolons are parsed correctly.)
  See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Constraints_in_Generics:
  """When you specify constraints, you separate multiple type parameters by semicolons,
  as you do with a parameter list declaration: """ }
unit ok_generic_multiple_constraints;

interface

uses Classes;

type
  TMyClass = class
  public
    { Call with two type params separated by semicolon }
    function Call<A; R>(const Arg: A): R; overload;

    { Call with three type params separated by semicolons }
    function Call<A; B; R>(const Arg1: A; const Arg2: B): R; overload;

    { Procedure variant }
    procedure Execute<A; B>(const Arg1: A; const Arg2: B); overload;
  end;

  { Generic type aliases with semicolons (in type section) }
  TMethodFunc<A; R> = function(Arg: A): R of object;
  TMethodFunc2<A; B; R> = function(Arg1: A; Arg2: B): R of object;

  TMyClassWithConstraints<A; B; R> = class(TMyClass)
  end;

  TMyClassWithConstraints2<A: TComponent; B; R> = class(TMyClass)
  end;

implementation

end.
