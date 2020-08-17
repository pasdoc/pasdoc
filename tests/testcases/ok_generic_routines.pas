{$ifdef FPC} {$mode objfpc}{$H+} {$endif}

unit ok_generic_routines;

interface

type
  TTestClass = class
  end;

  { TTestClass }

  TTestClass = class
  public
    (* Test documentation for class function DoSomething *)
    generic function DoSomething<T>(Input: String): T;
    (* Test documentation for class procedure DoSomething2 *)
    generic procedure DoSomething2<T>(Input: T);
  end;

(* Test documentation for function DoSomething *)
generic function DoSomething<T>(Input: String): T;
(* Test documentation for procedure DoSomething2 *)
generic procedure DoSomething2<T>(Input: T);

implementation

(* Test documentation for function DoSomething implementation *)
generic function DoSomething<T>(Input: String): T;
begin
  Result := Input;
end;

(* Test documentation for procedure DoSomething2 implementation *)
generic procedure DoSomething2<T>(Input: T);
begin
  X := specialize DoSomething<Integer>('asdads', 123);
  Writeln(X);

  specialize DoSomething2<Integer>(456);
end;

{ TTestClass }

(* Test documentation for class function DoSomething implementation *)
generic function TTestClass.DoSomething<T>(Input: String): T;
begin
  Result := Input;
end;

(* Test documentation for class procedure DoSomething2 implementation *)
generic procedure TTestClass.DoSomething2<T>(Input: T);
begin

end;

end.
