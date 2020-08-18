{$ifdef FPC} {$mode objfpc}{$H+} {$endif}

unit ok_generic_routines;

interface

type

  { TTestClass }

  TTestClass = class
  public
    (* Test documentation for class function DoSomething *)
    generic function DoSomething<T>(const Caption: String; const Input: T): T;
    (* Test documentation for class procedure DoSomething2 *)
    generic procedure DoSomething2<T>(const Input: T);
  end;

(* Test documentation for function DoSomething *)
generic function DoSomething<T>(const Caption: String; const Input: T): T;
(* Test documentation for procedure DoSomething2 *)
generic procedure DoSomething2<T>(const Input: T);

implementation

(* Test documentation for function DoSomething implementation *)
generic function DoSomething<T>(const Caption: String; const Input: T): T;
begin
  Result := Input;
end;

(* Test documentation for procedure DoSomething2 implementation *)
generic procedure DoSomething2<T>(const Input: T);
begin

end;

{ TTestClass }

(* Test documentation for class function DoSomething implementation *)
generic function TTestClass.DoSomething<T>(const Caption: String; const Input: T): T;
begin
  Result := Input;
end;

(* Test documentation for class procedure DoSomething2 implementation *)
generic procedure TTestClass.DoSomething2<T>(const Input: T);
begin

end;

procedure Foo;
var
  X: Integer;
  Y: TTestClass;
begin
  X := specialize DoSomething<Integer>('asdads', 123);
  Writeln(X);

  specialize DoSomething2<Integer>(456);

  Y := TTestClass.Create;
  try
    X := Y.specialize DoSomething<Integer>('asdads', 123);
    Writeln(X);

    Y.specialize DoSomething2<Integer>(456);
  finally
    Y.Free;
  end
end;

end.