{$ifdef FPC} {$mode objfpc}{$H+} {$endif}

unit ok_generic_routines;

interface

type
  TTestClass = class
  end;

generic function DoSomething<T>(const Caption: String; const Input: T): T;
generic procedure DoSomething2<T>(const Input: T);

implementation

generic function DoSomething<T>(const Caption: String; const Input: T): T;
begin
  Result := Input;
end;

generic procedure DoSomething2<T>(const Input: T);
begin

end;

procedure Foo;
var
  X: Integer;
begin
  X := specialize DoSomething<Integer>('asdads', 123);
  Writeln(X);

  specialize DoSomething2<Integer>(456);
end;

end.
