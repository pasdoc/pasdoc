unit ok_generic_routines;

interface

type

	TTestClass = class	
	end;

generic function DoSomething<T>(Input: String): T;
generic procedure DoSomething2<T>(Input: T);

implementation

generic function DoSomething<T>(Input: String): T;
begin
  Result := Input;
end

generic procedure DoSomething2<T>(Input: T);
begin

end;

end.