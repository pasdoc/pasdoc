unit ok_procedural_const.pas;

interface

const
	FnTest1 = function(const Foo: Integer): Integer; cdecl = nil;
	FnTest2 = procedure(const Foo: Integer); stdcall = nil;
	
implementation

end.