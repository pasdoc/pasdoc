{
	@abstract(Extended Pascal, ISO 10206, modules)
	
	@url(https://github.com/pasdoc/pasdoc/issues/114 issue #114)
	
	@author(GNU Pascal contributors)
	
	This demo has been adapted from:
	`gpc-3.4/examples/docdemos/demomodule.pas`
	
	This demo does not show that modules can be parameterized.
}
module demoModule interface;

export
	{ It is not necessary
	  that this identifier name coincides with the module name. }
	demoModule = (fooType, setFoo, getFoo);
	{ GPC would also accept `demoModule = all;` to export all identifiers. }

type
	fooType = integer;

procedure setFoo(f: fooType);
function  getFoo: fooType;

end.


{ The following may reside in a _different_ file. }


module demoModule implementation;

{ EP defines two standard modules: `standardInput` and `standardOutput`. }
import
	standardInput;
	standardOutput;

var
	foo: fooType;

procedure setFoo;
begin
	foo := f;
end;

function getFoo;
begin
	getFoo := foo;
end;

{ This part corresponds to `initialization` in a UCSD Pascal `unit`: }
to begin do
begin
	foo := 59;
	writeLn('Just an example of a module initializer. See comment below');
end;

{ This part corresponds to `finalization` in a UCSD Pascal `unit`: }
to end do
begin
	foo := 0;
	writeLn('Goodbye');
end;

end.
