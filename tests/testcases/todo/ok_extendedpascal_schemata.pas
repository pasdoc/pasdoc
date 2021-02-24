{
	@abstract(Extended Pascal, ISO 10206, schemata)
	
	@url(https://github.com/pasdoc/pasdoc/issues/114 issue #114)
	
	In a type definition
	the new identifier may be followed by a formal discriminant
	surrounded by parenthesis.
	There has to be at least one discriminant specification.
	Inside a _formal_ discriminant list,
	specifications are separated by a semicolon,
	pretty much like in a routine declaration,
	and every discriminant has a data type.
	
	When discriminating schemas,
	discriminant specifications are listed separated by commas,
	analogous to a routine call.
}
{ NB: FPC, as of version 3.2.0 [released in 2020] does not support Schemata. }
{$mode extendedPascal}
unit ok_ExtendedPascal_schemata;
{
	This code has been put into a UCSD Pascal `unit`,
	because as of PasDoc version 0.16.0 can only parse units.
	Extended Pascal, ISO 10206, does _not_ define units, but modules,
	confer @link(ok_extendedpascal_modules).
}


interface


type
	{ adapted from gpc-3.4/examples/docdemos/schemaexoticdemo.pas }
	primaryColor = (red, green, blue);
	coloredInteger(color: primaryColor) = integer;

var
	{ discriminated schema }
	foo: coloredInteger(red);


type
	{ adapted from gpc-3.4/examples/docdemos/schema1demo.pas }
	natural = 1..maxInt;
	{ short notation for multiple discrimants of the same data type }
	matrix(n, m: natural) = array[1..n, 1..m] of integer;

var
	{ discriminated schema with comma }
	i: matrix(7, 8);


implementation

end.
