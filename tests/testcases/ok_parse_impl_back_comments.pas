{ Parse implementation section.
  Separate test to check how that backcomments are handled correctly.
  Must be tested with --implementation-comments=*any* and --auto-back-comments }
unit ok_parse_impl_back_comments;

interface

var
  V: Byte;

implementation  // should be dropped instead of glueing to V

// parsing type/var/const sections
{ }
type
  T = byte; //< must pass
var
  v: byte; //< must pass
const
  c = 1; //< must pass

// parsing proc type with param
{ }
type
  TProc = procedure (par: byte); //< must pass

// parsing property
{ }
property Prop: Byte Read GetProp; //< must pass

// parsing routine
{ }
procedure Foo;
//< must pass
// parsing inner type
{ }
type
  TInner = byte; //< must pass
var
  lambda: TProc;
begin
  lambda := procedure(par: byte) // auto-back - must pass
    begin
      // must be ignored
    end;
end;

// list of items for back comments must be cleared after routine declaration
{ }
// Bar
procedure Bar;
begin
  readln; // auto-back - must pass and not glue to Bar
end;  

end. // auto-back - must pass