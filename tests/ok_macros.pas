{$define UNIT_DECL := unit ok_macros; interface}

{ @abstract(Test of FPC macros handling.)

  Parts based on
  [http://sourceforge.net/tracker/index.php?func=detail&aid=861356&group_id=4213&atid=354213]
}

UNIT_DECL

{$INCLUDE ok_macro_include.inc}

type
  TMyClass = class(TAncestor)
  public
    CLASS_CONSTRUCTOR;
  end;

{ Below is an example of a very bad and confusing (but valid)
  macro usage. Just to test pasdoc. }

{$define FOO := a:Integer); (* This is very stupid way to declare
  a procedure *) procedure MyProc2(}

procedure MyProc1(FOO b: Integer);

{$undef FOO}

function Foo(c: string): Integer;

procedure MyProc3(
  {$define FOO:=1} X: Integer = FOO;
  {$define FOO:=2} Y: Integer = FOO);

{$ifdef FOO}
procedure ThisShouldBeIncluded;
{$define FOO_WAS_DEFINED:=true}
{$else}
procedure ThisShouldNotBeIncluded;
{$define FOO_WAS_DEFINED:=false}
{$endif}

const
  ThisShouldBeTrue = FOO_WAS_DEFINED;

{$undef FOO}

{$ifndef FOO}
procedure ThisShouldBeIncluded;
{$else}
procedure ThisShouldNotBeIncluded;
{$endif}

{ Test of recursive macro expansion. }

{$define ONE:=1}
{$define TWO:=ONE + ONE}
{$define FOUR := (TWO) * (TWO)}
const
  FourConst = FOUR;
  
{ Test that symbol that is not a macro is something different than
  a macro that expands to nothing. }
  
{$define NOT_NOTHING}
{$define NOTHING :=}

const
  OneAndNotNothing = 1 NOT_NOTHING;
  OnlyOne = 1 NOTHING;

implementation

end.