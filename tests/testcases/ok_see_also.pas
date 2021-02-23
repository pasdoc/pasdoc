{ This is a testing unit for @@seealso tag.
  @seealso(Foo)
  @seealso(TSomeClass <special display name for TSomeClass in this unit>)
}
unit ok_see_also;

interface

{ @abstract(Abstract of Foo.) Further description of Foo.
  @seealso TSomeClass }
procedure Foo;

type
{ @abstract(Abstract of TSomeClass.) Further description of TSomeClass.
  @seealso Foo
  @seealso ok_see_also ok_see_also unit }
  TSomeClass = class
  end;

implementation

end.