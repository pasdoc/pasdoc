{ @abstract(Test various things with links.)

  Link to proc inside this unit : @link(MyProc),
  and a qualified link to the same thing : @link(ok_links.MyProc).

  Link to proc inside other unit: @link(ok_links_2.MyOtherProc),
  link to proc inside other unit that has the same name as proc in this unit:
  @link(ok_links_2.MyProc).

  Link to method in class in this unit:
  @link(TSomeClass.MyMethod),
  and a more qualified link to the same thing :
  @link(ok_links.TSomeClass.MyMethod).

  Link to method in class in second unit: @link(TSomeOtherClass.MyMethod),
  link to method in class in second unit
  that has the same name as class in this unit:
  @link(ok_links_2.TSomeClass.MyMethod).

  Link to this unit : @link(ok_links), to other unit : @link(ok_links_2).
}
unit ok_links;

interface

uses ok_links_2;

{ Link to self : @link(MyProc), and a second one: @link(ok_links.MyProc),
  link to MyProc in other unit: @link(ok_links_2.MyProc). }
procedure MyProc;

type
  TSomeClass = class
    { Two links to @code(MyOtherMethod) :
      qualified @link(TSomeClass.MyOtherMethod),
      not qualified @link(MyOtherMethod) }
    procedure MyMethod;

    procedure MyOtherMethod;
  end;

implementation

end.
