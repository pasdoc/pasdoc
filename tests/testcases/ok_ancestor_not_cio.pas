{ Testcase was causing a bug before this fix:
  https://github.com/pasdoc/pasdoc/commit/00c4e66ae1962398ff4f2ed76fbb2facbfaf84c0
  In this case, class has an ancestor, but for PasDoc the ancestor type
  is not "CIO", as it's an alias (and potentially, PasDoc doesn't even know
  the unit with it, so it really cannot determine it's CIO). }
unit ok_ancestor_not_cio;

interface

uses SomeInaccessibleUnit;

type
  IXmlReader = SomeInaccessibleUnit.Xml.Reader.Intf.IXmlReader;

  TXmlReader = class(TInterfacedObject, IXmlReader)
  private
  end;

implementation

end.
