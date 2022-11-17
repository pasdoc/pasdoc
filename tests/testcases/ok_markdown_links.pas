{ @abstract(Test markdown links.)

  Link to proc inside this unit with description: [Test Procedure](#MyProc)

  Not links:

  - \[Test Procedure](#MyProc)

}
unit ok_markdown_links;

interface

{ Link to [Self](#MyProc), and a [Second one](#ok_markdown_links.MyProc).
  Link without explicit title [ok_markdown_links].

  Not links to \[Self](#MyProc), and a \[Second one](#ok_markdown_links.MyProc).
  Not a link without explicit title \[ok_markdown_links]. }
procedure MyProc;

implementation

end.
