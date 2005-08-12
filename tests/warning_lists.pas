{ @abstract(Test various pasdoc warnings related to lists.)

  These should cause warnings:
  @orderedList( @@ @item(One) )
  @unorderedList( @- @item(One) )
  @orderedList( @item(One) -- )
  @unorderedList( @item(One) --- )
  @orderedList( foo @item(One) bar )
  @orderedList( http://pasdoc.sf.net/ @item(One) )
}
unit warning_lists;

interface

implementation

end.