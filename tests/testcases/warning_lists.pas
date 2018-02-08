{ @abstract(Test various pasdoc warnings related to lists.)

  These should cause warnings:
  @orderedList( @@ @item(One) )
  @unorderedList( @- @item(One) )
  @orderedList( @item(One) -- )
  @unorderedList( @item(One) --- )
  @orderedList( foo @item(One) bar )
  @orderedList( http://pasdoc.sf.net/ @item(One) )
  
  These should cause warnings, because @@item must always
  be placed directly inside @@orderedList or @@unorderedList,
  and @@orderedList and @@unorderedList can contain only
  @@item tags.
  
  @orderedList( @author(kambi) )
  @unorderedList( @code(begin end) )
  @item(Item at toplevel is not allowed.)
  @code( @item(Item inside @@-tag other than @@xxxList is not allowed.) )
  
  Warnings related to @@definitionList and @@itemLabel:
  
  @orderedList( @itemLabel( @@itemLabel is allowed only inside
    @@definitionList ) )
    
  Warnings related to @@itemSpacing:
    
  @itemSpacing(compact)
  @orderedList( @itemSpacing(ThisIsInvalid) )
}
unit warning_lists;

interface

implementation

end.