{ @abstract(Test of lists tags.)

  Normal lists:
  @orderedList( @item(One) @item(Two) @item(Three) )
  @unorderedList( @item(One) @item(Two) @item(Three) )

  Empty list is accceptable:
  @orderedList( )
  @unorderedList( )

  Whitespace inside lists is acceptable (and empty line
  that is not inside any @@item is @italic(not) a paragraph):
  @orderedList(


    @item         One

    @item Two

    @item(
      Other tags inside the items are allowed,
      e.g. @link(ok_lists link to self), @true,
      @longcode(# begin X := Y; end; #),
      @bold(something bold), URLs: http://pasdoc.sf.net/, paragraphs:

      2nd paragraph, dashes: em dash ---, en dash --, short one -,
      two consecutive short dashes @--.
    )
  )

  Nested lists are also freely allowed:
  @orderedList(
    @item(
      1st nested unordered list:
      @unorderedList( @item(One) @item(Two) @item(Three) )
    )

    @item(
      2nd nested unordered list:
      @unorderedList( @item(One) @item(Two) @item(Three) )
    )

    @item(
      And a couple of single-item ordered lists nested:
      @orderedList( @item( @orderedList( @item( ))))

      (Source code of this example begins to look like LISP :)
    )
  )
}
unit ok_lists;

interface

implementation

end.