{ @abstract(Test of @@( and @@) constructs.)

  @bold(This is bold, followed by two "at" chars and two parens. @@@@( ) )
  No longer bold.

  @italic(This is italic, followed by one "at" char and one opening paren. @@@( )
  No longer italic.

  @bold(This is bold, followed by two parens.  ( ) )
  No longer bold.

  @italic(This is italic, followed by one closing paren. @) )
  No longer italic.

  @italic(This is bold, followed by "at" char. @@)
  No longer italic.
}
unit ok_non_matching_paren;

interface

implementation

end.