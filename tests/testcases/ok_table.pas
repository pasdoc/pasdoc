(* @abstract(Test of @@table-related features.)

  Example from @@table doc page in wiki:

  @table(
    @rowHead( @cell(Value1) @cell(Value2) @cell(Result) )
    @row(     @cell(@false) @cell(@false) @cell(@false) )
    @row(     @cell(@false) @cell(@true)  @cell(@true) )
    @row(     @cell(@true)  @cell(@false) @cell(@true) )
    @row(     @cell(@true)  @cell(@true)  @cell(@false) )
  )

  Small tables tests:

  @table( @row( @cell(One-cell table is OK) ) )
  @table( @rowHead( @cell(One-cell head table is OK) ) )
  @table( @rowHead( @cell(A) @cell( @bold(Foo) ) ) )

  Test that everything within @@cell tag is OK.
  Actually, test below is a stripped down (to be accepted
  by LaTeX) version of a more advanced test in
  ok_table_nonlatex.pas file.

  @table(
    @rowHead(
      @cell(Dashes: ---, --, -, @--)
      @cell(URLs: http://pasdoc.sourceforge.net/)
    )
    @row( @cell(C) @cell(D) )
  )

  Now nested table and other nicies are within a normal row,
  instead of heading row.

  @table(
    @rowHead( @cell(C) @cell(D) )
    @row(
      @cell(Within a cell many things are are accepted)
      @cell(including paragraphs:

        This is new paragraph.)
    )
    @row(
      @cell(Dashes: ---, --, -, @--.)
      @cell(URLs: http://pasdoc.sourceforge.net/)
    )
    @row(
      @cell(And, last but not least, nested table:
        @table(
          @rowHead( @cell(1) @cell(2) )
          @rowHead( @cell(3) @cell(4) )
        ))
      @cell(B)
    )
  )

  Some 2-row table tests:

  @table(
    @rowHead( @cell(A) @cell(B) )
    @rowHead( @cell(C) @cell(D) )
  )

  @table(
    @row( @cell(A) @cell(B) )
    @row( @cell(C) @cell(D) )
  )

  @table(
    @row( @cell(A) @cell(B) )
    @rowHead( @cell(C) @cell(D) )
  )
*)
unit ok_table;

interface

implementation

end.
