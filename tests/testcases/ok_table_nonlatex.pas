(*
  @abstract(Test of @@table-related features that do not work in
    LaTeX, only in HTML.)

  Test that everything within @@cell tag is OK:

  @table(
    @rowHead(
      @cell(
        Anything within a cell is OK, including lists:
        @orderedList(
          @item One
          @item Two
          @item Three
        ), paragraphs:

        This is new paragraph.

        Dashes: ---, --, -, @--. URLs: http://pasdoc.sourceforge.net/

        And, last but not least, nested table:

        @table(
          @rowHead( @cell(1) @cell(2) )
          @rowHead( @cell(3) @cell(4) )
        )

      ) @cell(B) )
    @row( @cell(C) @cell(D) )
  )

  Now the same example table as before, but now nested table
  and other nicies are within a normal row, instead of heading row.

  @table(
    @rowHead( @cell(C) @cell(D) )
    @row(
      @cell(
        Anything within a cell is OK, including lists:
        @orderedList(
          @item One
          @item Two
          @item Three
        ), paragraphs:

        This is new paragraph.

        Dashes: ---, --, -, @--. URLs: http://pasdoc.sourceforge.net/

        And, last but not least, nested table:

        @table(
          @rowHead( @cell(1) @cell(2) )
          @rowHead( @cell(3) @cell(4) )
        )

      ) @cell(B) )
  )
*)
unit ok_table_nonlatex;

interface

implementation

end.
