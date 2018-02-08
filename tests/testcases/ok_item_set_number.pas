{ @abstract(Test of of @@itemSetNumber tag.)

  @orderedList(
    @item Foo
    @item Foo
    @item Foo
    @item Foo
    @item Foo
    @itemSetNumber 144
    @item Foo
    @item Foo
    @item Foo
    @item(
      @orderedList(
        @item Bar
        @item Bar
        @item Bar
        @itemSetNumber 20
        @item Bar
        @item Bar
        @item Bar
      )
    )
    @item Foo
    @item Foo
    @item Foo
  )
}

unit ok_item_set_number;

interface

implementation

end.