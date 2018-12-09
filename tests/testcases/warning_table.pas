{ @abstract(Test of @@table-related warnings.)

  Various incorrect table tags nesting:
  @table( @cell(Foo) )
  @table( @row(Foo) )
  @row( @cell(Foo) )
  @cell( @row(Foo) )
  
  Table with no rows is not allowed:
  @table( )
  
  Row with no cells is not allowed:
  @table( @row( ) )
}

unit warning_table;

interface

implementation

end.