unit A;

interface

type
  TMyException = class
  end;

{ @param(A This is some parameter.)
  @raises(TMyException When somethig bad happens.) }
procedure Foo(A: Integer);

type
  TMyClass = class
  private
    function GetItems(const Index: Integer): string;
    procedure SetItems(const Index: Integer; const Value: string);
    procedure SetBar(const Value: Integer);
  public
    { @raises(TMyException When setting to an invalid value.) }
    property Bar: Integer read FBar write SetBar;

    { @param(A This is some parameter.) }
    property Items [const Index: Integer]: string read GetItems write SetItems;
  end;

implementation

end.
