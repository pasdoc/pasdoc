unit ok_link_tag_overloads;

interface

type
  {
  @unorderedList(
    @item(@link(TOverloadedMethods.Foo))
    @item(@link(TOverloadedMethods.Foo(Integer,Boolean)))
    @item(@link(TOverloadedMethods.Foo(const)))

    @item(@link(TOverloadedMethods.Bar))
    @item(@link(TOverloadedMethods.Bar(integer, boolean)))
    @item(@link(TOverloadedMethods.Bar(String)))
    @item(@link(TOverloadedMethods.Bar(const)))

    @item(@link(TopLevelFunc(string) top level func, accepts string))
    @item(@link(TopLevelFunc(integer) top level func, accepts int))
  )
  }
  TLinkHolder = class(TObject)

  end;

  TOverloadedBase = class(TObject)
    { @className.@name }
    procedure Bar(MyInt: Integer; MyBool: Boolean); virtual;
  end;

  { @link(TLinkHolder) }
  TOverloadedMethods = class(TOverloadedBase)
    procedure Foo(MyInt: IntEGEr); overload;
    procedure Foo(MyInt: Integer; MyBool: Boolean); overload;
    procedure Foo; overload;
    procedure Foo(MyStr: string); overload;
    procedure Foo(const MyStr); overload;
    procedure Foo(const MyStr, MyOtherStr: STRING); overload;

    { @name }
    procedure Bar(MyInt: IntEGEr); overload;
    procedure Bar(MyInt: Integer; MyBool: Boolean); overload; override;
    procedure Bar(MyStr: string); overload;
    procedure Bar(const MyStr); overload;
    procedure Bar(const MyStr, MyOtherStr: STRING); overload;
  end;

function TopLevelFunc(MyStr: string): integer; overload;
function TopLevelFunc(MyInt: Integer): Integer; overload;

implementation

end.