unit ok_no_link_inside_class;

interface

type
  TMyClass = class
    procedure MyMethod;

    { This should be linked: @link(MyMethod).

      This should be linked: @link(TMyClass.MyMethod).

      This should be linked: @link(MyGlobalProcedure). }
    procedure MyOtherMethod;
  end;

{ This should NOT be linked: @link(MyMethod).

  This should be linked: @link(TMyClass.MyMethod).

  This should be linked: @link(MyOtherGlobalProcedure). }
procedure MyGlobalProcedure;

procedure MyOtherGlobalProcedure;

implementation

end.
