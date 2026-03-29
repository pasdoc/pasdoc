{ @abstract(Test that "on" can be used as a property name, Delphi allows this.) }
unit ok_on_property;

interface

type
  TMyClass = class
  private
    function GetOn: Boolean;
    procedure SetOn(Value: Boolean);
  public
    property on: Boolean read GetOn write SetOn;
  end;

implementation

end.
