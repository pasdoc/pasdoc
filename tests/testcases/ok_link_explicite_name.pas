unit ok_link_explicite_name;

interface

type
  { I'm a testing class, oh ! 
    And @link(MyMethod don't forget to look at my method !) }
  TTestingClass = class
    procedure MyMethod;
  end;

{ Some testing proc.
  @link TTestingClass.MyMethod Have you seen my method ? }
procedure MyProc;

implementation

end.