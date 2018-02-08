unit ok_implicit_visibility;

interface

type
  TMyClass = class
    MyField: Integer;
    property MyProperty: Integer read MyField write MyField;
    procedure MyMethod;
  end;

{$M+}

  TMyClassInMState = class
    MyField: Integer;
    property MyProperty: Integer read MyField write MyField;
    procedure MyMethod;
  end;

implementation

end.
