unit warning_inherited_test;

interface

type
  { This is a class without ancestor name specified.
    Inherited is @inherited. 
    Inherited class is @inheritedClass. }
  TMyClass0 = class
    { Inherited is @inherited. 
      Inherited class is @inheritedClass. }
    procedure Clear;
  end;

  { This is a class with specified ancestor name, but ancestor (TStringList)
    is not parsed by PasDoc. 
    Inherited is @inherited. 
    Inherited class is @inheritedClass. }
  TMyClass1 = class(TStringList)
    { Inherited is @inherited. 
      Inherited class is @inheritedClass. }
    procedure Clear;
  end;

  { This is a class with specified ancestor name, and ancestor (TMyClass1)
    is parsed by PasDoc. 
    Inherited is @inherited. 
    Inherited class is @inheritedClass. }
  TMyClass2 = class(TMyClass1)
    { Inherited is @inherited. 
      Inherited class is @inheritedClass. }
    procedure Clear;
  end;

{ Inherited is @inherited. 
  Inherited class is @inheritedClass. }
procedure Test;

implementation

end.