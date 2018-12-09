{ @abstract(Test @@deprecated tag.)

  Everything in this unit is deprecated. Even this whole unit itself.

  @deprecated }

unit ok_deprecated_tag;

interface

{ @deprecated }
procedure MyProc;

type
  { @deprecated
    Normal type deprecated. }
  TMyType = Integer;

  { @deprecated
    Deprecated class. }
  TMyClass = class

    { Deprecated field. @deprecated }
    MyField: Integer;
    
    { @deprecated @deprecated @deprecated @deprecated 
      Property is deprecated (you can specify @@deprecated
      as many times as you want, because it's harmless). }
    property MyProperty: Integer read MyField write MyField;

    { @deprecated }
    procedure MyMethodLibrary;
  end;

var
  { @deprecated }
  MyVar: Integer;

const
  { @deprecated }
  MyConst = 1;

implementation

end.