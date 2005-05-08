unit ok_value_member_tags;

interface

type
  { @member MyField Description of MyField here.
    @member(MyMethod Description of MyMethod here, 
      using parenthesis.)
    @member MyProperty Description of MyProperty here. }
  TMyClass = class
    MyField: Integer;
    procedure MyMethod;
    property MyProperty: Integer read MyField write MyField;
  end;
  
  { @member MyField Description of MyField in TMyRecord here. }
  TMyRecord = record
    MyField: Integer;
  end;
  
  { @value meOne Description of meOne follows.
    @value meThree Description of meThree follows. }
  TMyEnum = (meOne, meTwo, meThree);
  
implementation

end.

