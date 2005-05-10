unit ok_value_member_tags;

interface

type
  { @member MyField Description of MyField here.
    @member(MyMethod Description of MyMethod here, 
      using parenthesis.)
    @member(MyProperty Description of MyProperty here,
      with some recursive tags inside: @code(Some code with
      a link to @link(TMyRecord)).) }
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
    @value meThree Description of meThree, with some link: @link(TMyClass.MyField). }
  TMyEnum = (meOne, meTwo, meThree);
  
implementation

end.

