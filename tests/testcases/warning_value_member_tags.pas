unit warning_value_member_tags;

interface

type
  { @member MyField1 First description of MyField1.
  
    @member MyField2 First description of MyField2.
    @member MyField2 Second description of MyField2.
  
    @member NotExistsingMember Description of NotExistsingMember.

    This should cause 3 warnings: 
    MyField1 has two descriptions,
    MyField2 has two descriptions,
    and NotExistsingMember does not exist. }
  TMyClass = class
    { @abstract(Second description of MyField1.) }
    MyField1: Integer;
    MyField2: Integer;
  end;

  { @value meOne First description of meOne.
    @value meOne Second description of meOne.
    
    @value meTwo First description of meTwo.
    
    @value meNotExisting Description of meNotExisting.

    This should cause 3 warnings: 
    meOne has two descriptions,
    meTwo has two descriptions,
    and meNotExisting does not exist. }
  TMyEnum = (
    meOne, 
    { Second description of meTwo. }
    meTwo);

implementation

end.