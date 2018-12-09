unit ok_no_sort;

interface

type
  { Enum values are not sorted }
  TMyEnum = (meZZZ, meAAA);
  
  { Record fields are not sorted }
  TMyRecord = record
    ZZZ: Integer;
    AAA: Integer;
  end;
  
  { Class fields are sorted }
  TMyClass = class
    ZZZ: Integer;
    AAA: Integer;
  end;
  
implementation

end.