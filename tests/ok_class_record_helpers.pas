// Test case for Delphi class and record helpers
unit ok_class_record_helpers;

interface

type
  TForwardClass = class; 
  
  // description of @name  
  TSimpleRecord = record 
    // description of @name
    SimpleField: Integer;
  end;
  // description of @name
  TMyRecordHelper = record helper for TSimpleRecord
  protected
    // description of @name
    procedure HelloWorld;
  end;
  
  // description of @name
  TAncestorClassHelper = class helper for TApplication end;
  
  // description of @name
  TDescendantClassHelper = class helper(TAncestorClassHelper) for TApplication
    // description of @name
    procedure HelloWorld;
  end;

implementation

end.
