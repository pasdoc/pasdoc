unit ok_attributes;

interface

// Delphi attributes test from http://www.malcolmgroves.com/blog/?p=530

type
  TPerson = class
  private
    FName: String;
    FAge: Integer;
  public
    [NonEmptyString('Must provide a Name')]
    property Name : String read FName write FName;
    [MinimumInteger(18, 'Must be at least 18 years old')]
    [MaximumInteger(65, 'Must be no older than 65 years')]
    property Age : Integer read FAge write FAge;
  end;

  // Test that GUIDs are handled gracefully
  IUIContainer = interface
  ['{0F0BA87D-95C3-4520-B9F9-CDF30015FDB3}']
  end;

// Tests from https://github.com/pasdoc/pasdoc/issues/179

  TMyClass1 = class
    [Volatile] FLockCount: Integer;
  end;

  TMyClass2 = class
  var
    [Volatile] FLockCount: Integer;
  end;

  IEnumerator<T> = interface(IEnumerator)
    [HPPGEN('virtual T __fastcall GetCurrentT(void) = 0')]
    function GetCurrent: T;
    [HPPGEN('__property T Current = {read=GetCurrentT}')]
    property Current: T read GetCurrent;
  end;

implementation

end.
