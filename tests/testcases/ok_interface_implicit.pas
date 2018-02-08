unit ok_interface_implicit;

interface

type
  IMyInterface = interface
    procedure PublicMethod;
  end;

  TMyRecord = record
    PublicField: Integer;
  end;

  TMyPackedRecord = packed record
    PublicField: Integer;
  end;

  TMyClass = class
    ImplicitField: Integer;
  public
    PublicField: Integer;
  end;

implementation

end.