{ Test unit to test various @--sort settings. }

unit ok_sorting;

interface

uses ZZZ, AAA;

type
  TMyClass = class
    ZZZField: Integer;
    AAAField: Integer;
    procedure ZZZMethod;
    procedure AAAMethod;
    property ZZZProp;
    property AAAProp;
  end;

  TMyRecord = record
    ZZZField: Integer;
    AAAField: Integer;
  end;

  ZZZClass = class end;
  AAAClass = class end;

  ZZZType = Integer;
  AAAType = Integer;

const
  ZZZConst = 1;
  AAAConst = 2;

var
  ZZZVar: Integer;
  AAAVar: Integer;

procedure ZZZProc;
procedure AAAProc;

implementation

end.