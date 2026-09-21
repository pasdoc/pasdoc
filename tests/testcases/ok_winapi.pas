unit ok_winapi;

interface

function SslChangeNotify(
  hEvent: THandle;
  dwFlags: Integer): Integer; winapi; external 'foo.dll';

function SslChangeNotify2(
  hEvent: THandle;
  dwFlags: Integer): Integer; winapi;

implementation

function SslChangeNotify2(
  hEvent: THandle;
  dwFlags: Integer): Integer; winapi;
begin
  Result := 123;
end;

end.