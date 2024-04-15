unit ok_link_function_type;

interface

type
  TMyFunction = function(A: Integer): Integer;

  TMyRecord = record
    A: Integer;
  end;

{ Give @link(TMyFunction) and @link(TMyRecord) to test it. }
procedure Test(F: TMyFunction; R: TMyRecord);

implementation

end.