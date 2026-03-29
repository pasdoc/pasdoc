unit ok_type_helpers;

interface

type
  TStringHelper = record helper for string
  private type
    TSomething = class
    end;
  private
    function ConvertToSomething: TSomething; inline;
  end;

  TMyEnum = (one, two, three);

  { You can extend enums too.
    https://docwiki.embarcadero.com/RADStudio/Sydney/en/Class_and_Record_Helpers_(Delphi)
    suggests you cannot,
    but https://www.thoughtco.com/record-helpers-for-sets-1058204
    and actual test with Delphi 12 shows you can: }
  TMyEnumHelper = record helper for TMyEnum
    function GetNext: TMyEnum;
  end;

implementation

function TStringHelper.ConvertToSomething: TSomething;
begin
  Result := TSomething.Create;
end;

function TMyEnumHelper.GetNext: TMyEnum;
begin
  if Self = High(TMyEnum) then
    Result := Low(TMyEnum)
  else
    Result := Succ(Self);
end;

end.