{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @abstract(string vector - based on TStringList)
  The string vector is based on TStringList and simply exports
  a few extra functions - I did this so I didn't have to change
  so much old code, this has only little additional
  functionality
}
unit StringVector;

interface
uses
  Classes;
    
type
  TIterateFunc = function(const AString: string): string;
  TStringVector = class(TStringList)
  public
    function FirstName: string;
    procedure LoadFromTextFileAdd(const AFilename: string);
    procedure RemoveAllNamesCI(const AName: string);
    function ExistsNameCI(const AName: string): boolean;
    function IsEmpty: boolean;
    procedure Iterate(const AItFunc: TIterateFunc);
    procedure AddNotExisting(const AString: string);
  end;

function NewStringVector: TStringVector;
function StringVectorIsNilOrEmpty(const AOV: TStringVector): boolean;

implementation
uses
  SysUtils;

function StringVectorIsNilOrEmpty(const AOV: TStringVector): boolean;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

function NewStringVector: TStringVector;
begin
  Result := TStringVector.Create;
  Result.Duplicates := dupIgnore;
end;

{ TStringVector }

procedure TStringVector.AddNotExisting(const AString: string);
begin
  if IndexOf(AString) < 0 then begin
    Add(AString);
  end;
end;

function TStringVector.ExistsNameCI(const AName: string): boolean;
var
  i: Integer;
  LName: string;
begin
  LName := LowerCase(AName);
  Result := false;
  for i := Count - 1 downto 0 do begin
    if LowerCase(Get(i)) = LName then begin
      Result := True;
      break;
    end;
  end;
end;

function TStringVector.FirstName: string;
begin
  Result := Get(0);
end;

function TStringVector.IsEmpty: boolean;
begin
  Result := Count = 0;
end;

procedure TStringVector.Iterate(const AItFunc: TIterateFunc);
var
  i: Integer;
begin
  for i := 0 to count - 1 do begin
    Strings[i] := AItFunc(Strings[i]);
  end;
end;

procedure TStringVector.LoadFromTextFileAdd(
  const AFilename: string);
var
  LCurrent: string;
begin
  LCurrent := Text;
  LoadFromFile(AFilename);
  Add(LCurrent);
end;

procedure TStringVector.RemoveAllNamesCI(const AName: string);
var
  i: Integer;
  LName: string;
begin
  LName := LowerCase(AName);
  for i := Count - 1 downto 0 do begin
    if LowerCase(Get(i)) = LName then begin
      Delete(i);
    end;
  end;
end;

end.
