unit ok_generic_specialize;

{$mode ObjFPC}{$H+}

interface

uses
  FGL, Classes, SysUtils;

type

{ TSimpleMap }

generic TSimpleMap<T1,T2>=class
  private
    type
      TKeyList=specialize TFPGList<T1>;
      TValueList=specialize TFPGList<T2>;
    var
      KeyList : TKeyList;
      ValueList : TValueList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(aKey: T1);
  end;

implementation

{ TSimpleMap }

constructor TSimpleMap.Create;
begin
  KeyList:=TKeyList.Create;
  ValueList:=TValueList.Create;
end;

destructor TSimpleMap.Destroy;
begin
  KeyList.Free;
  ValueList.Free;
  inherited Destroy;
end;

procedure TSimpleMap.Delete(aKey: T1);
var
  ind : integer;
begin
  ind:=KeyList.IndexOf(aKey);
  if ind>-1 then
    begin
      KeyList.Delete(ind);
      ValueList.Delete(ind);
    end;
end;

end.

