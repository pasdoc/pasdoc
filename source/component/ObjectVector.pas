{
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a simple object vector
}
unit ObjectVector;

interface
uses
  Classes;

type
  TObjectVector = class
  protected
    FList: TList;
    FOwnsObject: boolean;
    function GetItem(AIndex: Integer): TObject;
    procedure SetItem(AIndex: Integer; const Value: TObject);
    procedure InsertObjectLast(const AObject: TObject);
    procedure DeleteAt(const AIndex: Integer);
  public
    constructor Create(const AOwnsObject: boolean); virtual;
    destructor Destroy; override;
    function Count: Integer;
    property ObjectAt[AIndex: Integer]: TObject read GetItem write SetItem;
    procedure Sort(Compare: TListSortCompare);
    procedure Clear;
    procedure Add(const AObject: TObject);
  end;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean; 
procedure FreeAndNilIfEmpty(var AObject);

implementation

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

procedure FreeAndNilIfEmpty(var AObject);
begin
  Assert(TObject(AObject) is TObjectVector);
  if TObjectVector(AObject).Count = 0 then begin
    TObjectVector(AObject).Free;
    TObjectVector(AObject) := nil;
  end;
end;

{ TObjectVector }

procedure TObjectVector.Add(const AObject: TObject);
begin
  InsertObjectLast(AObject);
end;

procedure TObjectVector.Clear;
var
  i: Integer;
begin
  if FOwnsObject then begin
    for i := 0 to count-1 do begin
      ObjectAt[i].Free;
    end;
  end;
  FList.Clear;
end;

function TObjectVector.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create;
  FList := TList.Create;
  FOwnsObject := AOwnsObject;
end;

procedure TObjectVector.DeleteAt(const AIndex: Integer);
begin
  if FOwnsObject then begin
    TObject(FList.Items[AIndex]).Free;
  end;
  FList.Delete(AIndex);
end;

destructor TObjectVector.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TObjectVector.GetItem(AIndex: Integer): TObject;
begin
  Result := FList.Items[AIndex];
end;

procedure TObjectVector.InsertObjectLast(const AObject: TObject);
begin
  FList.Add(AObject)
end;

procedure TObjectVector.SetItem(AIndex: Integer; const Value: TObject);
begin
  FList.Items[AIndex] := Value;
end;

procedure TObjectVector.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

end.
