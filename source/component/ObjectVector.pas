{
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a simple object vector
}
unit ObjectVector;

interface
uses
  Contnrs,
  Classes;

type
  TObjectVector = class
  protected
    FList: TObjectList;
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
  end;

function IsNilOrEmpty(const AOV: TObjectVector): boolean; overload;
procedure FreeAndNilIfEmpty(var AObject);

implementation

function IsNilOrEmpty(const AOV: TObjectVector): boolean;
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

function TObjectVector.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create;
  FList := TObjectList.Create(AOwnsObject);
end;

procedure TObjectVector.DeleteAt(const AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

destructor TObjectVector.Destroy;
begin
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
