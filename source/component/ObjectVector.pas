{
  @cvs($Date$)
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
    procedure DeleteAt(const AIndex: Integer);
  public
    constructor Create(const AOwnsObject: boolean); virtual;
    destructor Destroy; override;
    function Count: Integer;
    procedure Sort(Compare: TListSortCompare);
    procedure Clear; virtual;       
    procedure Insert(const AObject: TObject);
    property Items[AIndex: Integer]: TObject read GetItem write SetItem;
  end;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean; 

implementation

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

{ TObjectVector }

procedure TObjectVector.Clear;
var
  i: Integer;
begin
  if FOwnsObject then begin
    for i := 0 to count-1 do begin
      Items[i].Free;
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
  Flist := nil;
  inherited Destroy;
end;

function TObjectVector.GetItem(AIndex: Integer): TObject;
begin
  Result := TObject(FList.Items[AIndex]);
end;

procedure TObjectVector.Insert(const AObject: TObject);
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
