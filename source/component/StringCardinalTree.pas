{
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a n-ary tree for items consisting of a string and a cardinal each
}
unit StringCardinalTree;

interface
uses
  Contnrs;

type
  TStringCardinalTreeNode = class
  protected
    FChildren: TObjectList;
    FParent: TStringCardinalTreeNode;
  public
    name: string;
    Number: cardinal;
    constructor Create;
    destructor Destroy; override;
    function AddChild(const name: string; const number: Cardinal):
      TStringCardinalTreeNode;
    function Level: Integer;
    function FindItemCI(const AName: string): TStringCardinalTreeNode;
    procedure Adopt(const AChild: TStringCardinalTreeNode);
    function Orphan(const AChild: TStringCardinalTreeNode): boolean;
  end;

  TStringCardinalTree = class
  protected
    FRoot: TStringCardinalTreeNode;
    function GetIsEmpty: boolean;
    function GetFirstItem: TStringCardinalTreeNode;
    procedure NeedRoot;
  public
    function PItemOfNameCI(const name: string): TStringCardinalTreeNode;
    function InsertNameLast(const name: string): TStringCardinalTreeNode;
    procedure InsertNameNumberChildLast(
      const Parent: TStringCardinalTreeNode;
      const name: string;
      const dummy: Integer);
    procedure MoveChildLast(const Child, Parent: TStringCardinalTreeNode);

    property IsEmpty: boolean read GetIsEmpty;
    property PFirstItem: TStringCardinalTreeNode read GetFirstItem;

    function Level(const ANode: TStringCardinalTreeNode): Integer;
    function PNextItem(const ANode: TStringCardinalTreeNode):
      TStringCardinalTreeNode;

    procedure SortRecurseByNameCI;

    constructor Create;
    destructor Destroy; override;
  end;

function NewStringCardinalTree: TStringCardinalTree;

implementation
uses
  Utils,
  Classes,
  SysUtils;

function NewStringCardinalTree: TStringCardinalTree;
begin
  Result := TStringCardinalTree.Create;
end;

{ TStringCardinalTree }

constructor TStringCardinalTree.Create;
begin
  FRoot := nil;
end;

destructor TStringCardinalTree.Destroy;
begin
  FRoot.Free;
  inherited;
end;

function TStringCardinalTree.GetFirstItem: TStringCardinalTreeNode;
begin
  Result := nil;
  if Assigned(FRoot) then begin
    if FRoot.FChildren.Count > 0 then begin
      Result := TStringCardinalTreeNode(FRoot.FChildren[0]);
    end;
  end;
end;

function TStringCardinalTree.GetIsEmpty: boolean;
begin
  Result := not Assigned(FRoot);
end;

function TStringCardinalTree.InsertNameLast(
  const name: string): TStringCardinalTreeNode;
begin
  NeedRoot;
  Result := FRoot.AddChild(name, 0);
end;

procedure TStringCardinalTree.InsertNameNumberChildLast(
  const Parent: TStringCardinalTreeNode; const name: string;
  const dummy: Integer);
begin
  if Parent = nil then begin
    NeedRoot;
    FRoot.AddChild(name, dummy);
  end else begin
    Parent.AddChild(name, dummy);
  end
end;

function TStringCardinalTree.Level(
  const ANode: TStringCardinalTreeNode): Integer;
begin
  Result := ANode.Level;
end;

procedure TStringCardinalTree.MoveChildLast(const Child,
  Parent: TStringCardinalTreeNode);
begin
  NeedRoot;
  if FRoot.Orphan(Child) then begin
    Parent.Adopt(Child);
  end;
end;

procedure TStringCardinalTree.NeedRoot;
begin
  if not Assigned(FRoot) then begin
    FRoot := TStringCardinalTreeNode.Create;
  end;
end;

function TStringCardinalTree.PItemOfNameCI(
  const name: string): TStringCardinalTreeNode;
begin
  NeedRoot;
  Result := FRoot.FindItemCI(name);
end;

function TStringCardinalTree.PNextItem(
  const ANode: TStringCardinalTreeNode): TStringCardinalTreeNode;
var
  idx: Integer;
  LNode: TStringCardinalTreeNode;
begin
  Result := nil;
  if ANode.FChildren.Count > 0 then begin
    Result := TStringCardinalTreeNode(ANode.FChildren[0]);
  end;
  if Result = nil then begin
    if Assigned(ANode.FParent) then begin
      idx := ANode.FParent.FChildren.IndexOf(ANode);
      if idx + 1 < ANode.FParent.FChildren.Count then begin
        Result := TStringCardinalTreeNode(ANode.FParent.FChildren[idx + 1]);
      end;
    end;
  end;
  if Result = nil then begin
    LNode := ANode.FParent;
    while Assigned(LNode) do begin
      if Assigned(LNode.FParent) then begin
        idx := LNode.FParent.FChildren.IndexOf(LNode);
        if LNode.FParent.FChildren.Count > idx + 1 then begin
          Result := TStringCardinalTreeNode(LNode.FParent.FChildren[idx +
            1]);
          break;
        end;
      end;
      LNode := LNode.FParent;
    end;
  end;
end;

procedure TStringCardinalTree.SortRecurseByNameCI;
begin

end;

{ TStringCardinalTreeNode }

function TStringCardinalTreeNode.AddChild(const name: string;
  const number: Cardinal): TStringCardinalTreeNode;
begin
  Result := TStringCardinalTreeNode.Create;
  Result.name := name;
  Result.Number := number;
  Result.FParent := Self;
  FChildren.Add(Result);
end;

procedure TStringCardinalTreeNode.Adopt(const AChild:
  TStringCardinalTreeNode);
begin
  FChildren.Add(AChild);
  AChild.FParent := Self;
end;

constructor TStringCardinalTreeNode.Create;
begin
  FParent := nil;
  FChildren := TObjectList.Create;
  FChildren.OwnsObjects := False;
end;

destructor TStringCardinalTreeNode.Destroy;
begin
  FChildren.OwnsObjects := True;
  FChildren.Free;
  inherited;
end;

function TStringCardinalTreeNode.FindItemCI(
  const AName: string): TStringCardinalTreeNode;
var
  i: Integer;
  LName: string;
begin
  Result := nil;
  LName := LowerCase(AName);
  for i := 0 to FChildren.Count - 1 do begin
    if LowerCase(TStringCardinalTreeNode(FChildren[i]).name) = LName then
      begin
      Result := TStringCardinalTreeNode(FChildren[i]);
      break;
    end;
    Result := TStringCardinalTreeNode(fchildren[i]).FindItemCI(AName);
    if Assigned(Result) then break;
  end;
end;

function TStringCardinalTreeNode.Level: Integer;
begin
  if Assigned(FParent) then begin
    Result := FParent.Level + 1;
  end else begin
    Result := 0;
  end;
end;

function TStringCardinalTreeNode.Orphan(
  const AChild: TStringCardinalTreeNode): boolean;
var
  i: Integer;
begin
  i := FChildren.IndexOf(AChild);
  Result := false;
  if i >= 0 then begin
    FChildren.Delete(i);
    Result := true;
  end else begin
    for i := FChildren.Count - 1 downto 0 do begin
      Result := TStringCardinalTreeNode(FChildren[i]).Orphan(AChild);
      if Result then break;
    end;
  end;
end;

end.
