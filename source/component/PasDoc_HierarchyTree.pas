{
  Copyright 1998-2018 PasDoc developers.

  This file is part of "PasDoc".

  "PasDoc" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "PasDoc" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "PasDoc"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a n-ary tree for PasItems --- for use in Class Hierarchy
}
unit PasDoc_HierarchyTree;

{$I pasdoc_defines.inc}

interface

uses
  Classes,
  PasDoc_Items;

type
  TPasItemNode = class(TObject)
  protected
    FChildren: TList;
    FParent: TPasItemNode;
    FItem: TPasItem;
    FName: string;
    function GetName: string;
  protected
    procedure AddChild(const Child: TPasItemNode); overload;
    function AddChild(const AName: string): TPasItemNode; overload;
    function AddChild(const AItem: TPasItem): TPasItemNode; overload;
    function FindItem(const AName: string): TPasItemNode;
    procedure Adopt(const AChild: TPasItemNode);
    function Orphan(const AChild: TPasItemNode): boolean;
    procedure Sort;
  public
    constructor Create;
    destructor Destroy; override;
    function Level: Integer;
    property Name: string read GetName;
    property Item: TPasItem read FItem;
    property Parent: TPasItemNode read FParent;
  end;

  TStringCardinalTree = class
  protected
    FRoot: TPasItemNode;
    function GetIsEmpty: boolean;
    function GetFirstItem: TPasItemNode;
    procedure NeedRoot;
  public
    function ItemOfName(const AName: string): TPasItemNode;
    function InsertName(const AName: string): TPasItemNode; overload;
    function InsertItem(const AItem: TPasItem): TPasItemNode; overload;
    function InsertParented(const AParent: TPasItemNode;
      const AItem: TPasItem): TPasItemNode; overload;
    function InsertParented(const AParent: TPasItemNode;
      const AName: string): TPasItemNode; overload;
    procedure MoveChildLast(const Child, Parent: TPasItemNode);

    property IsEmpty: boolean read GetIsEmpty;
    property FirstItem: TPasItemNode read GetFirstItem;

    function Level(const ANode: TPasItemNode): Integer;
    function NextItem(const ANode: TPasItemNode): TPasItemNode;

    procedure Sort;

    constructor Create;
    destructor Destroy; override;
  end;

function NewStringCardinalTree: TStringCardinalTree;

implementation
uses
  SysUtils;

function SortProc(A, B: Pointer): Integer;
begin
  Result := CompareText(TPasItemNode(A).Name, TPasItemNode(B).Name)
end;

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

function TStringCardinalTree.GetFirstItem: TPasItemNode;
begin
  Result := nil;
  if Assigned(FRoot) then begin
    if FRoot.FChildren.Count > 0 then begin
      Result := TPasItemNode(FRoot.FChildren[0]);
    end;
  end;
end;

function TStringCardinalTree.GetIsEmpty: boolean;
begin
  Result := not Assigned(FRoot);
end;

function TStringCardinalTree.InsertName(
  const AName: string): TPasItemNode;
begin
  NeedRoot;
  Result := FRoot.AddChild(AName);
end;

function TStringCardinalTree.InsertParented(const AParent: TPasItemNode;
  const AItem: TPasItem): TPasItemNode;
begin
  if AParent = nil then begin
    NeedRoot;
    Result := FRoot.AddChild(AItem);
  end else begin
    Result := AParent.AddChild(AItem);
  end
end;

function TStringCardinalTree.InsertParented(const AParent: TPasItemNode;
  const AName: string): TPasItemNode;
begin
  if AParent = nil then begin
    NeedRoot;
    Result := FRoot.AddChild(AName);
  end else begin
    Result := AParent.AddChild(AName);
  end
end;

function TStringCardinalTree.Level(
  const ANode: TPasItemNode): Integer;
begin
  Result := ANode.Level;
end;

procedure TStringCardinalTree.MoveChildLast(const Child,
  Parent: TPasItemNode);
begin
  NeedRoot;
  if FRoot.Orphan(Child) then begin
    Parent.Adopt(Child);
  end;
end;

procedure TStringCardinalTree.NeedRoot;
begin
  if not Assigned(FRoot) then begin
    FRoot := TPasItemNode.Create;
  end;
end;

function TStringCardinalTree.ItemOfName(
  const AName: string): TPasItemNode;
begin
  NeedRoot;
  Result := FRoot.FindItem(AName);
end;

function TStringCardinalTree.NextItem(
  const ANode: TPasItemNode): TPasItemNode;
var
  idx: Integer;
  LNode: TPasItemNode;
begin
  Result := nil;
  if ANode.FChildren.Count > 0 then begin
    Result := TPasItemNode(ANode.FChildren[0]);
  end;
  if Result = nil then begin
    if Assigned(ANode.FParent) then begin
      idx := ANode.FParent.FChildren.IndexOf(ANode);
      if idx + 1 < ANode.FParent.FChildren.Count then begin
        Result := TPasItemNode(ANode.FParent.FChildren[idx + 1]);
      end;
    end;
  end;
  if Result = nil then begin
    LNode := ANode.FParent;
    while Assigned(LNode) do begin
      if Assigned(LNode.FParent) then begin
        idx := LNode.FParent.FChildren.IndexOf(LNode);
        if LNode.FParent.FChildren.Count > idx + 1 then begin
          Result := TPasItemNode(LNode.FParent.FChildren[idx +
            1]);
          break;
        end;
      end;
      LNode := LNode.FParent;
    end;
  end;
end;

procedure TStringCardinalTree.Sort;
begin
  if Assigned(FRoot) then begin
    FRoot.Sort;
  end;
end;

function TStringCardinalTree.InsertItem(
  const AItem: TPasItem): TPasItemNode;
begin
  Result := InsertParented(nil, AItem);
end;

{ TPasItemNode }

procedure TPasItemNode.AddChild(const Child: TPasItemNode);
begin
  FChildren.Add(Child);
end;

function TPasItemNode.AddChild(const AName: string): TPasItemNode;
begin
  Result := TPasItemNode.Create;
  Result.FItem := nil;
  Result.FName := AName;
  Result.FParent := Self;
  AddChild(Result);
end;

function TPasItemNode.AddChild(const AItem: TPasItem): TPasItemNode;
begin
  Result := TPasItemNode.Create;
  Result.FItem := AItem;
  Result.FParent := Self;
  AddChild(Result);
end;

procedure TPasItemNode.Adopt(const AChild:
  TPasItemNode);
begin
  FChildren.Add(AChild);
  AChild.FParent := Self;
end;

constructor TPasItemNode.Create;
begin
  FParent := nil;
  FChildren := TList.Create;
  FItem := nil;
end;

destructor TPasItemNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FChildren.Count-1 do begin
    TObject(FChildren.Items[i]).Free;
  end;
  FChildren.Free;
  inherited;
end;

function TPasItemNode.FindItem(
  const AName: string): TPasItemNode;
var
  i: Integer;
  LName: string;
begin
  Result := nil;
  LName := LowerCase(AName);
  for i := 0 to FChildren.Count - 1 do begin
    if LowerCase(TPasItemNode(FChildren[i]).Name) = LName then
      begin
      Result := TPasItemNode(FChildren[i]);
      break;
    end;
    Result := TPasItemNode(FChildren[i]).FindItem(AName);
    if Assigned(Result) then break;
  end;
end;

function TPasItemNode.GetName: string;
begin
  if Assigned(FItem) then begin
    Result := FItem.Name;
  end else begin
    Result := FName;
  end;
end;

function TPasItemNode.Level: Integer;
begin
  if Assigned(FParent) then begin
    Result := FParent.Level + 1;
  end else begin
    Result := 0;
  end;
end;

function TPasItemNode.Orphan(
  const AChild: TPasItemNode): boolean;
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
      Result := TPasItemNode(FChildren[i]).Orphan(AChild);
      if Result then break;
    end;
  end;
end;

procedure TPasItemNode.Sort;
var
  i: Integer;
begin
  FChildren.Sort( {$IFDEF FPC}@{$ENDIF} SortProc);
  for i := FChildren.Count-1 downto 0 do begin
    TPasItemNode(FChildren[i]).Sort;
  end;
end;

end.
