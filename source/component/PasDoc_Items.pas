{ @abstract(defines all items that can appear within a Pascal unit's interface)
  @created(11 Mar 1999)
  @lastmod(14 Jun 2002)
  @lastmod(2003-03-29)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))

For each type, variable, class etc. (short: item) that may appear in a Pascal
source code file and can thus be taken into the documentation, this unit
provides an object type which will store name, unit, description and more
on this item. }

unit PasDoc_Items;

interface

uses
  StringVector,
  ObjectVector,
  Hashes,
  Classes;

type
  { Accessibility of a field/method }
  TAccessibility = (
    { indicates field or method is public }
    STATE_PUBLIC,
    { indicates field or method is protected }
    STATE_PROTECTED,
    { indicates field or method is private }
    STATE_PRIVATE,
    { indicates field or method is published }
    STATE_PUBLISHED,
    { indicates field or method is automated }
    STATE_AUTOMATED
    );

  TAccessibilities = set of TAccessibility;

type
  TPasCio = class;
  TPasMethod = class;
  TPasProperty = class;
  TPasUnit = class;

  TPasItems = class;
  TPasMethods = class;
  TPasProperties = class;

  { basic linkable item in pasdoc hierarchy }
  TPasItem = class
  protected
    FDeprecated: boolean;
    FPlatform: boolean;
    FAbbreviations: TStringList;
    { list of strings, each representing one author of this item }
    FAuthors: TStringVector;
    { if assigned, contains string with date of creation }
    FCreated: string;
  public
    property Authors: TStringVector read FAuthors;
    property Created: string read FCreated;
  public
    { description of this item, a single sentence }
    Description: string;
    { more detailed description of this item, mostly more than one
      sentence }
    DetailedDescription: string;
    { a full link that should be enough to link this item from anywhere else }
    FullLink: string;
    { if assigned, contains string with date of last modification }
    LastMod: string;
    { if this item is part of an object or class, the corresponding info object is stored here, nil otherwise }
    MyObject: TPasCio;
    { pointer to unit this item belongs to }
    MyUnit: TPasUnit;
    { name of the item }
    Name: string;
    { One of the STATE_xxx constants, determines access rights
      (public, private, etc.). }
    State: TAccessibility;
    { }
    destructor Destroy; override;

    procedure DescriptionExtractTag(var ADescription: string; const Offs1,
      Offs2, Offs3: Integer; out s: string);
    function DescriptionFindTag(const ADescription, TagName: string; var
      Offs1, Offs2, Offs3: Integer): Boolean;
    function DescriptionFindTagParameters(const ADescription: string; var
      Offs1, Offs2: Integer): Boolean;
    function DescriptionGetTagName(const ADescription: string; var Offset:
      Integer): string;
    procedure DescriptionGetTag(var ADescription: string; const Remove:
      Boolean; const Offs1, Offs2, Offs3: Integer; out s: string);

    function FindItem(const ItemName: string): TPasItem; virtual;
    { }
    function FindName(S1, S2, S3: string; n: Integer): TPasItem; virtual;
    { Returns DetailedDescription if available, otherwise Description,
      otherwise nil. }
    function GetDescription: string;
    { Searches for an abstract tag within the Description field of
      this item. If one is found, Description is copied to DetailedDescription
      and the abstract tag becomes the new Description. This procedure
      should be called after the dates (created and lastmod) and the
      author tags have been handled, as they are searched in Description. }
    procedure HandleAbstractTag;
    { }
    procedure HandleAuthorTags;
    { }
    procedure HandleCreatedTag;
    { }
    procedure HandleLastModTag;
    { Returns true if there is a detailled or a normal description available. }
    function HasDescription: Boolean;
    { Inserts an item into a collection.
    Creates collection if it does not exist already. }
    procedure InsertItem(const Item: TPasItem; var c: TPasItems);
    { inserts a method into a collection; creates collection if necessary }
    procedure InsertMethod(const Method: TPasMethod; var c: TPasMethods);
    { inserts a property into a collection; creates collection if necessary }
    procedure InsertProperty(const Prop: TPasProperty; var c: TPasProperties);
    { returns the qualified name of the item }
    function QualifiedName: String;
    { is this item deprecated? }
    property IsDeprecated: boolean read FDeprecated write FDeprecated;
    { is this item platform specific? }
    property IsPlatform: boolean read FPlatform write FPlatform;

    property Abbreviations: TStringList read FAbbreviations write FAbbreviations;
  end;

  { @abstract(used for constants/variables) }
  TPasVarConst = class(TPasItem)
    { full declaration, including type, default values, etc }
    FullDeclaration: string;
  end;

  { ---------------------------------------------------------------------------- }

  { Methodtype for @link(TPasMethod) }
  TMethodType = (METHOD_CONSTRUCTOR, METHOD_DESTRUCTOR,
    METHOD_FUNCTION_PROCEDURE);

  { extends @link(TPasItem) to store method and function-/procedure-specific
    information }
  TPasMethod = class(TPasItem)
    { full declaration, including parameter list and procedural directives }
    FullDeclaration: string;
    { }
    What: TMethodType;
  end;

  TPasProperty = class(TPasItem)
    { full declaration, including read/write and storage specifiers }
    FullDeclaration: string;
    { contains the optional index declaration, including brackets }
    IndexDecl: string;
    { contains the type of the property }
    Proptype: string;
    { read specifier }
    Reader: string;
    { write specifier }
    Writer: string;
    { true if the property is the default property }
    Default: Boolean;
    { keeps default value specifier }
    DefaultID: string;
    { true if Nodefault property }
    NoDefault: Boolean;
    { keeps Stored specifier }
    StoredId: string;
  end;

  { enumeration type to determine type of TObjectInfo item: class,
    interface or object }
  TCIOType = (CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE, CIO_OBJECT, CIO_RECORD, CIO_PACKEDRECORD);

  { Extends @link(TPasItem) to store all items in a class / an object, e.g.
    fields. }
  TPasCio = class(TPasItem)
    { name of the ancestor class / object }
    Ancestors: TStringVector;
    { list of all fields }
    Fields: TPasItems;
    { list of all methods }
    Methods: TPasMethods;
    { determines if this is a class, an interface or an object }
    MyType: TCIOType;
    { name of documentation output file (if each class / object gets
      its own file, that's the case for HTML, but not for TeX) }
    OutputFileName: string;
    { list of properties }
    Properties: TPasProperties;

    destructor Destroy; override;

    { Simply returns the result of a call to @link(FindFieldMethodProperty). }
    function FindItem(const ItemName: string): TPasItem; override;
    { If this class (or interface or object) contains a field, method or
      property with the name of ItemName, the corresponding item pointer is
      returned. }
    function FindFieldMethodProperty(const ItemName: string): TPasItem;

    procedure SortPasItems;
  end;

  { extends @link(TPasItem) to store anything about a unit, its constants,
    types etc.; also provides methods for parsing a complete unit }
  TPasUnit = class(TPasItem)
    { list of classes and objects defined in this unit }
    CIOs: TPasItems;
    { list of constants defined in this unit }
    Constants: TPasItems;
    { list of functions and procedures defined in this unit }
    FuncsProcs: TPasMethods;
    { name of documentation output file
      THIS SHOULD NOT BE HERE! }
    OutputFileName: string;
    { the names of all units mentioned in a uses clause in the interface
      section of this unit }
    UsesUnits: TStringVector;
    { list of classes and objects defined in this unit }
    SourceFileName: string;
    { list of types defined in this unit }
    Types: TPasItems;
    { list of variables defined in this unit }
    Variables: TPasItems;
    { dispose of all dynamically allocated memory in this object }
    destructor Destroy; override;
    procedure AddCIO(const i: TPasCio);
    procedure AddConstant(const i: TPasItem);
    procedure AddType(const i: TPasItem);
    procedure AddVariable(const i: TPasItem);
    function FindFieldMethodProperty(const S1, S2: string): TPasItem;
    function FindItem(const ItemName: string): TPasItem; override;

    procedure SortPasItems;
  end;

  { ---------------------------------------------------------------------------- }

  { Container class to store a list of @link(TPasItem)s. }
  TPasItems = class(TObjectVector)
  private
    FHash: TObjectHash;
    function GetPasItemAt(const AIndex: Integer): TPasItem;
    procedure SetPasItemAt(const AIndex: Integer; const Value: TPasItem);
  public
    { Copies all Items from c to this object, not changing c at all. }
    procedure CopyItems(const c: TPasItems);
    { Counts classes, interfaces and objects within this collection. }
    procedure CountCIO(out c, i, o: Integer);
    { Compares each element's name field with Name and returns the item on
      success, nil otherwise.
      Name's case is not regarded. }
    function FindName(const AName: string): TPasItem;
    { Inserts all items of C into this collection.
      Disposes C and sets it to nil. }
    procedure InsertItems(const c: TPasItems);
    { Checks each element's State field and removes all elements with a value
      of STATE_PRIVATE. }
    procedure RemovePrivateItems;

    property PasItemAt[const AIndex: Integer]: TPasItem read GetPasItemAt
      write SetPasItemAt;

    procedure SortByPasItemName;

    procedure InsertObjectLast(const AObject: TPasItem);
    procedure DeleteAt(const AIndex: Integer);
    constructor Create(const AOwnsObject: Boolean); override;
    destructor Destroy; override;
  end;

  { ---------------------------------------------------------------------------- }

  { @Name holds a collection of methods. It introduces no
    new methods compared to @link(TPasItems), but this may be
    implemented in a later stage. }
  TPasMethods = class(TPasItems)
  end;

  { @Name holds a collection of properties. It introduces no
    new methods compared to @link(TPasItems), but this may be
    implemented in a later stage. }
  TPasProperties = class(TPasItems)
  end;

  { ---------------------------------------------------------------------------- }
  { TPasUnits
  { ---------------------------------------------------------------------------- }

  { @abstract(Holds a collection of units.) }
  TPasUnits = class(TPasItems)
  private
    function GetUnitAt(const AIndex: Integer): TPasUnit;
    procedure SetUnitAt(const AIndex: Integer; const Value: TPasUnit);
  public
    property UnitAt[const AIndex: Integer]: TPasUnit
      read GetUnitAt
      write SetUnitAt;
    function ExistsUnit(const AUnit: TPasUnit): Boolean;
  end;

const
  CIO_NonHierarchy = [Low(TCIOType)..High(TCIOType)] - [CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE, CIO_OBJECT];

implementation

uses
  SysUtils,
  Utils,
  Contnrs;

function ComparePasItemsByName(PItem1, PItem2: Pointer): Integer;
begin
  Result := CompareText(TPasItem(PItem1).Name, TPasItem(PItem2).Name);
  // Sort duplicate class names by unit name if available.
  if (Result = 0) and
    (TObject(PItem1).ClassType = TPasCio) and
    (TObject(PItem2).ClassType = TPasCio) then
    if TPasCio(PItem1).MyUnit = nil then begin
      Result := -1
    end else begin
      if TPasCio(PItem2).MyUnit = nil then begin
        Result := 1
      end else begin
        Result := CompareText(TPasCio(PItem1).MyUnit.Name, TPasCio(PItem2).MyUnit.Name);
      end;
    end;
end;

function ComparePasMethods(PItem1, PItem2: Pointer): Integer;
var
  P1: TPasMethod;
  P2: TPasMethod;
begin
  P1 := TPasMethod(PItem1);
  P2 := TPasMethod(PItem2);
  { compare 'method type', order is constructor > destructor > function, procedure }
  if P1.What = P2.What then begin
    { if 'method type' is equal, compare names }
    Result := CompareText(P1.Name, P2.Name)
  end else begin
    if P1.What < P2.What then begin
      Result := -1
    end else begin
      Result := 1;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TPasItem
{ ---------------------------------------------------------------------------- }

destructor TPasItem.Destroy;
begin
  Authors.Free;
  inherited;
end;

procedure TPasItem.DescriptionExtractTag(var ADescription: string; const
  Offs1, Offs2, Offs3: Integer; out s: string);
var
  idx: Integer;
begin
  DescriptionGetTag(ADescription, True, Offs1, Offs2, Offs3, s);
  if Assigned(Abbreviations) then begin
    idx := Abbreviations.IndexOfName(s);
    if idx>=0 then begin
      s := Abbreviations.Values[s];
    end;
  end;
end;

function TPasItem.DescriptionFindTag(const ADescription, TagName: string; var
  Offs1, Offs2, Offs3: Integer): Boolean;
var
  i: Integer;
  j: Integer;
  s: string;
  l: Integer;
  LTagName: string;
begin
  Result := False;
  if ADescription = '' then Exit;
  LTagName := LowerCase(TagName);
  l := Length(ADescription);

  i := Offs1;

  if (ADescription[i] = '@') then begin
    if (i + 1 = l) then Exit;

    Offs1 := i;
    j := i + 1;
    s := DescriptionGetTagName(ADescription, j);

    if LTagName <> LowerCase(s) then Exit;

    Offs2 := j;
    Result := DescriptionFindTagParameters(ADescription, Offs2, Offs3);
  end;
end;

function TPasItem.DescriptionFindTagParameters(const ADescription: string; var
  Offs1, Offs2: Integer): Boolean;
var
  Counter: Integer;
  i: Integer;
  l: Integer;
begin
  Result := False;
  i := Offs1;
  l := Length(ADescription);
  if (i < 1) or (i > l) then Exit;

  while (i <= l) and (ADescription[i] <> '(') do
    Inc(i);
  if i > l then Exit; { no ( found }

  Offs1 := i;
  Inc(i);
  Counter := 1;
  repeat
    case ADescription[i] of
      '(': Inc(Counter);
      ')': Dec(Counter);
    end;
    Inc(i);
  until (i > l) or (Counter = 0);
  if (Counter = 0) then begin
    Offs2 := i - 1;
    Result := True;
  end;
end;

function TPasItem.DescriptionGetTagName(const ADescription: string; var
  Offset: Integer): string;
var
  l: Integer;
begin
  Result := '';
  l := Length(ADescription);
  while (Offset < l) and (ADescription[Offset] in ['A'..'Z', 'a'..'z']) do
    begin
    Result := Result + UpCase(ADescription[Offset]);
    Inc(Offset);
  end;
end;

procedure TPasItem.DescriptionGetTag(var ADescription: string; const Remove:
  Boolean; const Offs1, Offs2, Offs3: Integer; out s: string);
var
  l: Integer;
begin
  l := Offs3 - Offs2 - 1;
  if l < 0 then l := 0;
  s := Copy(ADescription, Offs2 + 1, l);
  if Remove then Delete(ADescription, Offs1, Offs3 - Offs1 + 1);
end;

function TPasItem.FindItem(const ItemName: string): TPasItem;
begin
  Result := nil;
end;

function TPasItem.FindName(S1, S2, S3: string; n: Integer): TPasItem;
var
  p: TPasItem;
  LS1: string;
begin
  Result := nil;
  LS1 := LowerCase(S1);
  case n of
    0: begin
        if Assigned(MyObject) then begin { this item is a method or field }
          p := MyObject.FindFieldMethodProperty(S1);
          if Assigned(p) then begin
            Result := p;
            Exit;
          end;
        end;

        if Assigned(MyUnit) then begin
          p := MyUnit.FindItem(S1);
          if Assigned(p) then begin
            Result := p;
            Exit;
          end;
        end;

        if Assigned(MyUnit) and (LS1 = LowerCase(MyUnit.Name)) then begin
          Result := MyUnit;
          Exit;
        end;

      end;

    1: begin
        if Assigned(MyObject) then begin
          if LowerCase(MyObject.Name) = LS1 then begin
            p := MyObject.FindFieldMethodProperty(S2);
            if Assigned(p) then begin
              Result := p;
              Exit;
            end;
          end;
        end;

        // RJ: To find links in Unit's objects!
        if Assigned(MyUnit) then begin
          p := MyUnit.FindFieldMethodProperty(S1, S2);
          if Assigned(p) then begin
            Result := p;
            Exit;
          end;
        end;
      end;
  end;
end;

function TPasItem.GetDescription: string;
begin
  if Length(DetailedDescription) > 0 then begin
    Result := DetailedDescription
  end else begin
    Result := Description
  end;
end;

{ ---------- }

procedure TPasItem.HandleAbstractTag;
var
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  s: string;
begin
  if DetailedDescription = '' then Exit;
  Offs1 := 1;

  while Offs1 < Length(DetailedDescription) do begin
    if (DetailedDescription[Offs1] = '@') and
      DescriptionFindTag(DetailedDescription, 'ABSTRACT', Offs1, Offs2, Offs3) then
    begin
      DescriptionExtractTag(DetailedDescription, Offs1, Offs2, Offs3, s);
      if (Length(s) <= 0) then Continue;
      Description := s;
      Exit;
    end;
    Inc(Offs1);
  end;
end;

{ ---------- }

procedure TPasItem.HandleAuthorTags;
var
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  s: string;
  l: Integer;
begin
  if DetailedDescription = '' then Exit;
  Offs1 := 1;
  l := Length(DetailedDescription);
  { we could have more than one author, so repeat until we have all }
  while Offs1 < l do begin
    if (DetailedDescription[Offs1] = '@') and
      DescriptionFindTag(DetailedDescription, 'AUTHOR', Offs1, Offs2, Offs3) then
        begin
          { we found one, remove it from the description and add it to the author list }
      DescriptionExtractTag(DetailedDescription, Offs1, Offs2, Offs3, s);
      if s <> '' then begin
        if Authors = nil then FAuthors := NewStringVector;
        Authors.Add(s);
      end;
    end;
    Inc(Offs1);
  end;
end;

procedure TPasItem.HandleCreatedTag;
var
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  l: Integer;
begin
  if DetailedDescription = '' then Exit;
  Offs1 := 1;
  l := Length(DetailedDescription);
  while Offs1 < l do begin
    if (DetailedDescription[Offs1] = '@') and
      DescriptionFindTag(DetailedDescription, 'CREATED', Offs1, Offs2, Offs3) then
        begin
      DescriptionExtractTag(DetailedDescription, Offs1, Offs2, Offs3, FCreated);
      Exit;
    end;
    Inc(Offs1);
  end;
end;

procedure TPasItem.HandleLastModTag;
var
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  l: Integer;
begin
  if DetailedDescription = '' then Exit;
  Offs1 := 1;
  l := Length(DetailedDescription);
  while Offs1 < l do begin
    if (DetailedDescription[Offs1] = '@') and
      DescriptionFindTag(DetailedDescription, 'LASTMOD', Offs1, Offs2, Offs3) then
        begin
      DescriptionExtractTag(DetailedDescription, Offs1, Offs2, Offs3, LastMod);
      Exit;
    end;
    Inc(Offs1);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TPasItem.HasDescription: Boolean;
begin
  HasDescription := (Description <> '') or (DetailedDescription <> '');
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItem.InsertItem(const Item: TPasItem; var c: TPasItems);
begin
  if Item = nil then Exit;
  if c = nil then c := TPasItems.Create(True);
  c.InsertObjectLast(Item);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItem.InsertMethod(const Method: TPasMethod; var c:
  TPasMethods);
begin
  if Method = nil then Exit;
  if c = nil then c := TPasMethods.Create(True);
  c.InsertObjectLast(Method);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItem.InsertProperty(const Prop: TPasProperty; var c:
  TPasProperties);
begin
  if Prop = nil then Exit;
  if c = nil then c := TPasProperties.Create(True);
  c.InsertObjectLast(Prop);
end;

{ ---------------------------------------------------------------------------- }
{ TPasItems
{ ---------------------------------------------------------------------------- }

procedure TPasItems.CopyItems(const c: TPasItems);
var
  i: Integer;
begin
  if IsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    InsertObjectLast(TPasItem(c.GetPasItemAt(i)));
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItems.CountCIO(out c, i, o: Integer);
var
  j: Integer;
begin
  c := 0;
  i := 0;
  o := 0;

  for j := 0 to Count - 1 do
    case TPasCio(GetPasItemAt(j)).MyType of
      CIO_CLASS:
        Inc(c);
      CIO_INTERFACE:
        Inc(i);
      CIO_OBJECT:
        Inc(o);
    end;
end;

{ ---------------------------------------------------------------------------- }

constructor TPasItems.Create(const AOwnsObject: Boolean);
begin
  inherited;
  FHash := TObjectHash.Create;
end;

procedure TPasItems.DeleteAt(const AIndex: Integer);
var
  LObj: TPasItem;
begin
  LObj := GetPasItemAt(AIndex);
  FHash.Delete(LowerCase(LObj.Name));
  inherited DeleteAt(AIndex);
end;

destructor TPasItems.Destroy;
begin
  FHash.Free;
  inherited;
end;

function TPasItems.FindName(const AName: string): TPasItem;
begin
  Result := nil;
  if Length(AName) > 0 then begin
    result := TPasItem(FHash.Items[LowerCase(AName)]);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TPasItems.GetPasItemAt(const AIndex: Integer): TPasItem;
begin
  Result := TPasItem(ObjectAt[AIndex]);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItems.InsertItems(const c: TPasItems);
var
  i: Integer;
begin
  if IsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    InsertObjectLast(TPasItem(c.ObjectAt[i]));
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItems.InsertObjectLast(const AObject: TPasItem);
begin
  inherited InsertObjectLast(AObject);
  FHash.Items[LowerCase(AObject.Name)] := AObject;
end;

procedure TPasItems.RemovePrivateItems;
var
  i: Integer;
  Item: TPasItem;
begin
  i := 0;
  while (i < Count) do begin
    Item := PasItemAt[i];
    if Assigned(Item) and (Item.State = STATE_PRIVATE) then
      DeleteAt(i)
    else
      Inc(i);
  end;
end;

{ TPasCio }

destructor TPasCio.Destroy;
begin
  Ancestors.Free;
  Fields.Free;
  Methods.Free;
  Properties.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

function TPasCio.FindItem(const ItemName: string): TPasItem;
begin
  FindItem := FindFieldMethodProperty(ItemName);
end;

{ ---------------------------------------------------------------------------- }

function TPasCio.FindFieldMethodProperty(const ItemName: string): TPasItem;
begin
  if Fields <> nil then begin
    Result := Fields.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if Methods <> nil then begin
    Result := Methods.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if Properties <> nil then begin
    Result := Properties.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  Result := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasCio.SortPasItems;
begin
  if Fields <> nil then Fields.SortByPasItemName;
  if Methods <> nil then Methods.Sort(ComparePasMethods);
  if Properties <> nil then Properties.SortByPasItemName;
end;

{ ---------------------------------------------------------------------------- }
{ TPasUnit
{ ---------------------------------------------------------------------------- }

destructor TPasUnit.Destroy;
begin
  CIOs.Free;
  Constants.Free;
  FuncsProcs.Free;
  Types.Free;
  UsesUnits.Free;
  Variables.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasUnit.AddCIO(const i: TPasCio);
begin
  if CIOs = nil then CIOs := TPasItems.Create(True);
  CIOs.InsertObjectLast(i);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasUnit.AddConstant(const i: TPasItem);
begin
  if Constants = nil then Constants := TPasItems.Create(True);
  Constants.InsertObjectLast(i);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasUnit.AddType(const i: TPasItem);
begin
  if Types = nil then Types := TPasItems.Create(True);
  Types.InsertObjectLast(i);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasUnit.AddVariable(const i: TPasItem);
begin
  if Variables = nil then Variables := TPasItems.Create(True);
  Variables.InsertObjectLast(i);
end;

{ ---------------------------------------------------------------------------- }

function TPasUnit.FindFieldMethodProperty(const S1, S2: string): TPasItem;
var
  PI: TPasItem;
  po: TPasCio;
begin
  Result := nil;
  if CIOs = nil then Exit;

  po := TPasCio(CIOs.FindName(S1));
  if Assigned(po) then begin
    PI := po.FindFieldMethodProperty(S2);
    if Assigned(PI) then FindFieldMethodProperty := PI;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TPasUnit.FindItem(const ItemName: string): TPasItem;
var
  i: Integer;
  CioItem: TPasCio;
begin
  if LowerCase(Name) = LowerCase(ItemName) then begin
    Result := Self;
    Exit;
  end;

  if Constants <> nil then begin
    Result := Constants.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if Types <> nil then begin
    Result := Types.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if Variables <> nil then begin
    Result := Variables.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if FuncsProcs <> nil then begin
    Result := FuncsProcs.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if CIOs <> nil then begin
    Result := CIOs.FindName(ItemName);
    if Result <> nil then Exit;
  end;

  if CIOs <> nil then
    for i := 0 to CIOs.Count - 1 do begin
      CioItem := TPasCio(CIOs.PasItemAt[i]);
      if CioItem <> nil then begin
        Result := CioItem.FindFieldMethodProperty(ItemName);
        if Result <> nil then Exit;
      end;
    end;

  Result := nil;
end;

{ ---------------------------------------------------------------------------- }
{ TPasUnits
{ ---------------------------------------------------------------------------- }

function TPasUnits.ExistsUnit(const AUnit: TPasUnit): Boolean;
begin
  Result := FindName(AUnit.Name) <> nil;
end;

{ ---------------------------------------------------------------------------- }

function TPasUnits.GetUnitAt(const AIndex: Integer): TPasUnit;
begin
  Result := TPasUnit(ObjectAt[AIndex]);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasUnits.SetUnitAt(const AIndex: Integer; const Value: TPasUnit);
begin
  ObjectAt[AIndex] := Value;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasItems.SetPasItemAt(const AIndex: Integer; const Value:
  TPasItem);
begin
  ObjectAt[AIndex] := Value;
end;

procedure TPasItems.SortByPasItemName;
begin
  Sort(ComparePasItemsByName);
end;

function TPasItem.QualifiedName: String;
begin
  Result := '';
  if MyUnit <> nil then begin
    Result := Result + MyUnit.Name + '.';
  end;
  if MyObject <> nil then begin
    Result := Result + MyObject.Name + '.';
  end;
  Result := Result + Name;
end;

procedure TPasUnit.SortPasItems;
var
  i: Integer;
begin
  if CIOs <> nil then
    begin
      CIOs.SortByPasItemName;
      { Also sort Fields / Methods / Properties of each CIO. }
      for i := 0 to CIOs.Count - 1 do
        TPasCio(CIOs.PasItemAt[i]).SortPasItems;
    end;
  if Constants <> nil then Constants.SortByPasItemName;
  if FuncsProcs <> nil then FuncsProcs.SortByPasItemName;
  if Types <> nil then Types.SortByPasItemName;
  if Variables <> nil then Variables.SortByPasItemName;
end;

end.
