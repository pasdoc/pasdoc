unit Hashes;

{** Hash Library

    Slightly modified - will not own the objects any more in the object hash.
    Johannes Berg <johannes@sipsolutions.de>

    ---

    Original Author:     Ciaran McCreesh <keesh@users.sf.net>
    Copyright:           Copyright (c) 2002 Ciaran McCreesh
    Date:                20020621
    Purpose:             A collection of hash components for Delphi. These are
                         similar to arrays, but the index is a string. A hashing
                         algorithm is used to provide extremely fast searching.

    Generic Moan:        This would be a lot easier if Delphi had template
                         classes. If anyone at Borland / Inprise / whatever
                         you're calling yourselves this week reads this, let me
                         know how much I have to bribe you.

    Changelog:
      v2.6 (20020621)
        * Framework for dynamic bucket sizes. No actual resizing yet.
        * Changed TStringHash, TIntegerHash and TObjectHash slightly, and fixed
          potential bugs in them.
        * General performance improvements
        * Changed how iterators work. In particular, multiple iterators are now
          possible. Thanks to Daniel Trinter for code and Emanuel for
          suggestions.
        + Previous method (goes with Next)
        + AllowCompact property

      v2.5 (20020606)
        * Empty hash keys explicitly forbidden. Thanks to Marco Vink for the
          notice.
        + Clear method

      v2.4 (20020603)
        * Fixed Compact bug. Thanks to Daniel Trinter for the notice. Basically
          I was assuming something about the size of one of the internal arrays
          which wasn't always true.

      v2.3 (20020601)
        + ItemCount property
        + Compact method
        * Hash auto-compacts itself if overly inefficient
        * ItemIndexes are now recycled

      v2.2 (20020529)
        * Fixed iterator bug. Not all items were called under some
          circumstances. Thanks to Tom Walker for the notice.

      v2.1 (20020528, internal release only)
        + TObjectHash

      v2.0 (20020526)
        * Complete rewrite
        + THash
        + TStringHash
        + TIntegerHash

    License:

      This library is Copyright (c) 2002 Ciaran McCreesh.

      Permission is granted to anyone to use this software for any purpose on
      any computer system, and to redistribute it freely, subject to the
      following restrictions:

      1. This software is distributed in the hope that it will be useful,
         but WITHOUT ANY WARRANTY; without even the implied warranty of
         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

      2. The origin of this software must not be misrepresented.

      3. Altered versions must be plainly marked as such, and must not be
         misrepresented as being the original software.

    Documentation:
      Please see:
        * http://www.opensourcepan.co.uk/libraries/hashes/
        * http://www.undu.com/articles/020604.html

    Other notes:
      This unit provides three hashes, TIntegerHash, TStringHash and
      TObjectHash. If you want a more precise kind (eg TComponentHash), it's
      easiest to descend from THash and copy the TObjectHash code. Note that
      TObjectHash is slightly different from TIntegerHash and TStringHash
      because it has to free items -- it cannot just overwrite them.

    Internal data representation:
      Each hash object has an array (potentially dynamically sized, but this
      isn't used yet) of 'buckets' (dynamic arrays). Each bucket is mapped
      to a series of hash values (we take the high order bits of the value
      calculated), so that every possible hash value refers to exactly one
      bucket. This reduces the amount of searching that has to be done to
      find an item, so it's much faster than linear or B-Tree storage.

      Each bucket contains a series of integers. These are indexes into an
      items array, which for type reasons is maintained by the descendant
      classes. These are recycled when the hash detects that it is becoming
      inefficient.
}

interface

uses SysUtils;

const
    {** This constant controls the initial size of the hash. }
  c_HashInitialItemShift = 7;

    {** How inefficient do we have to be before we automatically Compact? }
  c_HashCompactR = 2; { This many spaces per item. }
  c_HashCompactM = 100; { Never for less than this number of spaces. }

type
    {** General exception classes. }
  EHashError = class(Exception);
  EHashErrorClass = class of EHashError;

    {** Exception for when an item is not found. }
  EHashFindError = class(EHashError);

    {** Exception for invalid Next op. }
  EHashIterateError = class(EHashError);

    {** Exception for invalid keys. }
  EHashInvalidKeyError = class(EHashError);

    {** Record, should really be private but OP won't let us... }
  THashRecord = record
    Hash: Cardinal;
    ItemIndex: integer;
    Key: string;
  end;

    {** Iterator Record. This should also be private. This makes me almost like
        the way Java does things. Almost. Maybe. }
  THashIterator = record
    ck, cx: integer;
  end;

    {** Base Hash class. Don't use this directly. }
  THash = class
  protected
        {** The keys. }
    f_Keys: array of array of THashRecord;

        {** Current bucket shift. }
    f_CurrentItemShift: integer;

        {** These are calculated from f_CurrentItemShift. }
    f_CurrentItemCount: integer;
    f_CurrentItemMask: integer;
    f_CurrentItemMaxIdx: integer;

        {** Spare items. }
    f_SpareItems: array of integer;

        {** Whether Next is allowed. }
    f_NextAllowed: boolean;

        {** Current key. }
    f_CurrentKey: string;

        {** Can we compact? }
    f_AllowCompact: boolean;

        {** Our current iterator. }
    f_CurrentIterator: THashIterator;

        {** Update the masks. }
    procedure FUpdateMasks;

        {** Update the buckets. }
    procedure FUpdateBuckets;

        {** Find a key's location. }
    function FFindKey(const Key: string; var k, x: integer): boolean;

        {** Add a new key, or change an existing one. Don't call this directly. }
    procedure FSetOrAddKey(const Key: string; ItemIndex: integer);

        {** Abstract method, delete value with a given index. Override this. }
    procedure FDeleteIndex(i: integer); virtual; abstract;

        {** Get the number of items. }
    function FGetItemCount: integer;

        {** Allocate an item index. }
    function FAllocItemIndex: integer;

        {** Abstract method, move an item with index OldIndex to NewIndex.
            Override this. }
    procedure FMoveIndex(oldIndex, newIndex: integer); virtual; abstract;

        {** Abstract method, trim the indexes down to count items. Override
            this. }
    procedure FTrimIndexes(count: integer); virtual; abstract;

        {** Abstract method, clear all items. Override this. }
    procedure FClearItems; virtual; abstract;

        {** Tell us where to start our compact count from. Override this. }
    function FIndexMax: integer; virtual; abstract;

        {** Compact, but only if we're inefficient. }
    procedure FAutoCompact;

  public
        {** Our own constructor. }
    constructor Create; reintroduce; virtual;

        {** Does a key exist? }
    function Exists(const Key: string): boolean;

        {** Rename a key. }
    procedure Rename(const Key, NewName: string);

        {** Delete a key. }
    procedure Delete(const Key: string);

        {** Reset iterator. }
    procedure Restart;

        {** Next key. }
    function Next: boolean;

        {** Previous key. }
    function Previous: boolean;

        {** Current key. }
    function CurrentKey: string;

        {** The number of items. }
    property ItemCount: integer read FGetItemCount;

        {** Compact the hash. }
    procedure Compact;

        {** Clear the hash. }
    procedure Clear;

        {** Allow compacting? }
    property AllowCompact: boolean read f_AllowCompact write f_AllowCompact;

        {** Current iterator. }
    property CurrentIterator: THashIterator read f_CurrentIterator write
      f_CurrentIterator;

        {** Create a new iterator. }
    function NewIterator: THashIterator;

  end;

    {** Hash of strings. }
  TStringHash = class(THash)
  protected
        {** The index items. }
    f_Items: array of string;

        {** Override FDeleteIndex abstract method. }
    procedure FDeleteIndex(i: integer); override;

        {** Get an item or raise an exception. }
    function FGetItem(const Key: string): string;

        {** Set or add an item. }
    procedure FSetItem(const Key, Value: string);

        {** Move an index. }
    procedure FMoveIndex(oldIndex, newIndex: integer); override;

        {** Trim. }
    procedure FTrimIndexes(count: integer); override;

        {** Clear all items. }
    procedure FClearItems; override;

        {** Where to start our compact count from. }
    function FIndexMax: integer; override;

  public
        {** Items property. }
    property Items[const Key: string]: string read FGetItem
    write FSetItem; default;
  end;

    {** Hash of integers. }
  TIntegerHash = class(THash)
  protected
        {** The index items. }
    f_Items: array of integer;

        {** Override FDeleteIndex abstract method. }
    procedure FDeleteIndex(i: integer); override;

        {** Get an item or raise an exception. }
    function FGetItem(const Key: string): integer;

        {** Set or add an item. }
    procedure FSetItem(const Key: string; Value: integer);

        {** Move an index. }
    procedure FMoveIndex(oldIndex, newIndex: integer); override;

        {** Trim. }
    procedure FTrimIndexes(count: integer); override;

        {** Clear all items. }
    procedure FClearItems; override;

        {** Where to start our compact count from. }
    function FIndexMax: integer; override;

  public
        {** Items property. }
    property Items[const Key: string]: integer read FGetItem
    write FSetItem; default;
  end;

    {** Hash of objects. }
  TObjectHash = class(THash)
  protected
        {** The index items. }
    f_Items: array of TObject;

        {** Override FDeleteIndex abstract method. }
    procedure FDeleteIndex(i: integer); override;

        {** Get an item or raise an exception. }
    function FGetItem(const Key: string): TObject;

        {** Set or add an item. }
    procedure FSetItem(const Key: string; Value: TObject);

        {** Move an index. }
    procedure FMoveIndex(oldIndex, newIndex: integer); override;

        {** Trim. }
    procedure FTrimIndexes(count: integer); override;

        {** Clear all items. }
    procedure FClearItems; override;

        {** Where to start our compact count from. }
    function FIndexMax: integer; override;

  public
        {** Items property. }
    property Items[const Key: string]: TObject read FGetItem
    write FSetItem; default;

        {** Destructor must destroy all items. }
    destructor Destroy; override;

  end;

implementation

  {** A basic hash function. This is pretty fast, and fairly good general
      purpose, but you may want to swap in a specialised version. }

function HashThis(const s: string): cardinal;
var
  h, g, i: cardinal;
begin
  if (s = '') then
    raise EHashInvalidKeyError.Create('Key cannot be an empty string');
  h := $12345670;
  for i := 1 to Length(s) do begin
    h := (h shl 4) + ord(s[i]);
    g := h and $F0000000;
    if (g > 0) then
      h := h or (g shr 24) or g;
  end;
  result := h;
end;

{ THash }

constructor THash.Create;
begin
  inherited Create;
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_CurrentItemShift := c_HashInitialItemShift;
  self.FUpdateMasks;
  self.FUpdateBuckets;
  self.f_AllowCompact := true;
end;

procedure THash.Delete(const Key: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Delete the Index entry. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    { Add the index to the Spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);

  self.FAutoCompact;
end;

function THash.Exists(const Key: string): boolean;
var
  dummy1, dummy2: integer;
begin
  result := FFindKey(Key, dummy1, dummy2);
end;

procedure THash.FSetOrAddKey(const Key: string; ItemIndex: integer);
var
  k, x, i: integer;
begin
  { Exists already? }
  if (self.FFindKey(Key, k, x)) then begin
    { Yep. Delete the old stuff and set the new value. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    self.f_Keys[k][x].ItemIndex := ItemIndex;
    { Add the index to the spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
  end else begin
    { No, create a new one. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := ItemIndex;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
  end;
end;

function THash.FFindKey(const Key: string; var k, x: integer): boolean;
var
  i: integer;
  h: cardinal;
begin
  { Which bucket? }
  h := HashThis(Key);
  k := h and f_CurrentItemMask;
  result := false;
  { Look for it. }
  for i := 0 to High(self.f_Keys[k]) do
    if (self.f_Keys[k][i].Hash = h) or true then
      if (self.f_Keys[k][i].Key = Key) then begin
        { Found it! }
        result := true;
        x := i;
        break;
      end;
end;

procedure THash.Rename(const Key, NewName: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Remember the ItemIndex. }
    i := self.f_Keys[k][x].ItemIndex;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
    { Create the new item. }
    self.FSetOrAddKey(NewName, i);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);

  self.FAutoCompact;
end;

function THash.CurrentKey: string;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot find CurrentKey as the hash has '
      + 'been modified since Restart was called')
  else if (self.f_CurrentKey = '') then
    raise
      EHashIterateError.Create('Cannot find CurrentKey as Next has not yet '
      + 'been called after Restart')
  else
    result := self.f_CurrentKey;
end;

function THash.Next: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck = -1) then begin
    self.f_CurrentIterator.ck := 0;
    self.f_CurrentIterator.cx := 0;
  end;
  while ((not result) and (self.f_CurrentIterator.ck <= f_CurrentItemMaxIdx))
    do begin
    if (self.f_CurrentIterator.cx <
      Length(self.f_Keys[self.f_CurrentIterator.ck])) then begin
      result := true;
      self.f_CurrentKey :=
        self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      inc(self.f_CurrentIterator.cx);
    end else begin
      inc(self.f_CurrentIterator.ck);
      self.f_CurrentIterator.cx := 0;
    end;
  end;
end;

procedure THash.Restart;
begin
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_NextAllowed := true;
end;

function THash.FGetItemCount: integer;
var
  i: integer;
begin
  { Calculate our item count. }
  result := 0;
  for i := 0 to f_CurrentItemMaxIdx do
    inc(result, Length(self.f_Keys[i]));
end;

function THash.FAllocItemIndex: integer;
begin
  if (Length(self.f_SpareItems) > 0) then begin
    { Use the top SpareItem. }
    result := self.f_SpareItems[High(self.f_SpareItems)];
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) - 1);
  end else begin
    result := self.FIndexMax + 1;
  end;
end;

procedure THash.Compact;
var
  aSpaces: array of boolean;
  aMapping: array of integer;
  i, j: integer;
begin
  { Find out where the gaps are. We could do this by sorting, but that's at
    least O(n log n), and sometimes O(n^2), so we'll go for the O(n) method,
    even though it involves multiple passes. Note that this is a lot faster
    than it looks. Disabling this saves about 3% in my benchmarks, but uses a
    lot more memory. }
  if (self.AllowCompact) then begin
    SetLength(aSpaces, self.FIndexMax + 1);
    SetLength(aMapping, self.FIndexMax + 1);
    for i := 0 to High(aSpaces) do
      aSpaces[i] := false;
    for i := 0 to High(aMapping) do
      aMapping[i] := i;
    for i := 0 to High(self.f_SpareItems) do
      aSpaces[self.f_SpareItems[i]] := true;

    { Starting at the low indexes, fill empty ones from the high indexes. }
    i := 0;
    j := self.FIndexMax;
    while (i < j) do begin
      if (aSpaces[i]) then begin
        while ((i < j) and (aSpaces[j])) do
          dec(j);
        if (i < j) then begin
          aSpaces[i] := false;
          aSpaces[j] := true;
          self.FMoveIndex(j, i);
          aMapping[j] := i
        end;
      end else
        inc(i);
    end;

    j := self.FIndexMax;
    while (aSpaces[j]) do
      dec(j);

    { Trim the items array down to size. }
    self.FTrimIndexes(j + 1);

    { Clear the spaces. }
    SetLength(self.f_SpareItems, 0);

    { Update our buckets. }
    for i := 0 to f_CurrentItemMaxIdx do
      for j := 0 to High(self.f_Keys[i]) do
        self.f_Keys[i][j].ItemIndex := aMapping[self.f_Keys[i][j].ItemIndex];
  end;
end;

procedure THash.FAutoCompact;
begin
  if (self.AllowCompact) then
    if (Length(self.f_SpareItems) >= c_HashCompactM) then
      if (self.FIndexMax * c_HashCompactR > Length(self.f_SpareItems)) then
        self.Compact;
end;

procedure THash.Clear;
var
  i: integer;
begin
  self.FClearItems;
  SetLength(self.f_SpareItems, 0);
  for i := 0 to f_CurrentItemMaxIdx do
    SetLength(self.f_Keys[i], 0);
end;

procedure THash.FUpdateMasks;
begin
  f_CurrentItemMask := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemMaxIdx := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemCount := (1 shl f_CurrentItemShift);
end;

procedure THash.FUpdateBuckets;
begin
  { This is just a temporary thing. }
  SetLength(self.f_Keys, self.f_CurrentItemCount);
end;

function THash.NewIterator: THashIterator;
begin
  result.ck := -1;
  result.cx := 0;
end;

function THash.Previous: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck >= 0) then begin
    while ((not result) and (self.f_CurrentIterator.ck >= 0)) do begin
      dec(self.f_CurrentIterator.cx);
      if (self.f_CurrentIterator.cx >= 0) then begin
        result := true;
        self.f_CurrentKey :=
          self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      end else begin
        dec(self.f_CurrentIterator.ck);
        if (self.f_CurrentIterator.ck >= 0) then
          self.f_CurrentIterator.cx :=
            Length(self.f_Keys[self.f_CurrentIterator.ck]);
      end;
    end;
  end;
end;

{ TStringHash }

procedure TStringHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i] := '';
end;

function TStringHash.FGetItem(const Key: string): string;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TStringHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TStringHash.FSetItem(const Key, Value: string);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value
  else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TStringHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TStringHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TStringHash.FClearItems;
begin
  SetLength(self.f_Items, 0);
end;

{ TIntegerHash }

procedure TIntegerHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i] := 0;
end;

function TIntegerHash.FGetItem(const Key: string): integer;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TIntegerHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TIntegerHash.FSetItem(const Key: string; Value: integer);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value
  else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TIntegerHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TIntegerHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TIntegerHash.FClearItems;
begin
  SetLength(self.f_Items, 0);
end;

{ TObjectHash }

procedure TObjectHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i] := nil;
end;

function TObjectHash.FGetItem(const Key: string): TObject;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    Result := nil;
end;

procedure TObjectHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TObjectHash.FSetItem(const Key: string; Value: TObject);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then begin
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value;
  end else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TObjectHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TObjectHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TObjectHash.FClearItems;
begin
  SetLength(self.f_Items, 0);
end;

destructor TObjectHash.Destroy;
begin
  inherited;
end;

end.
