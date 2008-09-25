(*
  This unit implements an associative array.
  Before writing this unit, I've always missed Perl commands
  like @code($h{abc}='def') in Pascal.

  @author(Copyright (C) 2001  Wolf Behrenhoff <wolf@behrenhoff.de>)

  Version 0.9.1 (works fine, don't know a bug, but 1.0? No,
                 error checks are missing!)

  @italic(
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.)
  
  @italic(
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.)

  @italic(
  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA)

  Thanks to:
  @unorderedList(
    @item(Larry Wall for perl! And because I found a way how to implement
      a hash in perl's source code (hv.c and hv.h). This is not a direct
      translation from C to Pascal, but the algortithms are more or less
      the same.))

  Be warned:
  @unorderedList(
    @item(There is NOT a single ERROR CHECK in this unit. So expect anything!
      Especially there are NO checks on NEW and GETMEM functions ---
      this might be dangerous on machines with low memory.))

  Programmer's information:
  @unorderedList(
    @item(you need Freepascal (http://www.freepascal.org) or 
      Delphi (http://www.borland.com)
      to compile this unit)
    @item(I recommend that you use Ansistrings {$H+} to be able to use keys
      longer than 255 chars))

  How to use this unit:

@preformatted(
  Simply put this unit in your uses line. You can use a new class - THash.

  Initialize a hash (assuming "var h: THash;"):
  h:=THash.Create;

  Save a String:
  h.SetString('key','value');          //perl: $h{key}='value'

  Get the String back:
  string_var:=h.GetString('key');      //perl: $string_var=$h{key}
  returns '' if 'key' is not set

  Test if a key has been set:
  if h.KeyExists('key') then...        //perl: if (exists $h{key}) ...
  returns a boolean

  Delete a key
  h.DeleteKey('key');                  //perl: delete $h{key};

  Which keys do exist?
  stringlist:=h.Keys;                  //perl: @list=keys %h;
  returns a TStringList

  Which keys do exist beginning with a special string?
  stinglist:=h.Keys('abc');
  returns all keys beginning with 'abc'  //perl: @list=grep /^abc/, keys %h;

  How many keys are there?
  number_of_keys:=h.Count;             //perl: $number=scalar keys %hash;

  How many keys fit in memory allocated by THash?
  c:=h.Capacity; (property)
  THash automatically increases h.Capacity if needed.
  This property is similar to Delphi's TList.Capacity property.
  Note #1: You can't decrease h.Capacity.
  Note #2: Capacity must be 2**n -- Create sets Capacity:=8;
           The same: Capacity:=17; , Capacity:=32;

  I know there will be 4097 key/values in my hash. I don't want
  the hash's capacity to be 8192 (wasting 50% ram). What to do?
  h.MaxCapacity:=4096; => Capacity will never be > 4096.
  Note: You can store more than MaxCapacity key/values in the
        hash (as many as you want) but Count should be >= Capacity
        for best performance.
  MaxCapacity is -1 by default, meaning no limit.

  Delete the hash
  h.Free;    OR
  h.Destroy;

  Instead of just strings you can also save objects in my hash -
  anything that is a pointer can be saved. Similar to SetString
  and GetString  there are SetObject  and GetObject. The latter
  returns nil if the key is unknown.
  You can use both Set/GetString and Set/GetObject for a single
  key string - no problem. But if DeleteKey is called, both the
  string and the pointer are lost.
  If you want to store a pointer  and a string, it is faster to
  call  SetStringObject(key,string,pointer)  than SetString and
  SetObject. The same is true getting the data back - GetString
  and GetObject are  significantly slower  then a singe call to
  GetStringObject(key, var string, var pointer).

  Happy programming!
)
*)

unit PasDoc_Hashes;

{$Q-} // no integer overflow checks (I need overflow in THash.Hash)
{$R-} // no range checks (because free bounds of TFakeArray[0..0])

{$DEFINE solid} //use array of record

interface

uses
  SysUtils, Classes;

type
  { }
  PPHashEntry=^PHashEntry;
  PHashEntry=^THashEntry;
  THashEntry=record //object?
  {$IFDEF solid}
    first, next: integer;
  {$ELSE}
    next: PHashEntry;
  {$ENDIF}
    hash: Integer;
    key: String;
    value: String;
    data: Pointer;
  end;

  { in FPC, I can simply use PPHashEntry as an array of PHashEntry -
    Delphi doesn't allow that. I need this stupid array[0..0] definition!
    From Delphi4, I could use a dynamic array. }
{$IFDEF solid}
  TFakeArray = array of THashEntry;
  PFakeArray = TFakeArray;
{$ELSE}
  TFakeArray=array[0..0] of PHashEntry;
  PFakeArray=^TFakeArray;
{$ENDIF}

{$IFDEF new}
  THash = class(TStrings)
{$ELSE}
  THash = class
{$ENDIF}
  private
    Feld: PFakeArray;
    FeldMaxValue: Integer;
    FeldBelegt: Integer;
    FMaxCapacity: Integer;
    function  Hash(key: String): Integer;
  {$IFDEF unused}
    function  Entry(_key: string): PHashEntry;
  {$ELSE}
  {$ENDIF}
    function  GetItem(index: integer): PHashEntry;
    procedure MakeHashLarger(power2: Integer);
    procedure SetCapacity(new_size: Integer);
    function  GetCapacity: Integer;
    procedure SetMaxCapacity(newmc: Integer);
  {$IFDEF solid}
    procedure _SetValueData(const _key, s: String; set_s: Boolean;
                                             p: Pointer; set_p: Boolean);
  {$ELSE}
    procedure _SetStringObject(const _key, s: String; set_s: Boolean;
                                             p: Pointer; set_p: Boolean);
  {$ENDIF}

{$IFDEF new}
  protected //override abstracts
    function  GetCount: integer; override;
    function  Get(index: integer): string; override;
  public
    procedure Delete(Index: Integer); override;
    procedure Insert(index: integer; s: string); override;
{$ELSE}
{$ENDIF}

  public
    property Count: Integer read FeldBelegt;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read FMaxCapacity write SetMaxCapacity;
    constructor Create; virtual;
  {$IFDEF solid}
    function  GetValue(const _key: String): String;
    procedure SetValue(const _key, data: String);
    function  GetData(const _key: String): Pointer;
    procedure SetData(const _key: String; data: Pointer);
    function  GetValueData(const _key: String; var s: String; var p: Pointer): integer;
    procedure SetValueData(const _key, s: String; p: Pointer);
    property Values[const _key: string]: string read GetValue write SetValue;
    property Data[const _key: string]: pointer read GetData write SetData;
    property Items[index: integer]: PHashEntry read GetItem;
  {$ELSE}
    destructor Destroy; override;
    function  GetString(const _key: String): String;
    procedure SetString(const _key, data: String);
    function  GetObject(const _key: String): Pointer;
    procedure SetObject(const _key: String; data: Pointer);
    function  GetStringObject(const _key: String; var s: String; var p: Pointer): boolean;
    procedure SetStringObject(const _key, s: String; p: Pointer);
  {$ENDIF}
    function  KeyExists(const _key: String): Boolean;
  {$IFDEF solid}
  //return index of _key, or -1 if not found
    function  IndexOf(const _key: string): integer;
    procedure DeleteKey(const _key: String);
    //If Keys() is needed, make TStrings the base class!
  {$ELSE}
    procedure DeleteKey(const _key: String);
    function  Keys: TStringList; overload;
    function  Keys(const beginning: String): TStringList; overload;
  {$ENDIF}
  end;

  TObjectHash = class(THash)
  public
  {$IFDEF solid}
    function  GetObject(const _key: String): TObject;
    procedure SetObject(const _key: String; _data: TObject);
    property Objects[const _key: string]: TObject read GetObject write SetObject;
  {$ELSE}
    procedure Delete(const _key: String);
    property Items[const _key: string]: Pointer read GetObject write SetObject;
  {$ENDIF}
  end;

implementation

constructor THash.Create;
var
  i: integer;
begin
  FeldMaxValue:=7; //irgendein kleiner Wert der Eigenschaft 2**n-1 (English: any small value of the property 2**n-1)
  FeldBelegt:=0;
  FMaxCapacity:=-1;
{$IFDEF solid}
  SetLength(Feld, succ(FeldMaxValue));
  for i := 0 to FeldMaxValue do begin
    Feld[i].first := -1;
  end;
{$ELSE}
  GetMem(Feld,sizeof(PHashEntry)*Succ(FeldMaxValue));
  FillChar(Feld^,sizeof(PHashEntry)*Succ(FeldMaxValue),0);
{$ENDIF}
end;

{$IFDEF solid}
{$ELSE}
destructor THash.Destroy;
var
  i: Integer;
  del: PHashEntry;
begin
  for i:=0 to FeldMaxValue do begin
    while Assigned(Feld^[i]) do begin
      del:=Feld^[i];
      Feld^[i]:=Feld^[i]^.next;
      dispose(del);
    end;
  end;
  Feld := nil;
  inherited Destroy;
end;
{$ENDIF}

function THash.Hash(key: String): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to length(key) do
    Result:=33*Result+Ord(key[i]);
  Result:=Result+Result shr 5;
end;

{$IFDEF solid}

{$IFDEF unused}
function THash.Entry(_key: string): PHashEntry;
var
  i: Integer;
begin
  i := IndexOf(_key);
  if i < 0 then
    Result := nil
  else
    Result := @Feld[i];
end;
{$ELSE}
{$ENDIF}

function THash.GetItem(index: integer): PHashEntry;
begin
  Result := @Feld[index];
end;

function THash.IndexOf(const _key: string): integer;
var
  h: Integer;
begin
  h := Hash(_key);
  Result := Feld[h and FeldMaxValue].first; //starting index
  while Result >= 0 do begin
    if (Feld[Result].key = _key) then
      exit;
    Result := Feld[Result].next;
  end;
//not found
  //Result := -1;
end;

function THash.KeyExists(const _key: String): Boolean;
begin
  Result := IndexOf(_key) >= 0;
end;
{$ELSE}
function THash.KeyExists(const _key: String): Boolean;
var
  h: Integer;
  he: PHashEntry;
begin
  h:=Hash(_key);
  he:=Feld^[h and FeldMaxValue];
  while Assigned(he) do begin
    if he^.Hash=h then
      if he^.key=_key then begin //gefunden (English: found)
        Result:=True;
        Exit;
      end;
    he:=he^.next;
  end;
  Result:=False;
end;
{$ENDIF}

function THash.GetCapacity: Integer;
begin
  Result:=FeldMaxValue+1;
end;

procedure THash.SetCapacity(new_size: Integer);
var
  power: Integer;
begin
  if new_size>capacity then begin
    power:=1+Trunc(Ln(new_size-1)/Ln(2)-Ln(capacity)/Ln(2)+0.00001);
    { power:=Ceil(Ln(new_size)/Ln(2)-Ln(capacity)/Ln(2)-0.00001);
      but Ceiling needs unit Math... }
    if power>0 then MakeHashLarger(power);
  end;
end;

procedure THash.MakeHashLarger(power2: Integer);
var
  i, oldsize, newsize: Integer;
{$IFDEF solid}
  h, inext: integer;
{$ELSE}
  newpos: Integer;
  he: PHashEntry;
  oe: PPHashEntry;
{$ENDIF}
begin
  //zunächst Speicher reservieren (English: reserve additional memory)
  oldsize:=Succ(FeldMaxValue);
  newsize:=oldsize shl power2;
  if (newsize>MaxCapacity) and (MaxCapacity<>-1) then Exit;
  if newsize <= MaxCapacity then
    exit; //don't shrink?
  FeldMaxValue:=Pred(newsize);
{$IFDEF solid}
  SetLength(Feld, newsize);
//clear indices
  for i := 0 to FeldMaxValue do begin
    Feld[i].first := -1; //full range
    Feld[i].next := -1; //to Count would be sufficient
  end;
//link entries
  for i := 0 to oldsize-1 do begin
    h := Feld[i].hash and FeldMaxValue;
    iNext := Feld[h].first; //in case an entry was already assigned
    Feld[h].first := i;
    Feld[i].next := iNext;
  end;
{$ELSE}
  {$ifdef FPC}
    Feld:=ReAllocMem(Feld,newsize*sizeof(PHashEntry));
  {$else}
    ReAllocMem(Feld,newsize*sizeof(PHashEntry));
  {$endif}

  //neu dazureservierten Speicher leeren (English: initialize the newly allocated memory)
  FillChar(Feld^[oldsize],(newsize-oldsize)*sizeof(PHashEntry),0);

  //dann Daten neu einordnen (English: then rearrange the data)
  for i:=0 to oldsize-1 do begin
    oe:=@Feld^[i];
    he:=oe^;
    while Assigned(he) do begin
      newpos:=he^.Hash and FeldMaxValue;
      if (i<>newpos) then begin
        oe^:=he^.next;
        he^.next:=Feld^[newpos];
        Feld^[newpos]:=he;
      end
        else oe:=@he^.next;
      he:=oe^;
    end;
  end;
{$ENDIF}
end;

procedure THash.SetMaxCapacity(newmc: Integer);
begin
  if newmc<0 then FMaxCapacity:=-1
  else if newmc<Capacity then FMaxCapacity:=Capacity
  else FMaxCapacity:=newmc;
end;

{$IFDEF solid}
procedure THash._SetValueData(const _key, s: String; set_s: Boolean;
                                        p: Pointer; set_p: Boolean);
var
  h, i: Integer;
  pFirst: ^integer;
begin
  h:=Hash(_key);
  pFirst := addr(Feld[h and FeldMaxValue].first);
  i := pFirst^;  // Feld[h and FeldMaxValue].first;
  while (i >= 0) and (Feld[i].key <> _key) do
    i := Feld[i].next;
  if i < 0 then begin
  //new entry
    if FeldBelegt > FeldMaxValue then begin
      MakeHashLarger(1); //invalidates the index
      pFirst := addr(Feld[h and FeldMaxValue].first);
    end;
    i := FeldBelegt;
    inc(FeldBelegt);
  //init internal fields
    Feld[i].hash := h;
    Feld[i].next := pFirst^;  // Feld[h and FeldMaxValue].first;
    Feld[i].key := _key;
    pFirst^ := i; //Feld[h and FeldMaxValue].first := i;
  end;
//put values
  if set_s then Feld[i].value := s;
  if set_p then Feld[i].data := p;
end;

procedure THash.SetData(const _key: String; data: Pointer);
begin
  _SetValueData(_key, '', false, data, true);
end;

procedure THash.SetValue(const _key, data: String);
begin
  _SetValueData(_key, data, true, nil, false);
end;

procedure THash.SetValueData(const _key, s: String; p: Pointer);
begin
  _SetValueData(_key, s, true, p, true);
end;

procedure THash.DeleteKey(const _key: String);
var
  h, i: Integer;
  pFirst: ^integer;
begin
  h := Hash(_key);
  pFirst := addr(Feld[h and FeldMaxValue].first);
  i := pFirst^;
  if i < 0 then
    exit; //no such entry
  if Feld[i].key = _key then
    pFirst^ := Feld[i].next
  else begin
    repeat
      pFirst := addr(Feld[i].next);
      i := pFirst^;
      if i < 0 then
        exit; //no such entry
    until Feld[i].key = _key;
  end;
//reset data fields
  Feld[i].key := '';
  Feld[i].value := '';
  Feld[i].data := nil;
end;

{$ELSE}

procedure THash._SetStringObject(const _key, s: String; set_s: Boolean;
                                        p: Pointer; set_p: Boolean);
var
  h: Integer;
  he, anker: PHashEntry;
begin
  h:=Hash(_key);
  if FeldBelegt>FeldMaxValue then MakeHashLarger(1);
  he:=Feld^[h and FeldMaxValue];
  anker:=he;
  while Assigned(he) do begin
    if he^.hash=h then
      if he^.key=_key then begin
        if set_s then he^.value:=s;
        if set_p then he^.data:=p;
        Exit;
      end;
    he:=he^.next;
  end;

  //nichts gefunden, key muß neu angelegt werden (English: not found; a new key must be created.)
  New(he); Inc(FeldBelegt);
  with he^ do begin
    next:=anker;
    hash:=h;
    key:=_key;
    if set_s then value:=s else value:='';
    if set_p then data:=p else data:=nil;
  end;
  Feld^[h and FeldMaxValue]:=he;
end;

procedure THash.SetObject(const _key: String; data: Pointer);
begin
  _SetStringObject(_key, '', false, data, true);
end;

procedure THash.SetString(const _key, data: String);
begin
  _SetStringObject(_key, data, true, nil, false);
end;

procedure THash.SetStringObject(const _key, s: String; p: Pointer);
begin
  _SetStringObject(_key, s, true, p, true);
end;

procedure THash.DeleteKey(const _key: String);
var
  he: PHashEntry;
  oe: PPHashEntry;
  h, i: Integer;
  pFirst: ^integer;
begin
  h := Hash(_key);
  oe:=@Feld^[h and FeldMaxValue];
  he:=oe^;
  while Assigned(he) do begin
    if he^.hash=h then
      if he^.key=_key then begin
        oe^:=he^.next;
        Dec(FeldBelegt);
        Dispose(he);
        Exit;
      end;
    oe:=@he^.next;
    he:=oe^;
  end;
end;
{$ENDIF}

{$IFDEF solid}
function THash.GetValueData(const _key: String; var s: String; var p: Pointer): integer;
begin
  Result := IndexOf(_key);
  if Result >= 0 then begin
    s := Feld[Result].value;
    p := Feld[Result].data;
  end else begin
    s := '';
    p := nil;
  end;
end;

function THash.GetData(const _key: String): Pointer;
var
  i: integer;
begin
  i := IndexOf(_key);
  if i >= 0 then
    Result := Feld[i].data
  else
    Result := nil;
end;

function THash.GetValue(const _key: String): String;
var
  i: integer;
begin
  i := IndexOf(_key);
  if i >= 0 then
    Result := Feld[i].value
  else
    Result := '';
end;

{$IFDEF new}
procedure THash.Delete(Index: Integer);
begin
  inherited;

end;

function THash.Get(index: integer): string;
begin

end;

function THash.GetCount: integer;
begin

end;

procedure THash.Insert(index: integer; s: string);
begin
  inherited;

end;
{$ELSE}
{$ENDIF}

{$ELSE}
procedure THash.GetStringObject(const _key: String; var s: String; var p: Pointer);
var
  h: Integer;
  he: PHashEntry;
begin
  s:=''; p:=nil;
  h:=Hash(_key); he:=Feld^[h and FeldMaxValue];
  while Assigned(he) do begin
    if he^.hash=h then
      if he^.key=_key then begin
        s:=he^.value;
        p:=he^.data;
        Exit;
      end;
    he:=he^.next;
  end;
end;

function THash.GetObject(const _key: String): Pointer;
var
  h: Integer;
  he: PHashEntry;
begin
  Result:=nil;
  h:=Hash(_key); he:=Feld^[h and FeldMaxValue];
  while Assigned(he) do begin
    if he^.hash=h then
      if he^.key=_key then begin
        Result:=he^.data;
        Exit;
      end;
    he:=he^.next;
  end;
end;

function THash.GetString(const _key: String): String;
var
  h: Integer;
  he: PHashEntry;
begin
  Result:='';
  h:=Hash(_key); he:=Feld^[h and FeldMaxValue];
  while Assigned(he) do begin
    if he^.hash=h then
      if he^.key=_key then begin
        Result:=he^.value;
        Exit;
      end;
    he:=he^.next;
  end;
end;

function THash.Keys: TStringList;
var
  i: Integer;
  he: PHashEntry;
begin
  Result:=TStringList.Create;
  for i:=0 to FeldMaxValue do begin
    he:=Feld^[i];
    while Assigned(he) do begin
      Result.Add(he^.key);
      he:=he^.next;
    end;
  end;
end;

function THash.Keys(const beginning: String): TStringList;
var
  i: Integer;
  he: PHashEntry;
begin
  Result:=TStringList.Create;
  for i:=0 to FeldMaxValue do begin
    he:=Feld^[i];
    while Assigned(he) do begin
      if Copy(he^.key,1,length(beginning))=beginning then
        Result.Add(he^.key);
      he:=he^.next;
    end;
  end;
end;
{$ENDIF}

{ $log:
2001-06-19  wolf: created this unit
2001-08-12  wolf: added overloaded Keys(String) Method
2001-08-14  wolf: bugfix: test if assigned
2001-09-12  wolf: completely rewritten (now using a real hashing algorithm)
2001-09-25  wolf: added Delphi compatibility
2001-12-21  wolf: added properties Count, Capacity, MaxCapacity
2001-12-23  wolf: MakeHashLarger fixed (now possible to set Capacity)
}

{ TObjectHash }

{$IFDEF solid}

function TObjectHash.GetObject(const _key: String): TObject;
var
  i: integer;
begin
  i := IndexOf(_key);
  if i < 0 then
    Result := nil
  else
    pointer(Result) := Feld[i].data;
end;

procedure TObjectHash.SetObject(const _key: String; _data: TObject);
begin
  _SetValueData(_key, '', false, _data, true);
end;
{$ELSE}
procedure TObjectHash.Delete(const _key: String);
begin
  DeleteKey(_key);
end;
{$ENDIF}

end.
