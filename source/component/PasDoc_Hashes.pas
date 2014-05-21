(*
  This unit implements an associative array.
  Before writing this unit, I've always missed Perl commands
  like @code($h{abc}='def') in Pascal.

  @author(Copyright (C) 2001-2014  Wolf Behrenhoff <wolf@behrenhoff.de> and PasDoc developers)

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

{$I pasdoc_defines.inc}

{$Q-} // no integer overflow checks (I need overflow in THash.Hash)
{$R-} // no range checks (because free bounds of TFakeArray[0..0])

interface

uses
  SysUtils, Classes;

type
  { }
  PPHashEntry=^PHashEntry;
  PHashEntry=^THashEntry;
  THashEntry=record
    next: PHashEntry;
    hash: Integer;
    key: String;
    value: String;
    data: Pointer;
  end;

  { in FPC, I can simply use PPHashEntry as an array of PHashEntry -
    Delphi doesn't allow that. I need this stupid array[0..0] definition!
    From Delphi4, I could use a dynamic array. }
  TFakeArray=array[0..0] of PHashEntry;
  PFakeArray=^TFakeArray;

  THash=class
  private
    Feld: PFakeArray;
    FeldMaxValue: Integer;
    FeldBelegt: Integer;
    FMaxCapacity: Integer;
    function Hash(key: String): Integer;
    procedure MakeHashLarger(power2: Integer);
    procedure SetCapacity(new_size: Integer);
    function GetCapacity: Integer;
    procedure SetMaxCapacity(newmc: Integer);
    procedure _SetStringObject(_key: String; s: String; set_s: Boolean;
                                             p: Pointer; set_p: Boolean);

  public
    property Count: Integer read FeldBelegt;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read FMaxCapacity write SetMaxCapacity;
    constructor Create;
    destructor Destroy; override;
    procedure SetObject(_key: String; data: Pointer);
    procedure SetString(_key: String; data: String);
    procedure SetStringObject(_key: String; s: String; p: Pointer);
    function GetObject(_key: String): Pointer;
    function GetString(_key: String): String;
    procedure GetStringObject(_key: String; var s: String; var p: Pointer);
    function KeyExists(_key: String): Boolean;
    procedure DeleteKey(_key: String);
    function Keys: TStringList; overload;
    function Keys(beginning: String): TStringList; overload;
  end;

  TObjectHash = class(THash)
  public
    procedure Delete(_key: String);
    property Items[_key: string]: Pointer read GetObject write SetObject;
  end;

implementation

constructor THash.Create;
begin
  FeldMaxValue:=7; //irgendein kleiner Wert der Eigenschaft 2**n-1 (English: any small value of the property 2**n-1)
  FeldBelegt:=0;
  FMaxCapacity:=-1;
  GetMem(Feld,sizeof(PHashEntry)*Succ(FeldMaxValue));
  FillChar(Feld^,sizeof(PHashEntry)*Succ(FeldMaxValue),0);
end;

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
  FreeMem(Feld,sizeof(PHashEntry)*(Succ(FeldMaxValue)));
  Feld := nil;
  inherited Destroy;
end;

function THash.Hash(key: String): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to length(key) do
    Result:=33*Result+Ord(key[i]);
  Result:=Result+Result shr 5;
end;

function THash.KeyExists(_key: String): Boolean;
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
  i, oldsize, newsize, newpos: Integer;
  he: PHashEntry;
  oe: PPHashEntry;
begin
  //zun„chst Speicher reservieren (Enlish?: reserve additional memory)
  oldsize:=Succ(FeldMaxValue);
  newsize:=oldsize shl power2;
  if (newsize>MaxCapacity) and (MaxCapacity<>-1) then Exit;
  FeldMaxValue:=Pred(newsize);

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
end;

procedure THash.SetMaxCapacity(newmc: Integer);
begin
  if newmc<0 then FMaxCapacity:=-1
  else if newmc<Capacity then FMaxCapacity:=Capacity
  else FMaxCapacity:=newmc;
end;

procedure THash._SetStringObject(_key: String; s: String; set_s: Boolean;
                                        p: Pointer; set_p: Boolean);
var
  h: Integer;
  he, anker: PHashEntry;
begin
  if FeldBelegt>FeldMaxValue then MakeHashLarger(1);
  h:=Hash(_key);
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

  //nichts gefunden, key muá neu angelegt werden (English?: not found; a new key must be created.)
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

procedure THash.SetStringObject(_key: String; s: String; p: Pointer);
begin
  _SetStringObject(_key,s,true,p,true);
end;

procedure THash.SetObject(_key: String; data: Pointer);
begin
  _SetStringObject(_key,'',false,data,true);
end;

procedure THash.SetString(_key: String; data: String);
begin
  _SetStringObject(_key,data,true,nil,false);
end;

procedure THash.DeleteKey(_key: String);
var
  he: PHashEntry;
  oe: PPHashEntry;
  h: Integer;
begin
  h:=Hash(_key);
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

procedure THash.GetStringObject(_key: String; var s: String; var p: Pointer);
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

function THash.GetObject(_key: String): Pointer;
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

function THash.GetString(_key: String): String;
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

function THash.Keys(beginning: String): TStringList;
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

procedure TObjectHash.Delete(_key: String);
begin
  DeleteKey(_key);
end;

end.
