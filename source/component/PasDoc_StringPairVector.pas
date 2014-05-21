{
  Copyright 1998-2014 PasDoc developers.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ @abstract(Simple container for a pair of strings.) }
unit PasDoc_StringPairVector;

{$I pasdoc_defines.inc}

interface

uses
  Classes,
  PasDoc_ObjectVector;

type
  TStringPair = class
    Name: string;
    Value: string;
    Data: Pointer;

    { Init Name and Value by @link(ExtractFirstWord) from S. }
    constructor CreateExtractFirstWord(const S: string);
    
    constructor Create; overload;
    constructor Create(const AName, AValue: string; AData: Pointer = nil); overload;
  end;
  
  { List of string pairs.
    This class contains only non-nil objects of class TStringPair.

    Using this class instead of TStringList (with it's Name and Value
    properties) is often better, because this allows both Name and Value
    of each pair to safely contain any special characters (including '=' 
    and newline markers). It's also faster, since it doesn't try to
    encode Name and Value into one string. }
  TStringPairVector = class(TObjectVector)
  private
    function GetItems(i: Integer): TStringPair;
    procedure SetItems(i: Integer; Item: TStringPair);
  public
    property Items[i: Integer]: TStringPair read GetItems write SetItems; default;
  
    { Returns all items Names and Values glued together.
      For every item, string Name + NameValueSepapator + Value is
      constructed. Then all such strings for every items all
      concatenated with ItemSeparator.
      
      Remember that the very idea of @link(TStringPair) and 
      @link(TStringPairVector) is that Name and Value strings
      may contain any special characters, including things you
      give here as NameValueSepapator and ItemSeparator.
      So it's practically impossible to later convert such Text
      back to items and Names/Value pairs. }
    function Text(const NameValueSepapator, ItemSeparator: string): string;
    
    { Finds a string pair with given Name.
      Returns -1 if not found. }
    function FindName(const Name: string; IgnoreCase: boolean = true): Integer;
    
    { Removes first string pair with given Name. 
      Returns if some pair was removed. }
    function DeleteName(const Name: string; IgnoreCase: boolean = true): boolean;
    
    { Load from a stream using the binary format.
      For each item, it's Name and Value are saved.
      (TStringPair.Data pointers are @italic(not) saved.) }
    procedure LoadFromBinaryStream(Stream: TStream);

    { Save to a stream, in a format readable by
      @link(LoadFromBinaryStream). }
    procedure SaveToBinaryStream(Stream: TStream);
    
    { Name of first item, or '' if list empty. }
    function FirstName: string;
  end;

implementation

uses 
  SysUtils { For LowerCase under Kylix 3 }, 
  PasDoc_Utils, PasDoc_Serialize;

{ TStringPair ---------------------------------------------------------------- }

constructor TStringPair.CreateExtractFirstWord(const S: string);
var 
  FirstWord, Rest: string;
begin
  ExtractFirstWord(S, FirstWord, Rest);
  Create(FirstWord, Rest);
end;

constructor TStringPair.Create;
begin
  inherited Create;
end;

constructor TStringPair.Create(const AName, AValue: string; AData: Pointer);
begin
  Create;
  Name := AName;
  Value := AValue;
  Data := AData;
end;

{ TStringPairVector ---------------------------------------------------------- }

function TStringPairVector.GetItems(i: Integer): TStringPair;
begin
  Result := TStringPair(inherited Items[i]);
end;

procedure TStringPairVector.SetItems(i: Integer; Item: TStringPair);
begin
  inherited Items[i] := Item;
end;

function TStringPairVector.Text(
  const NameValueSepapator, ItemSeparator: string): string;
var 
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Items[0].Name + NameValueSepapator + Items[0].Value;
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator +
        Items[i].Name + NameValueSepapator + Items[i].Value;
  end;
end;

function TStringPairVector.FindName(const Name: string; 
  IgnoreCase: boolean): Integer;
var
  LowerCasedName: string;
begin
  if IgnoreCase then
  begin
    LowerCasedName := LowerCase(Name);
    for Result := 0 to Count - 1 do
      if LowerCase(Items[Result].Name) = LowerCasedName then
        Exit;
    Result := -1;
  end else
  begin
    for Result := 0 to Count - 1 do
      if Items[Result].Name = Name then
        Exit;
    Result := -1;
  end;
end;

function TStringPairVector.DeleteName(const Name: string; 
  IgnoreCase: boolean): boolean;
var
  i: Integer;
begin
  i := FindName(Name, IgnoreCase);
  Result := i <> -1;
  if Result then
    Delete(i);
end;

procedure TStringPairVector.LoadFromBinaryStream(Stream: TStream);
var
  I, N: Integer;
  P: TStringPair;
begin
  Clear;
  N := TSerializable.LoadIntegerFromStream(Stream);
  Capacity := N;
  for I := 0 to N - 1 do
  begin
    P := TStringPair.Create;
    Add(P);
    P.Name := TSerializable.LoadStringFromStream(Stream);
    P.Value := TSerializable.LoadStringFromStream(Stream);
  end;
end;

procedure TStringPairVector.SaveToBinaryStream(Stream: TStream);
var 
  I: Integer;
begin
  TSerializable.SaveIntegerToStream(Count, Stream);
  for i := 0 to Count - 1 do
  begin
    TSerializable.SaveStringToStream(Items[I].Name, Stream);
    TSerializable.SaveStringToStream(Items[I].Value, Stream);
  end;
end;

function TStringPairVector.FirstName: string;
begin
  if Count > 0 then
    Result := Items[0].Name else
    Result := '';
end;

end.
