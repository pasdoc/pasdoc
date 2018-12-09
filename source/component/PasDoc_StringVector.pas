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
  @author(Michalis Kamburelis)
  @abstract(String vector based on TStringList.)
  The string vector is based on TStringList and simply exports
  a few extra functions - I did this so I didn't have to change
  so much old code, this has only little additional
  functionality
}
unit PasDoc_StringVector;

{$I pasdoc_defines.inc}

interface

uses
  Classes;

type
  TStringVector = class(TStringList)
  public
    { This is the same thing as Items[0] }
    function FirstName: string;

    procedure LoadFromTextFileAdd(const AFilename: string); overload;
    procedure LoadFromTextFileAdd(var ATextFile: TextFile); overload;
    procedure RemoveAllNamesCI(const AName: string);
    function ExistsNameCI(const AName: string): boolean;
    function IsEmpty: boolean;
    function AddNotExisting(const AString: string): Integer;

    { Load from a stream using the binary format.

      The binary format is
      @unorderedList(
        @item Count
        @item(followed by each string, loaded using
          @link(TSerializable.LoadStringFromStream).)
      )

      Note that you should never use our Text value to load/save this object
      from/into a stream, like
      @code(Text := TSerializable.LoadStringFromStream(Stream)).
      Using and assigning to the Text value breaks when some strings have
      newlines inside that should be preserved. }
    procedure LoadFromBinaryStream(Stream: TStream);

    { Save to a stream, in a format readable by
      @link(LoadFromBinaryStream). }
    procedure SaveToBinaryStream(Stream: TStream);
  end;

function NewStringVector: TStringVector;
function IsEmpty(const AOV: TStringVector): boolean; overload;

implementation
uses
  SysUtils, PasDoc_Serialize;

function IsEmpty(const AOV: TStringVector): boolean;
begin
  Result := (not Assigned(AOV)) or (AOV.Count = 0);
end;

function NewStringVector: TStringVector;
begin
  Result := TStringVector.Create;
  Result.Duplicates := dupIgnore;
end;

{ TStringVector }

function TStringVector.AddNotExisting(const AString: string): integer;
begin
  Result := IndexOf(AString);
  if Result < 0 then begin
    Result := Add(AString);
  end;
end;

function TStringVector.ExistsNameCI(const AName: string): boolean;
var
  i: Integer;
  LName: string;
begin
  LName := LowerCase(AName);
  Result := false;
  for i := Count - 1 downto 0 do begin
    if LowerCase(Get(i)) = LName then begin
      Result := True;
      break;
    end;
  end;
end;

function TStringVector.FirstName: string;
begin
  if Count > 0 then
  begin
    Result := Get(0);
  end else
  begin
    Result := '';
  end
end;

function TStringVector.IsEmpty: boolean;
begin
  Result := Count = 0;
end;

procedure TStringVector.LoadFromTextFileAdd(
  const AFilename: string);
var
  LCurrent: string;
begin
  LCurrent := Text;
  LoadFromFile(AFilename);
  Add(LCurrent);
end;

procedure TStringVector.LoadFromTextFileAdd(var ATextFile: TextFile);
var S: string;
begin
  while not Eof(ATextFile) do
  begin
    Readln(ATextFile, S);
    S := Trim(S);
    if S <> '' then Append(S);
  end;
end;

procedure TStringVector.RemoveAllNamesCI(const AName: string);
var
  i: Integer;
  LName: string;
begin
  LName := LowerCase(AName);
  for i := Count - 1 downto 0 do begin
    if LowerCase(Get(i)) = LName then begin
      Delete(i);
    end;
  end;
end;

procedure TStringVector.LoadFromBinaryStream(Stream: TStream);
var
  i, n: Integer;
begin
  Clear;
  n := TSerializable.LoadIntegerFromStream(Stream);
  Capacity := n;
  for i := 0 to n - 1 do
    Append(TSerializable.LoadStringFromStream(Stream));
end;

procedure TStringVector.SaveToBinaryStream(Stream: TStream);
var i: Integer;
begin
  TSerializable.SaveIntegerToStream(Count, Stream);
  for i := 0 to Count - 1 do
    TSerializable.SaveStringToStream(Strings[i], Stream);
end;

end.
