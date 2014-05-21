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

{ @abstract(Serializing/deserializing cached information.)
  @author(Arno Garrels <first name.name@nospamgmx.de>) }
unit PasDoc_Serialize;

{$I pasdoc_defines.inc}

interface

uses
  Classes,
  SysUtils,
  PasDoc_StreamUtils;

type
  TSerializable = class;
  TSerializableClass = class of TSerializable;
  
  EInvalidCacheFileVersion = class(Exception);

  TSerializable = class
  private
    FWasDeserialized: boolean;
  protected
    procedure Serialize(const ADestination: TStream); virtual;
    procedure Deserialize(const ASource: TStream); virtual;
  public
    class function Read7BitEncodedInt(const ASource: TStream): Integer;
    class procedure Write7BitEncodedInt(Value: Integer;
      const ADestination: TStream);
    class function LoadStringFromStream(const ASource: TStream): string;
    class procedure SaveStringToStream(const AValue: string; const ADestination: TStream);
    class function LoadDoubleFromStream(const ASource: TStream): double;
    class procedure SaveDoubleToStream(const AValue: double; const ADestination: TStream);
    class function LoadIntegerFromStream(const ASource: TStream): Longint;
    class procedure SaveIntegerToStream(const AValue: Longint; const ADestination: TStream);

    constructor Create; virtual;
    class procedure SerializeObject(const AObject: TSerializable; const ADestination: TStream);
    class function DeserializeObject(const ASource: TStream): TSerializable;
    class procedure Register(const AClass: TSerializableClass);
    procedure SerializeToFile(const AFileName: string);
    { Read back from file.
      @raises(EInvalidCacheFileVersion When the cached file contents
        are from an old pasdoc version (or invalid).) }
    class function DeserializeFromFile(const AFileName: string): TSerializable;
    property WasDeserialized: boolean read FWasDeserialized;
  end;

  ESerializedException = class(Exception);

implementation

uses PasDoc_Versions;

const
  { String to mark cache file version.

    When you change how/what is serialized, you generally break previous
    cache files. So we store, and read, a string at the beginning
    of cache file. Only when it's equal to our current CacheFormatVersion,
    we know it's Ok. This allows us to behave nicely when encountering
    cache files from previous pasdoc versions. Note that Delphi Unicode
    versions write UTF-16 strings, that's why Ansi and Unicode versions
    cannot share the same cache files.

    Changing PasDoc_Version always changes cache version, for safety.
    If you want, you can also bump the suffix -xxx added here,
    when some SVN revision changes cache format. }
  CacheFormatVersion = PasDoc_Version + '-2' {$IFDEF STRING_UNICODE} + 'U' {$ENDIF};

var
  GClassNames: TStringList;

{ TSerializable }

constructor TSerializable.Create;
begin
  inherited;
end;

class function TSerializable.Read7BitEncodedInt(const ASource: TStream): Integer;
var
  Shift: Integer;
  Value: Integer;
  B: Byte;
begin
  Shift  := 0;
  Result := 0;
  repeat
    if Shift = 35 then
      raise ESerializedException.Create('Invalid 7 bit integer encoding');
    ASource.Read(B, 1);
    Value := B;
    Result := Result or ((Value and $7F) shl Shift);
    Inc(Shift, 7);
  until Value and $80 = 0;
end;

class procedure TSerializable.Write7BitEncodedInt(Value: Integer;
 const ADestination: TStream);
var
  B: Byte;
begin
  repeat
    if Value > $7f then begin
      B := Byte((Value and $7f) or $80);
      ADestination.Write(B, 1);
    end
    else begin
      B := Byte(Value);
      ADestination.Write(B, 1);
    end;
    Value := Value shr 7;
  until Value = 0;
end;

procedure TSerializable.Serialize(const ADestination: TStream);
begin
end;

procedure TSerializable.Deserialize(const ASource: TStream);
begin
  FWasDeserialized := True;
end;

class function TSerializable.DeserializeFromFile(
  const AFileName: string): TSerializable;
var
  LF: TStream;
  CacheFormatVersionFromFile: string;
begin
  Result := nil; // makes the Delphi compiler happy
{$IFDEF USE_BUFFERED_STREAM}
  LF := TBufferedStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
{$ELSE}
  LF := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
{$ENDIF}
  try
    try 
      CacheFormatVersionFromFile := LoadStringFromStream(LF);
    except
      { Convert any exception from LoadStringToStream (maybe because 
        string length is invalid, or stream ends too soon or such)
        to EInvalidCacheFileVersion. }
      on E: Exception do 
        raise EInvalidCacheFileVersion.CreateFmt(
          'Cache file version is invalid (error when reading: %s), assuming the cache file is outdated',
          [E.ClassName]);
    end;
    
    if CacheFormatVersionFromFile <> CacheFormatVersion then
      raise EInvalidCacheFileVersion.Create('Cache file version is from a different PasDoc release');
    
    Result := DeserializeObject(LF);
  finally
    LF.Free;
  end;
end;

class function TSerializable.DeserializeObject(
  const ASource: TStream): TSerializable;
var
  S: string;
  LClass: TSerializableClass;
  Idx: Integer;
begin
  S := LoadStringFromStream(ASource);
  Idx := GClassNames.IndexOf(S);
  if Idx < 0 then begin
    raise ESerializedException.CreateFmt('Tried loading unknown class %s', [S]);
  end else begin
    LClass := TSerializableClass(GClassNames.Objects[Idx]);
    Result := LClass.Create;
    Result.Deserialize(ASource);
  end;
end;

class function TSerializable.LoadIntegerFromStream(
  const ASource: TStream): Longint;
begin
  ASource.Read(Result, SizeOf(Result));
end;

class function TSerializable.LoadDoubleFromStream(
  const ASource: TStream): double;
begin
  ASource.Read(Result, SizeOf(Result));
end;

class function TSerializable.LoadStringFromStream(
  const ASource: TStream): string;
var
  L: LongInt;
begin
  L := Read7BitEncodedInt(ASource);
  SetLength(Result, L);
  ASource.Read(Pointer(Result)^, L * SizeOf(Char));
end;

class procedure TSerializable.Register(const AClass: TSerializableClass);
begin
  GClassNames.AddObject(AClass.ClassName, TObject(AClass));
end;

class procedure TSerializable.SaveIntegerToStream(
  const AValue: Longint; const ADestination: TStream);
begin
  ADestination.Write(AValue, SizeOf(AValue));
end;

class procedure TSerializable.SaveDoubleToStream(const AValue: double;
  const ADestination: TStream);
begin
  ADestination.Write(AValue, SizeOf(AValue));
end;

class procedure TSerializable.SaveStringToStream(const AValue: string;
  const ADestination: TStream);
var
  L: Longint;
begin
  L := Length(AValue);
  Write7BitEncodedInt(L, ADestination);
  ADestination.Write(Pointer(AValue)^, L * SizeOf(Char));
end;

class procedure TSerializable.SerializeObject(const AObject: TSerializable;
  const ADestination: TStream);
var
  S: string;
begin
  S := AObject.ClassName;
  if GClassNames.IndexOf(S)< 0 then begin
    raise ESerializedException.CreateFmt('Tried saving unregistered class %s', [S]);
  end;
  SaveStringToStream(S, ADestination);
  AObject.Serialize(ADestination);
end;

procedure TSerializable.SerializeToFile(const AFileName: string);
var
  LF: TStream;
begin
{$IFDEF USE_BUFFERED_STREAM}
  LF := TBufferedStream.Create(AFileName, fmCreate);
{$ELSE}
  LF := TFileStream.Create(AFileName, fmCreate);
{$ENDIF}
  try
    SaveStringToStream(CacheFormatVersion, LF);
    SerializeObject(Self, LF);
  finally
    LF.Free;
  end;
end;

initialization
  GClassNames := TStringList.Create;
finalization
  GClassNames.Free;
end.
