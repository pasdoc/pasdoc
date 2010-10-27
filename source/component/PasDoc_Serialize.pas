{ @author(Arno Garrels <first name.name@nospamgmx.de>) }
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
    class function DeserializeFromFile(const AFileName: string): TSerializable;
    property WasDeserialized: boolean read FWasDeserialized;
  end;

  ESerializedException = class(Exception);

implementation

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
begin
{$IFDEF COMPILER_7_UP}
  LF := TBufferedStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
{$ELSE}
  LF := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
{$ENDIF}
  try
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
{$IFDEF COMPILER_7_UP}
  LF := TBufferedStream.Create(AFileName, fmCreate);
{$ELSE}
  LF := TFileStream.Create(AFileName, fmCreate);
{$ENDIF}
  try
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
