unit PasDoc_Serialize;

interface
uses
  Classes,
  SysUtils;

type
  TSerializable = class;
  TSerializableClass = class of TSerializable;

  TSerializable = class
  private
    FWasDeserialized: boolean;
  protected
    procedure Serialize(const ADestination: TStream); virtual; abstract;
    procedure Deserialize(const ASource: TStream); virtual;
    class function LoadStringFromStream(const ASource: TStream): string;
    class procedure SaveStringToStream(const AValue: string; const ADestination: TStream);
  public
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

procedure TSerializable.Deserialize(const ASource: TStream);
begin
  FWasDeserialized := True;
end;

class function TSerializable.DeserializeFromFile(
  const AFileName: string): TSerializable;
var
  LF: TFileStream;
begin
  LF := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := DeserializeObject(LF);
  finally
    LF.Free;
  end;
end;

class function TSerializable.DeserializeObject(
  const ASource: TStream): TSerializable;
var
  S: shortstring;
  LClass: TSerializableClass;
  Idx: Integer;
begin
  ASource.Read(S[0], 1);
  ASource.Read(S[1], Byte(S[0]));
  Idx := GClassNames.IndexOf(S);
  if Idx<0 then begin
    raise ESerializedException.CreateFmt('Tried loading unknown class %s', [S]);
  end else begin
    LClass := TSerializableClass(GClassNames.Objects[Idx]);
    Result := LClass.Create;
    Result.Deserialize(ASource);
  end;
end;

class function TSerializable.LoadStringFromStream(
  const ASource: TStream): string;
var
  L: Integer;
begin
  ASource.Read(L, SizeOf(L));
  SetLength(Result, L);
  ASource.Read(Result[1], L);
end;

class procedure TSerializable.Register(const AClass: TSerializableClass);
begin
  GClassNames.AddObject(AClass.ClassName, TObject(AClass));
end;

class procedure TSerializable.SaveStringToStream(const AValue: string;
  const ADestination: TStream);
var
  L: Integer;
begin
  L := Length(AValue);
  ADestination.Write(L, SizeOf(L));
  ADestination.Write(AValue[1], L);
end;

class procedure TSerializable.SerializeObject(const AObject: TSerializable;
  const ADestination: TStream);
var
  S: shortstring;
begin
  S := AObject.ClassName;
  if GClassNames.IndexOf(S)<0 then begin
    raise ESerializedException.CreateFmt('Tried saving unregistered class %s', [S]);
  end;
  ADestination.Write(S[0], Byte(S[0])+1);
  AObject.Serialize(ADestination);
end;

procedure TSerializable.SerializeToFile(const AFileName: string);
var
  LF: TFileStream;
begin
  LF := TFileStream.Create(AFileName, fmCreate);
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
 