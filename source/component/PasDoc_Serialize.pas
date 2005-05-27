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
    procedure Serialize(const ADestination: TStream); virtual;
    procedure Deserialize(const ASource: TStream); virtual;
  public  
    class function LoadStringFromStream(const ASource: TStream): string;
    class procedure SaveStringToStream(const AValue: string; const ADestination: TStream);
    class function LoadDoubleFromStream(const ASource: TStream): double;
    class procedure SaveDoubleToStream(const AValue: double; const ADestination: TStream);
    class function LoadIntegerFromStream(const ASource: TStream): Integer;
    class procedure SaveIntegerToStream(const AValue: Integer; const ADestination: TStream);

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

class function TSerializable.LoadIntegerFromStream(
  const ASource: TStream): Integer;
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
  L: Integer;
begin
  ASource.Read(L, SizeOf(L));
  SetLength(Result, L);
  ASource.Read(Pointer(Result)^, L);
end;

class procedure TSerializable.Register(const AClass: TSerializableClass);
begin
{$ifdef fpc}
{$ifdef ver1_0}
  GClassNames.AddObject(AClass.ClassName, TObject(pointer(AClass)));
{$else}
  GClassNames.AddObject(AClass.ClassName, TObject(AClass));
{$endif}  
{$else}
  GClassNames.AddObject(AClass.ClassName, TObject(AClass));
{$endif}
end;

class procedure TSerializable.SaveIntegerToStream(
  const AValue: Integer; const ADestination: TStream);
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
  L: Integer;
begin
  L := Length(AValue);
  ADestination.Write(L, SizeOf(L));
  ADestination.Write(Pointer(AValue)^, L);
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
 