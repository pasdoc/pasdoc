unit PasDoc_TagManager;

interface

uses
  SysUtils,
  Classes;

type
  TTagHandler = procedure(const TagName: string; const TagDesc: string; var ReplaceStr: string) of object;
  TStringConverter = function(const s: string): string of object;

type
  TTagHandlerObj = class
  private
    FTagHandler: TTagHandler;
  public
    constructor Create(TagHandler: TTagHandler);
    procedure Execute(const TagName: string; const TagDesc: string; var ReplaceStr: string);
  end;

type
  TTagManager = class
  private
    FTags: TStringList;
    FDescription: string;
    FOffset: Integer;
    FTagEnd: Integer;
    FDescLen: integer;
    FStringConverter: TStringConverter;
    FAbbreviations: TStringList;
    function FindTag(var TagName: string; var Parameters: string): Boolean;
    function ConvertString(const s: string): string;
    procedure Unabbreviate(var s: string);
    procedure ReplaceInDesc(Offset: integer; OldLen: integer; const s: string; var NewLen: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddHandler(const TagName: string; Handler: TTagHandler);
    procedure Execute(var Description: string);
    property StringConverter: TStringConverter read FStringConverter write FStringConverter;
    property Abbreviations: TStringList read FAbbreviations write FAbbreviations;
  end;

implementation

{ TTagHandlerObj }

constructor TTagHandlerObj.Create(TagHandler: TTagHandler);
begin
  inherited Create;
  fTagHandler := Taghandler;
end;

procedure TTagHandlerObj.Execute(const TagName, TagDesc: string; var ReplaceStr: string);
begin
  if Assigned(fTagHandler) then
    fTagHandler(TagName, TagDesc, ReplaceStr);
end;

{ TTagManager }

constructor TTagManager.Create;
begin
  inherited Create;
  FTags := TStringList.Create;
  FTags.Sorted := true;
end;

destructor TTagManager.Destroy;
var
  i: integer;
begin
  if FTags <> nil then
    begin
      for i:=0 to FTags.Count-1 do
        FTags.Objects[i].Free;
      FTags.Free;
    end;
  inherited;
end;

procedure TTagManager.AddHandler(const TagName: string; Handler: TTagHandler);
begin
  FTags.AddObject(LowerCase(Tagname), TTagHandlerObj.Create(Handler));
end;

function TTagManager.ConvertString(const s: string): string;
begin
  if Assigned(FStringConverter) then
    Result := FStringConverter(s)
  else
    Result := s;
end;

procedure TTagManager.Unabbreviate(var s: string);
var
  idx: Integer;
begin
  if Assigned(Abbreviations) then begin
    idx := Abbreviations.IndexOfName(s);
    if idx>=0 then begin
      s := Abbreviations.Values[s];
    end;
  end;
end;

procedure TTagManager.ReplaceInDesc(Offset: integer; OldLen: integer; const s: string; var NewLen: integer);
begin
  Delete(FDescription, Offset, OldLen);
  Insert(s, FDescription, Offset);
  NewLen := Length(s);
end;

procedure TTagManager.Execute(var Description: string);
var
  ConvertOffset: integer;

  procedure Convert;
  var
    s: string;
    OldLen: integer;
    NewLen: integer;
  begin
    if ConvertOffset >= FOffset then
      exit;
    OldLen := FOffset - ConvertOffset;
    s := ConvertString(Copy(FDescription, ConvertOffset, OldLen));
    ReplaceInDesc(ConvertOffset, OldLen, s, NewLen);
    FOffset := FOffset +  NewLen - OldLen;
    ConvertOffset := FTagEnd + NewLen - OldLen;
  end;

var
  ReplaceStr: string;
  TagName: string;
  Params: string;
  Idx: integer;
  OldLen: integer;
  NewLen: integer;
begin
  FDescription := Description;

  FOffset := 1;
  FTagEnd := -1;
  FDescLen := Length(fDescription);
  ConvertOffset := 1;

  while FOffset < FDescLen do begin
    if (FDescription[FOffset] = '@') and FindTag(TagName, Params) then begin
      if Params <> '' then begin
        Unabbreviate(Params);
        ReplaceStr := '(' + Params + ')';
      end else
        ReplaceStr := '';
      ReplaceStr := ConvertString('@' + TagName + ReplaceStr);
      if FTags.Find(TagName, Idx) then
        (FTags.Objects[Idx] as TTagHandlerObj).Execute(TagName, Params, ReplaceStr);
      OldLen := FTagEnd - FOffset;
      ReplaceInDesc(FOffset, OldLen, ReplaceStr, NewLen);
      FTagEnd := FTagEnd + NewLen - OldLen;
      Convert;
      fDescLen := length(FDescription);
    end;
    Inc(FOffset);
  end;
  Description := FDescription;
end;

function TTagManager.FindTag(var TagName: string; var Parameters: string): Boolean;
var
  i: Integer;
  BracketCount: integer;
begin
  Result := False;
  Parameters := '';
  i := FOffset;
  if (FDescription[i] <> '@') then
    exit;

  Inc(i);
  while (i < FDescLen) and (FDescription[i] in ['A'..'Z', 'a'..'z', '@']) do begin
    Inc(i);
  end;
  TagName := LowerCase(Copy(FDescription, FOffset + 1, i - FOffset - 1));

  FTagEnd := i;
  Result := true;

  { ok, we found the correct tag, now lets get the parameters }

  if i >= FDescLen then 
    Exit;

  if FDescription[i] <> '(' then { there must be at least 2 characters left }
    Exit; { no "(" found }

  Inc(i);
  BracketCount := 1;
  repeat
    case FDescription[i] of
      '(': Inc(BracketCount);
      ')': Dec(BracketCount);
    end;
    Inc(i);
  until (i > FDescLen) or (BracketCount = 0);
  if (BracketCount = 0) then begin
    Parameters := Copy(FDescription, FTagEnd + 1, i - FTagEnd - 2);
    FTagEnd := i;
  end;
end;

end.
