{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  a simple object vector
}
unit PasDoc_ObjectVector;

{$I pasdoc_defines.inc}

interface
uses
  Contnrs,
  Classes;

type
  TObjectVector = class(TObjectList)
  public
    { This is only to make constructor virtual, while original
      TObjectList has a static constructor. }
    constructor Create(const AOwnsObject: boolean); virtual;
{$IFNDEF FPC}
    // Fix bug in D7 TList.Sort.
    procedure Sort(Compare: TListSortCompare); reintroduce;
{$ENDIF}
  end;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean; 

implementation

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
begin
  Result := (not Assigned(AOV)) or (AOV.Count = 0);
end;

{ TObjectVector }

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create(AOwnsObject);
end;

{$IFNDEF FPC}
procedure TObjectVector.Sort(Compare: TListSortCompare);
begin
  if Count <= 1 then
    exit;
  inherited;
end;
{$ENDIF}

end.
