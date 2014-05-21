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
