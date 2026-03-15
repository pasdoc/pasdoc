{
  Copyright 2026-2026 PasDoc developers.

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

{ Test PasDoc_Types. }
unit TestPasdoc_Types;

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestPasDocTypes = class(TTestCase)
  published
    procedure TestSplitNameParts;
  end;

implementation

uses PasDoc_Types;

procedure TTestPasDocTypes.TestSplitNameParts;
var
  NameParts: TNameParts;
begin
  AssertTrue(SplitNameParts(
    'MyUnit.TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType', NameParts));
  AssertEquals(5, Length(NameParts));
  AssertEquals('MyUnit', NameParts[0]);
  AssertEquals('TMyClass', NameParts[1]);
  AssertEquals('TMyNestedClass', NameParts[2]);
  AssertEquals('TAnotherNestedClass', NameParts[3]);
  AssertEquals('TOriginalType', NameParts[4]);
end;

initialization
  RegisterTest(TTestPasDocTypes);
end.
