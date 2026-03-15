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
    procedure TestStripNamePart;
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

procedure TTestPasDocTypes.TestStripNamePart;
var
  NameParts, NewNameParts: TNameParts;
begin
  AssertTrue(SplitNameParts(
    'MyUnit.TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType', NameParts));

  NewNameParts := StripNamePart(NameParts);
  AssertEquals(4, Length(NewNameParts));
  AssertEquals('TMyClass', NewNameParts[0]);
  AssertEquals('TMyNestedClass', NewNameParts[1]);
  AssertEquals('TAnotherNestedClass', NewNameParts[2]);
  AssertEquals('TOriginalType', NewNameParts[3]);

  NewNameParts := StripNamePart(NewNameParts);
  AssertEquals(3, Length(NewNameParts));
  AssertEquals('TMyNestedClass', NewNameParts[0]);
  AssertEquals('TAnotherNestedClass', NewNameParts[1]);
  AssertEquals('TOriginalType', NewNameParts[2]);

  NewNameParts := StripNamePart(NewNameParts);
  AssertEquals(2, Length(NewNameParts));
  AssertEquals('TAnotherNestedClass', NewNameParts[0]);
  AssertEquals('TOriginalType', NewNameParts[1]);

  NewNameParts := StripNamePart(NewNameParts);
  AssertEquals(1, Length(NewNameParts));
  AssertEquals('TOriginalType', NewNameParts[0]);

  // NewNameParts := StripNamePart(NewNameParts); // not allowed to call StripNamePart when only 1 part remains
end;

initialization
  RegisterTest(TTestPasDocTypes);
end.
