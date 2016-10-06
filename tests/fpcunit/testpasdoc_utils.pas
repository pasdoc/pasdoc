{
  Copyright 2016-2016 PasDoc developers.

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

{ Test PasDoc_Utils. }
unit TestPasdoc_Utils;

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestPasDocUtils = class(TTestCase)
  published
    procedure TestRemoveIndentation;
  end;

implementation

uses PasDoc_Utils;

procedure TTestPasDocUtils.TestRemoveIndentation;
begin
  AssertEquals(
    '    ' + LineEnding + // whitespace line will be completely ignored
    ' foo' + LineEnding +
    'bar' + LineEnding +
    ' xyz',
    RemoveIndentation(
    '    ' + LineEnding + // whitespace line will be completely ignored
    '  foo' + LineEnding +
    ' bar' + LineEnding +
    '  xyz'));
  AssertEquals(
    '    '#9'  ' + LineEnding + // whitespace line will be completely ignored
    #9' foo' + LineEnding +
    'bar' + LineEnding +
    ' xyz',
    RemoveIndentation(
    '    '#9'  ' + LineEnding + // whitespace line will be completely ignored
    ' '#9#9' foo' + LineEnding +
    ' '#9'bar' + LineEnding +
    ' '#9' xyz'));
  AssertEquals('begin Writeln(''Hello world''); end; { This works ! }',
    RemoveIndentation(' begin Writeln(''Hello world''); end; { This works ! } '));
end;

initialization
  RegisterTest(TTestPasDocUtils);
end.
