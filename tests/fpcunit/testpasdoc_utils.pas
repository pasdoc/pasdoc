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
    procedure TestSwap16Buf;
    procedure TestStripHtml;
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

procedure TTestPasDocUtils.TestSwap16Buf;
var
  WordArray: array [1..3] of Word;
begin
  WordArray[1] := 1 shl 8 + 2;
  WordArray[2] := 3 shl 8 + 4;
  WordArray[3] := 5 shl 8 + 6;
  Swap16Buf(@WordArray, @WordArray, High(WordArray));
  AssertEquals(2 shl 8 + 1, WordArray[1]);
  AssertEquals(4 shl 8 + 3, WordArray[2]);
  AssertEquals(6 shl 8 + 5, WordArray[3]);
end;

procedure TTestPasDocUtils.TestStripHtml;
begin
  AssertEquals(' blah   blah ' + LineEnding + ' foo',
     StripHtml(' blah   blah ' + LineEnding + ' foo'));
  AssertEquals(' blah   blah ' + LineEnding + ' foo',
     StripHtml(' <i>blah   <b>blah</b></i> <blabla>' + LineEnding + ' <table>foo'));
end;

initialization
  RegisterTest(TTestPasDocUtils);
end.
