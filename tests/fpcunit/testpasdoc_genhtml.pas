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

{ Test PasDoc_GenHtml. }
unit TestPasdoc_GenHtml;

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestPasDocGenHtml = class(TTestCase)
  published
    procedure TestFormatPascalCode;
  end;

implementation

uses PasDoc_GenHtml;

type
  { Descendant of THTMLDocGenerator,
    to access here some protected methods of THTMLDocGenerator. }
  TMyHTMLDocGenerator = class(THTMLDocGenerator)
  end;

procedure TTestPasDocGenHtml.TestFormatPascalCode;
var
  Gen: TMyHTMLDocGenerator;
begin
  Gen := TMyHTMLDocGenerator.Create(nil);
  try
    AssertEquals('</p>' + LineEnding +
LineEnding +
'<pre class="longcode"><span class="pascal_keyword">begin</span> Writeln(<span class="pascal_string">''Hello world''</span>); <span class="pascal_keyword">end</span>; <span class="pascal_comment">{ This works ! }</span> </pre>' + LineEnding +
LineEnding +
'<p>', Gen.FormatPascalCode('begin Writeln(''Hello world''); end; { This works ! } '));

    { should work the same as before (there was a bug at some point where
      the lack of space right after comment was confusing the code) }
    AssertEquals('</p>' + LineEnding +
LineEnding +
'<pre class="longcode"><span class="pascal_keyword">begin</span> Writeln(<span class="pascal_string">''Hello world''</span>); <span class="pascal_keyword">end</span>; <span class="pascal_comment">{ This works ! }</span></pre>' + LineEnding +
LineEnding +
'<p>', Gen.FormatPascalCode('begin Writeln(''Hello world''); end; { This works ! }'));

    AssertEquals('</p>' + LineEnding +
LineEnding +
'<pre class="longcode"><span class="pascal_comment">{ unterminated </span></pre>' + LineEnding +
LineEnding +
'<p>', Gen.FormatPascalCode('{ unterminated '));

    AssertEquals('</p>' + LineEnding +
LineEnding +
'<pre class="longcode"><span class="pascal_comment">(* unterminated </span></pre>' + LineEnding +
LineEnding +
'<p>', Gen.FormatPascalCode('(* unterminated '));

    AssertEquals('</p>' + LineEnding +
LineEnding +
'<pre class="longcode"><span class="pascal_comment">// unterminated </span></pre>' + LineEnding +
LineEnding +
'<p>', Gen.FormatPascalCode('// unterminated '));
  finally FreeAndNil(Gen) end;
end;

initialization
  RegisterTest(TTestPasDocGenHtml);
end.
