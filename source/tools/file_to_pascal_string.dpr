{
  Copyright 1998-2018 PasDoc developers.

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

{ Reads text file $1 and writes Pascal source file $2,
  such that $2 file contains Pascal string that has the contents
  of file $1.

  Useful if you want to "embed" some text file inside compiled
  Pascal program.

  Note it treats $1 as text file, so difference between various line endings
  is lost in $2 (line endings are always encoded using LineEnding constant). }

program file_to_pascal_string;

{$apptype CONSOLE}
{$ifdef FPC} {$mode objfpc} {$endif}
{$H+}

uses Math, SysUtils;

const
  { String constants are (at max) this long.
    This avoids http://sourceforge.net/p/pasdoc/bugs/82/ problem
    with Delphi XE compilation like
    [DCC Fatal Error] jquery-1.7.1.min.js.inc(4): F2069 Line too long (more than 1023 characters) }
  MaxStringLength = 1000;
var
  Src, Dest: TextFile;
  SrcFileName, DestFileName, S, Next: string;
  LenProcessed, LenNext: Cardinal;
begin
  SrcFileName := ParamStr(1);
  DestFileName := ParamStr(2);

  Assign(Src, SrcFileName);
  Reset(Src);
  try
    Assign(Dest, DestFileName);
    Rewrite(Dest);
    try
      Writeln(Dest, '{ -*- buffer-read-only: t -*- }');
      Writeln(Dest, '{ DON''T EDIT -- this file was automatically generated from "'
        + SrcFileName + '" }');

      while not Eof(Src) do
      begin
        Readln(Src, S);

        { Split S into MaxStringLength chunks }
        if Length(S) <= MaxStringLength then
        begin
          { Special case when S already fits in MaxStringLength.
            We could allow the loop below to handle any S with Length(S) <> 0,
            the output would be correct, but for humans it looks better when
            we output ' LineEnding + ' on the same line. }
          S := StringReplace(S, '''', '''''', [rfReplaceAll]);
          Writeln(Dest, '''' + S + ''' + LineEnding +');
        end else
        begin
          LenProcessed := 0;
          while LenProcessed < Length(S) do
          begin
            LenNext := Min(Length(S) - LenProcessed, MaxStringLength);
            Next := Copy(S, LenProcessed + 1, LenNext);
            Next := StringReplace(Next, '''', '''''', [rfReplaceAll]);
            Writeln(Dest, '''' + Next + ''' + ');
            LenProcessed := LenProcessed + LenNext;
          end;
          Writeln(Dest, 'LineEnding +');
        end;
      end;

      Writeln(Dest, '''''');
    finally CloseFile(Dest) end;
  finally CloseFile(Src) end;
end.