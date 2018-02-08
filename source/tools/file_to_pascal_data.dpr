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

{ Reads file $1 and writes Pascal source file $2,
  such that $2 file contains type and value of Pascal constant, i.e.
    array[0 .. Xxx]of Byte = ( ... )
  that has the contents of file $1.

  File is treated as binary file, so file contents are reflected exactly,
  byte by byte, in $2.

  Useful if you want to "embed" some binary file inside compiled
  Pascal program. }

program file_to_pascal_data;

{$apptype CONSOLE}
{$ifdef FPC} {$mode objfpc} {$endif}
{$H+}

uses SysUtils, Classes;

var
  Src: TFileStream;
  Dest: TextFile;
  SrcFileName, DestFileName: string;
  B: Byte;
  i: Integer;
begin
  SrcFileName := ParamStr(1);
  DestFileName := ParamStr(2);

  Src := TFileStream.Create(SrcFileName, fmOpenRead);
  try
    Assign(Dest, DestFileName);
    Rewrite(Dest);
    try
      Writeln(Dest, '{ -*- buffer-read-only: t -*- }');
      Writeln(Dest, '{ DON''T EDIT -- this file was automatically generated from "'
        + SrcFileName + '" }');

      Writeln(Dest, 'array [0 .. ', Src.Size - 1, '] of Byte = (');

      for i := 0 to Src.Size - 2 do
      begin
        Src.ReadBuffer(B, 1);
        Write(Dest, '$', IntToHex(B, 2), ', ');
        if ((i + 1) mod 15) = 0 then
          Writeln(Dest);
      end;

      Src.ReadBuffer(B, 1);
      Write(Dest, '$', IntToHex(B, 2));
      Writeln(Dest, ')');
    finally CloseFile(Dest) end;
  finally Src.Free end;
end.
