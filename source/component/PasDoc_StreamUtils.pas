{
  Copyright 1998-2026 PasDoc developers.

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

{
  @abstract(A few stream utility functions.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Arno Garrels <first name.name@nospamgmx.de> (TBufferedStream and more))
  @author(Michalis Kamburelis)
}
unit PasDoc_StreamUtils;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils, Classes,
  {$ifdef FPC} {$ifdef USE_BUFFERED_STREAM} BufStream, {$endif} {$endif}
  PasDoc_Types;

type
  TBufferedFileStream =
    {$ifdef USE_BUFFERED_STREAM}
      {$ifdef FPC} BufStream.TBufferedFileStream
      {$else} Classes.TBufferedFileStream
      {$endif}
    {$else}
      TFileStream
    {$endif};

{$ifndef STRING_UNICODE}
{ Read next line from AStream, return it as AnsiString.
  Line ending is included in the result. }
function StreamReadLine(const AStream: TStream): AnsiString;

{ Write AString contents, then LineEnding to AStream. }
procedure StreamWriteLine(const AStream: TStream; const AString: AnsiString);

{ Just write AString contents to AStream. }
procedure StreamWriteString(const AStream: TStream; const AString: AnsiString);
{$endif}

implementation

uses PasDoc_Utils; // for LineEnding in Delphi

{$ifndef STRING_UNICODE}

function StreamReadLine(const AStream: TStream): AnsiString;
{ Simple implementation that reads one byte at a time.
  This is actually OK, since we use TBufferedFileStream throughout
  the codebase. }
var
  c: AnsiChar;
  l: Integer;
begin
  l := 0;
  SetLength(Result, 100);
  c := #0;
  while (AStream.Position < AStream.Size) and (c <> #13) and (c <> #10) do
  begin
    AStream.Read(c, 1);
    Inc(l);
    if l > Length(Result) then SetLength(Result, Length(Result) + 100);
    Result[l] := c;
  end;
  if (c = #13) then begin
    AStream.Read(c, 1);
    if c <> #10 then begin
      AStream.Seek(-1, soFromCurrent);
    end;
  end;
  SetLength(Result, l);
end;

procedure StreamWriteLine(const AStream: TStream; const AString: AnsiString);
var
 LineTerminator: AnsiString;
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
  LineTerminator := LineEnding;
  AStream.Write(LineTerminator[1], Length(LineTerminator));
end;

procedure StreamWriteString(const AStream: TStream; const AString: AnsiString);
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
end;

{$endif}

end.
