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

{ @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)
  @abstract(Basic types.) }
unit PasDoc_Types;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils;

type
{$IFNDEF COMPILER_11_UP}
  TBytes = array of Byte;
{$ENDIF}
{$IFNDEF COMPILER_12_UP}
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$ENDIF}
  { }
  TPasDocMessageType = (pmtPlainText, pmtInformation, pmtWarning, pmtError);
  { }
  TPasDocMessageEvent = procedure(const MessageType: TPasDocMessageType; const
    AMessage: string; const AVerbosity: Cardinal) of object;

  TCharSet = set of AnsiChar;

{ }
  EPasDoc = class(Exception)
  public
    constructor Create(const AMessage: string;
      const AArguments: array of const; const AExitCode: Word = 3);
  end;

  { This represents parts of a qualified name of some item.

    User supplies such name by separating each part with dot,
    e.g. 'UnitName.ClassName.ProcedureName', then @link(SplitNameParts)
    converts it to TNameParts like
    ['UnitName', 'ClassName', 'ProcedureName'].
    Length must be @italic(always) between 1 and @link(MaxNameParts). }
  TNameParts = array of string;

const
  MaxNameParts = 3;
  { Windows Unicode code page ID }
  CP_UTF16      = 1200;
  CP_UTF16Be    = 1201;
  CP_UTF32      = 12000;
  CP_UTF32Be    = 12001;

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
  LineEnding    = #13#10;
{$ENDIF}
{$ENDIF}

{ Splits S, which can be made of up to three parts, separated by dots.
  If S is not a valid identifier or if it has more than
  three parts, false is returned, otherwise true is returned
  and splitted name is returned as NameParts. }
function SplitNameParts(S: string; out NameParts: TNameParts): Boolean;

{ Simply returns an array with Length = 1 and one item = S. }
function OneNamePart(S: string): TNameParts;

{ Simply concatenates all NameParts with dot. }
function GlueNameParts(const NameParts: TNameParts): string;

type
  { See command-line option @--implicit-visibility documentation at
    [https://github.com/pasdoc/pasdoc/wiki/ImplicitVisibilityOption] }
  TImplicitVisibility = (ivPublic, ivPublished, ivImplicit);

implementation

{ EPasDoc -------------------------------------------------------------------- }

constructor EPasDoc.Create(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
begin
  ExitCode := AExitCode;
  CreateFmt(AMessage, AArguments);
end;

{ global routines ------------------------------------------------------------ }

function SplitNameParts(S: string;
  out NameParts: TNameParts): Boolean;

const
  { set of characters, including all letters and the underscore }
  IdentifierStart : TCharSet = ['A'..'Z', 'a'..'z', '_'];

  { set of characters, including all characters from @link(IdentifierStart)
    plus the ten decimal digits }
  IdentifierOther : TCharSet = ['A'..'Z', 'a'..'z', '_', '0'..'9', '.'];

  procedure SplitInTwo(s: string; var S1, S2: string);
  var
    i: Integer;
  begin
    i := Pos('.', s);
    if (i = 0) then begin
      S1 := s;
      S2 := '';
    end
    else begin
      S1 := System.Copy(s, 1, i - 1);
      S2 := System.Copy(s, i + 1, Length(s));
    end;
  end;

var
  i: Integer;
  t: string;
begin
  Result := False;

  SetLength(NameParts, 3);

  S := Trim(S);

  { Check that S starts with IdentifierStart and
    then only IdentifierOther chars follow }
  if S = '' then Exit;
{$IFNDEF COMPILER_12_UP}
  if (not (s[1] in IdentifierStart)) then Exit;
{$ELSE}
  if not CharInSet(s[1], IdentifierStart) then Exit;
{$ENDIF}
  i := 2;
  while (i <= Length(s)) do begin
  {$IFNDEF COMPILER_12_UP}
    if (not (s[i] in IdentifierOther)) then Exit;
  {$ELSE}
    if not CharInSet(s[i], IdentifierOther) then Exit;
  {$ENDIF}
    Inc(i);
  end;

  SplitInTwo(S, NameParts[0], NameParts[1]);
  if NameParts[1] = '' then
  begin
    SetLength(NameParts, 1);
  end else
  begin
    t := NameParts[1];
    SplitInTwo(t, NameParts[1], NameParts[2]);
    if NameParts[2] = '' then
      SetLength(NameParts, 2) else
      SetLength(NameParts, 3);
  end;
  Result := True;
end;

function OneNamePart(S: string): TNameParts;
begin
  SetLength(Result, 1);
  Result[0] := S;
end;

function GlueNameParts(const NameParts: TNameParts): string;
var
  i: Integer;
begin
  Result := NameParts[0];
  for i := 1 to Length(NameParts) - 1 do
    Result := Result + '.' + NameParts[i];
end;

end.
