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
  SysUtils, StrUtils, Types;

type
{$IFNDEF COMPILER_11_UP}
  TBytes = array of Byte;
{$ENDIF}
{$IFNDEF COMPILER_12_UP}
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$ENDIF}

{$IFNDEF PASDOC} // avoid PasDoc from trying to evaluate "NOT DECLARED"
  {$IF NOT DECLARED(TStringDynArray)}
    TStringDynArray = array of string;
  {$ENDIF}
{$ENDIF}

  TStringArray = TStringDynArray;
  { This represents parts of a qualified name of some item.

    User supplies such name by separating each part with dot,
    e.g. 'UnitName.ClassName.ProcedureName', then @link(SplitNameParts)
    converts it to TNameParts like
    ['UnitName', 'ClassName', 'ProcedureName'].
    Length must be @italic(always) between 1 and @link(MaxNameParts). }
  TNameParts = TStringArray;

  { }
  TPasDocMessageType = (pmtPlainText, pmtInformation, pmtWarning, pmtError);
  { }
  TPasDocMessageEvent = procedure(const MessageType: TPasDocMessageType; const
    AMessage: string; const AVerbosity: Cardinal) of object;

  TCharSet = set of AnsiChar;

  { Exception raised in many situations when PasDoc encounters an error. }
  EPasDoc = class(Exception)
  public
    constructor Create(const AMessageFormat: string;
      const AArguments: array of const;
      const AExitCode: Word = 3); overload;
    constructor Create(const AMessage: string;
      const AExitCode: Word = 3); overload;
  end;


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

{ Splits S, which can be made of any number of parts, separated by dots
  (Delphi namespaces, like PasDoc.Output.HTML.TWriter.Write).
  If S is not a valid identifier, @false is returned, otherwise @true is returned
  and splitted name is returned as NameParts. }
function SplitNameParts(S: string; out NameParts: TNameParts): Boolean;

{ Simply returns an array with Length = 1 and one item = S. }
function OneNamePart(const S: string): TNameParts;

{ Simply concatenates all NameParts with dot. }
function GlueNameParts(const NameParts: TNameParts): string;

type
  { See command-line option @--implicit-visibility documentation at
    [https://github.com/pasdoc/pasdoc/wiki/ImplicitVisibilityOption] }
  TImplicitVisibility = (ivPublic, ivPublished, ivImplicit);

implementation

{ EPasDoc -------------------------------------------------------------------- }

constructor EPasDoc.Create(const AMessageFormat: string;
  const AArguments: array of const; const AExitCode: Word);
begin
  ExitCode := AExitCode;
  inherited CreateFmt(AMessageFormat, AArguments);
end;

constructor EPasDoc.Create(const AMessage: string; const AExitCode: Word);
begin
  ExitCode := AExitCode;
  inherited Create(AMessage);
end;

{ global routines ------------------------------------------------------------ }

{$IF NOT DECLARED(SplitString)}
// Primitive implementation for ancient compilers, uses only 1st char of Delimiters
function SplitString(const S, Delimiters: string): TStringArray;
var
  DelimCount, PrevDelimPos, DelimPos, i: Integer;
begin
  // Count delims in name and set length of array
  DelimCount := 0;
  i := 0;
  repeat
    i := PosEx(Delimiters[1], s, i + 1);
    if i > 0 then
      Inc(DelimCount);
  until i = 0;
  SetLength(Result, DelimCount + 1);

  // no delims - simple case
  if DelimCount = 0 then
  begin
    Result[0] := s;
    Exit;
  end;

  PrevDelimPos := 1;
  for i := 0 to High(Result) do
  begin
    DelimPos := PosEx(Delimiters[1], s, PrevDelimPos);
    if DelimPos = 0 then // last delim in the string
    begin
      Result[i] := Copy(s, PrevDelimPos, MaxInt);
      Break;
    end;
    Result[i] := Copy(s, PrevDelimPos, DelimPos - PrevDelimPos);
    PrevDelimPos := DelimPos + 1;
  end;
end;
{$ENDIF}

function SplitNameParts(S: string;
  out NameParts: TNameParts): Boolean;

const
  { set of characters, including all letters and the underscore }
  IdentifierStart : TCharSet = ['A'..'Z', 'a'..'z', '_'];

  { set of characters, including all characters from @link(IdentifierStart)
    plus the ten decimal digits }
  IdentifierOther : TCharSet = ['A'..'Z', 'a'..'z', '_', '0'..'9', '.', ','];
var
  i: Integer;
  Depth: Integer;
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
  Depth := 0;
  while (i <= Length(s)) do begin
    if s[i] = '(' then
      Inc(Depth)
    else if s[i] = ')' then
      Dec(Depth)
{$IFNDEF COMPILER_12_UP}
    else if (Depth = 0) and (not (s[i] in IdentifierOther)) then Exit;
{$ELSE}
    else if (Depth = 0) and (not CharInSet(s[i], IdentifierOther)) then Exit;
{$ENDIF}
    Inc(i);
  end;

  NameParts := SplitString(s, '.');
  Result := True;
end;

function OneNamePart(const S: string): TNameParts;
begin
  Initialize(Result);
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
