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
    { Dynamic array of String, for compatibility with compilers that don't define this. }
    TStringDynArray = array of string;
  {$ENDIF}
{$ENDIF}

  { Dynamic array of String. }
  TStringArray = TStringDynArray;

  { Qualified name of a Pascal item.

    User supplies such name by separating each part with dot,
    e.g. 'UnitName.ClassName.MethodName', then @link(SplitNameParts)
    converts it to TNameParts like
    ['UnitName', 'ClassName', 'MethodName'].

    Note that in case of unit names with dots, the item on this list
    may also contain a dot inside, if it's determined to be a name of unit with dot.
    Like this: ['Unit.Name.With.Dot', 'ClassName', 'MethodName'].
    We have special code to do this in TDocGenerator.FindGlobal .

    The idea is that each string corresponds to some TPasItem.Name.

    Use such list for searching routines, for @link(TPasItem.FindItem) and friends.

    This must @italic(always have at least one part).

    There is no limit on the maximum length of TNameParts, since we can have
    arbitrary number of parts with nested classes, like
    "MyUnit.TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType". }
  TNameParts = TStringArray;

  { Message type to send with @link(TPasDocMessageEvent). }
  TPasDocMessageType = (pmtPlainText, pmtInformation, pmtWarning, pmtError);

  { Callback to send a message from one class and react to it in another. }
  TPasDocMessageEvent = procedure(const MessageType: TPasDocMessageType; const
    AMessage: string; const AVerbosity: Cardinal) of object;

  { Set of AnsiChars. }
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

{$ifndef FPC}
const
  LineEnding = SLineBreak;
{$endif}

{ Splits S, which can be made of any number of parts, separated by dots
  (Delphi namespaces, like PasDoc.Output.HTML.TWriter.Write).
  If S is not a valid identifier, @false is returned, otherwise @true is returned
  and splitted name is returned as NameParts. }
function SplitNameParts(S: string; out NameParts: TNameParts): Boolean;

{ Strip one name part. Only call when we have at least 2 parts. }
function StripNamePart(const NameParts: TNameParts): TNameParts;

{ Checks that the string is a valid multipart identifier }
function IsValidMultipartName(S: string): boolean;

{ Simply returns an array with Length = 1 and one item = S.

  TODO: This function, and all its usage, should be removed.
  Reason: all identifiers across Pascal code are potentially qualified.
  Using this function right now means e.g. that using ancestor names with
  qualified identifiers, like "TMyClass = class(OtherUnit.TAncestor)",
  is not resolved fully (they are not linked everywhere they should be). }
function OneNamePart(const S: string): TNameParts;

{ Simply concatenates all NameParts with dot. }
function GlueNameParts(const NameParts: TNameParts): string;

type
  { See command-line option @--implicit-visibility documentation at
    @url(https://pasdoc.github.io/ImplicitVisibilityOption --implicit-visibility documentation). }
  TImplicitVisibility = (ivPublic, ivPublished, ivImplicit);

implementation

uses PasDoc_Tokenizer;

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

function SplitNameParts(S: string; out NameParts: TNameParts): Boolean;
var
  i: Integer;
  Depth: Integer;
begin
  Result := False;

  S := Trim(S);

  { Check that S starts with IsIdentifierStartChar and
    then only IsIdentifierOtherChar and dot chars follow.
    TODO: Document why we also allow arbitrary stuff in parentheses. }
  if S = '' then Exit;
  if not IsIdentifierStartChar(s[1]) then Exit;
  i := 2;
  Depth := 0;
  while (i <= Length(s)) do begin
    if s[i] = '(' then
      Inc(Depth)
    else
    if s[i] = ')' then
      Dec(Depth)
    else
    if (Depth = 0) and
       (not IsIdentifierOtherChar(s[i])) and
       (s[i] <> '.') then
      Exit;
    Inc(i);
  end;

  NameParts := SplitString(s, '.');
  Assert(Length(NameParts) >= 1);
  Result := True;
end;

function StripNamePart(const NameParts: TNameParts): TNameParts;
begin
  Assert(Length(NameParts) >= 2);
  // Note that NameParts is indexed from 0
  Result := Copy(NameParts, 1, Length(NameParts) - 1);
end;

function IsValidMultipartName(S: string): boolean;
var
  DummyParts: TNameParts;
begin
  result := SplitNameParts(S, DummyParts);
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
