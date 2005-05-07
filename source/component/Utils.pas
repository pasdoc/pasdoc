{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @abstract(Some utility functions)
  This unit contains some utility functions for string handling
}
unit Utils;

{$I DEFINES.INC}

interface

uses PasDoc_Types;

{$IFNDEF DELPHI_6_UP}
type
  TMethod = record
    code, data: Pointer;
  end;
{$ENDIF}

{$IFNDEF DELPHI_6_UP}
{$IFNDEF FPC}
{$IFNDEF LINUX}
const
  DirectorySeparator = '\';
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ string empty means it contains only whitespace }
function IsStrEmptyA(const AString: string): boolean;
{ trim compress - only trims right now, TODO: compress whitespace }
function TrimCompress(const AString: string): string;
{ count occurences of AChar in AString }
function StrCountCharA(const AString: string; const AChar: Char): Integer;
{ Position of the ASub in AString. Return 0 if not found }
function StrPosIA(const ASub, AString: string): Integer;
{ loads a file into a string }
function LoadStrFromFileA(const AFile: string; var AStr: string): boolean;
{ creates a "method pointer" }
function MakeMethod(const AObject: Pointer; AMethod: Pointer): TMethod;

{$IFNDEF DELPHI_6_UP}
{$IFNDEF KYLIX}
function IncludeTrailingPathDelimiter(const S: string): string;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
function SameText(const A, B: string): boolean;
function DirectoryExists(const name: string): boolean;
{$ENDIF}

{$ifndef FPC}
const
  LineEnding = {$ifdef LINUX} #10 {$endif}
               {$ifdef MSWINDOWS} #13#10 {$endif};
{$endif}

{$ifdef VER1_0}
{ This is only for compatibity with FPC 1.0.x.
  FPC 1.9.x and Delphi/Kylix have this is StrUtils unit.
  Implementation of this is copied from FPC 1.9.9 RTL sources. }
Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;
{$endif}

type
  TCharReplacement = 
  record
    cChar: Char;
    sSpec: string;
  end;

{ Returns S with each char from ReplacementArray[].cChar replaced
  with ReplacementArray[].sSpec. }
function StringReplaceChars(const S: string; 
  const ReplacementArray: array of TCharReplacement): string;

{ Comfortable shortcut for Index <= Length(S) and S[Index] = C. }
function SCharIs(const S: string; Index: integer; C: char): boolean; overload;
{ Comfortable shortcut for Index <= Length(S) and S[Index] in Chars. }
function SCharIs(const S: string; Index: integer; 
  const Chars: TCharSet): boolean; overload;

const
  { Whitespace that is not any part of newline. }
  WhiteSpaceNotNL = [' ', #9];
  { Whitespace that is some part of newline. }
  WhiteSpaceNL = [#10, #13];
  { Any whitespace (that may indicate newline or not) }
  WhiteSpace = WhiteSpaceNotNL + WhiteSpaceNL;

implementation
uses
  SysUtils,
  Classes;

{$IFDEF FPC}
function SameText(const A, B: string): boolean;
var
  i: Integer;
begin  
  Result := Length(A) = Length(B);
  if Result then begin
    for i := 1 to Length(A) do begin
      if LowerCase(A[i]) <> LowerCase(B[i]) then begin
        Result := false; break;
      end;
    end;
  end;
end;
{$ENDIF}

function StrCompIA(const A, B: string): Integer;
begin
  Result := CompareText(A, B);
end;

function IsStrEmptyA(const AString: string): boolean;
begin
  Result := Length(Trim(AString)) = 0;
end;

function TrimCompress(const AString: string): string;
begin
  Result := Trim(AString);
end;

function StrCountCharA(const AString: string; const AChar: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(AString) downto 1 do begin
    if AString[i] = AChar then Inc(Result);
  end;
end;

function StrPosIA(const ASub, AString: string): Integer;
begin
  Result := Pos(LowerCase(ASub), LowerCase(AString))
end;

function LoadStrFromFileA(const AFile: string; var AStr: string): boolean;
var
  str: TFileStream;
  strstr: TStringStream;
begin
  Result := FileExists(AFile);
  strstr := tstringstream.create('');
  str := nil;
  if Result then try
    str := TFileStream.Create(AFile, fmOpenRead);
    strstr.CopyFrom(str, 0);
  except
    Result := false;
  end;
  str.Free;
  AStr := strstr.DataString;
  strstr.free;
end;

function MakeMethod(const AObject: Pointer; AMethod: Pointer): TMethod;
begin
  Result.Code := AMethod;
  Result.Data := AObject;
end;

{$IFNDEF DELPHI_6_UP}
{$IFNDEF KYLIX}
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Length(S)>0 then begin
    if S[Length(S)] <> DirectorySeparator then begin
      Result := S + DirectorySeparator;
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF FPC}
function DirectoryExists(const name: string): boolean;
begin
  Result := FileGetAttr(name) or faAnyFile <> 0;
end;
{$ENDIF}

{$ifdef VER1_0}
Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;

var i : pchar;
begin
  if (offset<1) or (offset>length(s)) then exit(0);
  i:=strpos(@s[offset],@substr[1]);
  if i=nil then
    PosEx:=0
  else
    PosEx:=succ(i-pchar(s));
end;

Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

var l : longint;
begin
  if (offset<1) or (offset>length(s)) then exit(0);
  l:=length(s);
{$ifndef useindexbyte}
  while (offset<=l) and (s[offset]<>c) do inc(offset);
  if offset>l then
   posex:=0
  else
   posex:=offset;
{$else}
  posex:=offset+indexbyte(s[offset],l-offset+1);
  if posex=(offset-1) then
    posex:=0;
{$endif}
end;
{$endif}

function StringReplaceChars(const S: string; 
  const ReplacementArray: array of TCharReplacement): string;

  function Replacement(const Special: Char): String;
  var
    i: Integer;
  begin
    for i := 0 to High(ReplacementArray) do
      with ReplacementArray[i] do
        if cChar = Special then
        begin
          Result := sSpec;
          Exit;
        end;
    Result := Special;
  end;

var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + Replacement(S[i]);
  end;
end;

function SCharIs(const S: string; Index: integer; C: char): boolean; overload;
begin
  Result := (Index <= Length(S)) and (S[Index] = C);
end;

function SCharIs(const S: string; Index: integer; 
  const Chars: TCharSet): boolean; overload;
begin
  Result := (Index <= Length(S)) and (S[Index] in Chars);
end;

end.
