{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @abstract(Some utility functions)
}
unit Utils;

{$I DEFINES.INC}

interface

uses PasDoc_Types;

{ TMethod is not defined for FPC 1.0.x and Delphi < 6, so we have to define 
  it here. }

{$define GOT_TMETHOD}
{$ifdef VER1_0} {$undef GOT_TMETHOD} {$endif}
{$ifndef FPC} {$ifndef DELPHI_6_UP} {$undef GOT_TMETHOD} {$endif} {$endif}

{$ifndef GOT_TMETHOD}
type
  TMethod = record
    code, data: Pointer;
  end;
{$endif}

{$ifndef DELPHI_6_UP}
{$ifndef FPC}
{$ifndef LINUX}
const
  DirectorySeparator = '\';
{$endif}
{$endif}
{$endif}

{ string empty means it contains only whitespace }
function IsStrEmptyA(const AString: string): boolean;
{ count occurences of AChar in AString }
function StrCountCharA(const AString: string; const AChar: Char): Integer;
{ Position of the ASub in AString. Return 0 if not found }
function StrPosIA(const ASub, AString: string): Integer;
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

{ Extracts all characters up to the first white-space encountered
  (ignoring white-space at the very beginning of the string)
  from the string specified by S. 
  
  If there is no white-space in S (or there is white-space
  only at the beginning of S, in which case it is ignored)
  then the whole S is regarded as it's first word.
  
  Both S and result are trimmed, i.e. they don't have any
  excessive white-space at the beginning or end. }
function ExtractFirstWord(var s: string): string; overload;

{ Another version of ExtractFirstWord.

  Splits S by it's first white-space (ignoring white-space at the 
  very beginning of the string). No such white-space means that
  whole S is regarded as the FirstWord.
  
  Both FirstWord and Rest are trimmed. }
procedure ExtractFirstWord(const S: string; 
  out FirstWord, Rest: string); overload;

const
  { Whitespace that is not any part of newline. }
  WhiteSpaceNotNL = [' ', #9];
  { Whitespace that is some part of newline. }
  WhiteSpaceNL = [#10, #13];
  { Any whitespace (that may indicate newline or not) }
  WhiteSpace = WhiteSpaceNotNL + WhiteSpaceNL;

function FileToString(const FileName: string): string;
procedure StringToFile(const FileName, S: string);
procedure DataToFile(const FileName: string; const Data: array of Byte);

{ Returns S with all Chars replaced by ReplacementChar }
function SCharsReplace(const S: string; const Chars: TCharSet; 
  ReplacementChar: char): string;

procedure CopyFile(const SourceFileName, DestinationFileName: string);

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

function IsStrEmptyA(const AString: string): boolean;
begin
  Result := Length(Trim(AString)) = 0;
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

function ExtractFirstWord(var S: String): String;
var
  Len: Integer;
  StartPos: Integer;
  EndPos: Integer;
begin
  StartPos := 1;
  Len := Length(S);

  while (StartPos <= Len) and (S[StartPos] in WhiteSpace) do
    Inc(StartPos);

  if StartPos <= Len then
  begin
    EndPos := StartPos + 1;
    while (EndPos <= Len) and not (S[EndPos] in WhiteSpace) do
      Inc(EndPos);

    Result := Copy(S, StartPos, EndPos - StartPos);
    S := Trim(Copy(S, EndPos, Len));
  end else
  begin
    { S is only whitespaces }
    Result := '';
    S := '';
  end;
end;

procedure ExtractFirstWord(const S: string; out FirstWord, Rest: string);
begin
  Rest := S;
  FirstWord := ExtractFirstWord(Rest);
end;

function FileToString(const FileName: string): string;
var F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, F.Size);
    F.ReadBuffer(Pointer(Result)^, F.Size);
  finally F.Free end;
end;

procedure StringToFile(const FileName, S: string);
var F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    F.WriteBuffer(Pointer(S)^, Length(S));
  finally F.Free end;
end;

procedure DataToFile(const FileName: string; const Data: array of Byte);
var F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    F.WriteBuffer(Data, High(Data) + 1);
  finally F.Free end;
end;

function SCharsReplace(const S: string; const Chars: TCharSet; 
  ReplacementChar: char): string;
var 
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if Result[i] in Chars then
      Result[i] := ReplacementChar;
end;

procedure CopyFile(const SourceFileName, DestinationFileName: string);
var Source, Destination: TFileStream;
begin
  Destination := TFileStream.Create(DestinationFileName, fmCreate);
  try
    Source := TFileStream.Create(SourceFileName, fmOpenRead);
    try
      Destination.CopyFrom(Source, Source.Size);
    finally Source.Free end;
  finally Destination.Free end;
end;

end.
