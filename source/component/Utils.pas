{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @abstract(Some utility functions)
  This unit contains some utility functions for string handling
}
unit Utils;

interface

{$I DEFINES.INC}

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
procedure TrimCompress(var AString: string);
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

{ 1. AdjustLineBreaks in d

  2. Trim any whitespaces (also newline characters, #10 and #13) 
     at the beginning and end of d

  3. Finally replace each sequence of blank lines
     (i.e. LineEnding + some optional spaces/tabs + LineEnding
     + some optional LineEndings and spaces/tabs)
     by one Paragraph string. }
procedure InsertParagraphs(var d:string; const Paragraph:string);

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

procedure TrimCompress(var AString: string);
begin
  AString := Trim(AString);
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

procedure InsertParagraphs(var d:string; const Paragraph:string);
const
  { Note that these WhiteSpaces include all possible newline characters }
  WhiteSpaces = [#10, #13, ' ', #9];
var
  BeginPos, EndPos, Done, LineEndingPos, I:Integer;
  TempString: string;
  LenD: integer;
begin
  d := AdjustLineBreaks(d);
  
  { Code below does work assuming that Length(d)>=1. 
    So Exit now if Length(d) = 0. }
  if d = '' then Exit;
  
  { Eliminate whitespace at beginning and ending of d }
  BeginPos := 1;
  while d[BeginPos] in WhiteSpaces do Inc(BeginPos);
  EndPos := Length(d);
  while d[  EndPos] in WhiteSpaces do Dec(  EndPos);
  if (BeginPos > 1) or (EndPos < Length(d)) then
  begin
    if BeginPos<=EndPos then
      d := Copy(d, BeginPos, EndPos - BeginPos + 1) else
      d := '';
  end;
  
  { Look for a sequences of blank lines. 
    Blank line is line filled only with spaces (' ') or tabs (#9), 
    in particular empty line is also a blank line.
    Each sequence of blank lines will be replaced by Paragraph string. }
  { Variable Done says "how many beginning chars from D are already done,
    i.e. blank line sequences in D[1..Done-1] are already converted 
    to Paragraphs" }
  Done := 1;
  LenD := Length(d);
  while Done<=LenD do
  begin
    TempString:=Copy(d, Done, LenD);
    LineEndingPos := Pos(LineEnding, TempString);
{    LineEndingPos:=PosEx(LineEnding,d,Done); }
    if LineEndingPos = 0 then Break;

    Inc(LineEndingPos, Done-1);

    I := LineEndingPos + Length(LineEnding);
    while (I<=LenD) and (d[I] in [' ', #9]) do Inc(i);
    if I > LenD then Break;

    if Copy(d, I, Length(LineEnding)) = LineEnding then
    begin
      { Then bingo ! We should make a paragraph here, and ignore all
        subsequent white chars (spaces, tabs, and newline characters). }
      while (I<=LenD) and (d[I] in WhiteSpaces) do Inc(i);

      Delete(d, LineEndingPos, I-LineEndingPos);
      Insert(Paragraph, d, LineEndingPos);

      Done := Done + Length(Paragraph) + 1;
      LenD := Length(d);
    end else
    begin
      { Then it's just a usual newline (possibly followed by some
        whitespaces). We can leave it as it is. }
      Done := I;
    end;
  end;
end;

end.
