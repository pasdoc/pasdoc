{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @abstract(Some utility functions)
}
unit PasDoc_Utils;

{$I pasdoc_defines.inc}

interface

uses SysUtils, PasDoc_Types;

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
  PathDelim = '\';
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

{$ifndef DELPHI_6_UP}
{$ifndef KYLIX}
{$ifndef FPC}
function IncludeTrailingPathDelimiter(const S: string): string;
function ExcludeTrailingPathDelimiter(const S: string): string;
{$endif}
{$endif}
{$endif}

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
  AllChars = [Low(Char)..High(Char)];
  
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

{$ifdef DELPHI_1_UP}
{ Default Delphi (under Windows) implementation of ExtractFilePath
  has a problem --- it doesn't treat '/' as a valid path delimiter
  under Windows (yes, it is valid path delimiter under Windows, just like '\').
  This is the fixed version (actually taken from FPC sources). }
function ExtractFilePath(const FileName: string): string;

{ Just like @link(ExtractFilePath), also default Delphi (under Windows) 
  implementation of ExtractFileName  is buggy. 
  This is the fixed version (actually taken from FPC sources). }
function ExtractFileName(const FileName: string): string;
{$endif}

{ Checks is Prefix a prefix of S. Not case-sensitive. }
function IsPrefix(const Prefix, S: string): boolean;

{$ifdef DELPHI_5}
{ BoolToStr for Delphi 5 compat.
  According to 
  [https://sourceforge.net/tracker/?func=detail&atid=104213&aid=1595890&group_id=4213]
  Delphi 5 RTL doesn't have this implemented. }
function BoolToStr(Value: Boolean): string;
{$endif DELPHI_5}

{ SEnding returns S contents starting from position P.
  Returns '' if P > length(S).
  Yes, this is simply equivalent to Copy(S, P, MaxInt). }
function SEnding(const s: string; P: integer): string;

{ Check is the given Path absolute.

  Path may point to directory or normal file,
  it doesn't matter. Also it doesn't matter whether Path ends with PathDelim or not.

  Note for Windows: while it's obvious that @code('c:\autoexec.bat') is an
  absolute path, and @code('autoexec.bat') is not, there's a question
  whether path like @code('\autoexec.bat') is absolute? It doesn't specify
  drive letter, but it does specify full directory hierarchy on some drive.
  This function treats this as @italic(not absolute), on the reasoning that
  "not all information is contained in Path".

  @seealso IsPathAbsoluteOnDrive }
function IsPathAbsolute(const Path: string): boolean;

{ Just like IsPathAbsolute, but on Windows accepts also paths that specify
  full directory tree without drive letter.

  @seealso IsPathAbsolute }
function IsPathAbsoluteOnDrive(const Path: string): boolean;

{ Combines BasePath with RelPath. BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath. }
function CombinePaths(BasePath, RelPath: string): string;

type
  { Raise this when some impossible situation (indicating bug in 
    pasdoc) occurs. }
  EInternalError = class(Exception)
    { Calls inherited with Message like
      'Internal error occured :' + BaseMessage,
      so BaseMessage should only contain text relevant to this
      particular internal error. }
    constructor Create(const BaseMessage: string);
  end;

implementation

uses
  Classes;

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

{$ifndef DELPHI_6_UP}
{$ifndef KYLIX}
{$ifndef FPC}
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Length(S)>0 then begin
    if S[Length(S)] <> PathDelim then begin
      Result := S + PathDelim;
    end;
  end;
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if (S <> '') and (S[Length(S)] in ['/', '\']) then
    SetLength(Result, Length(Result) - 1);
end;
{$endif}
{$endif}
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

{$ifdef DELPHI_1_UP}
function ExtractFilePath(const FileName: string): string;
var i: longint;
begin
i := Length(FileName);
while (i > 0) and not (FileName[i] in ['/', '\', ':']) do Dec(i);
If I>0 then
  Result := Copy(FileName, 1, i)
else
  Result:='';
end;

function ExtractFileName(const FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
Result := Copy(FileName, I + 1, 255);
end;
{$endif}

function IsPrefix(const Prefix, S: string): boolean;
begin
  Result := AnsiSameText(Copy(S, 1, Length(Prefix)), Prefix);
end;

{$ifdef DELPHI_5}
function BoolToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'TRUE' else
    Result := 'FALSE';
end;
{$endif}

function SEnding(const S: string; P: integer): string;
begin
 result := Copy(S, P, MaxInt)
end;

function IsPathAbsolute(const Path: string): boolean;
begin
  Result := {$ifdef UNIX} SCharIs(Path, 1, PathDelim) {$endif}
            {$ifdef MSWINDOWS} SCharIs(Path, 2, DriveDelim) {$endif};
end;

function IsPathAbsoluteOnDrive(const Path: string): boolean;
begin
  Result := IsPathAbsolute(Path)
    {$ifdef MSWINDOWS} or SCharIs(Path, 1, PathDelim) {$endif}
end;

function CombinePaths(BasePath, RelPath: string): string;
begin
  if IsPathAbsolute(RelPath) then
    result := RelPath else
  {$ifdef MSWINDOWS}
  if IsPathAbsoluteOnDrive(RelPath) then
    result := BasePath[1] +DriveDelim +RelPath else
  {$endif}
  begin
    repeat
      if (Copy(RelPath, 1, 2) = './')
        {$ifdef MSWINDOWS} or (Copy(RelPath, 1, 2) = '.\') {$endif} then
        RelPath := SEnding(RelPath, 3) else
      if (Copy(RelPath, 1, 3) = '../')
        {$ifdef MSWINDOWS} or (Copy(RelPath, 1, 3) = '..\') {$endif} then
      begin
        BasePath := ExtractFileDir(ExcludeTrailingPathDelimiter(BasePath));
        RelPath := SEnding(RelPath, 4);
      end else
        Break;
    until false;

    result := IncludeTrailingPathDelimiter(BasePath) + RelPath;
  end;
end;

{ EInternalError ------------------------------------------------------------- }

constructor EInternalError.Create(const BaseMessage: string);
begin
  inherited Create('Internal error occured : ' + BaseMessage);
end;

end.
