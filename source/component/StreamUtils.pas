{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a few stream utility functions
}
unit StreamUtils;

interface

uses
  Classes;
  
{$I VERSIONS.INC}  

function StreamReadLine(const AStream: TStream): string;
procedure WriteLine(const AStream: TStream; const AString: string);
procedure WriteString(const AStream: TStream; const AString: string);

implementation
uses
  Utils; // for LineEnding in Kylix/Delphi

function StreamReadLine(const AStream: TStream): string;
// totally junky implementation!!
var
  c: Char;
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

procedure WriteLine(const AStream: TStream; const AString: string);
var
 LineTerminator: string;
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
  LineTerminator := LineEnding;
  AStream.Write(LineTerminator[1], Length(LineTerminator));
end;

procedure WriteString(const AStream: TStream; const AString: string);
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
end;

end.
