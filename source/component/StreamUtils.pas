{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  a few stream utility functions
}
unit StreamUtils;

interface
uses
  Classes;

function StreamReadLine(const AStream: TStream): string;
procedure WriteLine(const AStream: TStream; const AString: string);
procedure WriteString(const AStream: TStream; const AString: string);

implementation

const
{$IFDEF LINUX}
  LINETERMINATOR: string = #10;
{$ENDIF}
{$IFDEF WIN32}
  LINETERMINATOR: string = #13#10;
{$ENDIF}

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
begin
  AStream.WriteBuffer(AString[1], Length(AString));
  AStream.Write(LINETERMINATOR[1], Length(LINETERMINATOR));
end;

procedure WriteString(const AStream: TStream; const AString: string);
begin
  AStream.WriteBuffer(AString[1], Length(AString));
end;

end.
