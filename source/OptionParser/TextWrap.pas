{ *********************************************************************** }
{                                                                         }
{ Delphi / Kylix Cross-Platform Runtime Library                           }
{ Parts of The System Utilities Unit                                      }
{ This file (TextWrap.pas) contains parts of SysUtils.pas as distributed  }
{ by Borland with Kylix 3                                                 }
{                                                                         }
{ Copyright (c) 1995-2002 Borland Softwrare Corporation                   }
{                                                                         }
{  This file may be distributed and/or modified under the terms of the    }
{  GNU General Public License version 2 as published by the Free Software }
{  Foundation and appearing at http://www.borland.com/kylix/gpl.html.     }
{                                                                         }
{ *********************************************************************** }
unit TextWrap;
interface

type
  TSysCharSet = set of char;

{ WrapText will scan a string for BreakChars and insert the BreakStr at the
  last BreakChar position before MaxCol.  Will not insert a break into an
  embedded quoted string (both ''' and '"' supported) }

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;
  MaxCol: Integer): string; overload;
function WrapText(const Line: string; MaxCol: Integer): string; overload;

implementation
uses
  SysUtils;

const
  sLineBreak = {$IFDEF LINUX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF};
  
{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
var
  LeadBytes: set of Char = [];

function CharLength(const S: string; Index: Integer): Integer;
begin
  Result := 1;
end;

function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;
  MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  L: Integer;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := #0;
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar in LeadBytes then
    begin
      L := CharLength(Line, Pos) - 1;
      Inc(Pos, L);
      Inc(Col, L);
    end
    else
    begin
      if CurChar in QuoteChars then
        if QuoteChar = #0 then
          QuoteChar := CurChar
        else if CurChar = QuoteChar then
          QuoteChar := #0;
      if QuoteChar = #0 then   
      begin
        if CurChar = BreakStr[1] then
        begin
          ExistingBreak := StrLComp(Pointer(BreakStr), Pointer(@Line[Pos]), BreakLen) = 0;
          if ExistingBreak then
          begin
            Inc(Pos, BreakLen-1);
            BreakPos := Pos;
          end;
        end;
       
        if not ExistingBreak then
          if CurChar in BreakChars then
            BreakPos := Pos;
      end;
    end;

    Inc(Pos);
    Inc(Col);

    if not (QuoteChar in QuoteChars) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := 1;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (CurChar in QuoteChars) then
      begin
        while Pos <= LineLen do
        begin
          if Line[Pos] in BreakChars then
          begin
            Inc(Pos);
            ExistingBreak := False;
          end
          else
          begin
            ExistingBreak := StrLComp(Pointer(@Line[Pos]), sLineBreak, Length(sLineBreak)) = 0;
            if ExistingBreak then
              Inc(Pos, Length(sLineBreak))
            else
              Break;
          end;
        end;
      end;
      if (Pos <= LineLen) and not ExistingBreak then
        Result := Result + BreakStr;

      Inc(BreakPos);
      LinePos := BreakPos;
      Pos := LinePos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function WrapText(const Line: string; MaxCol: Integer): string;
begin
  Result := WrapText(Line, sLineBreak, [' ', '-', #9], MaxCol); { do not localize }
end;


end.

