{
  Copyright 1998-2014 PasDoc developers.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Compatibility unit providing various utilities from
  standard StrUtils unit to Delphi 5.

  This unit should be compilable only with Delphi 5.
  When using Delphi 5, you should add this unit to your unit search
  path, and then pasdoc should compile "out of the box".
  When compiling pasdoc with newer compilers, you should just use
  StrUtils provided by them.
  
  It seems that many people are committed to keeping Delphi 5
  compatibility of pasdoc sources --- OK, so you will have to maintain 
  this unit and make sure that it works with Delphi 5 and allows compiling
  pasdoc with Delphi 5.

  Many bits in this unit may be copied from FPC StrUtils.pp
  implementation (but of course you should make sure that given
  implementation compiles and works with Delphi 5).
}
unit StrUtils;

interface

Function IfThen(AValue: Boolean; const ATrue: string; AFalse: string): string; overload;
Function IfThen(AValue: Boolean; const ATrue: string): string; overload; // ; AFalse: string = ''

Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer; overload;
Function PosEx(const SubStr, S: string): Integer; overload; // Offset: Cardinal = 1
Function PosEx(c:char; const S: string; Offset: Cardinal): Integer; overload;

Function DupeString(const AText: string; ACount: Integer): string;

implementation

uses SysUtils;

Function IfThen(AValue: Boolean; const ATrue: string; AFalse: string): string;

begin
  if avalue then
    result:=atrue
  else
    result:=afalse;
end;

Function IfThen(AValue: Boolean; const ATrue: string): string; // ; AFalse: string = ''

begin
  if avalue then
    result:=atrue
  else
    result:='';
end;

Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;

var i : pchar;
begin
  if (offset<1) or (offset>length(s)) then
  begin 
    Result := 0; 
    Exit 
  end;
  
  i:=strpos(@s[offset],@substr[1]);
  if i=nil then
    PosEx:=0
  else
    PosEx:=succ(i-pchar(s));
end;


Function PosEx(const SubStr, S: string): Integer; // Offset: Cardinal = 1

begin
  posex:=posex(substr,s,1);
end;

Function PosEx(c:char; const S: string; Offset: Cardinal): Integer;

var l : longint;
begin
  if (offset<1) or (offset>length(s)) then 
  begin 
    Result := 0; 
    Exit 
  end;
  
  l:=length(s);

  while (offset<=l) and (s[offset]<>c) do inc(offset);
  if offset>l then
   posex:=0
  else
   posex:=offset;
end;

Function DupeString(const AText: string; ACount: Integer): string;

var i,l : integer;

begin
 result:='';
 if aCount>=0 then
   begin
     l:=length(atext);
     SetLength(result,aCount*l);
     for i:=0 to ACount-1 do
       move(atext[1],Result[l*i+1],l);
   end;
end;

end.