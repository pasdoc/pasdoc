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

{ @abstract(Talking with another process through pipes.)
  @author(Michalis Kamburelis) 
  @author(Arno Garrels <first name.name@nospamgmx.de>)}

unit PasDoc_ProcessLineTalk;

{$I pasdoc_defines.inc}

{$ifdef FPC}
{$IFNDEF VER1_0}
  {$ifdef UNIX}
    {$define HAS_PROCESS}
  {$endif}
  {$ifdef WIN32}
    {$define HAS_PROCESS}
  {$endif}
{$ELSE}  
  {$ifdef LINUX}
    {$define HAS_PROCESS}
  {$endif}
  {$ifdef BSD}
    {$define HAS_PROCESS}
  {$endif}
  {$ifdef WIN32}
    {$define HAS_PROCESS}
  {$endif}
{$ENDIF}  
{$endif}

interface

uses SysUtils, Classes {$ifdef HAS_PROCESS} , Process {$endif};

type
  { TTextReader reads given Stream line by line.
    Lines may be terminated in Stream with #13, #10, #13+#10 or #10+#13.
    This way I can treat any TStream quite like standard Pascal text files:
    I have simple Readln method.

    After calling Readln or Eof you should STOP directly using underlying
    Stream (but you CAN use Stream right after creating
    TTextReader.Create(Stream) and before any Readln or Eof
    operations on this TTextReader).

    Original version of this class comes from Michalis Kamburelis
    code library, see [http://www.camelot.homedns.org/~michalis/],
    unit base/KambiClassUtils.pas. }

  TTextReader = class
  private
    Stream: TStream;
    ReadBuf: string;
    FOwnsStream: boolean;
    { This is either #0 or #10 (tells to ignore next #13 char) or #13
      (tells to ignore next #10 char) }
    LastNewLineChar: char;
  public
    { This is a comfortable constructor, equivalent to
        TTextReader.Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), true) }
    constructor CreateFromFileStream(const FileName: string);

    { If AOwnsStream then in Destroy we will free Stream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean);
    destructor Destroy; override;

    { Reads next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    function Eof: boolean;
  end;

  { This is a subclass of TProcess that allows to easy "talk"
    with executed process by pipes (read process stdout/stderr,
    write to process stdin) on a line-by-line basis.

    If symbol HAS_PROCESS is not defined, this defines a junky
    implementation of TProcessLineTalk class that can't do anything
    and raises exception when you try to execute a process. }

  {$ifdef HAS_PROCESS}

  TProcessLineTalk = class(TProcess)
  private
    OutputLineReader: TTextReader;
  public
    { Adds poUsePipes to Options, since it's not reasonable to use
      this class when you don't want to communicate with process using
      pipes. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    procedure WriteLine(const S: string);
    function ReadLine: string;
  end;

  {$else HAS_PROCESS}

  TProcessLineTalk = class(TComponent)
  private
    FCommandLine: string;
  public
    procedure Execute;
    procedure WriteLine(const S: string);
    function ReadLine: string;
  published
    property CommandLine: string read FCommandLine write FCommandLine;
  end;

  {$endif else HAS_PROCESS}

implementation

uses PasDoc_Types, PasDoc_Utils, PasDoc_StreamUtils;

{ TTextReader ---------------------------------------------------------------- }
constructor TTextReader.CreateFromFileStream(const FileName: string);
begin
 Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), true);
end;

constructor TTextReader.Create(AStream: TStream; AOwnsStream: boolean);
begin
 inherited Create;
 Stream := AStream;
 FOwnsStream := AOwnsStream;
 LastNewLineChar := #0;
end;

destructor TTextReader.Destroy;
begin
 if FOwnsStream then Stream.Free;
 inherited;
end;

function TTextReader.Readln: string;
const BUF_INC = 100;
var ReadCnt, i: integer;
begin
 i := 1;

 { Note that ReadBuf may contain data that we
   already read from stream at some time but did not returned it to
   user of this class
   (because we realized we have read too much). }

 repeat
  if i > Length(ReadBuf) then
  begin
   SetLength(ReadBuf, Length(ReadBuf) + BUF_INC);
   ReadCnt := Stream.Read(ReadBuf[Length(ReadBuf) - BUF_INC + 1], BUF_INC);
   SetLength(ReadBuf, Length(ReadBuf) - BUF_INC + ReadCnt);
   if ReadCnt = 0 then
   begin
    Result := ReadBuf;
    ReadBuf := '';
    Exit;
   end;
  end;

  if ((ReadBuf[i] = #10) and (LastNewLineChar = #13)) or 
     ((ReadBuf[i] = #13) and (LastNewLineChar = #10)) then
  begin
   { We got 2nd newline character ? Ignore it. }
   Assert(i = 1);
   Delete(ReadBuf, 1, 1);
   LastNewLineChar := #0;
  end else
  if IsCharInSet(ReadBuf[i], [#10, #13]) then
  begin
   Result := Copy(ReadBuf, 1, i-1);
   LastNewLineChar := ReadBuf[i];
   Delete(ReadBuf, 1, i);
   Exit;
  end else
  begin
   LastNewLineChar := #0;
   Inc(i);
  end;
 until false;
end;

function TTextReader.Eof: boolean;
var ReadCnt: Integer;
begin
 if ReadBuf = '' then
 begin
  SetLength(ReadBuf, 1);
  ReadCnt := Stream.Read(ReadBuf[1], 1);
  SetLength(ReadBuf, ReadCnt);
 end;
 Result := ReadBuf = '';
end;

{ TProcessLineTalk ----------------------------------------------------------- }

{$ifdef HAS_PROCESS}

constructor TProcessLineTalk.Create(AOwner: TComponent);
begin
  inherited;
  Options := Options + [poUsePipes, poStdErrToOutput];
end;

destructor TProcessLineTalk.Destroy;
begin
  FreeAndNil(OutputLineReader);
  Active := False;
  inherited;
end;

procedure TProcessLineTalk.Execute;
begin
  inherited;
  FreeAndNil(OutputLineReader);
  OutputLineReader := TTextReader.Create(Output, false);
end;

procedure TProcessLineTalk.WriteLine(const S: string);
begin
  StreamWriteLine(Input, S);
end;

function TProcessLineTalk.ReadLine: string;
begin
  Result := OutputLineReader.Readln;
end;

{$else HAS_PROCESS}

procedure TProcessLineTalk.Execute;
begin
  raise Exception.Create('TProcessLineTalk.Execute: not implemented');
end;

procedure TProcessLineTalk.WriteLine(const S: string);
begin
  raise Exception.Create('TProcessLineTalk.WriteLine: not implemented');
end;

function TProcessLineTalk.ReadLine: string;
begin
  raise Exception.Create('TProcessLineTalk.ReadLine: not implemented');
end;

{$endif else HAS_PROCESS}

end.