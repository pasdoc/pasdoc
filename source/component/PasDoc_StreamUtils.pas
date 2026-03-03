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

{
  @abstract(A few stream utility functions.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Arno Garrels <first name.name@nospamgmx.de> (TBufferedStream and more))
  @author(Michalis Kamburelis)
}
unit PasDoc_StreamUtils;

{$I pasdoc_defines.inc}

{$IFDEF COMPILER_12_UP}
  {.$WARN IMPLICIT_STRING_CAST       OFF}
  {.$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
  {$WARN EXPLICIT_STRING_CAST       OFF}
  {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  PasDoc_Types;

{$IFDEF USE_BUFFERED_STREAM}
const
    DEFAULT_BUFSIZE = 4096;
    MIN_BUFSIZE     = 128;
    MAX_BUFSIZE     = 1024 * 64;

type
    TBufferedStream = class(TStream)
    private
        FStream           : TStream;
        FOwnsStream       : Boolean;
        FBuffer           : TBytes;
        FBufferSize       : Integer;
        FBufferedDataSize : Integer; // Number of bytes currently buffered
        FDirtyCount       : Integer; // Range of dirty bytes in buffer counted from buffer index zero
        FStreamBufPos     : Int64;   // First byte of the buffer in stream
        FPosition         : Int64;   // Helper var, partly calculated
        FFastSize         : Int64;   // Size of FStream at the time IsReadOnly is set to TRUE. See property IsReadOnly below
        FIsReadOnly       : Boolean; // See property IsReadOnly below
    protected
        procedure   SetIsReadOnly(const Value: Boolean);
        procedure   SetSize(NewSize: Integer); override;
        procedure   SetSize(const NewSize: Int64); override;
        function    InternalGetSize: Int64; {$IFDEF USE_INLINE} inline;{$ENDIF}
        function    GetSize: Int64; override;
        procedure   Init; virtual;
        function    FillBuffer: Boolean; {$IFDEF USE_INLINE} inline;{$ENDIF}
    public
        constructor Create; overload; // Dummy, don't call!
        constructor Create(Stream     : TStream;
                           BufferSize : Integer = DEFAULT_BUFSIZE;
                           OwnsStream : Boolean = FALSE); overload; virtual;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload; virtual;
        destructor  Destroy; override;

        procedure   Flush; {$IFDEF USE_INLINE} inline;{$ENDIF}
        function    Read(var Buffer; Count: Integer): Integer; override;

        function    Seek(Offset: Integer; Origin: Word): Integer; override;
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function    Write(const Buffer; Count: Integer): Integer; override;
        { Set IsReadOnly if you are sure you will never write to the stream
         and nobody else will do, this speeds up getter Size and in turn
         Seeks as well. IsReadOnly is set to TRUE if a constructor with
         filename is called with a read only mode and a share lock.         }
        property    IsReadOnly: Boolean read FIsReadOnly write SetIsReadOnly;
        property    FastSize: Int64 read GetSize;
    end;
 {$ENDIF}

{$ifndef STRING_UNICODE}
{ Read next line from AStream, return it as AnsiString.
  Line ending is included in the result. }
function StreamReadLine(const AStream: TStream): AnsiString;

{ Write AString contents, then LineEnding to AStream. }
procedure StreamWriteLine(const AStream: TStream; const AString: AnsiString);

{ Just write AString contents to AStream. }
procedure StreamWriteString(const AStream: TStream; const AString: AnsiString);
{$endif}

implementation

uses PasDoc_Utils; // for LineEnding in Kylix/Delphi

{---------------------------------------------------------------------------}
{ TBufferedStream }
{---------------------------------------------------------------------------}

{$IFDEF USE_BUFFERED_STREAM}

{$IFOPT R+}
  {$DEFINE SETRANGECHECKSBACK} { We'll turn off range checking temporarily }
{$ENDIF}

type
    TDummyByteArray = array [0..0] of Byte; { Casts require range checks OFF }


constructor TBufferedStream.Create(Stream: TStream;
  BufferSize: Longint = DEFAULT_BUFSIZE; OwnsStream: Boolean = FALSE);
begin
    inherited Create;
    Assert(Stream <> nil, 'Stream must be assigned');
    FStream := Stream;
    FOwnsStream := OwnsStream;
    FBufferSize := BufferSize;
    Init;
end;


{---------------------------------------------------------------------------}
constructor TBufferedStream.Create;
begin
    Create(nil); // dummy!
end;


{---------------------------------------------------------------------------}
constructor TBufferedStream.Create(const FileName: String; Mode: Word;
  BufferSize: Integer);
begin
    inherited Create;
    { Even in mode fmOpenWrite we need to read from file as well }
    if (Mode <> fmCreate) and
       (Mode and fmOpenWrite <> 0) then begin
        Mode := Mode and not fmOpenWrite;
        Mode := Mode or fmOpenReadWrite;
    end;
    FStream := TFileStream.Create(FileName, Mode);
    FBufferSize := BufferSize;
    FOwnsStream := True;
    IsReadOnly := (Mode <> fmCreate) and
                  (Mode and fmOpenWrite = 0) and (Mode and fmOpenReadWrite = 0) and
                  ((Mode and fmShareDenyWrite = fmShareDenyWrite) or
                   (Mode and fmShareExclusive = fmShareExclusive));
    Init;
end;


{---------------------------------------------------------------------------}
destructor TBufferedStream.Destroy;
begin
    Flush;
    if FOwnsStream then
        FreeAndNil(FStream);
    inherited Destroy;
end;


{---------------------------------------------------------------------------}
procedure TBufferedStream.Flush;
begin
    if FDirtyCount > 0 then begin
        FStream.Position := FStreamBufPos;
        FStream.WriteBuffer(FBuffer[0], FDirtyCount);
        FDirtyCount := 0;
    end;
end;


{---------------------------------------------------------------------------}
procedure TBufferedStream.Init;
begin
    if FBufferSize < MIN_BUFSIZE then
        FBufferSize := MIN_BUFSIZE
    else if FBufferSize > MAX_BUFSIZE then
        FBufferSize := MAX_BUFSIZE
    else
        FBufferSize := (FBufferSize div MIN_BUFSIZE) * MIN_BUFSIZE;
    SetLength(FBuffer, FBufferSize);
    FPosition := FStream.Position;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.Read(var Buffer; Count: Integer): Longint;
var
    BufPos   : Integer;
    SrcIndex : Integer;
    Remaining: Integer;
begin
    Result := Count;
    while Count > 0 do begin
        if not ((FStreamBufPos <= FPosition) and
                (FPosition < (FStreamBufPos + FBufferedDataSize))) then
            if not FillBuffer then
                Break;
        { Read from buffer }
        SrcIndex := Result - Count;
        Remaining := Count;
        BufPos := FPosition - FStreamBufPos;
        if Remaining > FBufferedDataSize - BufPos then
            Remaining := FBufferedDataSize - BufPos;
      {$R-}
        Move(FBuffer[BufPos], TDummyByteArray(Buffer)[SrcIndex], Remaining);
    {$IFDEF SETRANGECHECKSBACK}
      {$R+}
    {$ENDIF}
        Inc(FPosition, Remaining);
        Dec(Count, Remaining);
    end;
    Result := Result - Count;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.FillBuffer: Boolean;
begin
    Flush;
    FStream.Position  := FPosition;
    FStreamBufPos     := FPosition;
    FBufferedDataSize := FStream.Read(FBuffer[0], FBufferSize);
    Result            := FBufferedDataSize > 0;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
    NewPos: Int64;
begin
    case Origin of
        soBeginning : NewPos := Offset;
        soCurrent   : NewPos := FPosition + Offset;
        soEnd       :
            begin
                NewPos := InternalGetSize + Offset;
                if (FDirtyCount > 0) and
                   (NewPos < FStreamBufPos + FDirtyCount) then
                begin
                    Flush;
                    NewPos := FStream.Size + Offset;
                end;
            end;
        else
            NewPos := -1;
    end;
    if NewPos < 0 then
        NewPos := -1
    else
        FPosition := NewPos;
    Result := NewPos;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;


{---------------------------------------------------------------------------}
procedure TBufferedStream.SetIsReadOnly(const Value: Boolean);
begin
    FIsReadOnly := Value;
    if FIsReadOnly then
        FFastSize := FStream.Size
    else
        FFastSize := -1;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.InternalGetSize: Int64;
begin
    if IsReadOnly then
        Result := FFastSize
    else
        Result := FStream.Size;
end;


{---------------------------------------------------------------------------}
function TBufferedStream.GetSize: Int64;
begin
    { Gets the calculated size in order to not trigger Flush in method  }
    { Seek which was wasted time. Do not call inherited.                }
    Result := InternalGetSize;
    if Result < FStreamBufPos + FDirtyCount then
        Result := FStreamBufPos + FDirtyCount;
end;


{---------------------------------------------------------------------------}
procedure TBufferedStream.SetSize(const NewSize: Int64);
begin
    FStream.Size := NewSize;
    FPosition := FStream.Position;
    if NewSize < (FStreamBufPos + FDirtyCount) then begin
        FDirtyCount := NewSize - FStreamBufPos;
        if FDirtyCount < 0 then
            FDirtyCount := 0;
    end;
    if NewSize < (FStreamBufPos + FBufferedDataSize) then begin
        FBufferedDataSize := NewSize - FStreamBufPos;
        if FBufferedDataSize < 0 then
            FBufferedDataSize := 0;
    end;
end;


{---------------------------------------------------------------------------}
procedure TBufferedStream.SetSize(NewSize: Integer);
begin
    SetSize(Int64(NewSize));
end;


{---------------------------------------------------------------------------}
function TBufferedStream.Write(const Buffer; Count: Integer): Longint;
var
    DestPos   : Integer;
    SrcIndex  : Integer;
    Remaining : Integer;
begin
    Result := Count;
    while Count > 0 do begin
        if not ((FStreamBufPos <= FPosition) and
                (FPosition < (FStreamBufPos + FBufferedDataSize))) then
            if not ((FStreamBufPos <= FPosition) and
                    (FPosition < (FStreamBufPos + FBufferSize))) then
                FillBuffer;
        { Write to buffer }
        SrcIndex := Result - Count;
        Remaining := Count;
        DestPos := FPosition - FStreamBufPos;
        if Remaining > FBufferSize - DestPos then
            Remaining := FBufferSize - DestPos;
        if FBufferedDataSize < DestPos + Remaining then
            FBufferedDataSize := DestPos + Remaining;
      {$R-}
        Move(TDummyByteArray(Buffer)[SrcIndex], FBuffer[DestPos], Remaining);
    {$IFDEF SETRANGECHECKSBACK}
      {$R+}
    {$ENDIF}
        FDirtyCount := DestPos + Remaining;
        Inc(FPosition, Remaining);
        Dec(Count, Remaining);
    end;
    Result := Result - Count;
end;

{$ENDIF USE_BUFFERED_STREAM}

{$ifndef STRING_UNICODE}

function StreamReadLine(const AStream: TStream): AnsiString;
// TODO: Optimize instead of reading one char at a time.
var
  c: AnsiChar;
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

procedure StreamWriteLine(const AStream: TStream; const AString: AnsiString);
var
 LineTerminator: AnsiString;
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
  LineTerminator := LineEnding;
  AStream.Write(LineTerminator[1], Length(LineTerminator));
end;

procedure StreamWriteString(const AStream: TStream; const AString: AnsiString);
begin
  if length(AString) > 0 then begin
    AStream.WriteBuffer(AString[1], Length(AString));
  end;
end;

{$endif}

end.
