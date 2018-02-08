{
  Copyright 1998-2018 PasDoc developers.

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
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Arno Garrels <first name.name@nospamgmx.de>)
  @abstract(A few stream utility functions.)
  TBufferedStream, TStreamReader and TStreamWriter by Arno Garrels.
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
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
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
 {$IFDEF STRING_UNICODE}
    EStreamReaderError = class(Exception);
    EStreamWriterError = class(Exception);
    TLineBreakStyle = (ilbsCRLF, ilbsLF, ilbsCR);
    TStreamReader = class(TBufferedStream)
    private
        FReadBuffer     : TBytes;
        FReadBufSize    : Integer;
        FCodePage       : LongWord;
        FLeadBytes      : TCharSet;
        FDetectBOM      : Boolean;
        { Max. number of chars (elements) forcing a new line even though     }
        { none of the break chars was found. Note that this value is not     }
        { accurate since multi-byte code points including UTF-8 will be      }
        { preserved. Default = MAXINT                                        }
        FMaxLineLength  : Integer;
        function    InternalReadLn: Boolean;
        function    InternalReadLnWLe: Boolean;
        function    InternalReadLnWBe: Boolean;
        procedure   EnsureReadBufferW(Size: Integer; var P: PWideChar);
                         {$IFDEF USE_INLINE} inline;{$ENDIF}
        procedure   EnsureReadBufferA(Size: Integer; var P: PAnsiChar);
                         {$IFDEF USE_INLINE} inline;{$ENDIF}
        procedure   SetMaxLineLength(const Value: Integer);
        procedure   SetCodePage(const Value : LongWord);
    protected
        function    GetCodePageFromBOM: LongWord; virtual;
        procedure   Init; override;
    public
        constructor Create(Stream     : TStream;
                           BufferSize : Integer = DEFAULT_BUFSIZE;
                           OwnsStream : Boolean = FALSE); override;
        constructor Create(Stream     : TStream;
                           DetectBOM  : Boolean = FALSE;
                           CodePage   : LongWord = CP_ACP;
                           OwnsStream : Boolean = FALSE;
                           BufferSize : Integer = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); override;
        constructor Create(const FileName : String;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;
        function    DetectLineBreakStyle: TLineBreakStyle;
        function    ReadLine(var S: RawByteString): Boolean; overload; virtual;
        function    ReadLine(var S: UnicodeString): Boolean; overload; virtual;
        procedure   ReadToEnd(var S: RawByteString); overload; virtual;
        procedure   ReadToEnd(var S: UnicodeString); overload; virtual;

        property    CurrentCodePage: LongWord read FCodePage write SetCodePage;
        property    MaxLineLength: Integer read FMaxLineLength write SetMaxLineLength;
        property    LeadBytes: TCharSet read FLeadBytes;
    end;

    TStreamWriter = class(TBufferedStream)
    private
        FWriteBuffer    : TBytes;  // For charset conversion
        FWriteBufSize   : Integer;
        FReadBuffer     : TBytes;  // For charset conversion
        FReadBufSize    : Integer;
        FLineBreakStyle : TLineBreakStyle;
        FCodePage       : LongWord;
        FLineBreak      : AnsiString;
        procedure   SetLineBreakStyle(Value: TLineBreakStyle);
        procedure   EnsureWriteBuffer(Size: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   EnsureReadBuffer(Size: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   SetCodePage(const Value: LongWord);
    protected
        function    GetBomFromCodePage(ACodePage: LongWord) : TBytes; virtual;
        function    GetCodePageFromBOM: LongWord; virtual;
        procedure   Init; override;
    public
        constructor Create(Stream     : TStream;
                           BufferSize : Integer = DEFAULT_BUFSIZE;
                           OwnsStream : Boolean = FALSE); override;
        constructor Create(Stream     : TStream;
                           Append     : Boolean = TRUE;
                           DetectBOM  : Boolean = FALSE;
                           CodePage   : LongWord = CP_ACP;
                           OwnsStream : Boolean = FALSE;
                           BufferSize : Longint = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); override;
        constructor Create(const FileName : String;
                           Append         : Boolean = TRUE;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;
        function    DetectLineBreakStyle: TLineBreakStyle;
        procedure   Write(const S     : UnicodeString); reintroduce; overload; virtual;
        procedure   Write(const S     : RawByteString;
                          SrcCodePage : LongWord = CP_ACP); reintroduce; overload; virtual;
        {
        procedure   Write(Value: Boolean); reintroduce; overload;
        procedure   Write(Value: WideChar); reintroduce; overload;
        procedure   Write(Value: AnsiChar); reintroduce; overload;
        ..
        }
        procedure   WriteLine(const S     : UnicodeString); overload;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   WriteLine(const S     : RawByteString;
                              SrcCodePage : LongWord = CP_ACP); overload;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   WriteBOM;
        property    CurrentCodePage : LongWord read FCodePage write SetCodePage;
        property    LineBreakStyle  : TLineBreakStyle    read  FLineBreakStyle
                                                         write SetLineBreakStyle;
    end;
{$ENDIF}

function StreamReadLine(const AStream: TStream): AnsiString;

{ Write AString contents, then LineEnding to AStream }
procedure StreamWriteLine(const AStream: TStream; const AString: AnsiString);

{ Just write AString contents to AStream }
procedure StreamWriteString(const AStream: TStream; const AString: AnsiString);


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


{---------------------------------------------------------------------------}
{$ENDIF}

{ TStreamReader }

{$IFDEF STRING_UNICODE}

const
  S_ERR_CP_NOSUPPORT = 'Unsupported code page %s';

const
    DEFAULT_READBUFFER_SIZE = 1024 * SizeOf(Char);

{---------------------------------------------------------------------------}
constructor TStreamReader.Create(Stream: TStream;
  BufferSize: Integer = DEFAULT_BUFSIZE; OwnsStream: Boolean = FALSE);
begin
    Create(Stream, False);
end;


{---------------------------------------------------------------------------}
constructor TStreamReader.Create(Stream: TStream; DetectBOM: Boolean = FALSE;
  CodePage: LongWord = CP_ACP; OwnsStream: Boolean = FALSE;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FDetectBOM := DetectBOM;
    FCodePage  := CodePage;
    inherited Create(Stream, BufferSize, OwnsStream);
end;


{---------------------------------------------------------------------------}
constructor TStreamReader.Create(const FileName: String; Mode: Word;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    Create(FileName, FALSE);
end;


{---------------------------------------------------------------------------}
constructor TStreamReader.Create(const FileName: String;
  DetectBOM: Boolean = TRUE; CodePage: LongWord = CP_ACP;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FDetectBOM := DetectBOM;
    FCodePage  := CodePage;
    inherited Create(FileName, fmOpenRead or fmShareDenyWrite, BufferSize);
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.Init;
begin
    inherited;
    FReadBufSize   := DEFAULT_READBUFFER_SIZE;
    SetMaxLineLength(MAXINT);
    SetLength(FReadBuffer, FReadBufSize + SizeOf(Char));
    if FDetectBom then
        SetCodePage(GetCodePageFromBOM)
    else
        SetCodePage(FCodePage);
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.SetMaxLineLength(const Value: Integer);
begin
    if Value < 1 then
        FMaxLineLength := 1
    else
        FMaxLineLength := Value
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.SetCodePage(const Value: LongWord);
var
    CPInfo : TCPInfo;
    I      : Integer;
    J      : Byte;
begin
    case Value of
        CP_UTF32    :
            raise EStreamReaderError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE']);
        CP_UTF32Be  :
            raise EStreamReaderError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 BE']);

        CP_ACP      :
            begin
                FLeadBytes := SysUtils.Leadbytes;
                FCodePage  := Value;
            end;
        CP_UTF8,
        CP_UTF16,
        CP_UTF16Be  :
            begin
                FLeadBytes := [];
                FCodePage  := Value;
            end;
        else
            if GetCPInfo(Value, CPInfo) then begin
                FCodePage := Value;
                if CPInfo.MaxCharSize > 1 then begin
                    I := 0;
                    while (I < MAX_LEADBYTES) and
                          ((CPInfo.LeadByte[I] or CPInfo.LeadByte[I + 1]) <> 0) do begin
                        for J := CPInfo.LeadByte[I] to CPInfo.LeadByte[I + 1] do
                            Include(FLeadBytes, AnsiChar(J));
                        Inc(I, 2);
                    end;
                end
                else
                    FLeadBytes := [];
            end
            else
                raise EStreamReaderError.Create(SysErrorMessage(GetLastError));
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.GetCodePageFromBOM: LongWord;
var
    OldPos : Int64;
    A : array [0..3] of Byte;
    BomLen : Integer;
begin
    FillChar(A, 4, #0);
    OldPos := Position;
    Seek(0, soBeginning);
    Read(A, 4);
    if (A[0] = $FF) and (A[1] = $FE) and (A[2] = 0) and (A[3] = 0) then begin
        raise EStreamReaderError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE detected']);
    end
    else if (A[0] = 0) and (A[1] = 0) and (A[2] = $FE) and (A[3] = $FF) then begin
        raise EStreamReaderError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 BE detected']);
    end
    else if (A[0] = $FF) and (A[1] = $FE) then begin
        Result := CP_UTF16;
        BomLen := 2;
    end
    else if (A[0] = $FE) and (A[1] = $FF) then begin
        Result := CP_UTF16Be;
        BomLen := 2;
    end
    else if (A[0] = $EF) and (A[1] = $BB) and (A[2] = $BF) then begin
        Result := CP_UTF8;
        BomLen := 3;
    end
    else begin
        Result := CP_ACP;
        BomLen := 0;
    end;
    if OldPos > BomLen then
        Position := OldPos
    else
        Position := BomLen;
end;


{---------------------------------------------------------------------------}
function TStreamReader.DetectLineBreakStyle: TLineBreakStyle;
var
    OldPos    : Int64;
    ChA       : AnsiChar;
    ChW       : WideChar;
    CodePage  : LongWord;
begin
    Result := ilbsCRLF;
    OldPos := Position;
    CodePage := GetCodePageFromBOM;
    try
    case CodePage of
        CP_UTF16, CP_UTF16Be :
        begin
            Seek(2, soBeginning);
            while Read(ChW, SizeOf(ChW)) = SizeOf(ChW) do
            begin
                if CodePage = CP_UTF16Be then
                    ChW := WideChar((Word(ChW) shl 8) or (Word(ChW) shr 8));
                case ChW of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
        end;
        else // case
            if CodePage = CP_UTF8 then
                Seek(3, soBeginning);

            while Read(ChA, SizeOf(ChA)) = SizeOf(ChA) do
            begin
                case ChA of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
    end;
    finally
        Position := OldPos;
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.InternalReadLn: Boolean;
var
    Ch       : AnsiChar;
    Idx      : Integer;
    P        : PAnsiChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PAnsiChar(@FReadBuffer[0]);
    while Read(Ch, SizeOf(AnsiChar)) = SizeOf(AnsiChar) do begin
        Inc(Idx);
        if (Idx >= FMaxLineLength) then begin
            if ((FCodePage <> CP_UTF8) and (not (Ch in FLeadBytes))) or
               ((FCodePage = CP_UTF8) and (not IsUtf8TrailByte(Byte(Ch)))) then
            begin
                Seek(-1, soCurrent);
                Result := TRUE;
                Exit;
            end;
        end;
        EnsureReadBufferA(Idx + 1, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-1, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end;
            else
                if Flag then begin
                    Seek(-1, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.InternalReadLnWLe: Boolean;
var
    Ch       : WideChar;
    Idx      : Integer;
    P        : PWideChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PWideChar(@FReadBuffer[0]);
    while Read(Ch, SizeOf(WideChar)) = SizeOf(WideChar) do
    begin
        Inc(Idx);
        if (Idx >= FMaxLineLength) and (not IsLeadChar(Ch)) then begin
            Seek(-2, soCurrent);
            Result := TRUE;
            Exit;
        end;
        EnsureReadBufferW((Idx + 1) * 2, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-2, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end;
            else
                if Flag then begin
                    Seek(-2, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.InternalReadLnWBe: Boolean;
var
    Ch       : WideChar;
    Wrd      : Word;
    Idx      : Integer;
    P        : PWideChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PWideChar(@FReadBuffer[0]);
    while Read(Wrd, SizeOf(Word)) = SizeOf(Word) do begin
        Inc(Idx);
        Ch := WideChar((Wrd shr 8) or (Wrd shl 8));
        if (Idx >= FMaxLineLength) and (not IsLeadChar(Ch)) then begin
            Seek(-2, soCurrent);
            Result := TRUE;
            Exit;
        end;
        EnsureReadBufferW((Idx + 1) * 2, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-2, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end;
            else
                if Flag then begin
                    Seek(-2, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.ReadLine(var S: UnicodeString): Boolean;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                Result := InternalReadLnWLe;
                S := PWideChar(@FReadBuffer[0]);
            end;
        CP_UTF16Be :
            begin
                Result := InternalReadLnWBe;
                S := PWideChar(@FReadBuffer[0]);
            end;
        else
            Result := InternalReadLn;
            S := AnsiToUnicode(PAnsiChar(@FReadBuffer[0]), FCodePage);
    end;
end;


{---------------------------------------------------------------------------}
function TStreamReader.ReadLine(var S: RawByteString): Boolean;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                Result := InternalReadLnWLe;
                S := UnicodeToAnsi(PWideChar(@FReadBuffer[0]), CP_ACP, TRUE);
            end;
        CP_UTF16Be :
            begin
                Result := InternalReadLnWBe;
                S := UnicodeToAnsi(PWideChar(@FReadBuffer[0]), CP_ACP, TRUE);
            end;
        else
            Result := InternalReadLn;
            S := RawByteString(PAnsiChar(@FReadBuffer[0]));
        {$IFDEF COMPILER_12_UP}
            if (S <> '') and (FCodePage <> CP_ACP) then
                PWord(INT_PTR(S) - 12)^ := FCodePage;
        {$ENDIF}
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.ReadToEnd(var S: RawByteString);
var
    Buf : TBytes;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                SetLength(Buf, (Size - Position) + 2);
                Read(Buf[0], Length(Buf) - 2);
                Buf[Length(Buf) - 1] := 0;
                Buf[Length(Buf) - 2] := 0;
                S := UnicodeToAnsi(PWideChar(@Buf[0]), CP_ACP, TRUE);
            end;
        CP_UTF16Be :
            begin
                SetLength(Buf, (Size - Position) + 2);
                Read(Buf[0], Length(Buf) - 2);
                Buf[Length(Buf) - 1] := 0;
                Buf[Length(Buf) - 2] := 0;
                Swap16Buf(@Buf[0], @Buf[0], (Length(Buf) - 2) div 2);
                S := UnicodeToAnsi(PWideChar(@Buf[0]), CP_ACP, TRUE);
            end;
        else
            SetLength(S, Size - Position);
            Read(PAnsiChar(S)^, Length(S));
        {$IFDEF COMPILER_12_UP}
            if (S <> '') and (FCodePage <> CP_ACP) then
                PWord(INT_PTR(S) - 12)^ := FCodePage;
        {$ENDIF}
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.ReadToEnd(var S: UnicodeString);
var
    Buf : TBytes;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                SetLength(S, (Size - Position) div 2);
                Read(PWideChar(S)^, Length(S) * 2);
            end;
        CP_UTF16Be :
            begin
                SetLength(S, (Size - Position) div 2);
                Read(PWideChar(S)^, Length(S) * 2);
                Swap16Buf(Pointer(S), Pointer(S), Length(S));
            end;
        else
            SetLength(Buf, (Size - Position) + 1);
            Read(Buf[0], Length(Buf) - 1);
            Buf[Length(Buf) - 1] := 0;
            S := AnsiToUnicode(PAnsiChar(@Buf[0]), FCodePage);
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.EnsureReadBufferW(Size: Integer; var P: PWideChar);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DEFAULT_READBUFFER_SIZE);
        SetLength(FReadBuffer, FReadBufSize);
        P := @FReadBuffer[0];
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamReader.EnsureReadBufferA(Size: Integer; var P: PAnsiChar);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DEFAULT_READBUFFER_SIZE);
        SetLength(FReadBuffer, FReadBufSize);
        P := @FReadBuffer[0];
    end;
end;


{---------------------------------------------------------------------------}
{ TStreamWriter }
{---------------------------------------------------------------------------}
constructor TStreamWriter.Create(Stream: TStream;
  BufferSize: Integer = DEFAULT_BUFSIZE; OwnsStream: Boolean = FALSE);
begin
    Create(Stream, TRUE, FALSE, CP_ACP, FALSE, BufferSize);
end;


{---------------------------------------------------------------------------}
constructor TStreamWriter.Create(Stream: TStream; Append: Boolean = TRUE;
  DetectBOM: Boolean = FALSE; CodePage: LongWord = CP_ACP;
  OwnsStream: Boolean = FALSE; BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FCodePage  := CodePage;
    inherited Create(Stream, BufferSize, OwnsStream);
    if DetectBom then
        FCodePage := GetCodePageFromBOM;
    if Append then Seek(0, soEnd);
end;


{---------------------------------------------------------------------------}
constructor TStreamWriter.Create(const FileName: String; Mode: Word;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FCodePage := CP_ACP;
    inherited Create(FileName, Mode, BufferSize);
end;


{---------------------------------------------------------------------------}
constructor TStreamWriter.Create(const FileName: String;
  Append: Boolean = TRUE; DetectBOM: Boolean = TRUE;
  CodePage: LongWord = CP_ACP; BufferSize: Integer = DEFAULT_BUFSIZE);
var
    Mode : Word;
begin
    FCodePage := CodePage;
    if Append and FileExists(FileName) then begin
        Mode := fmOpenReadWrite or fmShareDenyWrite;
        inherited Create(FileName, Mode, BufferSize);
        if DetectBom then
            FCodePage := GetCodePageFromBOM;
        Seek(0, soEnd);
    end
    else begin
        Mode := fmCreate;
        inherited Create(FileName, Mode, BufferSize);
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.Init;
begin
    inherited;
    FReadBufSize := DEFAULT_READBUFFER_SIZE;
    SetLength(FReadBuffer, FReadBufSize);
    FWriteBufSize := DEFAULT_READBUFFER_SIZE;
    SetLength(FWriteBuffer, FWriteBufSize);
    LineBreakStyle := ilbsCRLF;
end;


{---------------------------------------------------------------------------}
function TStreamWriter.GetCodePageFromBOM: LongWord;
var
    OldPos : Int64;
    A : array [0..3] of Byte;
    BomLen : Integer;
begin
    FillChar(A, 4, #0);
    OldPos := Position;
    Seek(0, soBeginning);
    Read(A, 4);
    if (A[0] = $FF) and (A[1] = $FE) and (A[2] = 0) and (A[3] = 0) then begin
        raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE detected']);
    end
    else if (A[0] = 0) and (A[1] = 0) and (A[2] = $FE) and (A[3] = $FF) then begin
        raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 BE detected']);
    end
    else if (A[0] = $FF) and (A[1] = $FE) then begin
        Result := CP_UTF16;
        BomLen := 2;
    end
    else if (A[0] = $FE) and (A[1] = $FF) then begin
        Result := CP_UTF16Be;
        BomLen := 2;
    end
    else if (A[0] = $EF) and (A[1] = $BB) and (A[2] = $BF) then begin
        Result := CP_UTF8;
        BomLen := 3;
    end
    else begin
        Result := CP_ACP;
        BomLen := 0;
    end;
    if OldPos > BomLen then
        Position := OldPos
    else
        Position := BomLen;
end;


{---------------------------------------------------------------------------}
function TStreamWriter.GetBomFromCodePage(ACodePage: LongWord) : TBytes;
begin
    case ACodePage of
        CP_UTF16 :
            begin
                SetLength(Result, 2);
                Result[0] := $FF;
                Result[1] := $FE;
            end;
        CP_UTF16Be :
            begin
                SetLength(Result, 2);
                Result[0] := $FE;
                Result[1] := $FF;
            end;
        CP_UTF8    :
            begin
                SetLength(Result, 3);
                Result[0] := $EF;
                Result[1] := $BB;
                Result[2] := $BF;
            end;
        CP_UTF32   :
            raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE']);
        CP_UTF32Be :
            raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE']);

        else
            SetLength(Result, 0);
    end;
end;


{---------------------------------------------------------------------------}
function TStreamWriter.DetectLineBreakStyle: TLineBreakStyle;
var
    OldPos    : Int64;
    ChA       : AnsiChar;
    ChW       : WideChar;
    CodePage  : LongWord;
begin
    Result := ilbsCRLF;
    OldPos := Position;
    CodePage := GetCodePageFromBOM;
    try
    case CodePage of
        CP_UTF16, CP_UTF16Be :
        begin
            Seek(2, soBeginning);
            while Read(ChW, SizeOf(ChW)) = SizeOf(ChW) do
            begin
                if CodePage = CP_UTF16Be then
                    ChW := WideChar((Word(ChW) shl 8) or (Word(ChW) shr 8));
                case ChW of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
        end;
        else // case
            if CodePage = CP_UTF8 then
                Seek(3, soBeginning);

            while Read(ChA, SizeOf(ChA)) = SizeOf(ChA) do
            begin
                case ChA of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
    end;
    finally
        Position := OldPos;
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.WriteBOM;
var
    Bom : TBytes;
begin
    Bom := GetBomFromCodePage(FCodePage);
    if Length(Bom) > 0 then begin
        Seek(0, soBeginning);
        Write(Bom[0], Length(Bom));
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.EnsureWriteBuffer(Size: Integer);
begin
    if Size > FWriteBufSize then begin
        while Size > FWriteBufSize do
            Inc(FWriteBufSize, DEFAULT_READBUFFER_SIZE);
        SetLength(FWriteBuffer, FWriteBufSize);
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.EnsureReadBuffer(Size: Integer);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DEFAULT_READBUFFER_SIZE);
        SetLength(FReadBuffer, FReadBufSize);
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.Write(const S: UnicodeString);
var
    Len   : Integer;
    SLen  : Integer;
begin
    SLen := Length(S);
    if SLen = 0 then Exit;
    case FCodePage of
        CP_UTF16   :
            begin
                WriteBuffer(Pointer(S)^, SLen * 2);
            end;
        CP_UTF16Be :
            begin
                EnsureWriteBuffer((SLen + 1) * 2);
                Move(Pointer(S)^, FWriteBuffer[0], SLen * 2);
                PWideChar(FWriteBuffer)[SLen] := #0;
                Swap16Buf(@FWriteBuffer[0], @FWriteBuffer[0], SLen);
                WriteBuffer(FWriteBuffer[0], SLen * 2);
            end;
        else
            Len := WideCharToMultiByte(FCodePage, 0, Pointer(S), SLen, nil, 0,
                                       nil, nil);
            EnsureWriteBuffer(Len);
            Len := WideCharToMultiByte(FCodePage, 0, Pointer(S), SLen,
                                       @FWriteBuffer[0], Len, nil, nil);
            WriteBuffer(FWriteBuffer[0], Len);
    end; //case
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.Write(const S: RawByteString;
  SrcCodePage: LongWord = CP_ACP);
var
    Len   : Integer;
    Len1  : Integer;
    SLen  : Integer;
begin
    SLen := Length(S);
    if SLen = 0 then Exit;
    case FCodePage of
        CP_UTF8,
        CP_UTF7     :
            begin
                if SrcCodePage <> FCodePage then
                begin
                    Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S),
                                               SLen, nil, 0);
                    EnsureReadBuffer(Len);
                    Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S), SLen,
                                               @FReadBuffer[0], Len);

                    Len1 := WideCharToMultibyte(FCodePage, 0, @FReadBuffer[0],
                                                Len, nil, 0, nil, nil);
                    EnsureWriteBuffer(Len1);
                    Len1 := WideCharToMultibyte(FCodePage, 0, @FReadBuffer[0],
                                                Len, @FWriteBuffer[0], Len1,
                                                nil, nil);
                    WriteBuffer(FWriteBuffer[0], Len1);
                end
                else
                    WriteBuffer(Pointer(S)^, SLen);
            end;
        CP_UTF16   :
            begin
                Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S), SLen,
                                           nil, 0);
                EnsureWriteBuffer(Len * 2);
                Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S), SLen,
                                           @FWriteBuffer[0], Len);
                WriteBuffer(FWriteBuffer[0], Len * 2);
            end;
        CP_UTF16Be :
            begin
                Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S), SLen,
                                           nil, 0);
                EnsureWriteBuffer((Len + 1) * 2);
                Len := MultibyteToWideChar(SrcCodePage, 0, Pointer(S), SLen,
                                           @FWriteBuffer[0], Len);
                PWideChar(FWriteBuffer)[Len] := #0;
                Swap16Buf(@FWriteBuffer[0], @FWriteBuffer[0], Len);
                WriteBuffer(FWriteBuffer[0], Len * 2);
            end;
        else
            WriteBuffer(Pointer(S)^, SLen);
    end; //case
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.WriteLine(const S: RawByteString;
  SrcCodePage: LongWord = CP_ACP);
begin
    Write(S, SrcCodePage);
    Write(FLineBreak, SrcCodePage);
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.WriteLine(const S: UnicodeString);
begin
    Write(S);
    Write(UnicodeString(FLineBreak));
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.SetLineBreakStyle(Value: TLineBreakStyle);
begin
    FLineBreakStyle := Value;
    case FLineBreakStyle of
        ilbsCRLF : FLineBreak := #13#10;
        ilbsLF   : FLineBreak := #10;
    else
        FLineBreak := #13;
    end;
end;


{---------------------------------------------------------------------------}
procedure TStreamWriter.SetCodePage(const Value: LongWord);
var
  LCPInfo: TCPInfo;
begin
  case Value of
    CP_ACP,
    CP_UTF8,
    CP_UTF16,
    CP_UTF16Be : FCodePage := Value;

    CP_UTF32   :
      raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 LE']);
    CP_UTF32Be :
      raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT, ['UTF-32 BE']);

    else
      if not GetCPInfo(Value, LCPInfo) then
         raise EStreamWriterError.CreateFmt(S_ERR_CP_NOSUPPORT,
          [SysErrorMessage(GetLastError)]);
      FCodePage := FCodepage;
  end;
end;


{---------------------------------------------------------------------------}
{$ENDIF}

function StreamReadLine(const AStream: TStream): AnsiString;
// totally junky implementation!!
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

end.
