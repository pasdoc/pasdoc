{ This is a minimized version of vorbisfile.pas from 
  http://vrmlengine.sourceforge.net/, just to test pasdoc2 macros/include
  file handling. }

{ API of vorbisfile library. Usually libvorbisfile.so under Unixes
  or vorbisfile.dll under Windows. This is just a quick translation
  of /usr/include/vorbis/vorbisfile.h header. }
unit ok_vorbisfile;

{$packrecords C}

{$I ok_vorbisfile_conf.inc}

interface

uses CTypes, VorbisCodec, KambiOgg;

const
  NOTOPEN   = 0;
  PARTOPEN  = 1;
  OPENED    = 2;
  STREAMSET = 3;
  INITSET   = 4;

type
  TSizeT = LongWord;

  TVorbisFileReadFunc = function (ptr: Pointer; Size: TSizeT; nmemb: TSizeT; DataSource: Pointer): TSizeT; libvorbisfile_decl;
  TVorbisFileSeekFunc = function (DataSource: Pointer; offset: Int64; whence: CInt): CInt; libvorbisfile_decl;
  TVorbisFileCloseFunc = function (DataSource: Pointer): CInt; libvorbisfile_decl;
  TVorbisFileTellFunc = function (DataSource: Pointer): CLong; libvorbisfile_decl;

  Tov_callbacks = record
    read_func: TVorbisFileReadFunc;
    seek_func: TVorbisFileSeekFunc;
    close_func: TVorbisFileCloseFunc;
    tell_func: TVorbisFileTellFunc;
  end;
  Pov_callbacks = ^Tov_callbacks;

  TOggVorbis_File = record
    datasource: Pointer; {* Pointer to a FILE *, etc. *}
    seekable: Cint;
    offset: Int64;
    _end: Int64;
    oy: Togg_sync_state;

    links: Cint;
    offsets: PInt64;
    dataoffsets: PInt64;
    serialnos: PCLong;
    pcmlengths: PInt64; {* overloaded to maintain binary
                                    compatability; x2 size, stores both
                                    beginning and end values *}
    vi: Pvorbis_info;
    vc: Pvorbis_comment;

    {* Decoding working state local storage *}
    pcm_offset: Int64;
    ready_state: CInt;
    current_serialno: Clong;
    current_link: CInt;

    bittrack: double;
    samptrack: double;

    os: Togg_stream_state; {* take physical pages, weld into a logical
                            stream of packets *}
    vd: Tvorbis_dsp_state; {* central working state for the packet->PCM decoder *}
    vb: Tvorbis_block; {* local working space for packet->PCM decode *}

    callbacks: Tov_callbacks;
  end;
  POggVorbis_File = ^TOggVorbis_File;

var
  ov_clear: function (Vf: POggVorbis_File): CInt; libvorbisfile_decl;
  
  VorbisFileInited: boolean;

implementation

uses SysUtils, KambiUtils, KambiDynLib;

var
  VorbisFileLibrary: TDynLib;
initialization
  VorbisFileLibrary :=

    {$ifdef UNIX}
    {$ifdef DARWIN}
    TDynLib.Load('libvorbisfile.3.dylib', false);
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libvorbisfile.dylib', false);
    {$else}
    TDynLib.Load('libvorbisfile.so.3', false);
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libvorbisfile.so', false);
    {$endif}
    {$endif}

    {$ifdef MSWINDOWS}
    TDynLib.Load('vorbisfile.dll', false);
    {$endif}

  VorbisFileInited := VorbisFileLibrary <> nil;
  if VorbisFileInited then
  begin
    Pointer(ov_clear) := VorbisFileLibrary.Symbol('ov_clear');
  end;
finalization
  VorbisFileInited := false;
  FreeAndNil(VorbisFileLibrary);
end.