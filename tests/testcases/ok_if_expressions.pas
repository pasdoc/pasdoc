unit ok_if_expressions;

interface

{$DEFINE MSWINDOWS}

{$IFDEF MSWINDOWS}
procedure Windows_IFDEF;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure Windows_IFDEF_ELSE;
{$ELSE}
procedure NotWindows_IFDEF_ELSE;
{$ENDIF}

{$IFNDEF MSWINDOWS}
procedure NotWindows_IFNDEF;
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
procedure Windows_IF;
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
procedure FpcAndUnix_IF;
{$ELSEIF DEFINED(POSIX)}
procedure Posix_IF;
{$ENDIF}

{$UNDEF MSWINDOWS}
{$DEFINE FPC}
{$DEFINE UNIX}

{$IF DEFINED(MSWINDOWS)}
procedure Windows_IF_2;
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
procedure FpcAndUnix_IF_2;
{$ELSEIF DEFINED(POSIX)}
procedure Posix_IF_2;
{$ENDIF}

procedure Always;

implementation

end.
