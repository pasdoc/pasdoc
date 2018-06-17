{ Testcase for various conditional directives, primarily $if and $elseif. }
unit ok_if_expressions;

{$R+}

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

{$IF DEFINED MSWINDOWS}
procedure Windows_IF_DEFINED_ALT;
{$ENDIF}

{$IF UNDEFINED MSWINDOWS}
procedure Windows_IF_UNDEFINED;
{$ENDIF}

{$IF OPTION(R+)}
procedure OptionR_IF;
{$ENDIF}

{$IF false}
procedure False_IF;
{$ENDIF}

{$IF true}
procedure True_IF;
{$ENDIF}

{$IF sizeof(Something)}
procedure SizeOf_IF;
{$ENDIF}

{$IF declared(Something)}
procedure Declared_IF;
{$ENDIF}

{$IF NOT DEFINED MSWINDOWS}
procedure NotWindows_IF_NOT_DEFINED;
{$ENDIF}

{$IF DEFINED(MSWINDOWS) = defined(UNIX)}
procedure WindowsAsUnix_IF_EQUALS;
{$ENDIF}
{$IF DEFINED(POSIX) = defined(UNIX)}
procedure POSIXAsUnix_IF_EQUALS;
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
