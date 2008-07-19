unit FPL4D7;
(* FreePascal Library for Delphi (7)
  Only hacks, so far :-(
*)

interface

function  GetAppConfigDir(fWhat: boolean = False): string;

implementation

{$IFDEF CONSOLE}
  //uses what?
{$ELSE}
uses
  Forms, SysUtils;  //Application
{$ENDIF}

function  GetAppConfigDir(fWhat: boolean): string;
begin
(* return the config directory, including(?) trailing separator
  Should return the app-specific AppData directory.
*)
  Result := Application.ExeName;
  Result := ExtractFileDir(Result);
end;

end.
