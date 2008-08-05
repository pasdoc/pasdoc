{ ------------------------------------------------------------------------------

  PasDoc

  * generates documentation from comments in Pascal unit source code files
  * command line program
  * written in Delphi
  * output formats Html and HtmlHelp
  * try PasDoc on its own source code

  * distributed under the GNU General Public License (GPL)

  * copyright (C) 1998-2000 by Marco Schmidt
  * copyright (C) 2001-2003 by Ralf Junker <delphi@zeitungsjunge.de>
  * Copyright (C) 2003 by Johannes Berg <johannes@sipsolutions.de>

  Hint:

  Whenever you use PasDoc for documentations, make sure the program file
  contains no code except for a call to a main routine in another unit or
  the instantiation of an object / class that does all the work
  (usually TApplication).
------------------------------------------------------------------------------ }

program pasdoc;

{$IFNDEF VPASCAL}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$ifdef USE_FASTMM} FastMM4, {$endif}
  PasDoc_Main;

begin
  Main;
end.
