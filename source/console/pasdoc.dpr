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

{ PasDoc

  * generates documentation from comments in Pascal unit source code files
  * command line program (GUI version also available, see ../gui/,
    as well as components for Lazarus and Delphi)
  * written in ObjectPascal (can be compiled by FPC or Delphi)
  * output formats Html, HtmlHelp, LaTeX
  * try PasDoc on its own source code (see ../autodoc/)

  * copyright (C) 1998-2000 by Marco Schmidt
  * copyright (C) 2001-2003 by Ralf Junker <delphi@zeitungsjunge.de>
  * Copyright (C) 2003 by Johannes Berg <johannes@sipsolutions.de>
  * Copyright 2005-2010 by Michalis Kamburelis, Richard B. Winston
    and other contributors, see ../../ChangeLog file

  Hint:

  Whenever you use PasDoc for documentations, make sure the program file
  contains no code except for a call to a main routine in another unit or
  the instantiation of an object / class that does all the work
  (usually TApplication).

  Pasdoc is restricted to work on unit files only, that's why the program file
  should contain no actual program-specific code - it would not become part of
  the documentation.

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
