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

{ PasDoc is a documentation generator for Pascal code.
  See https://pasdoc.github.io/ for more information.

  This is the main console program file,
  which simply calls the Main procedure defined in the PasDoc_Main unit.
  This allows the PasDoc_Main unit to be processed by PasDoc itself,
  generating documentation for the main logic of the application.
}

program pasdoc;

{$ifdef MSWINDOWS}
  {$apptype CONSOLE}
{$endif}

uses
  PasDoc_Main;
begin
  Main;
end.
