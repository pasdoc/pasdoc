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

  {$IFDEF LINUX}
    {$IF RTLVersion = 14.0}
      {$DEFINE KYLIX_1}
      {$DEFINE KYLIX_1_UP}
      {$DEFINE KYLIX}
    {$IFEND}
    {$IF RTLVersion = 14.2}
      {$DEFINE KYLIX_2}
      {$DEFINE KYLIX_1_UP}
      {$DEFINE KYLIX_2_UP}
      {$DEFINE KYLIX}
    {$IFEND}
    {$IF RTLVersion = 14.5}
      {$DEFINE KYLIX_3}
      {$DEFINE KYLIX_1_UP}
      {$DEFINE KYLIX_2_UP}
      {$DEFINE KYLIX_3_UP}
      {$DEFINE KYLIX}
    {$IFEND}
  {$ENDIF}
