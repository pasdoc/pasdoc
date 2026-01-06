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

{ @abstract(Information about PasDoc and compilers version.) }
unit PasDoc_Versions;

{$I pasdoc_defines.inc}

interface

{ ---------------------------------------------------------------------------- }
{ Compiler Identification Constants }
{ ---------------------------------------------------------------------------- }

{ Nice compiler name.
  This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function COMPILER_NAME: string;

const
  COMPILER_BITS =  {$ifdef CPU64} '64' {$else} '32' {$endif};

{$IFDEF LINUX}
  COMPILER_OS = 'Linux';
{$ENDIF}
{$IFDEF MSWINDOWS}
  COMPILER_OS = 'MSWindows';
{$ENDIF}
{$IFDEF BEOS}
  COMPILER_OS = 'BeOS';
{$ENDIF}
{$IFDEF QNX}
  COMPILER_OS = 'QNX';
{$ENDIF}
{$IFDEF AMIGA}
  COMPILER_OS = 'AmigaOS';
{$ENDIF}
{$IFDEF SUNOS}
  COMPILER_OS = 'SunOS';
{$ENDIF}
{$IFDEF GO32V2}
  COMPILER_OS = 'DOS/Go32v2';
{$ENDIF}
{$IFDEF OS2}
  COMPILER_OS = 'OS/2';
{$ENDIF}
{$IFDEF FREEBSD}
  COMPILER_OS = 'FreeBSD';
{$ENDIF}
{$IFDEF DARWIN}
  COMPILER_OS = 'Darwin';
{$ENDIF}

  { ---------------------------------------------------------------------------- }
  { PasDoc Version Constants }
  { ---------------------------------------------------------------------------- }

  {  }
  PASDOC_NAME = 'PasDoc';

  { Date of this pasdoc release, for stable releases.
    Otherwise just <snapshot>.

    We avoid using version control $ Date or such keyword, as it would only
    indicate the date when this file was modified. So this is neither
    the date of last stable release, nor the date of last commit.

    Also, different version control systems have different (or none) support
    for this.
  }
  //PASDOC_DATE = '2021-02-07';
  PASDOC_DATE = 'snapshot';
  { }
  PASDOC_VERSION = '0.17.0.snapshot';
  { }
  PASDOC_NAME_AND_VERSION = PASDOC_NAME + ' ' + PASDOC_VERSION;
  { }
  PASDOC_HOMEPAGE = 'https://pasdoc.github.io/';

{ Returns pasdoc name, version, used compiler version, etc.

  This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function PASDOC_FULL_INFO: string;

implementation

uses SysUtils;

function COMPILER_NAME: string;
begin
  COMPILER_NAME :=
    {$IFDEF FPC}
    'FPC ' + Format('%d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);
    {$define COMPILER_VERSION_DEFINED}
    {$ENDIF}

    {$IFDEF KYLIX_1}   'KYLIX 1';      {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF KYLIX_2}   'KYLIX 2';      {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF KYLIX_3}   'KYLIX 3';      {$define COMPILER_VERSION_DEFINED} {$ENDIF}

    {$IFDEF DELPHI_6_UP}
    Format('DELPHI (Compiler Version %2.1f)', [CompilerVersion]);
    {$define COMPILER_VERSION_DEFINED}
    {$ENDIF}

    {$IFDEF DELPHI_5}  'DELPHI 5';     {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF DELPHI_4}  'DELPHI 4';     {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF DELPHI_3}  'DELPHI 3';     {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF DELPHI_2}  'DELPHI 2';     {$define COMPILER_VERSION_DEFINED} {$ENDIF}
    {$IFDEF DELPHI_1}  'DELPHI 1';     {$define COMPILER_VERSION_DEFINED} {$ENDIF}

    {$IFNDEF COMPILER_VERSION_DEFINED}
    'UNKNOWN';
    {$ENDIF}

  {$undef COMPILER_VERSION_DEFINED} { symbol no longer needed }
end;

function PASDOC_FULL_INFO: string;
begin
  PASDOC_FULL_INFO :=
    PASDOC_NAME_AND_VERSION + ' [' + PASDOC_DATE + '|' +
      COMPILER_NAME + '|' + COMPILER_OS + '|' + COMPILER_BITS + ']';
end;

end.
