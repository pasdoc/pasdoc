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

{$I pasdoc_versions.inc}

{ Use TBufferedFileStream, always now both in FPC and Delphi:
  https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Classes.TBufferedFileStream
  https://www.freepascal.org/docs-html/fcl/bufstream/tbufferedfilestream.html . }
{$DEFINE USE_BUFFERED_STREAM}

{$IFDEF COMPILER_7_UP}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  (* Disables .NET warnings for Delphi 7 and later. *)

  {$IFDEF COMPILER_10_UP}
    {$IFDEF COMPILER_12_UP}
      {$DEFINE STRING_UNICODE}

      {$IFNDEF DELPHI_15_UP}
        { Delphi 12 and 14 generated additional code for string }
        { compatibility with C++ Builder.                       }
        {$STRINGCHECKS OFF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF} // Delphi

{ Detect regular expressions unit. }
{$ifdef FPC}
  { FPC RegExpr unit in FPC >= 2.6.0.
    Earlier FPC versions have incompatible, and less complete, regexpr unit,
    we do not use it for now. }
  {$define FPC_RegExpr}
  {$ifdef VER2_0} {$undef FPC_RegExpr} {$endif}
  {$ifdef VER2_2} {$undef FPC_RegExpr} {$endif}
  {$ifdef VER2_4} {$undef FPC_RegExpr} {$endif}
{$else}
  {$IFDEF COMPILER_15_UP}
    {$define DELPHI_RegularExpressions}
  {$endif}
{$endif}

{ If GENERICS_CONSTREF is defined, then various routines used with Generics.Collections
  (like callbacks we pass to TComparer, or OnNotify callback or Notify virtual method)
  should have "constref" parameter, not "const".
  This was the case in FPC <= 3.2.0.
  FPC changed constref->const in
  https://gitlab.com/freepascal.org/fpc/source/-/commit/693491048bf2c6f9122a0d8b044ad0e55382354d
  and the change is present in FPC fixes branch 3.2.3 and later 3.2.4(rc1). }
{$ifdef VER3_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2_2} {$define GENERICS_CONSTREF} {$endif}

// Needed by naive_dictionary_fix.inc
{$ifdef FPC}
  {$modeswitch advancedrecords}
{$endif}

{ FPC 3.3.1 produces many false warnings. }
{$ifdef FPC}
  {$ifndef VER3_0}
    {$ifndef VER3_2}
      { 2 warnings below are common because of case analysis.
        See https://castle-engine.io/coding_conventions#case_analysis .
        Adjusting code to avoid these warnings is possible in some cases,
        but is troublesome (not all cases have nice solutions, and it is even
        harder when we also have to support older FPC versions and Delphi
        that don't do this analysis).
        So we disable these warnings for now.

        Note: For unknown reasons, 2 warnings
        "Warning: (6060) Case statement does not handle all possible cases"
        in PasDoc_Parser.pas are still generated. }
      {$warn 6060 off} // case statement does not handle all possible cases
      {$warn 6018 off} // unreachable code

      { This happens because unfortunately FPC 3.3.1 doesn't understand
        that writing e.g. "WriteBuffer(Tmp, SizeOf(Tmp));" means we use Tmp value. }
      {$warn 5027 off} // Local variable is assigned but never used
    {$endif}
  {$endif}
{$endif}
