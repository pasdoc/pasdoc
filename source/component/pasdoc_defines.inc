{$I pasdoc_versions.inc}

{$IFDEF COMPILER_7_UP}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  (* Disables .NET warnings for Delphi 7 and later. *)

  { USE_BUFFERED_STREAM MUST be defined if STRING_UNICODE is defined as well. }
  {$DEFINE USE_BUFFERED_STREAM}

  {$IFDEF COMPILER_10_UP}
    {$DEFINE USE_INLINE}

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

{$ifdef FPC}
  { Turn macro on to get FPC_VERSION, FPC_RELEASE, FPC_PATCH macros,
    used in PasDoc.pas. Also to change "out" to "var" for FPC 1.0.x. }
  {$macro on}

  {$IFDEF VER2}
    {$DEFINE USE_INLINE}
    {$DEFINE USE_BUFFERED_STREAM}
  {$ENDIF}
{$endif}

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
  {$define DELPHI_RegularExpressions}
{$endif}
