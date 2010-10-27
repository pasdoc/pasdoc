{$I pasdoc_versions.inc}

{$IFDEF DELPHI_7_UP}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}
(* Disables .NET warnings for Delphi 7 and later. *)

{$IFDEF COMPILER_10_UP}
  {$DEFINE USE_INLINE}
  {$IFDEF COMPILER_12_UP}
    {$DEFINE STRING_UNICODE}
  {$ENDIF}
{$ENDIF}

{$ifdef FPC}
  { Turn macro on to get FPC_VERSION, FPC_RELEASE, FPC_PATCH macros,
    used in PasDoc.pas. Also to change "out" to "var" for FPC 1.0.x. }
  {$macro on}
{$endif}
