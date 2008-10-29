{$I pasdoc_versions.inc}

{$IFDEF DELPHI_7_UP}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$DEFINE Debug} //DoDi
{$ENDIF}
(*- Disables .NET warnings for Delphi 7 and later. *)

{$ifdef FPC}
  {- Turn macro on to get FPC_VERSION, FPC_RELEASE, FPC_PATCH macros,
    used in PasDoc.pas. Also to change "out" to "var" for FPC 1.0.x. }
  {$macro on}
{$endif}
