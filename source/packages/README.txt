Here you can find packages to install PasDoc components
in Lazarus and Delphi IDE.

For Delphi, we provide separate packages for specific Delphi versions.
The Kylix (discontinued "Delphi for Linux" project from Borland) packages
are inside the delphi/kylix3/ subdirectory.

For Lazarus, we just provide one package for the latest version of Lazarus.
Lazarus is free and you can always upgrade it, so this simple approach
works for everyone:) Also, Lazarus packages are cross-platform,
there is no need to maintain different package files for different platforms.

Note for Lazarus 0.9.6 beta and older:
  You must manually select the "Use Ansi Strings (-Sh)" compiler option
  (under "Parsing"). This will not be automatically selected because
  Lazarus >= 0.9.7 stores this setting in <SyntaxOptions> XML element
  in pasdoc_package.lpk, while 0.9.6 reads <SymantecChecking> element.

Package name is "pasdoc_package", not just "pasdoc", to avoid
any name conflicts with unit PasDoc.pas and the pasdoc program.
Version of the package reflects version stored in PASDOC_VERSION
constant in PasDoc unit.
