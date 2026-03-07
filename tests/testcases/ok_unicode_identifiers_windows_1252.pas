{ Delphi allows any Unicode character in an identifier, see
  https://stackoverflow.com/questions/62143736/what-unicode-characters-can-be-used-in-delphi-source-code
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Identifiers

  Here we save file with Windows 1252 encoding and some German characters.

  Note that using Windows 1252 encoding (not UTF-8) is not fully properly
  handled by PasDoc now: we just "pass through" the identifiers
  (we don't convert them to UTF-8), so PasDoc will not complain about them.
  But we place them in output like HTML or SimpleXML that is supposed to be UTF-8.
  So the PasDoc generation will pass, but

  - Resulting HTML or SimpleXML are not correct. HTML will show weird characters,
    SimpleXML will not pass xmllint validation (as it's not really UTF-8).

  - Moreover, resulting filenames may be wrong (if you used Unicode characters
    in unit or class names), this also may mean you cannot stage them with GIT.

  We recommend to use UTF-8 encoding for such source files.
  See ok_unicode_identifiers_utf8.pas test for the same source file
  saved with UTF-8 encoding, which is 100% properly handled by PasDoc.
}
unit ok_unicode_identifiers_windows_1252;
interface
type
  TMyClass = class
    pmEntL�chen: TPopupMenu;
    lbledtH�he: TLabeledEdit;
    btnAuftr�ge: TButton;
  end;

  TfrmArbeits�bersicht = class
    pmEntL�chen: TPopupMenu;
  end;

implementation
end.