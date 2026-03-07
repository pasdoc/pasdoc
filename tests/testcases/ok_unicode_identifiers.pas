{ Delphi allows any Unicode character in an identifier, see
  https://stackoverflow.com/questions/62143736/what-unicode-characters-can-be-used-in-delphi-source-code
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Identifiers

  Here we save file with Windows 1252 encoding and some German characters.
}
unit ok_unicode_identifiers;
interface
type
  TMyClass = class
    pmEntLöchen: TPopupMenu;
    lbledtHöhe: TLabeledEdit;
    btnAufträge: TButton;
  end;

  TfrmArbeitsÜbersicht = class
    pmEntLöchen: TPopupMenu;
  end;

implementation
end.