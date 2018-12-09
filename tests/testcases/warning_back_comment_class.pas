unit warning_back_comment_class;

{ This should report a warning that @@exclude is used twice for
  TfmHardwareControlSetup identifier.

  But (before fixing):

  @orderedList(
    @item(An @@exclude was assigned to the whole unit)
    @item(Even worse, it caused dummy output
      @preformatted(
Info[1]:    Starting Source File Parsing ...
Info[2]:    Now parsing file ok_back_comment_class.pas...
Info[2]:    ... 1 Source File(s) parsed
Fatal Error: At least one unit must have been successfully parsed to write docs.
)))
}

interface

type
  { @exclude one }
  TfmHardwareControlSetup = class(TForm)  //< @exclude two
  end;

implementation

end.
