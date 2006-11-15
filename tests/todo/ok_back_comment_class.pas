{ This should report a warning that @@exclude is used twice for
  TfmHardwareControlSetup identifier. But instead:

  @orderedList(
    @item(An @@exclude is assigned to the whole unit)
    @item(Even worse, it causes dummy output
      @preformatted(
Info[1]:    Starting Source File Parsing ...
Info[2]:    Now parsing file ok_back_comment_class.pas...
Info[2]:    ... 1 Source File(s) parsed
Fatal Error: At least one unit must have been successfully parsed to write docs.
)))
}
unit ok_back_comment_class;

interface

type
  { @exclude }
  TfmHardwareControlSetup = class(TForm)  //< @exclude
  end;

implementation

end.