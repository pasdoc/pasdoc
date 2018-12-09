unit warning_not_existing_tags;

interface

{ pasdoc should complain (display warnings) about
  wrong tags : @firstwarning(blah blah),
  @code(Wrong tag inside a @@code: @secondwarning(ble ble)). }
procedure TestWarnings;

implementation

end.