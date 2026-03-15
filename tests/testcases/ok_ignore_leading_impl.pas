unit ok_ignore_leading_impl;

interface

//--------------------------------------------------------------------------------
// Test function.
//
// @param arg1 Argument 1
// @param arg2 Argument 2
// @return @True or @False
//--------------------------------------------------------------------------------
function testfnc_only_intf(arg1: Integer; arg2: String): Boolean;

// Test function.
function testfnc(arg1: Integer; arg2: String): Boolean;

implementation

//--------------------------------------------------------------------------------
// Test function.
//
// @param arg1 Argument 1
// @param arg2 Argument 2
// @return @True or @False
//--------------------------------------------------------------------------------
function testfnc(arg1: Integer; arg2: String): Boolean;
begin
  result := False;
end;

end.
