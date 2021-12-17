unit ok_inline_var;

interface

procedure Foo;
function EdgeConsoleLog(const sender: ICoreWebView2; const args: ICoreWebView2DevToolsProtocolEventReceivedEventArgs): HResult;

// intf var
var t0: Byte;

implementation

// impl var
var t1: Byte;

procedure Foo;
// local var
var t2: Byte;
begin
  var t3: Byte = 1; // inline var
  for var t4: Byte = 1 to 2 do; // inline loop var
  var t5 := 1; // inline var without explicit type
end;

end.
