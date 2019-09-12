unit ok_auto_back_comments; // Test auto back comments

interface // should be ignored - no idents in list
{ }

var
  var1: Byte; // this is var1 (usual comment)
  var2: Byte;// this is var2 (no whitespace after ident)
  var3: Byte;
  // this is var4 (must not be considered as back-comment)
  var4: Byte;

implementation

end.