{ @abstract(This is a test what will happen when you try to recursively
  expand a macro.)
  
  pasdoc should exit with appropriate error message. Not something
  ugly, like out of memory (or hanging your system and eating
  all mem it can).
}

unit error_macros_recursive;

interface

{$define FOO:=FOO}
FOO

implementation

end.