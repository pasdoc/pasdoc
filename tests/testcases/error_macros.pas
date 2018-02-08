{ @abstract(This is a test what will happen when parsing error
  will occur within a macro.)

  pasdoc should print error message that clearly indicates
  where an error occured --- when expanding macro FOO.
}

unit error_macros;

{$define FOO := record implementation}

FOO