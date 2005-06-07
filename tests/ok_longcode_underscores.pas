{ 2005-06-07: Latex version of docs for this unit are wrong,
  because ConvertString is not called to convert _ char.
  So _ is not escaped and latex fails.
  Also, HTML version is bad, because "With" is formatted in bold,
  because it's treated like keyword.
  
  Fixed by adding _ to AlphaNumeric in PasDoc_Gen in FormatPascalCode.

  @longcode(#
    Identifier_With_Underscores;
  #)
}

unit ok_longcode_underscores;

interface

implementation

end.