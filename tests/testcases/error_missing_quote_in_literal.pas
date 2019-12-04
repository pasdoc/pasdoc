{ Bug: tokenizer got screwed up on a string with missing quote.
  While this is surely an invalid input, PasDoc crashed with AV on a certain file.
  Fix added check for newline chars during reading literal so this test must produce
  special "invalid char in literal" error instead of "unexpected end of stream".

  Invalid code could likely live for ages even in production sources inside IFDEF
  blocks that contain unused stuff and never gets compiled but PasDoc anyway needs
  to tokenize the whole file }

unit error_missing_quote_in_literal;

interface

{$IFDEF OLD_STUFF}
const
  s = 'foo'bar';
{$ENDIF}

implementation

end.