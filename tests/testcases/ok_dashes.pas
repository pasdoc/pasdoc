{ @abstract(Test of various dashes.)

  Triple dash produces em-dash, for separating parts of sentence and such,
  like "I know a secret --- but I won't tell".

  Double dash produces en-dash, intended to use for numbers ranges,
  like "10--20".

  Normal single dash is a short dash, for compound words,
  like "variable-width font".

  You can write @@@- in cases where you really want to write
  just 2 or more consecutive short dashes. E.g. @--long-option-name
  (here I escaped only the 1st "-", this means that the rest
  of dashes is also treated as a short dash),
  or -@-long-option-name (here I escaped only the 2nd dash),
  or @-@-long-option-name (here I escaped two first dashes,
  which wasn't really necessary, it's sufficient to escape
  either 1st or the 2nd dash), @-@-long@-option@-name
  (here I escaped all dashes; this looks unnecessary ugly
  in source code, but it's correct).
}
unit ok_dashes;

interface

implementation

end.