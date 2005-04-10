{@abstract(The abstract tag for @link(TAbstractTerminationClass) is
  unterminated.  PasDoc should either terminate the tag itself, give a warning,
  or both)

  Submitted in thread "Pasdoc tests" 2004-04-10 on pasdoc-main.
  The cause and the cure of this bug are known, 
  it will be fixed after pasdoc's release
  on sourceforge that should happen today (2004-04-10). }
unit AbstractTermination;

interface

Type
  {@abstract(This abstract tag lacks the closing parenthesis.
    How will PasDoc handle this error?}
  TAbstractTerminationClass = Class(TObject)
    DummyField: integer;
  end;

implementation

end.
