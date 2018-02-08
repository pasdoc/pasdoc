unit ok_accidental_exclude;

interface

{ @@exclude.
  This test shows that simplistic testing for @@exclude tag in pasdoc
  is not good -- since it excludes this item from documentation,
  but it shouldn't. }
procedure I_Dont_Wanna_Be_Excluded;

implementation

end.