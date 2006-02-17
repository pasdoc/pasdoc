{ @abstract(This is a test of @@include tag.)

  Behold included file ok_include_1.txt:
  @include(ok_include_1.txt)

  Behold file ok_include_1.txt that is included for the 2nd time here:
  @include(ok_include_1.txt)

  Behold file ok_include_1.txt that is included for the 3rd time
  here, and this time it's inside @@bold:
  @bold @include(ok_include_1.txt)

  Take a look at @link(ok_include_intro Introduction) too.
}
unit ok_include;

interface

implementation

end.