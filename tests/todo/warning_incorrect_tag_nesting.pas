unit warning_incorrect_tag_nesting;

interface

type
  EFoo = class(Exception);

{ There is still no control inside PasDoc *what* tags
  can be embedded inside what (e.g. you can't embed inside @@code
  inside @@code, you can't embed @@raises inside @@raises etc.
  but PasDoc will not even warn you about it).

  pasdoc should display a warning/error on such input:

  @code(This is a code @code(inside a code).)

  @raises(@raises(EFoo What are you doing, using raises inside @@raises ?!?))
}
procedure Test_Not_Working_1;

implementation

end.