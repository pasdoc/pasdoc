unit warning_incorrect_tag_nesting;

interface

type
  EFoo = class(Exception);

{ Now there is control inside PasDoc what tags
  can be embedded inside what.

  pasdoc should display appropriate warnings on following input:

  @code(This is a code @raises(EFoo with a raises inside ???).)

  @raises(EFoo @raises(EFoo What are you doing, using raises inside @@raises ?!?))
  @raises()
  
  @created(2005-05-13 @unknowntag, this will cause a warning 
    about unknown_tag.)
  
  @created(2005-05-13 
    @code(this will cause a warning) that no known tag 
    may be used within @@created.)
}
procedure TestMe;

implementation

end.