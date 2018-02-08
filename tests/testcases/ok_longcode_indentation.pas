unit ok_longcode_indentation;

interface

{ Should cut nothing:

  @longCode(#one
  two
    three
      #) }
procedure Foo1;

{ Should cut 4 spaces:

  @longCode(#
    one
    two
    three
  #) }
procedure Foo2;

{ Should cut 2 spaces:

  @longCode(#
    one
  two
    three
  #) }
procedure Foo3;

{ Should cut nothing (there's a tab here):

  @longCode(#
    one
	two
    three
  #) }
procedure Foo4;

{ Should cut tab + 2 spaces:

  @longCode(#
	  one
	    two
	  three
  #) }
procedure Foo5;

{ Should cut 4 spaces (there's trailing whitespace in 1st lines here, that should be ignored):

  @longCode(#
                 
                  
    one                  
  #) }
procedure Foo6;

{ Should cut 4 spaces (empty line doesn't shorten IndentationPrefix):

  @longCode(#
    if something then
    begin
      // empty line below

    end;
  #) }
procedure Foo7;

implementation