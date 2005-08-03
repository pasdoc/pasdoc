(* @abstract(Test of @@preformatted tag.)

  @preformatted(
Line one ?
1111 222 ?
IIII WWW ?

Some    long    space
    Some    long     space

Three empty lines below (*not* converted into paragraph):



-------- end of three empty lines.

Some html and LaTeX special chars, to make sure that ConvertString is
called when it should be: < > & \ { }

Note that @-tags inside are not expanded: @name, and consequently
you don't have to double @ char. Just like within @html and @latex
tags.)

*)

unit ok_preformatted_test;

interface

implementation

end.