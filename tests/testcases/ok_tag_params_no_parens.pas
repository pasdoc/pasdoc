{ @abstract This is a demo unit using tags without enclosing them in ()

  Parsing logic is simple: if a tag requires some parameters
  but you don't put open paren '(' char right after it,
  then tag parameters are understood to span to the end of line
  (or to the end of comment).

  This doesn't break compatibility with documentation that enclosed
  parameters in (), because tags that have parameters were
  *required* to have '(' char after them. So they will still be
  correctly seen and parsed to the matching closing paren.

  See @link(SomeProc) for more examples and comments.

  @author Michalis
  @created 2005-05-04
  @lastmod 2005-05-04
  @cvs $Author: kambi $ }
unit ok_tag_params_no_parens;

interface

type
  EFoo = class(Exception);

(*
  Note that this rule allows you to not specify () for *any* tag
  that has parameters. Even for @@link tag:
  @link ok_tag_params_no_parens

  This rule doesn't create any problems for tags without parameters,
  like the @@name tag: here it is: @name. Such tags never have parameters,
  and on the above line you *don't* have @@name tag with
  parameters "tag. Such tags never have parameters,".
  Instead, you just specified @name tag and
  "tag. Such tags never have parameters," is just a normal text.

  Check out this longcode:
  @longcode# begin Writeln('Hello world'); end; { This works ! } #

  See also @@html and @@latex tags:
  @html <font style="color: #ff0000">I'm red</font>
  @latex {\bf I'm bold.}

  And here is some code: @code begin X := Y + 1; end;

  @raises EFoo when it's in bad mood
  @param A means something
  @returns Some integer *)
function SomeProc(A: Integer): Integer;

implementation

end.