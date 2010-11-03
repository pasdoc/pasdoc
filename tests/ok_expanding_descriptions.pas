{ @abstract(This is a test of tags expanded by TPasItem handlers.
    Of course with @@abstract tag using some recursive tag:
    See also @link(TestPasMethodTags))

  This whole unit is actually a big test of many things
  related to pasdoc's @@-tags.

  @author(Michalis <my@email.address>)
  @created(2005-03-30)
  @cvs($Author: kambi $)
  @lastmod(2005-03-30)

  See also @link(TMyClass) for other test of @@cvs tag (with $Date,
  as an alternative specification of @@lastmod)
}

unit ok_expanding_descriptions;

interface

{ Write two at chars, like this @@@@, to get one @@ in output.

  E.g. @@ link(TSomeClass).

  E.g. @@link(TSomeClass).

  E.g. @@html foobar.

  E.g. @@link . }
procedure TwoAt;

{ aa aaaaa aa aaa
  @code(SHGetSpecialFolderPath(0, @@Path, CSIDL_APPDATA, true))
  aaaa aaaaaa aaaaaa aaaaaaaaa aaaa

  At some point, this test caused the bug:
  final </code> tag was inserted in converted form (processed with
  ConvertString) into html output.
  In effect, there was an opening <code> tag but there was
  no closing </code> tag. }
procedure RecursiveTwoAt;

(* Note that inside @@longcode below I should be able to write
  singe @@ char to get it in the output, no need to double it
  (like @@@@). No tags are expanded inside longcode.

  Also note that paragraphs are not expanded inside longcode
  (no <p> inside <pre>...</pre> in html output).

  Of course html characters are still correctly escaped
  (< changes to &lt; etc.).

  @longcode(#

    procedure Foo;
    begin
      if A < B then Bar; { @link(No, this is not really pasdoc tag) }
    end;

    procedure Bar(X: Integer);
    begin
      CompareMem(@X, @Y);
    end;
  #)
*)
procedure TestLongCode;

(*
  @html(
    This is some <b>dummy</b> html code, just to show that inside
    @html tag of pasdoc (note that I used single @ char in this sentence)
    nothing is expanded by pasdoc.

    No paragraphs are created by pasdoc. (This text is still in the 1st,
    not in the 2nd, paragraph in html output)

    <p>You must explicitly write &lt;p&gt; to get paragraph.
    No tags work, e.g. @link(TestLongCode).
  )

  @latex(
    This is some {\bf dummy} \LaTeX code, just to show that inside
    @latex tag of pasdoc (note that I used single @ char in this sentence)
    nothing is expanded by pasdoc.
    No paragraphs are created by pasdoc. Although, in case of LaTeX output,
    LaTeX rules for making paragraphs are the same as the pasdoc's rules
    (one empty lines marks paragraph), so you will not notice.
    This is brutal line-break: \\
    I'm still in the same paragraph, after line-break.

    I'm 2nd paragraph.
    No tags work, e.g. @link(TestLongCode).
  )

  Note that text inside @@html / @@latex tags is absolutely not touched by pasdoc.
  Characters are not escaped (< is *not* changed to &lt; in the html case),
  @@tags are not expanded, @@ needs not to be doubled, paragraphs
  (<p> in the html case) are not inserted.
*)
procedure TestHtmlAndLatexTags;

type
  EFoo = class(Exception) end;
  EBar = class(Exception) end;
  EXyz = class(Exception) end;

{ @@code and @@returns (and some others) tags are recursive,
  you can freely put other tags inside.

  @code(This is link to @link(TestHtmlAndLatexTags).)

  @raises(EFoo in case @link(TestHtmlAndLatexTags)
    returns value >= 4 (actually, this is just a test text).)
}
procedure TestRecursiveTag;

{ This is a test of tags expanded by TPasMethod handlers.
  Note that all three tags are expanded recursively.

  @param(A means sthg about @link(TestRecursiveTag))
  @param(B also means sthg. @code(Code inside.))

  @returns(You can make tags recursion any level deep :
    @code(This is a code with a link to @link(TestRecursiveTag)))

  @raises(EFoo when you do sthg nasty, like call @link(TestRecursiveTag)
    when you're not supposed to)
  @raises(EBar when code @code(if 1 = 0 then DoSomething;) will work as expected.)
}
function TestPasMethodTags(A, B: Integer): string;

type
  TMyClassAncestor  = class
  private
    MyField: boolean;
  public
  //@abstract(This comment should be inherited.)
    property inheritable: boolean read MyField;
  end;

  { These are some tags that are not allowed to have parameters:
    name @name, inherited @inherited, nil @nil, true @true, false @false,
    classname @classname. Some of them are valid only within a class,
    but this is not important for this test.

    @cvs($Date: 2010-04-26 04:04:35 +0200 (pon) $) }
  TMyClass = class(TMyClassAncestor)
  published
  //This is the detailed description.
    property inheritable;
  end;

{ And a test of @exclude.
  This should not be seen in final output! }
procedure TestExclude;

implementation

end.
