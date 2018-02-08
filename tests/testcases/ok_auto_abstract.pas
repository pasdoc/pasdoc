{$ifdef FPC} {$mode objfpc} {$endif}

{ This is the 1st sentence, it will be turned into
  @@abstact description of this item. This is the 2nd sentence of the
  description.
}
unit ok_auto_abstract;

interface

type
  { This is the 1st sentence of description.

    This is the 2nd sentence of description.

    @abstract(This is the explicit abstract section) }
  TTest1 = class
  end;

  { In this case there is no period char '.' that is followed by whitespace
    in this comment, so the whole comment will be treated as abstract
    description }
  TTest2 = class
  end;

  { Of course, 1st sentence may contain other tags,
    like this: @link(TTest1) and like this:
    @code Some code. Not really Pascal code, but oh well...
    and I'm still in the 1st sentence, here the
    @@abstract part ends. This is the 2nd sentence.

    Note that in this example the '.' char inside @@code tag
    did not confuse pasdoc -- it was not treated as the end of
    1st sentence, because it was part of parameters of @@code tag.
    Even though @@code tag in the example above used special
    syntax TagsParametersWithoutParenthesis. }
  TTest3 = class
  end;

  { First sentence, auto-abstracted, and the 1st paragraph at the same time. 

    Notice that html output will add <p> to DetailedDescription, but not
    to AbstractDescription. This is second paragraph. }
  TTest4 = class
  end;

implementation

end.