{ pasdoc's LaTeX documentation of this unit omitted many things
  before revision 1.20 of PasDoc_GenLatex.pas. Html docs were ok.

  I explained why omitting docs for things that don't have
  any documentation comment is bad in the letter
  "Fix for LaTeX genetator omitting some things"
  [http://sourceforge.net/mailarchive/forum.php?thread_id=6948809&forum_id=4647]

  Now, all identifiers in this unit should be visible in LaTeX documentation
  (and in html output too, of course).
}

unit ok_nodescription_printing;

interface

type
  TClass1 = class
    MyField: Integer;
    procedure MyMethod;
    property MyProperty read FMyProperty;
  end;

  TMyRecord = record
  end;

  { This record has a description, but still LaTeX output will not list
    it in it's summary list (in the "Overview" section).
    (i.e. before applying my patch) }
  TMyRecord2 = record
  end;

  TMyRecord3 = record
    MyRecField: Integer;
  end;

  TMySimpleType = Integer;

procedure MyProc;

const
  MyConst = 9;

var
  MyVariable: Integer;

implementation

end.