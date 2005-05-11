{ -*- compile-command: "pasdoc --output=/tmp/ --marker=: ok_const_1st_comment_missing.pas" -*- }
{:
  See bug [ 1198381 ] "Comment on 1st const does'nt work"

  This must be actually checked with special command-line (the fact that
  marker is present and non-optional was crucial here):

    pasdoc --output=/tmp/ --marker=: ok_const_1st_comment_missing.pas

  Unfortunately automatic testing versus correct_tests_output does not
  test this testcase sufficiently (for now).
}
unit ok_const_1st_comment_missing;

interface

const
  //Range 600: bla bla

  //: bla bla copy.
  ec_COPY           = 600;
  //: bla bla cut.
  ec_CUT            = 601;
  //: bla bla paste.
  ec_PASTE          = 602;

implementation

end.