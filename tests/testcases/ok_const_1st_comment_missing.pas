{:
  See bug [ 1198381 ] "Comment on 1st const does'nt work"
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