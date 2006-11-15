{ Testcase for
  [https://sourceforge.net/tracker/index.php?func=detail&aid=1596563&group_id=4213&atid=104213].

  When run with
@preformatted(
  pasdoc ok_back_comment_private.pas --visible-members public,private --output=/tmp/
)
  it's OK. But when run with
@preformatted(
  pasdoc ok_back_comment_private.pas --output=/tmp/
)
  the comment "This should be ether omitted" is incorrectly assigned
  to ActionDescription.
}

unit ok_back_comment_private;

interface

const
  ActionDescription = 'blah';

type
  EPingError = class(Exception)
  private
    FErrorType: TPingErrorType;
    //< This should be ether omitted or put for FErrorType: TPingErrorType;
  end;

implementation

end.