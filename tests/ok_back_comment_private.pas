{ Testcase for
  [https://sourceforge.net/tracker/index.php?func=detail&aid=1596563&group_id=4213&atid=104213].
  
  The comment "This should be ether omitted..." should not be included
  anywhere (if private members are omitted, and this is the default).
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