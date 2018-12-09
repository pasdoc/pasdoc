unit StaticMember;

interface

type
  TMyClass = class
    // All methods are static unless they are virtual or dynamic.
    // but @name is declared static.  Even if 'static' did not
    // appear after the declaration, @name would still be static.
    // @name is also a Class procedure but that is a separate story.
    Class Procedure StaticProcedure; static;
  end;

implementation

{ TMyClass }

class procedure TMyClass.StaticProcedure;
begin

end;

end.
