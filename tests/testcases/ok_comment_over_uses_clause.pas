unit ok_comment_over_uses_clause;

interface

{ This comment shouldn't be assigned to anything. }

uses Unit2, Unit3;

procedure A;

implementation

end.