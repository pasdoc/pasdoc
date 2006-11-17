unit warning_back_comment_over_uses_clause;

interface

uses Unit2, Unit3; {< This comment shouldn't be assigned to anything. }

implementation

end.