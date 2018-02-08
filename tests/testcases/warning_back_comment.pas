//< Back comment without anything before --- should produce a warning.

{ First descr of unit warning_back_comment }

unit warning_back_comment;

{< Descr of unit warning_back_comment }

interface

type
  TMyEnum = (
  {< Descr of TMyEnum }
    { First descr of meOne } meOne //< Descr of meOne
  );

var
  { First descr of V1 }
  V1,
  { First descr of V2 }
  V2: Integer; //< descr of V1 and V2

  //<Another back comment without anything before --- should produce a warning.

implementation

end.