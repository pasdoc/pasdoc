//< Back comment without anything before --- should produce a warning.

{ First descr of unit warning_back_comment }

unit warning_back_comment;

{< Descr of unit warning_back_comment }

interface

type
  T = byte;

type //< should produce a warning "no item declared right before" instead of glueing to T
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

  V4: string; {< descr of V4 after a keyword}
  
implementation

end.