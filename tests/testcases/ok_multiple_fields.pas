unit ok_multiple_fields;

interface

type
  TRec1 = record
    { Descr of F1, F2, F3 }
    F1, F2, F3: Integer;
    F4: Integer;
    { Descr of F5 and F6 }
    F5, F6,
      { Descr of F7 and F8 }
      F7, F8,
      { Descr of F9 and F10 }
      F9, F10: Integer;
    F11: Integer;
    F12,
    { Descr of F13 and F14 }
    F13, F14,
      { Descr of F15 }
      F15: Integer;
  end;

  TRec2 = record
    case Integer of
      0: (
        { Descr of F1, F2, F3 }
        F1, F2, F3: Integer;
        F4: Integer;
        { Descr of F5 and F6 }
        F5, F6,
          { Descr of F7 and F8 }
          F7, F8,
          { Descr of F9 and F10 }
          F9, F10: Integer;);
      1: (
        F11: Integer;
        F12,
        { Descr of F13 and F14 }
        F13, F14,
          { Descr of F15 }
          F15: Integer;);
  end;

var
  { Descr of F1, F2, F3
    (multiline) }
  F1, F2, F3: Integer;
  F4: Integer;
  { Descr of F5 and F6
    (multiline) }
  F5, F6,
    { Descr of F7 and F8
      (multiline) }
    F7, F8,
    { Descr of F9 and F10
      (multiline) }
    F9, F10: Integer;
  F11: Integer;
  F12,
  { Descr of F13 and F14
    (multiline) }
  F13, F14,
    { Descr of F15
      (multiline) }
    F15: Integer;

implementation

end.