unit ok_record_case_parsing;

interface

type
  TRec1 = record
    case boolean of
      false: (A: Integer);
      true: (B: Integer);
  end;

  TRec2 = record
    case Field1: boolean of
      false: (A: Integer);
      true: (B: Integer;);
  end;

  { @member C Description of C }
  TRec3 = record
    case Integer of
      0: (
        { Descr of A }
        A: Integer;
        { Descr of B }
        B: ShortString;);
      1: (
        C: Integer);
      2: (
        { Descr of D, E, F }
        D, E, F: Integer;
        G: ShortString;);
      3: (
        { Descr of H }
        H: Integer);
  end;

  TRec4 = record
    case boolean of
      false: (
        case Integer of
          0: (A: Integer);
          1: (B: Integer) );
      true: (C: Integer);
  end;

implementation

end.