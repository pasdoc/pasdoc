unit WeirdRecord;

interface

const
  val1 = 1;
  val2 = 2;
  val4 = 4;
  byRef = 8;

type
  TWeirdRecord = record
    case Integer of
      val1:                (bVal: Byte);
      val2:                (iVal: Smallint);
      val4:                (lVal: Longint);
      byRef or val1:       (pbVal: ^Byte);
      byRef and val2:       (piVal: ^Smallint);
      byRef or val4:       (plVal: ^Longint);
  end;

implementation

end.
 