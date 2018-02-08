// Submitted in thread "Pasdoc tests" on pasdoc-main on 2005-04-11
// by Richard B Winston.
// pasdoc passes it, but the test checks many important things
// (line glueing single-line comments by pasdoc, record with case etc.)
// so it's worth adding it to test cases.
//
// @abstract( This unit is used for converting to and from the BigEndian format.
// See http://community.borland.com/article/0,1410,28964,00.html.)

unit ok_complicated_record;

interface

type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  // @name is a pointer to a @link(TDoubleEndianCnvRec).
  PDoubleEndianCnvRec = ^TDoubleEndianCnvRec;
  { @abstract(@name is used in @link(ConvertDouble) to convert
   a double to or from the BigEndian format.)
   @longcode(#
  TDoubleEndianCnvRec = packed record
    case BytePos of
      EndVal: (EndianVal: double);
      ByteVal: (Bytes: array[0..SizeOf(double) - 1] of byte);
  end;
   #)
   }
  TDoubleEndianCnvRec = packed record
    case BytePos of
      //The value we are trying to convert
      EndVal: (EndianVal: double);
      //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(double) - 1] of byte);
  end;

// @abstract(@name copies @link(TDoubleEndianCnvRec.Bytes)
// in reverse order from Source^ to Dest^.)
// @name is used in @link(ConvertDouble).
procedure SwapDoubleBytes(Dest, Source: PDoubleEndianCnvRec);

// @abstract(@name converts Value to or from the BigEndian format.)
// @param(Value is the value to be converted.)
// @returns(Value after being converted to or from the BigEndian format.)
function ConvertDouble(const Value: double): double;

implementation

end.
