{ @abstract(Test that record alignment syntax "end align(N)" is accepted.)
  Delphi supports specifying record alignment with the align directive
  after the end keyword. }
unit ok_record_align;

interface

type
  TAlignedRecord = record
    Field1: Word;
    Field2: Int64;
  end align(8);

  TAnotherAligned = record
    X: Byte;
    Y: Byte;
  end align(16);

implementation

end.
