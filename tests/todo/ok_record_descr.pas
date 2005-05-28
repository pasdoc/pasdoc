{ Bug:

  Docs for this unit generate ok, but there is no documentation
  what type TMyRecord fields have.
  Update 2005-05-28: now type of field B is displayed OK, 
  but type of field A is still not correctly displayed
  (there is just a word "record", but there should be full record 
  declaration, i.e. "record A: Integer; end;").
}

unit ok_record_descr;

interface

type
  TMyRecord = record
    InsideRecord: record A: Integer; end;
    B: Integer;
  end;

implementation

end.