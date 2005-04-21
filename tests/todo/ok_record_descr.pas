{ Bug:

  Docs for this unit generate ok, but there is no documentation
  what type TMyRecord fields have.
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