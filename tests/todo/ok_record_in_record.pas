{ Bug:

  Parsing of this fails with
  Warning[2]: Error EPasDoc: todo/ok_record_in_record.pas(8): Unexpected keyword end. parsing unit ok_record_in_record.pas, continuing...
}

unit ok_record_in_record;

interface

var
  Foo: record
    InsideRecord: record Bar: Integer; end;
  end;

implementation

end.