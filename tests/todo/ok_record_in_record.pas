{ Bug:

  Parsing of this fails with
  Warning[2]: Error EPasDoc: todo/ok_record_in_record.pas(8): Unexpected keyword end. parsing unit ok_record_in_record.pas, continuing...
  
  Update 2005-05-28: now parsing is OK, 
  but type of field InsideRecord is still not correctly displayed
  (there is just a word "record"). This is duplicated problem of test
  ok_record_descr.pas.
}

unit ok_record_in_record;

interface

var
  Foo: record
    InsideRecord: record Bar: Integer; end;
  end;

implementation

end.