{ Bug:

  Parsing of this unit fails with
  Warning[2]: Error EPasDoc: todo/ok_cdecl_external.pas(5): Unexpected keyword external. parsing unit ok_cdecl_external.pas, continuing...

  Bar and Xyz added as additional tests.
  
  Update: it's fixed now, pasdoc parses it correctly.
}

unit ok_cdecl_external;

interface

procedure Foo; cdecl; external 'whatever';

procedure Bar; cdecl; external 'bar_library_name' name 'bar_name_in_library';

procedure Xyz; external 'xyz_library_name' name 'xyz_name_in_library'; cdecl;

implementation

end.