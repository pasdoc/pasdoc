{ Bug:

  Parsing of this unit fails with
  Warning[2]: Error EPasDoc: todo/ok_cdecl_external.pas(5): Unexpected keyword external. parsing unit ok_cdecl_external.pas, continuing...
}

unit ok_cdecl_external;

interface

procedure Foo; cdecl; external 'whatever';

implementation

end.