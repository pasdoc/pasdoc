unit ok_deprecated_const_string;

interface

procedure Foo; deprecated 'Since D2010 a const string may follow';

implementation

procedure Foo;
begin
end;

end.