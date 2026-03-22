{ Test parsing @code(exports) section. }
unit ok_exports;

interface

uses
  Windows;

{ Description of Foo. }
procedure Foo;

{ Description of Bar. }
procedure Bar(const S: TStr64);

exports
  Foo,
  Bar;

{ Description of Foo2. }
procedure Foo2;

implementation

end.