{ Test parsing asm block with " inside.
  See https://github.com/pasdoc/pasdoc/issues/69 }
unit ok_asm;
interface

procedure Foo;

implementation

procedure Foo;
begin
  asm
    CMP     AL,"'"
    MOV     AL,byte ptr [ESI]
  end;
end;

end.