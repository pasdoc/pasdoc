{ Parse implementation section.
  Separate test to check that asm blocks with Pascal keywords are handled correctly.
  Must be tested with --implementation-comments=*any* }
unit ok_parse_impl_asm;

interface

implementation

// Pascal keywords inside ASM blocks

procedure t_case; assembler;
asm
  @@case:
end;

procedure t_const; assembler;
asm
  @@const:
end;

procedure t_type; assembler;
asm
  @@type:
end;

procedure t_procedure; assembler;
asm
  @@procedure:
end;

procedure t_begin; assembler;
asm
  @@begin:
end;

// Keyword that should finish the block

procedure t_end; assembler;
asm
  jz    @@end
@@end:
  mov   edx,eax
end;

end.