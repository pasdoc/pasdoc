unit ok_flag_parameter;
interface

{ My proc.
  @param Xxx [in] Some input param.
  @param Yyy [out] Some output param.
  @param(Xxx1 [in] Some input param.)
  @param(Yyy1 [out] Some output param.)
  @param(Xxx3 [in, nil allowed] Some output param.)
  @param(Xxx4 [out, always non-nil on exit] Some output param.)
}
procedure Foo(const Xxx: Integer; out Yyy: Integer; const Xxx2: Integer; out Yyy2: Integer;
  const Xxx3: TObject; const Xxx4: TObject);

implementation

procedure Foo(const Xxx: Integer; out Yyy: Integer; const Xxx2: Integer; out Yyy2: Integer);
begin
end;

end.
