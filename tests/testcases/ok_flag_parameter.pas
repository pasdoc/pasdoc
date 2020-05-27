unit ok_flag_parameter;
interface

{ My proc.
  @param Xxx [in] Some input param.
  @param Yyy [out] Some output param.
  @param(Xxx1 [in] Some input param.)
  @param(Yyy1 [out] Some output param.)
  @param(Xxx3 [in, nil allowed] Some output param.)
  @param(Xxx4 [out, always non-nil on exit] Some output param.)
  @param(Xxx4 thisShouldNotBeFlag[out] Some output param.)
  @param(Xxx4 this should not be flag either[out] Some output param.)

  This link should work @link(TCastleTheme.Items Theme.Items[tiLoading]).

  This link should work too @link(TCastleTheme Blah blah (blah blah Items[Index])).
}
procedure Foo(const Xxx: Integer; out Yyy: Integer; const Xxx2: Integer; out Yyy2: Integer;
  const Xxx3: TObject; const Xxx4: TObject);

procedure Bar;

type
  TCastleTheme = class
  public
    property Items[const I: Integer]: String read GetItems;
  end;

implementation

procedure Foo(const Xxx: Integer; out Yyy: Integer; const Xxx2: Integer; out Yyy2: Integer);
begin
end;

end.
