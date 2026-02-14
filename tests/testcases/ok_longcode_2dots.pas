{ Testcase for https://github.com/pasdoc/pasdoc/issues/228 .
@longcode(#
procedure TMyScanner.GetSymbol;
begin
  case Self.Character of
  '+', '-', '*', '/':
    begin
      fSymbolId := tokOperator;
      fSymbol := Self.Character;
      Self.NextChar
    end;
  '0'..'9':
    begin
      fSymbolId := tokNumber;
      fSymbol := Self.GetNumber
    end;
  'a'..'z', 'A'..'Z':
    begin
      fSymbolId := tokIdentifier;
      Self.GetIdentifierOrKeyword
    end;
  else
    Self.RaiseExceptionFmt (
      'Illegal character "%s" ($%.2x)',
      [Self.Character, Ord(Self.Character)]
    );
  end
end;
#)
}
unit ok_longcode_2dots;

interface

implementation

end.