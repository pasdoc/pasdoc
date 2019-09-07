unit ok_skip_ifdefed_out;

interface

{$ifdef read_implementation}

{ TX3DPrototypeNode --------------------------------------------------------- }

procedure TX3DPrototype.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    // At one point, the { below (that is inside a string)
    // was confusing PasDoc's TTokenizer.SkipUntilCompilerDirective,
    // and in effect "ifndef COMPILER_CASE_ANALYSIS" was not seen, causing
    // TScanner.SkipUntilElseOrEndif to be incorrect.
    xeClassic: Writer.WritelnIndent('{');
    xeXML    : Writer.WritelnIndent('<ProtoBody>');
    {$ifndef COMPILER_CASE_ANALYSIS}
    else ;
    {$endif}
  end;
end;

{$endif read_implementation}

implementation

end.
