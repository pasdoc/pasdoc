{ @abstract(This is a test that macro support can be turned off
  in pasdoc.)

  With macro support turned on this unit would cause parsing error,
  because it would have brain-damaged declaration like
  @longcode(#
    procedure interface interface(a: Integer);
  #)
}

unit ok_macros_off;

interface

{$define FOO := interface interface}

procedure FOO(a: Integer);

implementation

end.