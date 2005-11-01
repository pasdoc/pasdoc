{ @abstract(Auto-link tests.)

  Self name: ok_auto_link, simple identifiers Var1,
  qualified ident: ok_auto_link.Var1. }

unit ok_auto_link;

interface

type
  TMyClass = class
    Field: Integer;
  end;

var
  { @definitionList(
      @itemLabel Test of auto-linking:
      @item(
        Self name is Var1, simple ident is TMyClass, qualified ident is
        TMyClass.Field.

        Note that auto-linking works also inside @@code:

        @code(
        Self name is Var1, simple ident is TMyClass, qualified ident is
        TMyClass.Field. )
      )

      @itemLabel Test of @@noAutoLink:
      @item(
        Things below should @italic(not) be converted to links:

        @noAutoLink(
          Self name is Var1, simple ident is TMyClass, qualified ident is
          TMyClass.Field.

          @noAutoLink(Simple ident once again TMyClass.)

          Inside @@code:

          @code(
          Self name is Var1, simple ident is TMyClass, qualified ident is
          TMyClass.Field.)
        )
      )
    )
  }
  Var1: Integer;

implementation

end.