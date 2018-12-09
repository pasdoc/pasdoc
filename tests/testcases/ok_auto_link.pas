{ @abstract(Auto-link tests.)

  Self name: ok_auto_link, simple identifiers Var1,
  qualified ident: ok_auto_link.Var1.

  Ident that can't be auto-linked: no_auto_link_to_me.
  Explicit link to ident that can't be auto-linked: @link(no_auto_link_to_me). }

unit ok_auto_link;

interface

const
  { @noAutoLinkHere }
  no_auto_link_to_me = 1;

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

        Ident that can't be auto-linked: no_auto_link_to_me.
        Explicit link to ident that can't be auto-linked: @link(no_auto_link_to_me).

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

          Ident that can't be auto-linked: no_auto_link_to_me.
          Explicit link to ident that can't be auto-linked: @link(no_auto_link_to_me).

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