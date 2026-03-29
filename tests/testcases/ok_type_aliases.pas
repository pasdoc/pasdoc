unit ok_type_aliases;

{$ifdef FPC}{$mode objfpc}{$endif}

interface

type
  { @abstract(TOriginalType abstract description) }
  TOriginalType = record x,y: integer end;
  TWeakAliasType = TOriginalType;
  TStrongAliasType = type TOriginalType;

  { @abstract(TOriginalType2 abstract description.) More and more description. }
  TOriginalType2 = record x,y: integer end;
  TWeakAliasType2 = TOriginalType2;
  TStrongAliasType2 = type TOriginalType2;

  { @abstract(TOriginalType3 abstract description.) More and more description. }
  TOriginalType3 = record x,y: integer end;
  { @abstract(Special weak alias abstract.) More and more description. }
  TWeakAliasType3 = TOriginalType3;
  { @abstract(Special strong alias abstract.) More and more description. }
  TStrongAliasType3 = type TOriginalType3;

  TMyClass = class
    type
      TMyNestedClass = class
        type
          TAnotherNestedClass = class
            type
              { @abstract(TOriginalType abstract description, inside nested class(es).) More and more description. }
              TOriginalType = record x,y: integer end;
          end;
      end;
  end;

  TWeakAliasTypeVeryNested = TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType;
  TStrongAliasTypeVeryNested = type TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType;

{ Also linking using @@tags to arbitrary qualified type identifiers should work:

  @unorderedList(
    @item(@link(TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType))
    @item(@link(ok_type_aliases.TMyClass.TMyNestedClass.TAnotherNestedClass.TOriginalType))
    @item(@link(TMyClass.TMyNestedClass.TAnotherNestedClass))
    @item(@link(ok_type_aliases.TMyClass.TMyNestedClass.TAnotherNestedClass))
    @item(@link(TMyClass.TMyNestedClass))
    @item(@link(ok_type_aliases.TMyClass.TMyNestedClass))
    @item(@link(TMyClass))
    @item(@link(ok_type_aliases.TMyClass))
    @item(@link(ok_type_aliases))
  )
}
procedure Test;

type
  TControls = class end;

  { This class manages a @link(Controls) list. }
  TMyContainer = class
  strict private
    function GetControls: TControls;
  public
    property Controls: TControls read GetControls;
  end;

  TMyContainerAlias = TMyContainer;
  TMyContainerStrongAlias = type TMyContainer;

  { Alias to subrange type, allowed by FPC (though not by Delphi, at least as of Delphi 12). }
  UCS4Char = type 0..$10ffff;

  TEnum = (One, Two, Three);
  { Alias to subrange type of anums, again allowed only by FPC. }
  TEnumAlias = type Two..Three;

implementation

end.