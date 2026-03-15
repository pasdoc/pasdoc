unit ok_type_aliases;

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

implementation

end.