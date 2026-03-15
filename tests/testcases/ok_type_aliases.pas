unit ok_type_aliases;

interface

type
  { @abstract(My original type abstract description) }
  TOriginalType = record x,y: integer end;
  TWeakAliasType = TOriginalType;
  TStrongAliasType = type TOriginalType;

  { @abstract(My original type abstract description.) More and more description. }
  TOriginalType2 = record x,y: integer end;
  TWeakAliasType2 = TOriginalType2;
  TStrongAliasType2 = type TOriginalType2;

  { @abstract(My original type abstract description.) More and more description. }
  TOriginalType3 = record x,y: integer end;
  { @abstract(Special weak alias abstract.) More and more description. }
  TWeakAliasType3 = TOriginalType3;
  { @abstract(Special strong alias abstract.) More and more description. }
  TStrongAliasType3 = type TOriginalType3;

implementation

end.