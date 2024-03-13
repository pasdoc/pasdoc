unit ok_type_aliases;

interface 

type
  { @abstract(My original type description) }
  TOriginalType = record x,y: integer end;
  TWeakAliasType = TOriginalType;
  TStrongAliasType = type TOriginalType;

implementation
 
end.