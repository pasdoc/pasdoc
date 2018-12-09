unit ok_enum_explicit_assign;

interface

type
  TEnum1 = (e1One, e1Two = 12, e1Three, e1Four := 15);
  TEnum2 = (e2One := 3, e1Two := 4);

implementation

end.