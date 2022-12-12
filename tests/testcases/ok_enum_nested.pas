unit ok_enum_nested;

interface

type
  TGlobalEnum = (geOne, geTwo, geThree);
  TMyObject = class
  type
    TObjectEnum = (oeOne, oeTwo, oeThree);
    TNestedObject = class
    type
      TNestedObjectEnum = (noeOne, noeTwo, noeThree);
    end;
  end;

implementation

end.
