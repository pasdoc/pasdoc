unit ok_inherited_members;

interface

type
  TBase = class
    BaseField: Integer;
    BaseField2: String;
    A, Z, B, Y: Integer; // Test sorting
    const
      BaseConst = 42;
      BaseConst2 = 'blabla';
    type
      TBaseType = Integer;
      TBaseEnum = (meOne, meTwo);
      TBaseNestedCio = class
        procedure NestedMethod;
      end;
    var
      BaseField3: String;
    procedure BaseMethod; virtual;
    procedure BaseMethod2;
    property BaseProp: Integer read BaseField write BaseField;
    property BaseProp2: String read BaseField2 write BaseField2;
  protected
    BaseProtectedField: Integer;
    procedure BaseProtectedMethod;
  end;

  TDerived = class(TBase)
    DerivedField: Integer;
    DerivedField2: String;
    A2, Z2, B2, Y2: Integer; // Test sorting
    const
      DerivedConst = 42;
      DerivedConst2 = 'blabla';
    type
      TDerivedType = Integer;
      TDerivedEnum = (meOne, meTwo);
      TDerivedNestedCio = class
        procedure NestedMethod;
      end;
    var
      DerivedField3: String;
    procedure DerivedMethod; virtual;
    procedure DerivedMethod2;
    property DerivedProp: Integer read DerivedField write DerivedField;
    property DerivedProp2: String read DerivedField2 write DerivedField2;
  protected
    DerivedProtectedField: Integer;
    procedure DerivedProtectedMethod;
  end;

  TNewDerived = class(TDerived)
    NewDerivedField: Integer;
    NewDerivedField2: String;
    A3, Z3, B3, Y3: Integer; // Test sorting
    const
      NewDerivedConst = 42;
      NewDerivedConst2 = 'blabla';
    type
      TNewDerivedType = Integer;
      TNewDerivedEnum = (meOne, meTwo);
      TNewDerivedNestedCio = class
        procedure NestedMethod;
      end;
    var
      NewDerivedField3: String;
    procedure NewDerivedMethod; virtual;
    procedure NewDerivedMethod2;
    property NewDerivedProp: Integer read NewDerivedField write NewDerivedField;
    property NewDerivedProp2: String read NewDerivedField2 write NewDerivedField2;
  protected
    NewDerivedProtectedField: Integer;
    procedure NewDerivedProtectedMethod;
  end;

implementation
end.