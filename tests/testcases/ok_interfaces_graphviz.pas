unit ok_interfaces_graphviz;
interface

type
  TBaseClass = class
  end;

  TDerivedClass = class(TBaseClass)
  end;

  TDerivedClassWithInterface = class(TBaseClass, IInterface)
  end;

  IMyInterface = interface
  end;

  IMyInterface2 = interface
  end;

  TDerivedClassWithMyInterface = class(TBaseClass, IMyInterface)
  end;

  TDerivedClassWithMoreInterfaces = class(TBaseClass, IMyInterface, IMyInterface2)
  end;

implementation
end.