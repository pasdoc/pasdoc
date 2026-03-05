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

  TDerivedClassWithMyInterface = class(TBaseClass, IIMyInterface)
  end;

  TDerivedClassWithMoreInterfaces = class(TBaseClass, IIMyInterface)
  end;

implementation
end.