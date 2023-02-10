unit ok_recursive_ancestor;

interface

type
  { Invalid Pascal, such relation is not allowed, FPC correctly reports:
      Error: Forward declaration of class "TMyClass" must be resolved here to use the class as ancestor
  }
  TMyClass = class;
  TMyClass = class(TMyClass)
  end;

  { Valid Pascal, FPC compiles this (just fix first TMyClass, e.g. to not declare explicit ancestor).
    Generic and non-generic TMyClass are different types. }
  TMyClass<T> = class(TMyClass)
  end;

  { Invalid Pascal, such relation is not allowed, FPC correctly reports:
      Error: Forward declaration of class "TMyInterface" must be resolved here to use the class as ancestor
  }
  TMyInterface = interface(TMyInterface)
  end;

  { Valid Pascal, FPC compiles this (just fix first TMyInterface, e.g. to not declare explicit ancestor).
    Generic and non-generic TMyClass are different types. }
  TMyInterface<T> = interface(TMyInterface)
  end;

implementation

end.
