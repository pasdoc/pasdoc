unit ok_external_class_hierarchy;

interface

type
  TMyClass0 = class end;

  TMyClass1 = class(TObject) end;

  { TMyClass2 is not defined in the source code.
    But we define in ok_external_class_hierarchy.txt that
    TMyClass2 descends from TMyClass1. }

  TMyClass3 = class(TMyClass2) end;

implementation
end.
