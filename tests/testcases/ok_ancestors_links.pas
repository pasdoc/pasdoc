{ Test that we show ancestors correctly, even when using type aliases. }
unit ok_ancestors_links;

interface

uses Classes;

type
  TMyClass = class
  end;

  TMyDescendant = class(TMyClass)
  end;

  TMyClassAlias = TMyClass;

  TMyDescendantUsingAlias = class(TMyClassAlias)
  end;

  TMyDescendantOfStdClass = class(TStringList)
  end;

  TStringListAlias = TStringList;

  TMyDescendantOfStdClassAlias = class(TStringListAlias)
  end;

implementation

end.
