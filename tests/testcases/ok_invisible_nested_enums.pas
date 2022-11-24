unit ok_invisible_nested_enums;

interface

type
  TOuterObject = class(TObject)
  private type
    TPrivateInnerEnum = (pieSecret, pieMoreSecret);
    TPrivateInnerObject = class(TObject)
    end;
  public type
    TPublicInnerEnum = (puePublic, pueSuperPublic);
    TPublicInnerObject = class(TObject)
    end;
  end;

implementation

end.