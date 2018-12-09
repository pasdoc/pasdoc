unit ok_enum_field_var;

interface

type
  TServerConnController = class
  public
    AutoConnectAction: (acNone,
                        acConnectOnDisconnect,
                        acLoginOnDisconnect,
                        acLoginOnConnect);
  end;

var
  ApplicationStatus: (apInitializing, apReady, apClosing, apDestroying);

implementation

end.