{ Testcase for bug
  "[ 1403317 ] Error parsing automatically generated unit" }

unit ok_dispid_method;

interface

type
  INetFwOpenPortsDisp = dispinterface
    ['{C0E9D7FA-E07E-430A-B19A-090CE82D92E2}']
    property Count: Integer readonly dispid 1;
    procedure Add(const Port: INetFwOpenPort); dispid 2;
    procedure Remove(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_); dispid 3;
    function Item(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_): INetFwOpenPort; dispid 4;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

implementation

end.