unit PasDoc_Types;

interface
uses
  SysUtils;
  
type
  { }
  TMessageType = (mtPlainText, mtInformation, mtWarning, mtError);
  { }
  TPasDocMessageEvent = procedure(const MessageType: TMessageType; const
    AMessage: string; const AVerbosity: Cardinal) of object;

{ }
  EPasDoc = class(Exception)
  public
    constructor Create(const AMessage: string;
      const AArguments: array of const; const AExitCode: Integer);
  end;

implementation

{ EPasDoc }

constructor EPasDoc.Create(const AMessage: string; const AArguments: array of
  const; const AExitCode: Integer);
begin
  ExitCode := AExitCode;
  CreateFmt(AMessage, AArguments);
end;

end.
 
