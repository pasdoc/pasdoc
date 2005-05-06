{ @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @abstract(basic types used in PasDoc) }
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

  TCharSet = set of Char;

{ }
  EPasDoc = class(Exception)
  public
    constructor Create(const AMessage: string;
      const AArguments: array of const; const AExitCode: Word);
  end;

implementation

{ EPasDoc }

constructor EPasDoc.Create(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
begin
  ExitCode := AExitCode;
  CreateFmt(AMessage, AArguments);
end;

end.
 
