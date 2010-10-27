{ @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>) }

unit WWWBrowserRunnerDM;

interface

{$IFDEF ConditionalExpressions}
  {$IF CompilerVersion >= 15}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 20}
    {$DEFINE STRING_UNICODE}
  {$IFEND}
{$ENDIF}

uses
  Classes, SysUtils, Forms, Controls, Dialogs;

type

  { TWWWBrowserRunner }

  TWWWBrowserRunner = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    BrowserCommand: string;
    procedure RunBrowser(const URL: string);
  end;

var
  WWWBrowserRunner: TWWWBrowserRunner;

const
  DefaultWWWBrowserCommand =
    {$ifdef WIN32} '' {$else} 'sh -c "$BROWSER %s"' {$endif};

implementation

uses {$ifdef WIN32} Windows, ShellAPI, {$endif} PasDocGuiSettings;

{$R *.dfm}

{ TWWWBrowserRunner }

procedure TWWWBrowserRunner.DataModuleCreate(Sender: TObject);
begin
  BrowserCommand := IniFile.ReadString('Main', 'WWWBrowserCommand',
    DefaultWWWBrowserCommand);
end;

procedure TWWWBrowserRunner.DataModuleDestroy(Sender: TObject);
begin
  IniFile.WriteString('Main', 'WWWBrowserCommand', BrowserCommand);
end;

procedure TWWWBrowserRunner.RunBrowser(const URL: string);

  {$ifdef WIN32}
  procedure ShellExecuteURL;
  var
    ExecInfo: TShellExecuteInfo;
  const
    OpenCommand = 'open';
  begin
    ExecInfo.cbSize := SizeOf(ExecInfo);
    ExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    ExecInfo.Wnd := 0; // <= Might be "ExecInfo.hWnd" in older FPC ?
    ExecInfo.lpVerb := PChar(OpenCommand);
    ExecInfo.lpFile := PChar(URL);
    ExecInfo.lpParameters := nil;
    ExecInfo.lpDirectory := nil;
    ExecInfo.nShow := SW_SHOWNORMAL;
    ShellExecuteEx(@ExecInfo);
  end;
  {$endif}

begin
  {$ifdef WIN32}
  if Trim(BrowserCommand) = '' then
  begin
    ShellExecuteURL;
    Exit;
  end;
  {$endif}
end;

end.

