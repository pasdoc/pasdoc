unit frDir;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls;

type
  TDirBox = class(TFrame)
    edFile: TEdit;
    buSelect: TButton;
    Label1: TLabel;
    dlgOpen: TOpenDialog;
    procedure buSelDirClick(Sender: TObject);
    procedure buSelFileClick(Sender: TObject);
  protected
  //read directory
    //function  GetDir: string; virtual;
    //procedure SetDir(s: string); virtual;
  //read file
    //function  GetFile: string; virtual;
    //procedure SetFile(s: string); virtual;
  //full name
    function  GetPath: string;
    procedure SetPath(s: string);
  //relative name
    function  GetRelative(const root: string): string;
    procedure SetRelative(const root: string; s: string);
  public
    function  SelFile: boolean;
    function  SelDir: boolean;
    property DirName: string read GetPath write SetPath;
      //equivalent to FullName for directory
    //property FileName: string read GetFile; // write SetFile;
    property FullName: string read GetPath write SetPath;
    property Relative[const root: string]: string read GetRelative write SetRelative;
    property Text: string read GetPath write SetPath;
  end;

implementation

{$R *.dfm}

uses
  fDocMain,
  uShell;

function TDirBox.SelDir: boolean;
var
  s: string;
begin
  s := uShell.QueryFolder(dlgOpen.Title, edFile.Text,
    not (ofPathMustExist in dlgOpen.Options));
  Result := s <> '';
  if Result then
    FullName := s;
end;

procedure TDirBox.buSelDirClick(Sender: TObject);
begin
  SelDir;
end;

function TDirBox.SelFile: boolean;
begin
  Result := dlgOpen.Execute;
  if Result then
    FullName := dlgOpen.FileName;
end;

procedure TDirBox.buSelFileClick(Sender: TObject);
begin
  SelFile;
end;

function TDirBox.GetPath: string;
begin
  Result := edFile.Text;
end;

procedure TDirBox.SetPath(s: string);
begin
  edFile.Text := s;
  DocMain.HasChanged := True;
end;

function TDirBox.GetRelative(const root: string): string;
begin
  Result := edFile.Text;
  if (Result <> '') and (Copy(Result, 1, Length(root)) = root) then
    Result := Copy(Result, Length(root)+1, Length(Result));
end;

procedure TDirBox.SetRelative(const root: string; s: string);
begin
  if ExtractFileDrive(s) = '' then
    edFile.Text := root + s //make absolute
  else
    edFile.Text := s; //is absolute
  DocMain.HasChanged := True;
end;

end.
