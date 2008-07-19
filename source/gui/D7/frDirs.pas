unit frDirs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls;

type
  TDirList = class(TFrame)
    GroupBox1: TGroupBox;
    buAdd: TButton;
    buAddAll: TButton;
    buRemove: TButton;
    lbFiles: TListBox;
    dlgAdd: TOpenDialog;
    procedure buRemoveClick(Sender: TObject);
    procedure buAddClick(Sender: TObject);
  private
    procedure HasChanged;
  public
    //dirs: TStrings; //assign to connect to Includes
    dirs: TDirList; //assign to connect to Includes
    //HasHasChanged: procedure of object;  //signal changes
    procedure AddFiles(lst: TStrings);
    procedure AddFile(const fn: string);
    procedure AddDir(const fn: string);
    procedure Clear;
    function  Items: TStrings;
  end;

implementation

{$R *.dfm}

uses
  fDocMain,
  uShell;

procedure TDirList.buRemoveClick(Sender: TObject);
begin
  lbFiles.DeleteSelected;
  HasChanged;
end;

procedure TDirList.buAddClick(Sender: TObject);
var
  s: string;
begin
  if assigned(dirs) then begin
  //file list behaviour
    if dlgAdd.Execute then begin
      AddFiles(dlgAdd.Files);
    end;
  end else begin
  //include list behaviour
    s := QueryFolder(dlgAdd.Title, ''); //root dir???
    if s <> '' then begin
      AddDir(s);
    end;
  end;
end;

procedure TDirList.AddFile(const fn: string);
var
  i: integer;
begin
//don't add dupes
  i := lbFiles.Items.IndexOf(fn);
  if i < 0 then begin
    lbFiles.Items.Add(fn);
    if Assigned(dirs) then
      dirs.AddDir(ExtractFilePath(fn));
    HasChanged;
  end;
end;

procedure TDirList.AddFiles(lst: TStrings);
var
  i: integer;
begin
  for i := 0 to lst.Count - 1 do
    AddFile(lst[i]);
end;

procedure TDirList.Clear;
begin
  lbFiles.Clear;
  HasChanged;
end;

function TDirList.Items: TStrings;
begin
  Result := lbFiles.Items;
end;

procedure TDirList.AddDir(const fn: string);
begin
  if assigned(dirs) then
  //file list behaviour
    //self.dirs.Add(ExtractFilePath(fn))
    dirs.AddDir(fn)
  else begin
  //dir list behaviour
    AddFile(fn);
  end;
end;

procedure TDirList.HasChanged;
begin
  DocMain.HasChanged := True;
end;

end.
