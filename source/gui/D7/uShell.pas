unit uShell;
(* Wrap SHBrowseForFolder...
  Implement directory enumerator
*)

interface

uses
  Classes, Windows;

type
  eFileEnum = (
  //final (boolean) results
    feFail, //False
    feSucc, //True
    feCont, //(continue enumeration)
    feBreak //(stop enumeration of the directory entries)
  );
  TEnumFilesCallback = function(const dir, fn: string; const fd: TWIN32FindData;
    isDir: boolean): eFileEnum of object;

  eWantEntries = (
    weFiles,  //want files
    weDirs,   //want directories
    //weLabel,  //want label?
    weRecurse   //recurse into subdirectories
  );
  sWantEntries = set of eWantEntries;


type
  TFileEnumerator = class(TStringList)
  protected
    LastAdded: string;
    fd: TWIN32FindData;
  public
  //list to add found files to - nil if not desired
    AddTo: TStrings;
  //callbacks
    function AddFiles(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    function AddDirs(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    function DeleteAll(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    function DeleteTry(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    function CheckExists(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;

  //main function
    function EnumFiles(const root, pat: string; cb: TEnumFilesCallback;
      fWant: sWantEntries;  // fRecurse: boolean = false;
      dirlst: TStrings = nil): boolean;
  //for external callbacks - please dont modify!
    property Found: TWIN32FindData read fd;
  end;

//function KillAllFiles(const dir: string; fRecurse: boolean = False): boolean;
function KillAllFiles(const dir: string; fWant: sWantEntries = [weFiles]): boolean;

function QueryFolder(const Title: string; const StartFolder: string = '';
  fAllowCreate: boolean = False): string;

var
//the default file enumerator object
  DirList: TFileEnumerator;

implementation

uses
  Forms,  //Application object
  SysUtils, //PathDelim
  ShellAPI,
  ShlObj, ActiveX, DateUtils;

const
  //BIF_NEWDIALOGSTYLE=$40;
  BIF_NONEWFOLDERBUTTON=$200;

var
  lg_StartFolder: string;

function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
  lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(Wnd, BFFM_SETSELECTION, 1, Integer(@lg_StartFolder[1]));
  result := 0;
end;


function QueryFolder(const Title: string; const StartFolder: string;
  fAllowCreate: boolean): string;
var
  bi: TBrowseInfo;
  pidl: PItemIDList;
  //srclst: ItemIDList;
  path: array[0..MAX_PATH+2] of char;
  ppMalloc: IMalloc;
begin
//init bi, everything but Title can be zero.
  FillChar(bi, sizeof(bi), 0);
  bi.hwndOwner := Application.Handle;
  bi.pidlRoot := nil; // srclst;
  bi.pszDisplayName := @path[1];
  bi.lpszTitle := PChar('pick a directory');
  bi.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
  if not fAllowCreate then
    bi.ulFlags := bi.ulFlags or BIF_NONEWFOLDERBUTTON;
  if StartFolder <> '' then begin
    lg_StartFolder := StartFolder;
    bi.lpfn := BrowseForFolderCallBack;
  end;
  //bi.lParam := 0;
  //bi.iImage := 0;
  Result := '';
  pidl := SHBrowseForFolder(bi);
  if pidl = nil then
    exit;
//get the folder name
  if SHGetPathFromIDList(pidl, path) then
    Result := string(path) + '\';
//cleanup
  //GlobalFreePtr(pidl);
  if SHGetMalloc(ppMalloc) = NOERROR then begin
    ppMalloc.Free(pidl);
    //ppMalloc._Release;
  end;
end;

function KillAllFiles(const dir: string; fWant: sWantEntries = [weFiles]): boolean;
begin
  DirList.AddTo := nil;
  Result := DirList.EnumFiles(dir, '*.*', DirList.DeleteTry, fWant);
end;

{ TFileEnumerator }

function TFileEnumerator.EnumFiles(const root, pat: string; cb: TEnumFilesCallback;
      fWant: sWantEntries;  // fRecurse: boolean = false;
      dirlst: TStrings = nil): boolean;
var
  dir: string;
  state: eFileEnum;

  procedure Traverse(fDir: boolean);
  var
    fh: THANDLE;
  begin
    if fDir then
      fh := FindFirstFile(PChar(dir + '*.*'), fd)
    else
      fh := FindFirstFile(PChar(dir + pat), fd);
    if fh <> INVALID_HANDLE_VALUE then try
      repeat
      //all files
        if (fd.dwFileAttributes and 8) <> 0 then begin
          //ignore drive label, long filenames...
        end else if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
        //is directory, exclude: invalid dirs, recursion
        //cFileName array is zero based, not a string!
          if fd.cFileName[0] <> '.' then begin
            if fDir then begin
            //recurse
              dirlst.Add(dir + fd.cFileName + pathdelim);
              Application.ProcessMessages;  //visual feedback?
            end else if (weDirs in fWant) then begin
            //user
              state := cb(dir, fd.cFileName, fd, True);
            end;
          end;
        end else if not fDir and (weFiles in fWant) then begin
          //HandleEntry;
          state := cb(dir, fd.cFileName, fd, False);
        end;
      until (state <> feCont) or not FindNextFile(fh, fd);
    finally
      Windows.FindClose(fh);
    end;
  end;

begin
(* Enumerate files, starting in directory root.
  Root must end with a directory separator.
*)
//at least files or dirs must be requested
  if (fWant * [weFiles, weDirs]) = [] then
    include(fWant, weFiles);  //assume files, or fail?
  DirList.LastAdded := '';
  state := feFail;
  if not (weRecurse in fWant) then begin
    dir := root;
    Traverse(False);
  end else begin
  //init recursion
    if dirlst = nil then
      dirlst := DirList;
    dirlst.Clear;
  //start enumeration
    dirlst.Add(root);
    Application.ProcessMessages;  //visual feedback?
    repeat  //while lst.Count > 0 do begin
    //all directories
      dir := dirlst[0];
      state := feCont;
      Traverse(False);
      if state in [feFail, feSucc] then begin
        dirlst.Clear;
        break;
      end;
      //if ewRecurse in fWant then
      state := feCont;
      Traverse(True);
    //next directory
      dirlst.Delete(0);
      Application.ProcessMessages;  //visual feedback?
    until dirlst.Count <= 0;
  //close file list
    DirList.AddTo := nil;
  end;
  Result := state = feSucc; //what if feBreak?
end;

function TFileEnumerator.AddDirs(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
///add directories containing pattern files
  if AddTo = nil then begin
    Result := feFail; //need list to add to
    exit;
  end;
  if not isDir and (LastAdded <> dir) then begin
    LastAdded := dir;
    AddTo.Add(dir); // + fn + PathDelim);
    Result := feBreak;
    exit;
  end;
  Result := feCont;
end;

function TFileEnumerator.AddFiles(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
///Add all files, not dirs
  if AddTo = nil then begin
    Result := feFail; //need list to add to
    exit;
  end;
  if not isDir then
    AddTo.Add(dir + fn);
  Result := feCont;
end;

function TFileEnumerator.CheckExists(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
///succ when found
  if AddTo <> nil then
    AddTo.Add(dir + fn);
  Result := feSucc;  //break if found, Result becomes True
end;

function TFileEnumerator.DeleteAll(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
///Delete all files, not dirs. Stop on error.
//deleteTree will be trickier, must process directories bottom-up!
  if isDir then
    Result := feCont
  else if DeleteFile(PChar(dir + fn)) then begin  //break if not deleted
    if AddTo <> nil then
      AddTo.Add(dir + fn);  //add deleted file
    Result := feCont;
  end else
    Result := feBreak;  //stop processing this dir(?)
end;

function TFileEnumerator.DeleteTry(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
///Delete all files, not dirs. Ignore errors.
  if not isDir and not DeleteFile(PChar(dir + fn)) and Assigned(AddTo) then  //ignore if not deleted
    AddTo.Add(dir + fn);  //add files NOT deleted
  Result := feCont;
end;

initialization
  DirList := TFileEnumerator.Create;
finalization
  DirList.Free;
end.
