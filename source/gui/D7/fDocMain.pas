unit fDocMain;

{.$DEFINE languages}  //implement language selection?

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, CheckLst,
  uShell, 
  PasDoc_Languages, PasDoc_GenLatex, PasDoc_Serialize, PasDoc_GenHtmlHelp,
  PasDoc_Base, PasDoc_Items, PasDoc_Gen, PasDoc_GenHtml, PasDoc_SortSettings,
  PasDoc_Types,
  frDirs, frDir, ExtCtrls, PasDoc_GenSimpleXML, PasDoc_GenFullXML;

type
  TDocMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Preferences1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    tabPages: TPageControl;
    tabOpts: TTabSheet;
    Label2: TLabel;
    tabLocs: TTabSheet;
    tabGraph: TTabSheet;
    tabHTML: TTabSheet;
    tabSpelling: TTabSheet;
    tabMarkers: TTabSheet;
    tabFiles: TTabSheet;
    tabDirectories: TTabSheet;
    tabDefines: TTabSheet;
    tabGenerate: TTabSheet;
    GroupBox1: TGroupBox;
    lbLog: TListBox;
    edTitle: TEdit;
    Label1: TLabel;
    lbOutType: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    swSort: TCheckListBox;
    swAutoAbstract: TCheckBox;
    Label5: TLabel;
    lbOutLang: TComboBox;
    swVisible: TCheckListBox;
    swBackRef: TCheckBox;
    OpenDialog1: TOpenDialog;
    PasDoc1: TPasDoc;
    HTMLDocGenerator: THTMLDocGenerator;
    TexDocGenerator: TTexDocGenerator;
    HTMLHelpDocGenerator: THTMLHelpDocGenerator;
    GroupBox2: TGroupBox;
    edHeader: TMemo;
    GroupBox3: TGroupBox;
    edFooter: TMemo;
    swTipue: TCheckBox;
    GroupBox4: TGroupBox;
    buClearOutput: TButton;
    buGenerate: TButton;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    swClassDiagram: TCheckBox;
    swUsesDiagram: TCheckBox;
    GroupBox5: TGroupBox;
    Label9: TLabel;
    lbHyphenate: TMemo;
    lbGraphPackage: TComboBox;
    Label11: TLabel;
    Label8: TLabel;
    lbLineBreaks: TComboBox;
    mnuDelphi: TMenuItem;
    mnuFPC: TMenuItem;
    GroupBox6: TGroupBox;
    cbVersion: TComboBox;
    Label13: TLabel;
    swConsole: TCheckBox;
    Label14: TLabel;
    cbPlatform: TComboBox;
    swDebug: TCheckBox;
    buCreate: TButton;
    edSysDefs: TMemo;
    GroupBox7: TGroupBox;
    memoDefines: TMemo;
    lbFiles: TDirList;
    lbInclude: TDirList;
    edIntro: TDirBox;
    edConclusion: TDirBox;
    edCSS: TDirBox;
    edOutput: TDirBox;
    Label7: TLabel;
    edProjectName: TEdit;
    Label10: TLabel;
    lbMarkers: TMemo;
    swMarkers: TComboBox;
    edProjectFile: TDirBox;
    buSave: TButton;
    edRoot: TDirBox;
    GroupBox8: TGroupBox;
    lbIgnored: TMemo;
    Label6: TLabel;
    swLevel: TUpDown;
    edMsgLvl: TEdit;
    swSpellCheck: TCheckBox;
    GroupBox9: TGroupBox;
    edMisspelledWords: TMemo;
    Label12: TLabel;
    swImplicitVisibility: TComboBox;
    buAnalyze: TButton;
    mnuLog: TPopupMenu;
    SaveAs1: TMenuItem;
    buCmdFile: TButton;
    mnCleardiagnostics: TMenuItem;
    tabDoc: TTabSheet;
    tvUnits: TTreeView;
    Splitter1: TSplitter;
    Panel1: TPanel;
    edRem: TMemo;
    GroupBox10: TGroupBox;
    cbRem: TComboBox;
    edDecl: TEdit;
    swShowUses: TCheckBox;
    SimpleXMLDocGenerator: TSimpleXMLDocGenerator;
    XMLDocGenerator: TXMLDocGenerator;
  {$IFDEF fpc}
    //procedure btnBrowseIncludeDirectoryClick(Sender: TObject);
  {$ELSE}
  {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure lbOutTypeChange(Sender: TObject);
    procedure lbOutLangChange(Sender: TObject);
    procedure AnyChange(Sender: TObject);
    procedure buGenerateClick(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure swLevelClick(Sender: TObject; Button: TUDBtnType);
    procedure swSpellCheckClick(Sender: TObject);
    procedure mnuDelphiClick(Sender: TObject);
    procedure mnuFPCClick(Sender: TObject);
    procedure buClearOutputClick(Sender: TObject);
    procedure buCreateClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edOutputbuSelectClick(Sender: TObject);
    procedure edRootbuSelectClick(Sender: TObject);
    procedure lbFilesbuAddAllClick(Sender: TObject);
    procedure lbIncludebuAddAllClick(Sender: TObject);
    procedure buCmdFileClick(Sender: TObject);
    procedure SaveLog(Sender: TObject);
    procedure lbLogKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  {$IFnDEF old}
    procedure PasDocWarning(const MessageType: TPasDocMessageType;
      const AMessage: String; const AVerbosity: Cardinal);
  {$ELSE}
    procedure PasDoc1Message(const MessageType: TMessageType;
      const AMessage: String; const AVerbosity: Cardinal);
  {$ENDIF}
    procedure mnCleardiagnosticsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure tvUnitsClick(Sender: TObject);
    procedure cbRemClick(Sender: TObject);
  private
    FHasChanged: boolean;
    MisspelledWords: TStringList;
    SelItem: TDescriptionItem;  // TBaseItem;
    SelToken: TToken;
    function  _AddDirs(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    function  _AddFiles(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
    procedure InitDirectives;
    function  CheckIfSpellCheckingAvailable: boolean;
    procedure NewProject;
    procedure SetHasChanged(const AValue: boolean);
    procedure SetDefaults;
  {$IFDEF old}
    procedure SetOutputDirectory(const FileName: string);
  {$ELSE}
  {$ENDIF}
    procedure UpdateCaption;
  //generator properties
  //member visibility: .ShowVisibilities: TVisibilities;
    function  VisibilityGet: TVisibilities;
    procedure VisibilityPut(Options: TVisibilities);
    property Visibility: TVisibilities read VisibilityGet write VisibilityPut;
  //item sort
    function  SortGet: TSortSettings;
    procedure SortPut(Options: TSortSettings);
    function  SSI(so: TSortSetting): integer;
    property SortedItems: TSortSettings read SortGet write SortPut;
  //output language: .Generator.Language: TLanguageID;
    function  LanguageGet: TLanguageID;
    procedure LanguagePut(id: TLanguageID);
    property Language: TLanguageID read LanguageGet write LanguagePut;
    procedure SaveSettings;
    procedure LoadSettings(const SaveName: TFileName);
  {$IFDEF fpc}
    function LanguageIdToString(const LanguageID: TLanguageID): string;
  {$ELSE}
  {$ENDIF}
    // @name fills @link(tvUnits) with a hierarchical representation of the
    // TPasItems in PasDoc1.
    procedure FillTreeView;

  //generator: .Generator:
  //HTML header: .Generator.Header: string;
  //HTML footer: .Generator.Footer: string;

  public
    function QuerySave: boolean;
    //DefaultDirectives: TStringList;
    // @name is @true when the user has changed the project settings.
    // Otherwise it is @false.
    // A property of this name already exists in Delphi!
    property HasChanged: boolean read FHasChanged write SetHasChanged;
    //property Changed: boolean read FHasChanged write SetHasChanged;
  end;

var
  DocMain: TDocMain;

implementation

uses
{$IFDEF fpc}
  frmAboutUnit,
  WWWBrowserRunnerDM,
  PreferencesFrm,
  HelpProcessor,
  PasDocGuiSettings, IniFiles;
{$ELSE}
  //fAbout,
  //WWWBrowserRunnerDM,
  //PreferencesFrm,
  ShellAPI,
  IniFiles;
{$ENDIF}

{$R *.dfm}

//------------- language helpers -----------------

{$IFDEF old}
function  LanguageFromIndex(i: integer): string;
begin
  Result := PasDoc_Languages.language_array[TLanguageID(i)].Name;
end;

function SyntaxFromIndex(i: integer): string;
var
  l: TLanguageID absolute i;
begin
  Result := Language_array[l].Syntax;
end;

function IDfromLanguage(const s: string): TLanguageID;
var
  i: TLanguageID;
begin
  for i := low(i) to high(i) do begin
    if (LANGUAGE_ARRAY[i].Name = s)
    or (LANGUAGE_ARRAY[i].Syntax = s) then begin
      Result := i;
      exit;
    end;
  end;
  Result := lgEnglish;
end;
{$ELSE}
{$ENDIF}


{ TDocMain }

procedure TDocMain.FormCreate(Sender: TObject);
var
  LanguageIndex: TLanguageID;
begin
//init lists
//spelling
  MisspelledWords:= TStringList.Create;
  MisspelledWords.Sorted := True;
  MisspelledWords.Duplicates := dupIgnore;

//languages
  lbOutLang.Clear;
  lbOutLang.Items.Capacity :=
    Ord(High(TLanguageID)) - Ord(Low(TLanguageID)) + 1;
  for LanguageIndex := Low(TLanguageID) to High(TLanguageID) do begin
    lbOutLang.Items.Add(LanguageDescriptor(LanguageIndex)^.Name);
  end;
  lbOutLang.ItemIndex := ord(DEFAULT_LANGUAGE);

{$IFDEF fpc}
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
{$ELSE}
{$ENDIF}

//init directives
  //DefaultDirectives := TStringList.Create;
  InitDirectives;

  SetDefaults;

// It's too easy to change it at design-time, so we set it at runtime.
{$IFDEF fpc}
  NotebookMain.PageIndex := 0;
  Application.ProcessMessages;
{$ELSE}
  self.tabPages.ActivePageIndex := 0;
{$ENDIF}

{$IFDEF lWIN32}
  // Deal with bug in display of TSpinEdit in Win32.
  seVerbosity.Constraints.MinWidth := 60;
  seVerbosity.Width := seVerbosity.Constraints.MinWidth;
{$ENDIF}

{$IFDEF fpc}
  { Workaround for Lazarus bug 0000713,
    [http://www.lazarus.freepascal.org/mantis/view.php?id=713]:
    we set menu shortcuts at runtime.
    (the bug is only for Win32, but we must do this workaround for every
    target). }
  MenuOpen.ShortCut := ShortCut(VK_O, [ssCtrl]);
  MenuSave.ShortCut := ShortCut(VK_S, [ssCtrl]);

// A Tag of 1 means the page should be visible.
  for Index := NotebookMain.PageCount -1 downto 0 do begin
    NotebookMain.CustomPage(Index).Tag := 1;
  end;
{$ELSE}
{$ENDIF}

  lbOutTypeChange(nil); //call handler

//connect files to include directories
  lbFiles.dirs := lbInclude;  //.lbFiles.Items;

  HasChanged := False;

  if ParamCount = 1 then begin
  //load project?
    self.LoadSettings(ParamStr(1));
  end;
end;


{$IFnDEF old}
procedure TDocMain.PasDocWarning(const MessageType: TPasDocMessageType;
  const AMessage: String; const AVerbosity: Cardinal);
const
  MisText = 'Word misspelled "';
var
  MisspelledWord: string;
begin
  lbLog.Items.Add(AMessage);
  lbLog.Refresh;
  if Pos(MisText, AMessage) =1 then begin
  //log misspelled word
    MisspelledWord := Copy(AMessage, Length(MisText)+1, MAXINT);
    SetLength(MisspelledWord, Length(MisspelledWord) -1); //strip trailing quote
    MisspelledWords.Add(MisspelledWord)
  end;
end;
{$ELSE}
procedure TDocMain.PasDoc1Message(const MessageType: TMessageType;
  const AMessage: String; const AVerbosity: Cardinal);
const
  MisText = 'Word misspelled "';
var
  MisspelledWord: string;
begin
  lbLog.Items.Add(AMessage);
  lbLog.Refresh;
  if Pos(MisText, AMessage) =1 then begin
  //log misspelled word
    MisspelledWord := Copy(AMessage, Length(MisText)+1, MAXINT);
    SetLength(MisspelledWord, Length(MisspelledWord) -1); //strip trailing quote
    MisspelledWords.Add(MisspelledWord)
  end;
end;
{$ENDIF}

// ------------------- changes ----------------------

procedure TDocMain.UpdateCaption;
var
  NewCaption: string;
begin
//Caption value follows GNOME HIG 2.0 standard
  NewCaption := edProjectFile.Text;
  if NewCaption <> '' then
    NewCaption := ExtractFileName(NewCaption)
  else if HasChanged then
    NewCaption := 'Unsaved PasDoc settings'
  else
    NewCaption := '';
  if NewCaption <> '' then
    NewCaption := NewCaption + ' - ';
  NewCaption := NewCaption + 'PasDoc GUI';
  if HasChanged then
    NewCaption := '*' + NewCaption;
  Caption := NewCaption;
end;

procedure TDocMain.SetHasChanged(const AValue: boolean);
begin
  if FHasChanged = AValue then
    Exit;
  FHasChanged := AValue;
  UpdateCaption;
end;

procedure TDocMain.AnyChange(Sender: TObject);
var
  s: string;
  obj: TPersistent;
begin
//debug code
  s := Sender.ClassName;
  obj := Sender as TPersistent;
  s := obj.GetNamePath;
  HasChanged := True;
end;

// ------------ Defines -------------------

procedure TDocMain.InitDirectives;
(** create standard defines
*)
var
  i: integer;
const
  VerStr: array[1..11] of string = (
    'FPC',
    'FPC',  //mode Delphi?
    //Kylix?
    'VER100', //D2
    'VER110',
    'VER120',
    'VER130',
    'VER140',
    'VER150', //D7
    'VER160', //BDS 2005?
    'VER170',
    'VER170'
  );

  procedure AddDir(const s: string);
  begin
    //DefaultDirectives.Append(s);
    edSysDefs.Lines.Add(s);
  end;

begin
  edSysDefs.Clear;
//compiler?
  i := cbVersion.ItemIndex;
  if i <= 0 then
    exit; //no defaults
//specific compiler
  AddDir(VerStr[i]);
  if i > 2 then begin
  //Delphi (Kylix???)
    //cbPlatform.ItemIndex := cbPlatform.Items.Count - 1; //last entry = Win32
    AddDir('CPU386');
    if i > 5 then //?
      AddDir('CONDITIONALEXPRESSIONS');
  end else begin
  //FPC
    AddDir('CONDITIONALEXPRESSIONS');
  end;

//platform
  i := cbPlatform.ItemIndex;
  case i of //add cases, alpha sort? keeping Win32 last
  1:  AddDir('LINUX');
  2:  AddDir('UNIX');
  3:  //Win32 (last one)
    begin
      AddDir('WIN32');
      AddDir('MSWINDOWS');
    end;
  end;
//console/GUI
  if swConsole.Checked then
    AddDir('CONSOLE');
  if swDebug.Checked then
    AddDir('DEBUG');
//changed
  HasChanged := True;
end;

procedure TDocMain.buCreateClick(Sender: TObject);
begin
  InitDirectives;
end;

// -------------- new project -----------------

procedure TDocMain.NewProject;  //(fFPC: boolean);
begin
//close project
  if not QuerySave then
    exit;
//init new project
  SetDefaults;
  edProjectFile.Text := '';
  tabPages.ActivePage := tabOpts;
  HasChanged := False;
end;

procedure TDocMain.mnuDelphiClick(Sender: TObject);
begin
  NewProject;
  cbVersion.ItemIndex := 8; //~D7
  cbPlatform.ItemIndex := cbPlatform.Items.Count - 1; //last entry = Win32
  InitDirectives;
end;

procedure TDocMain.mnuFPCClick(Sender: TObject);
begin
  NewProject;
  cbVersion.ItemIndex := 1;
  cbPlatform.ItemIndex := 2; //assume Linux
  InitDirectives;
end;

procedure TDocMain.SetDefaults;
var
  SortIndex: integer; // TSortSetting;
//const DefVisible: TVisibilities = [viPublished, viPublic];
const
  soff = ord(low(SortIndex));
begin
  Visibility := DefaultVisibilities;

  lbOutLang.ItemIndex := Ord(lgEnglish);
  lbOutType.ItemIndex := 0;

  edProjectName.Text := '';
  edOutput.Text := '';
  edMsgLvl.Text := '2'; //seVerbosity.Value := 2;

  lbFiles.Clear;
  lbInclude.Clear;
  lbLog.Clear;

  edCSS.Text := '';
  edIntro.Text := '';
  edConclusion.Text := '';
  //swAutoAbstract.Checked := false;
  //swTipue.Checked := false;
  swBackRef.Checked := True;  //DoDi!

//Sorting
  //for SortIndex := Low(TSortSetting) to High(TSortSetting) do
  for SortIndex := 0 to swSort.Count - 1 do
    swSort.Checked[SortIndex] := false;

  HasChanged := False;
end;

// ------------------- load/save project ---------------------

const
  secMain = 'Main';
    mainAutoAbstract = 'AutoAbstract';
    mainBackRef = 'BackRef';
    mainCheckSpelling = 'CheckSpelling';
    mainClassMembers_ = 'ClassMembers_';
    mainConclusionFileName = 'ConclusionFileName';
    mainCssFileName = 'CssFileName';
    mainGenerateFormat = 'GenerateFormat';
    mainIntroductionFileName = 'IntroductionFileName';
    mainLanguage = 'Language';
    mainLanguageName = 'LanguageName';
    mainLatexGraphicsPackage = 'LatexGraphicsPackage';
    mainLineBreakQuality = 'LineBreakQuality';
    mainOutputDir = 'OutputDir';
    mainRootDir = 'RootDir';
    mainProjectName = 'ProjectName';
    mainShowUses = 'ShowUses';
    mainSorting_ = 'Sorting_';
    mainSpecialMarkerTreatment = 'SpecialMarkerTreatment';
    mainTitle = 'Title';
    mainUseTipueSearch = 'UseTipueSearch';
    mainVerbosity = 'Verbosity';
    mainVisibility = 'Visibility';
    mainVizGraphClasses = 'VizGraphClasses';
    mainVizGraphUses = 'VizGraphUses';
  secDefines = 'Defines';
  secFiles = 'Files';
  secFooter = 'Footer';
  secHeader = 'Header';
  secHyphenatedWords = 'HyphenatedWords';
  secIgnoreWords = 'IgnoreWords';
  secIncludeDirectories = 'IncludeDirectories';
  secSpecialMarkers = 'SpecialMarkers';
    secCount = 'Count';
    secItem_ = 'Item_';

procedure TDocMain.SaveSettings;
var
  Ini: TIniFile;
  SaveName: TFileName;

  procedure WriteStrings(const Section: string; S: TStrings);
  var i: Integer;
  begin
    { It's not really necessary for correctness but it's nice to protect
      user privacy by removing trash data from file (in case previous
      value of S had larger Count). }
    Ini.EraseSection(Section);

    Ini.WriteInteger(Section, 'Count', S.Count);
    for i := 0 to S.Count - 1 do
      Ini.WriteString(Section, 'Item_' + IntToStr(i), S[i]);
  end;

var
  i: Integer;
begin
  SaveName := edProjectFile.Text;
  if SaveName = '' then
    exit; //error: no filename
  Ini := TIniFile.Create(SaveName);
  try
  {$IFDEF old}
    Ini.WriteInteger(secMain, mainLanguage, lbOutLang.ItemIndex);
      //name!?
  {$ELSE}
    i := lbOutLang.ItemIndex; //should read ID, LanguageFromID
    //if i < 0 then i := 0; //default
    Ini.WriteString(secMain, mainLanguageName, LanguageFromIndex(i));
  {$ENDIF}
    Ini.WriteString(secMain, mainRootDir, edRoot.Text);
    Ini.WriteString(secMain, mainOutputDir, edOutput.Text);
    Ini.WriteInteger(secMain, mainGenerateFormat, lbOutType.ItemIndex);
    Ini.WriteString(secMain, mainProjectName, edProjectName.Text);
    Ini.WriteInteger(secMain, mainVerbosity, swLevel.Position);
    Ini.WriteInteger(secMain, mainVisibility, swImplicitVisibility.ItemIndex);

    for i := Ord(Low(TVisibility)) to Ord(High(TVisibility)) do
      Ini.WriteBool(secMain, mainClassMembers_ + IntToStr(i),
        swVisible.Checked[i]);
      //only checked ones?

    for i := SSI(Low(TSortSetting)) to SSI(High(TSortSetting)) do
      Ini.WriteBool(secMain, mainSorting_ + IntToStr(i),
        swSort.Checked[i]);
      //only checked ones?

    WriteStrings(secDefines, memoDefines.Lines);
    WriteStrings(secHeader, edHeader.Lines);
    WriteStrings(secFooter, edFooter.Lines);
    WriteStrings(secIncludeDirectories, lbInclude.Items);
    WriteStrings(secFiles, lbFiles.Items);

    Ini.WriteString(secMain, mainCssFileName, edCSS.Text);
    Ini.WriteString(secMain, mainIntroductionFileName, edIntro.Text);
    Ini.WriteString(secMain, mainConclusionFileName, edConclusion.Text);
    Ini.WriteBool(secMain, mainAutoAbstract, swAutoAbstract.Checked);
    Ini.WriteBool(secMain, mainShowUses, swShowUses.Checked);

    Ini.WriteBool(secMain, mainUseTipueSearch, swTipue.Checked);
    Ini.WriteInteger(secMain, mainLatexGraphicsPackage, lbGraphPackage.ItemIndex);
    Ini.WriteInteger(secMain, mainLineBreakQuality, lbLineBreaks.ItemIndex);
    WriteStrings(secHyphenatedWords, lbHyphenate.Lines);
    Ini.WriteInteger(secMain, mainSpecialMarkerTreatment, swMarkers.ItemIndex);
    WriteStrings(secSpecialMarkers, lbMarkers.Lines);
    Ini.WriteBool(secMain, mainBackRef, swBackRef.Checked);
    Ini.WriteString(secMain, mainTitle, edTitle.Text);

    Ini.WriteBool(secMain, mainVizGraphClasses, swClassDiagram.Checked);
    Ini.WriteBool(secMain, mainVizGraphUses, swUsesDiagram.Checked);

    Ini.WriteBool(secMain, mainCheckSpelling, swSpellCheck.Checked);
    WriteStrings(secIgnoreWords, lbIgnored.Lines);

    Ini.UpdateFile;
  finally
    Ini.Free
  end;

  HasChanged := False;
  UpdateCaption;
end;

procedure TDocMain.LoadSettings(const SaveName: TFileName);
var
  Ini: TIniFile;

  procedure ReadStrings(const Section: string; S: TStrings);
  var i: Integer;
  begin
    S.Clear;
    i := Ini.ReadInteger(Section, secCount, 0);
    s.Capacity := i;
    for i := 0 to i - 1 do
      S.Add(Ini.ReadString(Section, secItem_ + IntToStr(i), ''));
  end;

var
  i: Integer;
  s: string;
  id: TLanguageID;
begin
  if (SaveName = '') then
    exit; //error: must specify filename

  Ini := TIniFile.Create(SaveName);
  try
    { Default values for ReadXxx() methods here are not so important,
      don't even try to set them right.
      *Good* default values are set in SetDefaults method of this class.
      Here we can assume that values are always present in ini file.

      Well, OK, in case user will modify settings file by hand we should
      set here some sensible default values... also in case we will add
      in the future some new values to this file...
      so actually we should set here sensible "default values".
      We can think of them as "good default values for user opening a settings
      file written by older version of pasdoc_gui program".
      They need not necessarily be equal to default values set by
      SetDefaults method, and this is very good, as it may give us
      additional possibilities. }

    id := lgEnglish;
    s := Ini.ReadString(secMain, mainLanguageName, '');
    if s <> '' then begin
    //get LanguageID
      id := IDfromLanguage(s);
    end else begin
      i := Ini.ReadInteger(secMain, mainLanguage, -1);
      if i = -1 then
        id := lgEnglish;
    end;
    //if lgDummy?
      i := ord(id);
    lbOutLang.ItemIndex := i;
    //comboLanguagesChange(nil);

    edRoot.Text := Ini.ReadString(secMain, mainRootDir, '');
    if edRoot.Text <> '' then
      lbFiles.dlgAdd.InitialDir := edRoot.Text;
    edOutput.Text := Ini.ReadString(secMain, mainOutputDir, '');

    lbOutType.ItemIndex := Ini.ReadInteger(secMain, mainGenerateFormat, 0);
    //comboGenerateFormatChange(nil);

    edProjectName.Text := Ini.ReadString(secMain, mainProjectName, '');
    swLevel.Position := Ini.ReadInteger(secMain, mainVerbosity, 0);
    swImplicitVisibility.ItemIndex := Ini.ReadInteger(secMain, mainVisibility, 0);

    Assert(Ord(High(TVisibility)) = swVisible.Items.Count -1);
    for i := Ord(Low(TVisibility)) to Ord(High(TVisibility)) do
      swVisible.Checked[i] := Ini.ReadBool(
        'Main', 'ClassMembers_' + IntToStr(i), true);
    //clbMethodVisibilityClick(nil);

    Assert(SSI(High(TSortSetting)) = swSort.Count -1);
    //for i := Ord(Low(TSortSetting)) to Ord(High(TSortSetting)) do begin
    for i := 0 to swSort.Count - 1 do begin
      swSort.Checked[i] := Ini.ReadBool(secMain, mainSorting_ + IntToStr(i), True);
    end;

    ReadStrings(secDefines, memoDefines.Lines);
    ReadStrings(secHeader, edHeader.Lines);
    ReadStrings(secFooter, edFooter.Lines);
    ReadStrings(secIncludeDirectories, lbInclude.Items);
    ReadStrings(secFiles, lbFiles.Items);

    edCSS.Text := Ini.ReadString(secMain, mainCssFileName, '');
    edIntro.Text := Ini.ReadString(secMain, mainIntroductionFileName, '');
    edConclusion.Text := Ini.ReadString(secMain, mainConclusionFileName, '');
    swAutoAbstract.Checked := Ini.ReadBool(secMain, mainAutoAbstract, false);
    swShowUses.Checked := Ini.ReadBool(secMain, mainShowUses, False);
    swTipue.Checked := Ini.ReadBool(secMain, mainUseTipueSearch, false);
    lbLineBreaks.ItemIndex := Ini.ReadInteger(secMain, mainLineBreakQuality, 0);
    lbGraphPackage.ItemIndex := Ini.ReadInteger(secMain, mainLatexGraphicsPackage, 0);
    ReadStrings(secHyphenatedWords, lbHyphenate.Lines);
    swMarkers.ItemIndex := Ini.ReadInteger(secMain, mainSpecialMarkerTreatment, 1);
    ReadStrings(secSpecialMarkers, lbMarkers.Lines);
    swBackRef.Checked := Ini.ReadBool(secMain, mainBackRef, True);
    edTitle.Text := Ini.ReadString(secMain, 'Title', '');
    swClassDiagram.Checked := Ini.ReadBool(secMain, mainVizGraphClasses, false);
    swUsesDiagram.Checked := Ini.ReadBool(secMain, mainVizGraphUses, false);

    swSpellCheck.Checked := Ini.ReadBool(secMain, mainCheckSpelling, false);

    ReadStrings(secIgnoreWords, lbIgnored.Lines);
    edProjectFile.Text := SaveName;
  finally
    Ini.Free
  end;

  HasChanged := False;
  UpdateCaption;
end;

procedure TDocMain.Open1Click(Sender: TObject);
begin
//cmd: open project
//check save project!
  if not QuerySave then
    exit; //cancel!
//open project
  if edProjectFile.SelFile then
    LoadSettings(edProjectFile.Text);
end;

procedure TDocMain.Save1Click(Sender: TObject);
begin
//cmd: save project
  SaveDialog1.FileName := edProjectFile.Text;
  SaveDialog1.FilterIndex := 2;
  if SaveDialog1.Execute then begin
    edProjectFile.Text := SaveDialog1.FileName;
    SaveSettings;
    //edProjectFile.Text := SaveDialog1.FileName;
  end;
end;

function TDocMain.QuerySave: boolean;
begin
  Result := True;
  if HasChanged then begin
    case MessageDlg('Save Project?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
    mrYes:  SaveSettings(); //check result? keep project name?
    mrNo: ;
    mrCancel: Result := False;
    end;
  end;
end;

procedure TDocMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := QuerySave;
end;

// -------------- files and directories ---------------

function TDocMain._AddDirs(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
//add directories containing pattern files to lbIncludes
  lbFiles.AddDir(dir);
  Result := feBreak;
end;

function TDocMain._AddFiles(const dir, fn: string;
      const fd: TWIN32FindData; isDir: boolean): eFileEnum;
begin
//add file to lbFiles, directory (unique) to lbIncludes
  lbFiles.AddFile(dir + fn);
  Result := feCont;
end;

procedure TDocMain.edRootbuSelectClick(Sender: TObject);
begin
  if edRoot.SelDir then begin
    lbFiles.dlgAdd.InitialDir := edRoot.Text;
    lbInclude.dlgAdd.InitialDir := edRoot.Text;
    //if edProjectFile.Text = '' then
  end;
end;

procedure TDocMain.lbFilesbuAddAllClick(Sender: TObject);
var
  s, pat, root: string;
begin
(** Cases on adding files
1) specific, one or more
2) by pattern, single or multiple dir???
*)
  lbFiles.dlgAdd.InitialDir := edRoot.Text;
  if lbFiles.dlgAdd.Execute then begin
    s := lbFiles.dlgAdd.FileName;
    if (Pos('*', s) > 0) or (Pos('?', s) > 0) then begin
    //add files by pattern, recursive
      s := lbFiles.dlgAdd.FileName;
      pat := ExtractFileName(s); //.pas ...
      root := ExtractFilePath(s);
      uShell.DirList.EnumFiles(root, pat, _AddFiles, [weFiles, weRecurse]);
    end else begin
    //only add selected files
      lbFiles.AddFiles(lbFiles.dlgAdd.Files);
    end;
  end;
end;

procedure TDocMain.lbIncludebuAddAllClick(Sender: TObject);
var
  s, ext, root: string;
begin
(** Add all directories, containing pattern file
*)
  lbInclude.dlgAdd.InitialDir := edRoot.Text;
  lbInclude.dlgAdd.FileName := '*.inc';
  if lbInclude.dlgAdd.Execute then begin
  //add dirs with files by pattern
    s := lbInclude.dlgAdd.FileName;
    ext := '*'+ExtractFileExt(s); //.pas ...
    root := ExtractFilePath(s);
    uShell.DirList.EnumFiles(root, ext, _AddDirs, [weFiles, weRecurse]);
  end;
end;

// ------------------ verbosity --------------------

procedure TDocMain.swLevelClick(Sender: TObject; Button: TUDBtnType);
begin
(* The associated Edit control will change even when loading.
  It's set to readonly, and only the up/down ctrl will signal changes.
*)
  AnyChange(Sender);
end;

// -------------- output format ------------

procedure TDocMain.lbOutTypeChange(Sender: TObject);
//event handler (comboGenerateFormat)
var
  f: boolean;
begin
{ TODO -oDoDi -cDelphi port : enum for output types }
//plain HTML
  f := lbOutType.ItemIndex = 0;
  swTipue.Enabled := f; // lbOutType.ItemIndex = 0;
//not plain HTML
  edProjectName.Enabled := not f; // lbOutType.ItemIndex <> 0;
  //SetColorFromEnabled(edProjectName);
//all HTML
  f := lbOutType.ItemIndex in [0,1];
  tabHTML.TabVisible := f;  // Ord(lbOutType.ItemIndex in [0,1]);
  edCSS.Enabled := f; //lbOutType.ItemIndex in [0,1];
  //SetColorFromEnabled(EditCssFileName);
//not HTML
{$IFDEF fpc}
  PageLatexOptions.TabVisible := not f; // Ord(lbOutType.ItemIndex in [2,3]);
  comboLatexGraphicsPackage.Enabled := not f; //lbOutType.ItemIndex in [2,3];
{$ELSE}
  tabGraph.TabVisible := not f;
{$ENDIF}

//general options
  tabSpelling.Visible := CheckIfSpellCheckingAvailable;
  //FillNavigationListBox; -> Tabs
  HasChanged := true;
end;

// -------------- member visibility ----------------

(* Options can be copied immediately to PasDoc1, or before Execute.
*)

function TDocMain.VisibilityGet: TVisibilities;
var
  Options: TVisibilities;

  procedure GetOption(f: TVisibility);
  begin
    if swVisible.Checked[ord(f)] then
      Include(Options, f);
  end;

var
  f: TVisibility;
begin
  Options := [];
  for f := low(f) to high(f) do
    GetOption(f);
  //PasDoc1.ShowVisibilities := Options;
  Result := Options;
end;

procedure TDocMain.VisibilityPut(Options: TVisibilities);

  procedure SetOption(f: TVisibility);
  begin
    swVisible.Checked[ord(f)] := (f in Options);
  end;

var
  f: TVisibility;
begin
  for f := low(f) to high(f) do
    SetOption(f);
  //PasDoc1.ShowVisibilities := Options;
end;

// ------------ member sorting -------------

function TDocMain.SSI(so: TSortSetting): integer;
const
  offset = ord(low(so));
begin
  Result := ord(so) - offset;
end;

function TDocMain.SortGet: TSortSettings;
var
  f: TSortSetting;
begin
  Result := [];
  for f := low(f) to high(f) do begin
    if swSort.Checked[SSI(f)] then
      include(Result, f);
  end;
end;

procedure TDocMain.SortPut(Options: TSortSettings);
var
  f: TSortSetting;
begin
  for f := low(f) to high(f) do
    swSort.Checked[SSI(f)] := (f in Options);
end;

// --------------- language ----------------

function TDocMain.LanguageGet: TLanguageID;
begin
  Result := TLanguageID(lbOutLang.ItemIndex);
end;

{$IFDEF fpc}
function TDocMain.LanguageIdToString(const LanguageID: TLanguageID): string;
begin
  Result := LANGUAGE_ARRAY[LanguageID].Syntax;
    //maybe invalid?
end;
{$ELSE}
  //use LanguageFromIndex
{$ENDIF}

function TDocMain.CheckIfSpellCheckingAvailable: boolean;
(** There exist no means to determine unhandled languages!
  Added flag .NoSpellCheck to the language description.
*)
const
  msg = 'Sorry, that language is not supported for spell checking';
begin
  Result := swSpellCheck.Checked and (lbOutType.ItemIndex in [0,1]); //HTML
  if not Result then
    exit; //no spell checking requested or applicable

//applies, but...
{$IFDEF new}
  if LANGUAGE_ARRAY[Language].NoSpellCheck then begin
    Beep;
    MessageDlg(msg, Dialogs.mtError, [mbOK], 0);
    Result := False;  //really not available
  end;
{$ELSE}
  //Result := Language <> lgChinese_950;  //wild guess - removed!
{$ENDIF}
end;

procedure TDocMain.LanguagePut(id: TLanguageID);
begin
  lbOutLang.ItemIndex := ord(id);
end;

procedure TDocMain.lbOutLangChange(Sender: TObject);
begin
  HasChanged := True;
  CheckIfSpellCheckingAvailable;
end;

procedure TDocMain.swSpellCheckClick(Sender: TObject);
begin
  //if swSpellCheck.Checked then
  tabSpelling.TabVisible := CheckIfSpellCheckingAvailable;
  HasChanged := True;
end;

// ---------------- execute -------------------

{$IFDEF old}
procedure TDocMain.SetOutputDirectory(const FileName: string);
begin
//directory names should always end with PathDelim (/ or \)
  edOutput.Text := ExtractFilePath(FileName)
    + 'PasDoc' + PathDelim;
end;
{$ELSE}
{$ENDIF}

procedure TDocMain.buClearOutputClick(Sender: TObject);
begin
  if edOutput.Text = '' then
    exit; //no directory specified
  if MessageDlg('Delete all files in ' + edOutput.Text + '?',
    mtWarning, [mbOK, mbCancel], 0) = mrOk then
  begin
    if not KillAllFiles(edOutput.Text) then
      ShowMessage('Not all files could be deleted');
  end;
end;

procedure TDocMain.edOutputbuSelectClick(Sender: TObject);
begin
//select directory, not file
  edOutput.buSelDirClick(Sender);
end;

procedure TDocMain.buGenerateClick(Sender: TObject);
var
  //Files: TStringList;
  //index: integer;
  //SortIndex: TSortSetting;
  generator: TDocGenerator; //the generators are constructed as components
  //genHTML: TGenericHTMLDocGenerator absolute generator;
  dir: string;
const
  VizGraphImageExtension = 'png';

  procedure SetGenericHTMLoptions(gen: TGenericHTMLDocGenerator);
  begin
    generator := gen;
  //now set the options...
    gen.Header := edHeader.Text;
    gen.Footer := edFooter.Text;

    if edCSS.Text <> '' then
      gen.CSS := edCSS.Text
    else
      gen.CSS := DefaultPasDocCss;

    gen.UseTipueSearch := swTipue.Checked;
    gen.CheckSpelling := swSpellCheck.Checked;
    if gen.CheckSpelling then begin
    //get the short (syntax) name
      //gen.AspellLanguage := LanguageIdToString(TLanguageID(lbOutLang.ItemIndex));
      gen.AspellLanguage := SyntaxFromIndex(lbOutLang.ItemIndex);
      gen.SpellCheckIgnoreWords.Assign(lbIgnored.Lines);
    end;
  end;

  procedure SetTEXoptions(gen: TTexDocGenerator);
  var
    Index: integer;
  begin
    generator := gen;
  //now set the options...
    gen.Latex2rtf := (lbOutType.ItemIndex = 3);
    gen.LatexHead.Clear;

    if lbLineBreaks.ItemIndex = 1 then
      gen.LatexHead.Add('\sloppy');
    if lbHyphenate.Lines.Count > 0 then begin
      gen.LatexHead.Add('\hyphenation{');
      for Index := 0 to lbHyphenate.Lines.Count -1 do begin
        gen.LatexHead.Add(lbHyphenate.Lines[Index]);
      end;
      gen.LatexHead.Add('}');
    end;

    case lbLineBreaks.ItemIndex of
    0: // none
        ; // do nothing
    1: // PDF
        gen.LatexHead.Add('\usepackage[pdftex]{graphicx}');
    2: // DVI
        gen.LatexHead.Add('\usepackage[dvips]{graphicx}');
    else
      Assert(False, 'unknown graphics package');
    end;
  end;

  procedure SetXMLoptions(gen: TDocGenerator);
  begin
    generator := gen;
  end;

var
  i: integer;
  fGenerate: boolean;
begin
//collect and check the parameters
//output directory
  if edOutput.Text = '' then begin
    Beep;
    MessageDlg('You need to specify the output directory on the "Locations" tab.',
      Dialogs.mtWarning, [mbOK], 0);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
try
//log
  lbLog.Clear;  // memoMessages.Clear;
//member visibility
  PasDoc1.ShowVisibilities := Visibility;
  i := swImplicitVisibility.ItemIndex;
  PasDoc1.ImplicitVisibility := TImplicitVisibility(i);
//generator - from components
  case lbOutType.ItemIndex of
  0:  //generic HTML
    SetGenericHTMLoptions(HtmlDocGenerator);
  1:
    SetGenericHTMLoptions(HtmlHelpDocGenerator);
  2, 3:
    SetTEXoptions(TexDocGenerator);
  4:  //simple XML
    SetXMLoptions(SimpleXMLDocGenerator);
  5:
    SetXMLoptions(XMLDocGenerator);
  else
    Assert(False, 'unknown output type');
  end;
  PasDoc1.Generator := generator;

  Generator.Language := Language;

// Create the output directory if it does not exist.
  dir := edOutput.Text;
  if not DirectoryExists(dir) then
    CreateDir(dir);
  Generator.DestinationDirectory := dir;

  Generator.AutoAbstract := swAutoAbstract.Checked;
  generator.WriteUsesClause := swShowUses.Checked;

  PasDoc1.ProjectName := edProjectName.Text;
  PasDoc1.IntroductionFileName := edIntro.Text;
  PasDoc1.ConclusionFileName := edConclusion.Text;

  PasDoc1.SourceFileNames.Clear;
  PasDoc1.AddSourceFileNames(lbFiles.Items);

  PasDoc1.IncludeDirectories.Assign(lbInclude.Items);

  PasDoc1.Directives.Clear;
  PasDoc1.Directives.AddStrings(edSysDefs.Lines);
  PasDoc1.Directives.AddStrings(memoDefines.Lines);

  PasDoc1.Verbosity := swLevel.Position;

  PasDoc1.SingleCharMarkers := False; //default
  case swMarkers.ItemIndex of
  1:
    begin
      PasDoc1.MarkerOptional := True;
      PasDoc1.CommentMarkers.Assign(lbMarkers.Lines);
    end;
  2:
    begin
      PasDoc1.MarkerOptional := False;
      PasDoc1.CommentMarkers.Assign(lbMarkers.Lines);
    end;
  3:
    begin
      PasDoc1.SingleCharMarkers := True;
    end;
  else  //also: none
    PasDoc1.CommentMarkers.Clear;
    PasDoc1.MarkerOptional := True;
    //Assert(False);
  end;
  //PasDoc1.DefaultBackRef := swBackRef.Checked;
  //PasDoc1.SingleCharMarkers := swBackRef.Checked;

  if edTitle.Text = '' then
    PasDoc1.Title := edProjectName.Text
  else
    PasDoc1.Title := edTitle.Text;

  if swClassDiagram.Checked then begin
    Generator.OutputGraphVizClassHierarchy := True;
    Generator.LinkGraphVizClasses := VizGraphImageExtension;
  end else begin
    Generator.OutputGraphVizClassHierarchy := False;
    Generator.LinkGraphVizClasses := '';
  end;

  if swUsesDiagram.Checked then begin
    Generator.OutputGraphVizUses := True;
    Generator.LinkGraphVizUses := VizGraphImageExtension;
  end else begin
    Generator.OutputGraphVizUses := False;
    Generator.LinkGraphVizUses := '';
  end;

  PasDoc1.SortSettings := SortedItems;

  MisspelledWords.Clear;

//now create the documentation
  fGenerate := Sender = buGenerate;
  PasDoc1.Execute(fGenerate);

//after creation
  if MisspelledWords.Count > 0 then begin
    lbLog.Items.Add('');
    lbLog.Items.Add('Misspelled Words');
    lbLog.Items.AddStrings(MisspelledWords);
  end;

  FillTreeView;

  if fGenerate then begin
    if swUsesDiagram.Checked or swClassDiagram.Checked then begin
      // To do: actually start dot here.
      MessageDlg('You will have to run the GraphViz "dot" program to generate '
        + 'the images used in your documentation.', Dialogs.mtInformation,
        [mbOK], 0);
    end;

  {$IFnDEF MSWINDOWS}
    if PasDoc1.Generator is TGenericHTMLDocGenerator then
      WWWBrowserRunner.RunBrowser(
        HtmlDocGenerator.DestinationDirectory + 'index.html');
  {$ELSE}
  {$IFDEF old}
    if PasDoc1.Generator is TGenericHTMLDocGenerator then begin
      ShellExecute(Self.Handle, 'open',
        PChar(HtmlDocGenerator.DestinationDirectory + 'index.html'), nil, nil,
        SW_SHOWNORMAL);
    end else if PasDoc1.Generator is TSimpleXMLDocGenerator then begin
      ShellExecute(Self.Handle, 'open',
        PChar(SimpleXMLDocGenerator.MasterFile), nil, nil,
        SW_SHOWNORMAL);
    end;
  {$ELSE}
    if generator.MasterFile <> '' then
      ShellExecute(Self.Handle, 'open',
        PChar(generator.MasterFile), nil, nil,
        SW_SHOWNORMAL);
  {$ENDIF}
  {$ENDIF}
  end;

finally
  Screen.Cursor := crDefault;
end;  //try

end;

procedure TDocMain.buCmdFileClick(Sender: TObject);
begin
  lbLog.Clear;
  lbLog.Items.Add('<todo: command file>');
end;

procedure TDocMain.SaveLog(Sender: TObject);
begin
  SaveDialog1.FilterIndex := 1;
  SaveDialog1.InitialDir := edProjectFile.DirName;
  SaveDialog1.FileName := ChangeFileExt(edProjectFile.FullName, '.txt');
  if SaveDialog1.Execute then begin
    lbLog.Items.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TDocMain.lbLogKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_DELETE: lbLog.Items.Delete(lbLog.ItemIndex);
  end;
end;

procedure TDocMain.mnCleardiagnosticsClick(Sender: TObject);
var
  i: integer;
  s: string;
  l: TStrings;
begin
  l := lbLog.Items;
  for i := l.Count - 1 downto 0 do begin
    s := l[i];
    if Pos('):', s) < 1 then
      l.Delete(i);
  end;
end;

procedure TDocMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

{$IFDEF old}
procedure TDocMain.FillTreeView;
var
  UnitItem: TPasUnit;
  AllUnitsNode: TTreeNode;
  UnitIndex: integer;
  UnitNode: TTreeNode;
  AllTypesNode: TTreeNode;
  AllVariablesNode: TTreeNode;
  AllCIOs_Node: TTreeNode;
  AllConstantsNode: TTreeNode;
  AllProceduresNode: TTreeNode;
  UsesNode: TTreeNode;
  PasItemIndex: integer;
  PasItem: TPasItem;
  UsesIndex: integer;
  ClassIndex: integer;
  ClassInterfaceObjectOrRecord: TPasCio;
  ClassNode: TTreeNode;
  FieldsNode: TTreeNode;
  MethodNode: TTreeNode;
  PropertiesNode: TTreeNode;
  Lang: TPasDocLanguages;
begin
  tvUnits.Items.Clear;
  Lang := TPasDocLanguages.Create;
  try
    Lang.Language := TLanguageID(lbOutLang.ItemIndex);
    if PasDoc1.IntroductionFileName <> '' then begin
      tvUnits.Items.AddObject(nil, PasDoc1.IntroductionFileName, PasDoc1.Introduction);
    end;
    AllUnitsNode := tvUnits.Items.AddObject(nil,
      Lang.Translation[trUnits], PasDoc1.Units);
    for UnitIndex := 0 to PasDoc1.Units.Count -1 do begin
      UnitItem := PasDoc1.Units.UnitAt[UnitIndex];
      UnitNode := tvUnits.Items.AddChildObject(AllUnitsNode,
        UnitItem.SourceFileName, UnitItem);
      if not IsEmpty(UnitItem.Types) then begin
        AllTypesNode := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trTypes], UnitItem.Types);
        for PasItemIndex := 0 to UnitItem.Types.Count -1 do begin
          PasItem := UnitItem.Types.PasItemAt[PasItemIndex];
          tvUnits.Items.AddChildObject(AllTypesNode, PasItem.Name, PasItem);
        end;
      end;
      if not IsEmpty(UnitItem.Variables) then begin
        AllVariablesNode := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trVariables], UnitItem.Variables);
        for PasItemIndex := 0 to UnitItem.Variables.Count -1 do begin
          PasItem := UnitItem.Variables.PasItemAt[PasItemIndex];
          tvUnits.Items.AddChildObject(AllVariablesNode, PasItem.Name, PasItem);
        end;
      end;
      if not IsEmpty(UnitItem.CIOs) then begin
        AllCIOs_Node := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trCio], UnitItem.CIOs);
        for ClassIndex := 0 to UnitItem.CIOs.Count-1 do begin
          ClassInterfaceObjectOrRecord := UnitItem.CIOs.PasItemAt[ClassIndex] as TPasCio;
          ClassNode := tvUnits.Items.AddChildObject(AllCIOs_Node,
            ClassInterfaceObjectOrRecord.Name, ClassInterfaceObjectOrRecord);
          if not IsEmpty(ClassInterfaceObjectOrRecord.Fields) then begin
            FieldsNode := tvUnits.Items.AddChildObject(ClassNode,
              Lang.Translation[trFields], ClassInterfaceObjectOrRecord.Fields);
            for PasItemIndex := 0 to ClassInterfaceObjectOrRecord.Fields.Count -1 do begin
              PasItem := ClassInterfaceObjectOrRecord.Fields.PasItemAt[PasItemIndex];
              tvUnits.Items.AddChildObject(FieldsNode, PasItem.Name, PasItem);
            end;
          end;
          if not IsEmpty(ClassInterfaceObjectOrRecord.Methods) then begin
            MethodNode := tvUnits.Items.AddChildObject(ClassNode,
              Lang.Translation[trMethods], ClassInterfaceObjectOrRecord.Methods);
            for PasItemIndex := 0 to ClassInterfaceObjectOrRecord.Methods.Count -1 do begin
              PasItem := ClassInterfaceObjectOrRecord.Methods.PasItemAt[PasItemIndex];
              tvUnits.Items.AddChildObject(MethodNode, PasItem.Name, PasItem);
            end;
          end;
          if not IsEmpty(ClassInterfaceObjectOrRecord.Properties) then begin
            PropertiesNode := tvUnits.Items.AddChildObject(ClassNode,
              Lang.Translation[trProperties], ClassInterfaceObjectOrRecord.Properties);
            for PasItemIndex := 0 to ClassInterfaceObjectOrRecord.Properties.Count -1 do begin
              PasItem := ClassInterfaceObjectOrRecord.Properties.PasItemAt[PasItemIndex];
              tvUnits.Items.AddChildObject(PropertiesNode, PasItem.Name, PasItem);
            end;
          end;
        end;
      end;
      if not IsEmpty(UnitItem.Constants) then begin
        AllConstantsNode := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trConstants], UnitItem.Constants);
        for PasItemIndex := 0 to UnitItem.Constants.Count -1 do begin
          PasItem := UnitItem.Constants.PasItemAt[PasItemIndex];
          tvUnits.Items.AddChildObject(AllConstantsNode, PasItem.Name, PasItem);
        end;
      end;
      if not IsEmpty(UnitItem.FuncsProcs) then begin
        AllProceduresNode := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trFunctionsAndProcedures], UnitItem.FuncsProcs);
        for PasItemIndex := 0 to UnitItem.FuncsProcs.Count -1 do begin
          PasItem := UnitItem.FuncsProcs.PasItemAt[PasItemIndex];
          tvUnits.Items.AddChildObject(AllProceduresNode, PasItem.Name, PasItem);
        end;
      end;
      if not IsEmpty(UnitItem.UsesUnits) then begin
        UsesNode := tvUnits.Items.AddChildObject(UnitNode,
          Lang.Translation[trUses], UnitItem.UsesUnits);
        for UsesIndex := 0 to UnitItem.UsesUnits.Count -1 do begin
          tvUnits.Items.AddChild(UsesNode, UnitItem.UsesUnits.Strings[UsesIndex]);
        end;
      end;
    end;
    if PasDoc1.ConclusionFileName <> '' then begin
      tvUnits.Items.AddObject(nil, PasDoc1.ConclusionFileName,
        PasDoc1.Conclusion);
    end;
  finally
    Lang.Free;
  end;
end;
{$ELSE}
procedure TDocMain.FillTreeView;
var
  Lang: TPasDocLanguages;
  UnitItem: TPasUnit;
  AllUnitsNode: TTreeNode;
  UnitIndex: integer;
  UnitNode: TTreeNode;

  procedure populate(root: TTreeNode; rootitem: TDescriptionItem);
  var
    i: integer;
    n: TTreeNode;
    item: TDescriptionItem;
    s: string;
  begin
    for i := 0 to rootitem.Count - 1 do begin
      item := rootitem.ItemAt(i);
      if item.ID = trNoTrans then
        //s := item.Name
        s := item.Text(' ', '')
      else
        s := Lang.Translation[item.ID];
      n := tvUnits.Items.AddChildObject(root, s, item);
      if item.Count > 0 then
        populate(n, item);
    end;
  end;

begin
  tvUnits.Items.Clear;
  Lang := TPasDocLanguages.Create;
  try
    Lang.Language := TLanguageID(lbOutLang.ItemIndex);
    if PasDoc1.IntroductionFileName <> '' then begin
      tvUnits.Items.AddObject(nil, PasDoc1.IntroductionFileName, PasDoc1.Introduction);
    end;
    AllUnitsNode := tvUnits.Items.AddObject(nil,
      Lang.Translation[trUnits], PasDoc1.Units);
    for UnitIndex := 0 to PasDoc1.Units.Count -1 do begin
      UnitItem := PasDoc1.Units.UnitAt[UnitIndex];
      UnitNode := tvUnits.Items.AddChildObject(AllUnitsNode,
        UnitItem.SourceFileName, UnitItem);
      populate(UnitNode, UnitItem);
    end;
  finally
    Lang.Free;
  end;
end;
{$ENDIF}

procedure TDocMain.tvUnitsClick(Sender: TObject);
var
  Item: TDescriptionItem;
  bi: TBaseItem absolute item;
  PasItem: TPasItem absolute Item;
  o: TObject;
  c: TToken absolute o;
  lst: TStrings;
  i: integer;
  s: string;
begin
  edRem.Clear;
  edDecl.Text := '';
  edRem.Hint := '';
  cbRem.Clear;
  if (tvUnits.Selected <> nil) and (tvUnits.Selected.Data <> nil) then begin
    if TObject(tvUnits.Selected.Data) is TDescriptionItem then begin
      Item := TDescriptionItem(tvUnits.Selected.Data);
      SelItem := Item;
      SelToken := nil;
      if item is TPasItem then
        edDecl.Text := PasItem.FullDeclaration
      else begin
        //edDecl.Text := '<no declaration>';
        s := Item.Name + ' ' + Item.Value; //Item.Text();
        if Item.ID <> trNoTrans then
          s := Translation(Item.ID, Language) + ': ' + s;
        edDecl.Text := s;
      end;
      edRem.Text := Item.RawDescription;
      if item is TBaseItem then begin
        lst := bi.RawDescriptions;
        cbRem.AddItem('<all>', nil);
        if assigned(lst) then begin
          for i := 0 to lst.Count-1 do begin
            o := lst.Objects[i];
            if assigned(o) then begin
              s := Format(
                '%s[%d - %d]',
                [ c.StreamName,
                  c.BeginPosition,
                  c.EndPosition ]);
            end else
              s := '<unknown/inherited>';
            cbRem.AddItem(s, o);
          end;
        end;
      end;
    end;
  end;
end;

procedure TDocMain.cbRemClick(Sender: TObject);
var
  i: integer;
  o: TObject;
  c: TToken absolute o;
begin
  i := cbRem.ItemIndex;
  if i < 1 then
    edRem.Text := SelItem.RawDescription
  else begin
    o := cbRem.Items.Objects[i];
    SelToken := c;
    if assigned(o) then begin
      edRem.Text := c.CommentContent;
    end else if SelItem is TBaseItem then
      edRem.Text := TBaseItem(SelItem).RawDescriptions.Strings[i-1]
    else
      edRem.Text := SelItem.RawDescription;
  end;
end;

end.
