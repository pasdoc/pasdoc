{ @abstract(Contains the main TPasDoc component. )
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michael van Canneyt (michael@tfdec1.fys.kuleuven.ac.be))
  @created(24 Sep 1999)
  }

unit PasDoc;

{$R BinData.res}
{$I VERSIONS.INC}
{$I DEFINES.INC}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector;

const
  DEFAULT_VERBOSITY_LEVEL = 3;

type
  { }
  TMessageType = (mtPlainText, mtInformation, mtWarning, mtError);
  { }
  TPasDocMessageEvent = procedure(const MessageType: TMessageType; const
    AMessage: string; const AVerbosity: Cardinal) of object;

  { ---------------------------------------------------------------------------- }
  { EPasDoc
  { ---------------------------------------------------------------------------- }

{ }
  EPasDoc = class(Exception)
  public
    constructor Create(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
  end;

  { ---------------------------------------------------------------------------- }

  { all supported output formats }
  TOutputFormat = (ofDefault, ofHtml, ofHtmlHelp);

  { The main object in the pasdoc application; first scans parameters, then
    parses files.
    All parsed units are then given to documentation generator,
    which creates one or more documentation output files. }
  TPasDoc = class(TComponent)
  private
    FDescriptionFileNames: TStringVector;
    { Title of documentation. }
    FTitle: string;
    FDirectives: TStringVector;
    FFooter: string;
    FGeneratorInfo: Boolean;
    FHeader: string;
    FHtmlHelpContentsFileName: string;
    FIncludeDirectories: TStringVector;
    FIncludePrivate: Boolean;
    FInvokeHtmlHelpCompiler: Boolean;
    FLanguage: TLanguageID;
    FOnMessage: TPasDocMessageEvent;
    FOutputFolder: string;
    FOutputFormat: TOutputFormat;
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    FProjectName: string;
    FSourceFileNames: TStringVector;
    { All TPasUnit objects which have been created from the list of file names
      during the parsing. }
    FUnits: TPasUnits;
    FVerbosity: Cardinal;
    FStarStyle: boolean;
    procedure SetDescriptionFileNames(const ADescriptionFileNames:
      TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
  protected
    { Creates a @link(TPasUnit) object from the stream and adds it to
   @link(Units). }
    procedure HandleStream(
      const InputStream: TStream;
      const SourceFileName: string);
    { Calls @link(HandleStream) for each file name in @link(FileNames). }
    procedure ParseFiles;
    { Searches the description of each TPasUnit item in the collection for an
      excluded tag.
      If one is found, the item is removed from the collection.
      If not, the fields, methods and properties collections are called
      with RemoveExcludedItems
      If the collection is empty after removal of all items, it is disposed
      of and the variable is set to nil. }
    procedure RemoveExcludedItems(var c: TPasItems);
    { Searches for descr tags in the comments of all TPasItem objects in C. }
    procedure SearchDescrFileTags(var c: TPasItems);
  public
    { Creates object and sets fields to default values. }
    constructor Create(AOwner: TComponent); override;
    { }
    destructor Destroy; override;

    { Loads names of Pascal unit source code files from a text file.
      Adds all file names to @link(SourceFileNames). }
    procedure AddSourceFileNamesFromFile(const FileName: string);
    { Raises an exception. }
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
    { Forwards a message to the @link(OnMessage) event. }
    procedure DoMessage(const AVerbosity: Cardinal; const AMessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);
    { Starts creating the documentation. }
    procedure Execute;
    { }
    procedure LoadFooterFromFile(const AFileName: string);
    { }
    procedure LoadHeaderFromFile(const AFileName: string);
  published
    property DescriptionFileNames: TStringVector read FDescriptionFileNames
      write SetDescriptionFileNames;
    property Directives: TStringVector read FDirectives write SetDirectives;
    property Footer: string read FFooter write FFooter;
    property Header: string read FHeader write FHeader;
    property HtmlHelpContentsFileName: string read FHtmlHelpContentsFileName
      write FHtmlHelpContentsFileName;
    property IncludeDirectories: TStringVector read FIncludeDirectories write
      SetIncludeDirectories;
    { Defines if the private parts (fields, methods and properties) of objects
      are to be included in the documentation (default is @False). }
    property IncludePrivate: Boolean read FIncludePrivate write
      FIncludePrivate;
    { Registry and HCC only exists on Windows. }
    { When creating HtmlHelp output, PasDoc will look for the Html Help Compiler
      "HCC.exe" in the registry and compile the project. Set @Name to @False
      to suppress automatic compilation of Html Help projects. }
    property InvokeHtmlHelpCompiler: Boolean read FInvokeHtmlHelpCompiler
      write FInvokeHtmlHelpCompiler default True;
    { Language used to write the documentation. }
    property Language: TLanguageID read FLanguage write FLanguage;
    { By default, PasDoc adds generator information and a time stamp to the documentation.
      Set @Name to @False to suppress this information. }
    property GeneratorInfo: Boolean read FGeneratorInfo write FGeneratorInfo
      default True;
    property OnWarning: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property OutputFolder: string read FOutputFolder write FOutputFolder;
    property OutputFormat: TOutputFormat read FOutputFormat write
      FOutputFormat;
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    property ProjectName: string read FProjectName write FProjectName;
    property SourceFileNames: TStringVector read FSourceFileNames write
      SetSourceFileNames;
    property Title: string read FTitle write FTitle;
    property Verbosity: Cardinal read FVerbosity write FVerbosity;
    property StarStyleOnly: boolean read FStarStyle write FStarStyle;
  end;

  { ---------------------------------------------------------------------------- }
  { Compiler Identification Constants
  { ---------------------------------------------------------------------------- }

const
{$IFDEF KYLIX_1}
  COMPILER_NAME = 'KYLIX 1';
{$ENDIF}

{$IFDEF KYLIX_2}
  COMPILER_NAME = 'KYLIX 2';
{$ENDIF}

{$IFDEF KYLIX_3}
  COMPILER_NAME = 'KYLIX 3';
{$ENDIF}

{$IFDEF DELPHI_7}
  COMPILER_NAME = 'DELPHI 7';
{$ENDIF}

{$IFDEF DELPHI_6}
  COMPILER_NAME = 'DELPHI 6';
{$ENDIF}

{$IFDEF DELPHI_5}
  COMPILER_NAME = 'DELPHI 5';
{$ENDIF}

{$IFDEF DELPHI_4}
  COMPILER_NAME = 'DELPHI 4';
{$ENDIF}

  COMPILER_BITS = '32';

{$IFDEF LINUX}
  COMPILER_OS = 'Linux';
{$ENDIF}
{$IFDEF MSWINDOWS}
  COMPILER_OS = 'MSWindows';
{$ENDIF}

  { ---------------------------------------------------------------------------- }
  { PasDoc Version Constants
  { ---------------------------------------------------------------------------- }

  {  }
  PASDOC_NAME = 'DelphiDoc';
  { }
  PASDOC_DATE = '2003-03-28';
  { }
  PASDOC_VERSION = '0.8.6';
  { }
  PASDOC_NAME_AND_VERSION = PASDOC_NAME + ' ' + PASDOC_VERSION;
  { }
  PASDOC_HOMEPAGE = '<old URL outdated>';
  { }
  PASDOC_FULL_INFO = PASDOC_NAME_AND_VERSION + ' [' + PASDOC_DATE + '|' +
    COMPILER_NAME + '|' + COMPILER_OS + '|' + COMPILER_BITS + ']';

implementation

uses
  PasDoc_Gen,
  PasDoc_GenHtml,
  PasDoc_Parser,
  ObjectVector,
  Utils;

const
  { The output format to be used when none is specified on command line. }
  DEFAULT_OUTPUT_FORMAT: TOutputFormat = ofHtml;
  { Names of all output formats. }
  OUTPUT_FORMAT_NAMES: array[TOutputFormat] of string = ('', 'HTML',
    'HTML Help');

  { ---------------------------------------------------------------------------- }

constructor TPasDoc.Create(AOwner: TComponent);
begin
  inherited;

  FGeneratorInfo := True;
  FInvokeHtmlHelpCompiler := True;

  FDescriptionFileNames := NewStringVector;
  FDirectives := NewStringVector;
  FIncludeDirectories := NewStringVector;
  FSourceFileNames := NewStringVector;

  FVerbosity := DEFAULT_VERBOSITY_LEVEL;
end;

{ ---------------------------------------------------------------------------- }

destructor TPasDoc.Destroy;
begin
  FDescriptionFileNames.Free;
  FDirectives.Free;
  FIncludeDirectories.Free;
  FSourceFileNames.Free;
  FUnits.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.HandleStream(
  const InputStream: TStream;
  const SourceFileName: string);
var
  p: TParser;
  U: TPasUnit;
begin
  p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
    FOnMessage, FVerbosity);
  try
    p.StarStyleOnly := StarStyleOnly;

    if p.ParseUnit(U) then begin
      if not IncludePrivate then U.RemovePrivateItems;
      if FUnits = nil then FUnits := NewPasUnits(True);

      if FUnits.ExistsUnit(U) then begin
        DoMessage(2, mtWarning,
          'Duplicate unit name "%s" in files "%s" and "%s" (discarded)', [U.Name,
          U.SourceFileName, SourceFileName]);
        U.Free;
      end else begin
        U.SourceFileName := SourceFileName;
        FUnits.InsertObjectLast(U);
      end;
    end else begin
      U.Free;
    end;
  finally
    p.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.AddSourceFileNamesFromFile(const FileName: string);
var
  ASV: TStringVector;
  i: Integer;
  FileMask, Path, s: string;
  SearchResult: Integer;
  SR: SysUtils.TSearchRec;
begin
  ASV := NewStringVector;
  ASV.LoadFromTextFileAdd(FileName);

  for i := 0 to ASV.Count - 1 do begin
    FileMask := ASV[i];
    Path := ExtractFilePath(FileMask);

    SearchResult := SysUtils.FindFirst(FileMask, 63, SR);
    while SearchResult = 0 do begin
      if (SR.Attr and 24) = 0 then begin
        s := Path + SR.Name;
        if not FSourceFileNames.ExistsNameCI(s) then
          FSourceFileNames.Add(s)
      end;
      SearchResult := FindNext(SR);
    end;
    SysUtils.FindClose(SR);
  end;

  ASV.Free;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.ParseFiles;
var
  Count, i: Integer;
  p: string;
  InputStream: TStream;
begin
  DoMessage(2, mtInformation, 'Starting Source File Parsing ...', []);
  if FSourceFileNames.IsEmpty then Exit;

  Count := 0;
  for i := 0 to FSourceFileNames.Count - 1 do begin
    p := FSourceFileNames[i];
    DoMessage(2, mtInformation, 'Parsing "%s"', [p]);
    InputStream := TFileStream.Create(p, fmOpenRead);
    if Assigned(InputStream) then begin
      // HandleStream frees InputStream!
      HandleStream(InputStream, p);
      Inc(Count);
    end else begin
      DoMessage(2, mtError, 'Parsing "%s"', [p]);
    end;
  end;

  DoMessage(2, mtInformation, '... %d Source File(s) parsed', [Count]);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.RemoveExcludedItems(var c: TPasItems);
var
  i: Integer;
  p: TPasItem;
begin
  if c = nil then Exit;
  i := 0;
  while (i < c.Count) do begin
    p := c.PasItemAt[i];
    if Assigned(p) and (p.Description <> '') and (StrPosIA('@EXCLUDE',
      p.Description) > 0) then begin
      DoMessage(3, mtInformation, 'Excluding item %s', [p.Name]);
      c.DeleteAt(i);
    end
    else begin
          { P has no excluded tag; but if it is a class, interface, object or
            unit, one of its parts may be excluded }
      if p.ClassType = TPasCio then begin
        RemoveExcludedItems(TPasCio(p).Fields);
        RemoveExcludedItems(TPasItems(TPasCio(p).Properties));
        RemoveExcludedItems(TPasItems(TPasCio(p).Methods));
      end
      else
        if p.ClassType = TPasUnit then begin
          RemoveExcludedItems(TPasUnit(p).CIOs);
          RemoveExcludedItems(TPasUnit(p).Constants);
          RemoveExcludedItems(TPasItems(TPasUnit(p).FuncsProcs));
          RemoveExcludedItems(TPasUnit(p).Types);
          RemoveExcludedItems(TPasUnit(p).Variables);
        end;
      Inc(i);
    end;
  end;

  FreeAndNilIfEmpty(c);
end;

{ ---------------------------------------------------------------------------- }

procedure IterateIncludeTrailingPathDelimiter(var AString: string);
begin
  AString := IncludeTrailingPathDelimiter(AString);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.Execute;
var
  Generator: TDocGenerator;
  t1, t2: TDateTime;
begin
  { Do a couple of tests before we actually start processing the source files. }
  if FSourceFileNames.IsEmpty then
    DoError('No Source Files have been specified.', [], 1);

  if not DirectoryExists(FOutputFolder) then
    DoError('Output Folder does not exist (%s).', [FOutputFolder]);
  FOutputFolder := IncludeTrailingPathDelimiter(FOutputFolder);

  { If no output format has been defined on the command line, pick
    default output format, show warning }
  if OutputFormat = ofDefault then begin
    DoMessage(1, mtWarning,
      'No Output Format specified, using default ("%s")',
      [OUTPUT_FORMAT_NAMES[DEFAULT_OUTPUT_FORMAT]]);
    OutputFormat := DEFAULT_OUTPUT_FORMAT;
  end;

  { Set default language in case a language does not provide a translation. }
  LANGUAGE_ARRAY[DEFAULT_LANGUAGE].Proc;
  if Language <> lgDefault then begin
    LANGUAGE_ARRAY[Language].Proc
  end else begin
    DoMessage(1, mtWarning,
      'No Output Language specified, using default ("%s")',
      [LANGUAGE_ARRAY[DEFAULT_LANGUAGE].Name]);
  end;

  { Make sure all IncludeDirectories end with a Path Separator. }
  FIncludeDirectories.Iterate(IterateIncludeTrailingPathDelimiter);

  t1 := Now;
  ParseFiles;
  RemoveExcludedItems(TPasItems(FUnits));
  { check if parsing was successful }

  if IsNilOrEmpty(FUnits) then
    DoError('At least one unit must have been successfully parsed to write docs.', [], 1);

  DoMessage(3, mtInformation, 'Creating %s documentation file(s)...',
    [OUTPUT_FORMAT_NAMES[OutputFormat]]);
  { create desired output generator }
  case OutputFormat of
    ofHtml, ofHtmlHelp: begin
        Generator := THTMLDocGenerator.Create(FOnMessage, FVerbosity);
        // Additional settings for Html Help
        if OutputFormat = ofHtmlHelp then
          with THTMLDocGenerator(Generator) do begin
            ContentsFile := Self.FHtmlHelpContentsFileName;
            HtmlHelp := True;
            NoHHC := not Self.InvokeHtmlHelpCompiler;
          end;
      end;
  else begin
      Generator := nil; { to prevent 'uninitialized' warnings }
      DoError('Output format unknown.', []);
    end;
  end;
  { copy some fields from P to Generator }
  Generator.Header := FHeader;
  Generator.Footer := FFooter;
  Generator.Language := Language;
  Generator.DestDir := FOutputFolder;
  Generator.NoGeneratorInfo := not GeneratorInfo;

  if FProjectName <> '' then
    Generator.ProjectName := FProjectName
  else
    Generator.ProjectName := Translation[trHelp];

  Generator.Title := Title;
  Generator.Units := FUnits;
  Generator.BuildLinks;

  Generator.ExpandDescriptions;
  Generator.LoadDescriptionFiles(FDescriptionFileNames);

  // Write Binary Files first, ...
  Generator.WriteBinaryFiles;
  // ... because WriteDocumentation may need them (i.e. when calling HHC.exe).
  Generator.WriteDocumentation;

  Generator.Free;

  t2 := Now;
  DoMessage(1, mtInformation, 'Done, worked %s minutes(s)',
    [FormatDateTime('nn:ss', (t2 - t1))]);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SearchDescrFileTags(var c: TPasItems);
var
  Found: Boolean;
  i: Integer;
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  p: TPasItem;
  s: string;
begin
  if (not Assigned(c)) then Exit;
  i := 0;
  while (i < c.Count) do begin
    p := c.PasItemAt[i];
    Inc(i);
    if (not Assigned(p)) then Continue;
    if p.Description <> '' then begin
      Offs1 := 0;
      repeat
        Found := p.DescriptionFindTag(p.Description, 'DESCRFILE', Offs1,
          Offs2, Offs3);
        if Found then begin
          p.DescriptionExtractTag(p.Description, Offs1, Offs2, Offs3, s);
          DoMessage(3, mtInformation, 'Adding description file "%s"', [s]);
          DescriptionFileNames.Add(s);
          Offs1 := Offs3 + 1;
        end;
      until (not Found);
    end;

    if p.ClassType = TPasCio then begin
      SearchDescrFileTags(TPasCio(p).Fields);
      SearchDescrFileTags(TPasItems(TPasCio(p).Methods));
      SearchDescrFileTags(TPasItems(TPasCio(p).Properties));
    end;

    if p.ClassType = TPasUnit then begin
      SearchDescrFileTags(TPasUnit(p).CIOs);
      SearchDescrFileTags(TPasUnit(p).Constants);
      SearchDescrFileTags(TPasItems(TPasUnit(p).FuncsProcs));
      SearchDescrFileTags(TPasUnit(p).Types);
      SearchDescrFileTags(TPasUnit(p).Variables);
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Integer = 0);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.DoMessage(const AVerbosity: Cardinal; const AMessageType:
  TMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity <= FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(AMessageType, Format(AMessage, AArguments), AVerbosity);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SetDescriptionFileNames(const ADescriptionFileNames:
  TStringVector);
begin
  FDescriptionFileNames.Assign(ADescriptionFileNames);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SetDirectives(const ADirectives: TStringVector);
begin
  FDirectives.Assign(ADirectives);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SetIncludeDirectories(const AIncludeDirectores:
  TStringVector);
begin
  FIncludeDirectories.Assign(AIncludeDirectores);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SetSourceFileNames(const ASourceFileNames: TStringVector);
begin
  FSourceFileNames.Assign(ASourceFileNames);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.LoadFooterFromFile(const AFileName: string);
begin
  if not LoadStrFromFileA(AFileName, FFooter) then begin
    DoMessage(1, mtError, 'Could not read Footer from file "%s".',
      [AFileName]);
    FFooter := '';
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.LoadHeaderFromFile(const AFileName: string);
begin
  if not LoadStrFromFileA(AFileName, FHeader) then begin
    DoMessage(1, mtError, 'Could not read Header from file "%s".',
      [AFileName]);
    FHeader := '';
  end;
end;

{ ---------------------------------------------------------------------------- }
{ EPasDoc
{ ---------------------------------------------------------------------------- }

constructor EPasDoc.Create(const AMessage: string; const AArguments: array of
  const; const AExitCode: Integer = 0);
begin
  ExitCode := AExitCode;
  CreateFmt(AMessage, AArguments);
end;

end.
