{ @abstract(Contains the main TPasDoc component. )
  @lastmodified(2003-03-28)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michael van Canneyt (michael@tfdec1.fys.kuleuven.ac.be))
  @created(24 Sep 1999)
  }

unit PasDoc;

{$I VERSIONS.INC}
{$I DEFINES.INC}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_Gen,
  PasDoc_Types,
  StringVector;
  
const
  DEFAULT_VERBOSITY_LEVEL = 3;

type
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
    FGeneratorInfo: Boolean;
    FHtmlHelpContentsFileName: string;
    FIncludeDirectories: TStringVector;
    FOnMessage: TPasDocMessageEvent;
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    FProjectName: string;
    FSourceFileNames: TStringVector;
    { All TPasUnit objects which have been created from the list of file names
      during the parsing. }
    FUnits: TPasUnits;
    FVerbosity: Cardinal;
    FStarStyle: boolean;
    FGenerator: TDocGenerator;
    FClassMembers: TAccessibilities;
    procedure SetDescriptionFileNames(const ADescriptionFileNames: TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetGenerator(const Value: TDocGenerator);
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Creates object and sets fields to default values. }
    constructor Create(AOwner: TComponent); override;
    { }
    destructor Destroy; override;

    { Adds source filenames from a stringlist }
    procedure AddSourceFileNames(const AFileNames: TStringList);
    { Loads names of Pascal unit source code files from a text file.
      Adds all file names to @link(SourceFileNames). }
    procedure AddSourceFileNamesFromFile(const FileName: string);
    { Raises an exception. }
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
    { Forwards a message to the @link(OnMessage) event. }
    procedure DoMessage(const AVerbosity: Cardinal; const AMessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);
    { for Generator messages }
    procedure GenMessage(const MessageType: TMessageType; const
      AMessage: string; const AVerbosity: Cardinal);
    { Starts creating the documentation. }
    procedure Execute;
  published
    property DescriptionFileNames: TStringVector read FDescriptionFileNames
      write SetDescriptionFileNames;
    property Directives: TStringVector read FDirectives write SetDirectives;
    property HtmlHelpContentsFileName: string read FHtmlHelpContentsFileName
      write FHtmlHelpContentsFileName;
    property IncludeDirectories: TStringVector read FIncludeDirectories write
      SetIncludeDirectories;

    property OnWarning: TPasDocMessageEvent read FOnMessage write FOnMessage;
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    property ProjectName: string read FProjectName write FProjectName;
    property SourceFileNames: TStringVector read FSourceFileNames write
      SetSourceFileNames;
    property Title: string read FTitle write FTitle;
    property Verbosity: Cardinal read FVerbosity write FVerbosity;
    property StarStyleOnly: boolean read FStarStyle write FStarStyle;

    property Generator: TDocGenerator read FGenerator write SetGenerator;
    property ClassMembers: TAccessibilities read FClassMembers write FClassMembers; 
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
  PASDOC_NAME = 'PasDoc';
  { }
  PASDOC_DATE = '2003-04-05';
  { }
  PASDOC_VERSION = '0.8.6-jb';
  { }
  PASDOC_NAME_AND_VERSION = PASDOC_NAME + ' ' + PASDOC_VERSION;
  { }
  PASDOC_HOMEPAGE = 'http://pasdoc.sourceforge.net/';
  { }
  PASDOC_FULL_INFO = PASDOC_NAME_AND_VERSION + ' [' + PASDOC_DATE + '|' +
    COMPILER_NAME + '|' + COMPILER_OS + '|' + COMPILER_BITS + ']';

implementation

uses
  PasDoc_GenHtml,
  PasDoc_Parser,
  ObjectVector,
  Utils;

constructor TPasDoc.Create(AOwner: TComponent);
begin
  inherited;

  FGeneratorInfo := True;
  FDescriptionFileNames := NewStringVector;
  FDirectives := NewStringVector;
  FIncludeDirectories := NewStringVector;
  FSourceFileNames := NewStringVector;

  FVerbosity := DEFAULT_VERBOSITY_LEVEL;

  FGenerator := nil;
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
  DoMessage(3, mtInformation, 'Now parsing file %s...', [SourceFileName]);
  p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
    FOnMessage, FVerbosity, SourceFileName);
  p.ClassMembers := ClassMembers;
  try
    p.StarStyleOnly := StarStyleOnly;

    if p.ParseUnit(U) then begin
      if FUnits = nil then FUnits := TPasUnits.Create(True);

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
      DoMessage(2, mtWarning, 'Could not parse unit %s, but continuing anyway', [U.Name]);
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
begin
  ASV := NewStringVector;
  ASV.LoadFromTextFileAdd(FileName);

  AddSourceFileNames(ASV);

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
    if not FileExists(p) then begin
       DoMessage(1, mtError, 'Could not find or open file "%s", skipping', [p]);
    end else begin
      InputStream := TFileStream.Create(p, fmOpenRead);
      if Assigned(InputStream) then begin
        // HandleStream frees InputStream!
        HandleStream(InputStream, p);
        Inc(Count);
      end else begin
        DoMessage(2, mtError, 'Parsing "%s"', [p]);
      end;
    end;
  end;
  FUnits.SortByPasItemName;
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
    if Assigned(p) and (StrPosIA('@EXCLUDE', p.DetailedDescription) > 0) then begin
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

procedure TPasDoc.Execute;
var
  t1, t2: TDateTime;
begin
  if not Assigned(Generator) then begin
    DoError('No Generator present!', [], 1);
  end;
  { Do a couple of tests before we actually start processing the source files. }
  if FSourceFileNames.IsEmpty then begin
    DoError('No Source Files have been specified.', [], 1);
  end;

  { Make sure all IncludeDirectories end with a Path Separator. }
  FIncludeDirectories.Iterate(IncludeTrailingPathDelimiter);

  t1 := Now;
  ParseFiles;
  RemoveExcludedItems(TPasItems(FUnits));
  { check if parsing was successful }

  if IsNilOrEmpty(FUnits) then begin
    DoError('At least one unit must have been successfully parsed to write docs.', [], 1);
  end;

  if FProjectName <> '' then begin
    Generator.ProjectName := FProjectName
  end else begin
    Generator.ProjectName := 'Help Project';
  end;

  Generator.Title := Title;
  Generator.Units := FUnits;
  Generator.BuildLinks;

  Generator.ExpandDescriptions;
  Generator.LoadDescriptionFiles(FDescriptionFileNames);

  Generator.WriteDocumentation;

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
    if p.DetailedDescription <> '' then begin
      Offs1 := 0;
      repeat
        Found := p.DescriptionFindTag(p.DetailedDescription, 'DESCRFILE', Offs1,
          Offs2, Offs3);
        if Found then begin
          p.DescriptionExtractTag(p.DetailedDescription, Offs1, Offs2, Offs3, s);
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
  FSourceFileNames.Clear;
  AddSourceFileNames(ASourceFileNames);
end;

procedure TPasDoc.GenMessage(const MessageType: TMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  DoMessage(AVerbosity, MessageType, AMessage, []);
end;

procedure TPasDoc.SetGenerator(const Value: TDocGenerator);
begin
  if Assigned(FGenerator) then begin
    FGenerator.OnMessage := nil;
  end;
  FGenerator := Value;
  if Assigned(FGenerator) then begin
    FGenerator.FreeNotification(Self);
    FGenerator.OnMessage := GenMessage;
  end;
end;

procedure TPasDoc.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FGenerator) and (Operation = opRemove) then begin
    FGenerator := nil;
  end;
end;

procedure TPasDoc.AddSourceFileNames(const AFileNames: TStringList);
var
  SR: TSearchRec;
  FileMask, Path, s: string;
  i: Integer;
  SearchResult: Integer;
begin
  for i := 0 to AFileNames.Count - 1 do begin
    FileMask := AFileNames[i];
    Path := ExtractFilePath(FileMask);

    SearchResult := SysUtils.FindFirst(FileMask, 63, SR);
    if SearchResult <> 0 then begin
      DoMessage(1, mtWarning, 'No files found for "%s", skipping', [FileMask]);
    end else begin
      repeat
        if (SR.Attr and 24) = 0 then begin
          s := Path + SR.Name;
          if not FSourceFileNames.ExistsNameCI(s) then
            FSourceFileNames.Add(s)
        end;
        SearchResult := FindNext(SR);
      until SearchResult <> 0;
    end;
    SysUtils.FindClose(SR);
  end;
end;

end.
