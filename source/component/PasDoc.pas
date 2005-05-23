{ @abstract(Contains the main TPasDoc component. )
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michael van Canneyt (michael@tfdec1.fys.kuleuven.ac.be))
  @created(24 Sep 1999)
}

{$ifdef FPC}
  { Turn macro on to get FPC_VERSION, FPC_RELEASE, FPC_PATCH macros }
  {$macro on}
{$endif}

unit PasDoc;

{$I DEFINES.INC}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_Gen,
  PasDoc_Types,
  StringVector
{$IFNDEF FPC}
{$IFDEF WIN32}
{$IFNDEF DELPHI_6_UP}
  ,FileCtrl
{$ENDIF}
{$ENDIF}
{$ENDIF}
  ;
  
const
  { }
  DEFAULT_VERBOSITY_LEVEL = 2;

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
    FCommentMarkers: TStringList;
    FGenerator: TDocGenerator;
    FClassMembers: TAccessibilities;
    FMarkerOptional: boolean;
    FCacheDir: string;
    procedure SetDescriptionFileNames(const ADescriptionFileNames: TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetGenerator(const Value: TDocGenerator);
    procedure SetStarStyle(const Value: boolean);
    function GetStarStyle: boolean;
    procedure SetCommentMarkers(const Value: TStringList);
    procedure HandleDescrfileTag(const TagName, TagDesc: string;
      var ReplaceStr: string);
      
    { Creates a @link(TPasUnit) object from the stream and adds it to
      @link(FUnits). }
    procedure HandleStream(
      const InputStream: TStream;
      const SourceFileName: string);
    { Calls @link(HandleStream) for each file name in @link(SourceFileNames). }
    procedure ParseFiles;
  protected
    { Searches the description of each TPasUnit item in the collection for an
      excluded tag.
      If one is found, the item is removed from the collection.
      If not, the fields, methods and properties collections are called
      with RemoveExcludedItems
      If the collection is empty after removal of all items, it is disposed
      of and the variable is set to nil. }
    procedure RemoveExcludedItems(const c: TPasItems);
    
    (*
    { Searches for descr tags in the comments of all TPasItem objects in C. }
    procedure SearchDescrFileTags(const c: TPasItems);
    *)
    
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Creates object and sets fields to default values. }
    constructor Create(AOwner: TComponent); override;
    { }
    destructor Destroy; override;

    { Adds source filenames from a stringlist }
    procedure AddSourceFileNames(const AFileNames: TStringList);
    { Loads names of Pascal unit source code files from a text file.
      Adds all file names to @link(SourceFileNames). 
      If DashMeansStdin and AFileName = '-' then it will load filenames
      from stdin. }
    procedure AddSourceFileNamesFromFile(const FileName: string;
      DashMeansStdin: boolean);
    { Raises an exception. }
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);
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

    { This is deprecated name for @link(OnMessage) }
    property OnWarning: TPasDocMessageEvent read FOnMessage write FOnMessage;
    
    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    property ProjectName: string read FProjectName write FProjectName;
    property SourceFileNames: TStringVector read FSourceFileNames write
      SetSourceFileNames;
    property Title: string read FTitle write FTitle;
    property Verbosity: Cardinal read FVerbosity write FVerbosity 
      default DEFAULT_VERBOSITY_LEVEL;
    property StarStyleOnly: boolean read GetStarStyle write SetStarStyle;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkerOptional: boolean read FMarkerOptional write FMarkerOptional
      default false;

    property Generator: TDocGenerator read FGenerator write SetGenerator;
    property ClassMembers: TAccessibilities read FClassMembers write FClassMembers;
    property CacheDir: string read FCacheDir write FCacheDir; 
  end;

  { ---------------------------------------------------------------------------- }
  { Compiler Identification Constants }
  { ---------------------------------------------------------------------------- }

{ This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function COMPILER_NAME: string;

const
  COMPILER_BITS = '32';

{$IFDEF LINUX}
  COMPILER_OS = 'Linux';
{$ENDIF}
{$IFDEF WIN32}
  COMPILER_OS = 'MSWindows';
{$ENDIF}
{$IFDEF BEOS}
  COMPILER_OS = 'BeOS';
{$ENDIF}
{$IFDEF QNX}
  COMPILER_OS = 'QNX';
{$ENDIF}
{$IFDEF AMIGA}
  COMPILER_OS = 'AmigaOS';
{$ENDIF}
{$IFDEF SUNOS}
  COMPILER_OS = 'SunOS';
{$ENDIF}
{$IFDEF GO32V2}
  COMPILER_OS = 'DOS/Go32v2';
{$ENDIF}
{$IFDEF OS2}
  COMPILER_OS = 'OS/2';
{$ENDIF}
{$IFDEF FREEBSD}
  COMPILER_OS = 'FreeBSD';
{$ENDIF}

  { ---------------------------------------------------------------------------- }
  { PasDoc Version Constants }
  { ---------------------------------------------------------------------------- }

  {  }
  PASDOC_NAME = 'PasDoc';
  { }
  PASDOC_DATE = '$Date$';
  { }
  PASDOC_VERSION = '0.8.8.3';
  { }
  PASDOC_NAME_AND_VERSION = PASDOC_NAME + ' ' + PASDOC_VERSION;
  { }
  PASDOC_HOMEPAGE = 'http://pasdoc.sourceforge.net/';

{ Returns pasdoc name, version, used compiler version, etc.

  This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function PASDOC_FULL_INFO: string;

implementation

uses
  PasDoc_Parser,
  PasDoc_TagManager,
  ObjectVector,
  Utils, PasDoc_Serialize;

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
  FCommentMarkers := TStringList.Create;
  FUnits := TPasUnits.Create(True);
end;

{ ---------------------------------------------------------------------------- }

destructor TPasDoc.Destroy;
begin
  FCommentMarkers.Free;
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
  LParseSuccessful,
  LLoaded: boolean;
  LCacheFileName: string;
begin
  LCacheFileName := CacheDir+ChangeFileExt(ExtractFileName(SourceFileName), '.pduc');
  p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
    {$IFDEF FPC}@{$ENDIF} GenMessage, FVerbosity, SourceFileName);
  p.ClassMembers := ClassMembers;
  try
    p.CommentMarkers := CommentMarkers;
    p.MarkersOptional := MarkerOptional;

    LLoaded := false;
    LParseSuccessful := false;

    if (CacheDir <> '') and FileExists(LCacheFileName) then
    begin
      DoMessage(2, mtInformation, 'Loading data for file %s from cache...', [SourceFileName]);
      U := TPasUnit(TPasUnit.DeserializeFromFile(LCacheFileName));
      U.CacheDateTime := FileDateToDateTime(FileAge(LCacheFileName));
      if U.CacheDateTime < FileDateToDateTime(FileAge(SourceFileName)) then
      begin
        DoMessage(2, mtInformation, 'Cache file for %s is outdated.', 
          [SourceFileName]);
      end else begin
        LParseSuccessful := True;
        LLoaded := True;
      end;
    end;

    if not LLoaded then begin
      DoMessage(2, mtInformation, 'Now parsing file %s...', [SourceFileName]);
      LParseSuccessful := p.ParseUnit(U);
    end;

    if LParseSuccessful then begin
      if FUnits.ExistsUnit(U) then begin
        DoMessage(2, mtWarning,
          'Duplicate unit name "%s" in files "%s" and "%s" (discarded)', [U.Name,
          TPasUnit(FUnits.FindName(U.Name)).SourceFileName, SourceFileName]);
        U.Free;
      end else 
      begin
        U.SourceFileName := SourceFileName;
        U.SourceFileDateTime := FileDateToDateTime(FileAge(SourceFileName));
        FUnits.Add(U);
        
        { Now we know that unit was 100% successfully parsed.
        
          So now we save it to the cache. The current approach to cache
          stores in cache the exact state of unit as it was generated by
          parser (that why we can use deserialization as an equivalent
          of parsing), so we want to save the unit to cache *now*,
          in case some later processing would change some things.
          E.g. processing @deprecated tag will change item's
          IsDeprecated, processing @member and @value will change
          some item's RawDescription. We want to write the cache
          *before* such changes occur. }

        if (CacheDir <> '') and not U.WasDeserialized then
          U.SerializeToFile(LCacheFileName);
      end;
    end else begin
      DoMessage(2, mtWarning, 'Could not parse unit %s, but continuing anyway', [U.Name]);
      U.Free;
    end;
  except
     on e: Exception do begin
       DoMessage(2, mtWarning, 'Error %s: %s parsing unit %s, continuing...', [e.ClassName, e.Message, ExtractFileName(SourceFileName)]); 
     end;
  end;
  p.Free;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.AddSourceFileNamesFromFile(const FileName: string;
  DashMeansStdin: boolean);
var
  ASV: TStringVector;
begin
  ASV := NewStringVector;
  try
    if DashMeansStdin and (FileName = '-') then
      ASV.LoadFromTextFileAdd(Input) else
      ASV.LoadFromTextFileAdd(FileName);

    AddSourceFileNames(ASV);
  finally  
    ASV.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.ParseFiles;
var
  Count, i: Integer;
  p: string;
  InputStream: TStream;
begin
  FUnits.clear;

  DoMessage(1, mtInformation, 'Starting Source File Parsing ...', []);
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

procedure TPasDoc.RemoveExcludedItems(const c: TPasItems);
var
  i: Integer;
  p: TPasItem;
begin
  if c = nil then Exit;
  i := 0;
  while (i < c.Count) do begin
    p := c.PasItemAt[i];
    
    { TODO -- code below checks for @exclude tag too trivially,
      it accidentaly excludes items with comments like '@@exclude'
      or '@html(@exclude)'. Checking for exclude should be
      incorporated into doing TTagManager.Execute
      in ExpandDescription. }

    if Assigned(p) and (StrPosIA('@EXCLUDE', p.RawDescription) > 0) then 
    begin
      DoMessage(3, mtInformation, 'Excluding item %s', [p.Name]);
      c.Delete(i);
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
  if (CacheDir <> '') then begin
    CacheDir := IncludeTrailingPathDelimiter(CacheDir);
    if not DirectoryExists(CacheDir) then begin
      if not CreateDir(CacheDir) then begin
        DoError('Cache directory does not exist and could not be created', [], 1);
      end;
    end;
  end;

  { Make sure all IncludeDirectories end with a Path Separator. }
  FIncludeDirectories.Iterate(@IncludeTrailingPathDelimiter);

  t1 := Now;
  ParseFiles;
  RemoveExcludedItems(TPasItems(FUnits));
  { check if parsing was successful }

  if ObjectVectorIsNilOrEmpty(FUnits) then begin
    DoError('At least one unit must have been successfully parsed to write docs.', [], 1);
  end;

  if FProjectName <> '' then begin
    Generator.ProjectName := FProjectName
  end else begin
    Generator.ProjectName := 'docs';
  end;

  Generator.Title := Title;
  Generator.Units := FUnits;
  Generator.BuildLinks;

  Generator.LoadDescriptionFiles(FDescriptionFileNames);
  Generator.ExpandDescriptions;  

  Generator.WriteDocumentation;

  if Generator.NoGeneratorInfo then
    DoMessage(1, mtInformation, 'Done', []) else
  begin
    t2 := Now;
    DoMessage(1, mtInformation, 'Done, worked %s minutes(s)',
      [FormatDateTime('nn:ss', (t2 - t1))]);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.HandleDescrfileTag(const TagName, TagDesc: string; var ReplaceStr: string);
begin
  DoMessage(3, mtInformation, 'Adding description file "%s"', [TagDesc]);
  DescriptionFileNames.Add(TagDesc);
  ReplaceStr := '';
end;

(*
TODO -- this code is not decided to be removed, but it needs to be 
rearranged (by someone who knows what is the intended purpose
of DescriptionFileNames, as they don't seem to be used now
and SearchDescrFileTags of this object is never called)
to use TTagManager.Execute inside TDocGenerator.ExpandDescription.

That's because current use of TTagManager.Execute is too simple
-- it will catch @descrfile also inside @html() and @longcode()
and it will complain to user about unknown tags,
since it will know only about @descrfile tag.

procedure TPasDoc.SearchDescrFileTags(const c: TPasItems);
var
  i: Integer;
  p: TPasItem;
  s: string;
  TagManager: TTagManager;
begin
  if (not Assigned(c)) then Exit;
  i := 0;
  while (i < c.Count) do begin
    p := c.PasItemAt[i];
    Inc(i);
    if (not Assigned(p)) then Continue;
    if p.RawDescription <> '' then begin
      TagManager := TTagManager.Create;
      try
        TagManager.AddHandler('descrfile', 
          {$IFDEF FPC}@{$ENDIF}HandleDescrfileTag, false, false);
        s := TagManager.Execute(p.RawDescription);
      finally
        TagManager.Free;
      end;
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
*)
{ ---------------------------------------------------------------------------- }

procedure TPasDoc.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
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
{$IFDEF FPC}
    FGenerator.OnMessage := @GenMessage;
{$ELSE}
    FGenerator.OnMessage := GenMessage;
{$ENDIF}
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

procedure TPasDoc.SetStarStyle(const Value: boolean);
var
  Idx: Integer;
begin
  if Value then begin
    FCommentMarkers.Add('**');
  end else begin
    Idx := FCommentMarkers.IndexOf('**');
    if Idx <> -1 then
      FCommentMarkers.Delete(Idx);
  end;
end;

function TPasDoc.GetStarStyle: boolean;
begin
  Result := FCommentMarkers.IndexOf('**') <> -1;
end;

procedure TPasDoc.SetCommentMarkers(const Value: TStringList);
begin
  FCommentMarkers.Assign(Value);
end;

{ non-object routines -------------------------------------------------------- }

function COMPILER_NAME: string;
begin
  COMPILER_NAME := 
    {$IFDEF FPC}
    'FPC ' + Format('%d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);
    {$ENDIF}

    {$IFDEF KYLIX_1} 'KYLIX 1'; {$ENDIF}
    {$IFDEF KYLIX_2} 'KYLIX 2'; {$ENDIF}
    {$IFDEF KYLIX_3} 'KYLIX 3'; {$ENDIF}

    {$IFDEF DELPHI_9} 'DELPHI 9'; {$ENDIF}
    {$IFDEF DELPHI_7} 'DELPHI 7'; {$ENDIF}
    {$IFDEF DELPHI_6} 'DELPHI 6'; {$ENDIF}
    {$IFDEF DELPHI_5} 'DELPHI 5'; {$ENDIF}
    {$IFDEF DELPHI_4} 'DELPHI 4'; {$ENDIF}
end;

function PASDOC_FULL_INFO: string;
begin
  PASDOC_FULL_INFO :=
    PASDOC_NAME_AND_VERSION + ' [' + PASDOC_DATE + '|' +
      COMPILER_NAME + '|' + COMPILER_OS + '|' + COMPILER_BITS + ']';
end;

end.
