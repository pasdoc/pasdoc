{ @abstract(Contains the main TPasDoc component.)
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michael van Canneyt (michael@tfdec1.fys.kuleuven.ac.be))
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @created(24 Sep 1999)
  
  Unit name must be @code(PasDoc_Base) instead of just @code(PasDoc)
  to not conflict with the name of base program name @code(pasdoc.dpr).
}

unit PasDoc_Base;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_Gen,
  PasDoc_Types,
  PasDoc_StringVector,
  PasDoc_SortSettings,
  PasDoc_TagManager
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
    FShowVisibilities: TVisibilities;
    FMarkerOptional, FSingleCharMarkers: boolean;
    FIgnoreLeading: string;
    FCacheDir: string;
    FSortSettings: TSortSettings;
    FConclusionFileName: string;
    FIntroductionFileName: string;
    FConclusion: TExternalItem;
    FIntroduction: TExternalItem;
    FImplicitVisibility: TImplicitVisibility;
    FHandleMacros: boolean;
    FAutoLink: boolean;
    procedure SetDescriptionFileNames(const ADescriptionFileNames: TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetGenerator(const Value: TDocGenerator);
    procedure SetStarStyle(const Value: boolean);
    function GetStarStyle: boolean;
    procedure SetCommentMarkers(const Value: TStringList);
      
    { Creates a @link(TPasUnit) object from the stream and adds it to
      @link(FUnits). }
    procedure HandleStream(
      const InputStream: TStream;
      const SourceFileName: string);
    procedure HandleExternalFile(
      const FileName: string; out ExternalItem: TExternalItem);
    { Calls @link(HandleStream) for each file name in @link(SourceFileNames). }
    procedure ParseFiles;
    procedure SkipBOM(InputStream: TStream);
  protected
  {$IFDEF old}
    { Searches the description of each TPasUnit item in the collection for an
      excluded tag.
      If one is found, the item is removed from the collection. }
    procedure RemoveExcludedItems(const c: TPasItems);
  {$ELSE}
    //- Units should never be bestroyed after BuildLinks.
  {$ENDIF}

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Creates object and sets fields to default values. }
    constructor Create(AOwner: TComponent); override;
    { }
    destructor Destroy; override;

    { Adds source filenames from a stringlist }
    procedure AddSourceFileNames(const AFileNames: TStrings);
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
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);
    { for Generator messages }
    procedure GenMessage(const MessageType: TPasDocMessageType; const
      AMessage: string; const AVerbosity: Cardinal);
    { Starts creating the documentation. }
    procedure Execute(fGenerate: boolean = True);
    // After @link(Execute) has been called, @name holds the units that have
    // been parsed. Copied into the generator.
    property Units: TPasUnits read FUnits;
    // After @link(Execute) has been called, @name holds the conclusion.
    property Conclusion: TExternalItem read FConclusion;
    // After @link(Execute) has been called, @name holds the introduction.
    property Introduction: TExternalItem read FIntroduction;
  published
    property DescriptionFileNames: TStringVector 
      read FDescriptionFileNames write SetDescriptionFileNames;
    property Directives: TStringVector read FDirectives write SetDirectives;
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
    property SingleCharMarkers: boolean read FSingleCharMarkers write FSingleCharMarkers;
    property IgnoreLeading: string read FIgnoreLeading write FIgnoreLeading;

    property Generator: TDocGenerator read FGenerator write SetGenerator;
    property ShowVisibilities: TVisibilities read FShowVisibilities write FShowVisibilities;
    property CacheDir: string read FCacheDir write FCacheDir; 
    
    { This determines how items inside will be sorted.
      See [http://pasdoc.sipsolutions.net/SortOption]. }
    property SortSettings: TSortSettings 
      read FSortSettings write FSortSettings default [];
      
    property IntroductionFileName: string read FIntroductionFileName
      write FIntroductionFileName;
      
    property ConclusionFileName: string read FConclusionFileName
      write FConclusionFileName;
      
    { See command-line option @--implicit-visibility documentation at
      [http://pasdoc.sipsolutions.net/ImplicitVisibilityOption].
      This will be passed to parser instance. }
    property ImplicitVisibility: TImplicitVisibility
      read FImplicitVisibility write FImplicitVisibility default ivPublic;
      
    property HandleMacros: boolean
      read FHandleMacros write FHandleMacros default true;
      
    { This controls auto-linking, see
      [http://pasdoc.sipsolutions.net/AutoLinkOption] }
    property AutoLink: boolean
      read FAutoLink write FAutoLink default false;
  end;

  { ---------------------------------------------------------------------------- }
  { Compiler Identification Constants }
  { ---------------------------------------------------------------------------- }

{ This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function COMPILER_NAME: string;

const
  COMPILER_BITS =  {$ifdef CPU64} '64' {$else} '32' {$endif};

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
{$IFDEF DARWIN}
  COMPILER_OS = 'Darwin';
{$ENDIF}

  { ---------------------------------------------------------------------------- }
  { PasDoc Version Constants }
  { ---------------------------------------------------------------------------- }

  {  }
  PASDOC_NAME = 'PasDoc';
  
  { Date of last pasdoc release.
  
    We used to have this constant set to CVS/SVN @code($ Date) keyword, but:
    @unorderedList(
      @item(That's not a really correct indication of pasdoc release.
        @code($ Date) is only the date when this file, @code(PasDoc_Base.pas),
        was last modified.
        
        As it happens, always when you make an official release
        you have to manually change PASDOC_VERSION constant
        in this file below. So PASDOC_DATE was
        (at the time when the official release was made) updated to current date.
        But, since you have to change PASDOC_VERSION constant manually
        anyway, then it's not much of a problem to also update PASDOC_DATE
        manually.
        
        For unofficial releases (i.e. when pasdoc is simply compiled from SVN
        by anyone, or when it's packaged for
        [http://pasdoc.sipsolutions.net/DevelopmentSnapshots]),
        PASDOC_DATE has no clear meaning. It's not the date of this
        release (since you don't update the PASDOC_VERSION constant)
        and it's not the date of last official release (since some
        commits possibly happened to @code(PasDoc_Base.pas) since
        last release).
      )

      @item(SVN makes this date look bad for the purpose of
        PASDOC_FULL_INFO. It's too long: contains the time,
        day of the week, and a descriptive version. Like
        @preformatted(2006-11-15 07:12:34 +0100 (Wed, 15 Nov 2006))
        
        Moreover, it contains indication of local user's system time,
        and the words (day of the week and month's name) are
        localized. So it depends on the locale developer has set
        (you can avoid localization of the words by doing things like
        @code(export LANG=C) before SVN operations, but it's too
        error-prone).
      )
    )
  }
  PASDOC_DATE = '2008-06-22';
  { }
  PASDOC_VERSION = '0.11.5';
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
  PasDoc_ObjectVector,
  PasDoc_Utils,
  PasDoc_Serialize;

constructor TPasDoc.Create(AOwner: TComponent);
begin
  inherited;

  FDescriptionFileNames := NewStringVector;
  FDirectives := NewStringVector;
  FIncludeDirectories := NewStringVector;
  FSourceFileNames := NewStringVector;

  { Set default property values }
  FGeneratorInfo := true;
  FVerbosity := DEFAULT_VERBOSITY_LEVEL;
  FImplicitVisibility := ivPublic;
  HandleMacros := true;

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
  FConclusion.Free;
  FIntroduction.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.SkipBOM(InputStream: TStream);
const
  BOM: string = #$EF#$BB#$BF;
var
  c: char;
  i: integer;
begin
  i := 0;
  repeat
    if InputStream.Position >= InputStream.Size then
      DoError('Tokenizer: could not read character', [], 0);
    InputStream.Read(c, 1);
    Inc(i);
    if i > Length(BOM) then
      begin
        InputStream.Position := InputStream.Position - 1;
        exit;
      end;
  until c <> BOM[i];
  InputStream.Position := 0;
end;

procedure TPasDoc.HandleStream(
  const InputStream: TStream;
  const SourceFileName: string);
var
  p: TParser;
  U: TPasUnit;
  LLoaded: boolean;
  LCacheFileName: string;
begin
  SkipBOM(InputStream);
  LCacheFileName := CacheDir+ChangeFileExt(ExtractFileName(SourceFileName), '.pduc');
  p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
    {$IFDEF FPC}@{$ENDIF} GenMessage, FVerbosity,
    SourceFileName, ExtractFilePath(SourceFileName), HandleMacros);
  try
    PasDoc_items.ShowVisibilities := ShowVisibilities; //must be known to all CIOs
    p.ImplicitVisibility := ImplicitVisibility;
    p.CommentMarkers := CommentMarkers;
    p.MarkersOptional := MarkerOptional;
    p.SingleCharMarkers := SingleCharMarkers;
    p.IgnoreLeading := IgnoreLeading;

    LLoaded := false;

    if (CacheDir <> '') and FileExists(LCacheFileName) then
    begin
      DoMessage(2, pmtInformation, 'Loading data for file %s from cache...', [SourceFileName]);
      U := TPasUnit(TPasUnit.DeserializeFromFile(LCacheFileName));
      U.CacheDateTime := FileDateToDateTime(FileAge(LCacheFileName));
      if U.CacheDateTime < FileDateToDateTime(FileAge(SourceFileName)) then
      begin
        DoMessage(2, pmtInformation, 'Cache file for %s is outdated.',
          [SourceFileName]);
      end else begin
        LLoaded := True;
      end;
    end;

    if not LLoaded then
    begin
      DoMessage(2, pmtInformation, 'Now parsing file %s...', [SourceFileName]);
      p.ParseUnitOrProgram(U);
    end;

    if FUnits.ExistsUnit(U) then begin
      DoMessage(2, pmtWarning,
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
  except
     on e: Exception do begin
       DoMessage(2, pmtWarning, 
         'Error %s: %s while parsing unit %s, continuing...', 
         [e.ClassName, e.Message, ExtractFileName(SourceFileName)]); 
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

  procedure ParseExternalFile(const FileName: string;
    var ExternalItem: TExternalItem);
  begin
    if FileName <> '' then
    begin
      HandleExternalFile(FileName, ExternalItem);
      Inc(Count);
    end;
  end;

begin
  FUnits.clear;

  DoMessage(1, pmtInformation, 'Starting Source File Parsing ...', []);
  if FSourceFileNames.IsEmpty then Exit;

  InputStream := nil;
  Count := 0;
  for i := 0 to FSourceFileNames.Count - 1 do
  begin
    p := FSourceFileNames[i];
    try
      InputStream := TFileStream.Create(p, fmOpenRead or fmShareDenyWrite);
    except
      on E: Exception do
      begin
        DoMessage(1, pmtError, 'Cannot open file "%s", skipping', [p]);
        Continue;
      end;
    end;

    { Note that HandleStream frees InputStream. }
    HandleStream(InputStream, p);
  //try auto-include *.txt
    p := ChangeFileExt(p, '.txt');
    if FileExists(p) then
      Self.DescriptionFileNames.AddNotExisting(p);
    Inc(Count);
  end;

  FreeAndNil(FIntroduction);
  ParseExternalFile(IntroductionFileName, FIntroduction);
  FreeAndNil(FConclusion);
  ParseExternalFile(ConclusionFileName, FConclusion);

  DoMessage(2, pmtInformation, '... %d Source File(s) parsed', [Count]);
end;

{ ---------------------------------------------------------------------------- }

{$IFDEF old}
procedure TPasDoc.RemoveExcludedItems(const c: TPasItems);
var
  i: Integer;
  p: TPasItem;
begin
(* Exclude units from FUnits.
  Other items are excluded by TPasScope.
  The units should be excluded only from the generator list!
  Here we construct the list!
*)
  if c = nil then Exit;
  for i := c.Count - 1 downto 0 do begin
    p := c.PasItemAt[i];
    if p.toBeExcluded then
      c.Delete(i);
  end;
end;
{$ELSE}
{$ENDIF}

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.Execute(fGenerate: boolean);
var
  t1, t2: TDateTime;
  CacheDirNoDelim: string;
begin
  if not Assigned(Generator) then begin
    DoError('No Generator present!', [], 1);
  end;
  { Do a couple of tests before we actually start processing the source files. }
  if FSourceFileNames.IsEmpty then begin
    DoError('No Source Files have been specified.', [], 1);
  end;
  if (CacheDir <> '') then
  begin
    {$ifdef WIN32}
    { This is needed to make DirectoryExists and CreateDir work
      when user used UNIX-like delimiters "/" inside CacheDir
      (yes, it's normally allowed under Windows, so pasdoc should work with
      it too) }
    CacheDir := SCharsReplace(CacheDir, ['/'], PathDelim);
    {$endif}

    CacheDirNoDelim := ExcludeTrailingPathDelimiter(CacheDir);
    CacheDir := IncludeTrailingPathDelimiter(CacheDir);
    if not DirectoryExists(CacheDirNoDelim) then begin
      if not CreateDir(CacheDirNoDelim) then begin
        DoError('Cache directory does not exist and could not be created', [], 1);
      end;
    end;
  end;

  { Make sure all IncludeDirectories end with a Path Separator. }
  FIncludeDirectories.Iterate( {$IFDEF FPC}@{$ENDIF} IncludeTrailingPathDelimiter);

  t1 := Now;
  ParseFiles;

  if ObjectVectorIsNilOrEmpty(FUnits) then
    DoError('At least one unit must have been successfully parsed ' +
        'to write docs', [], 1);

  if FProjectName <> '' then
    Generator.ProjectName := FProjectName
  else
    Generator.ProjectName := 'docs';

  Generator.Title := Title;
  Generator.Units := FUnits; //should be a separately shrinkable list
  Generator.Introduction := FIntroduction;
  Generator.Conclusion := FConclusion;
  Generator.AutoLink := AutoLink;
  Generator.BuildLinks; //may become invalid by destruction of excluded units!
  Generator.MasterFile := ''; //must be initialized by the specific generators.

  FUnits.SortDeep(SortSettings);

//read external descriptions, found while parsing the units.
//required for the editor, even if no docs are created.
  Generator.LoadDescriptionFiles(FDescriptionFileNames);

  if fGenerate then begin
    Generator.ExpandDescriptions; //here items are marked for removal
  //combine the following actions?
    //RemoveExcludedItems(TPasItems(FUnits)); //only after expanding descriptions!
    Generator.BuildUnitSections;
    //Generator.Units.BuildSections; <-- included in RemoveExcludedUnits
    //FUnits.BuildSections; //optional!

    Generator.WriteDocumentation;
  end else
    FUnits.BuildSections; //optional, for editor, debugger...

  if Generator.NoGeneratorInfo then
    DoMessage(1, pmtInformation, 'Done', [])
  else begin
    t2 := Now;
    DoMessage(1, pmtInformation, 'Done, worked %s minutes(s)',
      [FormatDateTime('nn:ss', (t2 - t1))]);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDoc.DoMessage(const AVerbosity: Cardinal; const AMessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
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

procedure TPasDoc.GenMessage(const MessageType: TPasDocMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  DoMessage(AVerbosity, MessageType, AMessage, []);
end;

procedure TPasDoc.SetGenerator(const Value: TDocGenerator);
begin
  if Assigned(FGenerator) then begin
    FGenerator.OnMessage := nil;
  end;
  FGenerator := Value; //obsolete(?)
  TheGenerator := Value;
  if Assigned(FGenerator) then begin
    FGenerator.FreeNotification(Self);
    FGenerator.OnMessage := {$IFDEF FPC}@{$ENDIF} GenMessage;
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

procedure TPasDoc.AddSourceFileNames(const AFileNames: TStrings);
var
  SR: TSearchRec;
  FileMask, Path, s: string;
  i: Integer;
  SearchResult: Integer;
begin
  for i := 0 to AFileNames.Count - 1 do begin
    FileMask := AFileNames[i];
    Path := ExtractFilePath(FileMask);

    { Just ignore last empty line of AFileNames, this may often occur
      when generating text files with filenames, and is harmless. }
    if (FileMask = '') and (I = AFileNames.Count - 1) then
      Continue;

    SearchResult := SysUtils.FindFirst(FileMask, 63, SR);
    if SearchResult <> 0 then begin
      DoMessage(1, pmtWarning, 'No files found for "%s", skipping', [FileMask]);
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

procedure TPasDoc.HandleExternalFile(const FileName: string;
  out ExternalItem: TExternalItem);
begin
  ExternalItem := TExternalItem.Create;
  try
    DoMessage(2, pmtInformation, 'Now parsing file %s...', [FileName]);

    { This check tries to avoid the possibility of accidentaly
      overwriting user introduction/conclusion file
      (in case some user would incorrectly think that
      introduction/conclusion is in raw html, and would create file like
      my_introduction.html -- without this check, pasdoc could
      overwrite this file too easily). }
    if SameText(ExtractFileExt(FileName), Generator.GetFileExtension) then
      raise Exception.CreateFmt('Introduction/conclusion file extension' +
        ' is the same as file extension of generated documentation ("%s"), ' +
        'refusing to generate documentation', [Generator.GetFileExtension]);

    ExternalItem.Name := SCharsReplace(
      ChangeFileExt( ExtractFileName(FileName) , ''), [' '], '_');

    ExternalItem.RawDescription := FileToString(FileName);
  except
    FreeAndNil(ExternalItem);
    raise;
  end;
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

    {$IFDEF DELPHI_10} 'DELPHI 10'; {$ENDIF}
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
