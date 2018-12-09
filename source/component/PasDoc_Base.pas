{
  Copyright 1998-2018 PasDoc developers.

  This file is part of "PasDoc".

  "PasDoc" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "PasDoc" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "PasDoc"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ @abstract(Contains the main TPasDoc component.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michael van Canneyt (michael@tfdec1.fys.kuleuven.ac.be))
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @author(Arno Garrels <first name.name@nospamgmx.de>)
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
  PasDoc_StreamUtils,
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
    FMarkerOptional: boolean;
    FIgnoreLeading: string;
    FCacheDir: string;
    FSortSettings: TSortSettings;
    FConclusionFileName: string;
    FIntroductionFileName: string;
    FAdditionalFilesNames: TStringList;
    FConclusion: TExternalItem;
    FIntroduction: TExternalItem;
    FAdditionalFiles: TExternalItemList;
    FImplicitVisibility: TImplicitVisibility;
    FHandleMacros: boolean;
    FAutoLink: boolean;
    procedure SetDescriptionFileNames(const ADescriptionFileNames: TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetGenerator(const Value: TDocGenerator);
    procedure SetStarOnly(const Value: boolean);
    function GetStarOnly: boolean;
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
{$IFNDEF STRING_UNICODE}
    procedure SkipBOM(InputStream: TStream);
{$ENDIF}
  protected
    { Searches the description of each TPasUnit item in the collection for an
      excluded tag.
      If one is found, the item is removed from the collection.
      If not, the fields, methods and properties collections are called
      with RemoveExcludedItems
      If the collection is empty after removal of all items, it is disposed
      of and the variable is set to nil. }
    procedure RemoveExcludedItems(const c: TPasItems);

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
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);
    { for Generator messages }
    procedure GenMessage(const MessageType: TPasDocMessageType; const
      AMessage: string; const AVerbosity: Cardinal);
    { Starts creating the documentation. }
    procedure Execute;
    // After @link(Execute) has been called, @name holds the units that have
    // been parsed.
    property Units: TPasUnits read FUnits;
    // After @link(Execute) has been called, @name holds the conclusion.
    property Conclusion: TExternalItem read FConclusion;
    // After @link(Execute) has been called, @name holds the introduction.
    property Introduction: TExternalItem read FIntroduction;
    // After @link(Execute) has been called, @name holds the additional external files.
    property AdditionalFiles: TExternalItemList read FAdditionalFiles;
  published
    property DescriptionFileNames: TStringVector
      read FDescriptionFileNames write SetDescriptionFileNames;
    property Directives: TStringVector read FDirectives write SetDirectives;
    property IncludeDirectories: TStringVector read FIncludeDirectories write
      SetIncludeDirectories;

    { This is deprecated name for @link(OnMessage) }
    property OnWarning: TPasDocMessageEvent read FOnMessage write FOnMessage stored false;

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;

    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    property ProjectName: string read FProjectName write FProjectName;
    property SourceFileNames: TStringVector read FSourceFileNames write
      SetSourceFileNames;
    property Title: string read FTitle write FTitle;
    property Verbosity: Cardinal read FVerbosity write FVerbosity
      default DEFAULT_VERBOSITY_LEVEL;
    property StarOnly: boolean read GetStarOnly write SetStarOnly stored false;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkerOptional: boolean read FMarkerOptional write FMarkerOptional
      default false;
    property IgnoreLeading: string read FIgnoreLeading write FIgnoreLeading;

    property Generator: TDocGenerator read FGenerator write SetGenerator;
    property ShowVisibilities: TVisibilities read FShowVisibilities write FShowVisibilities;
    property CacheDir: string read FCacheDir write FCacheDir;

    { This determines how items inside will be sorted.
      See [https://github.com/pasdoc/pasdoc/wiki/SortOption]. }
    property SortSettings: TSortSettings
      read FSortSettings write FSortSettings default [];

    property IntroductionFileName: string read FIntroductionFileName
      write FIntroductionFileName;

    property ConclusionFileName: string read FConclusionFileName
      write FConclusionFileName;

    property AdditionalFilesNames: TStringList read FAdditionalFilesNames;

    { See command-line option @--implicit-visibility documentation at
      [https://github.com/pasdoc/pasdoc/wiki/ImplicitVisibilityOption].
      This will be passed to parser instance. }
    property ImplicitVisibility: TImplicitVisibility
      read FImplicitVisibility write FImplicitVisibility default ivPublic;

    property HandleMacros: boolean
      read FHandleMacros write FHandleMacros default true;

    { This controls auto-linking, see
      [https://github.com/pasdoc/pasdoc/wiki/AutoLinkOption] }
    property AutoLink: boolean
      read FAutoLink write FAutoLink default false;
  end;

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
  FAdditionalFilesNames := TStringList.Create();

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
  FAdditionalFilesNames.Free;
  FUnits.Free;
  FConclusion.Free;
  FIntroduction.Free;
  FAdditionalFiles.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }
{$IFNDEF STRING_UNICODE}
procedure TPasDoc.SkipBOM(InputStream: TStream);
var
  A : array [0..3] of Byte;
begin
  InputStream.ReadBuffer(A, 4);

  { See also TStreamReader.GetCodePageFromBOM for an implementation
    that actually uses UTF-x BOM. Here, we only detect BOM to make
    nice error (in case of UTF-16/32) or skip it (in case of UTF-8). }

  if (A[0] = $FF) and (A[1] = $FE) and (A[2] = 0) and (A[3] = 0) then
  begin
    DoError('Detected UTF-32 (little endian) encoding (right now we cannot read such files)', [], 0);
  end else
  if (A[0] = 0) and (A[1] = 0) and (A[2] = $FE) and (A[3] = $FF) then
  begin
    DoError('Detected UTF-32 (big endian) encoding (right now we cannot read such files)', [], 0);
  end else
  if (A[0] = $FF) and (A[1] = $FE) then
  begin
    DoError('Detected UTF-16 (little endian) encoding (right now we cannot read such files, unless compiled with Delphi Unicode)', [], 0);
  end else
  if (A[0] = $FE) and (A[1] = $FF) then
  begin
    DoError('Detected UTF-16 (big endian) encoding (right now we cannot read such files, unless compiled with Delphi Unicode)', [], 0);
  end else
  if (A[0] = $EF) and (A[1] = $BB) and (A[2] = $BF) then
  begin
    DoMessage(6, pmtInformation, 'Detected UTF-8 BOM, skipping.', []);
    InputStream.Position := 3;
  end else
    { No BOM: get back to the beginning of the steam }
    InputStream.Position := 0;
end;
{$ENDIF}

procedure TPasDoc.HandleStream(
  const InputStream: TStream;
  const SourceFileName: string);
var
  p: TParser;
  U: TPasUnit;
  LLoaded: boolean;
  LCacheFileName: string;
begin
  LCacheFileName := CacheDir+ChangeFileExt(ExtractFileName(SourceFileName), '.pduc');
  p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
    {$IFDEF FPC}@{$ENDIF} GenMessage, FVerbosity,
    SourceFileName, ExtractFilePath(SourceFileName), HandleMacros);
  try
    {$IFNDEF STRING_UNICODE}
    SkipBOM(InputStream);
    {$ENDIF}

    p.ShowVisibilities := ShowVisibilities;
    p.ImplicitVisibility := ImplicitVisibility;
    p.CommentMarkers := CommentMarkers;
    p.MarkersOptional := MarkerOptional;
    p.IgnoreLeading := IgnoreLeading;

    LLoaded := false;

    U := nil;

    if (CacheDir <> '') and FileExists(LCacheFileName) then
    begin
      DoMessage(2, pmtInformation, 'Loading data for file %s from cache...', [SourceFileName]);
      try
        U := TPasUnit(TPasUnit.DeserializeFromFile(LCacheFileName));
        {$IFDEF COMPILER_10_UP}
        U.CacheDateTime := CheckGetFileDate(LCacheFileName);
        if U.CacheDateTime < CheckGetFileDate(SourceFileName) then
        {$ELSE}
        U.CacheDateTime := FileDateToDateTime(FileAge(LCacheFileName));
        if U.CacheDateTime < FileDateToDateTime(FileAge(SourceFileName)) then
        {$ENDIF}
        begin
          DoMessage(2, pmtInformation, 'Cache file for %s is outdated.',
            [SourceFileName]);
        end else begin
          LLoaded := True;
        end;
      except
        on E: EInvalidCacheFileVersion do
        begin
          { On EInvalidCacheFileVersion, make nice message and continue
            (with LLoaded = false, just like the cache would not exist). }
          DoMessage(2, pmtInformation, 'Cache file for %s is incompatible (probably from a different PasDoc release).',
            [SourceFileName]);
        end;
      end;
    end;

    if not LLoaded then
    begin
      DoMessage(2, pmtInformation, 'Now parsing file %s...', [SourceFileName]);
      { In case unit was loaded from cache, but rejected for whatever reason,
        free it to avoid memory leaks. }
      FreeAndNil(U);
      p.ParseUnitOrProgram(U);
    end;

    if FUnits.ExistsUnit(U) then begin
      DoMessage(2, pmtWarning,
        'Duplicate unit name "%s" in files "%s" and "%s" (discarded)', [U.Name,
        TPasUnit(FUnits.FindListItem(U.Name)).SourceFileName, SourceFileName]);
      U.Free;
    end else
    begin
      U.SourceFileName := SourceFileName;
    {$IFDEF COMPILER_10_UP}
      U.SourceFileDateTime := CheckGetFileDate(SourceFileName);
    {$ELSE}
      U.SourceFileDateTime := FileDateToDateTime(FileAge(SourceFileName));
    {$ENDIF}
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
  additionalFile: TExternalItem;

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
    {$IFDEF STRING_UNICODE}
      InputStream := TStreamReader.Create(p);
    {$ELSE}
    {$IFDEF USE_BUFFERED_STREAM}
      InputStream := TBufferedStream.Create(p, fmOpenRead or fmShareDenyWrite);
    {$ELSE}
      InputStream := TFileStream.Create(p, fmOpenRead or fmShareDenyWrite);
    {$ENDIF}
    {$ENDIF}
    except
      on E: Exception do
      begin
        DoMessage(1, pmtError, 'Cannot open file "%s". Reason: "%s", skipping',
          [p, E.Message]);
        Continue;
      end;
    end;

    { Note that HandleStream frees InputStream.
      Note that Delphi 7 reports here warning ("Variable "InputStream"
      might not have been initialized") that should be ignored. }
    HandleStream(InputStream, p);
    Inc(Count);
  end;

  FreeAndNil(FIntroduction);
  ParseExternalFile(IntroductionFileName, FIntroduction);
  FreeAndNil(FConclusion);
  ParseExternalFile(ConclusionFileName, FConclusion);
  FreeAndNil(FAdditionalFiles);
  FAdditionalFiles := TExternalItemList.Create(true);
  for i := 0 to FAdditionalFilesNames.Count - 1 do
  begin
    ParseExternalFile(AdditionalFilesNames[i], additionalFile);
    FAdditionalFiles.Add(additionalFile)
  end;

  DoMessage(2, pmtInformation, '... %d Source File(s) parsed', [Count]);
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
      it accidentally excludes items with comments like '@@exclude'
      or '@html(@exclude)'. Checking for exclude should be
      incorporated into doing TTagManager.Execute
      in ExpandDescription. }

    if Assigned(p) and (StrPosIA('@EXCLUDE', p.RawDescription) > 0) then
    begin
      DoMessage(3, pmtInformation, 'Excluding item %s', [p.Name]);
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
  TimeStart: TDateTime;
  CacheDirNoDelim: string;
  UnitsCountBeforeExcluding: Cardinal;
  I: Integer;
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
  for I := 0 to FIncludeDirectories.Count - 1 do
    FIncludeDirectories[I] := IncludeTrailingPathDelimiter(FIncludeDirectories[I]);

  TimeStart := Now;
  ParseFiles;

  UnitsCountBeforeExcluding := FUnits.Count;
  RemoveExcludedItems(TPasItems(FUnits));

  { check if we have any units successfully parsed and not @excluded }
  if ObjectVectorIsNilOrEmpty(FUnits) then
  begin
    if UnitsCountBeforeExcluding <> 0 then
      DoError('%d units were successfully parsed, but they are all ' +
        'marked with @exclude', [UnitsCountBeforeExcluding], 1) else
      DoError('At least one unit must have been successfully parsed ' +
        'to write docs', [], 1);
  end;

  if FProjectName <> '' then begin
    Generator.ProjectName := FProjectName
  end else begin
    Generator.ProjectName := 'docs';
  end;

  Generator.Title := Title;
  Generator.Units := FUnits;
  Generator.Introduction := FIntroduction;
  Generator.Conclusion := FConclusion;
  Generator.AdditionalFiles := FAdditionalFiles;
  Generator.AutoLink := AutoLink;
  Generator.BuildLinks;

  FUnits.SortDeep(SortSettings);

  Generator.LoadDescriptionFiles(FDescriptionFileNames);
  Generator.ExpandDescriptions;

  Generator.WriteDocumentation;

  DoMessage(3, pmtInformation, 'Worked %s minutes(s)',
    [FormatDateTime('nn:ss', (Now - TimeStart))]);
  DoMessage(1, pmtInformation, 'Done', []);
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

    { Just ignore last empty line of AFileNames, this may often occur
      when generating text files with filenames, and is harmless. }
    if (FileMask = '') and (I = AFileNames.Count - 1) then
      Continue;

    SearchResult := SysUtils.FindFirst(FileMask,
      faArchive or faSysFile or faHidden or faReadOnly, SR);
    if SearchResult <> 0 then begin
      DoMessage(1, pmtWarning, 'No regular files found for "%s", skipping', [FileMask]);
    end else begin
      repeat
        s := Path + SR.Name;
        if not FSourceFileNames.ExistsNameCI(s) then
          FSourceFileNames.Add(s);
        SearchResult := FindNext(SR);
      until SearchResult <> 0;
    end;
    SysUtils.FindClose(SR);
  end;
end;

procedure TPasDoc.SetStarOnly(const Value: boolean);
var
  Idx: Integer;
begin
  Idx := FCommentMarkers.IndexOf('**');
  { Compare Value with previous value,
    to not add a 2nd string '**' to FCommentMarkers,
    and only remove valid indexes. }
  if Value <> (Idx <> -1) then
  begin
    if Value then
      FCommentMarkers.Add('**') else
      FCommentMarkers.Delete(Idx);
  end;
end;

function TPasDoc.GetStarOnly: boolean;
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
      raise Exception.CreateFmt('External file extension' +
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

end.
