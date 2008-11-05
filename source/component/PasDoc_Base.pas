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
}
(* The project object and constants.
*)

unit PasDoc_Base;

{$I pasdoc_defines.inc}

interface

uses
  SysUtils,
  Classes,
  PasDoc_Items,
  PasDoc_Languages,
  //PasDoc_Gen,
  PasDoc_Types,
  PasDoc_StringVector,
  PasDoc_SortSettings,
{$IFNDEF FPC}
{$IFDEF WIN32}
{$IFNDEF DELPHI_6_UP}
  FileCtrl,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  PasDoc_TagManager;

const
  { }
  DEFAULT_VERBOSITY_LEVEL = 2;

type
  TPasDoc = class;

  TLinkLook = (llDefault, llFull, llStripped);

  TOptionRec = record
  //@groupbegin(out Results)
    Conclusion, Introduction: TExternalItem;
    MasterFile: string;
  //<@groupend
  //@groupbegin(go Generator Options)
    AutoAbstract: boolean;
    AutoLink: boolean;
    CheckSpelling: boolean;
    { if true, no link to pasdoc homepage will be included at the bottom of
      HTML files;
      default is false }
    GeneratorInfo: Boolean;
    GraphVizClasses: boolean;
    GraphVizUses: boolean;
    LinkLook: TLinkLook;
    WriteUsesClause: boolean;

    Abbreviations: TStringList;
    { the (human) output language of the documentation file(s) }
    AspellLanguage: string;
    AutoLinkExclude: TStringList;

    DescriptionFileNames: TStringVector;
    { destination directory for documentation; must include terminating
      forward slash or backslash so that valid file names can be created
      by concatenating DestinationDirectory and a pathless file name }
    DestDir: string;
    { Format of the document output file(s). }
    DocType: string;

    ConclusionFileName: string;
    IntroductionFileName: string;
    Language: TPasDocLanguages;

    LinkGraphVizUses: string;
    LinkGraphVizClasses: string;

    OnMessage: TPasDocMessageEvent;
    { Name of the project to create. }
    ProjectName: string;
    SourceFileNames: TStringVector;
    SpellCheckIgnoreWords: TStringList;
    { Title of documentation. }
    Title: string;

    Verbosity: Cardinal;
  //<@groupend

  {@groupbegin(html HTML Options)}
    ItemFiles: boolean;
    NumericFilenames: boolean;
    UseTipueSearch: boolean;

    ContentsFile: string;
    CSS: string;
    Footer: string;
    { The content of the CSS file. }
    Header: string;
  //<@groupend

  {@groupbegin(tex TeX Options)}
    Latex2rtf: boolean;
    LatexHead: TStrings;
  //<@groupend

  {@groupbegin(po Parser Options)}
    CommentMarkers: TStringList;
    Directives: TStringVector;
    HandleMacros: boolean;
    IgnoreLeading: string;
    IncludeDirectories: TStringVector;
    MarkerOptional, SingleCharMarkers: boolean;
  //<@groupend
  {@groupbegin(io Item Options)}
    CacheDir: string;
    ImplicitVisibility: TImplicitVisibility;
    ShowVisibilities: TVisibilities;
    SortSettings: TSortSettings;
  //<@groupend
  end;

(* Base class for both TPasDoc and all generators.
  It is used as a container for the options.
*)
  TPasDocBase = class(TComponent)
  protected
    FDoc: TPasDoc;  //<nil if not owned
    FUnits: TPasUnits;
    Options: TOptionRec;
  //@groupbegin(setopts Option Setters)
    function  GetLanguage: TLanguageID;
    procedure SetLanguage(const Value: TLanguageID);
    procedure SetAbbreviations(Value: TStringList);
    procedure SetCommentMarkers(const Value: TStringList);
    procedure SetDescriptionFileNames(const ADescriptionFileNames: TStringVector);
    procedure SetDirectives(const ADirectives: TStringVector);
    procedure SetIncludeDirectories(const AIncludeDirectores: TStringVector);
    procedure SetLatexHead(const Value: TStrings);
    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetSpellCheckIgnoreWords(Value: TStringList);
    procedure SetStarStyle(const Value: boolean);
    function  GetStarStyle: boolean;
  //<@groupend

    { Raises an exception. }
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);
    { Forwards a message to the @link(OnMessage) event. }
    procedure DoMessage(const AVerbosity: Cardinal; const AMessageType:
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);
    { for Generator messages }
    procedure GenMessage(const MessageType: TPasDocMessageType; const
      AMessage: string; const AVerbosity: Cardinal);

    { Searches all items in all units (given by field @link(Units)) for item
      with NameParts.
      Returns a pointer to the item on success, nil otherwise. }
    function  FindGlobal(const NameParts: TNameParts): TBaseItem;

    { This function is supposed to return a reference to an item, that is the
      name combined with some linking information like a hyperlink element in
      HTML or a page number in Tex.
      The XML generator also must provide appropriate names!
    }
    function CreateLink(const Item: TBaseItem): string; virtual;

  //- created read-only properties
    { All TPasUnit objects which have been created from the list of file names
      during the parsing. }
    property Units: TPasUnits read FUnits;
    // After @link(Execute) has been called, @name holds the conclusion.
    property Conclusion: TExternalItem read Options.Conclusion;
    // After @link(Execute) has been called, @name holds the introduction.
    property Introduction: TExternalItem read Options.Introduction;
    { the (human) output language of the documentation file(s) }
    property Language: TPasDocLanguages read Options.Language;
  public
    { Creates object and sets fields to default values. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Starts creating the documentation.
      Must be implemented for TPasDoc and TPasGen. }
    procedure Execute(fGenerate: boolean = True); virtual; abstract;

  //------------- command line helpers -----------

    { Adds source filenames from a stringlist }
    procedure AddSourceFileNames(const AFileNames: TStrings);
    { Loads names of Pascal unit source code files from a text file.
      Adds all file names to @link(SourceFileNames).
      If DashMeansStdin and AFileName = '-' then it will load filenames
      from stdin. }
    procedure AddSourceFileNamesFromFile(const FileName: string;
      DashMeansStdin: boolean);
    procedure ParseAbbreviationsFile(const AFileName: string);

  { Item names for which no auto links are created. Loaded from file, if ever used.}
    property AutoLinkExclude: TStringList read Options.AutoLinkExclude;
  //File usable for "open" command, to display the created documentation.
    property MasterFile: string read Options.MasterFile;

  published
    property Abbreviations: TStringList read Options.Abbreviations write SetAbbreviations;
    property AspellLanguage: string read Options.AspellLanguage write Options.AspellLanguage;
    { The meaning of this is just like @--auto-abstract command-line option.
      It is used in @link(ExpandDescriptions). }
    property AutoAbstract: boolean read Options.AutoAbstract write Options.AutoAbstract;
    { This controls auto-linking, see
      [http://pasdoc.sipsolutions.net/AutoLinkOption] }
    property AutoLink: boolean
      read Options.AutoLink write Options.AutoLink default false;
    property CacheDir: string read Options.CacheDir write Options.CacheDir;
    property CheckSpelling: boolean read Options.CheckSpelling write Options.CheckSpelling
      default false;
    property CommentMarkers: TStringList read Options.CommentMarkers write SetCommentMarkers;

    property ConclusionFileName: string read Options.ConclusionFileName
      write Options.ConclusionFileName;

    property DescriptionFileNames: TStringVector
      read Options.DescriptionFileNames write SetDescriptionFileNames;
    { destination directory for documentation; must include terminating
      forward slash or backslash so that valid file names can be created
      by concatenating DestinationDirectory and a pathless file name }
    property DestinationDirectory: string read Options.DestDir write Options.DestDir;
    property Directives: TStringVector read Options.Directives write SetDirectives;
    { Format of the document output file(s). }
    property DocType: string read Options.DocType write Options.DocType;

    { "generator info" are
      things that can change with each invocation of pasdoc,
      with different pasdoc binary etc.

      This includes
      @unorderedList(
        @item(time of generating docs)
        @item(compiler name and version used to compile pasdoc,
          time of compilation and such)
        @item(pasdoc's version)
      )
      See [http://pasdoc.sipsolutions.net/ExcludeGeneratorOption].
      Default value is true (i.e. show them),
      as this information is generally considered useful.

      Setting this to false is useful for automatically comparing two
      versions of pasdoc's output (e.g. when trying to automate pasdoc's
      tests). }
    property GeneratorInfo: Boolean read Options.GeneratorInfo write Options.GeneratorInfo;

    //whether the tokenizer should handle macros
    property HandleMacros: boolean
      read Options.HandleMacros write Options.HandleMacros default true;
    { See command-line option @--implicit-visibility documentation at
      [http://pasdoc.sipsolutions.net/ImplicitVisibilityOption].
      This will be passed to parser instance. }
    property ImplicitVisibility: TImplicitVisibility
      read Options.ImplicitVisibility write Options.ImplicitVisibility default ivPublic;
    property IgnoreLeading: string read Options.IgnoreLeading write Options.IgnoreLeading;
    property IncludeDirectories: TStringVector read Options.IncludeDirectories write
      SetIncludeDirectories;
    property IntroductionFileName: string read Options.IntroductionFileName
      write Options.IntroductionFileName;
    property LanguageID: TLanguageID read GetLanguage write SetLanguage
      default DEFAULT_LANGUAGE;
    { link the GraphViz uses diagram }
    property LinkGraphVizUses: string read Options.LinkGraphVizUses
      write Options.LinkGraphVizUses;
    { link the GraphViz classes diagram }
    property LinkGraphVizClasses: string read Options.LinkGraphVizClasses
      write Options.LinkGraphVizClasses;
    { This controls @link(SearchLink) behavior, as described in
      [http://pasdoc.sipsolutions.net/LinkLookOption]. }
    property LinkLook: TLinkLook read Options.LinkLook write Options.LinkLook;
    property MarkerOptional: boolean read Options.MarkerOptional
      write Options.MarkerOptional default false;

    property OnMessage: TPasDocMessageEvent read Options.OnMessage write Options.OnMessage;

    { generate a GraphViz diagram for the units dependencies }
    property OutputGraphVizUses: boolean read Options.GraphVizUses write Options.GraphVizUses default false;
    { generate a GraphViz diagram for the Class hierarchy }
    property OutputGraphVizClassHierarchy: boolean
      read Options.GraphVizClasses write Options.GraphVizClasses default false;
    { The name PasDoc shall give to this documentation project,
      also used to name some of the output files. }
    property ProjectName: string read Options.ProjectName write Options.ProjectName;
    property ShowVisibilities: TVisibilities read Options.ShowVisibilities write Options.ShowVisibilities;
    property SingleCharMarkers: boolean read Options.SingleCharMarkers write Options.SingleCharMarkers;

    { This determines how items inside will be sorted.
      See [http://pasdoc.sipsolutions.net/SortOption]. }
    property SortSettings: TSortSettings
      read Options.SortSettings write Options.SortSettings default [];
    property SourceFileNames: TStringVector read Options.SourceFileNames write
      SetSourceFileNames;
    property SpellCheckIgnoreWords: TStringList
      read Options.SpellCheckIgnoreWords write SetSpellCheckIgnoreWords;
    property StarStyleOnly: boolean read GetStarStyle write SetStarStyle;
    { Title of documentation. }
    property Title: string read Options.Title write Options.Title;
    property Verbosity: Cardinal read Options.Verbosity write Options.Verbosity
      default DEFAULT_VERBOSITY_LEVEL;
    property WriteUsesClause: boolean
      read Options.WriteUsesClause write Options.WriteUsesClause default false;
  {@groupbegin(html HTML Options)}
    property ItemFiles: boolean read Options.ItemFiles;
    property NumericFilenames: boolean read Options.NumericFilenames write Options.NumericFilenames;
    property UseTipueSearch: boolean read Options.UseTipueSearch write Options.UseTipueSearch;

    property ContentsFile: string read Options.ContentsFile write Options.ContentsFile;
    { The content of the CSS file. }
    property CSS: string read Options.CSS write Options.CSS;
    property Footer: string read Options.Footer write Options.Footer;
    property Header: string read Options.Header write Options.Header;
  //<@groupend

  {@groupbegin(tex TeX Options)}
    property Latex2rtf: boolean read Options.Latex2rtf;
    property LatexHead: TStrings read Options.LatexHead write Options.LatexHead;
  //<@groupend
  end;

(* Base class for generators. Declares the methods used by TPasDoc.
*)
  TPasDocGen = class(TPasDocBase)
  protected
    { Expands description for each item in each unit of @link(Units).
      "Expands description" means that TTagManager.Execute is called,
      and item's RawDescription is transformed into DetailedDescription,
      interpreting @@-tags as appropriate for the selected document type. }
    procedure ExpandDescriptions; virtual; abstract;
    { Abstract function that provides file extension for documentation format.
      Must be overwritten by descendants. }
    function GetFileExtension: string; virtual; abstract;
    { Creates a @link(TPasUnit) object from the stream and adds it to
      @link(FUnits). }
    procedure HandleStream(const InputStream: TStream; const SourceFileName: string);
    { Calls @link(HandleStream) for each file name in @link(SourceFileNames). }
    procedure ParseFiles;
    //Skip UTF-8 BOM, if present. @return(True if UTF-8 BOM was skipped.)
    function  SkipBOM(InputStream: TStream): boolean;

    { Write all documentation.

      This implementation only creates the GraphViz files, using WriteDocumentationGen.
      Must be overwritten for single or multiple output files, depending on the document type.
    }
    procedure WriteDocumentation; virtual; abstract;
  public
    procedure Execute(fGenerate: boolean = True); override;
  end;

  TGeneratorClass = class of TPasDocGen;

  { The main object in the pasdoc application, holds all parameters.
    Parses files, then invokes the documentation generator,
    which creates one or more documentation output files. }
  TPasDoc = class(TPasDocBase)
  private
    FGenerator: TPasDocGen; //- TDocGenerator;
  public
    { Starts creating the documentation. Creates and invokes the appropriate generator. }
    procedure Execute(fGenerate: boolean = True); override;
    { Create the appropriate generator and let it produce the documentation.
      @return(False if type is unhandled.) }
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
  PASDOC_DATE = '2008-10-30';
  { }
  PASDOC_VERSION = '2.0.0';
  { }
  PASDOC_NAME_AND_VERSION = PASDOC_NAME + ' ' + PASDOC_VERSION;
  { }
  PASDOC_HOMEPAGE = 'http://pasdoc.sourceforge.net/';

{ Returns pasdoc name, version, used compiler version, etc.

  This is a function only because we can't nicely declare it as a constant.
  But this behaves like a constant, i.e. every time you call it
  it returns the same thing (as long as this is the same binary). }
function PASDOC_FULL_INFO: string;


procedure RegisterGenerator(const AName: string; AClass: TGeneratorClass);

var
  Generators: TStringVector;

implementation

uses
  PasDoc_Parser,
  PasDoc_ObjectVector,
  PasDoc_StreamUtils,
  PasDoc_Utils,
  PasDoc_Serialize;

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

procedure RegisterGenerator(const AName: string; AClass: TGeneratorClass);
var
  i: integer;
begin
  i := Generators.AddNotExisting(LowerCase(AName));
  if i >= 0 then
    Generators.Objects[i] := TObject(AClass);
end;

{ TPasDocBase }

constructor TPasDocBase.Create(AOwner: TComponent);
var
  doc: TPasDoc absolute AOwner;
begin
  inherited;
  if AOwner is TPasDoc then begin
  //Created by TPasDoc
    FDoc := doc; //signal "owned" in destructor
    Options := doc.Options; //copy options, don't own them
  //get specific options
    FUnits := TPasUnits.Create(False);
    FUnits.Assign(doc.Units);
  end else begin
  //stand alone version, of TPasDoc or an independent generator
    FUnits := TPasUnits.Create(True);

  //Set default property values, init Options objects
    Options.Abbreviations := TStringList.Create;
      Options.Abbreviations.Duplicates := dupIgnore;
    Options.AutoLinkExclude := TStringList.Create;
      Options.AutoLinkExclude.CaseSensitive := false;
    Options.CommentMarkers := TStringList.Create;
    //Options.CSS := DefaultPasdocCss;
    Options.DescriptionFileNames := TStringVector.Create;
    Options.Directives := TStringVector.Create;
    Options.GeneratorInfo := true;
    Options.HandleMacros := true;
    Options.ImplicitVisibility := ivPublic;
    Options.IncludeDirectories := TStringVector.Create;
    Options.Language := TPasDocLanguages.Create;
    Options.SourceFileNames := TStringVector.Create;
    Options.SpellCheckIgnoreWords := TStringList.Create;
    Options.Verbosity := DEFAULT_VERBOSITY_LEVEL;
  end;
end;

destructor TPasDocBase.Destroy;
begin
  if FDoc = nil then begin //not owned, destroy option objects
    FreeAndNil(Options.Abbreviations);
    FreeAndNil(Options.AutoLinkExclude);
    FreeAndNil(Options.CommentMarkers);
    FreeAndNil(Options.DescriptionFileNames);
    FreeAndNil(Options.Directives);
    FreeAndNil(Options.IncludeDirectories);
    FreeAndNil(Options.Language);
    FreeAndNil(Options.SourceFileNames);
    FreeAndNil(Options.SpellCheckIgnoreWords);
    FUnits.Free;
  //generator objects
    FreeAndNil(Options.Conclusion);
    FreeAndNil(Options.Introduction);
  end;
  inherited;
end;

procedure TPasDocBase.DoError(const AMessage: string; const AArguments: array of
  const; const AExitCode: Word);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

procedure TPasDocBase.DoMessage(const AVerbosity: Cardinal; const AMessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity <= Verbosity) and Assigned(OnMessage) then
    OnMessage(AMessageType, Format(AMessage, AArguments), AVerbosity);
end;

procedure TPasDocBase.GenMessage(const MessageType: TPasDocMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  DoMessage(AVerbosity, MessageType, AMessage, []);
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocBase.AddSourceFileNames(const AFileNames: TStrings);
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
          if not SourceFileNames.ExistsNameCI(s) then
            SourceFileNames.Add(s)
        end;
        SearchResult := FindNext(SR);
      until SearchResult <> 0;
    end;
    SysUtils.FindClose(SR);
  end;
end;

procedure TPasDocBase.AddSourceFileNamesFromFile(const FileName: string;
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

function TPasDocBase.FindGlobal(const NameParts: TNameParts): TBaseItem;
var
  i: Integer;
  Item: TBaseItem;
  U: TPasUnit;
begin
  Result := nil;

  if IsEmpty(Units) then Exit;

  case Length(NameParts) of
    1: begin
        if (Introduction <> nil) then begin
          if  SameText(Introduction.Name, NameParts[0]) then begin
            Result := Introduction;
            Exit;
          end;
          Result := Introduction.FindItem(NameParts[0]);
          if Result <> nil then Exit;
        end;

        if (Conclusion <> nil) then begin
          if  SameText(Conclusion.Name, NameParts[0]) then begin
            Result := Conclusion;
            Exit;
          end;
          Result := Conclusion.FindItem(NameParts[0]);
          if Result <> nil then Exit;
        end;

        for i := 0 to Units.Count - 1 do begin
           U := Units.UnitAt[i];

           if SameText(U.Name, NameParts[0]) then begin
             Result := U;
             Exit;
           end;

           Result := U.FindItem(NameParts[0]);
           if Result <> nil then Exit;
         end;
       end;
    2: begin
         { object.field_method_property }
         for i := 0 to Units.Count - 1 do begin
           Result := Units.UnitAt[i].FindFieldMethodProperty(NameParts[0], NameParts[1]);
           if Assigned(Result) then Exit;
         end;

         { unit.cio_var_const_type }
         U := TPasUnit(Units.FindName(NameParts[0]));
         if Assigned(U) then
           Result := U.FindItem(NameParts[1]);
       end;
    3: begin
         { unit.objectorclassorinterface.fieldormethodorproperty }
         U := TPasUnit(Units.FindName(NameParts[0]));
         if (not Assigned(U)) then Exit;
         Item := U.FindItem(NameParts[1]);
         if (not Assigned(Item)) then Exit;
         Result := Item.FindItem(NameParts[2]);
       end;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocBase.ParseAbbreviationsFile(const AFileName: string);
var
  L: TStringList;
  i, p: Integer;
  s, lname, value: string;
begin
  if FileExists(AFileName) then begin
    L := TStringList.Create;
    try
      L.LoadFromFile(AFileName);
      for i := 0 to L.Count-1 do begin
        s := Trim(L[i]);
        if length(s)>0 then begin
          if s[1] = '[' then begin
            p := pos(']', s);
            if p>=0 then begin
              lname := Trim(copy(s, 2, p-2));
              value := Trim(copy(s,p+1,MaxInt));
              Abbreviations.Values[lname] := value;
            end;
          end;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TPasDocBase.GetLanguage: TLanguageID;
begin
  Result := Language.Language;
end;

function TPasDocBase.GetStarStyle: boolean;
begin
  Result := CommentMarkers.IndexOf('**') <> -1;
end;

procedure TPasDocBase.SetAbbreviations(Value: TStringList);
begin
  Abbreviations.Assign(Value);
end;

procedure TPasDocBase.SetCommentMarkers(const Value: TStringList);
begin
  CommentMarkers.Assign(Value);
end;

procedure TPasDocBase.SetDescriptionFileNames(
  const ADescriptionFileNames: TStringVector);
begin
  DescriptionFileNames.Assign(ADescriptionFileNames);
end;

procedure TPasDocBase.SetDirectives(const ADirectives: TStringVector);
begin
  Directives.Assign(ADirectives);
end;

procedure TPasDocBase.SetIncludeDirectories(
  const AIncludeDirectores: TStringVector);
begin
  IncludeDirectories.Assign(AIncludeDirectores);
end;

procedure TPasDocBase.SetLanguage(const Value: TLanguageID);
begin
  Language.Language := Value;
end;

procedure TPasDocBase.SetLatexHead(const Value: TStrings);
begin
  LatexHead.Assign(Value);
end;

procedure TPasDocBase.SetSourceFileNames(
  const ASourceFileNames: TStringVector);
begin
  SourceFileNames.Clear;
  AddSourceFileNames(ASourceFileNames);
end;

procedure TPasDocBase.SetSpellCheckIgnoreWords(Value: TStringList);
begin
  SpellCheckIgnoreWords.Assign(Value);
end;

procedure TPasDocBase.SetStarStyle(const Value: boolean);
var
  Idx: Integer;
begin
  if Value then begin
    CommentMarkers.Add('**');
  end else begin
    Idx := CommentMarkers.IndexOf('**');
    if Idx <> -1 then
      CommentMarkers.Delete(Idx);
  end;
end;

function TPasDocBase.CreateLink(const Item: TBaseItem): string;
begin
  Result := Item.Name;
end;

{ TPasDoc }

procedure TPasDoc.Execute(fGenerate: boolean);
var
  i: integer;
  cls: TGeneratorClass;

  procedure Generate;
  begin
    FGenerator.Execute(fGenerate);
  //copy results
    Options := FGenerator.Options;
  end;

begin
(* This is the PasDoc version.
  Create and invoke the appropriate generator.
*)
  if assigned(FGenerator) then
    Generate
  else begin
  //create, invoke and destroy generator
    i := Generators.IndexOf(LowerCase(DocType));
    if i < 0 then
      DoError('Unknown generator: %s', [DocType], 3);
    TObject(cls) := Generators.Objects[i];
    FGenerator := cls.Create(self);
    try
      Generate;
    finally
      FreeAndNil(FGenerator);
    end;
  end;
end;

{ TPasDocGen }

procedure TPasDocGen.Execute(fGenerate: boolean);
var
  t1, t2: TDateTime;
  CacheDirNoDelim: string;

  procedure AssignLinks(MyUnit: TPasUnit; MyObject: TPasCio; c: TPasItems);
  var
    i: Integer;
    p: TPasItem;
  begin
    if (not Assigned(c)) or (c.Count < 1) then Exit;
    for i := 0 to c.Count - 1 do begin
      p := c.PasItemAt[i];
      p.FullLink := CreateLink(p);
    end;
  end;

  procedure BuildLinks;
  (* Most of this is already done by parser.
    Only after deserialization some actions may be required.
  *)
  var
    i: Integer;
    U: TPasUnit;
  begin //BuildLinks
  (* The created files are specific to the generator.
    Assigning file names here is somewhat HTML specific.
    Filenames should be assigned by the CreateLink method of the actual generator.
  *)
    DoMessage(2, pmtInformation, 'Creating links ...', []);

  //assuming that FullLink equals the filename???
    if Introduction <> nil then begin
      Introduction.FullLink := CreateLink(Introduction);
      //Introduction.OutputFileName := Introduction.FullLink;
    end;

    if Conclusion <> nil then begin
      Conclusion.FullLink := CreateLink(Conclusion);
      //Conclusion.OutputFileName := Conclusion.FullLink;
    end;

    for i := Units.Count - 1 downto 0 do begin
      U := Units.UnitAt[i];
    //@exclude has not yet executed - should PreExpand execute earlier?
      u.BuildLinks(Units, {$IFDEF fpc}@{$ENDIF}self.CreateLink);
    end;
    DoMessage(2, pmtInformation, '... ' + ' links created', []);
  end;

  procedure BuildUnitSections;
  var
    i: Integer;
    U: TPasUnit;
  begin
  //remove @exclude'd units from our list.
    for i := Units.Count - 1 downto 0 do begin
      U := Units.UnitAt[i];
      if u.ToBeExcluded then
        Units.Delete(i)
      else
        U.BuildSections;
    end;
  end;

(* Add linked description.
  Create an description item (TToken), and add it to the item's RawDescriptions.
*)
  procedure StoreDescription(ItemName: string; const t, f: string; start: TTextStreamPos);
  var
    Item: TBaseItem;
    NameParts: TNameParts;
  begin
    if t = '' then Exit;

    DoMessage(5, pmtInformation, 'Storing description for ' + ItemName, []);
    if SplitNameParts(ItemName, NameParts) then begin
      //Item := Generator.FindGlobal(NameParts);
      Item := FindGlobal(NameParts);
      if Assigned(Item) then begin
        item.AddRawDescription(t, f, start);
      end else
        DoMessage(2, pmtWarning, 'Could not find item ' + ItemName, []);
    end else
      DoMessage(2, pmtWarning, 'Could not split item "' + ItemName + '"', []);
  end;

  procedure LoadDescriptionFile(n: string);
  var
    f           : TStream;
    i, IdentStart: Integer;
    s           : string;
    ItemName    : string;
    Description : string;
    LineStart, DescStart: TTextStreamPos;
  const
    IdentChars  = ['A'..'Z', 'a'..'z', '_', '.', '0'..'9'];

    procedure Store;  //Description
    begin
      StoreDescription(ItemName, Description, n, DescStart);
      Description := '';
    end;

  begin
    if n = '' then
      Exit;
    ItemName := '';
    Description := '';
    try
      f := TFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      try
        while f.Position < f.Size do begin
          LineStart := f.Position;
          s := StreamReadLine(f);
          if s[1] = '#' then begin
          //found new entry?
            i := 2;
            while s[i] in WhiteSpaceNotNL do Inc(i);
          { Make sure we read a valid name - the user might have used # in his
              description. }
            if s[i] in IdentChars then begin
              if ItemName <> '' then begin
              //save preceding description
                Store;
              end;
            { Read item name and beginning of the description }
              IdentStart := i;
              while s[i] in identChars do
                inc(i);
              ItemName := Copy(s, IdentStart, i - IdentStart);
              while s[i] in WhiteSpaceNotNL do Inc(i);
              DescStart := LineStart + i - 1; //begin of text
              Description := Copy(s, i, MaxInt);
              Continue; //bypass append s
            end;
          end;
          Description := Description + s;
        end;

        if ItemName = '' then
          DoMessage(2, pmtWarning, 'No descriptions read from "%s" -- invalid or empty file', [n])
        else
          Store;
      finally
        f.Free;
      end;
    except
    {$IFDEF old}
      DoError('Could not open description file "%s".', [n], 0);
    {$ELSE}
      DoMessage(1, pmtWarning, 'Could not open description file "%s".', [n]);
    {$ENDIF}
    end;
  end;

  procedure LoadDescriptionFiles(const c: TStringVector);
  var
    i: Integer;
  begin
    if (c <> nil) and (c.Count > 0) then begin
      DoMessage(3, pmtInformation, 'Loading description files ...', []);
      for i := 0 to c.Count - 1 do
        LoadDescriptionFile(c[i]);
    end;
  end;

begin //Execute
(* This is the generator version.
*)
  { Do a couple of tests before we actually start processing the source files. }
  if SourceFileNames.IsEmpty then begin
    DoError('No Source Files have been specified.', [], 1);
  end;
  { Sorted makes searching AutoLinkExclude.IndexOf (used heavily when
    auto-linking to respect this option) obviously much faster.
    The speed improvement can be literally felt when you specified
    large file like /usr/share/dict/american-english for this option. }
  AutoLinkExclude.Sorted := true; //or use hash list?

  if (CacheDir <> '') then begin
    {$ifdef WIN32}
    { This is needed to make DirectoryExists and CreateDir work
      when user used UNIX-like delimiters "/" inside CacheDir
      (yes, it's normally allowed under Windows, so pasdoc should work with
      it too) }
    CacheDir := SCharsReplace(CacheDir, ['/'], PathDelim);
    {$else}
    // POSIX is not forgiving wrong path delimiters
    CacheDir := SCharsReplace(CacheDir, ['\'], PathDelim);
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
  IncludeDirectories.Iterate( {$IFDEF FPC}@{$ENDIF} IncludeTrailingPathDelimiter);

  t1 := Now;
  ParseFiles;

  if IsEmpty(FUnits) then
    DoError('At least one unit must have been successfully parsed ' +
        'to write docs', [], 1);

  if ProjectName = '' then
    ProjectName := 'docs';

  BuildLinks; //may become invalid by destruction of excluded units!

  //FUnits.SortDeep(SortSettings);

(* Read external descriptions, found while parsing the units.
  Required for the editor, even if no docs are created.
  Should reside in TPasDoc, but stream handling is implemented in TDocGenerator.
*)
  LoadDescriptionFiles(DescriptionFileNames);

  if fGenerate then begin
    ExpandDescriptions; //here items are marked for removal
      //and items are grouped - must be done before sorting!
    BuildUnitSections; //based on it's private unit list
    FUnits.SortDeep(SortSettings);
    WriteDocumentation;
  end else
    FUnits.BuildSections; //optional, for editor, debugger...

  if Options.GeneratorInfo then begin
    t2 := Now;
    DoMessage(1, pmtInformation, 'Done, worked %s minutes(s)',
      [FormatDateTime('nn:ss', (t2 - t1))]);
  end else begin
    DoMessage(1, pmtInformation, 'Done', [])
  end;
end;

//procedure TPasDoc.SkipBOM(InputStream: TStream);
function TPasDocGen.SkipBOM(InputStream: TStream): boolean;
const
  UTF8BOM: string[3] = #$EF#$BB#$BF; //UTF-8
var
  BOM: string[3];
begin
  if InputStream.Read(BOM[1], 3) <> 3 then
    DoError('Could not read BOM', [], 0);
  Result := BOM = UTF8BOM;
  if not Result then //no BOM, rewind stream
    InputStream.Position := 0;
end;

procedure TPasDocGen.HandleStream(
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
  p := TParser.Create(InputStream, SourceFileName, ExtractFilePath(SourceFileName),
    Options);
  try
  { TODO : init item options only once }
    PasDoc_items.ShowVisibilities := ShowVisibilities; //must be known to all CIOs
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

procedure TPasDocGen.ParseFiles;
var
  Count, i: Integer;
  p: string;
  InputStream: TStream;


  procedure HandleExternalFile(const FileName: string;
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
      if SameText(ExtractFileExt(FileName), GetFileExtension) then
        raise Exception.CreateFmt('Introduction/conclusion file extension' +
          ' is the same as file extension of generated documentation ("%s"), ' +
          'refusing to generate documentation', [GetFileExtension]);

      ExternalItem.Name := SCharsReplace(
        ChangeFileExt( ExtractFileName(FileName) , ''), [' '], '_');

      ExternalItem.RawDescription := FileToString(FileName);
    except
      FreeAndNil(ExternalItem);
      raise;
    end;
  end;

  procedure ParseExternalFile(const FileName: string;
    var ExternalItem: TExternalItem);
  begin
    FreeAndNil(ExternalItem);
    if FileName <> '' then begin
      HandleExternalFile(FileName, ExternalItem);
      Inc(Count);
    end;
  end;

begin
  FUnits.clear;

  DoMessage(1, pmtInformation, 'Starting Source File Parsing ...', []);
  if SourceFileNames.IsEmpty then Exit;

  InputStream := nil;
  Count := 0;
  for i := 0 to SourceFileNames.Count - 1 do begin
    p := SourceFileNames[i];
    try
      InputStream := TFileStream.Create(p, fmOpenRead or fmShareDenyWrite);
    except
      on E: Exception do begin
        DoMessage(1, pmtError, 'Cannot open file "%s", skipping', [p]);
        Continue;
      end;
    end;

    { Note that HandleStream frees InputStream. }
    HandleStream(InputStream, p);
    Inc(Count);
  //try auto-include *.txt
    p := ChangeFileExt(p, '.txt');
    if FileExists(p) then
      Self.DescriptionFileNames.AddNotExisting(p);
  end;

  ParseExternalFile(IntroductionFileName, Options.Introduction);
  ParseExternalFile(ConclusionFileName, Options.Conclusion);

  DoMessage(2, pmtInformation, '... %d Source File(s) parsed', [Count]);
end;

initialization
  Generators := TStringVector.Create;
finalization
  FreeAndNil(Generators);
end.
