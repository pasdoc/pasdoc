{ @abstract(basic doc generator object)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Ivan Montes Velencoso (senbei@teleline.es))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Philippe Jean Dit Bailleul (jdb@abacom.com))
  @author(Rodrigo Urubatan Ferreira Jardim (rodrigo@netscape.net))
  @created(30 Aug 1998)
  @lastmod(2003-03-29)

  GenDoc contains the basic documentation generator object @link(TDocGenerator).
  It is not sufficient by itself but the basis for all generators that produce
  documentation in a specific format like HTML or LaTex.
  They override @link(TDocGenerator)'s virtual methods. }

unit PasDoc_Gen;

interface

uses
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  PasDoc_HierarchyTree,
  PasDoc_Types,
  Classes;

const
  { set of characters, including all letters and the underscore }
  IdentifierStart = ['A'..'Z', 'a'..'z', '_'];

  { set of characters, including all characters from @link(IdentifierStart)
    plus the ten decimal digits }
  IdentifierOther = ['A'..'Z', 'a'..'z', '_', '0'..'9', '.'];

  { number of overview files that pasdoc generates for
    multiple-document-formats like @link(HTML) }
  NUM_OVERVIEW_FILES = 8;

  { names of all overview files, extensions not included }
  OverviewFilenames: array[0..NUM_OVERVIEW_FILES - 1] of string =
  ( 'AllUnits',
    'ClassHierarchy',
    'AllClasses',
    'AllTypes',
    'AllVariables',
    'AllConstants',
    'AllFunctions',
    'AllIdentifiers');

type
  { @abstract(basic documentation generator object)
    @author(Marco Schmidt (marcoschmidt@geocities.com))
    This abstract object will do the complete process of writing
    documentation files.
    It will be given the collection of units that was the result of the
    parsing process and a configuration object that was created from default
    values and program parameters.
    Depending on the output format, one or more files may be created (HTML
    will create several, Tex only one). }
  TDocGenerator = class(TComponent)
  protected
    { the (human) output language of the documentation file(s);
      one of the LANG_xxx constants, e.g. @link(LANG_ENGLISH);
      default language is @link(DEFAULT_LANGUAGE) }
    FLanguage: TPasDocLanguages;
    { Name of the project to create. }
    FProjectName: string;
    { if true, no link to pasdoc homepage will be included at the bottom of
      HTML files;
      default is false }
    FNoGeneratorInfo: Boolean;
    { the output stream that is currently written to; depending on the
      output format, more than one output stream will be necessary to
      store all documentation }
    FCurrentStream: TStream;
    { Title of documentation. }
    FTitle: string;
    { destination directory for documentation; must include terminating
      forward slash or backslash so that valid file names can be created
      by concatenating DestinationDirectory and a pathless file name }
    FDestDir: string;

    FOnMessage: TPasDocMessageEvent;

    FClassHierarchy: TStringCardinalTree;

    function GetLanguage: TLanguageID;
    procedure SetLanguage(const Value: TLanguageID);
    procedure SetDestDir(const Value: string);

    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Integer = 0);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    property CurrentStream: TStream read FCurrentStream;

    procedure CreateClassHierarchy;

    { Calls @link(WriteString) with S, then writes a line feed. }
    procedure WriteLine(const s: string);
    
  protected
    { list of all units that were successfully parsed }
    FUnits: TPasUnits;

    { Checks if D is assigned and empty - if so, disposes of D and sets it to
      nil.
      If there are characters in D, it is checked whether at least one
      non-whitespace character is present - if all characters are whitespace,
      disposes of D. }
    { If field @link(Stream) is assigned, it is disposed and set to nil. }
    procedure CloseStream;

    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; virtual;

    { Calls @link(ConvertChar) for each character in S, thus assembling a
      String that is returned and can be written to the documentation file. }
    function ConvertString(const s: string): string; virtual;

    { This function is supposed to return a reference to an item, that is the
      name combined with some linking information like a hyperlink element in
      HTML or a page number in Tex. }
    function CreateLink(const Item: TPasItem): string; virtual;

    { If field @link(Stream) still exists (@<@> nil), it is closed.
      Then, a new output stream in the destination directory with given
      name and file extension typical for this document format is created and
      assigned to Stream.
      No path or extension should therefore be in Name.
      Typical values for Name would be 'Objects' or 'AllUnits'.
      Returns true if creation was successful, false otherwise. }
    function CreateStream(const Name: string): Boolean;

    { Must be overwritten.
      From an item name and its link, this creates a language-specific
      reference to that item. }
    function CreateReferencedLink(ItemName, Link: string): string; virtual; abstract;

    { Takes description D of the item Item, expands links (using Item),
      converts output-specific characters.
      Returns true on success, false otherwise (not enough memory?). }
    function ExpandDescription(Item: TPasItem; var d: string): Boolean;

    { Searches for an email address in String S. Searches for first appearance
      of the @@ character}
    function ExtractEmailAddress(s: string; out S1, S2, EmailAddress: string): Boolean;
    
    { Searches all items in all units (given by field @link(Units)) for item
      S1.S2.S3 (first N  strings not empty).
      Returns a pointer to the item on success, nil otherwise. }
    function FindGlobal(const S1, S2, S3: string; const n: Integer): TPasItem;

    function GetCIOTypeName(MyType: TCIOType): string;

    { Abstract function that provides file extension for documentation format.
      Must be overwritten by descendants. }
    function GetFileExtension: string; virtual; abstract;

    { Loads descriptions from file N and replaces or fills the corresponding
      comment sections of items. }
    procedure LoadDescriptionFile(n: string);
    
    function SearchItem(s: string; const Item: TPasItem): TPasItem;

    { Searches for an item of name S which was linked in the description
      of Item. Starts search within item, then does a search on all items in all
      units using @link(FindGlobal).
      Returns a link as String on success or an empty String on failure. }
    function SearchLink(s: string; const Item: TPasItem): string;

    { A link provided in a tag can be made up of up to three parts,
      separated by dots.
      If this link is not a valid identifier or if it has more than
      three parts, false is returned, true otherwise.
      The parts are returned in S1, S2 and S3, with the number of
      parts minus one being returned in N. }
    function SplitLink(s: string; var S1, S2, S3: string; var n: Integer): Boolean;

    procedure StoreDescription(ItemName: string; var t: string);

    { Writes all information on a class, object or interface (CIO) to output,
      at heading level HL. }
    procedure WriteCIO(HL: Byte; const CIO: TPasCio); virtual; abstract;

    { Writes all classes, interfaces and objects in C to output, calling
      @link(WriteCIO) with each, at heading level HL. }
    procedure WriteCIOs(HL: Byte; c: TPasItems); virtual;

    { Abstract procedure, must be overwritten by descendants.
      Writes a list of all classes, interfaces and objects in C at heading
      level HL to output. }
    procedure WriteCIOSummary(HL: Byte; c: TPasItems); virtual;

    { Writes collection T, which is supposed to contain constant items only
      to output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten by descendants. }
    procedure WriteConstants(HL: Byte; c: TPasItems); virtual;

    { If they are assigned, the date values for creation time and time of last
      modification are written to output at heading level HL. }
    procedure WriteDates(const HL: Byte; const Created, LastMod: string);
      virtual; abstract;

    { Writes an already-converted description T to output.
      Takes @link(TPasItem.DetailedDescription) if available,
      @link(TPasItem.Description) otherwise.
      If none of them is assigned, nothing is written. }
    procedure WriteDescription(HL: Byte; const Heading: string; const Item:
      TPasItem);

    { Writes a list of functions / procedure or constructors / destructors /
      methods I to output.
      Heading level HL is used.
      If Methods is true, the 'Methods' heading is used, 'Functions and
      procedures' otherwise.
      Usually, a list of all items is written first, followed by detailed
      descriptions of each item.
      However, this is dependent on the output format. }
    procedure WriteFuncsProcs(const HL: Byte; const Methods: Boolean; const
      FuncsProcs: TPasMethods); virtual; abstract;
      
    { Abstract procedure that must be overwritten by descendants.
      Writes a heading S at level HL to output.
      In HTML, heading levels are regarded by choosing the appropriate
      element from H1 to H6.
      The minimum heading level is 1, the maximum level depends on the
      output format.
      However, it is no good idea to choose a heading level larger than
      five or six.
      Anyway, a descendant should be able to deal with to large HL values,
      e.g. by assigning subsubsection to all Tex headings >= 4. }
    procedure WriteHeading(HL: Byte; const s: string); virtual; abstract;

    { Writes items in I to output, including a heading of level HL and text
      Heading.
      Each item in I should be written with its short description and a
      reference.
      In HTML, this results in a table with two columns. }
    procedure WriteItems(HL: Byte; Heading: string; const Anchor: string;
      const i: TPasItems); virtual; abstract;
      
    { Abstract method, must be overwritten by descendants to implement
      functionality.
      Writes a list of properties P to output.
      Heading level HL is used for the heading FLanguage.Translation[trPROPERTIES). }
    procedure WriteProperties(HL: Byte; const p: TPasProperties); virtual;
      abstract;
      
    { Writes a resources to DestinationDirectory. Existing files will not be overwritten. }
    procedure WriteResourceToFile(const ResourceName, ResourceType: PChar;
      const FileName: string);

    { Writes String S to output, converting each character using
      @link(ConvertChar). }
    procedure WriteString(const s: string);

    { Simply copies characters in text T to output. }
    procedure WriteText(const t: string); virtual;
    
    { Writes collection T, which is supposed to contain type items (TPasItem) to
      output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten in descendants. }
    procedure WriteTypes(const HL: Byte; const t: TPasItems); virtual;

    { Abstract method that writes all documentation for a single unit U to
      output, starting at heading level HL.
      Implementation must be provided by descendant objects and is dependent
      on output format.
      Will call some of the WriteXXX methods like @link(WriteHeading),
      @link(WriteCIOs) or @link(WriteUnitDescription). }
    procedure WriteUnit(const HL: Byte; const U: TPasUnit); virtual;
      abstract;

    { Abstract method to be implemented by descendant objects.
      Writes the (detailed, if available) description T of a unit to output,
      including a FLanguage.Translation[trDESCRIPTION) headline at heading level HL. }
    procedure WriteUnitDescription(HL: Byte; U: TPasUnit); virtual; abstract;

    { Writes documentation for all units, calling @link(WriteUnit) for each
      unit. }
    procedure WriteUnits(const HL: Byte);
    
    { Writes collection V, which is supposed to contain variable items (TPasItem)
      to output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten in descendants. }
    procedure WriteVariables(const HL: Byte; const V: TPasItems); virtual;

    procedure WriteStartOfCode; virtual;

    procedure WriteEndOfCode; virtual;

  public

    { Creates anchors and links for all items in all units. }
    procedure BuildLinks;
    
    { Calls @link(ExpandDescription) for each item in each unit of
      @link(Units). }
    procedure ExpandDescriptions;

    { Assumes C contains file names as PString variables.
      Calls @link(LoadDescriptionFile) with each file name. }
    procedure LoadDescriptionFiles(const c: TStringVector);

    { Abstract procedure, must be overwritten.
      Writes all documentation.
      Will create either a single file or one file for each unit and each
      class, interface or object, depending on output format. }
    procedure WriteDocumentation; virtual;

    property Units: TPasUnits read FUnits write FUnits;

  published
    { the (human) output language of the documentation file(s);
      one of the LANG_xxx constants, e.g. @link(LANG_ENGLISH);
      default language is @link(DEFAULT_LANGUAGE) }
    property Language: TLanguageID read GetLanguage write SetLanguage;
    { Name of the project to create. }
    property ProjectName: string read FProjectName write FProjectName;
    { if true, no link to pasdoc homepage will be included at the bottom of
      HTML files;
      default is false }
    property NoGeneratorInfo: Boolean read FNoGeneratorInfo write FNoGeneratorInfo default False;
    { the output stream that is currently written to; depending on the
      output format, more than one output stream will be necessary to
      store all documentation }
    property Title: string read FTitle write FTitle;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { destination directory for documentation; must include terminating
      forward slash or backslash so that valid file names can be created
      by concatenating DestinationDirectory and a pathless file name }
    property DestinationDirectory: string read FDestDir write SetDestDir;

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
  SysUtils,
  StreamUtils,
  Utils,
  ObjectVector;

{ ---------------------------------------------------------------------------- }
{ TDocGenerator
{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.BuildLinks;

  procedure AssignLinks(MyUnit: TPasUnit; MyObject: TPasCio;
    const DocName: string; c: TPasItems);
  var
    i: Integer;
    p: TPasItem;
  begin
    if (not Assigned(c)) or (c.Count < 1) then Exit;
    for i := 0 to c.Count - 1 do begin
      p := c.PasItemAt[i];
      p.MyObject := MyObject;
      p.MyUnit := MyUnit;
      p.FullLink := CreateLink(p);
      p.HandleAuthorTags;
      p.HandleCreatedTag;
      p.HandleLastModTag;
      p.HandleAbstractTag;
    end;
  end;

var
  CO: TPasCio;
  i: Integer;
  j: Integer;
  U: TPasUnit;
begin
  DoMessage(2, mtInformation, 'Creating links ...', []);
  if IsNilOrEmpty(Units) then Exit;

  for i := 0 to Units.Count - 1 do begin
    U := Units.UnitAt[i];
    U.FullLink := CreateLink(U);
    U.OutputFileName := DestinationDirectory + U.FullLink;
    U.HandleAuthorTags;
    U.HandleCreatedTag;
    U.HandleLastModTag;
    U.HandleAbstractTag;
    AssignLinks(U, nil, U.FullLink, U.Constants);
    AssignLinks(U, nil, U.FullLink, U.Variables);
    AssignLinks(U, nil, U.FullLink, U.Types);
    AssignLinks(U, nil, U.FullLink, U.FuncsProcs);

    if not IsNilOrEmpty(U.CIOs) then begin
      for j := 0 to U.CIOs.Count - 1 do begin
        CO := TPasCio(U.CIOs.PasItemAt[j]);
        CO.MyUnit := U;

        CO.FullLink := CreateLink(CO);
        CO.OutputFileName := DestinationDirectory + CO.FullLink;

        CO.HandleAuthorTags;
        CO.HandleCreatedTag;
        CO.HandleLastModTag;
        CO.HandleAbstractTag;
        AssignLinks(U, CO, CO.FullLink, CO.Fields);
        AssignLinks(U, CO, CO.FullLink, CO.Methods);
        AssignLinks(U, CO, CO.FullLink, CO.Properties);
      end;
    end;
  end;
  DoMessage(2, mtInformation, '... ' + ' links created', []);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.CloseStream;
begin
  if Assigned(FCurrentStream) then begin
    FCurrentStream.Free;
    FCurrentStream := nil;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.CreateLink(const Item: TPasItem): string;
begin
  Result := Item.Name;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.CreateStream(const Name: string): Boolean;
begin
  CloseStream;
  DoMessage(4, mtInformation, 'Creating output stream "' + Name + '".', []);
  Result := False;
  try
    FCurrentStream := TFileStream.Create(Name, fmCreate);
    Result := True;
  except
    on EFileStreamError do ;
  end;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.CodeString(const s: string): string;
begin
  Result := s;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.ExpandDescription(Item: TPasItem; var d: string):
  Boolean;
var
  Ancestor: TPasItem;
  Run: Integer;
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  TheLink: string;
  s: string;
  t: string;
  l: Integer;
begin
  Result := True;
  { check for cases "no id" and "id is empty" }
  if d = '' then Exit;

  l := Length(d);

  { create temporary TText object }
  Run := 1;
  repeat
    if (d[Run] = '@') then begin
        { this is @@ (literal '@')? }
      if (Run <= l - 1) and (d[Run + 1] = '@') then begin
            { literal @ }
        t := t + '@';
        Inc(Run, 2);
      end
      else
          { Is it @Name?
            * Name must follow directly after @.
            * There are no brackets after @Name. }
        if (Run <= l - 4) and
          ((d[Run + 1] = 'N') or (d[Run + 1] = 'n')) and
          ((d[Run + 2] = 'A') or (d[Run + 2] = 'a')) and
          ((d[Run + 3] = 'M') or (d[Run + 3] = 'm')) and
          ((d[Run + 4] = 'E') or (d[Run + 4] = 'e')) then begin
          t := t + CodeString(Item.Name);
          Inc(Run, 5);
        end
        else
          if (Run <= l - 9) and
            ((d[Run + 1] = 'C') or (d[Run + 1] = 'c')) and
            ((d[Run + 2] = 'L') or (d[Run + 2] = 'l')) and
            ((d[Run + 3] = 'A') or (d[Run + 3] = 'a')) and
            ((d[Run + 4] = 'S') or (d[Run + 4] = 's')) and
            ((d[Run + 5] = 'S') or (d[Run + 5] = 's')) and
            ((d[Run + 6] = 'N') or (d[Run + 6] = 'n')) and
            ((d[Run + 7] = 'A') or (d[Run + 7] = 'a')) and
            ((d[Run + 8] = 'M') or (d[Run + 8] = 'm')) and
            ((d[Run + 9] = 'E') or (d[Run + 9] = 'e')) and
            Assigned(Item.MyObject) then begin
            t := t + CodeString(Item.MyObject.Name);
            Inc(Run, 10);
          end
          else
              { Is it @True? }
            if (Run <= l - 4) and
              ((d[Run + 1] = 'T') or (d[Run + 1] = 't')) and
              ((d[Run + 2] = 'R') or (d[Run + 2] = 'r')) and
              ((d[Run + 3] = 'U') or (d[Run + 3] = 'u')) and
              ((d[Run + 4] = 'E') or (d[Run + 4] = 'e')) then begin
              t := t + CodeString('True');
              Inc(Run, 5);
            end
            else
                { Is it @False ? }
              if (Run <= l - 5) and
                ((d[Run + 1] = 'F') or (d[Run + 1] = 'f')) and
                ((d[Run + 2] = 'A') or (d[Run + 2] = 'a')) and
                ((d[Run + 3] = 'L') or (d[Run + 3] = 'l')) and
                ((d[Run + 4] = 'S') or (d[Run + 4] = 's')) and
                ((d[Run + 5] = 'E') or (d[Run + 5] = 'e')) then begin
                t := t + CodeString('False');
                Inc(Run, 6);
              end
              else
                  { Is it @nil ? }
                if (Run <= l - 3) and
                  ((d[Run + 1] = 'N') or (d[Run + 1] = 'n')) and
                  ((d[Run + 2] = 'I') or (d[Run + 2] = 'i')) and
                  ((d[Run + 3] = 'L') or (d[Run + 3] = 'l')) then begin
                  t := t + CodeString('nil');
                  Inc(Run, 4);
                end
                else
                  if (Run <= l - 9) and
                    ((d[Run + 1] = 'I') or (d[Run + 1] = 'i')) and
                    ((d[Run + 2] = 'N') or (d[Run + 2] = 'n')) and
                    ((d[Run + 3] = 'H') or (d[Run + 3] = 'h')) and
                    ((d[Run + 4] = 'E') or (d[Run + 4] = 'e')) and
                    ((d[Run + 5] = 'R') or (d[Run + 5] = 'r')) and
                    ((d[Run + 6] = 'I') or (d[Run + 6] = 'i')) and
                    ((d[Run + 7] = 'T') or (d[Run + 7] = 't')) and
                    ((d[Run + 8] = 'E') or (d[Run + 8] = 'e')) and
                    ((d[Run + 9] = 'D') or (d[Run + 9] = 'd')) and
                    Assigned(Item.MyObject) then begin
                    // Try to find inherited property of item.
                    // Updated 14 Jun 2002

                    if not IsNilOrEmpty(Item.MyObject.Ancestors) then begin
                      s := Item.MyObject.Ancestors.FirstName;
                      Ancestor := SearchItem(s, Item);
                      if Assigned(Ancestor) and (Ancestor.ClassType = TPasCio)
                        then begin
                        repeat
                          TheLink := SearchLink(s + '.' + Item.Name, Item);
                          if TheLink <> '' then Break;

                          if not IsNilOrEmpty(TPasCio(Ancestor).Ancestors)
                            then begin
                            s := TPasCio(Ancestor).Ancestors.FirstName;
                            Ancestor := SearchItem(s, Ancestor);
                          end else begin
                            Break;
                          end;
                        until Ancestor = nil;
                      end;
                    end;

                    Inc(Run, 10);
                    if TheLink <> '' then begin
                      t := t + TheLink;
                    end else begin
                      DoMessage(2, mtWarning, 'Could not resolve "@Inherited" (%s)', [Item.QualifiedName]);
                      t := t + CodeString(Item.Name);
                    end;
                  end
                  else
                      { Is it @<? }
                    if (Run <= l - 1) and
                      (d[Run + 1] = '<') then begin
                      t := t + '&lt;';
                      Inc(Run, 2);
                    end
                    else
                        { Is it @>? }
                      if (Run <= l - 1) and
                        (d[Run + 1] = '>') then begin
                        t := t + '&gt;';
                        Inc(Run, 2);
                      end
                      else
                          { Is it '@<' ? }
                        if (Run <= l - 1) and
                          (d[Run + 1] = '&') then begin
                          t := t + '&amp;';
                          Inc(Run, 2);
                        end
                        else
                            { Is it '@=' ? }
                          if (Run <= l - 1) and
                            (d[Run + 1] = '=') then begin
                            t := t + '&quot;';
                            Inc(Run, 2);
                          end
                          else begin
                            Offs1 := Run;
                            if Item.DescriptionFindTag(d, 'LINK', Offs1,
                              Offs2, Offs3) then begin
                              Item.DescriptionGetTag(d, False, Offs1, Offs2,
                                Offs3, s);
                              t := t + ConvertString(Copy(d, Run, Offs1 -
                                Run));
{
                                    while Run < Offs1 do
                                      begin
                                        t := t + ConvertString(d[Run]);
                                        Inc(Run);
                                      end;
}
                              Run := Offs3 + 1;
                              TheLink := SearchLink(s, Item);

                              if TheLink <> '' then
                                t := t + TheLink
                              else
                                begin
                                  DoMessage(1, mtWarning, 'Could not resolve "%s" (%s)', [s, Item.QualifiedName]);
                                  t := t + CodeString(s);
                                end;
                            end else begin
                              Offs1 := Run;
                              if Item.DescriptionFindTag(d, 'CODE', Offs1,
                                Offs2, Offs3) then begin
                                Item.DescriptionGetTag(d, False, Offs1,
                                  Offs2, Offs3, s);
                                t := t + ConvertString(Copy(d, Run, Offs1 -
                                  Run));
{
                                        while Run < Offs1 do
                                          begin
                                            t := t + ConvertString(d[Run]);
                                            Inc(Run);
                                          end;
}
                                Run := Offs3 + 1;
                                t := t + CodeString(s);
                              end
                              else begin
                                Inc(Run);
                                        // s := d^.GetTagName(Run);
                                        // if //(s <> 'ABSTRACT') and
                                        //  (s <> 'AUTHOR') and
                                        //  (s <> 'EXCLUDE') then
                                        //   begin
                                if Assigned(Item.MyUnit) then
                                  DoMessage(2, mtWarning,
                                    'Found non-link tag when expanding descriptions of "' +
                                    Item.Name + '" in unit ' + Item.MyUnit.Name,
                                    [])
                                else
                                  DoMessage(2, mtWarning,
                                    'Found non-link tag when expanding descriptions of "' +
                                    Item.Name + '"', []);
                                t := t + 'WARNING: @';
                                        //  end;
                                        //  Inc(Run);
                              end;
                            end;
                          end;
    end
    else begin
      if (d[Run] in [#9, #13, #10]) then d[Run] := ' ';
      t := t + ConvertString(d[Run]);
{
        t := t + ConvertChar(d[Run]);
}
      Inc(Run);
    end;
  until (Run > l);

  d := t;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.ExpandDescriptions;

{ expands Description and DetailedDescription of Item }

procedure ExpandItem(Item: TPasItem);
  begin
    if Item = nil then Exit;

    if IsStrEmptyA(Item.Description) then
      Item.Description := ''
    else
      TrimCompress(Item.Description);
    if IsStrEmptyA(Item.DetailedDescription) then
      Item.DetailedDescription := ''
    else
      TrimCompress(Item.DetailedDescription);

    if (not ExpandDescription(Item, Item.Description)) or
      (not ExpandDescription(Item, Item.DetailedDescription)) then begin
      DoMessage(2, mtWarning, 'Could not expand description from ' +
        Item.Name, []);
    end;
  end;

  { for all items in collection C, expands descriptions }

  procedure ExpandCollection(c: TPasItems);
  var
    i: Integer;
    p: TPasItem;
    {T: PText;}
  begin
    if IsNilOrEmpty(c) then Exit;
    for i := 0 to c.Count - 1 do begin
      p := c.PasItemAt[i];
      ExpandItem(p);
    end;
  end;

var
  {C: TPasItems;}
  CO: TPasCio;
  i: Integer;
  j: Integer;
  U: TPasUnit;
begin
  DoMessage(2, mtInformation, 'Expanding descriptions ...', []);

  if IsNilOrEmpty(Units) then Exit;

  for i := 0 to Units.Count - 1 do begin
    U := Units.UnitAt[i];
    ExpandItem(U);
    ExpandCollection(U.Constants);
    ExpandCollection(U.Variables);
    ExpandCollection(U.Types);
    ExpandCollection(U.FuncsProcs);

    if not IsNilOrEmpty(U.CIOs) then
      for j := 0 to U.CIOs.Count - 1 do begin
        CO := TPasCio(U.CIOs.PasItemAt[j]);
        ExpandItem(CO);
        ExpandCollection(CO.Fields);
        ExpandCollection(CO.Methods);
        ExpandCollection(CO.Properties);
      end;
  end;

  DoMessage(2, mtInformation, '... Descriptions expanded', []);
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.ExtractEmailAddress(s: string; out S1, S2,
  EmailAddress: string): Boolean;
const
  ALLOWED_CHARS = ['a'..'z', 'A'..'Z', '-', '.', '_', '0'..'9'];
  Letters = ['a'..'z', 'A'..'Z'];
var
  atPos: Integer;
  i: Integer;
begin
  Result := False;
  if (Length(s) < 6) { minimum length of email address: a@b.cd } then Exit;
  atPos := Pos('@', s);
  if (atPos < 2) or (atPos > Length(s) - 3) then Exit;
  { assemble address left of @ }
  i := atPos - 1;
  while (i >= 1) and (s[i] in ALLOWED_CHARS) do
    Dec(i);
  EmailAddress := System.Copy(s, i + 1, atPos - i - 1) + '@';
  S1 := '';
  if (i > 1) then S1 := System.Copy(s, 1, i);
  { assemble address right of @ }
  i := atPos + 1;
  while (i <= Length(s)) and (s[i] in ALLOWED_CHARS) do
    Inc(i);
  EmailAddress := EmailAddress + System.Copy(s, atPos + 1, i - atPos - 1);
  if (Length(EmailAddress) < 6) or
    (not (EmailAddress[Length(EmailAddress)] in Letters)) or
  (not (EmailAddress[Length(EmailAddress) - 1] in Letters)) then Exit;
  S2 := '';
  if (i <= Length(s)) then S2 := System.Copy(s, i, Length(s) - i + 1);
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.FindGlobal(const S1, S2, S3: string; const n: Integer): TPasItem;
var
  i: Integer;
  Item: TPasItem;
  CIO: TPasCio;
  U: TPasUnit;
begin
  Result := nil;

  if IsNilOrEmpty(Units) then Exit;
  
  case n of
    0: { }
      for i := 0 to Units.Count - 1 do begin
        U := Units.UnitAt[i];
        Item := U.FindItem(S1);
        if Assigned(Item) then begin
          Result := Item;
          Exit;
        end;
      end;
    1: begin
        { object.field_method_property }
        for i := 0 to Units.Count - 1 do begin
          U := Units.UnitAt[i];
          ;
          if Assigned(U.CIOs) then begin
            CIO := TPasCio(U.CIOs.FindName(S1));
            if Assigned(CIO) then begin
              Item := CIO.FindFieldMethodProperty(S2);
              if Assigned(Item) then begin
                Result := Item;
                Exit;
              end;
            end;
          end;
        end;
        { unit.cio_var_const_type }
        U := TPasUnit(Units.FindName(S1));
        if Assigned(U) then begin
          Item := U.FindItem(S2);
          Result := Item;
          Exit;
        end;
      end;
    2: { unit.objectorclassorinterface.fieldormethodorproperty } begin
        U := TPasUnit(Units.FindName(S1));
        if (not Assigned(U)) then Exit;
        Item := U.FindItem(S2);
        if (not Assigned(Item)) then Exit;
        Item := Item.FindItem(S3);
        if (not Assigned(Item)) then Exit;
        Result := Item;
        Exit;
      end;
  end;
  Result := nil;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.GetCIOTypeName(MyType: TCIOType): string;
begin
  case MyType of
    CIO_CLASS: Result := FLanguage.Translation[trClass];
    CIO_SPINTERFACE: Result := FLanguage.Translation[trDispInterface];
    CIO_INTERFACE: Result := FLanguage.Translation[trInterface];
    CIO_OBJECT: Result := FLanguage.Translation[trObject];
    CIO_RECORD: Result := 'record'; // TODO
  else
    Result := '';
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.LoadDescriptionFile(n: string);
var
  f: TStream;
  ItemName: string;
  s: string;
  t: string;
begin
  if n = '' then Exit;

  DoMessage(3, mtInformation, 'Loading descriptions from file "' + n + '"',
    []);
  f := TFileStream.Create(n, fmOpenRead);
  if not Assigned(f) then
    DoError('Could not open description file "%s%.', [n]);
  t := '';
  while (f.Position < f.Size) do begin
    s := StreamReadLine(f);
      {    DoMessage(4, 'DEBUG - descr "' + S + '"');}
    if (Length(s) > 0) then begin
          { # means: description of another item begins }
      if (s[1] = '#') then begin
              { if there is an old description, deal with it }
        StoreDescription(ItemName, t);
              { delete # char }
        System.Delete(s, 1, 1);
              { skip whitespace }
        while (Length(s) > 0) and (s[1] in [' ', #9]) do
          System.Delete(s, 1, 1);
              { find item }
        ItemName := '';
        while (Length(s) > 0) and (s[1] in ['A'..'Z', 'a'..'z', '_', '.',
          '0'..'9']) do begin
          ItemName := ItemName + s[1];
          System.Delete(s, 1, 1);
        end;
      end
      else begin
              { check if there is a text }
        if t = '' then begin
          DoMessage(2, mtError,
            'First line of description file must start with "# item_name"',
            []);
          Break; { leave while loop }
        end;
        t := t + s + #10;
      end;
    end;
  end;
  StoreDescription(ItemName, t);
  f.Free;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.LoadDescriptionFiles(const c: TStringVector);
var
  i: Integer;
begin
  if c <> nil then begin
    DoMessage(3, mtInformation, 'Loading description files ...', []);
    for i := 0 to c.Count - 1 do
      LoadDescriptionFile(c[i]);
  end;
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.SearchItem(s: string; const Item: TPasItem): TPasItem;
var
  n: Integer;
  S1: string;
  S2: string;
  S3: string;
begin
  { S is supposed to have 0 to 2 dots in it - S1, S2 and S3 contain
    the parts between the dots, N the number of dots }
  if (not SplitLink(s, S1, S2, S3, n)) then begin
    DoMessage(2, mtWarning, 'The link "' + s + '" is invalid', []);
    Result := nil;
    Exit;
  end;

  { first try to find link starting at Item }
  if Assigned(Item) then begin
    Result := Item.FindName(S1, S2, S3, n);
  end
  else
    Result := nil;

  if not Assigned(Result) then Result := FindGlobal(S1, S2, S3, n);
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.SearchLink(s: string; const Item: TPasItem): string;
var
  i, n: Integer;
  S1: string;
  S2: string;
  S3: string;
  UnitName: string;
  FoundItem: TPasItem;
  U: TPasUnit;
begin
  { S is supposed to have 0 to 2 dots in it - S1, S2 and S3 contain
    the parts between the dots, N the number of dots }
  if (not SplitLink(s, S1, S2, S3, n)) then begin
    if Item.MyUnit = nil then
      DoMessage(2, mtWarning, 'Invalid Link "' + s + '" (' + Item.Name + ')', [])
    else
      DoMessage(2, mtWarning, 'Invalid Link "' + s + '" (' + Item.MyUnit.Name + '.' + Item.Name + ')', []);
    Result := 'UNKNOWN';
    Exit;
  end;

  { first try to find link starting at Item }
  if Assigned(Item) then begin
    FoundItem := Item.FindName(S1, S2, S3, n);
  end
  else
    FoundItem := nil;

  { Next try to find link in items's unit uses units. }
  if FoundItem = nil then
    if Assigned(Item.MyUnit) then
      if Assigned(Item.MyUnit.UsesUnits) then begin
        i := Item.MyUnit.UsesUnits.Count;
        while i > 0 do begin
          Dec(i);
          UnitName := Item.MyUnit.UsesUnits[i];
          U := TPasUnit(Units.FindName(UnitName));
          if U <> nil then
                // FoundItem := U^.FindName(s1, s2, S3, n);
            FoundItem := U.FindFieldMethodProperty(S1, S2);
          if FoundItem <> nil then Break;
        end;
      end;

  { Find Global }
  if FoundItem = nil then
    FoundItem := FindGlobal(S1, S2, S3, n);

  if Assigned(FoundItem) then
    Result := CreateReferencedLink(FoundItem.Name, FoundItem.FullLink)
  else
    Result := ''; // RJ ConvertString(s);
end;

{ ---------------------------------------------------------------------------- }

function TDocGenerator.SplitLink(s: string; var S1, S2, S3: string;
  var n: Integer): Boolean;

  procedure SplitInTwo(s: string; var S1, S2: string);
  var
    i: Integer;
  begin
    i := Pos('.', s);
    if (i = 0) then begin
      S1 := s;
      S2 := '';
    end
    else begin
      S1 := System.Copy(s, 1, i - 1);
      S2 := System.Copy(s, i + 1, Length(s));
    end;
  end;

var
  i: Integer;
  t: string;
begin
  Result := False;
  S1 := '';
  S2 := '';
  S3 := '';
  n := 0;
  {  I := 1;}
  s := Trim(s);
  if (Length(s) = 0) then Exit;
  if (not (s[1] in IdentifierStart)) then Exit;
  i := 2;
  while (i <= Length(s)) do begin
    if (not (s[i] in IdentifierOther)) then Exit;
    Inc(i);
  end;
  SplitInTwo(s, S1, S2);
  if (Length(S2) = 0) then begin
    n := 0;
  end
  else begin
    t := S2;
    SplitInTwo(t, S2, S3);
    if (Length(S3) = 0) then
      n := 1
    else
      n := 2;
  end;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.StoreDescription(ItemName: string; var t: string);
var
  Item: TPasItem;
  n: Integer;
  S1: string;
  S2: string;
  S3: string;
begin
  if t = '' then Exit;

  DoMessage(5, mtInformation, 'Storing description for ' + ItemName, []);
  if SplitLink(ItemName, S1, S2, S3, n) then begin
    Item := FindGlobal(S1, S2, S3, n);
    if Assigned(Item) then begin
      if Item.Description <> '' then begin
        DoMessage(2, mtWarning, 'More than one description for ' + ItemName,
          []);
        t := '';
      end else begin
        Item.Description := t;
      end;
    end else begin
      DoMessage(2, mtWarning, 'Could not find item ' + ItemName, []);
      t := '';
    end;
  end else begin
    DoMessage(2, mtWarning, 'Could not split item "' + ItemName + '"', []);
  end;
  t := '';
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteCIOs(HL: Byte; c: TPasItems);
var
  i: Integer;
begin
  if IsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    WriteCIO(HL, TPasCio(c.PasItemAt[i]));
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteCIOSummary(HL: Byte; c: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trSummaryCio], 'Classes', c);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteConstants(HL: Byte; c: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trConstants], 'Constants', c);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteDescription(HL: Byte; const Heading: string;
  const Item: TPasItem);
var
  d: string;
begin
  if Item.DetailedDescription <> '' then
    d := Item.DetailedDescription
  else
    if Item.Description <> '' then
      d := Item.Description
    else
      Exit;

  if Length(Heading) > 0 then WriteHeading(HL, Heading);
  WriteText(d);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteLine(const s: string);
begin
  WriteString(s);
  StreamUtils.WriteLine(CurrentStream, '');
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteResourceToFile(const ResourceName, ResourceType:
  PChar; const FileName: string);
var
  HResInfo: HRSRC;
  HGlobal: THandle;
begin
  HResInfo := FindResource(MainInstance, ResourceName, ResourceType);
  if HResInfo = 0 then Exit;

  HGlobal := LoadResource(MainInstance, HResInfo);
  if HGlobal = 0 then Exit;

  with TFileStream.Create(DestinationDirectory + FileName, fmCreate) do begin
    try
      Write(LockResource(HGlobal)^, SizeOfResource(MainInstance, HResInfo));
    finally
      Free;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteString(const s: string);
var
  t: string;
begin
  t := ConvertString(s);
  CurrentStream.WriteBuffer(t[1], Length(t));
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteText(const t: string);
begin
  CurrentStream.WriteBuffer(t[1], Length(t));
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteTypes(const HL: Byte; const t: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trTypes], 'Types', t);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteUnits(const HL: Byte);
var
  i: Integer;
begin
  if IsNilOrEmpty(Units) then Exit;
  for i := 0 to Units.Count - 1 do begin
    WriteUnit(HL, Units.UnitAt[i]);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteVariables(const HL: Byte; const V: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trVariables], 'Variables', V);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.DoError(const AMessage: string; const AArguments:
  array of const; const AExitCode: Integer);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.DoMessage(const AVerbosity: Cardinal; const
  MessageType: TMessageType; const AMessage: string; const AArguments: array of
  const);
begin
  if Assigned(FOnMessage) then begin
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
  end;
end;

function TDocGenerator.ConvertString(const s: string): string;
begin
  Result := s;
end;

constructor TDocGenerator.Create(AOwner: TComponent);
begin
  inherited;
  FClassHierarchy := nil;
  FNoGeneratorInfo := False;
  FLanguage := TPasDocLanguages.Create;
end;

procedure TDocGenerator.CreateClassHierarchy;
var
  unitLoop: Integer;
  classLoop: Integer;
  PU: TPasUnit;
  ACIO: TPasCio;
  ParentItem: TPasItem;
  Parent, Child: TPasItemNode;
begin
  FClassHierarchy := TStringCardinalTree.Create;
  for unitLoop := 0 to Units.Count - 1 do begin
    PU := Units.UnitAt[unitLoop];
    if PU.CIOs = nil then Continue;
    for classLoop := 0 to PU.CIOs.Count - 1 do begin
      ACIO := TPasCio(PU.CIOs.PasItemAt[classLoop]);
      if ACIO.MyType in CIO_NonHierarchy then continue;

      if Assigned(ACIO.Ancestors) and (ACIO.Ancestors.Count > 0) then begin
        ParentItem := FindGlobal(ACIO.Ancestors.FirstName, '', '', 0);
        if Assigned(ParentItem) then begin
          Parent := FClassHierarchy.ItemOfName(ParentItem.Name);
          // Add parent if not already there.
          if Parent = nil then begin
            Parent := FClassHierarchy.InsertItem(ParentItem);
          end;
        end else begin
          Parent := FClassHierarchy.ItemOfName(ACIO.Ancestors.FirstName);
          if Parent = nil then begin
            Parent := FClassHierarchy.InsertName(ACIO.Ancestors.FirstName);
          end;
        end;
      end else begin
        Parent := nil;
      end;

      Child := FClassHierarchy.ItemOfName(ACIO.Name);
      if Child = nil then begin
        FClassHierarchy.InsertItemParented(Parent, ACIO)
      end else begin
        if Parent <> nil then begin
          FClassHierarchy.MoveChildLast(Child, Parent);
        end;
      end;
    end;
  end;
  FClassHierarchy.Sort;
end;

destructor TDocGenerator.Destroy;
begin
  FLanguage.Free;
  FClassHierarchy.Free;
  inherited;
end;

procedure TDocGenerator.WriteEndOfCode;
begin
// nothing - for some output this is irrelevant
end;

procedure TDocGenerator.WriteStartOfCode;
begin
// nothing - for some output this is irrelevant
end;

procedure TDocGenerator.WriteDocumentation;
begin
end;

procedure TDocGenerator.SetLanguage(const Value: TLanguageID);
begin
  FLanguage.Language := Value;
end;

procedure TDocGenerator.SetDestDir(const Value: string);
begin
  FDestDir := IncludeTrailingPathDelimiter(Value);
end;

function TDocGenerator.GetLanguage: TLanguageID;
begin
  Result := FLanguage.Language;
end;

end.
