{$B-}
{ @abstract(basic doc generator object)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Ivan Montes Velencoso (senbei@teleline.es))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Philippe Jean Dit Bailleul (jdb@abacom.com))
  @author(Rodrigo Urubatan Ferreira Jardim (rodrigo@netscape.net))
  @author(Grzegorz Skoczylas <gskoczylas@program.z.pl>)
  @author(Pierre Woestyn <pwoestyn@users.sourceforge.net>)
  @created(30 Aug 1998)
  @cvs($Date$)

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
  ObjectVector,
  PasDoc_HierarchyTree,
  PasDoc_Types,
  PasDoc_RunHelp,
  Classes;

const
  { set of characters, including all letters and the underscore }
  IdentifierStart = ['A'..'Z', 'a'..'z', '_'];

  { set of characters, including all characters from @link(IdentifierStart)
    plus the ten decimal digits }
  IdentifierOther = ['A'..'Z', 'a'..'z', '_', '0'..'9', '.'];

  { number of overview files that pasdoc generates for
    multiple-document-formats like @link(HTML) }
  NUM_OVERVIEW_FILES = 10;
  NUM_OVERVIEW_FILES_USED = 8;

  { names of all overview files, extensions not included }
  OverviewFilenames: array[0..NUM_OVERVIEW_FILES - 1] of shortstring =
  ( 'AllUnits',
    'ClassHierarchy',
    'AllClasses',
    'AllTypes',
    'AllVariables',
    'AllConstants',
    'AllFunctions',
    'AllIdentifiers',
    'GVUses',
    'GVClasses');

type
  TCodeType = (ctWhiteSpace, ctString, ctCode, ctEndString, ctChar,
    ctParenComment, ctBracketComment, ctSlashComment, ctCompilerComment,
    ctEndComment);

  { @abstract(class for spell-checking) }
  TSpellingError = class
  public
    { the mis-spelled word }
    Word: string;
    { offset inside the checked string }
    Offset: Integer;
    { comma-separated list of suggestions }
    Suggestions: string;
  end;

  { Result for @link(TDocGenerator.CreateStream) }
  TCreateStreamResult = (
    { normal result }
    csCreated,
    { if file exists this will be returned, unless overwrite is true }
    csExisted,
    { returned on error }
    csError
  );

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
  private
    FCheckSpelling,
    FSpellCheckStarted: boolean;
    FAspellLanguage: string;
    FAspellPipe: TRunRecord;
    FIgnoreWordsFile,
    FAspellMode: string;
    FFullLink: boolean;
  protected
    FAbbreviations: TStringList;
    FGraphVizClasses: boolean;
    FGraphVizUses: boolean;
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

    procedure SetAbbreviations(const Value: TStringList);
    function GetLanguage: TLanguageID;
    procedure SetLanguage(const Value: TLanguageID);
    procedure SetDestDir(const Value: string);

    procedure DoError(const AMessage: string; const AArguments: array of const;
      const AExitCode: Word);
    procedure DoMessage(const AVerbosity: Cardinal;
      const MessageType: TMessageType; const AMessage: string;
      const AArguments: array of const);

    property CurrentStream: TStream read FCurrentStream;

    procedure CreateClassHierarchy;

  protected
    { list of all units that were successfully parsed }
    FUnits: TPasUnits;

    { If field @link(Stream) is assigned, it is disposed and set to nil. }
    procedure CloseStream;

    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html.
      @param(s is the string to format)
      @returns(the formatted string) 
    }
    function CodeString(const s: string): string; virtual;

    { Called when an @html tag is encountered. }
    function HtmlString(const Desc: string; Len: integer;
      var CurPos: integer): string; virtual;

    (* Called when an @longcode tag is encountered. This tag is used to format
      the enclosed text in the same way it would be in Delphi (using the
      default settings in Delphi).

    Because any character including the ')' character might be in your code,
    there needs to be a special way to mark the end of the @longCode tag.
    To do this include a special character such as "#' just after the opening
    '(' of the @longcode tag.  Include that same character again just before
    the closing ')' of the @longcode tag.

      Here is an example of the @longcode tag in use. Check the source code
      to see how it was done.

      @longCode(#
procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // Note that your comments are formatted.
  {$H+} // You can even include compiler directives.
  // reserved words are formatted in bold.
  for i := 1 to 10 do
  begin
    It is OK to include pseudo-code like this line.
    // It will be formatted as if it were meaningful pascal code.
  end;
end;
      #)
      *)
    function LongCode(const Desc: string; Len: integer;
      var CurPos: integer): string; virtual;
    { Mark the string as a parameter, e.g. <b>TheString</b> }
    function ParameterString(const ParamType, Param: string): string; virtual;

    { Converts for each character in S, thus assembling a
      String that is returned and can be written to the documentation file. 
      
      The @@ character should not be converted, this will be done later on. 
    }
    function ConvertString(const s: string): string; virtual; abstract;
    { Converts a character to its converted form. This method
      should always be called to add characters to a string.
      
      @@ should also be converted by this routine.
    }
    function ConvertChar(c: char): string; virtual; abstract;

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
    function CreateStream(const AName: string; const AOverwrite: boolean): TCreateStreamResult;

    { Must be overwritten.
      From an item name and its link, this creates a language-specific
      reference to that item. }
    function CreateReferencedLink(ItemName, Link: string): string; virtual; abstract;

    { Takes description D of the item Item, expands links (using Item),
      converts output-specific characters.
      Returns true on success, false otherwise (not enough memory?). }
    function ExpandDescription(Item: TPasItem; var d: string): Boolean; virtual;  // GSk: changed to virtual

    { Searches for an email address in String S. Searches for first appearance
      of the @@ character}
    function ExtractEmailAddress(s: string; var S1, S2, EmailAddress: string): Boolean;

    { Searches for a link in string S, signified by
      xxx://xxxx/.../ }
    function ExtractLink(s: string; var S1,S2,Link: string): Boolean;

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
    procedure WriteCIO(HL: integer; const CIO: TPasCio); virtual; abstract;

    { Writes all classes, interfaces and objects in C to output, calling
      @link(WriteCIO) with each, at heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems); virtual;

    { Abstract procedure, must be overwritten by descendants.
      Writes a list of all classes, interfaces and objects in C at heading
      level HL to output. }
    procedure WriteCIOSummary(HL: integer; c: TPasItems); virtual;

    { Writes collection T, which is supposed to contain constant items only
      to output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten by descendants. }
    procedure WriteConstants(HL: integer; c: TPasItems); virtual;

    { If they are assigned, the date values for creation time and time of last
      modification are written to output at heading level HL. }
    procedure WriteDates(const HL: integer; const Created, LastMod: string);
      virtual; abstract;

    { Writes an already-converted description T to output.
      Takes @link(TPasItem.DetailedDescription) if available,
      @link(TPasItem.Description) otherwise.
      If none of them is assigned, nothing is written. }
    procedure WriteDescription(HL: integer; const Heading: string; const Item:
      TPasItem);

    { Writes a list of functions / procedure or constructors / destructors /
      methods I to output.
      Heading level HL is used.
      If Methods is true, the 'Methods' heading is used, 'Functions and
      procedures' otherwise.
      Usually, a list of all items is written first, followed by detailed
      descriptions of each item.
      However, this is dependent on the output format. }
    procedure WriteFuncsProcs(const HL: integer; const Methods: Boolean; const
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
    procedure WriteHeading(HL: integer; const s: string); virtual; abstract;

    { Writes items in I to output, including a heading of level HL and text
      Heading.
      Each item in I should be written with its short description and a
      reference.
      In HTML, this results in a table with two columns. }
    procedure WriteItems(HL: integer; Heading: string; const Anchor: string;
      const i: TPasItems); virtual; abstract;

    { Abstract method, must be overwritten by descendants to implement
      functionality.
      Writes a list of properties P to output.
      Heading level HL is used for the heading FLanguage.Translation[trPROPERTIES). }
    procedure WriteProperties(HL: integer; const p: TPasProperties); virtual;
      abstract;

    { Writes String S to output, converting each character using
      @link(ConvertString). }
    procedure WriteConverted(const s: string; Newline: boolean); overload; virtual;

    procedure WriteConverted(const s: string); overload; virtual;

    { Simply copies characters in text T to output. }
    procedure WriteDirect(const t: string; Newline: boolean); overload; virtual;

    procedure WriteDirect(const t: string); overload; virtual; 
    
    { Writes collection T, which is supposed to contain type items (TPasItem) to
      output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten in descendants. }
    procedure WriteTypes(const HL: integer; const t: TPasItems); virtual;

    { Abstract method that writes all documentation for a single unit U to
      output, starting at heading level HL.
      Implementation must be provided by descendant objects and is dependent
      on output format.
      Will call some of the WriteXXX methods like @link(WriteHeading),
      @link(WriteCIOs) or @link(WriteUnitDescription). }
    procedure WriteUnit(const HL: integer; const U: TPasUnit); virtual;
      abstract;

    { Abstract method to be implemented by descendant objects.
      Writes the (detailed, if available) description T of a unit to output,
      including a FLanguage.Translation[trDESCRIPTION) headline at heading level HL. }
    procedure WriteUnitDescription(HL: integer; U: TPasUnit); virtual; abstract;

    { Writes documentation for all units, calling @link(WriteUnit) for each
      unit. }
    procedure WriteUnits(const HL: integer);
    
    { Writes collection V, which is supposed to contain variable items (TPasItem)
      to output at heading level HL with heading FLanguage.Translation[trTYPES) calling
      @link(WriteItems).
      Can be overwritten in descendants. }
    procedure WriteVariables(const HL: integer; const V: TPasItems); virtual;

    procedure WriteStartOfCode; virtual;

    procedure WriteEndOfCode; virtual;

    { output graphviz uses tree }
    procedure WriteGVUses;
    { output graphviz class tree }
    procedure WriteGVClasses;

    { starts the spell checker - currently linux only }
    procedure StartSpellChecking(const AMode: string);

    { checks a word and returns suggestions.
      Will create an entry in AWords for each wrong word,
      and the object (if not nil meaning no suggestions) will contain
      another string list with suggestions. The value will be the
      offset from the start of AString.
      Example:
        check the string "the quieck brown fox"
        result is:
        AErrors contains a single item:
          quieck=5 with object a stringlist containing something like the words
          quick, quiesce, ... }
    procedure CheckString(const AString: string; const AErrors: TObjectVector);

    { closes the spellchecker }
    procedure EndSpellChecking;
    // FormatPascalCode will cause Line to be formatted in
    // the way that Pascal code is formatted in Delphi.
    function FormatPascalCode(const Line: string): string; virtual;
    // FormatCode will cause AString to be formatted in the
    // way that Pascal statements are in Delphi.
    function FormatCode(AString: string): string; virtual;
    // FormatComment will cause AString to be formatted in
    // the way that comments other than compiler directives are
    // formatted in Delphi.  See: @link(FormatCompilerComment).
    function FormatComment(AString: string): string; virtual;
    // FormatKeyWord will cause AString to be formatted in
    // the way that strings are formatted in Delphi.
    function FormatString(AString: string): string; virtual;
    // FormatKeyWord will cause AString to be formatted in
    // the way that reserved words are formatted in Delphi.
    function FormatKeyWord(AString: string): string; virtual;
    // FormatCompilerComment will cause AString to be formatted in
    // the way that compiler directives are formatted in Delphi.
    function FormatCompilerComment(AString: string): string; virtual;
  public

    { Creates anchors and links for all items in all units. }
    procedure BuildLinks; virtual;
    
    { Calls @link(ExpandDescription) for each item in each unit of
      @link(Units). }
    procedure ExpandDescriptions;

    { Assumes C contains file names as PString variables.
      Calls @link(LoadDescriptionFile) with each file name. }
    procedure LoadDescriptionFiles(const c: TStringVector);

    { Must be overwritten, writes all documentation.
      Will create either a single file or one file for each unit and each
      class, interface or object, depending on output format. }
    procedure WriteDocumentation; virtual;

    property Units: TPasUnits read FUnits write FUnits;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ParseAbbreviationsFile(const AFileName: string);

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

    { destination directory for documentation; must include terminating
      forward slash or backslash so that valid file names can be created
      by concatenating DestinationDirectory and a pathless file name }
    property DestinationDirectory: string read FDestDir write SetDestDir;

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;

    property OutputGraphVizUses: boolean read FGraphVizUses write FGraphVizUses;
    property OutputGraphVizClassHierarchy: boolean read FGraphVizClasses write FGraphVizClasses;

    property Abbreviations: TStringList read FAbbreviations write SetAbbreviations;

    property CheckSpelling: boolean read FCheckSpelling write FCheckSpelling;
    property AspellLanguage: string read FAspellLanguage write FAspellLanguage;
    property IgnoreWordsFile: string read FIgnoreWordsFile write FIgnoreWordsFile;
    property FullLink: boolean read FFullLink write FFullLink;
  end;

var
  ReservedWords: TStringList;


implementation

uses
  SysUtils,
  StreamUtils,
  Utils;

{ ---------------------------------------------------------------------------- }
{ TDocGenerator                                                                }
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
      if not p.WasDeserialized then begin
        p.Abbreviations := FAbbreviations;
        p.HandleAuthorTags;
        p.HandleCreatedTag;
        p.HandleLastModTag;
        p.HandleCVSTag;
        p.HandleAbstractTag;
        p.HandleParamTag;
        p.HandleReturnsTag;
        p.HandleRaisesTag;
      end;
    end;
  end;

var
  CO: TPasCio;
  i: Integer;
  j: Integer;
  U: TPasUnit;
begin
  DoMessage(2, mtInformation, 'Creating links ...', []);
  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  for i := 0 to Units.Count - 1 do begin
    U := Units.UnitAt[i];
    U.FullLink := CreateLink(U);
    U.OutputFileName := U.FullLink;
    U.Abbreviations := FAbbreviations;
    if not U.WasDeserialized then begin
      U.HandleAuthorTags;
      U.HandleCreatedTag;
      U.HandleLastModTag;
      U.HandleCVSTag;
      U.HandleAbstractTag;
      U.HandleParamTag;
      U.HandleReturnsTag;
      U.HandleRaisesTag;
    end;
    AssignLinks(U, nil, U.FullLink, U.Constants);
    AssignLinks(U, nil, U.FullLink, U.Variables);
    AssignLinks(U, nil, U.FullLink, U.Types);
    AssignLinks(U, nil, U.FullLink, U.FuncsProcs);

    if not ObjectVectorIsNilOrEmpty(U.CIOs) then begin
      for j := 0 to U.CIOs.Count - 1 do begin
        CO := TPasCio(U.CIOs.PasItemAt[j]);
        CO.MyUnit := U;

        if not CO.WasDeserialized then begin
          CO.FullLink := CreateLink(CO);
          CO.OutputFileName := CO.FullLink;

          CO.Abbreviations := FAbbreviations;

          CO.HandleAuthorTags;
          CO.HandleCreatedTag;
          CO.HandleLastModTag;
          CO.HandleCVSTag;
          CO.HandleAbstractTag;
          CO.HandleParamTag;
          CO.HandleReturnsTag;
          CO.HandleRaisesTag;
        end;
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

function TDocGenerator.CreateStream(const AName: string;
  const AOverwrite: boolean): TCreateStreamResult;
begin
  CloseStream;
  DoMessage(4, mtInformation, 'Creating output stream "' + AName + '".', []);
  Result := csError;
  if FileExists(DestinationDirectory + AName) and not AOverwrite then begin
    Result := csExisted;
  end else begin
    try
      FCurrentStream := TFileStream.Create(DestinationDirectory+AName, fmCreate);
      Result := csCreated;
    except
    end;
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

  function GetNextWord(const Desc: string; Len: integer; var CurPos: integer): string;
  begin
    Result := '';
    while (CurPos < Len) and (Desc[CurPos] in ['(', ' ', #9]) do
      Inc(CurPos);
    while (CurPos < Len) and not (Desc[CurPos] in [')', ' ', #9]) do
      begin
        Result := Result + Desc[CurPos];
        Inc(CurPos);
      end;
  end;

  function IsMacro(const Desc: string; Len: integer;
    const Macro: string; var CurPos: integer): boolean;
  var
    i: integer;
    l: integer;
    s: string;
  begin
    Result := false;
    s := UpperCase(Macro);
    l := Length(s);

    if CurPos + l > Len then
      exit;

    for i:=1 to l do
      if UpCase(Desc[CurPos+i]) <> s[i] then
        exit;

    { TODO -cfixme -ojmb: is NOT (a..z,A..Z) ok? I don't see what difference it makes... }
    if (CurPos + l = Len) or (not (Desc[CurPos + l + 1] in ['a'..'z','A'..'Z','0'..'9'])) then begin
      Inc(CurPos, l + 1);
      Result := true;
    end;
  end;

var
  Run: Integer;
  Offs1: Integer;
  Offs2: Integer;
  Offs3: Integer;
  TheLink: string;
  s: string;
  t: string;
  l: Integer;
  TheObject: TPasCio;
  Ancestor: TPasItem;
begin
  Result := True;
  { check for cases "no id" and "id is empty" }
  if d = '' then Exit;
  { first convert the string to its correct representation }
  l := Length(d);

  { create temporary TText object }
  Run := 1;
  repeat
    if (d[Run] = '@') then begin
        { this is @@ (literal '@')? }
      if (Run <= l - 1) and (d[Run + 1] = '@') then
      begin
        { literal @ }
        t := t + ConvertChar('@');
        Inc(Run, 2);
      end
      else
        if IsMacro(d, l, 'LONGCODE', Run) then begin
          t := t + LongCode(d, l, Run) + ' ';
        end
        else
        if IsMacro(d, l, 'HTML', Run) then begin
          t := t + HtmlString(d, l, Run) + ' ';
        end
        else
        if IsMacro(d, l, 'RAISES', Run) then begin
          t := t + ParameterString('Raises', ConvertString(GetNextWord(d, l, Run))) + ' ';
        end
        else
        if IsMacro(d, l, 'PARAM', Run) then begin
          t := t + ParameterString('', ConvertString(GetNextWord(d, l, Run))) + ' ';
        end
        else
        if IsMacro(d, l, 'RETURN', Run) or
          IsMacro(d, l, 'RETURNS', Run) then begin
          t := t + ParameterString('', 'Returns') + ' ';
        end
        else
          { Is it @Name?
            * Name must follow directly after @.
            * There are no brackets after @Name. }
        if IsMacro(d, l, 'NAME', Run) then begin
          t := t + CodeString(ConvertString(Item.Name));
        end
        else
          if IsMacro(d, l, 'CLASSNAME', Run) then begin
            if Assigned(Item.MyObject) then begin
              t := t + CodeString(ConvertString(Item.MyObject.Name));
            end else if Item is TPasCio then begin
              t := t + CodeString(ConvertString(Item.Name));
            end
          end
          else
              { Is it @True? }
            if IsMacro(d, l, 'TRUE', Run) then begin
              t := t + CodeString('True');
            end
            else
                { Is it @False ? }
              if IsMacro(d, l, 'FALSE', Run) then begin
                t := t + CodeString('False');
              end
              else
                  { Is it @nil ? }
                if IsMacro(d, l, 'NIL', Run) then begin
                  t := t + CodeString('nil');
                end
                else
                  if IsMacro(d, l, 'INHERITED', Run) then begin
                    if Assigned(Item.MyObject) then
                      TheObject := Item.MyObject
                    else if Item is TPasCio then
                      TheObject := TPasCio(Item)
                    else
                      TheObject := nil;
                    // Try to find inherited property of item.
                    // Updated 14 Jun 2002

                    if Assigned(TheObject)
                      and not StringVectorIsNilOrEmpty(TheObject.Ancestors) then begin
                      s := TheObject.Ancestors.FirstName;
                      Ancestor := SearchItem(s, Item);
                      if Assigned(Ancestor) and (Ancestor.ClassType = TPasCio)
                        then begin
                        repeat
                          if Item.MyObject = nil then
                            // we are looking for the ancestor itself
                            TheLink := SearchLink(s, Item)
                          else
                            // we are looking for an ancestor's property or method
                            TheLink := SearchLink(s + '.' + Item.Name, Item);
                          if TheLink <> '' then Break;

                          if not StringVectorIsNilOrEmpty(TPasCio(Ancestor).Ancestors)
                            then begin
                            s := TPasCio(Ancestor).Ancestors.FirstName;
                            Ancestor := SearchItem(s, Ancestor);
                          end else begin
                            Break;
                          end;
                        until Ancestor = nil;
                      end;
                    end;

                    if TheLink <> '' then begin
                      t := t + TheLink;
                    end else begin
                      DoMessage(2, mtWarning, 'Could not resolve "@Inherited" (%s)', [Item.QualifiedName]);
                      t := t + CodeString(ConvertString(Item.Name));
                    end;
                  end
                  else begin
                    Offs1 := Run;
                    if Item.DescriptionFindTag(d, 'LINK', Offs1,
                      Offs2, Offs3) then begin
                      Item.DescriptionGetTag(d, False, Offs1, Offs2,
                        Offs3, s);
                      t := t + ConvertString(Copy(d, Run, Offs1 -
                        Run));
                      Run := Offs3 + 1;
                      TheLink := SearchLink(s, Item);

                      if TheLink <> '' then
                        t := t + TheLink
                      else
                        begin
                          DoMessage(1, mtWarning, 'Could not resolve "%s" (%s)', [s, Item.QualifiedName]);
                          t := t + CodeString(ConvertString(s));
                        end;
                    end else begin
                      Offs1 := Run;
                      if Item.DescriptionFindTag(d, 'CODE', Offs1,
                        Offs2, Offs3) then begin
                        Item.DescriptionGetTag(d, False, Offs1,
                          Offs2, Offs3, s);
                        t := t + ConvertString(Copy(d, Run, Offs1 -
                          Run));
                        Run := Offs3 + 1;
                        t := t + CodeString(ConvertString(s));
                      end else begin
                        Inc(Run);
                        if Assigned(Item.MyUnit) then begin
                          DoMessage(2, mtWarning,
                            'Found non-link tag when expanding descriptions of "' +
                            Item.Name + '" in unit ' + Item.MyUnit.Name,
                            [])
                        end else begin
                          DoMessage(2, mtWarning,
                            'Found non-link tag when expanding descriptions of "' +
                            Item.Name + '"', []);
                          t := t + 'WARNING: @';
                        end;
                      end;
                    end;
                  end;
    end
    else begin
      if (d[Run] in [#9{, #13, #10}]) then d[Run] := ' ';   // GSk: Removed CR and LF
      t := t + ConvertChar(d[Run]);
      Inc(Run);
    end;
  until (Run > l);

  d := t;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.ExpandDescriptions;

{ expands Description and DetailedDescription of Item }

  procedure ExpandItem(Item: TPasItem);
  var
    i: Integer;
    s: string;
  begin
    if Item = nil then Exit;

    if IsStrEmptyA(Item.Description) then
      Item.Description := ''
    else begin
      s := Item.Description;
      TrimCompress(s);
      Item.Description := s;
    end;
    if IsStrEmptyA(Item.DetailedDescription) then
      Item.DetailedDescription := ''
    else begin
      s := Item.DetailedDescription;
      TrimCompress(s);
      Item.DetailedDescription := s;
    end;

    if (not ExpandDescription(Item, Item.FDescription)) or
      (not ExpandDescription(Item, Item.FDetailedDescription)) then begin
      DoMessage(2, mtWarning, 'Could not expand description from ' +
        Item.Name, []);
    end;

    if Item is TPasEnum then begin
      for i := 0 to TPasEnum(Item).Members.Count-1 do begin
        ExpandItem(TPasItem(TPasEnum(Item).Members.PasItemAt[i]));
      end;
    end;
  end;

  { for all items in collection C, expands descriptions }

  procedure ExpandCollection(c: TPasItems);
  var
    i: Integer;
    p: TPasItem;
    {T: PText;}
  begin
    if ObjectVectorIsNilOrEmpty(c) then Exit;
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

  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  for i := 0 to Units.Count - 1 do begin
    U := Units.UnitAt[i];
    if U.WasDeserialized then continue;
    ExpandItem(U);
    ExpandCollection(U.Constants);
    ExpandCollection(U.Variables);
    ExpandCollection(U.Types);
    ExpandCollection(U.FuncsProcs);

    if not ObjectVectorIsNilOrEmpty(U.CIOs) then
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

function TDocGenerator.ExtractEmailAddress(s: string; var S1, S2,
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

  if ObjectVectorIsNilOrEmpty(Units) then Exit;
  
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
    CIO_PACKEDRECORD: Result := 'packed record'; // TODO
  else
    Result := '';
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.LoadDescriptionFile(n: string);
var
  f           : TStream;
  ItemName    : string;
  Description : string;
  i           : Integer;
  s           : string;
const
  IdentChars  = ['A'..'Z', 'a'..'z', '_', '.', '0'..'9'];
begin
  ItemName := '';
  if n = '' then Exit;
  try
    f := TFileStream.Create(n, fmOpenRead);
  
    Assert(Assigned(f));
  
    try
      while f.Position < f.Size do begin
        s := StreamReadLine(f);
        if s[1] = '#' then begin
          i := 2;
          while s[i] in [' ', #9] do Inc(i);
          { Make sure we read a valid name - the user might have used # in his
            description. }
          if s[i] in IdentChars then begin
            if ItemName <> '' then StoreDescription(ItemName, Description);
            { Read item name and beginning of the description }
            ItemName := '';
            repeat
              ItemName := ItemName + s[i];
              Inc(i);
            until not (s[i] in IdentChars);
            while s[i] in [' ', #9] do Inc(i);
            Description := Copy(s, i, MaxInt);
            Continue;
          end;
        end;
        Description := Description + s;
      end;
      
      if ItemName = '' then
        DoMessage(2, mtWarning, 'No descriptions read from "%s" -- invalid or empty file', [n])
      else
        StoreDescription(ItemName, Description);
    finally
      f.Free;
    end;
  except
    DoError('Could not open description file "%s".', [n], 0);
  end;
end; {TDocGenerator.LoadDescriptionFile}

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
  FoundItem := nil;
  if Assigned(Item) then begin
    FoundItem := Item.FindName(S1, S2, S3, n);
  end;

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
    Result := '';

  if FullLink then begin
    if (S3 <> '') and (result <> '') then begin
      FoundItem := FindGlobal(S1, S2, '', 1);
      Result := CreateReferencedLink(FoundItem.Name,FoundItem.FullLink) + '.' + Result;
    end;

    if (S2 <> '') and (result <> '') then begin
      FoundItem := FindGlobal(S1, '', '', 0);
      Result := CreateReferencedLink(FoundItem.Name,FoundItem.FullLink) + '.' + Result;
    end;
  end;
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

procedure TDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
var
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    WriteCIO(HL, TPasCio(c.PasItemAt[i]));
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trSummaryCio], 'Classes', c);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteConstants(HL: integer; c: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trConstants], 'Constants', c);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteDescription(HL: integer; const Heading: string;
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
  WriteDirect(d);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteConverted(const s: string; Newline: boolean);
begin
  WriteDirect(ConvertString(s), Newline);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteDirect(const t: string; Newline: boolean);
begin
  if length(t) > 0 then
    CurrentStream.WriteBuffer(t[1], Length(t));
  if Newline then
    StreamUtils.WriteLine(CurrentStream, '');
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteTypes(const HL: integer; const t: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trTypes], 'Types', t);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteUnits(const HL: integer);
var
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(Units) then Exit;
  for i := 0 to Units.Count - 1 do begin
    WriteUnit(HL, Units.UnitAt[i]);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.WriteVariables(const HL: integer; const V: TPasItems);
begin
  WriteItems(HL, FLanguage.Translation[trVariables], 'Variables', V);
end;

{ ---------------------------------------------------------------------------- }

procedure TDocGenerator.DoError(const AMessage: string; const AArguments:
  array of const; const AExitCode: Word);
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

constructor TDocGenerator.Create(AOwner: TComponent);
begin
  inherited;
  FClassHierarchy := nil;
  FNoGeneratorInfo := False;
  FLanguage := TPasDocLanguages.Create;
  FAbbreviations := TStringList.Create;
  FAbbreviations.Duplicates := dupIgnore;
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
  FAbbreviations.Free;
  FCurrentStream.Free;
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
  if OutputGraphVizUses then WriteGVUses;
  if OutputGraphVizClassHierarchy then WriteGVClasses;
end;

procedure TDocGenerator.SetLanguage(const Value: TLanguageID);
begin
  FLanguage.Language := Value;
end;

procedure TDocGenerator.SetDestDir(const Value: string);
begin
  if Value <> '' then begin
    FDestDir := IncludeTrailingPathDelimiter(Value);
  end else begin
    FDestDir := '';
  end;
end;

function TDocGenerator.GetLanguage: TLanguageID;
begin
  Result := FLanguage.Language;
end;

procedure TDocGenerator.WriteGVClasses;
var
  LNode: TPasItemNode;

begin
  CreateClassHierarchy;
  LNode := FClassHierarchy.FirstItem;
  if Assigned(LNode) then begin
    CreateStream(OverviewFilenames[9]+'.dot', True);
    WriteConverted('DiGraph Classes {', true);
    while Assigned(LNode) do begin
      if Assigned(LNode.Parent) then begin
        if Length(LNode.Parent.Name) > 0 then begin
          WriteConverted('  '+LNode.Name + ' -> '+LNode.Parent.Name, true);
        end;
      end;
      LNode := FClassHierarchy.NextItem(LNode);
    end;

    WriteConverted('}', true);
    CloseStream;
  end;
end;

procedure TDocGenerator.WriteGVUses;
var
  i, j: Integer;
  U: TPasUnit;
begin
  if not ObjectVectorIsNilOrEmpty(FUnits) then begin
    CreateStream(OverviewFilenames[8]+'.dot', True);
    WriteConverted('DiGraph Uses {', true);
    for i := 0 to FUnits.Count-1 do begin
      if FUnits.PasItemAt[i] is TPasUnit then begin
        U := TPasUnit(FUnits.PasItemAt[i]);
        if not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
          for j := 0 to U.UsesUnits.Count-1 do begin
            WriteConverted('  '+U.Name+' -> '+U.UsesUnits[j], true);
          end;
        end;
      end;
    end;
    WriteConverted('}', true);
    CloseStream;
  end;
end;

procedure TDocGenerator.SetAbbreviations(const Value: TStringList);
begin
  FAbbreviations.Assign(Value);
end;

procedure TDocGenerator.ParseAbbreviationsFile(const AFileName: string);
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
              FAbbreviations.Values[lname] := value;
            end;
          end;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TDocGenerator.ExtractLink(s: string; var S1, S2,
  Link: string): Boolean;
const
  AlphaNum      = ['A'..'Z', 'a'..'z', '0'..'9'];
  FullLinkChars = AlphaNum + ['_', '%', '/', '#', '~', '@'];
  HalfLinkChars = ['.', ',', '-', ':', ';', '?'];
var
  p, i: Integer;
  scheme, url: string;
begin
  Result := False;
  p := Pos('://', s);
  if p > 0 then begin
    i := p-1;
    while (i>0) and (s[i] in AlphaNum) do Dec(i); // find beginning of scheme
    scheme := Copy(s, i+1, p-i+2);
    S1 := Copy(s, 1, i);
    i := p+2;
    while (i<=length(s)) and (s[i] in FullLinkChars + HalfLinkChars) do Inc(i);
    Dec(i);
    while (s[i] in HalfLinkChars) do Dec(i);
    Inc(i);
    S2 := Copy(s, i, MaxInt);
    url := Copy(s, p+3, i - p-3);
    link := scheme + url;
    Result := True; 
  end;
end;

procedure TDocGenerator.CheckString(const AString: string;
  const AErrors: TObjectVector);
var
  s: string;
  p, p2: Integer;
  LError: TSpellingError;
begin
  AErrors.Clear;
  if FCheckSpelling and FSpellCheckStarted then begin
    s := StringReplace(AString, #10, ' ', [rfReplaceAll]);
    s := StringReplace(AString, #13, ' ', [rfReplaceAll]);
    if Length(FAspellMode) > 0 then begin
      PasDoc_RunHelp.WriteLine('-', FAspellPipe);
      PasDoc_RunHelp.WriteLine('+'+FAspellMode, FAspellPipe);
    end;
    PasDoc_RunHelp.WriteLine('^'+s, FAspellPipe);
    s := ReadLine(FAspellPipe);
    while Length(s) > 0 do begin
      case s[1] of
        '*': continue; // no error
        '#': begin
               LError := TSpellingError.Create; 
               s := copy(s, 3, MaxInt); // get rid of '# '
               p := Pos(' ', s);
               LError.Word := copy(s, 1, p-1); // get word
               LError.Suggestions := '';
               s := copy(s, p+1, MaxInt);
               LError.Offset := StrToIntDef(s, 0)-1;
               DoMessage(2, mtWarning, 'possible spelling error for word "%s"', [LError.Word]);
               AErrors.Insert(LError);
             end;
        '&': begin
               LError := TSpellingError.Create; 
               s := copy(s, 3, MaxInt); // get rid of '& '
               p := Pos(' ', s);
               LError.Word := copy(s, 1, p-1); // get word
               s := copy(s, p+1, MaxInt);
               p := Pos(' ', s);
               s := copy(s, p+1, MaxInt);
               p2 := Pos(':', s);
               LError.Suggestions := Copy(s, Pos(':', s)+2, MaxInt);
               SetLength(s, p2-1);
               LError.Offset := StrToIntDef(s, 0)-1;
               DoMessage(2, mtWarning, 'possible spelling error for word "%s"', [LError.Word]);
               AErrors.Insert(LError);
             end;
      end;
      s := ReadLine(FAspellPipe);
    end;
  end;
end;

procedure TDocGenerator.EndSpellChecking;
begin
  if FCheckSpelling and FSpellCheckStarted then begin
    CloseProgram(FAspellPipe);
  end;
end;

procedure TDocGenerator.StartSpellChecking(const AMode: string);
var
  s: string;
  L: TStringList;
  i: Integer;
begin
  FSpellCheckStarted := False;
  if FCheckSpelling then begin
    try
      FAspellMode := AMode;
      if AMode <> '' then begin
        FAspellPipe := RunProgram('/usr/bin/aspell', '-a --lang='+FAspellLanguage+' --mode='+AMode);
      end else begin
        FAspellPipe := RunProgram('/usr/bin/aspell', '-a --lang='+FAspellLanguage);
      end;
      FSpellCheckStarted := True;
    except
      DoMessage(1, mtWarning, 'spell checking is not supported yet, disabling', []);
      FSpellCheckStarted := False;
    end;
    s := ReadLine(FAspellPipe);
    if copy(s,1,4) <> '@(#)' then begin
      CloseProgram(FAspellPipe);
      FSpellCheckStarted := False;
      DoError('Could not initialize aspell: "%s"', [s], 1);
    end else begin
      PasDoc_RunHelp.WriteLine('!', FAspellPipe);
      if Length(IgnoreWordsFile)>0 then begin
        L := TStringList.Create;
        try
          L.LoadFromFile(IgnoreWordsFile);
          for i := L.Count-1 downto 0 do begin
            PasDoc_RunHelp.WriteLine('@'+L[i], FAspellPipe);
          end;
        except
          DoMessage(1, mtWarning, 'Could not load ignore words file %s', [IgnoreWordsFile]);
        end;
        L.Free;
      end;
    end;
  end;
end;

function TDocGenerator.ParameterString(const ParamType,
  Param: string): string;
begin
  Result := #10 + ParamType + ' ' + Param;
end;

procedure TDocGenerator.WriteDirect(const t: string);
begin
  WriteDirect(t, false);
end;

procedure TDocGenerator.WriteConverted(const s: string);
begin
  WriteConverted(s, false);
end;

function TDocGenerator.LongCode(const Desc: string; Len: integer;
  var CurPos: integer): string;
var
  CharPos: integer;
  ClosingCharacter: Char;
  FoundEnd: boolean;
begin
  CharPos := CurPos;
  if (CharPos+1 > Len) or (Desc[CharPos] <> '(') then
  begin
    result := '@LONGCODE';
  end
  else
  begin
    ClosingCharacter := Desc[CharPos+1];
    CharPos := CharPos + 1;
    FoundEnd := False;
    while (CharPos <= Len) do
    begin
      Inc(CharPos);
      if Desc[CharPos] = ClosingCharacter then
      begin
        if (CharPos + 1 <= Len) and (Desc[CharPos+1] = ')') then
        begin
          FoundEnd := True;
          break;
        end;
      end;
    end;
    if FoundEnd then
    begin
      result := FormatPascalCode(Copy(Desc, CurPos + 2, CharPos - CurPos-2));
      CurPos := CharPos + 2;
    end
    else
    begin
      result := '@LONGCODE';
    end;
  end;
end;

function TDocGenerator.HtmlString(const Desc: string; Len: integer;
  var CurPos: integer): string;
var
  ParenthesesLevel: integer;
  CharPos: integer;
begin
  CharPos := CurPos;
  if (CharPos > Len) or (Desc[CharPos] <> '(') then
  begin
    result := '@HTML';
  end
  else
  begin
    ParenthesesLevel := 1;
    while (ParenthesesLevel <> 0) and (CharPos <= Len) do
    begin
      Inc(CharPos);
      if Desc[CharPos] = '(' then
      begin
        Inc(ParenthesesLevel)
      end
      else if Desc[CharPos] = ')' then
      begin
        Dec(ParenthesesLevel)
      end;
    end;
    if ParenthesesLevel = 0 then
    begin
      result := '';
      CurPos := CharPos + 1;
    end
    else
    begin
      result := '@HTML';
    end;
  end;
end;

function TDocGenerator.FormatPascalCode(const Line: string): string;
var
  CharIndex: integer;
  CodeType: TCodeType;
  CommentBegining: integer;
  StringBeginning: integer;
  CodeBeginning: integer;
  EndOfCode: boolean;
  WhiteSpaceBeginning: integer;
const
  Separators = [' ', ',', '(', ')', #9, #10, #13, ';', '[', ']', '{', '}',
    '''', ':', '<', '>', '=', '+', '-', '*', '/', '@', '.'];
  LineEnd = [#10, #13];
  AlphaNumeric = ['0'..'9', 'a'..'z', 'A'..'Z'];
  function TestCommentStart: boolean;
  begin
    result := False;
    if Line[CharIndex] = '(' then
    begin
      if (CharIndex < Length(Line)) and (Line[CharIndex + 1] = '*') then
      begin
        CodeType := ctParenComment;
        result := True;
      end
    end
    else if Line[CharIndex] = '{' then
    begin
      if (CharIndex < Length(Line)) and (Line[CharIndex + 1] = '$') then
      begin
        CodeType := ctCompilerComment;
      end
      else
      begin
        CodeType := ctBracketComment;
      end;
      result := True;
    end
    else if Line[CharIndex] = '/' then
    begin
      if (CharIndex < Length(Line)) and (Line[CharIndex + 1] = '/') then
      begin
        CodeType := ctSlashComment;
        result := True;
      end
    end;
    if result then
    begin
      CommentBegining := CharIndex;
    end;
  end;
  function TestStringBeginning: boolean;
  begin
    result := False;
    if Line[CharIndex] = '''' then
    begin
      if CodeType <> ctChar then
      begin
        StringBeginning := CharIndex;
      end;
      CodeType := ctString;
      result := True;
    end
  end;
begin
  CommentBegining := 1;
  StringBeginning := 1;
  result := '';
  CodeType := ctWhiteSpace;
  WhiteSpaceBeginning := 1;
  CodeBeginning := 1;
  for CharIndex := 1 to Length(Line) do
  begin
    case CodeType of
      ctWhiteSpace:
        begin
          EndOfCode := False;
          if TestStringBeginning then
          begin
            EndOfCode := True;
          end
          else if Line[CharIndex] = '#' then
          begin
            StringBeginning := CharIndex;
            CodeType := ctChar;
            EndOfCode := True;
          end
          else if TestCommentStart then
          begin
            EndOfCode := True;
          end
          else if Line[CharIndex] in AlphaNumeric then
          begin
            CodeType := ctCode;
            CodeBeginning := CharIndex;
            EndOfCode := True;
          end;
          if EndOfCode then
          begin
            result := result + (Copy(Line, WhiteSpaceBeginning, CharIndex -
              WhiteSpaceBeginning));
          end;
        end;
      ctString:
        begin
          if Line[CharIndex] = '''' then
          begin
            if (CharIndex = Length(Line)) or (Line[CharIndex + 1] <> '''') then
            begin
              CodeType := ctEndString;
              result := result + FormatString(Copy(Line, StringBeginning,
                CharIndex - StringBeginning + 1));
            end;
          end;
        end;
      ctCode:
        begin
          EndOfCode := False;
          if TestStringBeginning then
          begin
            EndOfCode := True;
          end
          else if Line[CharIndex] = '#' then
          begin
            EndOfCode := True;
            CodeType := ctChar;
            StringBeginning := CharIndex;
          end
          else if TestCommentStart then
          begin
            EndOfCode := True;
          end
          else if not (Line[CharIndex] in AlphaNumeric) then
          begin
            EndOfCode := True;
            CodeType := ctWhiteSpace;
            WhiteSpaceBeginning := CharIndex;
          end;
          if EndOfCode then
          begin
            result := result + FormatCode(Copy(Line, CodeBeginning, CharIndex -
              CodeBeginning));
          end;
        end;
      ctEndString:
        begin
          if Line[CharIndex] = '#' then
          begin
            CodeType := ctChar;
          end
          else if TestCommentStart then
          begin
            // do nothing
          end
          else if Line[CharIndex] in AlphaNumeric then
          begin
            CodeType := ctCode;
            CodeBeginning := CharIndex;
          end
          else
          begin
            CodeType := ctWhiteSpace;
            WhiteSpaceBeginning := CharIndex;
          end;
        end;
      ctChar:
        begin
          if Line[CharIndex] = '''' then
          begin
            CodeType := ctString;
          end
          else if TestCommentStart then
          begin
            // do nothing
          end
          else if Line[CharIndex] in Separators then
          begin
            result := result + FormatString(Copy(Line, StringBeginning,
              CharIndex - StringBeginning));
            CodeType := ctWhiteSpace;
            WhiteSpaceBeginning := CharIndex;
          end;
        end;
      ctParenComment:
        begin
          if Line[CharIndex] = ')' then
          begin
            if (CharIndex > 1) and (Line[CharIndex - 1] = '*') then
            begin
              CodeType := ctEndComment;
              result := result + FormatComment(Copy(Line, CommentBegining,
                CharIndex - CommentBegining + 1));
            end;
          end;
        end;
      ctBracketComment:
        begin
          if Line[CharIndex] = '}' then
          begin
            CodeType := ctEndComment;
            result := result + FormatComment(Copy(Line, CommentBegining,
              CharIndex - CommentBegining + 1));
          end;
        end;
      ctCompilerComment:
        begin
          if Line[CharIndex] = '}' then
          begin
            CodeType := ctEndComment;
            result := result + FormatCompilerComment(Copy(Line, CommentBegining,
              CharIndex - CommentBegining + 1));
          end;
        end;
      ctSlashComment:
        begin
          if Line[CharIndex] in LineEnd then
          begin
            CodeType := ctWhiteSpace;
            result := result + FormatComment(Copy(Line, CommentBegining,
              CharIndex - CommentBegining));
            WhiteSpaceBeginning := CharIndex;
          end;
        end;
      ctEndComment:
        begin
          if TestCommentStart then
          begin
            // do nothing
          end
          else if Line[CharIndex] in Separators then
          begin
            CodeType := ctWhiteSpace;
            WhiteSpaceBeginning := CharIndex;
          end
          else if Line[CharIndex] in AlphaNumeric then
          begin
            CodeType := ctCode;
            CodeBeginning := CharIndex;
          end;
        end;
    else
      Assert(False);
    end;
  end;
  CharIndex := Length(Line);
  case CodeType of
    ctWhiteSpace:
      begin
        result := result + (Copy(Line, WhiteSpaceBeginning, CharIndex -
          WhiteSpaceBeginning));
      end;
    ctString:
      begin
      end;
    ctCode:
      begin
        result := result + FormatCode(Copy(Line, CodeBeginning, CharIndex -
          CodeBeginning));
      end;
    ctEndString:
      begin
      end;
    ctChar:
      begin
        result := result + FormatString(Copy(Line, StringBeginning,
          CharIndex - StringBeginning));
      end;
    ctParenComment:
      begin
        result := result + FormatComment(Copy(Line, CommentBegining,
          CharIndex - CommentBegining + 1));
      end;
    ctBracketComment:
      begin
        result := result + FormatComment(Copy(Line, CommentBegining,
          CharIndex - CommentBegining + 1));
      end;
    ctCompilerComment:
      begin
        result := result + FormatCompilerComment(Copy(Line, CommentBegining,
          CharIndex - CommentBegining + 1));
      end;
    ctSlashComment:
      begin
      end;
    ctEndComment:
      begin
        result := result + FormatComment(Copy(Line, CommentBegining,
          CharIndex - CommentBegining + 1));
      end;
  else Assert(False);
  end;
end;

function TDocGenerator.FormatCode(AString: string): string;
begin
  if ReservedWords.IndexOf(LowerCase(AString)) >= 0 then
  begin
    Result := FormatKeyWord(AString);
  end
  else
  begin
    result := AString;
  end;
end;

function TDocGenerator.FormatComment(AString: string): string;
begin
  result := AString;
end;

function TDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := AString;
end;

function TDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := AString;
end;

function TDocGenerator.FormatString(AString: string): string;
begin
  result := AString;
end;

initialization
  ReservedWords := TStringList.Create;

  // construct the list of reserved words.  These will be displayed
  // in bold text.
  ReservedWords.Add('and');
  ReservedWords.Add('array');
  ReservedWords.Add('as');
  ReservedWords.Add('asm');
  ReservedWords.Add('begin');
  ReservedWords.Add('case');
  ReservedWords.Add('class');
  ReservedWords.Add('const');
  ReservedWords.Add('constructor');
  ReservedWords.Add('destructor');
  ReservedWords.Add('dispinterface');
  ReservedWords.Add('div');
  ReservedWords.Add('do');
  ReservedWords.Add('downto');
  ReservedWords.Add('else');
  ReservedWords.Add('end');
  ReservedWords.Add('except');
  ReservedWords.Add('exports');
  ReservedWords.Add('file');
  ReservedWords.Add('finalization');
  ReservedWords.Add('finally');
  ReservedWords.Add('for');
  ReservedWords.Add('function');
  ReservedWords.Add('goto');
  ReservedWords.Add('if');
  ReservedWords.Add('implementation');
  ReservedWords.Add('in');
  ReservedWords.Add('inherited');
  ReservedWords.Add('initialization');
  ReservedWords.Add('inline');
  ReservedWords.Add('interface');
  ReservedWords.Add('is');
  ReservedWords.Add('label');
  ReservedWords.Add('library');
  ReservedWords.Add('mod');
  ReservedWords.Add('nil');
  ReservedWords.Add('not');
  ReservedWords.Add('object');
  ReservedWords.Add('of');
  ReservedWords.Add('or');
  ReservedWords.Add('out');
  ReservedWords.Add('packed');
  ReservedWords.Add('procedure');
  ReservedWords.Add('program');
  ReservedWords.Add('property');
  ReservedWords.Add('raise');
  ReservedWords.Add('record');
  ReservedWords.Add('repeat');
  ReservedWords.Add('resourcestring');
  ReservedWords.Add('set');
  ReservedWords.Add('shl');
  ReservedWords.Add('shr');
  ReservedWords.Add('string');
  ReservedWords.Add('then');
  ReservedWords.Add('threadvar');
  ReservedWords.Add('to');
  ReservedWords.Add('try');
  ReservedWords.Add('type');
  ReservedWords.Add('unit');
  ReservedWords.Add('until');
  ReservedWords.Add('uses');
  ReservedWords.Add('var');
  ReservedWords.Add('while');
  ReservedWords.Add('with');
  ReservedWords.Add('xor');
  ReservedWords.Sorted := True;

finalization
  ReservedWords.Free;

end.
