{ @abstract(Provides Latex document generator object.)
  @cvs($Date$)

  Implements an object to generate latex documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenLatex;

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  PasDoc_Types,
  Classes;

type
  { @abstract(generates latex documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in laTex (HyperText Markup Language) format.
    This type of output is well suited to be read with a web browser at the
    computer, as a reference manual that does not have to be printed.
    For printed output, use @link(Tex.TTexDocGenerator). }
  TTexDocGenerator = class(TDocGenerator)
  protected
    FNumericFilenames: boolean;
    FWriteUses: boolean;
    FLinkCount: Integer;
    FFooter: string;
    FHeader: string;
    FOddTableRow: Integer;
    { number of cells (= columns) per table row }
    NumCells: Integer;
    { number of cells we've already written in current table row }
    CellCounter: LongInt;
    
    { Contains Name of a file to read HtmlHelp Contents from.
      If empty, create default contents file. }
    FContentsFile: string;
    { If True, generate Html Help project files. }
    FHtmlHelp: Boolean;
    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: integer; Authors: TStringVector);
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string; const
      ItemLink: string);
    { Writes an empty table cell, '&nbsp;'. }
    procedure WriteEmptyCell;

    { Writes the end of an HTML anchor, '</A>'. }
    procedure WriteEndOfAnchor;
    procedure WriteEndOfDocument;
    { Finishes an HTML paragraph element by writing a closing P tag. }
    procedure WriteEndOfParagraph;
    { Finishes an HTML table cell by writing a closing TD tag. }
    procedure WriteEndOfTableCell;
    { Finishes an HTML table by writing a closing TABLE tag. }
    procedure WriteEndOfTable;
    { Finishes an HTML table row by writing a closing TR tag. }
    procedure WriteEndOfTableRow;
    procedure WriteFields(const Order: integer; const Fields: TPasItems);
    procedure WriteFooter;
    { Writes a Hireachy list - this is more useful than the simple class list }
    procedure WriteHierarchy;
    procedure WriteItemDescription(const AItem: TPasItem);
    { Writes the Item's DetailedDescription. If the Item also has Discription
      (extracted from @@abstract), this is written to a separate paragraph
      in front of the DetailedDescription. }
    procedure WriteItemDetailedDescription(const AItem: TPasItem);
    procedure WriteOverviewFiles;
    procedure WriteParagraph(HL: integer; s: string; t: string);
    procedure WritePropertiesSummary(HL: integer; p: TPasProperties);

    { Writes an opening A element, including a name attribute given by the
      argument. }
    procedure WriteStartOfAnchor(const AName: string);
    procedure WriteStartOfDocument(AName: string);

    procedure WriteStartOfLink(const href: string); overload;
    procedure WriteStartOfLink(const href, css: string); overload;
    procedure WriteStartOfLink(const href, css, Target: string); overload;

    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph;

    procedure WriteStartOfTableCell; overload;
    procedure WriteStartOfTableCell(const css: string); overload;
    procedure WriteStartOfTableCell(const Params, css: string); overload;
    
    procedure WriteStartOfTable1Column(t: string);
    procedure WriteStartOfTable2Columns(t1, t2: string);
    procedure WriteStartOfTable3Columns(t1, t2, T3: string);
    procedure WriteStartOfTableRow(const CssClass: string);
    { Writes the topic files for Html Help Generation }
    procedure WriteHtmlHelpProject;

    { Creates an output stream that lists up all units and short descriptions. }
    procedure WriteUnitOverviewFile;
    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);

    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertString(const s: string): string; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
    procedure WriteUnitUses(const HL: integer; U: TPasUnit);
    procedure WriteUnitDescription(HL: integer; U: TPasUnit); override;
    procedure WriteProperties(HL: integer; const p: TPasProperties); override;

    procedure WriteSpellChecked(const AString: string);

    procedure WriteWithURLs(s: string);
    { Return the text within the parentheses after the @HTML field.  The user
      is required to provided correctly formatted html text within the
      parentheses  and to have matching parentheses.  If no parentheses are found
      after @HTML, the string '@HTML' is returned instead. }
    function HtmlString(const Desc: string; Len: integer; var CurPos: integer): string; override;

    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; override;
    { Returns a link to an anchor within a document. HTML simply concatenates
      the strings with a "#" character between them. }
    function CreateLink(const Item: TPasItem): string; override;
    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio); override;
    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems); override;
    procedure WriteCIOSummary(HL: integer; c: TPasItems); override;
    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string); override;
    procedure WriteStartOfCode; override;
    procedure WriteItems(HL: integer; Heading: string; const Anchor: string;
      const i: TPasItems); override;
    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(Level: integer; const s: string); override;

    procedure WriteEndOfCode; override;
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument Methods is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the FuncsProcs argument. }
    procedure WriteFuncsProcs(const HL: integer; const Methods: Boolean; const
      FuncsProcs: TPasMethods); override;

    { output all the necessary images }
    procedure WriteBinaryFiles;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    procedure WriteImage(const src, alt, css: string);
    procedure WriteLink(const href, caption, css: string); overload;
    procedure WriteLink(const href, caption, css, target: string); overload;
    procedure WriteAnchor(const AName: string); overload;
    procedure WriteAnchor(const AName, Caption: string); overload;
    procedure WriteEndOfLink;
  public
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure LoadFooterFromFile(const AFileName: string);
    procedure LoadHeaderFromFile(const AFileName: string);
    procedure BuildLinks; override;

    function EscapeURL(const AString: string): string; virtual;
  published
    property HtmlHelp: boolean read FHtmlHelp write FHtmlHelp;
    property ContentsFile: string read FContentsFile write FContentsFile;
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses;
  end;

{$INCLUDE automated.inc}
{$INCLUDE private.inc}
{$INCLUDE public.inc}
{$INCLUDE published.inc}
{$INCLUDE protected.inc}

const
  { HTML table padding inside each cell. }
  HTML_TABLE_CELLPADNG = '4';
  { HTML table spacing between cells. }
  HTML_TABLE_CELLSPACING = '2';

implementation

uses
  SysUtils,
  PasDoc,
  ObjectVector,
  StreamUtils,
  Utils,
  PasDoc_Tokenizer,
  PasDoc_HierarchyTree;

{ HTML things to be customized:
    - standard background color (white)
    - background color for table headings and overview list at the top of each file (light gray)
    - background color for normal table cells (light gray, slightly lighter than the above)
    - standard foreground color (black)
    - unused link color (blue)
    - used link color (purple)
    - link color while being clicked on (red)
    - normal font (Times Roman)
    - heading font (Helvetica)
    - code font (Courier New) }

function TTexDocGenerator.HtmlString(const Desc: string; Len: integer; var CurPos: integer): string;
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
      result := Copy(Desc, CurPos + 1, CharPos - CurPos - 1);
      CurPos := CharPos + 1;
    end
    else
    begin
      result := '@HTML';
    end;
  end;
end;

function TTexDocGenerator.CodeString(const s: string): string;
begin
  Result := '\textt{' + s + '}';
end;

function TTexDocGenerator.CreateLink(const Item: TPasItem): string;
  function NewLink(const AFullName: string): string;
  begin
    if NumericFilenames then begin
      Result := Format('%.8d', [FLinkCount]) + GetFileExtension;
      Inc(FLinkCount);
    end else begin
      Result := AFullName + GetFileExtension;
    end;
  end;

begin
  Result := '';
  if (not Assigned(Item)) then Exit;
  if Assigned(Item.MyUnit) then begin
    if Assigned(Item.MyObject) then begin
      { it's a method, a field or a property - only those have MyObject initialized }
      Result := Item.MyObject.FullLink + '#' + Item.Name;
    end else begin
      if Item.ClassType = TPasCio then begin
        { it's an object / a class }
        Result := NewLink(Item.MyUnit.Name + '.' + Item.Name);
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := Item.MyUnit.FullLink + '#' + Item.Name;
      end;
    end;
  end else begin
    { it's a unit - only units don't have a MyUnit pointer }
    Result := NewLink(Item.Name);
  end;
end;

function TTexDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result := '<a class="normal" href="' + EscapeURL(Link) + '">' + ItemName + '</a>';
end;

function TTexDocGenerator.GetFileExtension: string;
begin
  Result := '.tex';
end;

procedure TTexDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  WriteDirect('% '+FLanguage.Translation[trGeneratedBy] + ' ');
  WriteDirect(PASDOC_HOMEPAGE+ PASDOC_NAME_AND_VERSION);
  WriteDirect(' ' + FLanguage.Translation[trOnDateTime] + ' ' +
    FormatDateTime('ddd dd/ mmm yyyy hh:mm:ss', Now));
  WriteDirect('', true);
end;

procedure TTexDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  EmailAddress: string;
begin
  if StringVectorIsNilOrEmpty(Authors) then Exit;

  if (Authors.Count = 1) then
    WriteHeading(HL, FLanguage.Translation[trAuthor])
  else
    WriteHeading(HL, FLanguage.Translation[trAuthors]);

  for i := 0 to Authors.Count - 1 do begin
    s := Authors[i];
    WriteStartOfParagraph;

    if ExtractEmailAddress(s, S1, S2, EmailAddress) then begin
      WriteConverted(S1);
      WriteConverted(S2);
    end else begin
      WriteConverted(s);
    end;

    WriteEndOfParagraph;
  end;
end;

procedure TTexDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);
type
  TSections = (dsDescription, dsHierarchy, dsFields, dsMethods, dsProperties);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
  TCIONames = array[TCIOType] of string;
const
  SectionAnchors: TSectionAnchors = (
    '@Description',
    '@Hierarchy',
    '@Fields',
    '@Methods',
    '@Properties');
  CIO_NAMES: TCIONames = (
    'class',
    'dispinterface',
    'interface',
    'object',
    'record',
    'packed record');
var
  i: Integer;
  s: string;
  Item: TPasItem;
  TheLink: string;
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;
  Section: TSections;
begin
  if not Assigned(CIO) then Exit;

  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsHierarchy] := FLanguage.Translation[trHierarchy];
  SectionHeads[dsFields ]:= FLanguage.Translation[trFields];
  SectionHeads[dsMethods ]:= FLanguage.Translation[trMethods];
  SectionHeads[dsProperties ]:= FLanguage.Translation[trProperties];

  SectionsAvailable := [dsDescription];
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then
    Include(SectionsAvailable, dsHierarchy);
  if not ObjectVectorIsNilOrEmpty(CIO.Fields) then
    Include(SectionsAvailable, dsFields);
  if not ObjectVectorIsNilOrEmpty(CIO.Methods) then
    Include(SectionsAvailable, dsMethods);
  if not ObjectVectorIsNilOrEmpty(CIO.Properties) then
    Include(SectionsAvailable, dsProperties);

  CIO.SortPasItems;
  s := GetCIOTypeName(CIO.MyType) + ' ' + CIO.Name;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);

  WriteAnchor(CIO.Name);
  WriteHeading(HL, s);

  WriteDirect('<table class="sections"><tr>', true);
  for Section := Low(TSections) to High(TSections) do
    begin
      WriteDirect('<td>');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section], 'section')
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirect('</td>');
    end;
  WriteDirect('</tr></table>', true);

  WriteAnchor(SectionAnchors[dsDescription]);

  { write unit link }
  if Assigned(CIO.MyUnit) then begin
    WriteHeading(HL + 1, FLanguage.Translation[trUnit]);
    WriteLink(CIO.MyUnit.FullLink, ConvertString(CIO.MyUnit.Name), 'bold');
    WriteDirect('<br/>');
  end;

  { write declaration link }
  WriteHeading(HL + 1, FLanguage.Translation[trDeclaration]);
  WriteStartOfParagraph;
  WriteStartOfCode;
  WriteConverted('type ' + CIO.Name + ' = ');
  WriteConverted(CIO_NAMES[CIO.MyType]);

  if not StringVectorIsNilOrEmpty(CIO.Ancestors) then begin
    WriteConverted('(');
    for i := 0 to CIO.Ancestors.Count - 1 do begin
      s := CIO.Ancestors[i];
      TheLink := SearchLink(s, CIO);
      if TheLink <> '' then
        s := TheLink;
      WriteDirect(s);
      if (i <> CIO.Ancestors.Count - 1) then
        WriteConverted(', ');
    end;
    WriteConverted(')');
  end;
  WriteEndOfCode;
  WriteEndOfParagraph;

  { Write Description }
  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(CIO);

  { Write Hierarchy }
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then begin
    WriteAnchor(SectionAnchors[dsHierarchy]);
    WriteHeading(HL + 1, SectionHeads[dsHierarchy]);

    WriteDirect(CIO.Name);
    WriteDirect('&nbsp;&gt; ');
    s := CIO.Ancestors.FirstName;
    Item := SearchItem(s, CIO);
    if Assigned(Item) and (Item is TPasCio) then begin
      repeat
        s := CreateReferencedLink(Item.Name, Item.FullLink);
        WriteDirect(s);

        if not StringVectorIsNilOrEmpty(TPasCio(Item).Ancestors) then begin
          s := TPasCio(Item).Ancestors.FirstName;
          Item := SearchItem(s, Item);

          WriteDirect('&nbsp;&gt; ');
          if (Item <> nil) and (Item is TPasCio) then begin
            Continue;
          end;
        end;
        Break;
      until False;
    end;
    if Item = nil then WriteDirect(s);
  end;

  WriteFields(HL + 1, CIO.Fields);

  WriteFuncsProcs(HL + 1, True, CIO.Methods);

  if (CIO.MyType in [CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE]) then begin
    WritePropertiesSummary(HL + 1, CIO.Properties);
    WriteProperties(HL + 1, CIO.Properties);
  end;

  WriteAuthors(HL + 1, CIO.Authors);
  WriteDates(HL + 1, CIO.Created, CIO.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
end;

procedure TTexDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
var
  i: Integer;
  p: TPasCio;
begin
  if c = nil then Exit;

  for i := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[i]);
    case CreateStream(p.OutputFileName, not p.WasDeserialized) of
      csError: begin
          DoMessage(1, mtError, 'Could not create Class/Interface/Object documentation file.', []);
          Continue;
        end;
      csCreated: begin
          DoMessage(3, mtInformation, 'Creating Class/Interface/Object file for "%s"...', [p.Name]);
          WriteCIO(HL, p);
        end;
      csExisted: begin
          DoMessage(3, mtInformation, 'File for "%s" already existed and data was loaded from cache, skipped.', [p.Name]);
      end;
    end;
  end;
  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  WriteAnchor('@Classes');

  WriteHeading(HL, FLanguage.Translation[trCio]);
  WriteStartOfTable2Columns(FLanguage.Translation[trName], FLanguage.Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteStartOfTableRow('');
    { name of class/interface/object and unit }
    WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
    WriteConverted(GetCIOTypeName(p.MyType));
    WriteDirect('&nbsp;');
    WriteLink(p.FullLink, CodeString(p.Name), 'bold');
    WriteEndOfTableCell;

    { Description of class/interface/object }
    if j = 0 then
      WriteStartOfTableCell('width="100%"', '')
    else
      WriteStartOfTableCell;
    { Write only the description and do not opt for DetailedDescription,
      like WriteItemDescription does. }
    if p.Description <> '' then
      WriteWithURLs(p.Description)
    else
      WriteDirect('&nbsp;');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

procedure TTexDocGenerator.WriteCodeWithLinks(const p: TPasItem; const Code:
  string; const ItemLink: string);
var
  NameFound,SearchForLink: Boolean;
  FoundItem: TPasItem;
  i, j, l: Integer;
  s: string;
  pl: TStandardDirective;
  n, ncstart: Integer;
  S1: string;
  S2: string;
  S3: string;
begin
  WriteStartOfCode;
  i := 1;
  NameFound := false;
  SearchForLink := false;
  l := Length(Code);
  ncstart := i;
  while i <= l do
  begin
    case Code[i] of
      '_', 'A'..'Z', 'a'..'z':
         begin
          WriteConverted(Copy(Code, ncstart, i - ncstart));
          { assemble item }
          j := i;
          repeat
            Inc(i);
          until (i > l) or (not (Code[i] in ['.', '_', '0'..'9', 'A'..'Z', 'a'..'z']));
          s := Copy(Code, j, i - j);

           { Special processing for standard directives. }
          pl := StandardDirectiveByName(s);
          case pl of
            SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
              SD_FAR, SD_FORWARD, SD_NAME, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
              SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL:
              begin
                WriteConverted(s);
                SearchForLink := False;
                ncstart := i;
                Continue;
              end;
            SD_EXTERNAL:
              begin
                WriteConverted(s);
                SearchForLink := False;
                ncstart := i;
                Continue;
              end;
          end;
          if not NameFound and (s = p.Name) then begin
            if ItemLink <> '' then begin
              WriteLink(ItemLink, '\em{' + ConvertString(s) + '}', '');
            end else begin
              WriteDirect('\em{');
              WriteConverted(s);
              WriteDirect('}');
            end;
            NameFound := True;
          end else begin
            { search for item of name  L }
            if SearchForLink and (SplitLink(s, S1, S2, S3, n)) then begin
              FoundItem := p.FindName(S1, S2, S3, n);
              if not Assigned(FoundItem) then
                FoundItem := FindGlobal(S1, S2, S3, n);
            end
            else
              FoundItem := nil;

            if Assigned(FoundItem) then
              WriteLink(FoundItem.FullLink, ConvertString(s), '')
            else begin
              WriteConverted(s);
            end;
          end;
          ncstart := i;
          Continue; // We don't want to miss out on any ':' or ';' for SearchForLink
        end;
      ':',
      '=': SearchForLink := True;
      ';': SearchForLink := False;
      '''': begin
          Inc(i);
          while (i<=l) and (Code[i] <> '''') do Inc(i);
        end;
    end;
    Inc(i);
  end;
  WriteConverted(Copy(Code, ncstart, i - ncstart));
  WriteEndOfCode;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteDates(const HL: integer; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteConverted(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteConverted(LastMod, true);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteDocumentation;
begin
{  StartSpellChecking('sgml');}
  inherited;
  WriteUnits(1);
  WriteHierarchy;
  WriteBinaryFiles;
  WriteOverviewFiles;
  WriteVisibilityLegendFile;
{  EndSpellChecking;}
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteEmptyCell;
begin
end;

procedure TTexDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('\end{document}',true);
end;

procedure TTexDocGenerator.WriteEndOfAnchor;
begin
  WriteDirect('</a>');
end;

procedure TTexDocGenerator.WriteEndOfLink;
begin
  WriteDirect('</a>');
end;

procedure TTexDocGenerator.WriteEndOfCode;
begin
  WriteDirect('}');
end;

procedure TTexDocGenerator.WriteLink(const href, caption, css: string);
begin
  WriteLink(href, caption, css, '');
end;

procedure TTexDocGenerator.WriteLink(const href, caption, css, target: string);
var
  s: string;
begin
  WriteDirect(caption);
end;

procedure TTexDocGenerator.WriteEndOfParagraph;
begin
  WriteDirect('', true);
  WriteDirect('',true);
end;

procedure TTexDocGenerator.WriteEndOfTableCell;
begin
  Inc(CellCounter);
  if (CellCounter < NumCells)
  then WriteDirect(' & ');
end;

procedure TTexDocGenerator.WriteEndOfTable;
begin
 WriteDirect('\end{tabular}',true);
end;

procedure TTexDocGenerator.WriteEndOfTableRow;
begin
   WriteDirect('\\',true);
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFields(const Order: integer; const Fields:
  TPasItems);
var
  j: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(Fields) then Exit;

  WriteAnchor('@Fields');
  WriteHeading(Order, FLanguage.Translation[trFields]);

  WriteDirect('<table class="fields" cellspacing="' +
    HTML_TABLE_CELLSPACING + '" cellpadding="' + HTML_TABLE_CELLPADNG +
    '" width="100%">');
  WriteDirect('<tr class="listheader">');
  WriteDirect('<th>&nbsp;</th><th>');
  WriteConverted(FLanguage.Translation[trName]);
  WriteDirect('</th><th class="listheader">');
  WriteConverted(FLanguage.Translation[trDescription]);
  WriteDirect('</th></tr>', true);

  for j := 0 to Fields.Count - 1 do begin
    Item := Fields.PasItemAt[j];
    WriteStartOfTableRow('');

    WriteVisibilityCell(Item);

    WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
    WriteAnchor(Item.Name);
    { TODO -otwm : This should not only write the name but the full declaration of the field. }
    WriteDirect(CodeString(Item.Name));
    WriteEndOfTableCell;

    if j = 0 then
      WriteStartOfTableCell('width="100%"', '')
    else
      WriteStartOfTableCell;

    WriteItemDetailedDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  WriteDirect('</table>');
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFooter;
begin
  WriteConverted(Footer);
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFuncsProcs(const HL: integer; const Methods:
  Boolean; const FuncsProcs: TPasMethods);

  function ExtractFirstWord(var s: string): string;
  var
    p: integer;
    Len: integer;
  begin
    Result := '';
    Len := Length(s);
    p := 1;
    while (p <= Len) and (s[p] in [' ', #9, #13, #10]) do
      Inc(p);
    while (p <= Len) and not (s[p] in [' ', #9, #13, #10]) do
      begin
        Result := Result + s[p];
        Inc(p);
      end;
    s := Copy(s, p, Length(s));
  end;

  procedure WriteParameter(const ParamName: string; const Desc: string);
  begin
    WriteDirect('<dt class="parameters">', true);
    WriteConverted(ParamName);
    WriteDirect('</dt>', true);
    WriteDirect('<dd class="parameters">', true);
    WriteWithURLs(Desc);
    WriteDirect('</dd>', true);
  end;

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
    List: TStringVector);
  var
    i: integer;
    s: string;
    ParamName: string;
  begin
    if StringVectorIsNilOrEmpty(List) then
      exit;

    WriteHeading(6, Caption);
    WriteDirect('<dl class="parameters">', true);
    for i := 0 to List.Count - 1 do begin
      s := List[i];
      ParamName := ExtractFirstWord(s);
      ExpandDescription(Func, s);
      WriteParameter(ParamName, s);
    end;
    WriteDirect('</dl>', true);
  end;

  procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteHeading(6, 'return value');
    ExpandDescription(Func, ReturnDesc);
    WriteDirect('<p class="return">');
    WriteWithURLs(ReturnDesc);
    WriteDirect('</p>');
  end;

var
  i: Integer;
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;

  if Methods then begin
    WriteHeading(HL, FLanguage.Translation[trMethods]);
  end
  else begin
    WriteHeading(HL, FLanguage.Translation[trFunctionsAndProcedures]);
  end;

  // Sort alphabatically
  FuncsProcs.SortByPasItemName;
  // two passes, in the first (i=0) we write the overview
  // in the second (i=1) we write the descriptions
  WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
  WriteDirect('\begin{description}',true);
  for j := 0 to FuncsProcs.Count - 1 do 
  begin
    p := TPasMethod(FuncsProcs.PasItemAt[j]);
    
    WriteDirect('\item[{\ttfamily ');
    { overview of functions and procedures }
    { Only write visibility for methods of classes and objects. }
    if Methods then 
      WriteVisibilityCell(p);

    s := p.FullLink;
      if Assigned(p.MyUnit) then
         if CompareText(p.MyUnit.FullLink, Copy(s, 1,
            Length(p.MyUnit.FullLink))) = 0 then
            begin
              Delete(s, 1, Length(p.MyUnit.FullLink));
              { remove # character from the link }
              Delete(s, 1,1);
            end;
        
    WriteDirect(p.name);
    WriteDirect(' (\pageref{'+s+'})');
    WriteDirect('}]');

    if p.Description <> '' then
      WriteDirect(p.Description,true)
    else
      WriteDirect('',true);
  end;
  WriteDirect('\end{description}',true);
  
  
  { write the complete definition }  
  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);

  for j := 0 to FuncsProcs.Count - 1 do 
  begin
   p := TPasMethod(FuncsProcs.PasItemAt[j]);
      { overview of functions and procedures }
      begin

        if Methods then WriteVisibilityCell(p);
        
        WriteDirect('\textbf{'+p.Name+'}\hspace{1ex}\rule{\textwidth}{0.1mm}',true);
        WriteAnchor(p.Name);
        WriteDirect('\linebreak',true);

        WriteCodeWithLinks(p, p.FullDeclaration, '');

        WriteStartOfParagraph;
        WriteItemDetailedDescription(p);
        WriteEndOfParagraph;

        WriteParamsOrRaises(p, 'parameters', p.Params);
        WriteReturnDesc(p, p.Returns);
        WriteParamsOrRaises(p, 'exceptions raised', p.Raises);
      end;
  end;
end;

procedure TTexDocGenerator.WriteHeading(Level: integer; const s: string);
var
  c: string;
begin
  if (Level < 1) then Level := 1;
  if Level > 4 then begin
    DoMessage(2, mtWarning, 'latex generator cannot write headlines of level 4 or greater; will use 4 instead.', []);
    Level := 4;
  end;
  case Level of
    1: WriteDirect('\chapter{');
    2: WriteDirect('\section{');
    3: WriteDirect('\subsection{');
    4: WriteDirect('\subsubsection{');
  end;
  WriteConverted(s);
  WriteDirect('}', true);
end;

{ ---------- }

procedure TTexDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;

  if AItem.Description <> '' then begin
    WriteWithURLs(AItem.Description);
  end else begin
    if AItem.DetailedDescription <> '' then begin
      WriteWithURLs(AItem.DetailedDescription)
    end else begin
      WriteDirect(' ');
    end;
  end;
end;

procedure TTexDocGenerator.WriteItemDetailedDescription(const AItem: TPasItem);
var
  Ancestor: TPasCio;
  AncestorName: string;
begin
  if not Assigned(AItem) then Exit;

  if AItem.Description <> '' then begin
    WriteWithURLs(AItem.Description);

    if AItem.DetailedDescription <> '' then begin
      WriteStartOfParagraph;
      WriteWithURLs(AItem.DetailedDescription);
      WriteEndOfParagraph;
    end;
  end else begin
    if AItem.DetailedDescription <> '' then begin
      WriteStartOfParagraph;
      WriteWithURLs(AItem.DetailedDescription);
      WriteEndOfParagraph;
    end else begin
      if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := TPasCio(SearchItem(AncestorName, AItem));
        if Assigned(Ancestor) then
          begin
            WriteDirect('<div class="nodescription">');
            WriteConverted(Format('no description available, %s description follows', [AncestorName]));
            WriteDirect('</div>');
            WriteItemDetailedDescription(Ancestor);
          end;
      end else
      begin
        WriteDirect(' ');
      end;
    end;
  end;
end;

procedure TTexDocGenerator.WriteItems(HL: integer; Heading: string; const
  Anchor: string; const i: TPasItems);
var
  j, k: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(i) then Exit;

  if HtmlHelp and (Anchor <> '') then
    WriteAnchor('@' + Anchor);

  WriteHeading(HL, Heading);

  WriteDirect('<table class="itemlist" cellspacing="' +
    HTML_TABLE_CELLSPACING + '" cellpadding="' + HTML_TABLE_CELLPADNG +
    '" width="100%">');
  WriteDirect('<tr class="listheader">');
  WriteDirect('<th class="listheader" nowrap="nowrap">');
  WriteConverted(FLanguage.Translation[trName]);
  WriteDirect('</th><th class="listheader" width="100%">');
  WriteConverted(FLanguage.Translation[trDescription]);
  WriteDirect('</th></tr>', true);

  for j := 0 to i.Count - 1 do begin
    Item := i.PasItemAt[j];
    WriteStartOfTableRow('');

    WriteStartOfTableCell('itemname');
    WriteAnchor(Item.Name);
    WriteConverted(Item.Name);
    if Item is TPasVarConst then begin
      WriteCodeWithLinks(Item, TPasVarConst(Item).FullDeclaration, '');
    end;
    WriteEndOfTableCell;

    WriteStartOfTableCell;
    WriteItemDetailedDescription(Item);
    if Item is TPasEnum then begin
      WriteDirect('<ul>', true);
      for k := 0 to TPasEnum(Item).Members.Count-1 do begin
        WriteDirect('<li>', true);
        WriteConverted(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).Name);
        WriteConverted(': ');
        WriteWithURLs(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).GetDescription);
        WriteDirect('</li>', true);
      end;
      WriteDirect('</ul>', true);
    end;
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  WriteDirect('</table>');
end;

{ ---------- }

procedure TTexDocGenerator.WriteOverviewFiles;
var
  ItemsToCopy: TPasItems;
  PartialItems: TPasItems;
  TotalItems: TPasItems; // Collect all Items for final listing.
  i: Integer;
  Item: TPasItem;
  j: Integer;
  PU: TPasUnit;
begin
  if HtmlHelp then
    WriteHtmlHelpProject;

  WriteUnitOverviewFile;

  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  // Make sure we don't free the Itmes when we free the container.
  TotalItems := TPasItems.Create(False);

  for i := 2 to NUM_OVERVIEW_FILES_USED - 1  do begin
    if (CreateStream(OverviewFilenames[i] + GetFileExtension, True) = csError)
      then begin
      DoMessage(1, mtError, 'Error: Could not create output file "' +
        OverviewFilenames[i] + '".', []);
      Exit;
    end;
    DoMessage(3, mtInformation, 'Writing overview file ' +
      OverviewFilenames[i] + '...', []);

    case i of
      2: WriteStartOfDocument(FLanguage.Translation[trHeadlineCio]);
      3: WriteStartOfDocument(FLanguage.Translation[trHeadlineTypes]);
      4: WriteStartOfDocument(FLanguage.Translation[trHeadlineVariables]);
      5: WriteStartOfDocument(FLanguage.Translation[trHeadlineConstants]);
      6: WriteStartOfDocument(FLanguage.Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteStartOfDocument(FLanguage.Translation[trHeadlineIdentifiers]);
    end;

    case i of
      2: WriteHeading(1, FLanguage.Translation[trHeadlineCio]);
      3: WriteHeading(1, FLanguage.Translation[trHeadlineTypes]);
      4: WriteHeading(1, FLanguage.Translation[trHeadlineVariables]);
      5: WriteHeading(1, FLanguage.Translation[trHeadlineConstants]);
      6: WriteHeading(1, FLanguage.Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteHeading(1, FLanguage.Translation[trHeadlineIdentifiers]);
    end;

      // Make sure we don't free the Itmes when we free the container.
    PartialItems := TPasItems.Create(False);

    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      case i of
        2: ItemsToCopy := PU.CIOs;
        3: ItemsToCopy := PU.Types;
        4: ItemsToCopy := PU.Variables;
        5: ItemsToCopy := PU.Constants;
        6: ItemsToCopy := PU.FuncsProcs;
      else
        ItemsToCopy := nil;
      end;
      PartialItems.InsertItems(ItemsToCopy);
    end;

    if not ObjectVectorIsNilOrEmpty(PartialItems) then begin
      WriteStartOfTable3Columns(FLanguage.Translation[trName], FLanguage.Translation[trUnit],
        FLanguage.Translation[trDescription]);

      PartialItems.SortByPasItemName;

      for j := 0 to PartialItems.Count - 1 do begin
        Item := PartialItems.PasItemAt[j];
        WriteStartOfTableRow('');

        WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
        WriteLink(Item.FullLink, Item.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell;
        WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
        WriteEndOfTableCell;

        if j = 0 then
          WriteStartOfTableCell('width="100%"', '')
        else
          WriteStartOfTableCell;
        WriteItemDescription(Item);
        WriteEndOfTableCell;

        WriteEndOfTableRow;
      end;
      WriteEndOfTable;
    end
    else begin
      WriteStartOfParagraph;
      WriteConverted(FLanguage.Translation[trNone]);
      WriteEndOfParagraph;
    end;

    TotalItems.InsertItems(PartialItems);
    PartialItems.Free;
    WriteFooter;
    WriteEndOfDocument;
    CloseStream;
  end;

  if CreateStream(OverviewFilenames[7] + GetFileExtension, True) = csError then
    begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[7] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing overview file ' + OverviewFilenames[7]
    + '...', []);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineIdentifiers]);
  WriteHeading(1, FLanguage.Translation[trHeadlineIdentifiers]);
  WriteStartOfTable3Columns(FLanguage.Translation[trName], FLanguage.Translation[trUnit],
    FLanguage.Translation[trDescription]);

  TotalItems.SortByPasItemName;
  for j := 0 to TotalItems.Count - 1 do begin
    Item := TotalItems.PasItemAt[j];
    WriteStartOfTableRow('');

    WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
    WriteLink(Item.FullLink, Item.Name, 'bold');
    WriteEndOfTableCell;

    WriteStartOfTableCell;
    WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
    WriteEndOfTableCell;

    if j = 0 then
      WriteStartOfTableCell('width="100%"', '')
    else
      WriteStartOfTableCell;
    WriteItemDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;

  TotalItems.Free;

  WriteEndOfTable;
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

procedure TTexDocGenerator.WriteParagraph(HL: integer; s: string; t: string);
begin
  // if (not Assigned(t)) or (t.Content < 1) then Exit;
  WriteHeading(HL, s);
  WriteStartOfParagraph;
  WriteWithURLs(t);
  WriteEndOfParagraph;
end;

procedure TTexDocGenerator.WriteProperties(HL: integer; const p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
begin
  if ObjectVectorIsNilOrEmpty(p) then Exit;

  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);

    WriteStartOfTable1Column('');
    WriteStartOfTableRow('');

    WriteVisibilityCell(Prop);

    WriteStartOfTableCell('width="100%"', '');
    WriteAnchor(Prop.Name);
    WriteCodeWithLinks(Prop, 'property ' + Prop.FullDeclaration, '');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
    WriteEndOfTable;

    WriteStartOfParagraph;
    WriteItemDetailedDescription(Prop);
    WriteEndOfParagraph;

  end;
end;

procedure TTexDocGenerator.WritePropertiesSummary(HL: integer; p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
begin
  if ObjectVectorIsNilOrEmpty(p) then Exit;

//  if HtmlHelp then
    WriteAnchor('@Properties');

  WriteHeading(HL, FLanguage.Translation[trProperties]);
  WriteHeading(HL + 1, FLanguage.Translation[trOverview]);

  WriteStartOfTable1Column('');
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);
    WriteStartOfTableRow('');

    WriteVisibilityCell(Prop);
    if j = 0 then
      WriteStartOfTableCell('width="100%"', '')
    else
      WriteStartOfTableCell;

    WriteCodeWithLinks(Prop, 'property ' + Prop.FullDeclaration,
      Prop.FullLink);

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteAnchor(const AName: string);
begin
  WriteAnchor(AName, '');
end;

procedure TTexDocGenerator.WriteAnchor(const AName, Caption: string);
begin
  WriteDirect(Format('\label{%s}', [AName]),true);
  WriteDirect('\index{'+AName+'}',true);
end;

procedure TTexDocGenerator.WriteStartOfAnchor(const AName: string);
begin
  WriteDirect('<a namae="' + AName + '">');
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteStartOfCode;
begin
  WriteDirect('{\ttfamily ');
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteStartOfDocument(AName: string);
var
 i : integer;
begin
  { write basic header }
  WriteAppInfo;
  WriteDirect('\documentclass{report}',true);
  WriteDirect('\begin{document}',true);
  if Length(Header) > 0 then begin
    WriteWithURLs(Header);
  end;
end;

procedure TTexDocGenerator.WriteStartOfLink(const href, css, Target: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<a class="%s"', [css])
  else
    s := '<a class="normal"';
  if target <> '' then
    s := Format('%s target="%s"', [s, target]);
  WriteDirect(Format('%s href="%s">', [s, EscapeURL(href)]));
end;

procedure TTexDocGenerator.WriteStartOfLink(const href, css: string);
begin
  WriteStartOfLink(href, css, '');
end;

procedure TTexDocGenerator.WriteStartOfLink(const href: string);
begin
  WriteStartOflink(href, '', '');
end;

procedure TTexDocGenerator.WriteStartOfParagraph;
begin
  WriteDirect('',true);
end;

procedure TTexDocGenerator.WriteStartOfTable1Column(T: String);
begin
  FOddTableRow := 0;
  NumCells := 1;
  WriteDirect('\begin{tabular}{|l|}',true);
  WriteDirect('\hline',true);
  if t <> '' then
    begin
     WriteConverted(t);
     WriteDirect(' \\',true);
     WriteDirect('\hline',true);
    end;
end;

procedure TTexDocGenerator.WriteStartOfTable2Columns(T1, T2: String);
begin
  WriteDirect('\begin{tabular}{|ll|}',true);
  WriteDirect('\hline',true);
  if t1 <> '' then
  Begin
     WriteConverted(t1);
     WriteDirect(' & ');
     WriteConverted(t2);
     WriteDirect(' \\',true);
     WriteDirect('\hline',true);
  end;
  NumCells := 2;
end;

procedure TTexDocGenerator.WriteStartOfTable3Columns(T1, T2, T3: String);
begin
  WriteDirect('\begin{tabular}{|lll|}',true);
  WriteDirect('\hline',true);
  WriteConverted(T1);
  WriteDirect(' & ');
  WriteConverted(T2);
  WriteDirect(' & ');
  WriteConverted(T3);
  WriteDirect(' \\',true);
  WriteDirect('\hline',true);
  NumCells := 3;
end;


procedure TTexDocGenerator.WriteStartOfTableCell(const Params, css: string);
var
  s: string;
begin
end;

procedure TTexDocGenerator.WriteStartOfTableCell(const css: string);
begin
  WriteStartOfTableCell('', css);
end;

procedure TTexDocGenerator.WriteStartOfTableCell;
begin
  WriteStartOfTableCell('', '');
end;

procedure TTexDocGenerator.WriteStartOfTableRow(const CssClass: string);
var
  s: string;
begin
  CellCounter := 0;
end;

{ ---------------------------------------------------------------------------- }
{ HtmlHelp Content Generation inspired by Wim van der Vegt <wvd_vegt@knoware.nl> }
{ ---------------------------------------------------------------------------- }

function BeforeEqualChar(const s: string): string;
var
  i: Cardinal;
begin
  Result := s;
  i := Pos('=', Result);
  if i <> 0 then
    SetLength(Result, i - 1);
end;

function AfterEqualChar(const s: string): string;
var
  i: Cardinal;
begin
  Result := s;
  i := Pos('=', Result);
  if i <> 0 then
    Delete(Result, 1, i)
  else
    Result := '';
end;

function GetLevel(var s: string): Integer;
var
  l: Cardinal;
  p: PChar;
begin
  Result := 0;
  p := Pointer(s);
  l := Length(s);
  while (l > 0) and (p^ in [' ', #9]) do begin
    Inc(Result);
    Inc(p);
    Dec(l);
  end;
  Delete(s, 1, Result);
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteHtmlHelpProject;
var
  DefaultContentsWritten: Boolean;
  DefaultTopic: string;

  procedure WriteLiObject(const Name, Local: string);
  begin
    WriteDirect('<li><object type="text/sitemap">', true);
    WriteDirect('<param name="Name" value="' + Name + '">', true);
    if Local <> '' then begin
      WriteDirect('<param name="Local" value="' + Local + '">', true);
      if DefaultTopic = '' then
        DefaultTopic := Local;
    end;
    WriteDirect('</object>', true);
  end;

  { ---------- }

  procedure WriteItemCollection(const _Filename: string; const c: TPasItems);
  var
    i: Integer;
    Item: TPasItem;
  begin
    if Assigned(c) then begin
      WriteDirect('<ul>', true);
      for i := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[i];
        WriteLiObject(Item.Name, _Filename + '#' + Item.Name);
      end;
      WriteDirect('</ul>', true);
    end;
  end;

  { ---------- }

  procedure WriteItemHeadingCollection(const Title, ParentLink, Anchor: string; const
    c: TPasItems);
  begin
    if Assigned(c) and (c.Count > 0) then begin
      WriteLiObject(Title, ParentLink + '#' + Anchor);
      WriteItemCollection(ParentLink, c);
    end;
  end;

  { ---------- }

  procedure InternalWriteCIO(const ClassItem: TPasCio);
  begin
    WriteLiObject(ClassItem.Name, ClassItem.FullLink);
    WriteDirect('<ul>', true);

    WriteItemHeadingCollection(fLanguage.Translation[trFields], ClassItem.FullLink, '@Fields', ClassItem.Fields);
    WriteItemHeadingCollection(fLanguage.Translation[trProperties], ClassItem.FullLink, '@Properties', ClassItem.Properties);
    WriteItemHeadingCollection(fLanguage.Translation[trMethods], ClassItem.FullLink, '@Methods', ClassItem.Methods);

    WriteDirect('</ul>', true);
  end;

  { ---------- }

  procedure ContentWriteUnits(const Text: string);
  var
    c: TPasItems;
    j, k: Integer;
    PU: TPasUnit;
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[0] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trUnits], OverviewFilenames[0] +
        GetFileExtension);
    WriteDirect('<ul>', true);

    // Iterate all Units
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      WriteLiObject(PU.Name, PU.FullLink);
      WriteDirect('<ul>', true);

        // For each unit, write classes (if there are any).
      c := PU.CIOs;
      if Assigned(c) then begin
        WriteLiObject(FLanguage.Translation[trClasses], PU.FullLink + '#@Classes');
        WriteDirect('<ul>', true);

        for k := 0 to c.Count - 1 do
          InternalWriteCIO(TPasCio(c.PasItemAt[k]));

        WriteDirect('</ul>', true);
      end;

        // For each unit, write Functions & Procedures.
      WriteItemHeadingCollection(FLanguage.Translation[trFunctionsAndProcedures],
        PU.FullLink, '@FuncsProcs', PU.FuncsProcs);
        // For each unit, write Types.
      WriteItemHeadingCollection(FLanguage.Translation[trTypes], PU.FullLink,
        '@Types', PU.Types);
        // For each unit, write Constants.
      WriteItemHeadingCollection(FLanguage.Translation[trConstants], PU.FullLink,
        '@Constants', PU.Constants);

      WriteDirect('</ul>', true);
    end;
    WriteDirect('</ul>', true);
  end;

  { ---------- }

  procedure ContentWriteClasses(const Text: string);
  var
    c: TPasItems;
    j: Integer;
    PU: TPasUnit;
  begin
    // Write Classes to Contents
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[2] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trClasses], OverviewFilenames[2] +
        GetFileExtension);
    WriteDirect('<ul>', true);

    c := TPasItems.Create(False);
    // First collect classes
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      c.CopyItems(PU.CIOs);
    end;
    // Output sorted classes
    // TODO: Sort
    for j := 0 to c.Count - 1 do
      InternalWriteCIO(TPasCio(c.PasItemAt[j]));
    c.Free;
    WriteDirect('</ul>', true);
  end;

  { ---------- }

  procedure ContentWriteClassHierarchy(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[1] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trClassHierarchy], OverviewFilenames[1] +
        GetFileExtension);
  end;

  { ---------- }

  procedure ContentWriteOverview(const Text: string);

    procedure WriteParam(Id: TTranslationId);
    begin
      WriteDirect('<param name="Name" value="');
      WriteConverted(FLanguage.Translation[Id]);
      WriteDirect('">', true);
    end;

  var
    j: Integer;
  begin
    if Text <> '' then
      WriteLiObject(Text, '')
    else
      WriteLiObject(FLanguage.Translation[trOverview], '');
    WriteDirect('<ul>', true);
    for j := 0 to NUM_OVERVIEW_FILES_USED - 1 do begin
      WriteDirect('<li><object type="text/sitemap">', true);
      case j of
        0: WriteParam(trHeadlineUnits);
        1: WriteParam(trClassHierarchy);
        2: WriteParam(trHeadlineCio);
        3: WriteParam(trHeadlineTypes);
        4: WriteParam(trHeadlineVariables);
        5: WriteParam(trHeadlineConstants);
        6: WriteParam(trHeadlineFunctionsAndProcedures);
        7: WriteParam(trHeadlineIdentifiers);
      end;
      WriteDirect('<param name="Local" value="');
      WriteConverted(OverviewFilenames[j] + GetFileExtension);
      WriteDirect('">', true);
      WriteDirect('</object>', true);
    end;
    WriteDirect('</ul>', true);
  end;

  { ---------- }

  procedure ContentWriteLegend(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, 'Legend' + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trLegend], 'Legend' + GetFileExtension);
  end;

  { ---------- }

  procedure ContentWriteCustom(const Text, Link: string);
  begin
    if CompareText('@Classes', Link) = 0 then begin
      DefaultContentsWritten := True;
      ContentWriteClasses(Text);
    end
    else
      if CompareText('@ClassHierarchy', Link) = 0 then begin
        DefaultContentsWritten := True;
        ContentWriteClassHierarchy(Text);
      end
      else
        if CompareText('@Units', Link) = 0 then begin
          DefaultContentsWritten := True;
          ContentWriteUnits(Text);
        end
        else
          if CompareText('@Overview', Link) = 0 then begin
            DefaultContentsWritten := True;
            ContentWriteOverview(Text);
          end
          else
            if CompareText('@Legend', Link) = 0 then begin
              DefaultContentsWritten := True;
              ContentWriteLegend(Text);
            end
            else
              WriteLiObject(Text, Link);
  end;

  procedure IndexWriteItem(const Item, PreviousItem, NextItem: TPasItem);
    { Item is guaranteed to be assigned, i.e. not to be nil. }
  begin
    if Assigned(Item.MyObject) then begin
      if (Assigned(NextItem) and Assigned(NextItem.MyObject) and
        (CompareText(Item.MyObject.Name, NextItem.MyObject.Name) = 0)) or
        (Assigned(PreviousItem) and Assigned(PreviousItem.MyObject) and
          (CompareText(Item.MyObject.Name, PreviousItem.MyObject.Name) = 0))
          then
        WriteLiObject(Item.MyObject.Name + ' - ' + Item.MyUnit.Name + #32 +
          FLanguage.Translation[trUnit], Item.FullLink)
      else
        WriteLiObject(Item.MyObject.Name, Item.FullLink);
    end
    else begin
      WriteLiObject(Item.MyUnit.Name + #32 + FLanguage.Translation[trUnit],
        Item.FullLink);
    end;
  end;

  { ---------------------------------------------------------------------------- }

var
  j, k, l: Integer;
  CurrentLevel, Level: Integer;
  CIO: TPasCio;
  PU: TPasUnit;
  c: TPasItems;
  Item, NextItem, PreviousItem: TPasItem;
  Item2: TPasCio;
  s, Text, Link: string;
  SL: TStringVector;
begin
  { At this point, at least one unit has been parsed:
    Units is assigned and Units.Count > 0
    No need to test this again. }

  if CreateStream(ProjectName + '.hhc', True) = csError then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Content file "%s.hhc' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Content file "' + ProjectName
    + '"...', []);

  // File Header
  WriteDirect('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">', true);
  WriteDirect('<html>', true);
  WriteDirect('<head>', true);
  WriteDirect('<meta name="GENERATOR" content="' +
    PASDOC_NAME_AND_VERSION + '"/>', true);
  WriteDirect('</head><body>', true);
  WriteDirect('<ul>', true);

  DefaultContentsWritten := False;
  DefaultTopic := '';
  if ContentsFile <> '' then begin
    SL := NewStringVector;
    try
      SL.LoadFromTextFileAdd(ContentsFile);
    except
      on e: Exception do
        DoMessage(1, mtError, e.Message +
          '. Writing default HtmlHelp contents.', []);
    end;

    CurrentLevel := 0;
    for j := 0 to SL.Count - 1 do begin
      s := SL[j];
      Text := BeforeEqualChar(s);
      Level := GetLevel(Text);
      Link := AfterEqualChar(s);

      if Level = CurrentLevel then
        ContentWriteCustom(Text, Link)
      else
        if CurrentLevel = (Level - 1) then begin
          WriteDirect('<ul>', true);
          Inc(CurrentLevel);
          ContentWriteCustom(Text, Link)
        end
        else
          if CurrentLevel > Level then begin
            WriteDirect('</ul>', true);
            Dec(CurrentLevel);
            while CurrentLevel > Level do begin
              WriteDirect('</ul>', true);
              Dec(CurrentLevel);
            end;
            ContentWriteCustom(Text, Link)
          end

          else begin
            DoMessage(1, mtError, 'Invalid level ' + IntToStr(Level) +
              'in Content file (line ' + IntToStr(j) + ').', []);
            Exit;
          end;
    end;
    SL.Free;
  end;

  if not DefaultContentsWritten then begin
    ContentWriteUnits('');
    ContentWriteClassHierarchy(FLanguage.Translation[trClassHierarchy]);
    ContentWriteClasses('');
    ContentWriteOverview('');
    ContentWriteLegend('');
  end;

  // End of File
  WriteDirect('</ul>', true);
  WriteDirect('</body></html>', true);
  CloseStream;

  // Create Keyword Index
  // First collect all Items
  c := TPasItems.Create(False); // Don't free Items when freeing the container

  for j := 0 to Units.Count - 1 do begin
    PU := Units.UnitAt[j];

    if Assigned(PU.CIOs) then
      for k := 0 to PU.CIOs.Count - 1 do begin
        CIO := TPasCio(PU.CIOs.PasItemAt[k]);
        c.Add(CIO);
        c.CopyItems(CIO.Fields);
        c.CopyItems(CIO.Properties);
        c.CopyItems(CIO.Methods);
      end;

    c.CopyItems(PU.Types);
    c.CopyItems(PU.Variables);
    c.CopyItems(PU.Constants);
    c.CopyItems(PU.FuncsProcs);
  end;

  if CreateStream(ProjectName + '.hhk', True) = csError then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Index file "%s.hhk' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Index file "%s"...',
    [ProjectName]);

  WriteDirect('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">', true);
  WriteDirect('<html>', true);
  WriteDirect('<head>', true);
  WriteDirect('<meta name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">', true);
  WriteDirect('</head><body>', true);
  WriteDirect('<ul>', true);

  // Write all Items to KeyWord Index

  c.SortByPasItemName;

  if c.Count > 0 then begin
    Item := c.PasItemAt[0];
    j := 1;

    while j < c.Count do begin
      NextItem := c.PasItemAt[j];

          // Does the next Item have a different name?
      if CompareText(Item.Name, NextItem.Name) <> 0 then begin
        WriteLiObject(Item.Name, Item.FullLink);
        Item := NextItem;
      end
      else begin
        // Write the Item. It acts as a header for the subitems to follow.
        WriteLiObject(Item.Name, Item.FullLink);
        // Indent by one.
        WriteDirect('<ul>', true);

        // No previous Item as we start.
        PreviousItem := nil;

        // Keep on writing Items with the same name as subitems.
        repeat
          IndexWriteItem(Item, PreviousItem, NextItem);

          PreviousItem := Item;
          Item := NextItem;
          Inc(j);

          if j >= c.Count then Break;
          NextItem := c.PasItemAt[j];

                // Break as soon Items' names are different.
        until CompareText(Item.Name, NextItem.Name) <> 0;

              // No NextItem as we write the last one of the same Items.
        IndexWriteItem(Item, PreviousItem, nil);

        Item := NextItem;
        WriteDirect('</ul>', true);
      end;

      Inc(j);
    end;

      // Don't forget to write the last item. Can it ever by nil?
    WriteLiObject(Item.Name, Item.FullLink);
  end;

  c.Free;

  WriteDirect('</ul>', true);
  WriteDirect('</body></html>', true);
  CloseStream;

  // Create a HTML Help Project File
  if CreateStream(ProjectName + '.hhp', True) = csError then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Project file "%s.hhp' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing Html Help Project file "%s"...',
    [ProjectName]);

  WriteDirect('[OPTIONS]', true);
  WriteDirect('Binary TOC=Yes', true);
  WriteDirect('Compatibility=1.1 or later', true);
  WriteDirect('Compiled file=' + ProjectName + '.chm', true);
  WriteDirect('Contents file=' + ProjectName + '.hhc', true);
  WriteDirect('Default Window=Default', true);
  WriteDirect('Default topic=' + DefaultTopic, true);
  WriteDirect('Display compile progress=Yes', true);
  WriteDirect('Error log file=' + ProjectName + '.log', true);
  WriteDirect('Full-text search=Yes', true);
  WriteDirect('Index file=' + ProjectName + '.hhk', true);
  if Title <> '' then
    WriteDirect('Title=' + Title, true)
  else
    WriteDirect('Title=' + ProjectName, true);

  WriteDirect('', true);
  WriteDirect('[WINDOWS]', true);
  if Title <> '' then
    WriteDirect('Default="' + Title + '","' + ProjectName +
      '.hhc","' + ProjectName + '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true)
  else
    WriteDirect('Default="' + ProjectName + '","' +
      ProjectName + '.hhc","' + ProjectName +
      '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true);

  WriteDirect('', true);
  WriteDirect('[FILES]', true);

  { HHC seems to know about the files by reading the Content and Index.
    So there is no need to specify them in the FILES section. }

  WriteDirect('Legend.html', true);
  for k := 0 to NUM_OVERVIEW_FILES_USED - 1 do
    WriteDirect(OverviewFilenames[k] + '.html', true);

  if Assigned(Units) then
    for k := 0 to units.Count - 1 do
      begin
        Item := units.PasItemAt[k];
        PU := TPasUnit(units.PasItemAt[k]);
        WriteDirect(Item.FullLink, true);
        c := PU.CIOs;
        if Assigned(c) then
          for l := 0 to c.Count - 1 do
            begin
              Item2 := TPasCio(c.PasItemAt[l]);
              WriteDirect(Item2.OutputFilename, true);
            end;
      end;

  WriteDirect('', true);

  WriteDirect('[INFOTYPES]', true);

  WriteDirect('', true);

  WriteDirect('[MERGE FILES]', true);

  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
type
  TSections = (dsDescription, dsUses, dsClasses, dsFuncsProcs,
    dsTypes, dsConstants, dsVariables);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
const
  SectionAnchors: TSectionAnchors = (
    '@Description',
    '@Uses',
    '@Classes',
    '@FuncsProcs',
    '@Types',
    '@Constants',
    '@Variables');
var
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;
  Section: TSections;

  procedure ConditionallyAddSection(Section: TSections; Condition: boolean);
  begin
    if Condition then
      Include(SectionsAvailable, Section);
  end;

begin
  if not Assigned(U) then begin
    DoMessage(1, mtError,
      'TTexDocGenerator.WriteUnit: Unit variable has not been initialized.',
      []);
    Exit;
  end;

  case CreateStream(U.OutputFileName, not U.WasDeserialized) of
    csError: begin
      DoMessage(1, mtError, 'Could not create HTML unit doc file for unit %s.', [U.Name]);
      Exit;
    end;
    csExisted: begin
      Exit;
    end;
  end;

  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsUses] := 'uses';
  SectionHeads[dsClasses] := FLanguage.Translation[trCio];
  SectionHeads[dsFuncsProcs]:= FLanguage.Translation[trFunctionsAndProcedures];
  SectionHeads[dsTypes]:= FLanguage.Translation[trTypes];
  SectionHeads[dsConstants]:= FLanguage.Translation[trConstants];
  SectionHeads[dsVariables]:= FLanguage.Translation[trVariables];

  SectionsAvailable := [dsDescription];
  ConditionallyAddSection(dsUses, WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits));
  ConditionallyAddSection(dsClasses, not ObjectVectorIsNilOrEmpty(U.CIOs));
  ConditionallyAddSection(dsFuncsProcs, not ObjectVectorIsNilOrEmpty(U.FuncsProcs));
  ConditionallyAddSection(dsTypes, not ObjectVectorIsNilOrEmpty(U.Types));
  ConditionallyAddSection(dsConstants, not ObjectVectorIsNilOrEmpty(U.Constants));
  ConditionallyAddSection(dsVariables, not ObjectVectorIsNilOrEmpty(U.Variables));

  DoMessage(2, mtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteStartOfDocument(U.Name);

  WriteHeading(HL, FLanguage.Translation[trUnit] + ' ' + U.Name);

  WriteUnitDescription(HL + 1, U);

  WriteUnitUses(HL + 1, U);

  WriteCIOSummary(HL + 1, U.CIOs);

  WriteFuncsProcs(HL + 1, False, U.FuncsProcs);

  WriteAnchor(SectionAnchors[dsTypes]);
  WriteTypes(HL + 1, U.Types);

  WriteAnchor(SectionAnchors[dsConstants]);
  WriteConstants(HL + 1, U.Constants);

  WriteAnchor(SectionAnchors[dsVariables]);
  WriteVariables(HL + 1, U.Variables);

  WriteAuthors(HL + 1, U.Authors);
  WriteDates(HL + 1, U.Created, U.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
  WriteCIOs(HL, U.CIOs);
end;

procedure TTexDocGenerator.WriteUnitDescription(HL: integer; U: TPasUnit);
begin
  WriteHeading(HL, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(U);
end;

procedure TTexDocGenerator.WriteUnitOverviewFile;
var
  c: TPasItems;
  Item: TPasItem;
  j: Integer;
begin
  c := Units;
  if CreateStream(OverviewFilenames[0] + GetFileExtension, True) = csError
    then begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[0] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing unit overview file "%s" ...',
    [OverviewFilenames[0]]);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineUnits]);
  WriteHeading(1, FLanguage.Translation[trHeadlineUnits]);
  if Assigned(c) and (c.Count > 0) then begin
    WriteStartOfTable2Columns(FLanguage.Translation[trName],
      FLanguage.Translation[trDescription]);
    for j := 0 to c.Count - 1 do begin
      Item := c.PasItemAt[j];
      WriteStartOfTableRow('');
      WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
      WriteLink(Item.FullLink, Item.Name, 'bold');
      WriteEndOfTableCell;

      if j = 0 then
        WriteStartOfTableCell('width="100%"', '')
      else
        WriteStartOfTableCell;
      WriteItemDescription(Item);
      WriteEndOfTableCell;
      WriteEndOfTableRow;
    end;
    WriteEndOfTable;
  end;
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

procedure TTexDocGenerator.WriteImage(const src, alt, css: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<img class="%s"', [css])
  else
    s := '<img border="0"';
  WriteDirect(Format('%s src="%s" alt="%s"/>', [s, src, alt]));
end;

procedure TTexDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(const Image: string; trans: TTranslationID);
  begin
    WriteStartOfLink('legend.html');
    WriteImage(Image, ConvertString(FLanguage.Translation[trans]), '');
    WriteEndOfLink;
  end;

begin
  WriteStartOfTableCell;
  case Item.State of
    STATE_PRIVATE:
      WriteVisibilityImage('private.gif', trPrivate);
    STATE_PROTECTED:
      WriteVisibilityImage('protected.gif', trProtected);
    STATE_PUBLIC:
      WriteVisibilityImage('public.gif', trPublic);
    STATE_PUBLISHED:
      WriteVisibilityImage('published.gif', trPublished);
    STATE_AUTOMATED:
      WriteVisibilityImage('automated.gif', trAutomated);
  end;
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteVisibilityLegendFile;

  procedure WriteLegendEntry(const Image: string; trans: TTranslationID);
  begin
    WriteStartOfTableRow('');
    WriteStartOfTableCell();
    WriteImage(Image, ConvertString(FLanguage.Translation[trans]), '');
    WriteEndOfTableCell;
    WriteStartOfTableCell();
    WriteConverted(FLanguage.Translation[trans]);
    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;

const
  Filename = 'legend';
begin
  if CreateStream(Filename + GetFileextension, True) = csError then
    begin
      DoMessage(1, mtError, 'Could not create output file "%s".',
        [Filename + GetFileExtension]);
      Abort;
    end;
  WriteStartOfDocument(FLanguage.Translation[trLegend]);

  WriteHeading(1, FLanguage.Translation[trLegend]);

  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG + '">', true);
  WriteDirect('<tr class="listheader"><th class="listheader">');
  { TODO -otwm : needs translation }
  WriteConverted('Marker');
  WriteDirect('</th><th class="listheader">');
  { TODO -otwm : needs translation }
  WriteConverted('Visibility');
  WriteDirect('</th></tr>', true);

  WriteLegendEntry('private.gif', trPrivate);
  WriteLegendEntry('protected.gif', trProtected);
  WriteLegendEntry('public.gif', trPublic);
  WriteLegendEntry('published.gif', trPublished);
  WriteLegendEntry('automated.gif', trAutomated);
  WriteEndOfTable;

  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteHierarchy;
var
  Level, OldLevel: Integer;
  Node: TPasItemNode;
begin
  CreateClassHierarchy;

  if CreateStream(OverviewFilenames[1] + GetFileExtension, True) = csError then begin
    DoMessage(1, mtError, 'Could not create output file "%s".',
      [OverviewFilenames[1] + GetFileExtension]);
    Abort;
  end;

  WriteStartOfDocument(FLanguage.Translation[trClassHierarchy]);
  WriteHeading(1, FLanguage.Translation[trClassHierarchy]);

  if FClassHierarchy.IsEmpty then begin
    WriteStartOfParagraph;
    WriteConverted(FLanguage.Translation[trNone]);
    WriteEndOfParagraph;
  end else begin
    OldLevel := -1;
    Node := FClassHierarchy.FirstItem;
    while Node <> nil do begin
      Level := Node.Level;
      if Level > OldLevel then
        WriteDirect('\begin{itemize}',true)
      else
        while Level < OldLevel do begin
          WriteDirect('\end{itemize}',true);
          Dec(OldLevel);
        end;
      OldLevel := Level;

      if Node.Item = nil then
        begin
          WriteDirect('\item ');
          WriteConverted(Node.Name);
          WriteDirect('',true);
        end
      else
        begin
          WriteDirect('\item ');
          WriteLink(Node.Item.FullLink, ConvertString(Node.Name), 'bold');
          WriteDirect('',true);
        end;
      Node := FClassHierarchy.NextItem(Node);
    end;

    while OldLevel > 0 do begin
      WriteDirect('\end{itemize}',true);
      Dec(OldLevel);
    end;
  end;

  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;

  CloseStream;
end;

procedure TTexDocGenerator.LoadFooterFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FFooter);
end;

procedure TTexDocGenerator.LoadHeaderFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FHeader);
end;

procedure TTexDocGenerator.WriteUnitUses(const HL: integer; U: TPasUnit);
var
  i: Integer;
  ULink: TPasItem;
begin
  if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
    WriteHeading(HL, 'uses');
    WriteDirect('\begin{itemize}',true);
    for i := 0 to U.UsesUnits.Count-1 do begin
      WriteDirect('\item ');
      ULink := FUnits.FindName(U.UsesUnits[i]);
      if ULink is TPasUnit then begin
        WriteLink(ULink.FullLink, U.UsesUnits[i], 'bold');
      end else begin
        WriteDirect(U.UsesUnits[i]);
      end;
      WriteDirect('');
    end;   
    WriteDirect('\end{itemize}',true);
  end;
end;

procedure TTexDocGenerator.WriteWithURLs(s: string);
var
  s1, s2, link: string;
begin
  while ExtractLink(s, s1, s2, link) do begin
    WriteSpellChecked(S1);
    s:=s2;
  end;
  WriteSpellChecked(s);
end;

procedure TTexDocGenerator.WriteSpellChecked(const AString: string);
var
  LErrors: TObjectVector;
  i, temp: Integer;
  LString, s: string;
begin
  LErrors := TObjectVector.Create(True);
  CheckString(AString, LErrors);
  if LErrors.Count = 0 then begin
    WriteDirect(AString);
  end else begin
    // build s
    s := '';
    LString := AString;
    for i := LErrors.Count-1 downto 0 do begin
      // everything after the offending word
      temp := TSpellingError(LErrors.Items[i]).Offset+Length(TSpellingError(LErrors.Items[i]).Word) + 1;
      s := ( '">' + TSpellingError(LErrors.Items[i]).Word +  '</acronym>' + Copy(LString, temp, MaxInt)) + s; // insert into string
      if Length(TSpellingError(LErrors.Items[i]).Suggestions) > 0 then begin
        s := 'suggestions: '+TSpellingError(LErrors.Items[i]).Suggestions + s;
      end else begin
        s := 'no suggestions' + s;
      end;
      s := '<acronym style="#0000FF; border-bottom: 1px solid crimson" title="' + s;
      SetLength(LString, TSpellingError(LErrors.Items[i]).Offset);
    end;
    WriteDirect(LString);
    WriteDirect(s);
  end;
  LErrors.Free;
end;

procedure TTexDocGenerator.WriteBinaryFiles;
var
  i: Integer;
begin
  CreateStream('automated.gif', True);
  CurrentStream.Write(img_automated[0], High(img_automated)+1);  CloseStream;

  CreateStream('private.gif', True);
  CurrentStream.Write(img_private[0], High(img_private)+1);
  CloseStream;

  CreateStream('protected.gif', True);
  CurrentStream.Write(img_protected[0], High(img_protected)+1);
  CloseStream;

  CreateStream('public.gif', True);
  CurrentStream.Write(img_public[0], High(img_public)+1);
  CloseStream;

  CreateStream('published.gif', True);
  CurrentStream.Write(img_published[0], High(img_published)+1);
  CloseStream;

  if not FileExists(DestinationDirectory+'pasdoc.css') then begin
    CreateStream('pasdoc.css', True);
    StreamUtils.WriteLine(CurrentStream, 'body {' +
      'font-family: Verdana,Arial;' +
      'color: black;' +
      'background-color: white; font-size: 12px; }');
    StreamUtils.WriteLine(CurrentStream, 'body.navigationframe {' +
      'font-family: Verdana,Arial;' +
      'color: white;' +
      'background-color: #787878; font-size: 12px; }');

    StreamUtils.WriteLine(CurrentStream, 'a.navigation:link {' +
      'color: white; text-decoration: none;  font-size: 12px;}');
    StreamUtils.WriteLine(CurrentStream, 'a.navigation:visited {' +
      'color: white; text-decoration: none;  font-size: 12px;}');
    StreamUtils.WriteLine(CurrentStream, 'a.navigation:hover {' +
      'color: white;' +
      'font-weight: bold; text-decoration: none;  font-size: 12px;}');
    StreamUtils.WriteLine(CurrentStream, 'a.navigation:active {' +
      'color: white; text-decoration: none;  font-size: 12px;}');

    StreamUtils.WriteLine(CurrentStream, 'a.normal:link {' +
      'color:#C91E0C; text-decoration: none; }');
    StreamUtils.WriteLine(CurrentStream, 'a.normal:visited {' +
      'color:#7E5C31; text-decoration: none; }');
    StreamUtils.WriteLine(CurrentStream, 'a.normal:hover {' +
      'text-decoration: underline; }');
    StreamUtils.WriteLine(CurrentStream, 'a.normal:active {' +
      'text-decoration: underline; }');

    StreamUtils.WriteLine(CurrentStream, 'a.bold:link {' +
      'color:#C91E0C; text-decoration: none; font-weight:bold; }');
    StreamUtils.WriteLine(CurrentStream, 'a.bold:visited {' +
      'color:#7E5C31; text-decoration: none; font-weight:bold; }');
    StreamUtils.WriteLine(CurrentStream, 'a.bold:hover {' +
      'text-decoration: underline; font-weight:bold; }');
    StreamUtils.WriteLine(CurrentStream, 'a.bold:active {' +
      'text-decoration: underline; font-weight:bold; }');

    StreamUtils.WriteLine(CurrentStream, 'tr.list { background: #FFBF44; }');
    StreamUtils.WriteLine(CurrentStream, 'tr.list2 { background: #FFC982; }');
    StreamUtils.WriteLine(CurrentStream, 'tr.listheader { background: #C91E0C; }');
    StreamUtils.WriteLine(CurrentStream, 'th.listheader { color: white; }');

    StreamUtils.WriteLine(CurrentStream, 'a.section {' +
      'color: green; '+
      'text-decoration: none; '+
      'font-weight: bold; }');
    StreamUtils.WriteLine(CurrentStream, 'a.section:hover {' +
      'color: green; '+
      'text-decoration: underline; '+
      'font-weight: bold; }');
    StreamUtils.WriteLine(CurrentStream, 'td.itemname {' +
      'white-space:nowrap; }');
    StreamUtils.WriteLine(CurrentStream, 'div.nodescription {' +
      'color:red;}');
    StreamUtils.WriteLine(CurrentStream, 'dl.parameters {;}');
    StreamUtils.WriteLine(CurrentStream, 'dt.parameters {' +
      'color:blue;}');
    StreamUtils.WriteLine(CurrentStream, 'dd.parameters {;}');

    CloseStream;
  end;

  CreateStream('index.html', True);
  WriteLine(CurrentStream, '<html><head><title>'+Title+'</title>');
  WriteLine(CurrentStream, '</head><frameset cols="200,*" border="1">');
  WriteLine(CurrentStream, '<frame src="navigation.html"/>');
  WriteLine(CurrentStream, '<frame src="AllUnits.html" name="content"/>');
  WriteLine(CurrentStream, '</frameset></html>');
  CloseStream;

  CreateStream('navigation.html', True);
  WriteLine(CurrentStream, '<html><head>');
  WriteDirect('<link rel="StyleSheet" href="');
  WriteDirect(EscapeURL('pasdoc.css'));
  WriteDirect('"/>', true);
  WriteLine(CurrentStream, '</head>');
  WriteLine(CurrentStream, '<body class="navigationframe">');
  WriteDirect('<h2>'+Title+'</h2>');
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG
    + '" width="100%">', true);
  for i := 0 to NUM_OVERVIEW_FILES_USED - 1 do begin
    WriteDirect('<tr><td><a target="content" href="' + EscapeURL(OverviewFilenames[i] + GetFileExtension) + '" class="navigation">');
    case i of
      0: WriteConverted(FLanguage.Translation[trUnits]);
      1: WriteConverted(FLanguage.Translation[trClassHierarchy]);
      2: WriteConverted(FLanguage.Translation[trCio]);
      3: WriteConverted(FLanguage.Translation[trTypes]);
      4: WriteConverted(FLanguage.Translation[trVariables]);
      5: WriteConverted(FLanguage.Translation[trConstants]);
      6: WriteConverted(FLanguage.Translation[trFunctionsAndProcedures]);
      7: WriteConverted(FLanguage.Translation[trIdentifiers]);
    end;
    WriteDirect('</a></td></tr>', true);
  end;
  WriteDirect('</table>', true);
  WriteLine(CurrentStream, '</body></html>');
  CloseStream;
end;

function TTEXDocGenerator.ConvertString(const s: String): String;
const
  NumSpecials = 68;  
  Entities: array [1..NumSpecials] of string[12] =
    ('\c{C}','\c{c}',
     '\~{N}','\~{n}',
     '\~{A}','\~{a}',
     '\~{O}','\~{o}',
     '\''{A}','\`{A}',
     '\''{a}','\`{a}',
     '\''{o}','\`{o}',
     '\''{E}','\`{E}',
     '\''{e}','\`{e}',
     '\''{U}','\`{U}',
     '\''{u}','\`{u}',
     '\''{O}','\`{O}',
     '\''{I}','\`{I}',
     '\''{i}','\`{i}',
     '\AA',  ',\aa',
     '\"{A}','\"{a}',
     '\"{E}','\"{e}',
     '\"{U}','\"{u}',
     '\"{O}','\"{o}',
     '\"{I}','\"{i}',
     '\^{U}','\^{u}',
     '\^{I}','\^{i}',
     '\^{a}','\ae',
     '\''{y}','\"{y}',
     '\ss','\AE',
     '\^{E}','\^{e}',
     '\^{o}','\copyright',
     '\S','\pounds',
     '\P','?''',
     '\$','\&',
     '\%','\#',
     '\{','\}',
      '$>$','$<$',
      '\^{}','\verb|\|'
      
    );
  Specials: array [1..NumSpecials] of char =
     ('Ç','ç',
      'Ñ','ñ',
      'Ã','ã',
      'Õ','õ',
      'Á','À',
      'á','à',
      'ó','ò',
      'É','È',
      'é','è',
      'Ú','Ù',
      'ú','ù',
      'Ó','Ò',
      'Í','Ì',
      'í','ì',
      'Å','å',
      'Ä','ä',
      'Ë','ë',
      'Ü','ü',
      'Ö','ö',
      'Ï','ï',
      'Û','û',
      'Î','î',
      'â','æ',    
      'ý','ÿ',
      'ß','Æ',
      'Ê','ê',
      'ô','©',
      '§','£',
      '¶','¿',
      '$','&',
      '%','#',
      '{','}',
      '>','<',
      '^','\'
      );
      

    function Entity(const Special: Char): String;
    var
      i: Integer;
    begin
      Result := Special;
      for i := 1 to NumSpecials do
        if Specials[i] = Special then
        begin
          Result := Entities[i];
          break;
        end
    end;

var
  i: Integer;
  Ent: string;
begin
  i := 1;
  Result := s;
  while i <= length (Result) do
    if (ord(Result[i]) > 127) or (Result[i] in ['<','>','&','"']) then begin
      Ent := Entity(Result[i]);
      delete(Result, i, 1);
      System.Insert(Ent, Result, i);
      inc (i, Length (ent));
    end else begin
      inc(i);
    end;
end;

procedure TTexDocGenerator.BuildLinks;
begin
  FLinkCount := 1;
  inherited;
end;

function TTexDocGenerator.EscapeURL(const AString: string): string;
var
  i: Integer;
begin
  EscapeURL := AString;
end;

end.

