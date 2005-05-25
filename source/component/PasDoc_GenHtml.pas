{ @abstract(Provides HTML document generator object.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Alexander Lisnevsky (alisnevsky@yandex.ru))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Hendy Irawan (ceefour@gauldong.net))
  @author(Wim van der Vegt (wvd_vegt@knoware.nl))
  @author(Thomas W. Mueller <no-email>)
  @author(David Berg (HTML Layout) <david@sipsolutions.de>)
  @author(Grzegorz Skoczylas <gskoczylas@program.z.pl>)
  @author(Michalis Kamburelis)
  @cvs($Date$)

  Implements an object to generate HTML documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenHtml;

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  PasDoc_Types,
  Classes;

type
  { @abstract(generates HTML documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in HTML (HyperText Markup Language) format.
    This type of output is well suited to be read with a web browser at the
    computer, as a reference manual that does not have to be printed.
    For printed output, use @link(TTexDocGenerator). }
  THTMLDocGenerator = class(TDocGenerator)
  private
    FUseTipueSearch: boolean;
    
    { Writes line (using WriteDirect) with <meta http-equiv="Content-Type" ...>
      html element describing current charset (from FLanguage). }
    procedure WriteMetaContentType;
    
    function MakeLinkTarget(
      const href, caption, localcss, target: string): string;    
  protected
    FNumericFilenames: boolean;
    FWriteUses: boolean;
    FLinkCount: Integer;
    FFooter: string;
    { If specified, using external CSS file }
    FCSS: string;
    FHeader: string;
    FOddTableRow: Integer;
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

    procedure WriteItemDescription(const AItem: TPasItem);
    (*Writes the Item's DetailedDescription. If the Item also has
      AbstractDescription, this is also written in front of the 
      DetailedDescription.
      
      Code here will open and close paragraph for itself, so you shouldn't
      surround it inside WriteStart/EndOfParagraph, like
      @longcode(#
        { BAD EXAMPLE }
        WriteStartOfParagraph;
        WriteItemDetailedDescription(Item);
        WriteEndOfParagraph;
      #) *)
    procedure WriteItemDetailedDescription(const AItem: TPasItem);
    procedure WriteOverviewFiles;
    procedure WritePropertiesSummary(HL: integer; p: TPasProperties);

    { Writes an opening A element, including a name attribute given by the
      argument. }
    procedure WriteStartOfAnchor(const AName: string);
    procedure WriteStartOfDocument(AName: string);

    procedure WriteStartOfLink(const href: string); overload;
    procedure WriteStartOfLink(const href, localcss: string); overload;
    procedure WriteStartOfLink(const href, localcss, Target: string); overload;

    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph;

    procedure WriteStartOfTableCell; overload;
    procedure WriteStartOfTableCell(const localcss: string); overload;
    procedure WriteStartOfTableCell(const Params, localcss: string); overload;

    procedure WriteStartOfTable1Column(t: string);
    procedure WriteStartOfTable2Columns(t1, t2: string);
    { TODO: Such thing as Width100 should be better done by giving here css 
      classname and setting rest in css }
    procedure WriteStartOfTable2ColumnsExt(t1, t2: string; Width100: boolean);
    procedure WriteStartOfTable3Columns(t1, t2, T3: string);
    procedure WriteStartOfTableRow(const CssClass: string);
    { Writes the topic files for Html Help Generation }
    procedure WriteHtmlHelpProject;

    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);

    function ConvertString(const s: string): string; override;
    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): string; override;
    

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
    procedure WriteUnitUses(const HL: integer; U: TPasUnit);
    procedure WriteUnitDescription(HL: integer; U: TPasUnit); override;
    procedure WriteProperties(HL: integer; const p: TPasProperties); override;

    procedure WriteSpellChecked(const AString: string);

    function HtmlString(const S: string): string; override;
    // FormatPascalCode will cause Line to be formatted in
    // the way that Pascal code is formatted in Delphi.
    function FormatPascalCode(const Line: string): string; override;
    // FormatComment will cause AString to be formatted in
    // the way that comments other than compiler directives are
    // formatted in Delphi.  See: @link(FormatCompilerComment).
    function FormatComment(AString: string): string; override;
    // FormatKeyWord will cause AString to be formatted in
    // the way that strings are formatted in Delphi.
    function FormatString(AString: string): string; override;
    // FormatKeyWord will cause AString to be formatted in
    // the way that reserved words are formatted in Delphi.
    function FormatKeyWord(AString: string): string; override;
    // FormatCompilerComment will cause AString to be formatted in
    // the way that compiler directives are formatted in Delphi.
    function FormatCompilerComment(AString: string): string; override;
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

    { output the index.html and navigation.html files }
    procedure WriteFramesetFiles;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    procedure WriteImage(const src, alt, localcss: string);
    procedure WriteLink(const href, caption, localcss: string);
    procedure WriteLinkTarget(const href, caption, localcss, target: string);
    procedure WriteAnchor(const AName: string); overload;
    procedure WriteAnchor(const AName, Caption: string); overload;
    procedure WriteEndOfLink;
    
    function Paragraph: string; override;
    
    function LineBreak: string; override;
    
    function URLLink(const URL: string): string; override;
  public
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure LoadFooterFromFile(const AFileName: string);
    procedure LoadHeaderFromFile(const AFileName: string);
    procedure BuildLinks; override;

    function EscapeURL(const AString: string): string; virtual;
  published
    property HtmlHelp: boolean read FHtmlHelp write FHtmlHelp default false;
    property ContentsFile: string read FContentsFile write FContentsFile;
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property CSS: string read FCSS write FCSS;
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames
      default false;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses
      default false;

    property UseTipueSearch: boolean read FUseTipueSearch write FUseTipueSearch;
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
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  PasDoc,
  ObjectVector,
  StreamUtils,
  Utils,
  PasDoc_HierarchyTree,
  PasDoc_Tipue;

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

const
  DoctypeFrameset = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Frameset//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/frameset.dtd">';
  DoctypeNormal = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Transitional//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">';


function THTMLDocGenerator.HtmlString(const S: string): string; 
begin
  Result := S;
end;

function THTMLDocGenerator.FormatString(AString: string): string;
begin
  result := '<span class="pascal_string">' + AString + '</span>';
end;

function THTMLDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := '<span class="pascal_keyword">' + AString + '</span>';
end;

function THTMLDocGenerator.FormatComment(AString: string): string;
begin
  result := '<span class="pascal_comment">' + AString + '</span>';
end;

function THTMLDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := '<span class="pascal_compiler_comment">' + AString + '</span>';
end;

function THTMLDocGenerator.CodeString(const s: string): string;
begin
  Result := '<code>' + s + '</code>';
end;

function THTMLDocGenerator.CreateLink(const Item: TPasItem): string;
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

function THTMLDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result := '<a class="normal" href="' + EscapeURL(Link) + '">' + ItemName + '</a>';
end;

function THTMLDocGenerator.GetFileExtension: string;
begin
  Result := '.html';
end;

procedure THTMLDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  WriteDirect('<hr noshade size="1"><em>');
  WriteConverted(FLanguage.Translation[trGeneratedBy] + ' ');
  WriteLinkTarget(PASDOC_HOMEPAGE, PASDOC_NAME_AND_VERSION, '', '_parent');
  WriteConverted(' ' + FLanguage.Translation[trOnDateTime] + ' ' +
    FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
  WriteDirectLine('</em>');
end;

procedure THTMLDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
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
      WriteLink('mailto:' + EmailAddress, ConvertString(EmailAddress), '');
      WriteConverted(S2);
    end else begin
      WriteConverted(s);
    end;

    WriteEndOfParagraph;
  end;
end;

procedure THTMLDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);
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

  WriteDirectLine('<table class="sections"><tr>');
  for Section := Low(TSections) to High(TSections) do
    begin
      WriteDirect('<td>');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section], 'section')
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirect('</td>');
    end;
  WriteDirectLine('</tr></table>');

  WriteAnchor(SectionAnchors[dsDescription]);

  { write unit link }
  if Assigned(CIO.MyUnit) then begin
    WriteHeading(HL + 1, FLanguage.Translation[trUnit]);
    WriteLink(CIO.MyUnit.FullLink, ConvertString(CIO.MyUnit.Name), 'bold');
    WriteDirect('<br>');
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
      TheLink := SearchLink(s, CIO, '');
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

procedure THTMLDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
var
  i: Integer;
  p: TPasCio;
begin
  if c = nil then Exit;

  for i := 0 to c.Count - 1 do
  begin
    p := TPasCio(c.PasItemAt[i]);
    
    if (p.MyUnit <> nil) and
       p.MyUnit.FileNewerThanCache(DestinationDirectory + p.OutputFileName) then
    begin
      DoMessage(3, mtInformation, 'Data for "%s" was loaded from cache, '+
        'and output file of this item exists and is newer than cache, '+
        'skipped.', [p.Name]);
      Continue;
    end;
    
    case CreateStream(p.OutputFileName, true) of
      csError: begin
          DoMessage(1, mtError, 'Could not create Class/Interface/Object documentation file.', []);
          Continue;
        end;
      csCreated: begin
          DoMessage(3, mtInformation, 'Creating Class/Interface/Object file for "%s"...', [p.Name]);
          WriteCIO(HL, p);
        end;
    end;
  end;
  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  // if HtmlHelp then
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
    { Write only the AbstractDescription and do not opt for DetailedDescription,
      like WriteItemDescription does. }
    if p.AbstractDescription <> '' then
      WriteSpellChecked(p.AbstractDescription)
    else
      WriteDirect('&nbsp;');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

procedure THTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem; 
  const Code: string; const ItemLink: string);
begin
  WriteCodeWithLinksCommon(p, Code, ItemLink, '<b>', '</b>', 
    {$ifdef FPC}@{$endif} WriteLink);
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDates(const HL: integer; const Created,
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
    WriteConvertedLine(LastMod);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDocumentation;
begin
  StartSpellChecking('sgml');
  inherited;
  WriteUnits(1);
  WriteBinaryFiles;
  WriteOverviewFiles;
  WriteVisibilityLegendFile;
  WriteFramesetFiles;
  if UseTipueSearch then
  begin
    DoMessage(2, mtInformation, 
      'Writing additional files for tipue search engine', []);
    TipueAddFiles(Units, DestinationDirectory);
  end;
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteEmptyCell;
begin
  WriteDirect('&nbsp;');
end;

procedure THTMLDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('</body>');
  WriteDirectLine('</html>');
end;

procedure THTMLDocGenerator.WriteEndOfAnchor;
begin
  WriteDirect('</a>');
end;

procedure THTMLDocGenerator.WriteEndOfLink;
begin
  WriteDirect('</a>');
end;

procedure THTMLDocGenerator.WriteEndOfCode;
begin
  WriteDirect('</code>');
end;

procedure THTMLDocGenerator.WriteLink(const href, caption, localcss: string);
begin
  WriteLinkTarget(href, caption, localcss, '');
end;

function THTMLDocGenerator.MakeLinkTarget(
  const href, caption, localcss, target: string): string;
var
  s: string;
begin
  if css <> '' then
    s := Format('<a class="%s"', [localcss])
  else
    s := '<a class="normal"';
  if target <> '' then
    s := Format('%s target="%s"', [s, target]);
  Result := Format('%s href="%s">%s</a>', [s, EscapeURL(href), caption]);
end;

procedure THTMLDocGenerator.WriteLinkTarget(
  const href, caption, localcss, target: string);
begin
  WriteDirect(MakeLinkTarget(href, caption, localcss, target));
end;

procedure THTMLDocGenerator.WriteEndOfParagraph;
begin
  WriteDirectLine('</p>');
end;

procedure THTMLDocGenerator.WriteEndOfTableCell;
begin
  WriteDirectLine('</td>');
end;

procedure THTMLDocGenerator.WriteEndOfTable;
begin
  WriteDirectLine('</table>');
end;

procedure THTMLDocGenerator.WriteEndOfTableRow;
begin
  WriteDirectLine('</tr>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFields(const Order: integer; const Fields:
  TPasItems);
var
  j: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(Fields) then Exit;

  WriteAnchor('@Fields');
  WriteHeading(Order, FLanguage.Translation[trFields]);

  { TODO: this should call WriteStartOfTable* }
  FOddTableRow := 0;
  WriteDirect('<table class="fields" cellspacing="' +
    HTML_TABLE_CELLSPACING + '" cellpadding="' + HTML_TABLE_CELLPADNG +
    '" width="100%">');
  WriteDirect('<tr class="listheader">');
  WriteDirect('<th>&nbsp;</th><th class="listheader">');
  WriteConverted(FLanguage.Translation[trName]);
  WriteDirect('</th><th class="listheader">');
  WriteConverted(FLanguage.Translation[trDescription]);
  WriteDirectLine('</th></tr>');

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

procedure THTMLDocGenerator.WriteFooter;
begin
  WriteDirect(Footer);
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFuncsProcs(const HL: integer; const Methods:
  Boolean; const FuncsProcs: TPasMethods);

  procedure WriteMethodTableRow(p: TPasMethod; const ItemLink: string;
    MakeAnchor: boolean);
  begin
    WriteStartOfTableRow('');

    { Only write visibility for methods of classes and objects. }
    if Methods then WriteVisibilityCell(p);

    WriteStartOfTableCell('width="100%"', '');

    if MakeAnchor then WriteAnchor(p.Name);

    WriteCodeWithLinks(p, p.FullDeclaration, ItemLink);

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;

var
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;

  if Methods then begin
//    if HtmlHelp then
      WriteAnchor('@Methods');
    WriteHeading(HL, FLanguage.Translation[trMethods]);
  end
  else begin
//    if HtmlHelp then
      WriteAnchor('@FuncsProcs');
    WriteHeading(HL, FLanguage.Translation[trFunctionsAndProcedures]);
  end;

  // now resort the list alphabetically
  FuncsProcs.SortByPasItemName;

  // 1st pass: write the overview
  WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
  WriteStartOfTable1Column('');

  for j := 0 to FuncsProcs.Count - 1 do
  begin
    p := TPasMethod(FuncsProcs.PasItemAt[j]);

    s := p.FullLink;
    if Assigned(p.MyUnit) then
      if CompareText(p.MyUnit.FullLink, Copy(s, 1,
        Length(p.MyUnit.FullLink))) = 0 then
        Delete(s, 1, Length(p.MyUnit.FullLink));

    WriteMethodTableRow(p, s, false);
  end;

  WriteEndOfTable;

  // 2nd pass: write the detailed descriptions
  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);

  for j := 0 to FuncsProcs.Count - 1 do
  begin
    p := TPasMethod(FuncsProcs.PasItemAt[j]);

    WriteStartOfTable1Column('');
    WriteMethodTableRow(p, '', true);
    WriteEndOfTable;

    WriteItemDetailedDescription(p);
  end;
end;

procedure THTMLDocGenerator.WriteHeading(Level: integer; const s: string);
var
  c: string;
begin
  if (Level < 1) then Level := 1;
  if Level > 6 then begin
    DoMessage(2, mtWarning, 'HTML generator cannot write headlines of level 7 or greater; will use 6 instead.', []);
    Level := 6;
  end;
  c := IntToStr(Level);
  WriteDirect('<h' + c + '>');
  WriteConverted(s);
  WriteDirectLine('</h' + c + '>');
end;

{ ---------- }

procedure THTMLDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;

  if AItem.AbstractDescription <> '' then begin
    WriteSpellChecked(AItem.AbstractDescription);
  end else begin
    if AItem.DetailedDescription <> '' then begin
      WriteSpellChecked(AItem.DetailedDescription)
    end else begin
      WriteDirect('&nbsp;');
    end;
  end;
end;

procedure THTMLDocGenerator.WriteItemDetailedDescription(const AItem: TPasItem);

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
    List: TStringVector; LinkToParamNames: boolean);
    
    procedure WriteParameter(const ParamName: string; const Desc: string);
    begin
      WriteDirectLine('<dt class="parameters">');
      WriteDirect(ParamName);
      WriteDirectLine('</dt>');
      WriteDirectLine('<dd class="parameters">');
      WriteSpellChecked(Desc);
      WriteDirectLine('</dd>');
    end;
    
  var
    i: integer;
    s: string;
    ParamName: string;
  begin
    if StringVectorIsNilOrEmpty(List) then
      exit;

    WriteHeading(6, Caption);
    WriteDirectLine('<dl class="parameters">');
    for i := 0 to List.Count - 1 do begin
      s := List[i];

      { TODO -- splitting S to ParamName and the rest should be done
        inside TTagManager.Execute, not here. See analogous place in
        TTexDocgenerator.WriteParamsOrRaises for more comments
        about why & how. }
 
      ParamName := ExtractFirstWord(s);
      
      if LinkToParamNames then
       ParamName := SearchLinkOrWarning(ParamName, Func, '',
         'Could not resolve link to "%s" from description of item "%s"');
      
      WriteParameter(ParamName, s);
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteHeading(6, LowerCase(FLanguage.Translation[trReturns]));
    WriteDirect('<p class="return">');
    WriteSpellChecked(ReturnDesc);
    WriteDirect('</p>');
  end;

  procedure WriteHintDirective(const S: string);
  begin
    WriteDirect('<p class="hint_directive">');
    WriteConverted('Warning: ' + S + '.');
    WriteDirect('</p>');
  end;

var
  Ancestor: TPasItem;
  AncestorName: string;
  AItemMethod: TPasMethod;
begin
  if not Assigned(AItem) then Exit;
  
  if AItem.IsDeprecated then
    WriteHintDirective(FLanguage.Translation[trDeprecated]);
  if AItem.IsPlatformSpecific then
    WriteHintDirective(FLanguage.Translation[trPlatformSpecific]);
  if AItem.IsLibrarySpecific then
    WriteHintDirective(FLanguage.Translation[trLibrarySpecific]);  

  if AItem.AbstractDescription <> '' then
  begin
    WriteStartOfParagraph;
    WriteSpellChecked(AItem.AbstractDescription);

    if AItem.DetailedDescription <> '' then
    begin
      if not AItem.AbstractDescriptionWasAutomatic then
      begin
        WriteEndOfParagraph; { always try to write closing </p>, to be clean }
        WriteStartOfParagraph;
      end;
      WriteSpellChecked(AItem.DetailedDescription);
    end;
    
    WriteEndOfParagraph;
  end else begin
    if AItem.DetailedDescription <> '' then
    begin
      WriteStartOfParagraph;
      WriteSpellChecked(AItem.DetailedDescription);
      WriteEndOfParagraph;
    end else 
    begin
      if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := SearchItem(AncestorName, AItem);
        if Assigned(Ancestor) then
          begin
            WriteDirect('<div class="nodescription">');
            WriteConverted(Format('no description available, %s description follows', [AncestorName]));
            WriteDirect('</div>');
            WriteItemDetailedDescription(Ancestor);
          end;
      end else begin
        WriteDirect('&nbsp;');
      end;
    end;
  end;
  
  if AItem is TPasMethod then
  begin
    AItemMethod := TPasMethod(AItem);
    WriteParamsOrRaises(AItemMethod, 
      LowerCase(FLanguage.Translation[trParameters]), AItemMethod.Params, false);
    WriteReturnDesc(AItemMethod, AItemMethod.Returns);
    WriteParamsOrRaises(AItemMethod, 
      LowerCase(FLanguage.Translation[trExceptions]), AItemMethod.Raises, true);
 end;
end;

procedure THTMLDocGenerator.WriteItems(HL: integer; Heading: string; const
  Anchor: string; const i: TPasItems);
var
  j, k: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(i) then Exit;

  if HtmlHelp and (Anchor <> '') then
    WriteAnchor('@' + Anchor);

  WriteHeading(HL, Heading);

  { TODO: this should call WriteStartOfTable* }
  FOddTableRow := 0;
  WriteDirect('<table class="itemlist" cellspacing="' +
    HTML_TABLE_CELLSPACING + '" cellpadding="' + HTML_TABLE_CELLPADNG +
    '" width="100%">');
  WriteDirect('<tr class="listheader">');
  WriteDirect('<th class="listheader" nowrap="nowrap">');
  WriteConverted(FLanguage.Translation[trName]);
  WriteDirect('</th><th class="listheader" width="100%">');
  WriteConverted(FLanguage.Translation[trDescription]);
  WriteDirectLine('</th></tr>');

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
      WriteDirectLine('<ul>');
      for k := 0 to TPasEnum(Item).Members.Count-1 do begin
        WriteDirectLine('<li>');
        WriteConverted(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).Name);
        WriteConverted(': ');
        WriteSpellChecked(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).GetDescription);
        WriteDirectLine('</li>');
      end;
      WriteDirectLine('</ul>');
    end;
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  WriteDirect('</table>');
end;

{ ---------- }

procedure THTMLDocGenerator.WriteOverviewFiles;

  function CreateOverviewStream(Overview: TCreatedOverviewFile): boolean;
  var BaseFileName, Headline: string;
  begin
    BaseFileName := OverviewFilesInfo[Overview].BaseFileName;
    Result := CreateStream(BaseFileName + GetFileExtension, True) <> csError;
    
    if not Result then
    begin
      DoMessage(1, mtError, 'Error: Could not create output file "' +
        BaseFileName + '".', []);
      Exit;
    end;
    
    DoMessage(3, mtInformation, 'Writing overview file "' +
      BaseFileName + '" ...', []);

    Headline := FLanguage.Translation[
      OverviewFilesInfo[Overview].TranslationHeadlineId];
    WriteStartOfDocument(Headline);
    WriteHeading(1, Headline);  
  end;

  { Creates an output stream that lists up all units and short descriptions. }
  procedure WriteUnitOverviewFile;
  var
    c: TPasItems;
    Item: TPasItem;
    j: Integer;
  begin
    c := Units;
    
    if not CreateOverviewStream(ofUnits) then Exit;
    
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

  { Writes a Hireachy list - this is more useful than the simple class list }
  procedure WriteHierarchy;
  var
    Level, OldLevel: Integer;
    Node: TPasItemNode;
  begin
    CreateClassHierarchy;

    if not CreateOverviewStream(ofClassHierarchy) then Exit;

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
          WriteDirect('<ul>')
        else
          while Level < OldLevel do begin
            WriteDirect('</ul>');
            Dec(OldLevel);
          end;
        OldLevel := Level;

        WriteDirect('<li>');
        if Node.Item = nil then
          WriteConverted(Node.Name) else
          WriteLink(Node.Item.FullLink, ConvertString(Node.Name), 'bold');
        { We can't simply write here an explicit '</li>' because current 
          list item may be not finished yet (in case next Nodes 
          (with larger Level) will follow in the FClassHierarchy). }

        Node := FClassHierarchy.NextItem(Node);
      end;

      while OldLevel > 0 do begin
        WriteDirect('</ul>');
        Dec(OldLevel);
      end;
    end;

    WriteFooter;
    WriteAppInfo;
    WriteEndOfDocument;

    CloseStream;
  end;

var
  ItemsToCopy: TPasItems;
  PartialItems: TPasItems;
  TotalItems: TPasItems; // Collect all Items for final listing.
  Item: TPasItem;
  j: Integer;
  PU: TPasUnit;
  Overview: TCreatedOverviewFile;
begin
  if HtmlHelp then
    WriteHtmlHelpProject;

  WriteUnitOverviewFile;
  WriteHierarchy;

  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  // Make sure we don't free the Itmes when we free the container.
  TotalItems := TPasItems.Create(False);

  for Overview := ofCios to HighCreatedOverviewFile do
  begin
    if not CreateOverviewStream(Overview) then Exit;

      // Make sure we don't free the Itmes when we free the container.
    PartialItems := TPasItems.Create(False);

    for j := 0 to Units.Count - 1 do
    begin
      PU := Units.UnitAt[j];
      case Overview of
        ofCIos                  : ItemsToCopy := PU.CIOs;
        ofTypes                 : ItemsToCopy := PU.Types;
        ofVariables             : ItemsToCopy := PU.Variables;
        ofConstants             : ItemsToCopy := PU.Constants;
        ofFunctionsAndProcedures: ItemsToCopy := PU.FuncsProcs;
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
    WriteAppInfo;
    WriteEndOfDocument;
    CloseStream;
  end;

  CreateOverviewStream(ofIdentifiers);
  
  WriteStartOfTable3Columns(
    FLanguage.Translation[trName],
    FLanguage.Translation[trUnit],
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

procedure THTMLDocGenerator.WriteProperties(HL: integer; const p:
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
    WriteCodeWithLinks(Prop, Prop.FullDeclaration, '');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
    WriteEndOfTable;

    WriteItemDetailedDescription(Prop);
  end;
end;

procedure THTMLDocGenerator.WritePropertiesSummary(HL: integer; p:
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
    WriteStartOfTableCell('width="100%"', '');

    WriteCodeWithLinks(Prop, Prop.FullDeclaration, Prop.FullLink);

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteAnchor(const AName: string);
begin
  WriteAnchor(AName, '');
end;

procedure THTMLDocGenerator.WriteAnchor(const AName, Caption: string);
begin
  WriteDirect(Format('<a name="%s">%s</a>', [AName, Caption]));
end;

procedure THTMLDocGenerator.WriteStartOfAnchor(const AName: string);
begin
  WriteDirect('<a name="' + AName + '">');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfCode;
begin
  WriteDirect('<code>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteMetaContentType;
begin
  if FLanguage.CharSet <> '' then begin
    WriteDirect('<meta http-equiv="content-type" content="text/html; charset=' 
      + FLanguage.CharSet + '">', true);
  end;
end;

procedure THTMLDocGenerator.WriteStartOfDocument(AName: string);
begin
  WriteDirectLine(DoctypeNormal);
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  WriteDirectLine('<meta name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  WriteMetaContentType;
  // Title
  WriteDirect('<title>');
  if {not HtmlHelp and}(Title <> '') then begin
    WriteConverted(Title + ': ');
  end;
  WriteConverted(AName);
  WriteDirectLine('</title>');
  // StyleSheet
  WriteDirect('<link rel="StyleSheet" type="text/css" href="');
  WriteDirect(EscapeURL('pasdoc.css'));
  WriteDirectLine('">');

  WriteDirectLine('</head>');
  WriteDirectLine('<body bgcolor="#ffffff" text="#000000" link="#0000ff" vlink="#800080" alink="#FF0000">');

  if Length(Header) > 0 then begin
    WriteSpellChecked(Header);
  end;
end;

procedure THTMLDocGenerator.WriteStartOfLink(const href, localcss, Target: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<a class="%s"', [localcss])
  else
    s := '<a class="normal"';
  if target <> '' then
    s := Format('%s target="%s"', [s, target]);
  WriteDirect(Format('%s href="%s">', [s, EscapeURL(href)]));
end;

procedure THTMLDocGenerator.WriteStartOfLink(const href, localcss: string);
begin
  WriteStartOfLink(href, localcss, '');
end;

procedure THTMLDocGenerator.WriteStartOfLink(const href: string);
begin
  WriteStartOflink(href, '', '');
end;

procedure THTMLDocGenerator.WriteStartOfParagraph;
begin
  WriteDirect('<p>');
end;

procedure THTMLDocGenerator.WriteStartOfTable1Column(t: string);
begin
  FOddTableRow := 0;
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG + '" width="100%">', true);
end;

procedure THTMLDocGenerator.WriteStartOfTable2ColumnsExt(t1, t2: string;
  Width100: boolean);
begin
  FOddTableRow := 0;
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG + '"');
  if Width100 then
    WriteDirect(' width="100%"');
  WriteDirectLine('>');
  WriteDirect('<tr class="listheader"><th class="listheader">');
  WriteConverted(t1);
  WriteDirect('</th><th class="listheader">');
  WriteConverted(t2);
  WriteDirectLine('</th></tr>');
end;

procedure THTMLDocGenerator.WriteStartOfTable2Columns(t1, t2: string);
begin
  WriteStartOfTable2ColumnsExt(t1, t2, true);
end;

procedure THTMLDocGenerator.WriteStartOfTable3Columns(t1, t2, T3: string);
begin
  FOddTableRow := 0;
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG + '" width="100%">', true);
  WriteDirect('<tr class="listheader"><th class="listheader">');
  WriteConverted(t1);
  WriteDirect('</th><th class="listheader">');
  WriteConverted(t2);
  WriteDirect('</th><th class="listheader">');
  WriteConverted(T3);
  WriteDirectLine('</th></tr>');
end;

procedure THTMLDocGenerator.WriteStartOfTableCell(const Params, localcss: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<td class="%s"',[localcss])
  else
    s := '<td';
  if Params <> '' then
    s := s + ' ' + Params;
  WriteDirect(s+'>');
end;

procedure THTMLDocGenerator.WriteStartOfTableCell(const localcss: string);
begin
  WriteStartOfTableCell('', localcss);
end;

procedure THTMLDocGenerator.WriteStartOfTableCell;
begin
  WriteStartOfTableCell('', '');
end;

procedure THTMLDocGenerator.WriteStartOfTableRow(const CssClass: string);
var
  s: string;
begin
  if CssClass <> '' then begin
    s := Format('<tr class="%s"', [CssClass])
  end else begin
    s := '<tr class="list';
    if FOddTableRow = 1 then begin
      s := s + '2';
    end;
    FOddTableRow := (FOddTableRow + 1) mod 2;
    s := s + '"';
  end;
  WriteDirect(s + ' valign="top">');
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

procedure THTMLDocGenerator.WriteHtmlHelpProject;
var
  DefaultContentsWritten: Boolean;
  DefaultTopic: string;

  procedure WriteLiObject(const Name, Local: string);
  begin
    WriteDirectLine('<li><object type="text/sitemap">');
    WriteDirectLine('<param name="Name" value="' + Name + '">');
    if Local <> '' then begin
      WriteDirectLine('<param name="Local" value="' + Local + '">');
      if DefaultTopic = '' then
        DefaultTopic := Local;
    end;
    WriteDirectLine('</object>');
  end;

  { ---------- }

  procedure WriteItemCollection(const _Filename: string; const c: TPasItems);
  var
    i: Integer;
    Item: TPasItem;
  begin
    if Assigned(c) then begin
      WriteDirectLine('<ul>');
      for i := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[i];
        WriteLiObject(Item.Name, _Filename + '#' + Item.Name);
      end;
      WriteDirectLine('</ul>');
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
    WriteDirectLine('<ul>');

    WriteItemHeadingCollection(fLanguage.Translation[trFields], ClassItem.FullLink, '@Fields', ClassItem.Fields);
    WriteItemHeadingCollection(fLanguage.Translation[trProperties], ClassItem.FullLink, '@Properties', ClassItem.Properties);
    WriteItemHeadingCollection(fLanguage.Translation[trMethods], ClassItem.FullLink, '@Methods', ClassItem.Methods);

    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteUnits(const Text: string);
  var
    c: TPasItems;
    j, k: Integer;
    PU: TPasUnit;
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilesInfo[ofUnits].BaseFileName + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trUnits], OverviewFilesInfo[ofUnits].BaseFileName +
        GetFileExtension);
    WriteDirectLine('<ul>');

    // Iterate all Units
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      WriteLiObject(PU.Name, PU.FullLink);
      WriteDirectLine('<ul>');

        // For each unit, write classes (if there are any).
      c := PU.CIOs;
      if Assigned(c) then begin
        WriteLiObject(FLanguage.Translation[trClasses], PU.FullLink + '#@Classes');
        WriteDirectLine('<ul>');

        for k := 0 to c.Count - 1 do
          InternalWriteCIO(TPasCio(c.PasItemAt[k]));

        WriteDirectLine('</ul>');
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

      WriteDirectLine('</ul>');
    end;
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteClasses(const Text: string);
  var
    c: TPasItems;
    j: Integer;
    PU: TPasUnit;
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofCios].BaseFileName + GetFileExtension;
    
    // Write Classes to Contents
    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trClasses], FileName);
    WriteDirectLine('<ul>');

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
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteClassHierarchy(const Text: string);
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofClassHierarchy].BaseFileName + 
      GetFileExtension;
    
    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trClassHierarchy], FileName);
  end;

  { ---------- }

  procedure ContentWriteOverview(const Text: string);

    procedure WriteParam(Id: TTranslationId);
    begin
      WriteDirect('<param name="Name" value="');
      WriteConverted(FLanguage.Translation[Id]);
      WriteDirectLine('">');
    end;

  var
    Overview: TCreatedOverviewFile;
  begin
    if Text <> '' then
      WriteLiObject(Text, '')
    else
      WriteLiObject(FLanguage.Translation[trOverview], '');
    WriteDirectLine('<ul>');
    for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    begin
      WriteDirectLine('<li><object type="text/sitemap">');
      WriteParam(OverviewFilesInfo[Overview].TranslationHeadlineId);
      WriteDirect('<param name="Local" value="');
      WriteConverted(OverviewFilesInfo[Overview].BaseFileName + GetFileExtension);
      WriteDirectLine('">');
      WriteDirectLine('</object>');
    end;
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteLegend(const Text: string);
  var
    FileName: string;
  begin
    FileName := 'Legend' + GetFileExtension;
    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trLegend], FileName);
  end;

  { ---------- }

  procedure ContentWriteGVUses();
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofGraphVizUses].BaseFileName + 
      LinkGraphVizUses;
      
    if LinkGraphVizUses <> '' then
      WriteLiObject(FLanguage.Translation[trGvUses], FileName);
  end;

  { ---------- }

  procedure ContentWriteGVClasses();
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofGraphVizClasses].BaseFileName + 
      LinkGraphVizClasses;
      
    if LinkGraphVizClasses <> '' then
      WriteLiObject(FLanguage.Translation[trGvClasses], FileName);
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
  Overview: TCreatedOverviewFile;
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
  WriteDirectLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  WriteDirect('<meta name="GENERATOR" content="' +
    PASDOC_NAME_AND_VERSION + '">', true);
  WriteDirectLine('</head><body>');
  WriteDirectLine('<ul>');

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
          WriteDirectLine('<ul>');
          Inc(CurrentLevel);
          ContentWriteCustom(Text, Link)
        end
        else
          if CurrentLevel > Level then begin
            WriteDirectLine('</ul>');
            Dec(CurrentLevel);
            while CurrentLevel > Level do begin
              WriteDirectLine('</ul>');
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
    ContentWriteGVClasses();
    ContentWriteGVUses();
  end;

  // End of File
  WriteDirectLine('</ul>');
  WriteDirectLine('</body></html>');
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

  WriteDirectLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  WriteDirectLine('<meta name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  WriteDirectLine('</head><body>');
  WriteDirectLine('<ul>');

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
        WriteDirectLine('<ul>');

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
        WriteDirectLine('</ul>');
      end;

      Inc(j);
    end;

      // Don't forget to write the last item. Can it ever by nil?
    WriteLiObject(Item.Name, Item.FullLink);
  end;

  c.Free;

  WriteDirectLine('</ul>');
  WriteDirectLine('</body></html>');
  CloseStream;

  // Create a HTML Help Project File
  if CreateStream(ProjectName + '.hhp', True) = csError then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Project file "%s.hhp' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing Html Help Project file "%s"...',
    [ProjectName]);

  WriteDirectLine('[OPTIONS]');
  WriteDirectLine('Binary TOC=Yes');
  WriteDirectLine('Compatibility=1.1 or later');
  WriteDirectLine('Compiled file=' + ProjectName + '.chm');
  WriteDirectLine('Contents file=' + ProjectName + '.hhc');
  WriteDirectLine('Default Window=Default');
  WriteDirectLine('Default topic=' + DefaultTopic);
  WriteDirectLine('Display compile progress=Yes');
  WriteDirectLine('Error log file=' + ProjectName + '.log');
  WriteDirectLine('Full-text search=Yes');
  WriteDirectLine('Index file=' + ProjectName + '.hhk');
  if Title <> '' then
    WriteDirectLine('Title=' + Title)
  else
    WriteDirectLine('Title=' + ProjectName);

  WriteDirectLine('');
  WriteDirectLine('[WINDOWS]');
  if Title <> '' then
    WriteDirect('Default="' + Title + '","' + ProjectName +
      '.hhc","' + ProjectName + '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true)
  else
    WriteDirect('Default="' + ProjectName + '","' +
      ProjectName + '.hhc","' + ProjectName +
      '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true);

  WriteDirectLine('');
  WriteDirectLine('[FILES]');

  { HHC seems to know about the files by reading the Content and Index.
    So there is no need to specify them in the FILES section. }

  WriteDirectLine('Legend.html');

  if (LinkGraphVizClasses <> '') then
    WriteDirectLine(OverviewFilesInfo[ofGraphVizClasses].BaseFileName + '.' +
      LinkGraphVizClasses);
    
  if LinkGraphVizUses <> '' then
    WriteDirectLine(OverviewFilesInfo[ofGraphVizUses].BaseFileName + '.' + 
      LinkGraphVizUses);

  for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    WriteDirectLine(OverviewFilesInfo[Overview].BaseFileName + '.html');

  if Assigned(Units) then
    for k := 0 to units.Count - 1 do
      begin
        Item := units.PasItemAt[k];
        PU := TPasUnit(units.PasItemAt[k]);
        WriteDirectLine(Item.FullLink);
        c := PU.CIOs;
        if Assigned(c) then
          for l := 0 to c.Count - 1 do
            begin
              Item2 := TPasCio(c.PasItemAt[l]);
              WriteDirectLine(Item2.OutputFilename);
            end;
      end;

  WriteDirectLine('');

  WriteDirectLine('[INFOTYPES]');

  WriteDirectLine('');

  WriteDirectLine('[MERGE FILES]');

  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
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
      'THTMLDocGenerator.WriteUnit: Unit variable has not been initialized.',
      []);
    Exit;
  end;
  
  if U.FileNewerThanCache(DestinationDirectory + U.OutputFileName) then
  begin
    DoMessage(3, mtInformation, 'Data for unit "%s" was loaded from cache, '+
      'and output file of this unit exists and is newer than cache, '+
      'skipped.', [U.Name]);
    Exit;
  end;

  case CreateStream(U.OutputFileName, true) of
    csError: begin
      DoMessage(1, mtError, 'Could not create HTML unit doc file for unit %s.', [U.Name]);
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

  WriteDirectLine('<table class="sections"><tr>');
  for Section := Low(TSections) to High(TSections) do
    begin
      WriteDirect('<td>');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section], 'section')
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirect('</td>');
    end;
  WriteDirectLine('</tr></table>');

  WriteAnchor(SectionAnchors[dsDescription]);
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

procedure THTMLDocGenerator.WriteUnitDescription(HL: integer; U: TPasUnit);
begin
  WriteHeading(HL, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(U);
end;

procedure THTMLDocGenerator.WriteImage(const src, alt, localcss: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<img class="%s"', [localcss])
  else
    s := '<img border="0"';
  WriteDirect(Format('%s src="%s" alt="%s" title="%s">', [s, src, alt, alt]));
end;

procedure THTMLDocGenerator.WriteVisibilityCell(const Item: TPasItem);

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

procedure THTMLDocGenerator.WriteVisibilityLegendFile;

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

  WriteStartOfTable2ColumnsExt(
    { TODO -otwm : needs translation } 'Marker',
    { TODO -otwm : needs translation } 'Visibility',
    false);

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

procedure THTMLDocGenerator.LoadFooterFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FFooter);
end;

procedure THTMLDocGenerator.LoadHeaderFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FHeader);
end;

procedure THTMLDocGenerator.WriteUnitUses(const HL: integer; U: TPasUnit);
var
  i: Integer;
  ULink: TPasItem;
begin
  if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
    WriteHeading(HL, 'uses');
    WriteDirect('<ul>');
    for i := 0 to U.UsesUnits.Count-1 do begin
      WriteDirect('<li>');
      ULink := TPasUnit(U.UsesUnits.Objects[i]);
      if ULink <> nil then begin
        WriteLink(ULink.FullLink, U.UsesUnits[i], 'bold');
      end else begin
        WriteDirect(U.UsesUnits[i]);
      end;
      WriteDirect('</li>');
    end;   
    WriteDirect('</ul>');
  end;
end;

procedure THTMLDocGenerator.WriteSpellChecked(const AString: string);
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

procedure THTMLDocGenerator.WriteBinaryFiles;

  procedure WriteGifFile(const Img: array of byte; const Filename: string);
  begin
    if CreateStream(Filename, True) = csError 
      then begin
        DoMessage(1, mtError, 'Could not create output file "%s".', [Filename]);
      Exit;
    end;
    CurrentStream.Write(img[0], High(img)+1);
    CloseStream;
  end;

var
  { If external CSS file specified, copy set CSS file to pasdoc.css.
    Fin: open external file, Fout: target file (pasdoc.css). }
  Fin, Fout:TFileStream; 
  PasdocCssFileName: string;
begin
  WriteGifFile(img_automated, 'automated.gif');
  WriteGifFile(img_private, 'private.gif');
  WriteGifFile(img_protected, 'protected.gif');
  WriteGifFile(img_public, 'public.gif');
  WriteGifFile(img_published, 'published.gif');
  
  PasdocCssFileName := DestinationDirectory + 'pasdoc.css';

  if not FileExists(PasdocCssFileName) then begin
    { If external CSS specified, copy it to pasdoc.css file. }
    if FCSS <> '' then begin
      FIn := TFileStream.Create(FCSS, fmOpenRead);
      FOut := TFileStream.Create(PasdocCssFileName, fmCreate or fmOpenWrite);
      FOut.CopyFrom(FIn, FIn.Size);
      FIn.Free;
      FOut.Free;
    end else begin
      CreateStream('pasdoc.css', True);
      WriteDirectLine('body {' +
        'font-family: Verdana,Arial;' +
        'color: black;' +
        'background-color: white; font-size: 12px; }');
      WriteDirectLine('body.navigationframe {' +
        'font-family: Verdana,Arial;' +
        'color: white;' +
        'background-color: #787878; font-size: 12px; }');

      WriteDirectLine('a.navigation:link {' +
        'color: white; text-decoration: none;  font-size: 12px;}');
      WriteDirectLine('a.navigation:visited {' +
        'color: white; text-decoration: none;  font-size: 12px;}');
      WriteDirectLine('a.navigation:hover {' +
        'color: white;' +
        'font-weight: bold; text-decoration: none;  font-size: 12px;}');
      WriteDirectLine('a.navigation:active {' +
        'color: white; text-decoration: none;  font-size: 12px;}');

      WriteDirectLine('a.normal:link {' +
        'color:#C91E0C; text-decoration: none; }');
      WriteDirectLine('a.normal:visited {' +
        'color:#7E5C31; text-decoration: none; }');
      WriteDirectLine('a.normal:hover {' +
        'text-decoration: underline; }');
      WriteDirectLine('a.normal:active {' +
        'text-decoration: underline; }');

      WriteDirectLine('a.bold:link {' +
        'color:#C91E0C; text-decoration: none; font-weight:bold; }');
      WriteDirectLine('a.bold:visited {' +
        'color:#7E5C31; text-decoration: none; font-weight:bold; }');
      WriteDirectLine('a.bold:hover {' +
        'text-decoration: underline; font-weight:bold; }');
      WriteDirectLine('a.bold:active {' +
        'text-decoration: underline; font-weight:bold; }');

      WriteDirectLine('tr.list { background: #FFBF44; }');
      WriteDirectLine('tr.list2 { background: #FFC982; }');
      WriteDirectLine('tr.listheader { background: #C91E0C; }');
      WriteDirectLine('th.listheader { color: white; }');

      WriteDirectLine('a.section {' +
        'color: green; '+
        'text-decoration: none; '+
        'font-weight: bold; }');
      WriteDirectLine('a.section:hover {' +
        'color: green; '+
        'text-decoration: underline; '+
        'font-weight: bold; }');
      WriteDirectLine('td.itemname {' +
        'white-space:nowrap; }');
      WriteDirectLine('div.nodescription {' +
        'color:red;}');
      WriteDirectLine('dl.parameters {;}');
      WriteDirectLine('dt.parameters {' +
        'color:blue;}');
      WriteDirectLine('dd.parameters {;}');

      { Style applied to Pascal code in documentation 
        (e.g. produced by @longcode tag) }
      WriteDirectLine(
        'span.pascal_string { color: #000080; }');
      WriteDirectLine(
        'span.pascal_keyword { font-weight: bolder; }');
      WriteDirectLine(
        'span.pascal_comment { color: #000080; font-style: italic; }');
      WriteDirectLine(
        'span.pascal_compiler_comment { color: #008000; }');

      WriteDirectLine(
        'p.hint_directive { color: red; }');

      CloseStream;
    end;
  end else
    DoMessage(2, mtInformation, '"%s" file already exists, not overwriting', 
      [PasdocCssFileName]);
end;

procedure THTMLDocGenerator.WriteFramesetFiles;

  procedure LocalWriteLink(const Filename: string; CaptionId: TTranslationID);
  begin
    WriteDirect('<tr><td><a target="content" href="' + EscapeURL(Filename) + '" class="navigation">');
    WriteConverted(FLanguage.Translation[CaptionId]);
    WriteDirectLine('</a></td></tr>');
  end;

var
  Overview: TCreatedOverviewFile;
begin
  CreateStream('index.html', True);
  WriteDirectLine(DoctypeFrameset);
  WriteDirectLine('<html><head>');
  WriteMetaContentType;
  WriteDirectLine('<title>'+Title+'</title>');
  WriteDirectLine('</head><frameset cols="200,*">');
  WriteDirectLine('<frame src="navigation.html" frameborder="0">');
  WriteDirectLine('<frame src="AllUnits.html" frameborder="0" name="content">');
  WriteDirectLine('</frameset></html>');
  CloseStream;

  CreateStream('navigation.html', True);
  WriteDirectLine(DoctypeNormal);
  WriteDirectLine('<html><head>');
  WriteDirect('<link rel="StyleSheet" type="text/css" href="');
  WriteDirect(EscapeURL('pasdoc.css'));
  WriteDirectLine('">');
  WriteMetaContentType;
  WriteDirectLine('<title>Navigation</title>');
  if UseTipueSearch then
    WriteDirect(TipueSearchButtonHead);
  WriteDirectLine('</head>');
  WriteDirectLine('<body class="navigationframe">');
  WriteDirect('<h2>'+Title+'</h2>');
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG
    + '" width="100%">', true);
    
  for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    LocalWriteLink(
      OverviewFilesInfo[Overview].BaseFileName + GetFileExtension,
      OverviewFilesInfo[Overview].TranslationId);
      
  if LinkGraphVizUses <> '' then
    LocalWriteLink(
      OverviewFilesInfo[ofGraphVizUses].BaseFileName + '.' + LinkGraphVizUses,
      OverviewFilesInfo[ofGraphVizUses].TranslationId);

  if LinkGraphVizClasses <> '' then
    LocalWriteLink(
      OverviewFilesInfo[ofGraphVizClasses].BaseFileName + '.' + LinkGraphVizClasses,
      OverviewFilesInfo[ofGraphVizClasses].TranslationId);  

  if UseTipueSearch then
    WriteDirect('<tr><td>' + TipueSearchButton + '</td></tr>');
    
  WriteDirectLine('</table>');
  WriteDirectLine('</body></html>');
  CloseStream;
end;

function THTMLDocGenerator.ConvertString(const S: String): String;
const
  ReplacementArray: array[0..5] of TCharReplacement = (
    (cChar: '<'; sSpec: '&lt;'),
    (cChar: '>'; sSpec: '&gt;'),
    (cChar: '&'; sSpec: '&amp;'),
    (cChar: '"'; sSpec: '&quot;'),
    (cChar: '^'; sSpec: '&circ;'),
    (cChar: '~'; sSpec: '&tilde;')
  );
begin
  Result := StringReplaceChars(S, ReplacementArray);
end;

function THTMLDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;


procedure THTMLDocGenerator.BuildLinks;
begin
  FLinkCount := 1;
  inherited;
end;

function THTMLDocGenerator.EscapeURL(const AString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AString) do
  begin
    { Kambi: It's obvious that we must escape '&'.
      I don't know why, but escaping it using '%26' does not work
      (tested with Mozilla 1.7.7, Firefox 1.0.3, Konqueror 3.3.2, 
      and finally even IE, so it's certainly not a bug of some browser).
      But escaping it using '&amp;' works OK.
      
      On the other hand, escaping '~' using '&tilde;' does not work.
      (So EscapeURL function still *must* be something different than 
      ConvertString.) }
      
    if AString[i] = '&' then
      Result := Result + '&amp;' else
    if AString[i] in [Chr($21)..Chr($7E)] then
      Result := Result + AString[i] else
      Result := Result + '%' + IntToHex(Ord(AString[i]), 2);
  end;
end;

function THTMLDocGenerator.FormatPascalCode(const Line: string): string;
begin
  { Why these </p> and <p> are needed ?
    Well, basic idea is that pasdoc should always try to make closing
    and opening tags explicit, even though they can be omitted for paragraphs 
    in html. And paragraph must end before <pre> and if there is any text after
    </pre> than new paragraph must be opened.
    
    Besides the feeling of being "clean", specifying explicit paragraph
    endings is also important because IE sometimes reacts stupidly
    when paragraph is not explicitly closed, see
    [http://sourceforge.net/mailarchive/message.php?msg_id=11388479].
    In order to fix it, WriteItemDetailedDescription always wraps
    what it writes between <p> ... </p>

    This works perfectly except for the cases where @longcode
    is at the end of description, then we have 
      <p>Some text <pre>Some Pascal code</pre></p>
    Because there is no text between "</pre>" and "</p>" this means
    that paragraph is not implicitly opened there. This, in turn,
    means that html validator complains that we have </p> without
    opening a paragraph. 
    
    So the clean solution must be to mark explicitly that paragraph
    always ends before <pre> and always begins after </pre>. }

  result := '</p><pre class="longcode">' + 
    inherited FormatPascalCode(ConvertString(Line)) + '</pre><p>';
end;

function THTMLDocGenerator.Paragraph: string; 
begin
  Result := '<p>';
end;

function THTMLDocGenerator.LineBreak: string; 
begin
  Result := '<br>';
end;

function THTMLDocGenerator.URLLink(const URL: string): string; 
begin
  Result := MakeLinkTarget(URL, ConvertString(URL), '', '_parent');
end;

end.