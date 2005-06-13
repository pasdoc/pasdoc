{ @abstract(Provides HTML document generator object.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Alexander Lisnevsky (alisnevsky@yandex.ru))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Hendy Irawan (ceefour@gauldong.net))
  @author(Wim van der Vegt (wvd_vegt@knoware.nl))
  @author(Thomas Mueller (www.dummzeuch.de))
  @author(David Berg (HTML Layout) <david@sipsolutions.de>)
  @author(Grzegorz Skoczylas <gskoczylas@program.z.pl>)
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
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
    output in HTML (HyperText Markup Language) format. }
  TGenericHTMLDocGenerator = class(TDocGenerator)
  private
    FUseTipueSearch: boolean;
    FNumericFilenames: boolean;
    FLinkCount: Integer;
    FFooter: string;
    { The content of the CSS file. }
    FCSS: string;
    FHeader: string;
    FOddTableRow: boolean;
    
    { Returns line with <meta http-equiv="Content-Type" ...>
      describing current charset (from FLanguage). }
    function MetaContentType: string;
    { makes a link with a target frame
      @param href is the link's reference
      @param caption is the link's text
      @param CssClass is the link's CSS class
      @param TargetFrame is the link's target frame (or empty) }
    function MakeTargettedLink(
      const href, caption, CssClass, TargetFrame: string): string;
      
    { Used by WriteItemsSummary and WriteItemsDetailed. }
    procedure WriteItemTableRow(Item: TPasItem; ShowVisibility: boolean;
      WriteItemLink: boolean; MakeAnchor: boolean);
      
    procedure WriteItemsSummary(Items: TPasItems; ShowVisibility: boolean; 
      HeadingLevel: Integer;
      const SectionAnchor: string; SectionName: TTranslationId);
      
    procedure WriteItemsDetailed(Items: TPasItems; ShowVisibility: boolean;
      HeadingLevel: Integer; SectionName: TTranslationId);

    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: integer; Authors: TStringVector);
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string;
      WriteItemLink: boolean);
    procedure WriteEndOfDocument;
    { Finishes an HTML paragraph element by writing a closing P tag. }
    procedure WriteEndOfParagraph;
    { Finishes an HTML table cell by writing a closing TD tag. }
    procedure WriteEndOfTableCell;
    { Finishes an HTML table by writing a closing TABLE tag. }
    procedure WriteEndOfTable;
    { Finishes an HTML table row by writing a closing TR tag. }
    procedure WriteEndOfTableRow;
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

    procedure WriteStartOfDocument(AName: string);

    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph; overload;
    procedure WriteStartOfParagraph(const CssClass: string); overload;

    { Starts an HTML table with a css class }
    procedure WriteStartOfTable(const CssClass: string);

    procedure WriteStartOfTableCell; overload;
    procedure WriteStartOfTableCell(const CssClass: string); overload;

    procedure WriteStartOfTable1Column(const CssClass: string; const t: string);
    procedure WriteStartOfTable2Columns(const CssClass: string; const t1, t2: string);
    procedure WriteStartOfTable3Columns(const CssClass: string; const t1, t2, t3: string);
    procedure WriteStartOfTableRow(const CssClass: string);

    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);
    
    { output all the necessary images }
    procedure WriteBinaryFiles;

    { output the index.html and navigation.html files }
    procedure WriteFramesetFiles;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    function MakeImage(const src, alt, CssClass: string): string;
    { writes a link
      @param href is the link's reference
      @param caption is the link's caption (must already been converted)
      @param CssClass is the link's CSS class }
    procedure WriteLink(const href, caption, CssClass: string);
    { writes a link with a target frame
      @param href is the link's reference
      @param caption is the link's caption (must already been converted)
      @parem CssClass is the link's CSS class
      @param TargetFrame is the link's target frame (or empty) }
    procedure WriteTargettedLink(const href, caption, CssClass, TargetFrame: string);

    procedure WriteSpellChecked(const AString: string);

    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio);

    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems);

    procedure WriteCIOSummary(HL: integer; c: TPasItems);

    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(HL: integer; const CssClass: string; const s: string);
    
    function FormatHeading(HL: integer; const CssClass: string; 
      const s: string): string;

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string);
    
    function FormatAnAnchor(const AName, Caption: string): string; 
  protected
    function ConvertString(const s: string): string; override;

    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): string; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;

    { overrides @inherited.HtmlString to return the string verbatim 
      (@inherited discards those strings) }
    function HtmlString(const S: string): string; override;
    
    // FormatPascalCode will cause Line to be formatted in
    // the way that Pascal code is formatted in Delphi.
    function FormatPascalCode(const Line: string): string; override;

    // FormatComment will cause AString to be formatted in
    // the way that comments other than compiler directives are
    // formatted in Delphi.  See: @link(FormatCompilerComment).
    function FormatComment(AString: string): string; override;

    // FormatHex will cause AString to be formatted in
    // the way that Hex are formatted in Delphi.
    function FormatHex(AString: string): string; override;

    // FormatNumeric will cause AString to be formatted in
    // the way that Numeric are formatted in Delphi.
    function FormatNumeric(AString: string): string; override;
    
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
    function CreateLink(const Item: TBaseItem): string; override;

    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;

    procedure WriteStartOfCode; override;
    procedure WriteEndOfCode; override;

    procedure WriteAnchor(const AName: string); overload;
    procedure WriteAnchor(const AName, Caption: string); overload;

    function Paragraph: string; override;

    function LineBreak: string; override;

    function URLLink(const URL: string): string; override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;

    function MakeItemLink(const Item: TBaseItem;
      const LinkCaption: string): string; override;
      
    function EscapeURL(const AString: string): string; virtual;
    
    function FormatSection(HL: integer; const Anchor: string;
      const Caption: string): string; override;
    function FormatAnchor(const Anchor: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;

  published
    { some HTML code to be written as header for every page }
    property Header: string read FHeader write FHeader;
    { some HTML code to be written as footer for every page }
    property Footer: string read FFooter write FFooter;
    { the content of the cascading stylesheet }
    property CSS: string read FCSS write FCSS;
    { if set to true, numeric filenames will be used rather than names with multiple dots }
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames
      default false;
    { Enable Tiptue fulltext search. See [http://pasdoc.sipsolutions.net/UseTipueSearchOption] }
    property UseTipueSearch: boolean read FUseTipueSearch write FUseTipueSearch
      default False;
  end;

  { Right now this is the same thing as TGenericHTMLDocGenerator.
    In the future it may be extended to include some things not needed
    for HtmlHelp generator. }
  THTMLDocGenerator = class(TGenericHTMLDocGenerator)
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  StrUtils,
  PasDoc,
  ObjectVector,
  Utils,
  PasDoc_HierarchyTree,
  PasDoc_Tipue,
  PasDoc_StringPairVector,
  PasDoc_Aspell;

{$INCLUDE automated.inc}
{$INCLUDE private.inc}
{$INCLUDE public.inc}
{$INCLUDE published.inc}
{$INCLUDE protected.inc}

const
  DoctypeFrameset = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Frameset//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/frameset.dtd">';
  DoctypeNormal = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Transitional//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">';
const
  DefaultPasdocCss = {$I pasdoc.css.inc};

constructor TGenericHTMLDocGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkCount := 1;
  FCSS := DefaultPasdocCss;
end;

function TGenericHTMLDocGenerator.HtmlString(const S: string): string;
begin
  Result := S;
end;

function TGenericHTMLDocGenerator.FormatString(AString: string): string;
begin
  result := '<span class="pascal_string">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := '<span class="pascal_keyword">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.FormatComment(AString: string): string;
begin
  result := '<span class="pascal_comment">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.FormatHex(AString: string): string;
begin
  result := '<span class="pascal_hex">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.FormatNumeric(AString: string): string;
begin
  result := '<span class="pascal_numeric">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := '<span class="pascal_compiler_comment">' + ConvertString(AString) + '</span>';
end;

function TGenericHTMLDocGenerator.CodeString(const s: string): string;
begin
  Result := '<code>' + s + '</code>';
end;

function TGenericHTMLDocGenerator.CreateLink(const Item: TBaseItem): string;

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
  
  if (Item is TPasItem) and Assigned(TPasItem(Item).MyUnit) then
  begin
    if Assigned(TPasItem(Item).MyObject) then 
    begin
      { it's a method, a field or a property - only those have MyObject initialized }
      Result := TPasItem(Item).MyObject.FullLink + '#' + Item.Name;
    end else begin
      if Item is TPasCio then 
      begin
        { it's an object / a class }
        Result := NewLink(TPasItem(Item).MyUnit.Name + '.' + Item.Name);
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := TPasItem(Item).MyUnit.FullLink + '#' + Item.Name;
      end;
    end;
  end else if Item is TSubItem then
  begin
    Result := TSubItem(Item).ExternalItem.FullLink + '#' + Item.Name;
  end
  else
  begin
    Result := NewLink(Item.Name);
  end;
end;

function TGenericHTMLDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin { todo: change class to something more generic }
  Result := '<a class="normal" href="' + EscapeURL(Link) + '">' + ItemName + '</a>';
end;

function TGenericHTMLDocGenerator.GetFileExtension: string;
begin
  Result := '.html';
end;

procedure TGenericHTMLDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then
    Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  WriteDirect('<hr noshade size="1">');
  WriteDirect('<span class="appinfo">');
  WriteDirect('<em>');
  WriteConverted(FLanguage.Translation[trGeneratedBy] + ' ');
  WriteTargettedLink(PASDOC_HOMEPAGE, PASDOC_NAME_AND_VERSION, '', '_parent');
  WriteConverted(' ' + FLanguage.Translation[trOnDateTime] + ' ' +
    FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
  WriteDirectLine('</em>');
  WriteDirectLine('</span>');
end;

procedure TGenericHTMLDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  Address: string;
begin
  if StringVectorIsNilOrEmpty(Authors) then Exit;

  if (Authors.Count = 1) then
    WriteHeading(HL, 'authors', FLanguage.Translation[trAuthor])
  else
    WriteHeading(HL, 'authors', FLanguage.Translation[trAuthors]);

  WriteDirectLine('<ul class="authors">');
  for i := 0 to Authors.Count - 1 do begin
    s := Authors[i];
    WriteDirect('<li>');

    if ExtractEmailAddress(s, S1, S2, Address) then begin
      WriteConverted(S1);
      WriteLink('mailto:' + Address, ConvertString(Address), '');
      WriteConverted(S2);
    end else if ExtractWebAddress(s, S1, S2, Address) then begin
      WriteConverted(S1);
      WriteLink('http://' + Address, ConvertString(Address), '');
      WriteConverted(S2);
    end else begin
      WriteConverted(s);
    end;

    WriteDirectLine('</li>');
  end;
  WriteDirectLine('</ul>');
end;

procedure TGenericHTMLDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);

  procedure WriteMethodsSummary;
  begin
    WriteItemsSummary(CIO.Methods, CIO.ShowVisibility, HL + 1, '@Methods', trMethods);
  end;

  procedure WriteMethodsDetailed;
  begin
    WriteItemsDetailed(CIO.Methods, CIO.ShowVisibility, HL + 1, trMethods);
  end;

  procedure WritePropertiesSummary;
  begin
    WriteItemsSummary(CIO.Properties, CIO.ShowVisibility, HL + 1, '@Properties', trProperties);
  end;

  procedure WritePropertiesDetailed;
  begin
    WriteItemsDetailed(CIO.Properties, CIO.ShowVisibility, HL + 1, trProperties);
  end;

  procedure WriteFieldsSummary;
  begin
    WriteItemsSummary(CIO.Fields, CIO.ShowVisibility, HL + 1, '@Fields', trFields);
  end;

  procedure WriteFieldsDetailed;
  begin
    WriteItemsDetailed(CIO.Fields, CIO.ShowVisibility, HL + 1, trFields);
  end;

  { writes all ancestors of the given item and the item itself }
  procedure WriteHierarchy(Name: string; Item: TBaseItem);
  var
    s: string;
    CIO: TPasCio;
  begin
    if not Assigned(Item) then begin
      WriteDirectLine('<li class="ancestor">' + Name + '</li>');
      { recursion ends here, when the item is an external class }
    end else if Item is TPasCio then begin
      CIO := TPasCio(Item);
      { first, write the ancestors }
      s := CIO.Ancestors.FirstName;
      WriteHierarchy(s, SearchItem(s, Item));
      { then write itself }
      s := CreateReferencedLink(CIO.Name, CIO.FullLink);
      WriteDirectLine('<li class="ancestor">' + s + '</li>')
    end;
    { todo --check: Is it possible that the item is assigned but is not a TPasCio ? }
  end;

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
  TheLink: string;
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;
  Section: TSections;
  AnyItem: boolean;
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

  s := GetCIOTypeName(CIO.MyType) + ' ' + CIO.Name;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);

  WriteAnchor(CIO.Name);
  WriteHeading(HL, 'cio', s);

  WriteStartOfTable('sections');
  WriteDirectLine('<tr>');
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
    WriteHeading(HL + 1, 'unit', FLanguage.Translation[trUnit]);
    WriteStartOfParagraph('unitlink');
    WriteLink(CIO.MyUnit.FullLink, ConvertString(CIO.MyUnit.Name), '');
    WriteEndOfParagraph;
  end;

  { write declaration link }
  WriteHeading(HL + 1, 'declaration', FLanguage.Translation[trDeclaration]);
  WriteStartOfParagraph('declaration');
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
  WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(CIO);

  { Write Hierarchy }
  if not StringVectorIsNilOrEmpty(CIO.Ancestors) then begin
    WriteAnchor(SectionAnchors[dsHierarchy]);
    WriteHeading(HL + 1, 'hierarchy', SectionHeads[dsHierarchy]);
    WriteDirect('<ul class="hierarchy">');
    s := CIO.Ancestors.FirstName;
    WriteHierarchy(s, SearchItem(s, Cio));
    WriteDirect('<li class="thisitem">' + CIO.Name + '</li>');
    WriteDirect('</ul>');
  end;

  AnyItem :=
    (not ObjectVectorIsNilOrEmpty(CIO.Fields)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Methods)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Properties));

  { AnyItem is used here to avoid writing headers "Overview"
    and "Description" when there are no items. }
  if AnyItem then
  begin
    WriteHeading(HL + 1, 'overview', FLanguage.Translation[trOverview]);
    WriteFieldsSummary;
    WriteMethodsSummary;
    WritePropertiesSummary;

    WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
    WriteFieldsDetailed;
    WriteMethodsDetailed;
    WritePropertiesDetailed;
  end;

  WriteAuthors(HL + 1, CIO.Authors);
  WriteDates(HL + 1, CIO.Created, CIO.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
end;

procedure TGenericHTMLDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
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

procedure TGenericHTMLDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  WriteAnchor('@Classes');

  WriteHeading(HL, 'cio', FLanguage.Translation[trCio]);
  WriteStartOfTable2Columns('classestable', FLanguage.Translation[trName], FLanguage.Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteStartOfTableRow('');
    { name of class/interface/object and unit }
    WriteStartOfTableCell('itemname');
    WriteConverted(GetCIOTypeName(p.MyType));
    WriteDirect('&nbsp;');
    WriteLink(p.FullLink, CodeString(p.Name), 'bold');
    WriteEndOfTableCell;

    { Description of class/interface/object }
    WriteStartOfTableCell('itemdesc');
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

procedure TGenericHTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem; 
  const Code: string; WriteItemLink: boolean);
begin
  WriteCodeWithLinksCommon(p, Code, WriteItemLink, '<b>', '</b>');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteDates(const HL: integer; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, 'created', FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteConverted(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, 'modified', FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteConvertedLine(LastMod);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteDocumentation;
begin
  StartSpellChecking('sgml');
  inherited;
  WriteUnits(1);
  WriteBinaryFiles;
  WriteOverviewFiles;
  WriteVisibilityLegendFile;
  WriteIntroduction;
  WriteConclusion;
  WriteFramesetFiles;
  if UseTipueSearch then
  begin
    DoMessage(2, mtInformation, 
      'Writing additional files for tipue search engine', []);
    TipueAddFiles(Units, Introduction, Conclusion, MetaContentType,
    DestinationDirectory);
  end;
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('</body>');
  WriteDirectLine('</html>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfCode;
begin
  WriteDirect('</code>');
end;

procedure TGenericHTMLDocGenerator.WriteLink(const href, caption, CssClass: string);
begin
  WriteTargettedLink(href, caption, CssClass, '');
end;

function TGenericHTMLDocGenerator.MakeItemLink(const Item: TBaseItem;
  const LinkCaption: string): string;
begin
  Result := MakeTargettedLink(Item.FullLink, ConvertString(LinkCaption), '', '');
end;

function TGenericHTMLDocGenerator.MakeTargettedLink(
  const href, caption, CssClass, TargetFrame: string): string;
begin
  Result := Format('<a %s %s href="%s">%s</a>',
    [ifthen(CssClass = '', '', 'class="' + CssClass + '"'),
     ifthen(TargetFrame = '', '', 'target="' + TargetFrame + '"'),
     EscapeURL(href), caption]);
end;

procedure TGenericHTMLDocGenerator.WriteTargettedLink(
  const href, caption, CssClass, TargetFrame: string);
begin
  WriteDirect(MakeTargettedLink(href, caption, CssClass, TargetFrame));
end;

procedure TGenericHTMLDocGenerator.WriteEndOfParagraph;
begin
  WriteDirectLine('</p>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfTableCell;
begin
  WriteDirectLine('</td>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfTable;
begin
  WriteDirectLine('</table>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfTableRow;
begin
  WriteDirectLine('</tr>');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteFooter;
begin
  WriteDirect(Footer);
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteItemTableRow(
  Item: TPasItem; ShowVisibility: boolean; 
  WriteItemLink: boolean; MakeAnchor: boolean);
begin
  WriteStartOfTableRow('');

  if ShowVisibility then
    WriteVisibilityCell(Item);
  { todo: assign a class }
  WriteStartOfTableCell('itemcode');

  if MakeAnchor then WriteAnchor(Item.Name);

  WriteCodeWithLinks(Item, Item.FullDeclaration, WriteItemLink);

  WriteEndOfTableCell;
  WriteEndOfTableRow;
end;

procedure TGenericHTMLDocGenerator.WriteItemsSummary(
  Items: TPasItems; ShowVisibility: boolean; HeadingLevel: Integer;
  const SectionAnchor: string; SectionName: TTranslationId);
var 
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;
  
  WriteAnchor(SectionAnchor);

  WriteHeading(HeadingLevel + 1, 'summary', FLanguage.Translation[SectionName]);
  
  WriteStartOfTable1Column('summary', '');

  for i := 0 to Items.Count - 1 do
    WriteItemTableRow(Items.PasItemAt[i], ShowVisibility, true, false);

  WriteEndOfTable;  
end;

procedure TGenericHTMLDocGenerator.WriteItemsDetailed(
  Items: TPasItems; ShowVisibility: boolean;
  HeadingLevel: Integer; SectionName: TTranslationId);
var 
  Item: TPasItem;
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, 'detail', FLanguage.Translation[SectionName]);
  
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.PasItemAt[i];

    WriteStartOfTable1Column('detail', '');
    WriteItemTableRow(Item, ShowVisibility, false, true);
    WriteEndOfTable;

    WriteItemDetailedDescription(Item);
  end;
end;

function TGenericHTMLDocGenerator.FormatHeading(HL: integer; 
  const CssClass: string; const s: string): string;
var
  c: string;
begin
  if (HL < 1) then HL := 1;
  if HL > 6 then begin
    DoMessage(2, mtWarning, 'HTML generator cannot write headlines of level 7 or greater; will use 6 instead.', []);
    HL := 6;
  end;
  c := IntToStr(HL);
  
  Result := '<h' + c + ' class="' + CssClass + '">' +
    ConvertString(s) +
    '</h' + c + '>' +
    LineEnding;
end;

procedure TGenericHTMLDocGenerator.WriteHeading(HL: integer; 
  const CssClass: string; const s: string);
begin
  WriteDirect(FormatHeading(HL, CssClass, s));
end;

procedure TGenericHTMLDocGenerator.WriteItemDescription(const AItem: TPasItem);
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

procedure TGenericHTMLDocGenerator.WriteItemDetailedDescription(const AItem: TPasItem);

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
    List: TStringPairVector; LinkToParamNames: boolean);
    
    procedure WriteParameter(const ParamName: string; const Desc: string);
    begin
      WriteDirect('<dt>'); // doesn't need a class, can be accessed via "dl.parameters dt"
      WriteDirect(ParamName);
      WriteDirectLine('</dt>');
      WriteDirect('<dd>');  // doesn't need a class, can be accessed via "dl.parameters dd"
      WriteSpellChecked(Desc);
      WriteDirectLine('</dd>');
    end;
    
  var
    i: integer;
    ParamName: string;
  begin
    if ObjectVectorIsNilOrEmpty(List) then
      Exit;

    WriteHeading(6, 'parameters', Caption);
    WriteDirectLine('<dl class="parameters">');
    for i := 0 to List.Count - 1 do 
    begin
      ParamName := List[i].Name;
      
      if LinkToParamNames then
       ParamName := SearchLinkOrWarning(ParamName, Func, '',
         'Could not resolve link to "%s" from description of item "%s"');
      
      WriteParameter(ParamName, List[i].Value);
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteHeading(6, 'return', LowerCase(FLanguage.Translation[trReturns]));
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
  Ancestor: TBaseItem;
  AncestorName: string;
  AItemMethod: TPasMethod;
  i: Integer;
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
        if Assigned(Ancestor) and (Ancestor is TPasItem) then
        begin
          WriteDirect('<div class="nodescription">');
          WriteConverted(Format('no description available, %s description follows', [AncestorName]));
          WriteDirect('</div>');
          WriteItemDetailedDescription(TPasItem(Ancestor));
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
 
  if AItem is TPasEnum then 
  begin
    WriteDirectLine('<ul>');
    for i := 0 to TPasEnum(AItem).Members.Count - 1 do 
    begin
      WriteDirectLine('<li>');
      WriteConverted(TPasEnum(AItem).Members.PasItemAt[i].Name);
      WriteConverted(': ');
      WriteSpellChecked(TPasEnum(AItem).Members.PasItemAt[i].GetDescription);
      WriteDirectLine('</li>');
    end;
    WriteDirectLine('</ul>');
  end;
end;

{ ---------- }

procedure TGenericHTMLDocGenerator.WriteOverviewFiles;

  function CreateOverviewStream(Overview: TCreatedOverviewFile): boolean;
  var
    BaseFileName, Headline: string;
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
    WriteHeading(1, 'allitems', Headline);  
  end;

  { Creates an output stream that lists up all units and short descriptions. }
  procedure WriteUnitOverviewFile;
  var
    c: TPasItems;
    Item: TPasItem;
    j: Integer;
  begin
    c := Units;

    if not CreateOverviewStream(ofUnits) then
      Exit;

    if Assigned(c) and (c.Count > 0) then begin
      WriteStartOfTable2Columns('unitstable', FLanguage.Translation[trName],
        FLanguage.Translation[trDescription]);
      for j := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[j];
        WriteStartOfTableRow('');
        WriteStartOfTableCell('itemname');
        WriteLink(Item.FullLink, Item.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell('itemdesc');
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

  { Writes a Hierarchy list - this is more useful than the simple class list }
  procedure WriteHierarchy;
  { todo -o twm: Make this recursive to handle closing </li> easily }
  var
    Level, OldLevel: Integer;
    Node: TPasItemNode;
  begin
    CreateClassHierarchy;

    if not CreateOverviewStream(ofClassHierarchy) then
      Exit;

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
          WriteDirectLine('<ul class="hierarchylevel">')
        else
          while Level < OldLevel do begin
            WriteDirectLine('</ul>');
            if OldLevel > 1 then
              WriteDirectLine('</li>');
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
        WriteDirectLine('</ul>');
        if OldLevel > 1 then
          WriteDirectLine('</li>');
        Dec(OldLevel);
      end;
    end;

    WriteFooter;
    WriteAppInfo;
    WriteEndOfDocument;

    CloseStream;
  end;
  
  procedure WriteItemsOverviewFile(Overview: TCreatedOverviewFile; 
    Items: TPasItems);
  var
    Item: TPasItem;
    j: Integer;
  begin
    if not CreateOverviewStream(Overview) then Exit;
    
    if not ObjectVectorIsNilOrEmpty(Items) then 
    begin
      WriteStartOfTable3Columns('itemstable',
        FLanguage.Translation[trName], 
        FLanguage.Translation[trUnit],
        FLanguage.Translation[trDescription]);

      Items.SortShallow;

      for j := 0 to Items.Count - 1 do
      begin
        Item := Items.PasItemAt[j];
        WriteStartOfTableRow('');

        WriteStartOfTableCell('itemname');
        WriteLink(Item.FullLink, Item.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell('itemunit');
        WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell('itemdesc');
        WriteItemDescription(Item);
        WriteEndOfTableCell;

        WriteEndOfTableRow;
      end;
      WriteEndOfTable;
    end else
    begin
      WriteStartOfParagraph;
      WriteConverted(FLanguage.Translation[trNone]);
      WriteEndOfParagraph;
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
  PU: TPasUnit;
  Overview: TCreatedOverviewFile;
  j: Integer;
begin
  WriteUnitOverviewFile;
  WriteHierarchy;

  // Make sure we don't free the Items when we free the container.
  TotalItems := TPasItems.Create(False);
  try
    for Overview := ofCios to HighCreatedOverviewFile do
    begin
      // Make sure we don't free the Items when we free the container.
      PartialItems := TPasItems.Create(False);
      try
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

        WriteItemsOverviewFile(Overview, PartialItems);

        TotalItems.InsertItems(PartialItems);
      finally PartialItems.Free end;
    end;

    WriteItemsOverviewFile(ofIdentifiers, TotalItems);
  finally TotalItems.Free end;
end;

{ ---------------------------------------------------------------------------- }

function TGenericHTMLDocGenerator.FormatAnAnchor(
  const AName, Caption: string): string;
begin
  result := Format('<a name="%s">%s</a>', [AName, Caption]);
end;

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName: string);
begin
  WriteAnchor(AName, '');
end;

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName, Caption: string);
begin
  WriteDirect(FormatAnAnchor(AName, Caption));
end;

{ procedure TGenericHTMLDocGenerator.WriteStartOfAnchor(const AName: string);
begin
  WriteDirect('<a name="' + AName + '">');
end; }

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteStartOfCode;
begin
  WriteDirect('<code>');
end;

{ ---------------------------------------------------------------------------- }

function TGenericHTMLDocGenerator.MetaContentType: string;
begin
  if FLanguage.CharSet <> '' then
    Result := '<meta http-equiv="content-type" content="text/html; charset='
      + FLanguage.CharSet + '">' + LineEnding else
    Result := '';
end;

procedure TGenericHTMLDocGenerator.WriteStartOfDocument(AName: string);
begin
  WriteDirectLine(DoctypeNormal);
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  WriteDirectLine('<meta name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  WriteDirect(MetaContentType);
  // Title
  WriteDirect('<title>');
  if Title <> '' then 
    WriteConverted(Title + ': ');
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

procedure TGenericHTMLDocGenerator.WriteStartOfParagraph(const CssClass: string);
begin
  if CssClass <> '' then
    WriteDirectLine('<p class="' + CssClass + '">')
  else
    WriteStartOfParagraph;
end;

procedure TGenericHTMLDocGenerator.WriteStartOfParagraph;
begin
  WriteDirectLine('<p>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable(const CssClass: string);
begin
  FOddTableRow := false;
  if CssClass <> '' then
    WriteDirectLine('<table class="' + CssClass + '">')
  else
    WriteDirectLine('<table>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable1Column(const CssClass: string; const t: string);
begin { TODO -ccheck : Why isn't the parameter t used? Is it always empty? If yes, remove it! }      
  WriteStartOfTable(CssClass);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable2Columns(const CssClass: string;
  const t1, t2: string);
begin
  FOddTableRow := false;
  WriteStartOfTable(CssClass);
  WriteDirectLine('<tr class="listheader">');
  WriteDirect('<th class="itemname">');
  WriteConverted(t1);
  WriteDirectLine('</th>');
  WriteDirect('<th class="itemdesc">');
  WriteConverted(t2);
  WriteDirectLine('</th>');
  WriteDirectLine('</tr>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable3Columns(const CssClass: string;
  const t1, t2, t3: string);
begin
  FOddTableRow := false;
  WriteStartOfTable(CssClass);
  WriteDirectLine('<tr class="listheader">');
  WriteDirect('<th class="itemname">');
  WriteConverted(t1);
  WriteDirectLine('</th>');
  WriteDirect('<th class="itemunit">');
  WriteConverted(t2);
  WriteDirectLine('</th>');
  WriteDirect('<th class="itemdesc">');
  WriteConverted(t3);
  WriteDirectLine('</th>');
  WriteDirectLine('</tr>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell(const CssClass: string);
var
  s: string;
begin
  if CssClass <> '' then
    s := Format('<td class="%s"',[CssClass])
  else
    s := '<td';
  WriteDirect(s+'>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell;
begin
  WriteStartOfTableCell('');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableRow(const CssClass: string);
var
  s: string;
begin
  if CssClass <> '' then begin
    s := Format('<tr class="%s"', [CssClass])
  end else begin
    s := '<tr class="list';
    if FOddTableRow then begin
      s := s + '2';
    end;
    FOddTableRow := not FOddTableRow;
    s := s + '"';
  end;
  WriteDirectLine(s + '>');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);

  procedure WriteUnitDescription(HL: integer; U: TPasUnit);
  begin
    WriteHeading(HL, 'description', FLanguage.Translation[trDescription]);
    WriteItemDetailedDescription(U);
  end;

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
  var
    i: Integer;
    ULink: TPasItem;
  begin
    if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
      WriteHeading(HL, 'uses', 'uses');
      WriteDirect('<ul class="useslist">');
      for i := 0 to U.UsesUnits.Count-1 do begin
        WriteDirect('<li>');
        ULink := TPasUnit(U.UsesUnits.Objects[i]);
        if ULink <> nil then begin
          WriteLink(ULink.FullLink, U.UsesUnits[i], '');
        end else begin
          WriteConverted(U.UsesUnits[i]);
        end;
        WriteDirect('</li>');
      end;   
      WriteDirect('</ul>');
    end;
  end;

  procedure WriteFuncsProcsSummary;
  begin
    WriteItemsSummary(U.FuncsProcs, false, HL + 1, '@FuncsProcs', 
      trFunctionsAndProcedures);
  end;

  procedure WriteFuncsProcsDetailed;
  begin
    WriteItemsDetailed(U.FuncsProcs, false, HL + 1,
      trFunctionsAndProcedures);
  end;

  procedure WriteTypesSummary;
  begin
    WriteItemsSummary(U.Types, false, HL + 1, '@Types', trTypes);
  end;

  procedure WriteTypesDetailed;
  begin
    WriteItemsDetailed(U.Types, false, HL + 1, trTypes);
  end;

  procedure WriteConstantsSummary;
  begin
    WriteItemsSummary(U.Constants, false, HL + 1, '@Constants', trConstants);
  end;

  procedure WriteConstantsDetailed;
  begin
    WriteItemsDetailed(U.Constants, false, HL + 1, trConstants);
  end;

  procedure WriteVariablesSummary;
  begin
    WriteItemsSummary(U.Variables, false, HL + 1, '@Variables', trVariables);
  end;

  procedure WriteVariablesDetailed;
  begin
    WriteItemsDetailed(U.Variables, false, HL + 1, trVariables);
  end;

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

var
  AnyItemSummary, AnyItemDetailed: boolean;
begin
  if not Assigned(U) then begin
    DoMessage(1, mtError, 'TGenericHTMLDocGenerator.WriteUnit: ' +
      'Unit variable has not been initialized.', []);
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

  WriteHeading(HL, 'unit', FLanguage.Translation[trUnit] + ' ' + U.Name);

  WriteStartOfTable('sections');
  WriteDirectLine('<tr>');
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

  AnyItemDetailed := 
    (not ObjectVectorIsNilOrEmpty(U.FuncsProcs)) or
    (not ObjectVectorIsNilOrEmpty(U.Types)) or
    (not ObjectVectorIsNilOrEmpty(U.Constants)) or
    (not ObjectVectorIsNilOrEmpty(U.Variables));

  AnyItemSummary := AnyItemDetailed or
    (not ObjectVectorIsNilOrEmpty(U.CIOs));

  { AnyItemSummary/Detailed are used here to avoid writing headers 
    "Overview" and "Description" when there are no items. }
  if AnyItemSummary then
  begin
    WriteHeading(HL + 1, 'overview', FLanguage.Translation[trOverview]);  
    WriteCIOSummary(HL + 2, U.CIOs);
    WriteFuncsProcsSummary;
    WriteTypesSummary;
    WriteConstantsSummary;
    WriteVariablesSummary;
  end;
  
  if AnyItemDetailed then
  begin
    WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);  
    WriteFuncsProcsDetailed;
    WriteTypesDetailed;
    WriteConstantsDetailed;
    WriteVariablesDetailed;
  end;

  WriteAuthors(HL + 1, U.Authors);
  WriteDates(HL + 1, U.Created, U.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
  WriteCIOs(HL, U.CIOs);
end;

function TGenericHTMLDocGenerator.MakeImage(const src, alt, CssClass: string): string;
begin
  Result := Format('<img %s src="%s" alt="%s" title="%s">',
    [IfThen(CssClass = '', '', 'class="' + CssClass + '"'),
     src, alt, alt]);
end;

procedure TGenericHTMLDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(const Image: string; trans: TTranslationID);
  begin
    WriteLink('legend.html', MakeImage(Image, ConvertString(FLanguage.Translation[trans]), ''), '');
  end;

begin
  WriteStartOfTableCell('visibility');
  case Item.Visibility of
    viPrivate:
      WriteVisibilityImage('private.gif', trPrivate);
    viProtected:
      WriteVisibilityImage('protected.gif', trProtected);
    viPublic:
      WriteVisibilityImage('public.gif', trPublic);
    viPublished:
      WriteVisibilityImage('published.gif', trPublished);
    viAutomated:
      WriteVisibilityImage('automated.gif', trAutomated);
  end;
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteVisibilityLegendFile;

  procedure WriteLegendEntry(const Image: string; trans: TTranslationID);
  begin
    WriteStartOfTableRow('');
    WriteStartOfTableCell('legendmarker');
    WriteDirect(MakeImage(Image, ConvertString(FLanguage.Translation[trans]), ''));
    WriteEndOfTableCell;
    WriteStartOfTableCell('legenddesc');
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

  WriteHeading(1, 'markerlegend', FLanguage.Translation[trLegend]);

  WriteStartOfTable2Columns('markerlegend',
    { TODO -otwm : needs translation } 'Marker',
    { TODO -otwm : needs translation } 'Visibility');

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

procedure TGenericHTMLDocGenerator.WriteSpellChecked(const AString: string);

{ TODO -- this code is scheduled to convert it to some generic
  version like WriteSpellCheckedGeneric in TDocGenerator to be able
  to easily do the similar trick for other output formats like LaTeX
  and future output formats.
  
  Note: don't you dare to copy&paste this code to TTexDocGenerator !
  If you want to work on it, make it generic, i.e. copy&paste this code
  to TDocGenerator and make it "generic" there. *Then* create specialized
  version in TTexDocGenerator that calls the generic version. 
  
  Or maybe such generic version should be better inside PasDoc_Aspell ? 
  This doesn't really matter. }

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
    for i := LErrors.Count-1 downto 0 do 
    begin
      // everything after the offending word
      temp := TSpellingError(LErrors.Items[i]).Offset+Length(TSpellingError(LErrors.Items[i]).Word) + 1;
      s := ( '">' + TSpellingError(LErrors.Items[i]).Word +  '</acronym>' + Copy(LString, temp, MaxInt)) + s; // insert into string
      if Length(TSpellingError(LErrors.Items[i]).Suggestions) > 0 then begin
        s := 'suggestions: '+TSpellingError(LErrors.Items[i]).Suggestions + s;
      end else begin
        s := 'no suggestions' + s;
      end;
      s := '<acronym class="mispelling" title="' + s;
      SetLength(LString, TSpellingError(LErrors.Items[i]).Offset);
    end;
    WriteDirect(LString);
    WriteDirect(s);
  end;
  LErrors.Free;
end;

procedure TGenericHTMLDocGenerator.WriteBinaryFiles;

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
  PasdocCssFileName: string;
begin
  WriteGifFile(img_automated, 'automated.gif');
  WriteGifFile(img_private, 'private.gif');
  WriteGifFile(img_protected, 'protected.gif');
  WriteGifFile(img_public, 'public.gif');
  WriteGifFile(img_published, 'published.gif');

  PasdocCssFileName := DestinationDirectory + 'pasdoc.css';
  StringToFile(PasdocCssFileName, CSS);
end;

procedure TGenericHTMLDocGenerator.WriteFramesetFiles;

  procedure LocalWriteLink(const Filename, Caption: string); overload;
  begin
    WriteDirect('<tr><td><a target="content" href="' + EscapeURL(Filename) + '" class="navigation">');
    WriteConverted(Caption);
    WriteDirectLine('</a></td></tr>');
  end;  

  procedure LocalWriteLink(const Filename: string; CaptionId: TTranslationID); overload;
  begin
    LocalWriteLink(Filename, FLanguage.Translation[CaptionId]);
  end;

var
  Overview: TCreatedOverviewFile;
begin
  CreateStream('index.html', True);
  WriteDirectLine(DoctypeFrameset);
  WriteDirectLine('<html><head>');
  WriteDirect(MetaContentType);
  WriteDirectLine('<title>'+Title+'</title>');
  WriteDirectLine('</head><frameset cols="200,*">');
  WriteDirectLine('<frame src="navigation.html" frameborder="0">');
  if Introduction <> nil then
  begin
    WriteDirectLine('<frame src="' +
      Introduction.OutputFileName +
      '" frameborder="0" name="content">');
  end
  else
  begin
    WriteDirectLine('<frame src="AllUnits.html" frameborder="0" name="content">');
  end;
  WriteDirectLine('</frameset></html>');
  CloseStream;

  CreateStream('navigation.html', True);
  WriteDirectLine(DoctypeNormal);
  WriteDirectLine('<html><head>');
  WriteDirect('<link rel="StyleSheet" type="text/css" href="');
  WriteDirect(EscapeURL('pasdoc.css'));
  WriteDirectLine('">');
  WriteDirect(MetaContentType);
  WriteDirectLine('<title>Navigation</title>');
  if UseTipueSearch then
    WriteDirect(TipueSearchButtonHead);
  WriteDirectLine('</head>');
  WriteDirectLine('<body class="navigationframe">');
  WriteDirect('<h2>'+Title+'</h2>');

  WriteStartOfTable('navigation');

  if Introduction <> nil then
  begin
    if Introduction.ShortTitle = '' then
    begin
      LocalWriteLink(Introduction.OutputFileName, trIntroduction);
    end else
    begin
      LocalWriteLink(Introduction.OutputFileName, Introduction.ShortTitle)
    end;
  end;
    
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

  if Conclusion <> nil then
  begin
    if Conclusion.ShortTitle = '' then
    begin
      LocalWriteLink(Conclusion.OutputFileName, trConclusion);
    end else
    begin
      LocalWriteLink(Conclusion.OutputFileName, Conclusion.ShortTitle)
    end;
  end;
    
  if UseTipueSearch then
    WriteDirect('<tr><td>' + TipueSearchButton + '</td></tr>');
    
  WriteDirectLine('</table>');
  WriteDirectLine('</body></html>');
  CloseStream;
end;

function TGenericHTMLDocGenerator.ConvertString(const S: String): String;
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

function TGenericHTMLDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;

function TGenericHTMLDocGenerator.EscapeURL(const AString: string): string;
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

function TGenericHTMLDocGenerator.FormatPascalCode(const Line: string): string;
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

  result := '</p>' + LineEnding + LineEnding +
    '<pre class="longcode">' +
       inherited FormatPascalCode(Line) + '</pre>' +
     LineEnding + LineEnding + '<p>';
end;

function TGenericHTMLDocGenerator.Paragraph: string; 
begin 
  { LineEndings are inserted here only to make HTML sources look
    more readable (this makes life easier when looking for pasdoc's bugs,
    comparing generating two tests results etc.).
    They are of course meaningless for anything that interprets this HTML. }
  Result := LineEnding + LineEnding + '<p>';
end;

function TGenericHTMLDocGenerator.LineBreak: string; 
begin
  Result := '<br>';
end;

function TGenericHTMLDocGenerator.URLLink(const URL: string): string;
begin
  Result := MakeTargettedLink(URL, ConvertString(URL), '', '_parent');
end;

procedure TGenericHTMLDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
var
  HL: integer;
begin
  case CreateStream(ExternalItem.OutputFileName, true) of
    csError: begin
      DoMessage(1, mtError, 'Could not create HTML unit doc file '
        + 'for the %s file %s.', [FLanguage.Translation[Id], ExternalItem.Name]);
      Exit;
    end;
  end;

  WriteStartOfDocument(ExternalItem.ShortTitle);

  HL := 1;

  WriteHeading(HL, 'externalitem', ExternalItem.Title);

  WriteSpellChecked(ExternalItem.DetailedDescription);

  WriteAuthors(HL + 1, ExternalItem.Authors);
  WriteDates(HL + 1, ExternalItem.Created, ExternalItem.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

function TGenericHTMLDocGenerator.FormatSection(HL: integer;
  const Anchor, Caption: string): string;
begin
  { We use `HL + 1' because user is allowed to use levels
    >= 1, and heading level 1 is reserved for section title. }
  result := FormatAnAnchor(Anchor, '') +
    FormatHeading(HL + 1, '', Caption);
end;

function TGenericHTMLDocGenerator.FormatAnchor(
  const Anchor: string): string;
begin
  result := FormatAnAnchor(Anchor, '');
end;


end.
