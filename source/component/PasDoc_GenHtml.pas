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
    FWriteUses: boolean;
    FLinkCount: Integer;
    FFooter: string;
    { If specified, using external CSS file }
    FCSS: string;
    FHeader: string;
    FOddTableRow: Integer;
    
    { Writes line (using WriteDirect) with <meta http-equiv="Content-Type" ...>
      html element describing current charset (from FLanguage). }
    procedure WriteMetaContentType;
    
    function MakeLinkTarget(
      const href, caption, localcss, target: string): string;    
      
    { Used by WriteItemsSummary and WriteItemsDetailed. }
    procedure WriteItemTableRow(Item: TPasItem; ShowVisibility: boolean;
      const ItemLink: string; MakeAnchor: boolean);
      
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

    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);
    
    { output all the necessary images }
    procedure WriteBinaryFiles;

    { output the index.html and navigation.html files }
    procedure WriteFramesetFiles;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    procedure WriteImage(const src, alt, localcss: string);
    procedure WriteLink(const href, caption, localcss: string);
    procedure WriteLinkTarget(const href, caption, localcss, target: string);
    procedure WriteEndOfLink;

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
    procedure WriteHeading(Level: integer; const s: string);

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string); 
  protected
    function ConvertString(const s: string): string; override;
    
    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): string; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
    
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
  public
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure LoadFooterFromFile(const AFileName: string);
    procedure LoadHeaderFromFile(const AFileName: string);
    procedure BuildLinks; override;

    function EscapeURL(const AString: string): string; virtual;
  published
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property CSS: string read FCSS write FCSS;
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames
      default false;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses
      default false;

    property UseTipueSearch: boolean read FUseTipueSearch write FUseTipueSearch;
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
  PasDoc,
  ObjectVector,
  Utils,
  PasDoc_HierarchyTree,
  PasDoc_Tipue,
  PasDoc_StringPairVector;

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

const
  DoctypeFrameset = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Frameset//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/frameset.dtd">';
  DoctypeNormal = '<!DOCTYPE HTML PUBLIC ' +
    '"-//W3C//DTD HTML 4.01 Transitional//EN" ' +
    '"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">';


function TGenericHTMLDocGenerator.HtmlString(const S: string): string; 
begin
  Result := S;
end;

function TGenericHTMLDocGenerator.FormatString(AString: string): string;
begin
  result := '<span class="pascal_string">' + AString + '</span>';
end;

function TGenericHTMLDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := '<span class="pascal_keyword">' + AString + '</span>';
end;

function TGenericHTMLDocGenerator.FormatComment(AString: string): string;
begin
  result := '<span class="pascal_comment">' + AString + '</span>';
end;

function TGenericHTMLDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := '<span class="pascal_compiler_comment">' + AString + '</span>';
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
  end else
  begin
    Result := NewLink(Item.Name);
  end;
end;

function TGenericHTMLDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result := '<a class="normal" href="' + EscapeURL(Link) + '">' + ItemName + '</a>';
end;

function TGenericHTMLDocGenerator.GetFileExtension: string;
begin
  Result := '.html';
end;

procedure TGenericHTMLDocGenerator.WriteAppInfo;
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

procedure TGenericHTMLDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
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
  Item: TBaseItem;
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
  
  AnyItem := 
    (not ObjectVectorIsNilOrEmpty(CIO.Fields)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Methods)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Properties));

  { AnyItem is used here to avoid writing headers "Overview"
    and "Description" when there are no items. }
  if AnyItem then
  begin
    WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
    WriteFieldsSummary;
    WriteMethodsSummary;
    WritePropertiesSummary;

    WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
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

procedure TGenericHTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem; 
  const Code: string; const ItemLink: string);
begin
  WriteCodeWithLinksCommon(p, Code, ItemLink, '<b>', '</b>', 
    {$ifdef FPC}@{$endif} WriteLink);
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteDates(const HL: integer; const Created,
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
    TipueAddFiles(Units, Introduction, Conclusion, DestinationDirectory);
  end;
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteEmptyCell;
begin
  WriteDirect('&nbsp;');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('</body>');
  WriteDirectLine('</html>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfAnchor;
begin
  WriteDirect('</a>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfLink;
begin
  WriteDirect('</a>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfCode;
begin
  WriteDirect('</code>');
end;

procedure TGenericHTMLDocGenerator.WriteLink(const href, caption, localcss: string);
begin
  WriteLinkTarget(href, caption, localcss, '');
end;

function TGenericHTMLDocGenerator.MakeLinkTarget(
  const href, caption, localcss, target: string): string;
var
  s: string;
begin
  if localcss <> '' then
    s := Format('<a class="%s"', [localcss])
  else
    s := '<a class="normal"';
  if target <> '' then
    s := Format('%s target="%s"', [s, target]);
  Result := Format('%s href="%s">%s</a>', [s, EscapeURL(href), caption]);
end;

procedure TGenericHTMLDocGenerator.WriteLinkTarget(
  const href, caption, localcss, target: string);
begin
  WriteDirect(MakeLinkTarget(href, caption, localcss, target));
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
  const ItemLink: string; MakeAnchor: boolean);
begin
  WriteStartOfTableRow('');

  if ShowVisibility then WriteVisibilityCell(Item);

  WriteStartOfTableCell('width="100%"', '');

  if MakeAnchor then WriteAnchor(Item.Name);

  WriteCodeWithLinks(Item, Item.FullDeclaration, ItemLink);

  WriteEndOfTableCell;
  WriteEndOfTableRow;
end;

procedure TGenericHTMLDocGenerator.WriteItemsSummary(
  Items: TPasItems; ShowVisibility: boolean; HeadingLevel: Integer;
  const SectionAnchor: string; SectionName: TTranslationId);
var 
  ItemLink: string;
  Item: TPasItem;
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;
  
  WriteAnchor(SectionAnchor);

  WriteHeading(HeadingLevel + 1, FLanguage.Translation[SectionName]);
  
  WriteStartOfTable1Column('');

  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.PasItemAt[i];

    ItemLink := Item.FullLink;
    if Assigned(Item.MyUnit) then
      if CompareText(Item.MyUnit.FullLink, 
        Copy(ItemLink, 1, Length(Item.MyUnit.FullLink))) = 0 then
        Delete(ItemLink, 1, Length(Item.MyUnit.FullLink));

    WriteItemTableRow(Item, ShowVisibility, ItemLink, false);
  end;

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

  WriteHeading(HeadingLevel + 1, FLanguage.Translation[SectionName]);
  
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.PasItemAt[i];

    WriteStartOfTable1Column('');
    WriteItemTableRow(Item, ShowVisibility, '', true);
    WriteEndOfTable;

    WriteItemDetailedDescription(Item);
  end;
end;

procedure TGenericHTMLDocGenerator.WriteHeading(Level: integer; const s: string);
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
      WriteDirectLine('<dt class="parameters">');
      WriteDirect(ParamName);
      WriteDirectLine('</dt>');
      WriteDirectLine('<dd class="parameters">');
      WriteSpellChecked(Desc);
      WriteDirectLine('</dd>');
    end;
    
  var
    i: integer;
    ParamName: string;
  begin
    if ObjectVectorIsNilOrEmpty(List) then
      Exit;

    WriteHeading(6, Caption);
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
  
  procedure WriteItemsOverviewFile(Overview: TCreatedOverviewFile; 
    Items: TPasItems);
  var
    Item: TPasItem;
    j: Integer;
  begin
    if not CreateOverviewStream(Overview) then Exit;
    
    if not ObjectVectorIsNilOrEmpty(Items) then 
    begin
      WriteStartOfTable3Columns(
        FLanguage.Translation[trName], 
        FLanguage.Translation[trUnit],
        FLanguage.Translation[trDescription]);

      Items.SortShallow;

      for j := 0 to Items.Count - 1 do
      begin
        Item := Items.PasItemAt[j];
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

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName: string);
begin
  WriteAnchor(AName, '');
end;

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName, Caption: string);
begin
  WriteDirect(Format('<a name="%s">%s</a>', [AName, Caption]));
end;

procedure TGenericHTMLDocGenerator.WriteStartOfAnchor(const AName: string);
begin
  WriteDirect('<a name="' + AName + '">');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteStartOfCode;
begin
  WriteDirect('<code>');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteMetaContentType;
begin
  if FLanguage.CharSet <> '' then begin
    WriteDirect('<meta http-equiv="content-type" content="text/html; charset=' 
      + FLanguage.CharSet + '">', true);
  end;
end;

procedure TGenericHTMLDocGenerator.WriteStartOfDocument(AName: string);
begin
  WriteDirectLine(DoctypeNormal);
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  WriteDirectLine('<meta name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  WriteMetaContentType;
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

procedure TGenericHTMLDocGenerator.WriteStartOfLink(const href, localcss, Target: string);
var
  s: string;
begin
  if localcss <> '' then
    s := Format('<a class="%s"', [localcss])
  else
    s := '<a class="normal"';
  if target <> '' then
    s := Format('%s target="%s"', [s, target]);
  WriteDirect(Format('%s href="%s">', [s, EscapeURL(href)]));
end;

procedure TGenericHTMLDocGenerator.WriteStartOfLink(const href, localcss: string);
begin
  WriteStartOfLink(href, localcss, '');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfLink(const href: string);
begin
  WriteStartOflink(href, '', '');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfParagraph;
begin
  WriteDirect(Paragraph);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable1Column(t: string);
begin
  FOddTableRow := 0;
  WriteDirect('<table cellspacing="' + HTML_TABLE_CELLSPACING
    + '" cellpadding="' + HTML_TABLE_CELLPADNG + '" width="100%">', true);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable2ColumnsExt(t1, t2: string;
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

procedure TGenericHTMLDocGenerator.WriteStartOfTable2Columns(t1, t2: string);
begin
  WriteStartOfTable2ColumnsExt(t1, t2, true);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable3Columns(t1, t2, T3: string);
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

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell(const Params, localcss: string);
var
  s: string;
begin
  if localcss <> '' then
    s := Format('<td class="%s"',[localcss])
  else
    s := '<td';
  if Params <> '' then
    s := s + ' ' + Params;
  WriteDirect(s+'>');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell(const localcss: string);
begin
  WriteStartOfTableCell('', localcss);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell;
begin
  WriteStartOfTableCell('', '');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTableRow(const CssClass: string);
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

procedure TGenericHTMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);

  procedure WriteUnitDescription(HL: integer; U: TPasUnit);
  begin
    WriteHeading(HL, FLanguage.Translation[trDescription]);
    WriteItemDetailedDescription(U);
  end;

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
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
    WriteHeading(HL + 1, FLanguage.Translation[trOverview]);  
    WriteCIOSummary(HL + 2, U.CIOs);
    WriteFuncsProcsSummary;
    WriteTypesSummary;
    WriteConstantsSummary;
    WriteVariablesSummary;
  end;
  
  if AnyItemDetailed then
  begin
    WriteHeading(HL + 1, FLanguage.Translation[trDescription]);  
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

procedure TGenericHTMLDocGenerator.WriteImage(const src, alt, localcss: string);
var
  s: string;
begin
  if localcss <> '' then
    s := Format('<img class="%s"', [localcss])
  else
    s := '<img border="0"';
  WriteDirect(Format('%s src="%s" alt="%s" title="%s">', [s, src, alt, alt]));
end;

procedure TGenericHTMLDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(const Image: string; trans: TTranslationID);
  begin
    WriteStartOfLink('legend.html');
    WriteImage(Image, ConvertString(FLanguage.Translation[trans]), '');
    WriteEndOfLink;
  end;

begin
  WriteStartOfTableCell;
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

procedure TGenericHTMLDocGenerator.LoadFooterFromFile(const AFileName: string);
begin
  FFooter := FileToString(AFileName);
end;

procedure TGenericHTMLDocGenerator.LoadHeaderFromFile(const AFileName: string);
begin
  FHeader := FileToString(AFileName);
end;

procedure TGenericHTMLDocGenerator.WriteSpellChecked(const AString: string);
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

const
  DefaultPasdocCss = {$I pasdoc.css.inc};
var
  PasdocCssFileName: string;
begin
  WriteGifFile(img_automated, 'automated.gif');
  WriteGifFile(img_private, 'private.gif');
  WriteGifFile(img_protected, 'protected.gif');
  WriteGifFile(img_public, 'public.gif');
  WriteGifFile(img_published, 'published.gif');
  
  PasdocCssFileName := DestinationDirectory + 'pasdoc.css';

  if CSS <> '' then 
  begin
    { If external CSS specified, copy it to pasdoc.css file. }
    CopyFile(CSS, PasdocCssFileName);
  end else
  begin
    StringToFile(PasdocCssFileName, DefaultPasdocCss);
  end;
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

procedure TGenericHTMLDocGenerator.BuildLinks;
begin
  FLinkCount := 1;
  inherited;
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
       inherited FormatPascalCode(ConvertString(Line)) + '</pre>' +
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
  Result := MakeLinkTarget(URL, ConvertString(URL), '', '_parent');
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

  WriteHeading(HL, ExternalItem.Title);

  WriteDirect(ExternalItem.DetailedDescription);

  WriteAuthors(HL + 1, ExternalItem.Authors);
  WriteDates(HL + 1, ExternalItem.Created, ExternalItem.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

end.
