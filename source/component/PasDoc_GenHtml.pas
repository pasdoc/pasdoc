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
  @author(Grzegorz Skoczylas <gskoczylas@rekord.pl>)
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @author(Ascanio Pressato)
  @author(Arno Garrels <first name.name@nospamgmx.de>)

  Implements an object to generate HTML documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenHtml;

{$I pasdoc_defines.inc}

interface

uses
  PasDoc_Utils,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_StringVector,
  PasDoc_Types,
  Classes,
  PasDoc_StringPairVector;

type
  { @abstract(generates HTML documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in HTML (HyperText Markup Language) format. }
  TGenericHTMLDocGenerator = class(TDocGenerator)
  private
    FUseTipueSearch: boolean;
    FNumericFilenames: boolean;
    FLinkCount: Integer;
    FHeader, FFooter, FHtmlBodyBegin, FHtmlBodyEnd, FHtmlHead: string;
    { The content of the CSS file. }
    FCSS: string;
    FOddTableRow: boolean;

    FImages: TStringList;

    { Makes a link.
      @param href is the link's reference
      @param caption is the link's text
      @param CssClass is the link's CSS class }
    function MakeLink(const href, caption, CssClass: string): string;

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

    { Writes the Item's AbstractDescription. Only if AbstractDescription
      is not available, uses DetailedDescription. }
    procedure WriteItemShortDescription(const AItem: TPasItem);

    (*Writes the Item's AbstractDescription followed by DetailedDescription.

      If OpenCloseParagraph then code here will open and close paragraph
      for itself. So you shouldn't
      surround it inside WriteStart/EndOfParagraph, like

@longcode(#
  { BAD EXAMPLE }
  WriteStartOfParagraph;
  WriteItemLongDescription(Item, true);
  WriteEndOfParagraph;
#)

      While you can pass OpenCloseParagraph = @false, do it with caution,
      and note that long description has often such large content that it
      really should be separated by paragraph. Passing
      OpenCloseParagraph = @false is sensible only if you will wrap this
      anyway inside some paragraph or similar block level element.
    *)
    procedure WriteItemLongDescription(const AItem: TPasItem;
      OpenCloseParagraph: boolean = true);

    { Does WriteItemLongDescription writes anything.
      When @false, you can avoid calling WriteItemLongDescription altogether. }
    function HasItemLongDescription(const AItem: TPasItem): boolean;

    procedure WriteOverviewFiles;

    procedure WriteStartOfDocument(AName: string);

    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph; overload;
    procedure WriteStartOfParagraph(const CssClass: string); overload;

    { Starts an HTML table with a css class }
    procedure WriteStartOfTable(const CssClass: string);

    procedure WriteStartOfTableCell; overload;
    procedure WriteStartOfTableCell(const CssClass: string); overload;

    procedure WriteStartOfTable1Column(const CssClass: string);
    procedure WriteStartOfTable2Columns(const CssClass: string; const t1, t2: string);
    procedure WriteStartOfTable3Columns(const CssClass: string; const t1, t2, t3: string);
    procedure WriteStartOfTableRow(const CssClass: string);

    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);

    { output all the necessary images }
    procedure WriteBinaryFiles;

    { output the index.html file }
    procedure WriteIndex;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    function MakeImage(const src, alt, CssClass: string): string;
    { writes a link
      @param href is the link's reference
      @param caption is the link's caption (must already been converted)
      @param CssClass is the link's CSS class }
    procedure WriteLink(const href, caption, CssClass: string);

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

    { Returns HTML heading tag. You can also make the anchor
      at this heading by passing AnchorName <> ''. }
    function FormatHeading(HL: integer; const CssClass: string;
      const s: string; const AnchorName: string): string;

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string);

    function FormatAnAnchor(const AName, Caption: string): string;

  protected
    { Return common HTML content that goes inside <head>. }
    function MakeHead: string;
    { Return common HTML content that goes right after <body>. }
    function MakeBodyBegin: string; virtual;
    { Return common HTML content that goes right before </body>. }
    function MakeBodyEnd: string; virtual;

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

    // FormatFloat will cause AString to be formatted in
    // the way that Float are formatted in Delphi.
    function FormatFloat(AString: string): string; override;

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

    procedure WriteStartOfCode; override;
    procedure WriteEndOfCode; override;

    procedure WriteAnchor(const AName: string); overload;
    { Write an anchor. Note that the Caption is assumed to be already processed
      with the @link(ConvertString). }
    procedure WriteAnchor(const AName, Caption: string); overload;

    function Paragraph: string; override;

    function EnDash: string; override;
    function EmDash: string; override;

    function LineBreak: string; override;

    function URLLink(const URL: string): string; override;
    function URLLink(const URL, LinkDisplay: string): string; override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;

    function MakeItemLink(const Item: TBaseItem;
      const LinkCaption: string;
      const LinkContext: TLinkContext): string; override;

    function EscapeURL(const AString: string): string; virtual;

    function FormatSection(HL: integer; const Anchor: string;
      const Caption: string): string; override;
    function FormatAnchor(const Anchor: string): string; override;

    function FormatBold(const Text: string): string; override;
    function FormatItalic(const Text: string): string; override;

    function FormatWarning(const Text: string): string; override;
    function FormatNote(const Text: string): string; override;
    function FormatPreformatted(const Text: string): string; override;

    function FormatImage(FileNames: TStringList): string; override;

    function FormatList(ListData: TListData): string; override;

    function FormatTable(Table: TTableData): string; override;

    function FormatTableOfContents(Sections: TStringPairVector): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    property HtmlBodyBegin: string read FHtmlBodyBegin write FHtmlBodyBegin;
    property HtmlBodyEnd: string read FHtmlBodyEnd write FHtmlBodyEnd;
    property HtmlHead: string read FHtmlHead write FHtmlHead;
    { the content of the cascading stylesheet }
    property CSS: string read FCSS write FCSS;
    { if set to true, numeric filenames will be used rather than names with multiple dots }
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames
      default false;
    { Enable Tiptue fulltext search. See [https://github.com/pasdoc/pasdoc/wiki/UseTipueSearchOption] }
    property UseTipueSearch: boolean read FUseTipueSearch write FUseTipueSearch
      default False;
  end;

  { Right now this is the same thing as TGenericHTMLDocGenerator.
    In the future it may be extended to include some things not needed
    for HtmlHelp generator. }
  THTMLDocGenerator = class(TGenericHTMLDocGenerator)
  protected
    function MakeBodyBegin: string; override;
    function MakeBodyEnd: string; override;
  end;

const
  DefaultPasdocCss = {$I pasdoc.css.inc};

implementation

uses
  SysUtils,
  StrUtils, { if you are using Delphi 5 or fpc 1.1.x you must add ..\component\strutils to your search path }
  PasDoc_Base,
  PasDoc_ObjectVector,
  PasDoc_HierarchyTree,
  PasDoc_Tipue,
  PasDoc_Aspell,
  PasDoc_Versions;

const
  img_automated : {$I automated.gif.inc};
  img_private   : {$I private.gif.inc};
  img_public    : {$I public.gif.inc};
  img_published : {$I published.gif.inc};
  img_protected : {$I protected.gif.inc};

constructor TGenericHTMLDocGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkCount := 1;
  FCSS := DefaultPasdocCss;
  FImages := TStringList.Create;
end;

destructor TGenericHTMLDocGenerator.Destroy;
begin
  FImages.Free;
  inherited;
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

function TGenericHTMLDocGenerator.FormatFloat(AString: string): string;
begin
  result := '<span class="pascal_float">' + ConvertString(AString) + '</span>';
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
    if (not (Item is TPasCio)) and Assigned(TPasItem(Item).MyObject) then
    begin
      { it's a method, a field or a property }
      Result := TPasItem(Item).MyObject.FullLink + '#' + Item.Name;
    end else begin
      if Item is TPasCio then
      begin
        { it's an object / a class }
        Result := NewLink(TPasItem(Item).QualifiedName)
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := TPasItem(Item).MyUnit.FullLink + '#' + Item.Name;
      end;
    end;
  end else if Item is TAnchorItem then
  begin
    Result := TAnchorItem(Item).ExternalItem.FullLink + '#' + Item.Name;
  end
  else
  begin
    Result := NewLink(Item.Name);
  end;
end;

function TGenericHTMLDocGenerator.GetFileExtension: string;
begin
  Result := '.html';
end;

procedure TGenericHTMLDocGenerator.WriteAppInfo;
begin
  if (not ExcludeGenerator) or IncludeCreationTime then
  begin
    { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
    WriteDirect('<hr>');
    WriteDirect('<span class="appinfo">');
    WriteDirect('<em>');
    if not ExcludeGenerator then
    begin
      WriteConverted(FLanguage.Translation[trGeneratedBy] + ' ');
      WriteLink(PASDOC_HOMEPAGE, PASDOC_NAME_AND_VERSION, '');
      WriteConverted('. ');
    end;
    if IncludeCreationTime then
    begin
      WriteConverted(FLanguage.Translation[trGeneratedOn] + ' ' +
        FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
      WriteConverted('.');
    end;
    WriteDirectLine('</em>');
    WriteDirectLine('</span>');
  end;
end;

procedure TGenericHTMLDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  Address: string;
begin
  if IsEmpty(Authors) then Exit;

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
type
  TSections = (dsDescription, dsHierarchy, dsEnclosingClass, dsNestedCRs,
  dsNestedTypes, dsFields, dsMethods, dsProperties);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
const
  SectionAnchors: TSectionAnchors = (
    'PasDoc-Description',
    'PasDoc-Hierarchy',
    'PasDoc-EnclosingClass',
    'PasDoc-NestedCRs',
    'PasDoc-NestedTypes',
    'PasDoc-Fields',
    'PasDoc-Methods',
    'PasDoc-Properties');

type
  TCIONames = array[TCIOType] of string;

const
  CIO_NAMES: TCIONames = (
    'class',
    'packed class',
    'dispinterface',
    'interface',
    'object',
    'packed object',
    'record',
    'packed record',
    'type helper');

  procedure WriteMethodsSummary;
  begin
    WriteItemsSummary(CIO.Methods, CIO.ShowVisibility, HL + 1,
      SectionAnchors[dsMethods], trMethods);
  end;

  procedure WriteMethodsDetailed;
  begin
    WriteItemsDetailed(CIO.Methods, CIO.ShowVisibility, HL + 1, trMethods);
  end;

  procedure WritePropertiesSummary;
  begin
    WriteItemsSummary(CIO.Properties, CIO.ShowVisibility, HL + 1,
      SectionAnchors[dsProperties], trProperties);
  end;

  procedure WritePropertiesDetailed;
  begin
    WriteItemsDetailed(CIO.Properties, CIO.ShowVisibility, HL + 1, trProperties);
  end;

  procedure WriteFieldsSummary;
  begin
    WriteItemsSummary(CIO.Fields, CIO.ShowVisibility, HL + 1,
      SectionAnchors[dsFields], trFields);
  end;

  procedure WriteFieldsDetailed;
  begin
    WriteItemsDetailed(CIO.Fields, CIO.ShowVisibility, HL + 1, trFields);
  end;

  procedure WriteNestedCioSummary;
  var
    I, J: Integer;
    LCio: TPasCio;
  begin
    for I := 0 to CIO.Cios.Count - 1 do
    begin
      LCio := TPasCio(CIO.Cios.PasItemAt[I]);
      LCio.FullDeclaration := LCIO.NameWithGeneric + ' = ' +
        CIO_NAMES[LCIO.MyType] + GetClassDirectiveName(LCIO.ClassDirective);
      if LCio.Ancestors.Count <> 0 then
      begin
        LCio.FullDeclaration := LCio.FullDeclaration + '(';
        for J := 0 to LCIO.Ancestors.Count - 1 do
        begin
            LCio.FullDeclaration := LCio.FullDeclaration + LCio.Ancestors[J].Value;
            if (J <> LCio.Ancestors.Count - 1) then
              LCio.FullDeclaration := LCio.FullDeclaration + ', ';
        end;
        LCio.FullDeclaration := LCio.FullDeclaration + ')';
      end;
    end;
    WriteItemsSummary(CIO.Cios, CIO.ShowVisibility, HL + 1,
      SectionAnchors[dsNestedCRs], trNestedCR);
  end;

  procedure WriteNestedTypesSummary;
  begin
    WriteItemsSummary(CIO.Types, CIO.ShowVisibility, HL + 1,
      SectionAnchors[dsNestedTypes], trNestedTypes);
  end;

  procedure WriteNestedTypesDetailed;
  begin
    WriteItemsDetailed(CIO.Types, CIO.ShowVisibility, HL + 1, trNestedTypes);
  end;

  { writes all ancestors of the given item and the item itself }
  procedure WriteHierarchy(const Name: string; const Item: TBaseItem);
  var
    CIO: TPasCio;
    ParentName: String;
    ParentItem: TBaseItem;
  begin
    if not Assigned(Item) then
    begin
      { First, write the ancestors.
        In case Item = nil, look for parent using ExternalClassHierarchy. }
      ParentName := ExternalClassHierarchy.Values[Name];
      if ParentName <> '' then
      begin
        { Although we found ParentName using ExternalClassHierarchy,
          it's possible that it's actually present in parsed files.
          This may happen when you have classes A -> B -> C (descending like
          this), and your source code includes classes A and C, but not B.
          So we have to use here FindGlobalPasItem. }
        ParentItem := FindGlobalPasItem(ParentName);
        WriteHierarchy(ParentName, ParentItem);
      end;

      WriteDirectLine('<li class="ancestor">' + Name + '</li>');
    end else
    if Item is TPasCio then
    begin
      CIO := TPasCio(Item);
      { first, write the ancestors }
      WriteHierarchy(CIO.Ancestors.FirstName, CIO.FirstAncestor);
      { then write itself }
      WriteDirectLine('<li class="ancestor">' +
        MakeItemLink(CIO, CIO.UnitRelativeQualifiedName, lcNormal) + '</li>')
    end;
    { todo --check: Is it possible that the item is assigned but is not a TPasCio ? }
  end;

var
  i: Integer;
  s: string;
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;
  Section: TSections;
  AnyItem: boolean;
  Fv: TPasFieldVariable;
begin
  if not Assigned(CIO) then Exit;

  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsHierarchy] := FLanguage.Translation[trHierarchy];
  SectionHeads[dsFields ]:= FLanguage.Translation[trFields];
  SectionHeads[dsMethods ]:= FLanguage.Translation[trMethods];
  SectionHeads[dsProperties ]:= FLanguage.Translation[trProperties];
  SectionHeads[dsNestedTypes]:= FLanguage.Translation[trNestedTypes];
  SectionHeads[dsNestedCRs]:= FLanguage.Translation[trNestedCR];
  SectionHeads[dsEnclosingClass]:= FLanguage.Translation[trEnclosingClass];

  SectionsAvailable := [dsDescription];
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then
    Include(SectionsAvailable, dsHierarchy);
  if not ObjectVectorIsNilOrEmpty(CIO.Fields) then
    Include(SectionsAvailable, dsFields);
  if not ObjectVectorIsNilOrEmpty(CIO.Methods) then
    Include(SectionsAvailable, dsMethods);
  if not ObjectVectorIsNilOrEmpty(CIO.Properties) then
    Include(SectionsAvailable, dsProperties);
  if not ObjectVectorIsNilOrEmpty(CIO.Types) then
    Include(SectionsAvailable, dsNestedTypes);
  if not ObjectVectorIsNilOrEmpty(CIO.Cios) then
    Include(SectionsAvailable, dsNestedCRs);
  if CIO.MyObject <> nil then
    Include(SectionsAvailable, dsEnclosingClass);

  if not ObjectVectorIsNilOrEmpty(CIO.Fields) then
  begin
    for I := 0 to CIO.Fields.Count - 1 do
    begin
      Fv := TPasFieldVariable(CIO.Fields.PasItemAt[I]);
      if Fv.IsConstant then
        Fv.FullDeclaration := FLanguage.Translation[trNested] + ' ' +
          Fv.FullDeclaration;
    end;
  end;

  s := GetCIOTypeName(CIO.MyType) + ' ' + CIO.UnitRelativeQualifiedName;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);

  WriteAnchor(CIO.Name);
  WriteHeading(HL, 'cio', s);

  WriteDirectLine('<div class="sections">');
  for Section := Low(TSections) to High(TSections) do
    begin
      { Most classes don't contain nested types so exclude this stuff
        if not available in order to keep it simple. }
      if (not (Section in SectionsAvailable)) and
        (Section in [dsEnclosingClass..dsNestedTypes]) then
        Continue;

      WriteDirect('<div class="one_section">');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section], 'section')
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirect('</div>');
    end;
  WriteDirectLine('</div>');

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
  WriteConverted('type ' + CIO.NameWithGeneric + ' = ');
  WriteConverted(CIO_NAMES[CIO.MyType]);
  WriteConverted(GetClassDirectiveName(CIO.ClassDirective));

  if CIO.Ancestors.Count <> 0 then
  begin
    WriteConverted('(');
    for i := 0 to CIO.Ancestors.Count - 1 do
    begin
      if CIO.Ancestors[i].Data <> nil then
        WriteDirect(MakeItemLink(TObject(CIO.Ancestors[i].Data) as TPasItem,
          CIO.Ancestors[i].Value, lcNormal)) else
        WriteConverted(CIO.Ancestors[i].Value);
      if (i <> CIO.Ancestors.Count - 1) then
        WriteConverted(', ');
    end;
    WriteConverted(')');
  end;
  if CIO.ClassDirective = CT_HELPER then
    WriteConverted(' for ' + CIO.HelperTypeIdentifier);

  WriteEndOfCode;
  WriteEndOfParagraph;

  { Write Description }
  WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
  WriteItemLongDescription(CIO);

  { Write Hierarchy }
  if CIO.Ancestors.Count <> 0 then
  begin
    WriteAnchor(SectionAnchors[dsHierarchy]);
    WriteHeading(HL + 1, 'hierarchy', SectionHeads[dsHierarchy]);
    WriteDirect('<ul class="hierarchy">');
    WriteHierarchy(CIO.Ancestors.FirstName, CIO.FirstAncestor);
    WriteDirect('<li class="thisitem">' + CIO.UnitRelativeQualifiedName + '</li>');
    WriteDirect('</ul>');
  end;

  { Write Enclosing Class }
  if CIO.MyObject <> nil then
  begin
    WriteAnchor(SectionAnchors[dsEnclosingClass]);
    WriteHeading(HL + 1, 'hierarchy', SectionHeads[dsEnclosingClass]);
    WriteDirect('<ul class="hierarchy"><li class="thisitem">');
    WriteLink(CIO.MyObject.FullLink, CIO.MyObject.Name, 'ancestor');
    WriteDirect('</li></ul>');
  end;

  AnyItem :=
    (not ObjectVectorIsNilOrEmpty(CIO.Fields)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Methods)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Properties)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Types)) or
    (not ObjectVectorIsNilOrEmpty(CIO.Cios));

  { AnyItem is used here to avoid writing headers "Overview"
    and "Description" when there are no items. }
  if AnyItem then
  begin
    WriteHeading(HL + 1, 'overview', FLanguage.Translation[trOverview]);
    WriteNestedCioSummary;
    WriteNestedTypesSummary;
    WriteFieldsSummary;
    WriteMethodsSummary;
    WritePropertiesSummary;

    WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
    WriteNestedTypesDetailed;
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

  procedure LocalWriteCio(const HL: Integer; const ACio: TPasCio);
  begin
    if (ACio.MyUnit <> nil) and
       ACio.MyUnit.FileNewerThanCache(DestinationDirectory + ACio.OutputFileName) then
    begin
      DoMessage(3, pmtInformation, 'Data for "%s" was loaded from cache, '+
        'and output file of this item exists and is newer than cache, '+
        'skipped.', [ACio.Name]);
      Exit;
    end;

    if not CreateStream(ACio.OutputFileName) then Exit;
    DoMessage(3, pmtInformation, 'Creating Class/Interface/Object file for "%s"...', [ACio.Name]);
    WriteCIO(HL, ACio);
  end;

  procedure LocalWriteCios(const HL: Integer; const ACios: TPasItems);
  var
    LCio: TPasCio;
    I: Integer;
  begin
    for I := 0 to ACios.Count -1 do
    begin
      LCio := TPasCio(ACios.PasItemAt[I]);
      LocalWriteCio(HL, LCio);
      if LCio.Cios.Count > 0 then
        LocalWriteCios(HL, LCio.Cios);
    end;
  end;

begin
  if c = nil then Exit;

  LocalWriteCios(HL, c);

  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);

  procedure WriteCioRow(ACio: TPasCio);
  begin
    WriteStartOfTableRow('');
    { name of class/interface/object and unit }
    WriteStartOfTableCell('itemname');
    WriteConverted(GetCIOTypeName(ACio.MyType));
    WriteDirect('&nbsp;');
    WriteLink(ACio.FullLink, CodeString(ACio.UnitRelativeQualifiedName), 'bold');
    WriteEndOfTableCell;

    { Description of class/interface/object }
    WriteStartOfTableCell('itemdesc');
    { Write only the AbstractDescription and do not opt for DetailedDescription,
      like WriteItemShortDescription does. }
    if ACio.AbstractDescription <> '' then
      WriteSpellChecked(ACio.AbstractDescription)
    else
      WriteDirect('&nbsp;');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;

var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  WriteAnchor('PasDoc-Classes');

  WriteHeading(HL, 'cio', FLanguage.Translation[trCio]);
  WriteStartOfTable2Columns('classestable', FLanguage.Translation[trName], FLanguage.Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteCioRow(p);
  end;
  WriteEndOfTable;
end;

procedure TGenericHTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem;
  const Code: string; WriteItemLink: boolean);
begin
  WriteCodeWithLinksCommon(p, Code, WriteItemLink, '<strong>', '</strong>');
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteDates(const HL: integer; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, 'created', FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteDirectLine(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, 'modified', FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteDirectLine(LastMod);
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
  WriteAdditionalFiles;
  WriteIndex;
  if UseTipueSearch then
  begin
    DoMessage(2, pmtInformation,
      'Writing additional files for tipue search engine', []);
    TipueAddFiles(Units, Introduction, Conclusion, AdditionalFiles,
      MakeHead, MakeBodyBegin, MakeBodyEnd, LanguageCode(FLanguage.Language),
      DestinationDirectory);
  end;
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteEndOfDocument;
begin
  WriteDirect(MakeBodyEnd);
  WriteDirect('</body>');
  WriteDirectLine('</html>');
end;

procedure TGenericHTMLDocGenerator.WriteEndOfCode;
begin
  WriteDirect('</code>');
end;

function TGenericHTMLDocGenerator.MakeItemLink(
  const Item: TBaseItem;
  const LinkCaption: string;
  const LinkContext: TLinkContext): string;
var
  CssClass: string;
begin
  if LinkContext = lcNormal then
    CssClass := 'normal' else
    CssClass := '';

  Result := MakeLink(Item.FullLink, ConvertString(LinkCaption), CssClass);
end;

function TGenericHTMLDocGenerator.MakeLink(
  const href, caption, CssClass: string): string;
begin
  Result := Format('<a %shref="%s">%s</a>', [
    IfThen(CssClass = '', '', 'class="' + CssClass + '" '),
    EscapeURL(href),
    Caption
  ]);
end;

procedure TGenericHTMLDocGenerator.WriteLink(
  const href, caption, CssClass: string);
begin
  WriteDirect(MakeLink(href, caption, CssClass));
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

  WriteStartOfTable1Column('summary');

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
  ColumnsCount: Cardinal;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, 'detail', FLanguage.Translation[SectionName]);

  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.PasItemAt[i];

    { calculate ColumnsCount }
    ColumnsCount := 1;
    if ShowVisibility then Inc(ColumnsCount);

    WriteStartOfTable('detail');
    WriteItemTableRow(Item, ShowVisibility, false, true);

    { Using colspan="0" below would be easier, but Konqueror and IE
      can't handle it correctly. It seems that they treat it as colspan="1" ? }
    WriteDirectLine(Format('<tr><td colspan="%d">', [ColumnsCount]));
    WriteItemLongDescription(Item);
    WriteDirectLine('</td></tr>');

    WriteEndOfTable;
  end;
end;

function TGenericHTMLDocGenerator.FormatHeading(HL: integer;
  const CssClass: string; const s: string;
  const AnchorName: string): string;
var
  c: string;
begin
  if (HL < 1) then HL := 1;
  if HL > 6 then begin
    DoMessage(2, pmtWarning, 'HTML generator cannot write headlines of level 7 or greater; will use 6 instead.', []);
    HL := 6;
  end;
  c := IntToStr(HL);

  Result := ConvertString(S);
  if AnchorName <> '' then
    Result := '<span id="' + AnchorName + '"></span>' + Result;

  Result := '<h' + c + ' class="' + CssClass + '">' + Result +
    '</h' + c + '>' + LineEnding;
end;

procedure TGenericHTMLDocGenerator.WriteHeading(HL: integer;
  const CssClass: string; const s: string);
begin
  WriteDirect(FormatHeading(HL, CssClass, s, ''));
end;

procedure TGenericHTMLDocGenerator.WriteItemShortDescription(const AItem: TPasItem);
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

function TGenericHTMLDocGenerator.HasItemLongDescription(const AItem: TPasItem): boolean;
begin
  Result := Assigned(AItem) and
    (
      (AItem.HintDirectives <> []) or
      (AItem.AbstractDescription <> '') or
      (AItem.DetailedDescription <> '') or
      (AItem is TPasCio) or
      (not ObjectVectorIsNilOrEmpty(AItem.Attributes)) or
      (AItem is TPasMethod) or
      (not ObjectVectorIsNilOrEmpty(AItem.SeeAlso)) or
      (AItem is TPasEnum)
    );
end;

procedure TGenericHTMLDocGenerator.WriteItemLongDescription(
  const AItem: TPasItem; OpenCloseParagraph: boolean);

  procedure WriteDescriptionSectionHeading(const Caption: TTranslationID);
  begin
    WriteHeading(6, 'description_section', FLanguage.Translation[Caption]);
  end;

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(ItemToSearchFrom: TPasItem; const Caption: TTranslationID;
    List: TStringPairVector; LinkToParamNames: boolean;
    const CssListClass: string);

    procedure WriteParameter(const ParamName: string; const Desc: string);
    begin
      { Note that <dt> and <dd> below don't need any CSS class,
        they can be accessed via "dl.parameters dt" or "dl.parameters dd"
        (assuming that CssListClass = 'parameters'). }
      WriteDirect('<dt>');
      WriteDirect(ParamName);
      WriteDirectLine('</dt>');
      WriteDirect('<dd>');
      WriteSpellChecked(Desc);
      WriteDirectLine('</dd>');
    end;

  var
    i: integer;
    ParamName: string;
  begin
    if ObjectVectorIsNilOrEmpty(List) then
      Exit;

    WriteDescriptionSectionHeading(Caption);
    WriteDirectLine('<dl class="' + CssListClass + '">');
    for i := 0 to List.Count - 1 do
    begin
      ParamName := List[i].Name;

      if LinkToParamNames then
       ParamName := SearchLink(ParamName, ItemToSearchFrom, '', true);

      WriteParameter(ParamName, List[i].Value);
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteSeeAlso(SeeAlso: TStringPairVector);
  var
    i: integer;
    SeeAlsoItem: TBaseItem;
    SeeAlsoLink: string;
  begin
    if ObjectVectorIsNilOrEmpty(SeeAlso) then
      Exit;

    WriteDescriptionSectionHeading(trSeeAlso);
    WriteDirectLine('<dl class="see_also">');
    for i := 0 to SeeAlso.Count - 1 do
    begin
      SeeAlsoLink := SearchLink(SeeAlso[i].Name, AItem,
        SeeAlso[i].Value, true, SeeAlsoItem);
      WriteDirect('  <dt>');
      if SeeAlsoItem <> nil then
        WriteDirect(SeeAlsoLink) else
        WriteConverted(SeeAlso[i].Name);
      WriteDirectLine('</dt>');

      WriteDirect('  <dd>');
      if (SeeAlsoItem <> nil) and (SeeAlsoItem is TPasItem) then
        WriteDirect(TPasItem(SeeAlsoItem).AbstractDescription);
      WriteDirectLine('</dd>');
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteAttributes(Attributes: TStringPairVector);
  var
    i: integer;
    name, value: string;
    AttributesItem: TBaseItem;
    AttributesLink: string;
  begin
    if ObjectVectorIsNilOrEmpty(Attributes) then
      Exit;

    WriteDescriptionSectionHeading(trAttributes);
    WriteDirectLine('<dl class="attributes">');
    for i := 0 to Attributes.Count - 1 do
    begin
      WriteDirect('  <dt>');
      name := Attributes.Items[I].Name;
      value := Attributes.Items[I].Value;
      { In case of attribute named 'GUID', it (may) come from interface GUID.
        So we should not actually search for identifier named 'GUID'
        (neither should we make a confusing warning that it cannot be found). }
      if name = 'GUID' then
      begin
        AttributesLink := name;
        AttributesItem := nil;
      end else
        AttributesLink := SearchLink(name, AItem, name, true, AttributesItem);
      WriteDirect(AttributesLink);

      WriteConverted(value);
      WriteDirectLine('</dt>');
      WriteDirect('  <dd>');

      if (AttributesItem <> nil) and (AttributesItem is TPasItem) then
        WriteDirect(TPasItem(AttributesItem).AbstractDescription);
      WriteDirectLine('</dd>');
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteReturnDesc(ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteDescriptionSectionHeading(trReturns);
    WriteDirect('<p class="return">');
    WriteSpellChecked(ReturnDesc);
    WriteDirect('</p>');
  end;

  procedure WriteHintDirective(const S: string; const Note: string = '');
  var
    Text: string;
  begin
    WriteDirect('<p class="hint_directive">');
    Text := FLanguage.Translation[trWarning] + ': ' + S;
    if Note <> '' then
      Text := Text + ': ' + Note else
      Text := Text + '.';
    WriteConverted(Text);
    WriteDirect('</p>');
  end;

var
  Ancestor: TBaseItem;
  AncestorName: string;
  EnumMember: TPasItem;
  i: Integer;
begin
  if not Assigned(AItem) then Exit;

  if hdDeprecated in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trDeprecated], AItem.DeprecatedNote);
  if hdPlatform in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trPlatformSpecific]);
  if hdLibrary in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trLibrarySpecific]);
  if hdExperimental in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trExperimental]);

  if AItem.AbstractDescription <> '' then
  begin
    if OpenCloseParagraph then WriteStartOfParagraph;

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

    if OpenCloseParagraph then WriteEndOfParagraph;
  end else begin
    if AItem.DetailedDescription <> '' then
    begin
      if OpenCloseParagraph then WriteStartOfParagraph;

      WriteSpellChecked(AItem.DetailedDescription);

      if OpenCloseParagraph then WriteEndOfParagraph;
    end else
    begin
      if (AItem is TPasCio) and
         (TPasCio(AItem).Ancestors.Count <> 0) then
      begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := TPasCio(AItem).FirstAncestor;
        if Assigned(Ancestor) and (Ancestor is TPasItem) then
        begin
          WriteDirect('<div class="nodescription">');
          WriteConverted(Format(
            'No description available, ancestor %s description follows', [AncestorName]));
          WriteDirect('</div>');
          WriteItemLongDescription(TPasItem(Ancestor));
        end;
      end else begin
        WriteDirect('&nbsp;');
      end;
    end;
  end;

  WriteAttributes(AItem.Attributes);

  WriteParamsOrRaises(AItem, trParameters, AItem.Params, false, 'parameters');
  if AItem is TPasMethod then
    WriteReturnDesc(TPasMethod(AItem).Returns);
  WriteParamsOrRaises(AItem, trExceptionsRaised, AItem.Raises, true, 'exceptions_raised');

  WriteSeeAlso(AItem.SeeAlso);

  if AItem is TPasEnum then
  begin
    WriteDescriptionSectionHeading(trValues);
    WriteDirectLine('<ul>');
    for i := 0 to TPasEnum(AItem).Members.Count - 1 do
    begin
      EnumMember := TPasEnum(AItem).Members.PasItemAt[i];
      WriteDirectLine('<li>');
      WriteAnchor(EnumMember.Name, ConvertString(EnumMember.FullDeclaration));
      if HasItemLongDescription(EnumMember) then
      begin
        WriteConverted(': ');
        WriteItemLongDescription(EnumMember, false);
      end;
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
    Result := CreateStream(BaseFileName + GetFileExtension);
    if not Result then Exit;

    DoMessage(3, pmtInformation, 'Writing overview file "' +
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
        WriteDirect('<p>');
        WriteItemShortDescription(Item);
        WriteDirect('</p>');
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
      WriteConverted(FLanguage.Translation[trNoCIOsForHierarchy]);
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
          WriteConverted(Node.Name)
        else
          WriteLink(Node.Item.FullLink,
            ConvertString(Node.Item.UnitRelativeQualifiedName), 'bold');
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
        WriteLink(Item.FullLink, Item.UnitRelativeQualifiedName, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell('itemunit');
        WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell('itemdesc');
        WriteDirect('<p>');
        WriteItemShortDescription(Item);
        WriteDirect('</p>');
        WriteEndOfTableCell;

        WriteEndOfTableRow;
      end;
      WriteEndOfTable;
    end else
    begin
      WriteStartOfParagraph;
      WriteConverted(FLanguage.Translation[
        OverviewFilesInfo[Overview].NoItemsTranslationId]);
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
  Overview: TCreatedOverviewFile;

  procedure CiosInsertIntoPartialItems(const ACios: TPasNestedCios);
  var
    I: Integer;
    LCio: TPasCio;
  begin
    if Overview = ofCIos then
      PartialItems.InsertItems(ACios);
    for I := 0 to ACios.Count -1 do
    begin
      LCio := TPasCio(ACios.PasItemAt[I]);
      if Overview = ofTypes then
        PartialItems.InsertItems(LCio.Types);
      if LCio.Cios.Count > 0 then
        CiosInsertIntoPartialItems(LCio.Cios);
    end;
  end;

var
  TotalItems: TPasItems; // Collect all Items for final listing.
  PU: TPasUnit;
  i, j: Integer;
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

          if (Overview in [ofCIos, ofTypes]) and
             not ObjectVectorIsNilOrEmpty(PU.CIOs) then
            for i := 0 to PU.CIOs.Count - 1 do
              CiosInsertIntoPartialItems(TPasCio(PU.CIOs.PasItemAt[i]).Cios);
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
  result := Format('<span id="%s">%s</span>', [AName, Caption]);
end;

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName: string);
begin
  WriteAnchor(AName, '');
end;

procedure TGenericHTMLDocGenerator.WriteAnchor(const AName, Caption: string);
begin
  WriteDirect(FormatAnAnchor(AName, Caption));
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteStartOfCode;
begin
  WriteDirect('<code>');
end;

{ ---------------------------------------------------------------------------- }

function TGenericHTMLDocGenerator.MakeHead: string;
begin
  Result := '<meta name="viewport" content="width=device-width, initial-scale=1">' + LineEnding;

  if not ExcludeGenerator then
    Result := Result + '<meta name="generator" content="'
      + PASDOC_NAME_AND_VERSION + '">' + LineEnding;
  if FLanguage.CharSet <> '' then
    Result := Result + '<meta http-equiv="content-type" content="text/html; charset='
      + FLanguage.CharSet + '">' + LineEnding;
  if UseTipueSearch then
    Result := Result + TipueSearchButtonHead + LineEnding;

  // StyleSheet
  Result := Result + '<link rel="StyleSheet" type="text/css" href="' +
    EscapeURL('pasdoc.css') + '">' + LineEnding;

  Result := Result + FHtmlHead;
end;

function TGenericHTMLDocGenerator.MakeBodyBegin: string;
begin
  Result := FHtmlBodyBegin;
end;

function TGenericHTMLDocGenerator.MakeBodyEnd: string;
begin
  Result := FHtmlBodyEnd;
end;

procedure TGenericHTMLDocGenerator.WriteStartOfDocument(AName: string);
begin
  WriteDirectLine('<!DOCTYPE html>');
  WriteDirectLine('<html lang="' + LanguageCode(FLanguage.Language) + '">');
  WriteDirectLine('<head>');
  // Title
  WriteDirect('<title>');
  if Title <> '' then
    WriteConverted(Title + ': ');
  WriteConverted(AName);
  WriteDirectLine('</title>');
  WriteDirect(MakeHead);
  WriteDirectLine('</head>');
  WriteDirectLine('<body>');
  WriteDirect(MakeBodyBegin);
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
  { Every table create by WriteStartOfTable has class wide_list }
  WriteDirectLine('<table class="' + CssClass + ' wide_list">');
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable1Column(const CssClass: string);
begin
  WriteStartOfTable(CssClass);
end;

procedure TGenericHTMLDocGenerator.WriteStartOfTable2Columns(const CssClass: string;
  const t1, t2: string);
begin
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

procedure TGenericHTMLDocGenerator.WriteStartOfTable3Columns(
  const CssClass: string; const t1, t2, t3: string);
begin
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

procedure TGenericHTMLDocGenerator.WriteStartOfTableCell(
  const CssClass: string);
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
type
  TSections = (dsDescription, dsUses, dsClasses, dsFuncsProcs,
    dsTypes, dsConstants, dsVariables);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
const
  SectionAnchors: TSectionAnchors = (
    'PasDoc-Description',
    'PasDoc-Uses',
    'PasDoc-Classes',
    'PasDoc-FuncsProcs',
    'PasDoc-Types',
    'PasDoc-Constants',
    'PasDoc-Variables');

  procedure WriteUnitDescription(HL: integer; U: TPasUnit);
  begin
    WriteHeading(HL, 'description', FLanguage.Translation[trDescription]);
    WriteItemLongDescription(U);
  end;

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
  var
    i: Integer;
    ULink: TPasItem;
  begin
    if WriteUsesClause and not IsEmpty(U.UsesUnits) then begin
      WriteHeading(HL, 'uses', FLanguage.Translation[trUses]);
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
    WriteItemsSummary(U.FuncsProcs, false, HL + 1, SectionAnchors[dsFuncsProcs],
      trFunctionsAndProcedures);
  end;

  procedure WriteFuncsProcsDetailed;
  begin
    WriteItemsDetailed(U.FuncsProcs, false, HL + 1,
      trFunctionsAndProcedures);
  end;

  procedure WriteTypesSummary;
  begin
    WriteItemsSummary(U.Types, false, HL + 1, SectionAnchors[dsTypes], trTypes);
  end;

  procedure WriteTypesDetailed;
  begin
    WriteItemsDetailed(U.Types, false, HL + 1, trTypes);
  end;

  procedure WriteConstantsSummary;
  begin
    WriteItemsSummary(U.Constants, false, HL + 1, SectionAnchors[dsConstants],
      trConstants);
  end;

  procedure WriteConstantsDetailed;
  begin
    WriteItemsDetailed(U.Constants, false, HL + 1, trConstants);
  end;

  procedure WriteVariablesSummary;
  begin
    WriteItemsSummary(U.Variables, false, HL + 1, SectionAnchors[dsVariables],
      trVariables);
  end;

  procedure WriteVariablesDetailed;
  begin
    WriteItemsDetailed(U.Variables, false, HL + 1, trVariables);
  end;

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
    DoMessage(1, pmtError, 'TGenericHTMLDocGenerator.WriteUnit: ' +
      'Unit variable has not been initialized.', []);
    Exit;
  end;

  if U.FileNewerThanCache(DestinationDirectory + U.OutputFileName) then
  begin
    DoMessage(3, pmtInformation, 'Data for unit "%s" was loaded from cache, '+
      'and output file of this unit exists and is newer than cache, '+
      'skipped.', [U.Name]);
    Exit;
  end;

  if not CreateStream(U.OutputFileName) then Exit;

  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsUses] := FLanguage.Translation[trUses];
  SectionHeads[dsClasses] := FLanguage.Translation[trCio];
  SectionHeads[dsFuncsProcs]:= FLanguage.Translation[trFunctionsAndProcedures];
  SectionHeads[dsTypes]:= FLanguage.Translation[trTypes];
  SectionHeads[dsConstants]:= FLanguage.Translation[trConstants];
  SectionHeads[dsVariables]:= FLanguage.Translation[trVariables];

  SectionsAvailable := [dsDescription];
  ConditionallyAddSection(dsUses, WriteUsesClause and not IsEmpty(U.UsesUnits));
  ConditionallyAddSection(dsClasses, not ObjectVectorIsNilOrEmpty(U.CIOs));
  ConditionallyAddSection(dsFuncsProcs, not ObjectVectorIsNilOrEmpty(U.FuncsProcs));
  ConditionallyAddSection(dsTypes, not ObjectVectorIsNilOrEmpty(U.Types));
  ConditionallyAddSection(dsConstants, not ObjectVectorIsNilOrEmpty(U.Constants));
  ConditionallyAddSection(dsVariables, not ObjectVectorIsNilOrEmpty(U.Variables));

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteStartOfDocument(U.Name);

  if U.IsUnit then
    WriteHeading(HL, 'unit', FLanguage.Translation[trUnit] + ' ' + U.Name)
  else if U.IsProgram then
    WriteHeading(HL, 'program', FLanguage.Translation[trProgram] + ' ' + U.Name)
  else
    WriteHeading(HL, 'library', FLanguage.Translation[trLibrary] + ' ' + U.Name);

  WriteDirectLine('<div class="sections">');
  for Section := Low(TSections) to High(TSections) do
    begin
      WriteDirect('<div class="one_section">');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section], 'section')
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirect('</div>');
    end;
  WriteDirectLine('</div>');

  WriteAnchor(SectionAnchors[dsDescription]);
  WriteUnitDescription(HL + 1, U);

  WriteAnchor(SectionAnchors[dsUses]);
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

const
  VisibilityImageName: array[TVisibility] of string =
  ( 'published.gif',
    'public.gif',
    'protected.gif',
    'protected.gif',
    'private.gif',
    'private.gif',
    'automated.gif',
    { Implicit visibility uses published visibility image, for now }
    'published.gif'
  );
  VisibilityTranslation: array[TVisibility] of TTranslationID =
  ( trPublished,
    trPublic,
    trProtected,
    trStrictProtected,
    trPrivate,
    trStrictPrivate,
    trAutomated,
    trImplicit
  );

procedure TGenericHTMLDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(Vis: TVisibility);
  begin
    WriteLink('legend.html', MakeImage(VisibilityImageName[Vis],
      ConvertString(FLanguage.Translation[
        VisibilityTranslation[Vis]]), ''), '');
  end;

begin
  WriteStartOfTableCell('visibility');
  WriteVisibilityImage(Item.Visibility);
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure TGenericHTMLDocGenerator.WriteVisibilityLegendFile;

  procedure WriteLegendEntry(Vis: TVisibility);
  var VisTrans: string;
  begin
    VisTrans := FLanguage.Translation[VisibilityTranslation[Vis]];
    WriteStartOfTableRow('');
    WriteStartOfTableCell('legendmarker');
    WriteDirect(MakeImage(VisibilityImageName[Vis],
      ConvertString(VisTrans), ''));
    WriteEndOfTableCell;
    WriteStartOfTableCell('legenddesc');
    WriteConverted(VisTrans);
    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;

const
  Filename = 'legend';
begin
  if not CreateStream(Filename + GetFileextension) then
    Abort;

  try
    WriteStartOfDocument(FLanguage.Translation[trLegend]);

    WriteHeading(1, 'markerlegend', FLanguage.Translation[trLegend]);

    WriteStartOfTable2Columns('markerlegend',
      FLanguage.Translation[trMarker],
      FLanguage.Translation[trVisibility]);

    { Order of entries below is important (because it is shown to the user),
      so we don't just write all TVisibility values in the order they
      were declared in TVisibility type. }
    WriteLegendEntry(viStrictPrivate);
    WriteLegendEntry(viPrivate);
    WriteLegendEntry(viStrictProtected);
    WriteLegendEntry(viProtected);
    WriteLegendEntry(viPublic);
    WriteLegendEntry(viPublished);
    WriteLegendEntry(viAutomated);
    WriteLegendEntry(viImplicit);
    WriteEndOfTable;

    WriteFooter;
    WriteAppInfo;
    WriteEndOfDocument;
  finally CloseStream; end;
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
begin
  DataToFile(DestinationDirectory + 'automated.gif', img_automated);
  DataToFile(DestinationDirectory + 'private.gif'  , img_private  );
  DataToFile(DestinationDirectory + 'protected.gif', img_protected);
  DataToFile(DestinationDirectory + 'public.gif'   , img_public   );
  DataToFile(DestinationDirectory + 'published.gif', img_published);
  StringToFile(DestinationDirectory + 'pasdoc.css', CSS);
end;

procedure TGenericHTMLDocGenerator.WriteIndex;
var
  IndexSourceFileName: string;
begin
  { TODO: It would be cleaner to actually rename the appropriate file
    (introduction or AllUnits) to index.html, instead of copying it? }
  if Introduction <> nil then
    IndexSourceFileName := Introduction.OutputFileName else
    IndexSourceFileName := 'AllUnits.html';
  CopyFile(DestinationDirectory + IndexSourceFileName, DestinationDirectory + 'index.html');
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
    if IsCharInSet(AString[i], [AnsiChar($21)..AnsiChar($7E)]) then
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
    In order to fix it, WriteItemLongDescription always wraps
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

function TGenericHTMLDocGenerator.EnDash: string;
begin
  Result := '&ndash;';
end;

function TGenericHTMLDocGenerator.EmDash: string;
begin
  Result := '&mdash;';
end;

function TGenericHTMLDocGenerator.LineBreak: string;
begin
  Result := '<br>';
end;

function TGenericHTMLDocGenerator.URLLink(const URL: string): string;
begin
  Result := MakeLink(URL, ConvertString(URL), '');
end;

function TGenericHTMLDocGenerator.URLLink(const URL, LinkDisplay: string): string;
var
  Link: String;
begin
  Link := FixEmailaddressWithoutMailTo(URL);

  if LinkDisplay <> '' then
    Result := MakeLink(Link, ConvertString(LinkDisplay), '')
  else
    Result := MakeLink(Link, ConvertString(URL), '');
end;

procedure TGenericHTMLDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
var
  HL: integer;
begin
  if not CreateStream(ExternalItem.OutputFileName) then Exit;

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
  result := FormatHeading(HL + 1, '', Caption, Anchor);
end;

function TGenericHTMLDocGenerator.FormatAnchor(
  const Anchor: string): string;
begin
  result := FormatAnAnchor(Anchor, '');
end;

function TGenericHTMLDocGenerator.FormatBold(const Text: string): string;
begin
  Result := '<strong>' + Text + '</strong>';
end;

function TGenericHTMLDocGenerator.FormatItalic(const Text: string): string;
begin
  Result := '<em>' + Text + '</em>';
end;

function TGenericHTMLDocGenerator.FormatWarning(const Text: string): string;
begin
  Result := '<dl class="tag warning"><dt>' + FormatBold(FLanguage.Translation[trWarningTag]) + '</dt><dd>';
  Result := Result + Text;
  Result := Result + '</dd></dl>';
end;

function TGenericHTMLDocGenerator.FormatNote(const Text: string): string;
begin
  Result := '<dl class="tag note"><dt>' + FormatBold(FLanguage.Translation[trNoteTag]) + '</dt><dd>';
  Result := Result + Text;
  Result := Result + '</dd></dl>';
end;

function TGenericHTMLDocGenerator.FormatPreformatted(
  const Text: string): string;
begin
  { See TGenericHTMLDocGenerator.FormatPascalCode
    for comments why these </p> and <p> are needed here.
    LineEndings are added only to make html source more readable. }
  Result := '</p>' + LineEnding + LineEnding +
    '<pre class="preformatted">' +
       inherited FormatPreformatted(Text) + '</pre>' +
     LineEnding + LineEnding + '<p>';
end;

function TGenericHTMLDocGenerator.FormatImage(FileNames: TStringList): string;
var
  ChosenFileName, OutputImageFileName: string;
  ImageId, I: Integer;
  CopyNeeded: boolean;
begin
  { Calculate ChosenFileName, i.e. choose right image format for html.
    Anything other than eps or pdf is good. }
  ChosenFileName := '';
  for I := 0 to FileNames.Count - 1 do
    if (LowerCase(ExtractFileExt(FileNames[I])) <> '.eps') and
       (LowerCase(ExtractFileExt(FileNames[I])) <> '.pdf') then
    begin
      ChosenFileName := FileNames[I];
      Break;
    end;
  if ChosenFileName = '' then
    ChosenFileName := FileNames[0];

  { Calculate ImageId and CopyNeeded }
  ImageId := FImages.IndexOf(ChosenFileName);
  CopyNeeded := ImageId = -1;
  if CopyNeeded then
    ImageId := FImages.Add(ChosenFileName);

  OutputImageFileName :=
    'image_' + IntToStr(ImageId) + ExtractFileExt(ChosenFileName);

  if CopyNeeded then
    CopyFile(ChosenFileName, DestinationDirectory + OutputImageFileName);

  Result := Format('<img src="%s" alt="%s" />',
    [ OutputImageFileName,
      { Just use basename of chosen filename, that's the best
        alt text for the image as we can get... }
      DeleteFileExt(ExtractFileName(ChosenFileName))]);
end;

function TGenericHTMLDocGenerator.FormatList(ListData: TListData): string;
const
  ListTag: array[TListType]of string =
  ( 'ul', 'ol', 'dl' );
  ListClass: array[TListItemSpacing]of string =
  ( 'compact_spacing', 'paragraph_spacing' );
var
  i: Integer;
  ListItem: TListItemData;
  Attributes: string;
begin
  { We're explicitly marking end of previous paragraph and beginning
    of next one. This is required to always validate clearly.
    This also makes empty lists (no items) be handled correctly,
    i.e. they should produce paragraph break. }
  Result := '</p>' + LineEnding + LineEnding;

  { HTML requires that <ol> / <ul> contains at least one <li>. }
  if ListData.Count <> 0 then
  begin
    Result := Result + Format('<%s class="%s">',
      [ListTag[ListData.ListType], ListClass[ListData.ItemSpacing]]) + LineEnding;

    for i := 0 to ListData.Count - 1 do
    begin
      ListItem := ListData.Items[i] as TListItemData;

      if ListData.ListType = ltDefinition then
      begin
        { Note: We're not writing <p> .. </p> inside <dt>, because
          officially <dt> can't contain any paragraphs.

          Yes, this means that if user will use paragraphs inside
          @itemLabel then our output HTML will not be validated
          as correct HTML. I don't see any easy way to fix this ?
          After all we don't want to "fake" <dl>, <dt> and <dd>
          using some other tags and complex css.

          So I guess that this should be blamed as an "unavoidable
          limitation of HTML output", if someone will ask :)

          -- Michalis }

        Result := Result +
          '  <dt>' + ListItem.ItemLabel + '</dt>' + LineEnding +
          '  <dd><p>' + ListItem.Text + '</p></dd>' + LineEnding;
      end else
      begin
        if ListData.ListType = ltOrdered then
          Attributes := Format(' value="%d"', [ListItem.Index]) else
          Attributes := '';

        Result := Result + Format('  <li%s><p>%s</p></li>',
          [Attributes, ListItem.Text])  + LineEnding;
      end;
    end;

    Result := Result + Format('</%s>', [ListTag[ListData.ListType]]) +
      LineEnding + LineEnding;
  end;

  Result := Result + '<p>';
end;

function TGenericHTMLDocGenerator.FormatTable(Table: TTableData): string;
const
  CellTag: array[boolean]of string = ('td', 'th');
var
  RowNum, ColNum: Integer;
  Row: TRowData;
  NormalRowOdd: boolean;
  RowClass: string;
begin
  Result := '</p>' + LineEnding + LineEnding +
    '<table class="table_tag">' + LineEnding;
  NormalRowOdd := true;
  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum] as TRowData;

    if Row.Head then
      RowClass := 'head' else
    begin
      if NormalRowOdd then
        RowClass := 'odd' else
        RowClass := 'even';
      NormalRowOdd := not NormalRowOdd;
    end;

    Result := Result + '  <tr class="' + RowClass + '">' + LineEnding;

    for ColNum := 0 to Row.Cells.Count - 1 do
      Result := Result + Format('    <%s><p>%s</p></%0:s>%2:s',
        [CellTag[Row.Head], Row.Cells[ColNum], LineEnding]);

    Result := Result + '  </tr>' + LineEnding;
  end;
  Result := Result + '</table>' + LineEnding + LineEnding + '<p>';
end;

function TGenericHTMLDocGenerator.FormatTableOfContents(
  Sections: TStringPairVector): string;
var
  i: Integer;
begin
  if Sections.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := '<ol>' + LineEnding;
  for i := 0 to Sections.Count - 1 do
  begin
    Result := Result +
      '<li><a href="#' + Sections[i].Name + '">' + Sections[i].Value + '</a>' +
      LineEnding +
      FormatTableOfContents(TStringPairVector(Sections[i].Data)) + '</li>' +
      LineEnding;
  end;
  Result := Result + '</ol>' + LineEnding;
end;

{ THTMLDocGenerator ---------------------------------------------------------- }

function THTMLDocGenerator.MakeBodyBegin: string;

  function MakeNavigation: string;

    function LocalMakeLink(const Filename, Caption: string): string;
    begin
      Result := '<a href="' + EscapeURL(Filename) + '">' + ConvertString(Caption) + '</a>';
    end;

    function LocalMakeListItemLink(const Filename, Caption: string): string; overload;
    begin
      Result := '<li>' + LocalMakeLink(Filename, Caption) + '</li>';
    end;

    function LocalMakeListItemLink(const Filename: string; CaptionId: TTranslationID): string; overload;
    begin
      Result := LocalMakeListItemLink(Filename, FLanguage.Translation[CaptionId]);
    end;

  var
    Overview: TCreatedOverviewFile;
    i: Integer;
    IndexLinkText: String;
  begin
    Result := '';

    if (Introduction <> nil) or (Title <> '') then
    begin
      { Note: We don't make a special link to introduction,
        LocalMakeLink(Introduction.OutputFileName, ...),
        because introduction is already copied to "index.html" if Introduction <> nil
        (by WriteIndex). }

      { Determine the most suitable text for the "index.html" link. }
      IndexLinkText := '';
      if (IndexLinkText = '') and (Introduction <> nil) then
        IndexLinkText := Introduction.ShortTitle;
      if IndexLinkText = '' then
        IndexLinkText := Title;
      if IndexLinkText = '' then
        IndexLinkText := FLanguage.Translation[trIntroduction];
      { We need to have some non-empty text,
        which is why we use Translation[trIntroduction] as a last fallback.
        Otherwise the link would not be visible and introduction would not be visible. }
      Assert(IndexLinkText <> '');

      Result := Result + '<h2>' + LocalMakeLink('index.html', IndexLinkText) + '</h2>';
    end;

    Result := Result + '<ul>';

    for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    begin
      Result := Result + LocalMakeListItemLink(
        OverviewFilesInfo[Overview].BaseFileName + GetFileExtension,
        OverviewFilesInfo[Overview].TranslationId);
    end;

    if LinkGraphVizUses <> '' then
    begin
      Result := Result + LocalMakeListItemLink(
        OverviewFilesInfo[ofGraphVizUses].BaseFileName + '.' + LinkGraphVizUses,
        OverviewFilesInfo[ofGraphVizUses].TranslationId);
    end;

    if LinkGraphVizClasses <> '' then
    begin
      Result := Result + LocalMakeListItemLink(
        OverviewFilesInfo[ofGraphVizClasses].BaseFileName + '.' + LinkGraphVizClasses,
        OverviewFilesInfo[ofGraphVizClasses].TranslationId);
    end;

    if (AdditionalFiles <> nil) and (AdditionalFiles.Count > 0) then
    begin
      for i := 0 to AdditionalFiles.Count - 1 do
      begin
        if AdditionalFiles.Get(i).ShortTitle = '' then
          Result := Result + LocalMakeListItemLink(AdditionalFiles.Get(i).OutputFileName, trAdditionalFile) else
          Result := Result + LocalMakeListItemLink(AdditionalFiles.Get(i).OutputFileName, AdditionalFiles.Get(i).ShortTitle);
      end;
    end;

    if Conclusion <> nil then
    begin
      if Conclusion.ShortTitle = '' then
        Result := Result + LocalMakeListItemLink(Conclusion.OutputFileName, trConclusion) else
        Result := Result + LocalMakeListItemLink(Conclusion.OutputFileName, Conclusion.ShortTitle);
    end;

    if UseTipueSearch then
      Result := Result + '<li>' + Format(TipueSearchButton, [ConvertString(FLanguage.Translation[trSearch])]) + '</li>';
  end;

begin
  Result := inherited;
  { TODO: get rid of <table> layout, use <div> for navigation instead }
  Result := Result + '<div class="container"><div class="navigation">' + LineEnding;
  Result := Result + MakeNavigation;
  Result := Result + '</ul></div><div class="content">' + LineEnding;
end;

function THTMLDocGenerator.MakeBodyEnd: string;
begin
  Result := '</div></div>'; // end <table class="container">
  Result := Result + inherited;
end;

end.
