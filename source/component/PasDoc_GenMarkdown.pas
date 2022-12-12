{
  Copyright 1998-2022 PasDoc developers.

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

{ @abstract(Provides Markdown document generator object.)
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
  @author(Pablo Duboue <pablo.duboue@gmail.com>)

  Implements an object to generate Markdown documentation, overriding many of
  @link(TGenericHTMLDocGenerator)'s virtual methods.}

unit PasDoc_GenMarkdown;

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
  PasDoc_StringPairVector,
  PasDoc_GenHtml;

type
  { @abstract(generates Markdown documentation)
    Extends @link(TGenericHTMLDocGenerator) and overwrites many of its methods
    to generate output in Markdown format. }
  TMarkdownDocGenerator = class(TGenericHTMLDocGenerator)
  private
    FInHtmlCode: boolean;

    procedure WriteEndOfDocument; override;
    procedure WriteFooter; override;

    { Makes a link.
      @param href is the link's reference
      @param caption is the link's text }
    function MakeLink(const href, caption: string): string;
    function MakeLink(const href, caption, cssStr: string): string; override;

    procedure WriteItemsSummary(Items: TPasItems; ShowVisibility: boolean;
      HeadingLevel: Integer;
      const SectionAnchor: string; SectionName: TTranslationId); override;

    procedure WriteItemsDetailed(Items: TPasItems; ShowVisibility: boolean;
      HeadingLevel: Integer; SectionName: TTranslationId);  override;

    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo; override;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: integer; Authors: TStringVector);

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

    procedure WriteStartOfDocument(AName: string); override;

    procedure WriteStartOfTableCell(const CssClass: string=''); override;

    procedure WriteStartOfTable(const cssString: string=''); override;
    procedure WriteStartOfTable2Columns(const t1, t2: string);
    procedure WriteStartOfTable3Columns(const t1, t2, t3: string);
    procedure WriteStartOfTableRow(const cssStr: string = ''); override;

    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem); override;

    { write the legend file for visibility markers }
    procedure WriteVisibilityLegendFile;
    function MakeImage(const src, alt: string): string;
    function MakeHtmlImage(const src, alt: string): string;
    { writes a link
      @param href is the link's reference
      @param caption is the link's caption (must already been converted) }
    procedure WriteLink(const href, caption: String);
    procedure WriteHtmlLink(const href, caption: String);

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
    procedure WriteHeading(HL: integer; const s: string);

    procedure WriteHeading(HL: integer; const cssStr, s: string); override;

    { Returns a heading line. You can also make the anchor
      at this heading by passing AnchorName <> ''. }
    function FormatHeading(HL: integer; const s: string;
      const AnchorName: string): string;

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string);

    function FormatAnAnchor(const AName, Caption: string): string; override;

  protected
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string;
      WriteItemLink: boolean); override;

    procedure WriteStartOfCode; override;
    procedure WriteEndOfCode; override;

    procedure WriteOverviewFiles; override;

    { Finishes an HTML table cell by writing a closing TD tag. }
    procedure WriteEndOfTableCell; override;

    function ConvertString(const s: string): string; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;

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

    { Makes a String look like a coded String, i.e. `TheString`
      in Html. }
    function CodeString(const s: string): string; override;

    function Paragraph: string; override;

    function EnDash: string; override;
    function EmDash: string; override;

    function LineBreak: string; override;

    function MakeItemLinkMd(const Item: TBaseItem;
      const LinkCaption: string;
      const LinkContext: TLinkContext): string;

    function FormatBold(const Text: string): string; override;
    function FormatItalic(const Text: string): string; override;

    function FormatPreformatted(const Text: string): string; override;

    function FormatList(ListData: TListData): string; override;

    function FormatTable(Table: TTableData): string; override;

    function FormatTableOfContents(Sections: TStringPairVector): string; override;
    function FormatTableOfContents(Sections: TStringPairVector; Depth: integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;

    { Markdown only works with auto-abstract. }
    procedure ExpandDescriptions; override;
  end;

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

constructor TMarkdownDocGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInHtmlCode := false;
end;

destructor TMarkdownDocGenerator.Destroy;
begin
  inherited;
end;

function TMarkdownDocGenerator.FormatString(AString: string): string;
begin
  result := '*' + ConvertString(AString) + '*';
end;

function TMarkdownDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := '**' + ConvertString(AString) + '**';
end;

function TMarkdownDocGenerator.FormatComment(AString: string): string;
begin
  result := '* **' + ConvertString(AString) + '** *';
end;

function TMarkdownDocGenerator.FormatHex(AString: string): string;
begin
  result := '`' + ConvertString(AString) + '`';
end;

function TMarkdownDocGenerator.FormatNumeric(AString: string): string;
begin
  result := ConvertString(AString);
end;

function TMarkdownDocGenerator.FormatFloat(AString: string): string;
begin
  result := ConvertString(AString);
end;

function TMarkdownDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := '`' + ConvertString(AString) + '`';
end;

function TMarkdownDocGenerator.CodeString(const s: string): string;
begin
  Result := '`' + s + '`';
end;

function TMarkdownDocGenerator.GetFileExtension: string;
begin
  Result := '.md';
end;

procedure TMarkdownDocGenerator.WriteAppInfo;
begin
  if (not ExcludeGenerator) or IncludeCreationTime then
  begin
    { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
    WriteDirectLine(LineEnding + '<hr>' + LineEnding);
    WriteDirect('* ');
    if not ExcludeGenerator then
    begin
      WriteConverted(FLanguage.Translation[trGeneratedBy] + ' ');
      WriteLink(PASDOC_HOMEPAGE, PASDOC_NAME_AND_VERSION);
      WriteConverted('. ');
    end;
    if IncludeCreationTime then
    begin
      WriteConverted(FLanguage.Translation[trGeneratedOn] + ' ' +
        FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
      WriteConverted('.');
    end;
    WriteDirectLine('');
    WriteDirectLine(LineEnding);
  end;
end;

procedure TMarkdownDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  Address: string;
begin
  if IsEmpty(Authors) then Exit;

  if (Authors.Count = 1) then
    WriteHeading(HL, FLanguage.Translation[trAuthor])
  else
    WriteHeading(HL, FLanguage.Translation[trAuthors]);

  for i := 0 to Authors.Count - 1 do begin
    s := Authors[i];
    WriteDirect('- ');

    if ExtractEmailAddress(s, S1, S2, Address) then begin
      WriteConverted(S1);
      WriteLink('mailto:' + Address, ConvertString(Address));
      WriteConverted(S2);
    end else if ExtractWebAddress(s, S1, S2, Address) then begin
      WriteConverted(S1);
      WriteLink('http://' + Address, ConvertString(Address));
      WriteConverted(S2);
    end else begin
      WriteConverted(s);
    end;

    WriteDirectLine('');
  end;
  WriteDirectLine('');
end;

procedure TMarkdownDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);
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
    'packed record');

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

      WriteDirectLine('- ' + Name);
    end else
    if Item is TPasCio then
    begin
      CIO := TPasCio(Item);
      { first, write the ancestors }
      WriteHierarchy(CIO.Ancestors.FirstName, CIO.FirstAncestor);
      { then write itself }
      WriteDirectLine('- ' + MakeItemLinkMd(CIO, CIO.UnitRelativeQualifiedName, lcNormal))
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

  WriteHeading(HL, s);
  WriteAnchor(CIO.Name);
  WriteDirectLine('');

  WriteDirectLine('');
  for Section := Low(TSections) to High(TSections) do
    begin
      { Most classes don't contain nested types so exclude this stuff
        if not available in order to keep it simple. }
      if (not (Section in SectionsAvailable)) and
        (Section in [dsEnclosingClass..dsNestedTypes]) then
        Continue;

      WriteDirect('- ');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section])
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirectLine('');
    end;
  WriteDirectLine('');
  WriteAnchor(SectionAnchors[dsDescription]);
  WriteDirectLine('');

  { write unit link }
  if Assigned(CIO.MyUnit) then begin
    WriteHeading(HL + 1, FLanguage.Translation[trUnit]);
    WriteDirectLine(LineEnding);
    WriteLink(CIO.MyUnit.FullLink, ConvertString(CIO.MyUnit.Name));
    WriteDirectLine(LineEnding);
  end;

  { write declaration link }
  WriteHeading(HL + 1, FLanguage.Translation[trDeclaration]);
  WriteDirectLine(LineEnding);
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
  WriteDirectLine(LineEnding);

  { Write Description }
  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
  WriteItemLongDescription(CIO, false);

  { Write Hierarchy }
  if CIO.Ancestors.Count <> 0 then
  begin
    WriteDirectLine('');
    WriteHeading(HL + 1, SectionHeads[dsHierarchy]);
    WriteDirectLine(LineEnding);
    WriteAnchor(SectionAnchors[dsHierarchy]);
    WriteDirectLine(LineEnding);
    WriteHierarchy(CIO.Ancestors.FirstName, CIO.FirstAncestor);
    WriteDirectLine('- ' + CIO.UnitRelativeQualifiedName);
    WriteDirectLine('');
  end;

  { Write Enclosing Class }
  if CIO.MyObject <> nil then
  begin
    WriteDirectLine('');
    WriteHeading(HL + 1, SectionHeads[dsEnclosingClass]);
    WriteAnchor(SectionAnchors[dsEnclosingClass]);
    WriteDirectLine(LineEnding);
    WriteDirect('- ');
    WriteLink(CIO.MyObject.FullLink, CIO.MyObject.Name);
    WriteDirectLine('');
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
    WriteDirectLine('');
    WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
    WriteNestedCioSummary;
    WriteNestedTypesSummary;
    WriteFieldsSummary;
    WriteMethodsSummary;
    WritePropertiesSummary;

    WriteDirectLine('');
    WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
    WriteNestedTypesDetailed;
    WriteFieldsDetailed;
    WriteMethodsDetailed;
    WritePropertiesDetailed;
  end;

  WriteDirectLine('');
  WriteAuthors(HL + 1, CIO.Authors);
  WriteDates(HL + 1, CIO.Created, CIO.LastMod);
  WriteAppInfo;
end;

procedure TMarkdownDocGenerator.WriteCIOs(HL: integer; c: TPasItems);

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

procedure TMarkdownDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);

  procedure WriteCioRow(ACio: TPasCio);
  begin
    WriteStartOfTableRow;
    { name of class/interface/object and unit }
    WriteStartOfTableCell;
    WriteConverted(GetCIOTypeName(ACio.MyType));
    WriteDirect('&nbsp;');
    WriteLink(ACio.FullLink, CodeString(ACio.UnitRelativeQualifiedName));
    WriteEndOfTableCell;

    { Description of class/interface/object }
    WriteStartOfTableCell;
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


  WriteHeading(HL, FLanguage.Translation[trCio]);
  WriteAnchor('PasDoc-Classes');
  WriteDirectLine(LineEnding);

  inherited WriteStartOfTable2Columns('', FLanguage.Translation[trName], FLanguage.Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteCioRow(p);
  end;
  WriteEndOfTable;
end;

procedure TMarkdownDocGenerator.WriteCodeWithLinks(const p: TPasItem;
  const Code: string; WriteItemLink: boolean);
begin
  FInHtmlCode := true;
  inherited WriteCodeWithLinks(p, Code, WriteItemLink);
  FInHtmlCode := false;
end;


{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteDates(const HL: integer; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trCreated]);
    WriteDirectLine(LineEnding);
    WriteDirectLine(Created);
    WriteDirectLine(LineEnding);
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trLastModified]);
    WriteDirectLine(LineEnding);
    WriteDirectLine(LastMod);
    WriteDirectLine(LineEnding);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteEndOfDocument;
begin
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.ExpandDescriptions;
var
  OldAutoAbstract: Boolean;
begin
  OldAutoAbstract := AutoAbstract;
  AutoAbstract := true;
  inherited;
  AutoAbstract := OldAutoAbstract;
end;

{ ---------------------------------------------------------------------------- }

function TMarkdownDocGenerator.MakeItemLinkMd(
  const Item: TBaseItem;
  const LinkCaption: string;
  const LinkContext: TLinkContext): string;
begin
  Result := MakeLink(Item.FullLink, ConvertString(LinkCaption));
end;

function TMarkdownDocGenerator.MakeLink(
  const href, caption: string): string;
begin
  Result := Format('[%s](%s)', [
    Caption,
    EscapeURL(href)
  ]);
end;

function TMarkdownDocGenerator.MakeLink(
  const href, caption, cssStr: string): string;
begin
  if FInHtmlCode then
    Result := inherited MakeLink(href, caption, cssStr)
  else
    Result := MakeLink(href, caption);
end;

procedure TMarkdownDocGenerator.WriteLink(const href, caption: String);
begin
  WriteDirect(MakeLink(href, caption));
end;

procedure TMarkdownDocGenerator.WriteHtmlLink(const href, caption: String);
begin
  WriteDirect(inherited MakeLink(href, caption, ''));
end;

procedure TMarkdownDocGenerator.WriteEndOfTableCell;
begin
  WriteDirectLine(LineEnding + '</td>');
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteFooter;
begin
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteItemsSummary(
  Items: TPasItems; ShowVisibility: boolean; HeadingLevel: Integer;
  const SectionAnchor: string; SectionName: TTranslationId);
var
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, FLanguage.Translation[SectionName]);
  WriteAnchor(SectionAnchor);
  WriteDirectLine(LineEnding);

  WriteStartOfTable1Column;

  for i := 0 to Items.Count - 1 do
    WriteItemTableRow(Items.PasItemAt[i], ShowVisibility, true, false);

  WriteEndOfTable;
end;

procedure TMarkdownDocGenerator.WriteItemsDetailed(
  Items: TPasItems; ShowVisibility: boolean;
  HeadingLevel: Integer; SectionName: TTranslationId);
var
  Item: TPasItem;
  i: Integer;
  ColumnsCount: Cardinal;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, FLanguage.Translation[SectionName]);

  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.PasItemAt[i];

    { calculate ColumnsCount }
    ColumnsCount := 1;
    if ShowVisibility then Inc(ColumnsCount);

    WriteStartOfTable;
    WriteItemTableRow(Item, ShowVisibility, false, true);
    { Using colspan="0" below would be easier, but Konqueror and IE
      can't handle it correctly. It seems that they treat it as colspan="1" ? }
    WriteDirectLine(Format('<tr><td colspan="%d">', [ColumnsCount]) + LineEnding);
    WriteItemLongDescription(Item, false);
    WriteDirectLine(LineEnding + LineEnding + '</td></tr>');
    WriteEndOfTable;
  end;
end;

function TMarkdownDocGenerator.FormatHeading(HL: integer;
  const s: string; const AnchorName: string): string;
var
  c: string;
begin
  if (HL < 1) then HL := 1;
  if HL > 6 then begin
    DoMessage(2, pmtWarning, 'Markdown generator cannot write headlines of level 7 or greater; will use 6 instead.', []);
    HL := 6;
  end;
  c := '#';
  Dec(HL);
  while HL > 0 do
  begin
    c := c + '#';
    Dec(HL);
  end;

  Result := LineEnding + c + ' ' + ConvertString(S);
  if AnchorName <> '' then
    Result := Result + ' {#' + AnchorName + '}';
  Result := Result + LineEnding;
end;

procedure TMarkdownDocGenerator.WriteHeading(HL: integer; const s: string);
begin
  WriteDirect(FormatHeading(HL, s, ''));
end;

procedure TMarkdownDocGenerator.WriteHeading(HL: integer; const cssStr, s: string);
begin
  WriteHeading(HL, s);
end;

procedure TMarkdownDocGenerator.WriteItemShortDescription(const AItem: TPasItem);
var
  i: integer;
  found: boolean;
  text: string;
begin
  if AItem = nil then Exit;

  text := ' ';
  if AItem.AbstractDescription <> '' then
    text := AItem.AbstractDescription
  else begin
    if AItem.DetailedDescription <> '' then begin
      text := AItem.DetailedDescription
    end;
  end;

  found := false;
  for i := 1 to High(text) do
    if text[i] = Chr(10) then
    begin
      WriteSpellChecked(LeftStr(text, i-1));
      found := true;
      break;
    end;
  if not found then
    WriteSpellChecked(text);
end;

procedure TMarkdownDocGenerator.WriteItemLongDescription(
  const AItem: TPasItem; OpenCloseParagraph: boolean);

  procedure WriteDescriptionSectionHeading(const Caption: TTranslationID);
  begin
    WriteHeading(6, FLanguage.Translation[Caption]);
  end;

  procedure WriteNoDescription(const AItem: TPasItem);
  var
    InheritedDescriptions: TStringPairVector;
    AncestorItem: TPasItem;
    I: Integer;
  begin
    InheritedDescriptions := AItem.GetInheritedItemDescriptions;
    try
      if InheritedDescriptions.Count > 0 then
      begin
        WriteDirect(LineEnding + 'This item has no description. ');
        if InheritedDescriptions.Count = 1 then
        begin
          AncestorItem := TObject(InheritedDescriptions[0].Data) as TPasItem;
          WriteDirect('Showing description inherited from ');
          WriteDirect(MakeItemLinkMd(
            AncestorItem,
            InheritedDescriptions[0].Name,
            lcNormal));
          WriteDirectLine(':' + LineEnding);

          WriteDirectLine(LineEnding);
          WriteSpellChecked(InheritedDescriptions[0].Value);
          WriteDirectLine(LineEnding);
        end
        else begin
          WriteDirectLine('Showing descriptions inherited from ancestors.' +
            LineEnding);

          for I := 0 to InheritedDescriptions.Count - 1 do
          begin
            AncestorItem := TObject(InheritedDescriptions[I].Data) as TPasItem;

            WriteDirect('- From **');
            WriteDirect(MakeItemLinkMd(
              AncestorItem,
              InheritedDescriptions[I].Name,
              lcNormal));
            WriteDirect('**:');

            WriteDirectLine(LineEnding);
            WriteSpellChecked(InheritedDescriptions[I].Value);
            WriteDirectLine(LineEnding);
          end;
        end;
      end
      else begin
        WriteDirectLine('This item has no description.' + LineEnding);
      end;
    finally
      FreeAndNil(InheritedDescriptions);
    end;
  end;

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(ItemToSearchFrom: TPasItem; const Caption: TTranslationID;
    List: TStringPairVector; LinkToParamNames: boolean);

    procedure WriteParameter(const ParamName: string; const Desc: string);
    begin
      WriteDirect('<dt>');
      WriteDirect(ParamName);
      WriteDirectLine('</dt>');
      WriteDirectLine('<dd>');
      WriteDirectLine('');
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
    WriteDirectLine('<dl>');
    for i := 0 to List.Count - 1 do
    begin
      ParamName := List[i].Name;

      if LinkToParamNames then
       ParamName := SearchLink(
        ParamName,
        ItemToSearchFrom,
        '',
        lnfWarnIfNotInternal);

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
    WriteDirectLine('<dl>');
    for i := 0 to SeeAlso.Count - 1 do
    begin
      SeeAlsoLink := SearchLink(SeeAlso[i].Name, AItem,
        SeeAlso[i].Value, lnfWarn, SeeAlsoItem);
      WriteDirectLine('  <dt>');
      WriteDirectLine('');
      if SeeAlsoItem <> nil then
        WriteDirect(SeeAlsoLink) else
        WriteConverted(SeeAlso[i].Name);
      WriteDirectLine('');
      WriteDirectLine('</dt>');

      WriteDirectLine('  <dd>');
      WriteDirectLine('');
      if (SeeAlsoItem <> nil) and (SeeAlsoItem is TPasItem) then
        WriteDirect(TPasItem(SeeAlsoItem).AbstractDescription);
      WriteDirectLine('');
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
        AttributesLink := SearchLink(
          name,
          AItem,
          name,
          lnfWarnIfNotInternal,
          AttributesItem);
      WriteDirect(AttributesLink);

      WriteConverted(value);
      WriteDirectLine('</dt>');
      WriteDirectLine('  <dd>');
      WriteDirectLine('');

      if (AttributesItem <> nil) and (AttributesItem is TPasItem) then
        WriteDirect(TPasItem(AttributesItem).AbstractDescription);
      WriteDirectLine('');
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
  end else if AItem.DetailedDescription <> '' then
  begin
    if OpenCloseParagraph then WriteStartOfParagraph;

    WriteSpellChecked(AItem.DetailedDescription);

    if OpenCloseParagraph then WriteEndOfParagraph;
  end else
  begin
    if (AItem is TPasItem) then
      WriteNoDescription(AItem as TPasItem);
  end;

  WriteAttributes(AItem.Attributes);

  WriteParamsOrRaises(AItem, trParameters, AItem.Params, false);
  if AItem is TPasRoutine then
    WriteReturnDesc(TPasRoutine(AItem).Returns);
  WriteParamsOrRaises(AItem, trExceptionsRaised, AItem.Raises, true);

  WriteSeeAlso(AItem.SeeAlso);

  if AItem is TPasEnum then
  begin
    WriteDescriptionSectionHeading(trValues);
    WriteDirectLine('');
    for i := 0 to TPasEnum(AItem).Members.Count - 1 do
    begin
      EnumMember := TPasEnum(AItem).Members.PasItemAt[i];
      WriteDirect('- ');
      WriteAnchor(EnumMember.Name, ConvertString(EnumMember.FullDeclaration));
      if HasItemLongDescription(EnumMember) then
      begin
        WriteConverted(': ');
        WriteItemLongDescription(EnumMember, false);
      end;
      WriteDirectLine('');
    end;
    WriteDirectLine('');
  end;
end;

{ ---------- }

procedure TMarkdownDocGenerator.WriteOverviewFiles;

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
      WriteStartOfTable2Columns(FLanguage.Translation[trName],
        FLanguage.Translation[trDescription]);
      for j := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[j];
        WriteDirect('| ');
        WriteLink(Item.FullLink, Item.Name);
        WriteDirect(' | ');
        WriteItemShortDescription(Item);
        WriteDirectLine(' |');
      end;
      WriteDirectLine('');
    end;
    WriteAppInfo;
    CloseStream;
  end;

  { Writes a Hierarchy list - this is more useful than the simple class list }
  procedure WriteHierarchy;
  { todo -o twm: Make this recursive to handle closing </li> easily }
  var
    Level, OldLevel, j: Integer;
    Node: TPasItemNode;
  begin
    CreateClassHierarchy;

    if not CreateOverviewStream(ofClassHierarchy) then
      Exit;

    if FClassHierarchy.IsEmpty then begin
      WriteDirectLine(LineEnding);
      WriteConverted(FLanguage.Translation[trNoCIOsForHierarchy]);
      WriteDirectLine(LineEnding);
    end else begin
      OldLevel := -1;
      Node := FClassHierarchy.FirstItem;
      while Node <> nil do begin
        Level := Node.Level;
        while Level < OldLevel do begin
          Dec(OldLevel);
        end;
        OldLevel := Level;

        for j := 1 to Level do
           WriteDirect('   ');
        WriteDirect('- ');
        if Node.Item = nil then
          WriteConverted(Node.Name)
        else
          WriteLink(Node.Item.FullLink,
            ConvertString(Node.Item.UnitRelativeQualifiedName));
        WriteDirectLine('');
        Node := FClassHierarchy.NextItem(Node);
      end;

      WriteDirectLine('');
    end;

    WriteAppInfo;

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
      WriteStartOfTable3Columns(FLanguage.Translation[trName],
        FLanguage.Translation[trUnit],
        FLanguage.Translation[trDescription]);

      Items.SortShallow;

      for j := 0 to Items.Count - 1 do
      begin
        Item := Items.PasItemAt[j];
        WriteDirect('| ');
        WriteLink(Item.FullLink, Item.UnitRelativeQualifiedName);
        WriteDirect(' | ');
        WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name);
        WriteDirect(' | ');
        WriteItemShortDescription(Item);
        WriteDirectLine(' |');
      end;
    end else
    begin
      WriteDirectLine('');
      WriteConverted(FLanguage.Translation[
        OverviewFilesInfo[Overview].NoItemsTranslationId]);
      WriteDirectLine('');
    end;

    WriteAppInfo;
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

function TMarkdownDocGenerator.FormatAnAnchor(
  const AName, Caption: string): string;
begin
  if Caption <> '' then
    Result := Format('<span id="%s">%s</span>', [AName, Caption])
  else
    Result := '<span id="' + AName + '"/>';
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteStartOfCode;
begin
  if FInHtmlCode then
    inherited WriteStartOfCode
  else
    WriteDirect('```');
end;

procedure TMarkdownDocGenerator.WriteEndOfCode;
begin
  if FInHtmlCode then
    inherited WriteEndOfCode
  else
    WriteDirect('```');
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteStartOfDocument(AName: string);
begin
  // Title
  WriteDirect('# ');
  if Title <> '' then
    WriteConverted(Title + ': ' + AName + LineEnding + LineEnding)
  else
    WriteConverted(AName + LineEnding + LineEnding);
end;

procedure TMarkdownDocGenerator.WriteStartOfTable(const cssString: string='');
begin
  WriteDirectLine('');
  WriteDirectLine('<table>');
end;

procedure TMarkdownDocGenerator.WriteStartOfTable2Columns(const t1, t2: string);
begin
  WriteDirectLine('');
  WriteDirect('| ');
  WriteConverted(t1);
  WriteDirect(' | ');
  WriteConverted(t2);
  WriteDirectLine(' |');
  WriteDirectLine('|---|---|');
end;

procedure TMarkdownDocGenerator.WriteStartOfTable3Columns(const t1, t2, t3: string);
begin
  WriteDirectLine('');
  WriteDirect('| ');
  WriteConverted(t1);
  WriteDirect(' | ');
  WriteConverted(t2);
  WriteDirect(' | ');
  WriteConverted(t3);
  WriteDirectLine(' |');
  WriteDirectLine('|---|---|---|');
end;

procedure TMarkdownDocGenerator.WriteStartOfTableCell(const CssClass: string='');
begin
  WriteDirectLine(LineEnding + '<td>' + LineEnding);
end;

procedure TMarkdownDocGenerator.WriteStartOfTableRow(const cssStr: string='');
begin
  WriteDirectLine('<tr>');
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
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
    WriteHeading(HL, FLanguage.Translation[trDescription]);
    WriteItemLongDescription(U, false);
  end;

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
  var
    i: Integer;
    ULink: TPasItem;
  begin
    if WriteUsesClause and not IsEmpty(U.UsesUnits) then begin
      WriteHeading(HL, FLanguage.Translation[trUses]);
      for i := 0 to U.UsesUnits.Count-1 do begin
        WriteDirect('- ');
        ULink := TPasUnit(U.UsesUnits.Objects[i]);
        if ULink <> nil then begin
          WriteLink(ULink.FullLink, U.UsesUnits[i]);
        end else begin
          WriteConverted(U.UsesUnits[i]);
        end;
        WriteDirectLine('');
      end;
      WriteDirectLine('');
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
    DoMessage(1, pmtError, 'TMarkdownDocGenerator.WriteUnit: ' +
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
    WriteHeading(HL, FLanguage.Translation[trUnit] + ' ' + U.Name)
  else if U.IsProgram then
    WriteHeading(HL, FLanguage.Translation[trProgram] + ' ' + U.Name)
  else
    WriteHeading(HL, FLanguage.Translation[trLibrary] + ' ' + U.Name);

  WriteDirectLine('');
  for Section := Low(TSections) to High(TSections) do
    begin
      WriteDirect('- ');
      if Section in SectionsAvailable then
        WriteLink('#'+SectionAnchors[Section], SectionHeads[Section])
      else
        WriteConverted(SectionHeads[Section]);
      WriteDirectLine('');
    end;
  WriteDirectLine('');

  WriteAnchor(SectionAnchors[dsDescription]);
  WriteDirectLine('');
  WriteUnitDescription(HL + 1, U);

  WriteAnchor(SectionAnchors[dsUses]);
  WriteDirectLine('');
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
  WriteAppInfo;
  CloseStream;

  WriteCIOs(HL, U.CIOs);
end;

function TMarkdownDocGenerator.MakeImage(const src, alt: string): string;
begin
  Result := Format('![%s](%s "%s")', [alt, src, alt]);
end;

function TMarkdownDocGenerator.MakeHtmlImage(const src, alt: string): string;
begin
  Result := Format('<img src="%s" alt="%s" title="%s"></img>', [src, alt, alt]);
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

procedure TMarkdownDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(Vis: TVisibility);
  begin
    WriteHtmlLink('legend.md', MakeHtmlImage(VisibilityImageName[Vis],
      ConvertString(FLanguage.Translation[
        VisibilityTranslation[Vis]])));
  end;

begin
  WriteStartOfTableCell;
  WriteVisibilityImage(Item.Visibility);
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure TMarkdownDocGenerator.WriteVisibilityLegendFile;

  procedure WriteLegendEntry(Vis: TVisibility);
  var VisTrans: string;
  begin
    VisTrans := FLanguage.Translation[VisibilityTranslation[Vis]];
    WriteStartOfTableRow;
    WriteStartOfTableCell;
    WriteDirect(MakeImage(VisibilityImageName[Vis],
      ConvertString(VisTrans)));
    WriteEndOfTableCell;
    WriteStartOfTableCell;
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

    inherited WriteStartOfTable2Columns('', FLanguage.Translation[trMarker],
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

    WriteAppInfo;
  finally CloseStream; end;
end;

{ ---------------------------------------------------------------------------- }

function TMarkdownDocGenerator.ConvertString(const S: String): String;
const
  // do not escape '.', '(', ')', '-', '!', '+', '{', '}', '#' as they are rare cases
  ReplacementArray: array[0..8] of TCharReplacement = (
    (cChar: '\'; sSpec: '\\'),
    (cChar: '`'; sSpec: '\`'),
    (cChar: '*'; sSpec: '\*'),
    (cChar: '_'; sSpec: '\_'),
    (cChar: '['; sSpec: '\['),
    (cChar: ']'; sSpec: '\]'),
    (cChar: '&'; sSpec: '&amp;'),
    (cChar: '"'; sSpec: '&quot;'),
    (cChar: '^'; sSpec: '&circ;')
  );
begin
  Result := StringReplaceChars(S, ReplacementArray);
end;

function TMarkdownDocGenerator.FormatPascalCode(const Line: string): string;
begin
  result := LineEnding + LineEnding + '```pascal' + LineEnding +
       inherited FormatPascalCode(Line) + LineEnding + '```' + LineEnding +
       LineEnding;
end;

function TMarkdownDocGenerator.Paragraph: string;
begin
  Result := LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.EnDash: string;
begin
  Result := '--';
end;

function TMarkdownDocGenerator.EmDash: string;
begin
  Result := '---';
end;

function TMarkdownDocGenerator.LineBreak: string;
begin
  Result := LineEnding;
end;

function TMarkdownDocGenerator.FormatBold(const Text: string): string;
begin
  Result := '**' + Text + '**';
end;

function TMarkdownDocGenerator.FormatItalic(const Text: string): string;
begin
  Result := '*' + Text + '*';
end;

function TMarkdownDocGenerator.FormatPreformatted(
  const Text: string): string;
begin
  { See TMarkdownDocGenerator.FormatPascalCode
    for comments why these </p> and <p> are needed here.
    LineEndings are added only to make html source more readable. }
  Result := LineEnding + LineEnding +
    '```' +
       inherited FormatPreformatted(Text) + LineEnding + '```' +
     LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.FormatList(ListData: TListData): string;
const
  ListTag: array[TListType] of string = ( '-', '1.', '-' );
var
  i: Integer;
  ListItem: TListItemData;
  Attributes: string;
begin
  Result := LineEnding + LineEnding;

  for i := 0 to ListData.Count - 1 do
  begin
    ListItem := ListData.Items[i] as TListItemData;

    Result := Result + ListTag[ListData.ListType] + ' ';
    if ListData.ListType = ltDefinition then
    begin
      Result := Result +
        '  **' + ListItem.ItemLabel + '**:' +
        ListItem.Text + LineEnding;
    end else
    begin
      Result := Result + ListItem.Text + LineEnding;
    end;

    Result := Result + LineEnding;
  end;

  Result := Result + LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.FormatTable(Table: TTableData): string;
var
  RowNum, ColNum: Integer;
  Row: TRowData;
begin
  Result := LineEnding + LineEnding;
  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum] as TRowData;
    Result := Result + '|';

    for ColNum := 0 to Row.Cells.Count - 1 do
      Result := Result + ' ' + Row.Cells[ColNum] + ' |';
    if Row.Head then
    begin
        Result := Result + '|';
        for ColNum := 0 to Row.Cells.Count - 1 do
        begin
          Result := Result + '---|';
        end;
        Result := Result + LineEnding;
    end;
  end;
  Result := LineEnding + LineEnding;
end;

function TMarkdownDocGenerator.FormatTableOfContents(
  Sections: TStringPairVector): string;
begin
  Result := FormatTableOfContents(Sections, 0);
end;

function TMarkdownDocGenerator.FormatTableOfContents(
  Sections: TStringPairVector; Depth: integer): string;
var
  i: Integer;
  indent: String;
begin
  if Sections.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  indent := '';
  for i := 0 to Depth do
    indent := indent + '   ';

  for i := 0 to Sections.Count - 1 do
  begin
    Result := Result +
      indent + '1. [' + Sections[i].Value + '](#' + Sections[i].Name + ')' + LineEnding +
      FormatTableOfContents(TStringPairVector(Sections[i].Data), Depth + 1) +
      LineEnding;
  end;
  Result := Result + LineEnding + LineEnding;
end;

end.
