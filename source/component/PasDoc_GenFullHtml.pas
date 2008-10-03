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
  @author(Hans-Peter Diettrich <DrDiettrich1@aol.com>)
  @cvs($Date$)

  Implements an object to generate HTML documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenFullHtml;

interface

uses
  Classes,
  PasDoc_Utils,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_StringVector,
  PasDoc_GenHTML,
  PasDoc_Types;

type
  { @abstract(generates HTML documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in HTML (HyperText Markup Language) format. }
  TFullHTMLDocGenerator = class(TGenericHTMLDocGenerator)
  private
    procedure WriteSeeAlso(SeeAlso: TDescriptionItem; AItem: TPasScope);
  protected
    function  GetSectionAnchor(tid: TTranslationID): string;
    procedure WriteDescriptionSectionHeading(const Caption: TTranslationID);

  {$IFDEF old}
    procedure WriteItemsSummary(Items: TPasItems; ShowVisibility: boolean;
      HeadingLevel: Integer;
      const SectionAnchor: string; SectionName: TTranslationId);

    procedure WriteItemsDetailed(Items: TPasItems; ShowVisibility: boolean;
      HeadingLevel: Integer; SectionName: TTranslationId);
  {$ELSE}
    procedure WriteSummary(Items: TDescriptionItem; ShowVisibility: boolean;
      HeadingLevel: Integer);

    procedure WriteDetailed(Items: TDescriptionItem; ShowVisibility: boolean;
      HeadingLevel: Integer);
  {$ENDIF}

    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    //procedure WriteAuthors(HL: integer; Authors: TStringVector);
    procedure WriteAuthors(HL: integer; Authors: TDescriptionItem);

    { Writes the Item's short description.
      This is either the explicit AbstractDescription (@@abstract)
      or the (abbreviated) DetailedDescription. }
    procedure WriteItemShortDescription(const AItem: TPasItem);

    (*Writes the Item's AbstractDescription followed by DetailedDescription.
      Include further descriptions, depening on item kind (parameters...).

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

    procedure WriteOverviewFiles;

    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio);

    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems);

    procedure WriteCIOSummary(HL: integer; c: TPasItems);

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; Created, LastMod: TDescriptionItem);

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;

  public
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
  end;

implementation

uses
  SysUtils,
  StrUtils, { if you are using Delphi 5 or fpc 1.1.x you must add ..\component\strutils to your search path }
  PasDoc_Base,
  PasDoc_ObjectVector,
  PasDoc_Tipue,
  PasDoc_Aspell;

{ TFullHTMLDocGenerator }

function  TFullHTMLDocGenerator.GetSectionAnchor(tid: TTranslationID): string;
begin
  Result := '#%40' + IntToStr(ord(tid));
end;

procedure TFullHTMLDocGenerator.WriteAuthors(HL: integer; Authors: TDescriptionItem);
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
    s := Authors.GetString(i);
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

procedure TFullHTMLDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);
{$IFDEF old}
type
  TSections = (dsDescription, dsHierarchy, dsFields, dsMethods, dsProperties);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
{$ELSE}
type
  TSections = (dsDescription, dsHierarchy, dsFields, dsMethods, dsProperties);
  TSectionSet = set of TTranslationID;
const
//ordered menu entries
  SectionsInMenu: array[TSections] of TTranslationID = (
    trDescription, trHierarchy, trFields, trMethods, trProperties
  );
{$ENDIF}
{$IFDEF old}
const
  SectionAnchors: TSectionAnchors = (
    '%40Description',
    '%40Hierarchy',
    '%40Fields',
    '%40Methods',
    '%40Properties');
  SectionIDs: array[TSections] of TTranslationID = (
    trDescription, trHierarchy, trFields, trMethods, trProperties
  );
{$ELSE}
{$ENDIF}

{$IFDEF todo}
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

  { writes all ancestors of the given item and the item itself }
  procedure WriteHierarchy(Name: string; Item: TBaseItem);
  var
    CIO: TPasCio;
  begin
    if not Assigned(Item) then begin
      WriteDirectLine('<li class="ancestor">' + Name + '</li>');
      { recursion ends here, when the item is an external class }
    end else if Item is TPasCio then begin
      CIO := TPasCio(Item);
      { first, write the ancestors }
      WriteHierarchy(CIO.FirstAncestorName, CIO.FirstAncestor);
      { then write itself }
      WriteDirectLine('<li class="ancestor">' +
        MakeItemLink(CIO, CIO.Name, lcNormal) + '</li>')
    end;
    { todo --check: Is it possible that the item is assigned but is not a TPasCio ?
      Answer: type A = B; will result in an ordinary type A, even if B is a CIO.
      Type inheritance should be handled in the parser.
    }
  end;
{$ELSE}
  { writes an ancestor }
  procedure WriteAncestor(Item: TDescriptionItem);
  var
    CIO: TPasItem;
  begin
    CIO := Item.PasItem;
    if not Assigned(CIO) then begin
      WriteDirectLine('<li class="ancestor">' + Item.Name + '</li>');
      { recursion ends here, when the item is an external class }
    end else begin
      WriteDirectLine('<li class="ancestor">' +
        MakeItemLink(CIO, CIO.Name, lcNormal) + '</li>')
    end;
  end;
{$ENDIF}

var
  //i: Integer;
  s: string;
  SectionsAvailable: TSectionSet;
  Section: TSections;
  //AnyItem: boolean;
  //ancestor: TDescriptionItem;
{$IFDEF old}
  SectionHeads: array[TSections] of string;
{$ELSE}
  procedure FindSections;
  var
    i: integer;
    d: TDescriptionItem;
  begin
    SectionsAvailable := [];
    for i := 0 to CIO.Count - 1 do begin
      d := CIO.ItemAt(i);
      Include(SectionsAvailable, d.ID);
    end;
  end;
{$ENDIF}
var
  i, j: integer;
  tid: TTranslationID;
  d: TDescriptionItem;
begin //WriteCIO
  if not Assigned(CIO) then Exit;

  FindSections;

  s := GetCIOTypeName(CIO.MyType) + ' ' + CIO.Name;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);

  WriteAnchor(CIO.Name);
  WriteHeading(HL, 'cio', s);

//write menu bar
  WriteStartOfTable('sections');
  WriteDirectLine('<tr>');
  for Section := Low(Section) to High(Section) do begin
    WriteDirect('<td>');
    tid := SectionsInMenu[section];
    s := FLanguage.Translation[tid];
    if tid in SectionsAvailable then
      //WriteLink('#'+SectionAnchors[Section], s, 'section')
      WriteLink(GetSectionAnchor(tid), s, 'section')
    else
      WriteConverted(s);
    WriteDirect('</td>');
  end;
  WriteDirectLine('</tr></table>');

//write unit link
  if True then begin
    WriteHeading(HL + 1, 'unit', FLanguage.Translation[trUnit]);
    WriteStartOfParagraph('unitlink');
      WriteLink(CIO.MyUnit.FullLink, ConvertString(CIO.MyUnit.Name), '');
    WriteEndOfParagraph;
  end;

//write declaration link
  //if trDeclaration in SectionsAvailable then begin
  if True then begin
    WriteHeading(HL + 1, 'declaration', FLanguage.Translation[trDeclaration]);
    WriteStartOfParagraph('declaration');
      WriteStartOfCode;
        WriteConverted(CIO.FullDeclaration);
      WriteEndOfCode;
    WriteEndOfParagraph;
  end;

  for i := 0 to CIO.Count - 1 do begin
    d := CIO.ItemAt(i);
    case d.ID of
    trDescription:
      begin
        WriteAnchor(GetSectionAnchor(d.ID));
        WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
        WriteItemLongDescription(CIO);
      end;
    trHierarchy:
      begin
        WriteAnchor(GetSectionAnchor(d.ID));
        s := FLanguage.Translation[CIO.Ancestors.id];
        WriteHeading(HL + 1, 'hierarchy', s); // SectionHeads[dsHierarchy]);
        WriteDirect('<ul class="hierarchy">');
          //WriteHierarchy(CIO.FirstAncestorName, CIO.FirstAncestor);
          for j := 0 to d.Count - 1 do begin
            WriteAncestor(d.Items[j]);
          end;
          //WriteDirect('<li class="thisitem">' + CIO.Name + '</li>');
        WriteDirect('</ul>');
      end;
    trOverview:
      begin
        WriteHeading(HL + 1, 'overview', FLanguage.Translation[trOverview]);
      {$IFDEF old}
        WriteFieldsSummary;
        WriteMethodsSummary;
        WritePropertiesSummary;
      {$ELSE}
      {$ENDIF}
        for j := 0 to d.Count - 1 do begin
          WriteSummary(d.ItemAt(j), CIO.ShowVisibility, HL+1);
        end;

        WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
      {$IFDEF old}
        WriteFieldsDetailed;
        WriteMethodsDetailed;
        WritePropertiesDetailed;
      {$ELSE}
        for j := 0 to d.Count - 1 do begin
          WriteSummary(d.ItemAt(j), CIO.ShowVisibility, HL+1);
        end;
      {$ENDIF}
      end;
    trAuthors:
      WriteAuthors(HL + 1, CIO.Authors);
    trCreated, trLastModified:
      WriteDates(HL + 1, CIO.Created, CIO.LastMod);
    trSeeAlso:
    else
    end;
  end;

  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
end;

procedure TFullHTMLDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
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
      DoMessage(3, pmtInformation, 'Data for "%s" was loaded from cache, '+
        'and output file of this item exists and is newer than cache, '+
        'skipped.', [p.Name]);
      Continue;
    end;

    case CreateStream(p.OutputFileName, true) of
      csError: begin
          DoMessage(1, pmtError, 'Could not create Class/Interface/Object documentation file.', []);
          Continue;
        end;
      csCreated: begin
          DoMessage(3, pmtInformation, 'Creating Class/Interface/Object file for "%s"...', [p.Name]);
          WriteCIO(HL, p);
        end;
    end;
  end;
  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure TFullHTMLDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  WriteAnchor('%40Classes');

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
    WriteItemShortDescription(p);

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

{ ---------------------------------------------------------------------------- }

procedure TFullHTMLDocGenerator.WriteDates(const HL: integer;
  Created, LastMod: TDescriptionItem);
begin
  if assigned(Created) and (Created.Name <> '') then begin
    WriteHeading(HL, 'created', FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteDirectLine(Created.Name);
    WriteEndOfParagraph;
  end;
  if assigned(LastMod) and (LastMod.Name <> '') then begin
    WriteHeading(HL, 'modified', FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteDirectLine(LastMod.Name);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

{$IFDEF old}
procedure TFullHTMLDocGenerator.WriteItemsSummary(
  Items: TPasItems; ShowVisibility: boolean; HeadingLevel: Integer;
  const SectionAnchor: string; SectionName: TTranslationId);
var
  i: Integer;
begin
  if IsEmpty(Items) then Exit; //should never happen

  WriteAnchor(SectionAnchor);
  WriteHeading(HeadingLevel + 1, 'summary', FLanguage.Translation[SectionName]);
  WriteStartOfTable1Column('summary');

  for i := 0 to Items.Count - 1 do
    WriteItemTableRow(Items.PasItemAt[i], ShowVisibility, true, false);

  WriteEndOfTable;
end;

procedure TFullHTMLDocGenerator.WriteItemsDetailed(
  Items: TPasItems; ShowVisibility: boolean;
  HeadingLevel: Integer; SectionName: TTranslationId);
var
  Item: TPasItem;
  i: Integer;
  ColumnsCount: Cardinal;
begin
  if IsEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, 'detail', FLanguage.Translation[SectionName]);

  for i := 0 to Items.Count - 1 do begin
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
{$ELSE}
procedure TFullHTMLDocGenerator.WriteSummary(Items: TDescriptionItem; ShowVisibility: boolean;
      HeadingLevel: Integer);
var
  i: Integer;
begin
  if IsEmpty(Items) then Exit; //should never happen

  WriteAnchor(GetSectionAnchor(Items.ID));
  WriteHeading(HeadingLevel + 1, 'summary', FLanguage.Translation[Items.id]);
  WriteStartOfTable1Column('summary');

  for i := 0 to Items.Count - 1 do
    WriteItemTableRow(Items.PasItemAt(i), ShowVisibility, true, false);

  WriteEndOfTable;
end;

procedure TFullHTMLDocGenerator.WriteDetailed(Items: TDescriptionItem; ShowVisibility: boolean;
  HeadingLevel: Integer);
var
  Item: TPasItem;
  i: Integer;
  ColumnsCount: Cardinal;
begin
  if IsEmpty(Items) then Exit;

  WriteHeading(HeadingLevel + 1, 'detail', FLanguage.Translation[Items.id]);

//calculate ColumnsCount
  ColumnsCount := 1;
  if ShowVisibility then Inc(ColumnsCount);

  for i := 0 to Items.Count - 1 do begin
    Item := Items.PasItemAt(i);

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
{$ENDIF}

procedure TFullHTMLDocGenerator.WriteDescriptionSectionHeading(const Caption: TTranslationID);
begin
  WriteHeading(6, 'description_section', FLanguage.Translation[Caption]);
end;

procedure TFullHTMLDocGenerator.WriteItemShortDescription(const AItem: TPasItem);
begin
//no spell checking, short description can be truncated
  if AItem <> nil then
    WriteDirect(AItem.ShortDescription);
end;

procedure TFullHTMLDocGenerator.WriteItemLongDescription(
  const AItem: TPasItem; OpenCloseParagraph: boolean);

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: TTranslationID;
    List: TDescriptionItem; LinkToParamNames: boolean;
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
    if IsEmpty(List) then
      Exit;

    WriteDescriptionSectionHeading(Caption);
    WriteDirectLine('<dl class="' + CssListClass + '">');
    for i := 0 to List.Count - 1 do begin
      ParamName := List.Items[i].Name;

      if LinkToParamNames then
       ParamName := SearchLink(ParamName, Func, '', true);

      WriteParameter(ParamName, List.Items[i].Value);
    end;
    WriteDirectLine('</dl>');
  end;

  procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: TDescriptionItem);
  begin
    if (ReturnDesc = nil) or (ReturnDesc.Text = '') then
      exit;
    WriteDescriptionSectionHeading(trReturns);
    WriteDirect('<p class="return">');
    WriteSpellChecked(ReturnDesc.Text);
    WriteDirect('</p>');
  end;

  procedure WriteHintDirective(const S: string);
  begin
    WriteDirect('<p class="hint_directive">');
    WriteConverted(FLanguage.Translation[trWarning] + ': ' + S + '.');
    WriteDirect('</p>');
  end;

var
  Ancestor: TBaseItem;
  AItemMethod: TPasMethod;
  i: Integer;
begin //WriteItemLongDescription
  if not Assigned(AItem) then Exit;

  //if AItem.IsDeprecated then
  if AItem.HasAttribute[SD_DEPRECATED] then
    WriteHintDirective(FLanguage.Translation[trDeprecated]);
  //if AItem.IsPlatformSpecific then
  if AItem.HasAttribute[SD_PLATFORM] then
    WriteHintDirective(FLanguage.Translation[trPlatformSpecific]);
  //if AItem.IsLibrarySpecific then
  if AItem.HasAttribute[SD_LIBRARY_] then
    WriteHintDirective(FLanguage.Translation[trLibrarySpecific]);

(* Write Abstract and Description, if not empty.
  If neither exists, give inherited description (CIOs only)
  The same for overloaded methods???
  Every description item should be inheritable!
*)
{$IFDEF old}
  if AItem.AbstractDescription <> '' then begin
    if OpenCloseParagraph then WriteStartOfParagraph;

    WriteSpellChecked(AItem.AbstractDescription);

    if AItem.DetailedDescription <> '' then begin
      if not AItem.AbstractDescriptionWasAutomatic then begin
        WriteEndOfParagraph; { always try to write closing </p>, to be clean }
        WriteStartOfParagraph;
      end;
      WriteSpellChecked(AItem.DetailedDescription);
    end;

    if OpenCloseParagraph then WriteEndOfParagraph;
  end else if AItem.DetailedDescription <> '' then begin
    if OpenCloseParagraph then WriteStartOfParagraph;

    WriteSpellChecked(AItem.DetailedDescription);

    if OpenCloseParagraph then WriteEndOfParagraph;

  end else if (AItem is TPasCio) and not IsEmpty(TPasCio(AItem).Ancestors) then begin
    Ancestor := TPasCio(AItem).FirstAncestor;
    if Assigned(Ancestor) then begin
      //AncestorName := TPasCio(AItem).FirstAncestorName;
      //AncestorName := Ancestor.Name;
      WriteDirect('<div class="nodescription">');
      WriteConverted(Format(
        'no description available, %s description follows', [Ancestor.Name]));
      WriteDirect('</div>');
      WriteItemLongDescription(TPasItem(Ancestor));
    end;
{$ELSE}
//search for non-empty description
  Ancestor := AItem;
  while assigned(Ancestor)
  and (Ancestor.AbstractDescription = '') and (Ancestor.DetailedDescription = '') do begin
    if Ancestor is TPasCio then
      Ancestor := TPasCio(Ancestor).FirstAncestor
    else
      Ancestor := nil;
  end;

  if (Ancestor <> nil) then begin
    if AItem <> Ancestor then begin
      WriteDirect('<div class="nodescription">');
      WriteConverted(Format(
        'no description available, %s description follows', [Ancestor.Name]));
      WriteDirect('</div>');
    end;

    if Ancestor.AbstractDescription <> '' then begin
      if OpenCloseParagraph then WriteStartOfParagraph;
      WriteSpellChecked(Ancestor.AbstractDescription);
      if OpenCloseParagraph then WriteEndOfParagraph;
    end;

    if Ancestor.DetailedDescription <> '' then begin
      if OpenCloseParagraph then WriteStartOfParagraph;
      WriteSpellChecked(Ancestor.DetailedDescription);
      if OpenCloseParagraph then WriteEndOfParagraph;
    end;
  end else begin
    //WriteDirect('&nbsp;'); //oops?
  end;
{$ENDIF}

  if AItem is TPasMethod then begin
    AItemMethod := TPasMethod(AItem);
    WriteParamsOrRaises(AItemMethod, trParameters,
      AItemMethod.Params, false, 'parameters');
    WriteReturnDesc(AItemMethod, AItemMethod.Returns);
    WriteParamsOrRaises(AItemMethod, trExceptionsRaised,
      AItemMethod.Raises, true, 'exceptions_raised');
  end;

  WriteSeeAlso(AItem.SeeAlso, AItem.MyOwner);

  if AItem is TPasEnum then begin
    WriteDescriptionSectionHeading(trValues);
    WriteDirectLine('<ul>');
    for i := 0 to TPasEnum(AItem).Members.Count - 1 do begin
      WriteDirectLine('<li>');
      WriteConverted(TPasEnum(AItem).Members.PasItemAt[i].FullDeclaration);
      WriteConverted(': ');
      WriteItemLongDescription(TPasEnum(AItem).Members.PasItemAt[i], false);
      WriteDirectLine('</li>');
    end;
    WriteDirectLine('</ul>');
  end;
end;

procedure TFullHTMLDocGenerator.WriteSeeAlso(SeeAlso: TDescriptionItem; AItem: TPasScope);
var
  i: integer;
  item: TDescriptionItem;
  SeeAlsoItem: TBaseItem;
  SeeAlsoLink: string;
begin
  if IsEmpty(SeeAlso) then Exit;

  WriteDescriptionSectionHeading(trSeeAlso);
  WriteDirectLine('<dl class="see_also">');
  for i := 0 to SeeAlso.Count - 1 do begin
    item := SeeAlso.ItemAt(i);
    SeeAlsoLink := SearchLink(item.Name, AItem,
      item.Value, true, SeeAlsoItem);
    WriteDirect('  <dt>');
      if SeeAlsoItem <> nil then
        WriteDirect(SeeAlsoLink)
      else
        WriteConverted(item.Name);
    WriteDirectLine('</dt>');

    WriteDirect('  <dd>');
      if (SeeAlsoItem <> nil) and (SeeAlsoItem is TPasItem) then
      {$IFDEF old}
      //direct write???
        WriteDirect(TPasItem(SeeAlsoItem).AbstractDescription);
      {$ELSE}
        //WriteConverted(TPasItem(SeeAlsoItem).AbstractDescription, False);
        WriteConverted(SeeAlsoItem.ShortDescription, False);
      {$ENDIF}
    WriteDirectLine('</dd>');
  end;
  WriteDirectLine('</dl>');
end;

{ ---------- }

procedure TFullHTMLDocGenerator.WriteOverviewFiles;

  function CreateOverviewStream(Overview: TCreatedOverviewFile): boolean;
  var
    BaseFileName, Headline: string;
  begin
    BaseFileName := OverviewFilesInfo[Overview].BaseFileName;
    Result := CreateStream(BaseFileName + GetFileExtension, True) <> csError;

    if not Result then
    begin
      DoMessage(1, pmtError, 'Error: Could not create output file "' +
        BaseFileName + '".', []);
      Exit;
    end;

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
        WriteItemShortDescription(Item);
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

    procedure WriteLevel(lst: TDescriptionItem);
    var
      i: integer;
      item: TDescriptionItem;
    begin
      if IsEmpty(lst) then
        exit;
      WriteDirectLine('<ul class="hierarchylevel">');
      for i := 0 to lst.Count - 1 do begin
        WriteDirect('<li>');
        item := lst.ItemAt(i);
        if Item.PasItem = nil then
          WriteConverted(item.Name)
        else
          WriteLink(Item.PasItem.FullLink, ConvertString(item.Name), 'bold');
        WriteLevel(item);
        WriteDirectLine('</li>');
      end;
      WriteDirectLine('</ul>');
    end;

  begin
    CreateClassHierarchy;

    if not CreateOverviewStream(ofClassHierarchy) then
      Exit;

    if IsEmpty(FClassHierarchy) then begin
      WriteStartOfParagraph;
      WriteConverted(FLanguage.Translation[trNoCIOsForHierarchy]);
      WriteEndOfParagraph;
    end else begin
      WriteLevel(FClassHierarchy);
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
        WriteItemShortDescription(Item);
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
  TotalItems: TPasItems; // Collect all Items for final listing.
  PU: TPasUnit;
  Overview: TCreatedOverviewFile;
  j: Integer;
begin //WriteOverviewFiles
  WriteUnitOverviewFile;
  WriteHierarchy;

  // Make sure we don't free the Items when we free the container.
  TotalItems := TPasItems.Create(False);
  try
    for Overview := ofCios to HighCreatedOverviewFile do begin
      // Make sure we don't free the Items when we free the container.
      PartialItems := TPasItems.Create(False);
      try
        for j := 0 to Units.Count - 1 do begin
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

procedure TFullHTMLDocGenerator.WriteDocumentation;
begin
(* Problem: skip inherited GenHTML!
  Solution: made method WriteDocumentationGen.
*)
  StartSpellChecking('sgml');
  WriteDocumentationGen; //bypass inherited WriteDocumentation
  WriteUnits(1); //calls WriteUnit() for every unit.
  WriteBinaryFiles;
  WriteOverviewFiles;
  WriteVisibilityLegendFile;
  WriteIntroduction;
  WriteConclusion;
  WriteFramesetFiles;
  if UseTipueSearch then begin
    DoMessage(2, pmtInformation,
      'Writing additional files for tipue search engine', []);
    TipueAddFiles(Units, Introduction, Conclusion, MetaContentType,
      DestinationDirectory);
  end;
  EndSpellChecking;
end;

procedure TFullHTMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
type
(* Menu bar entries.
*)
  TSections = (dsDescription, dsUses, dsClasses, dsFuncsProcs,
    dsTypes, dsConstants, dsVariables);
  TSectionSet = set of TSections;
  TSectionAnchors = array[TSections] of string;
const
  SectionAnchors: TSectionAnchors = (
    '%40Description',
    '%40Uses',
    '%40Classes',
    '%40FuncsProcs',
    '%40Types',
    '%40Constants',
    '%40Variables');
  SectionIDs: array[TSections] of TTranslationID = (
    trDescription, trUses, trClasses, trFunctionsAndProcedures,
    trTypes, trConstants, trVariables
  );

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
  (* Write section (anchor+caption), and list of links (merge with WriteSeeAlso?)
    Added: write section anchor.
  *)
    if WriteUsesClause and not IsEmpty(U.UsesUnits) then begin
      WriteHeading(HL, 'uses', FLanguage.Translation[trUses], SectionAnchors[dsUses]);
      WriteDirect('<ul class="useslist">');
      for i := 0 to U.UsesUnits.Count-1 do begin
        WriteDirect('<li>');
        ULink := u.UsesUnits.PasItemAt(i);
        if ULink <> nil then begin
          WriteLink(ULink.FullLink, U.UsesUnits.Items[i].Name, '');
        end else begin
          WriteConverted(U.UsesUnits.Items[i].Name);
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
  Section: TSections;
  s: string;
  AnyItemSummary, AnyItemDetailed: boolean;
{$IFDEF old}
  SectionHeads: array[TSections] of string;

  procedure ConditionallyAddSection(Section: TSections; Condition: boolean);
  begin
    if Condition then
      Include(SectionsAvailable, Section);
  end;

{$ELSE}
  procedure FindSections;
  var
    i: integer;
    d: TDescriptionItem;
  begin
    for i := 0 to U.Count - 1 do begin
      d := U.ItemAt(i);
      case d.ID of
      trDescription:  include(SectionsAvailable, dsDescription);
      trUses: if WriteUsesClause then Include(SectionsAvailable, dsUses);
      trClasses:  include(SectionsAvailable, dsClasses);
      trFunctionsAndProcedures:  include(SectionsAvailable, dsFuncsProcs);
      trTypes:  include(SectionsAvailable, dsTypes);
      trConstants:  include(SectionsAvailable, dsConstants);
      trVariables:  include(SectionsAvailable, dsVariables);
      end;
    end;
  end;
{$ENDIF}
begin
  case CreateStream(U.OutputFileName, true) of
    csError: begin
      DoMessage(1, pmtError, 'Could not create HTML unit doc file for unit %s.', [U.Name]);
      Exit;
    end;
  end;

{$IFDEF old}
  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsUses] := FLanguage.Translation[trUses];
  SectionHeads[dsClasses] := FLanguage.Translation[trCio];
  SectionHeads[dsFuncsProcs]:= FLanguage.Translation[trFunctionsAndProcedures];
  SectionHeads[dsTypes]:= FLanguage.Translation[trTypes];
  SectionHeads[dsConstants]:= FLanguage.Translation[trConstants];
  SectionHeads[dsVariables]:= FLanguage.Translation[trVariables];

  SectionsAvailable := [dsDescription];
  ConditionallyAddSection(dsUses, WriteUsesClause and not IsEmpty(U.UsesUnits));
  ConditionallyAddSection(dsClasses, not IsEmpty(U.CIOs));
  ConditionallyAddSection(dsFuncsProcs, not IsEmpty(U.FuncsProcs));
  ConditionallyAddSection(dsTypes, not IsEmpty(U.Types));
  ConditionallyAddSection(dsConstants, not IsEmpty(U.Constants));
  ConditionallyAddSection(dsVariables, not IsEmpty(U.Variables));
{$ELSE}
  FindSections;
{$ENDIF}

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteStartOfDocument(U.Name);
  WriteHeading(HL, 'unit', FLanguage.Translation[U.id] + ' ' + U.Name);

  WriteStartOfTable('sections');
  WriteDirectLine('<tr>');
  for Section := Low(TSections) to High(TSections) do begin
    WriteDirect('<td>');
    s := FLanguage.Translation[SectionIDs[section]];
    if Section in SectionsAvailable then
      WriteLink('#'+SectionAnchors[Section], s, 'section')
    else
      WriteConverted(s);
    WriteDirect('</td>');
  end;
  WriteDirectLine('</tr></table>');

  if dsDescription in SectionsAvailable then begin
    WriteAnchor(SectionAnchors[dsDescription]);
    WriteUnitDescription(HL + 1, U);
  end;

  WriteAnchor(SectionAnchors[dsUses]);
  WriteUnitUses(HL + 1, U);

  AnyItemDetailed :=
    (not ObjectVectorIsNilOrEmpty(U.FuncsProcs)) or
    (not ObjectVectorIsNilOrEmpty(U.Types)) or
    (not ObjectVectorIsNilOrEmpty(U.Constants)) or
    (not ObjectVectorIsNilOrEmpty(U.Variables));

//CIOs are included in the summary, but details reside in an different file.
  AnyItemSummary := AnyItemDetailed or
    (not ObjectVectorIsNilOrEmpty(U.CIOs));

  { AnyItemSummary/Detailed are used here to avoid writing headers
    "Overview" and "Description" when there are no items. }
  if AnyItemSummary then begin
    WriteHeading(HL + 1, 'overview', FLanguage.Translation[trOverview]);
    WriteCIOSummary(HL + 2, U.CIOs);
    WriteFuncsProcsSummary;
    WriteTypesSummary;
    WriteConstantsSummary;
    WriteVariablesSummary;
  end;

  if AnyItemDetailed then begin
    WriteHeading(HL + 1, 'description', FLanguage.Translation[trDescription]);
    //CIOs reside in their own files!
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

{ ---------------------------------------------------------------------------- }

procedure TFullHTMLDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
var
  HL: integer;
begin
  case CreateStream(ExternalItem.OutputFileName, true) of
    csError: begin
      DoMessage(1, pmtError, 'Could not create HTML unit doc file '
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

end.
