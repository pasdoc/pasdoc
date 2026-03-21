{
  Copyright 1998-2026 PasDoc developers.

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

{ @abstract(SimpleXML documentation generator @link(TSimpleXMLDocGenerator).) }
unit PasDoc_GenSimpleXML;

{$I pasdoc_defines.inc}

interface

uses
  PasDoc_Utils,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_StringVector,
  PasDoc_Types,
  Classes, Contnrs,
  PasDoc_StringPairVector;

type
  TSimpleXMLDocGenerator = class(TDocGenerator)
  protected
    function CodeString(const s: string): string; override;
    function ConvertString(const s: string): string; override;
    function ConvertChar(c: char): string; override;
    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;
    function FormatSection(HL: integer; const Anchor: string;
      const Caption: string): string; override;
    function FormatAnchor(const Anchor: string): string; override;
    function FormatTable(Table: TTableData): string; override;
    function FormatList(ListData: TListData): string; override;
    function FormatBold(const Text: string): string; override;
    function FormatItalic(const Text: string): string; override;
  private
    Space: String;

    { Returns XML <description> element with Item's AbstractDescription
      and DetailedDescription.
      Returns '' if Item doesn't have any description. }
    function ItemDescription(Item: TPasItem): string;

    { Common attributes from TPasItem properties. }
    function CommonAttributes(const Item: TPasItem): String;

    procedure WriteRoutine(const Item: TPasRoutine);
    procedure WriteConstant(const Item: TPasItem);
    procedure WriteVariable(const Item: TPasItem);
    procedure WriteType(const Item: TPasItem);
    procedure WriteStructure(const Item: TPasCIO);
    procedure WriteProperty(const Item: TPasProperty);

    { Add some indentation to Space. }
    procedure Indent;

    { Remove indentation from Space, reverting the work of last @link(Indent). }
    procedure UnIndent;
  public
    procedure WriteDocumentation; override;
    function GetFileExtension: string; override;
  end;

implementation

uses SysUtils;

function TSimpleXMLDocGenerator.GetFileExtension:string;
begin
  Result := '.xml';
end;

procedure TSimpleXMLDocGenerator.WriteDocumentation;
begin
  StartSpellChecking('sgml');
  inherited;
  WriteUnits(1);
  WriteIntroduction;
  WriteAdditionalFiles;
  WriteConclusion;
  EndSpellChecking;
end;

function TSimpleXMLDocGenerator.CodeString(const s: string): string;
begin
  Result := '<code>' + S + '</code>';
end;

function TSimpleXMLDocGenerator.ConvertString(const S: String): String;
const
  ReplacementArray: array[0..3] of TCharReplacement = (
    (cChar: '<'; sSpec: '&lt;'),
    (cChar: '>'; sSpec: '&gt;'),
    (cChar: '&'; sSpec: '&amp;'),
    (cChar: '"'; sSpec: '&quot;')
  );
begin
  Result := StringReplaceChars(S, ReplacementArray);
end;

function TSimpleXMLDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;

function TSimpleXMLDocGenerator.ItemDescription(Item: TPasItem): string;
begin
  if Item.HasDescription then
  begin
    { Abstract and Detailed descriptions are somewhat siblings,
      for most normal uses you want to glue them together.
      That's why I (Michalis) decided it's most sensible to put them
      as sibling XML elements, not make <abstract> child of <detailed>
      of something like this. }
    Result := '<description>';
    if Item.AbstractDescription <> '' then
      Result := Result + '<abstract>' + Item.AbstractDescription + '</abstract>';
    if Item.DetailedDescription <> '' then
      Result := Result + '<detailed>' + Item.DetailedDescription + '</detailed>';
    Result := Result + '</description>';
  end else
    Result := '';
end;

function TSimpleXMLDocGenerator.CommonAttributes(const Item: TPasItem): String;

  { XML attributes derived from TPasItem.SourceAbsoluteFileName and SourceLine. }
  function SourcePositionAttributes(const Item: TPasItem): String;
  var
    ItemName, ItemFilenameInRoot, ItemUrl: string;
  begin
    if HasSourcePosition(Item, ItemName, ItemFilenameInRoot, ItemUrl) then
      Result :=
        // machine-specific, don't output
        //' source-absolute-file-name="' + ConvertString(Item.SourceAbsoluteFileName) + '"' +
        ' source-line="' + IntToStr(Item.SourceLine) + '"' +
        ' source-relative-file-name="' + ConvertString(ItemFilenameInRoot) + '"' +
        ' source-url="' + ConvertString(ItemUrl) + '"'
    else
      Result := '';
  end;

  { XML attributes derived from TPasItem.HintDirectives and TPasItem.DeprecatedNote. }
  function HintDirectivesToString(const Item: TPasItem): String;
  const
    HintDirectiveXmlAttribute: array[THintDirective] of string = (
      'deprecated',
      'platform',
      'library',
      'experimental',
      'unimplemented'
    );
  var
    Directive: THintDirective;
  begin
    Result := '';
    if Item.DeprecatedNote <> '' then
      Result := Result + ' deprecated_note="' + ConvertString(Item.DeprecatedNote) + '"';
    for Directive := Low(THintDirective) to High(THintDirective) do
      if Directive in Item.HintDirectives then
        Result := Result + ' ' + HintDirectiveXmlAttribute[Directive] + '="true"';
  end;

begin
  Result := ' name="' + ConvertString(Item.name) + '"';
  if Item.FullDeclaration <> '' then
    Result := Result + ' declaration="' + ConvertString(Item.FullDeclaration) + '"';
  if Item.MyObject <> nil then // otherwise Item.visibility is meaningless
    Result := Result + ' visibility="' + VisToStr(Item.visibility) + '"';
  Result := Result +
    SourcePositionAttributes(item) +
    HintDirectivesToString(item);
end;

procedure TSimpleXMLDocGenerator.WriteRoutine(const Item: TPasRoutine);
var
  I: Integer;
begin
  WriteDirectLine(space +
    '<routine' + CommonAttributes(Item) +
    ' type="' + ConvertString(RoutineTypeToString(Item.What)) + '">');
    for I := 0 to Item.params.count - 1 do
      WriteDirectLine(space +
        '  <param name="' + ConvertString(Item.params[i].name) + '">' +
          Item.params[i].value +'</param>');
    if Item.returns <> '' then
      WriteDirectLine(space +
        '  <result>' + Item.returns + '</result>');
  if Item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space + '</routine>');
end;

procedure TSimpleXMLDocGenerator.WriteProperty(const Item: TPasProperty);
begin
  WriteDirectLine(space +
    '<property' + CommonAttributes(Item) +
       ' indexdecl="' + ConvertString(Item.indexDecl) +
            '" type="' + ConvertString(Item.Proptype) +
          '" reader="' + ConvertString(Item.reader) +
          '" writer="' + ConvertString(Item.writer) +
'" default_in_class="' + ConvertString(BoolToStr(Item.DefaultInClass, true)) +
   '" default_value="' + ConvertString(Item.DefaultValue) +
       '" nodefault="' + ConvertString(BoolToStr(Item.NoDefault, true)) +
        '"   stored="' + ConvertString(Item.Stored) + '">');
  if Item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</property>');
end;

procedure TSimpleXMLDocGenerator.WriteConstant(const Item: TPasItem);
begin
  WriteDirectLine(space +
    '<constant' + CommonAttributes(Item) + '>');
  if Item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</constant>');
end;

procedure TSimpleXMLDocGenerator.WriteVariable(const Item: TPasItem);
begin
  WriteDirectLine(space +
    '<variable' + CommonAttributes(Item) + '>');
  if Item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</variable>');
end;

procedure TSimpleXMLDocGenerator.WriteType(const Item: TPasItem);

  procedure WriteEnumMembers(const Item: TPasEnum);
  var
    I: Integer;
  begin
    for I := 0 to Item.Members.Count - 1 do
      WriteConstant(Item.Members.PasItemAt[i]);
  end;

begin
  WriteDirectLine(space +
      '<type' + CommonAttributes(Item) + '>');
  if Item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  if Item is TPasEnum then
  begin
    Indent;
    WriteEnumMembers(TPasEnum(Item));
    UnIndent;
  end;
  WriteDirectLine(space+'</type>');
end;

procedure TSimpleXMLDocGenerator.Indent;
begin
  Space := Space + '  ';
end;

procedure TSimpleXMLDocGenerator.UnIndent;
begin
  Space := Copy(Space, 0, Length(Space) - 2);
end;

procedure TSimpleXMLDocGenerator.WriteStructure(const Item: TPasCIO);
var
  I: Integer;
begin
  WriteDirectLine(space +
      '<structure' + CommonAttributes(Item) +
   ' name_with_generic="' + ConvertString(Item.NameWithGeneric) +
               '" type="' + ConvertString(CioTypeToString(Item.MyType)) + '">');
  Indent;

  if Item.HasDescription then
    WriteDirectLine(space + ItemDescription(Item));

  for I := 0 to Item.ancestors.count-1 do
    WriteDirectLine(space +
      '<ancestor name="' + ConvertString(Item.ancestors[i].Name) +
       '" declaration="' + ConvertString(Item.ancestors[i].Value) + '" />');

  for I := 0 to Item.Methods.count-1 do
    WriteRoutine(Item.Methods.PasItemAt[i] as TPasRoutine);

  for I := 0 to Item.Constants.count-1 do
    WriteConstant(Item.Constants.PasItemAt[i]);

  for I := 0 to Item.Fields.count-1 do
    WriteVariable(Item.fields.PasItemAt[i]);

  for I := 0 to Item.Properties.count-1 do
    WriteProperty(Item.Properties.PasItemAt[i] as TPasProperty);

  for I := 0 to Item.Types.count-1 do
    WriteType(Item.Types.PasItemAt[i]);

  for I := 0 to Item.Cios.count-1 do
    WriteStructure(Item.Cios.PasItemAt[i] as TPasCio);

  UnIndent;
  WriteDirectLine(space+'</structure>');
end;

procedure TSimpleXMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
var
  i: Integer;
begin
  U.OutputFileName:=U.OutputFileName+'.xml';
  if not Assigned(U) then begin
    DoMessage(1, pmtError, 'TSimpleXMLDocGenerator.WriteUnit: ' +
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

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteDirectLine('<unit' + CommonAttributes(U) + '>');
  space:='  ';
  if u.HasDescription then
    WriteDirectLine(space + ItemDescription(u));

  // used units
  if WriteUsesClause and not IsEmpty(U.UsesUnits) then
    for I := 0 to u.UsesUnits.count-1 do
      WriteDirectLine(space +
        '<uses name="' + ConvertString(u.UsesUnits[i]) + '"/>');

  // global functions
  for I := 0 to u.FuncsProcs.count-1 do
    WriteRoutine(u.FuncsProcs.PasItemAt[i] as TPasRoutine);

  // global constants
  for I := 0 to u.Constants.count-1 do
    WriteConstant(u.Constants.PasItemAt[i]);

  // global vars
  for I := 0 to u.Variables.count-1 do
    WriteVariable(u.Variables.PasItemAt[i]);

  // global types
  for I := 0 to u.Types.count-1 do
    WriteType(u.types.PasItemAt[i]);

  // global classes
  for I := 0 to u.CIOs.count-1 do
    WriteStructure(u.CIOs.PasItemAt[i] as TPasCIO);

  WriteDirectLine('</unit>');
end;

procedure TSimpleXMLDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
begin
  { TODO }
end;

function TSimpleXMLDocGenerator.FormatSection(HL: integer; const Anchor: string;
  const Caption: string): string;
begin
  Result := '';
  { TODO }
end;

function TSimpleXMLDocGenerator.FormatAnchor(const Anchor: string): string;
begin
  { TODO: untested, as this is used only by introduction-conclusion stuff
    and WriteExternalCore is not impl yet. }
  Result := Format('<anchor target="%s" />', [Anchor]);
end;

function TSimpleXMLDocGenerator.FormatTable(Table: TTableData): string;
const
  RowElement: array [boolean] of string = ('row', 'rowhead');
var
  RowNum, ColNum: Integer;
  Row: TRowData;
begin
  Result := LineEnding + LineEnding + '<table>' + LineEnding;

  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum];
    Result := Result + '  <' + RowElement[Row.Head] + '>' + LineEnding;
    for ColNum := 0 to Row.Cells.Count - 1 do
      Result := Result +
        Format('    <cell>%s</cell>', [Row.Cells[ColNum]]) + LineEnding;
    Result := Result + '  </' + RowElement[Row.Head] + '>' + LineEnding;
  end;

  Result := Result + '</table>' + LineEnding + LineEnding;
end;

function TSimpleXMLDocGenerator.FormatList(ListData: TListData): string;
const
  ListTag: array[TListType]of string =
  ( 'unorderedlist', 'orderedlist', 'definitionlist' );
var
  ListItem: TListItemData;
begin
  Result := LineEnding + LineEnding +
    Format('<%s>', [ListTag[ListData.ListType]]) + LineEnding;

  for ListItem in ListData do
  begin
    Result := Result + '<item>';
    if ListData.ListType = ltDefinition then
      Result := Result + '<label>' + ListItem.ItemLabel + '</label>';
    Result := Result + ListItem.Text + '</item>' + LineEnding;
  end;

  Result := Result + Format('</%s>', [ListTag[ListData.ListType]]) +
    LineEnding + LineEnding;
end;

function TSimpleXMLDocGenerator.FormatBold(const Text: string): string;
begin
  Result := '<bold>' + Text + '</bold>';
end;

function TSimpleXMLDocGenerator.FormatItalic(const Text: string): string;
begin
  Result := '<italic>' + Text + '</italic>';
end;

end.
