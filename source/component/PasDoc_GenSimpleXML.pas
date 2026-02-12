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

{ @abstract(SimpleXML output generator.) }
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

    { XML attributes derived from TPasItem.SourceAbsoluteFileName and SourceLine. }
    function SourcePositionAttributes(const Item: TPasItem): string;

  public
    procedure WriteDocumentation; override;
    function GetFileExtension: string; override;
  end;

implementation

uses
  PasDoc_ObjectVector, SysUtils;

function TSimpleXMLDocGenerator.SourcePositionAttributes(
  const Item: TPasItem): string;
var
  ItemName, ItemFilenameInRoot, ItemUrl: string;
begin
  if HasSourcePosition(Item, ItemName, ItemFilenameInRoot, ItemUrl) then
    Result :=
      ' source-absolute-file-name="' + ConvertString(Item.SourceAbsoluteFileName) + '"' +
      ' source-line="' + IntToStr(Item.SourceLine) + '"' +
      ' source-relative-file-name="' + ConvertString(ItemFilenameInRoot) + '"' +
      ' source-url="' + ConvertString(ItemUrl) + '"'
  else
    Result := '';
end;

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

procedure TSimpleXMLDocGenerator.WriteRoutine(const Item: TPasRoutine);
var
  I: Integer;
begin
  WriteDirectLine(space +
      '<routine name="' + ConvertString(item.name) +
            '" type="' + ConvertString(RoutineTypeToString(item.What)) +
     '" declaration="' + ConvertString(item.FullDeclaration) +
     '" visibility="' + VisToStr(item.visibility) + '"' +
     SourcePositionAttributes(item) + '>');
    for I := 0 to item.params.count - 1 do
      WriteDirectLine(space +
        '  <param name="' + ConvertString(item.params[i].name) + '">' +
          item.params[i].value +'</param>');
    if item.returns <> '' then
      WriteDirectLine(space +
        '  <result>' + item.returns + '</result>');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space + '</routine>');
end;

procedure TSimpleXMLDocGenerator.WriteProperty(const Item: TPasProperty);
begin
  WriteDirectLine(space +
    '<property name="' + ConvertString(item.name) +
       '" indexdecl="' + ConvertString(item.indexDecl) +
            '" type="' + ConvertString(item.Proptype) +
          '" reader="' + ConvertString(item.reader) +
          '" writer="' + ConvertString(item.writer) +
'" default_in_class="' + ConvertString(BoolToStr(item.DefaultInClass, true)) +
   '" default_value="' + ConvertString(item.DefaultValue) +
       '" nodefault="' + ConvertString(BoolToStr(item.NoDefault, true)) +
        '"   stored="' + ConvertString(item.Stored) +
      '" visibility="' + VisToStr(item.visibility) + '"' +
      SourcePositionAttributes(item) + '>');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</property>');
end;

procedure TSimpleXMLDocGenerator.WriteConstant(const Item: TPasItem);
begin
  WriteDirectLine(space +
    '<constant name="' + ConvertString(item.Name) +
     '" declaration="' + ConvertString(item.FullDeclaration) +
      '" visibility="' + VisToStr(item.visibility) + '"' +
      SourcePositionAttributes(item) + '>');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</constant>');
end;

procedure TSimpleXMLDocGenerator.WriteVariable(const Item: TPasItem);
begin
  WriteDirectLine(space +
    '<variable name="' + ConvertString(item.Name) +
     '" declaration="' + ConvertString(item.FullDeclaration) +
      '" visibility="' + VisToStr(item.visibility) + '"' +
      SourcePositionAttributes(item) + '>');
  if item.HasDescription then
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
        '<type name="' + ConvertString(item.Name) +
     '" declaration="' + ConvertString(item.FullDeclaration) +
      '" visibility="' + VisToStr(item.visibility) + '"' +
      SourcePositionAttributes(item) + '>');
  if item.HasDescription then
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
      '<structure name="' + ConvertString(item.name) +
  '" name_with_generic="' + ConvertString(item.NameWithGeneric) +
               '" type="' + ConvertString(CioTypeToString(item.MyType)) +
         '" visibility="' + VisToStr(item.visibility) + '"' +
         SourcePositionAttributes(item) + '>');
  Indent;

  if item.HasDescription then
    WriteDirectLine(space + ItemDescription(Item));

  for I := 0 to item.ancestors.count-1 do
    WriteDirectLine(space +
      '<ancestor name="' + ConvertString(item.ancestors[i].Name) +
       '" declaration="' + ConvertString(item.ancestors[i].Value) + '" />');

  for I := 0 to item.Methods.count-1 do
    WriteRoutine(item.Methods.PasItemAt[i] as TPasRoutine);

  for I := 0 to item.Fields.count-1 do
    WriteVariable(item.fields.PasItemAt[i]);

  for I := 0 to item.Properties.count-1 do
    WriteProperty(item.Properties.PasItemAt[i] as TPasProperty);

  for I := 0 to item.Types.count-1 do
    WriteType(item.Types.PasItemAt[i]);

  for I := 0 to item.Cios.count-1 do
    WriteStructure(item.Cios.PasItemAt[i] as TPasCio);

  UnIndent;
  WriteDirectLine(space+'</structure>');
end;

procedure TSimpleXMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
var
  i: Integer;
begin
  U.OutputFileName:=U.OutputFileName+'.xml';
  if not Assigned(U) then begin
    DoMessage(1, pmtError, 'TGenericXMLDocGenerator.WriteUnit: ' +
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
  WriteDirectLine('<unit name="' + ConvertString(U.SourceFileName) + '">');
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
    Row := Table.Items[RowNum] as TRowData;
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
  i: Integer;
  ListItem: TListItemData;
begin
  Result := LineEnding + LineEnding +
    Format('<%s>', [ListTag[ListData.ListType]]) + LineEnding;

  for i := 0 to ListData.Count - 1 do
  begin
    ListItem := ListData.Items[i] as TListItemData;
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
