{
  Copyright 2021-2022 PasDoc developers.

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

{ PHP output generator. }
unit PasDoc_GenPHP;

{$I pasdoc_defines.inc}

interface

uses PasDoc_Utils, PasDoc_Gen, PasDoc_Items, PasDoc_Types, PasDoc_Languages,
  PasDoc_StringVector;

type
  { PHP output generator. }
  TPHPDocGenerator = class(TDocGenerator)
  private
    AllIdentifiers: TStringVector;

    { Output a PHP line mapping given Identifier -> to given HtmlFileName, ItemType. }
    procedure WriteMap(const Identifier, HtmlFileName, ItemType: String);

    { Calculate HTML filename of given Item, following HTML generator logic. }
    function ItemFileName(const Item: TPasItem): String;

    procedure WriteRoutine(const Namespace: String; const Item: TPasRoutine);
    procedure WriteConstant(const Namespace: String; const Item: TPasItem);
    procedure WriteVariable(const Namespace: String; const Item: TPasItem);
    procedure WriteType(const Namespace: String; const Item: TPasItem);
    procedure WriteStructure(Namespace: String; const Item: TPasCIO);
    procedure WriteProperty(const Namespace: String; const Item: TPasProperty);
  protected
    { Overrides of ancestor abstract methods, not really used by PHP generation.
      As we output only a simple map (name->html_filename) for PHP now,
      we don't really use most of these methods.
      But we override them, as they are absract in ancestor. }
    function CodeString(const s: string): string; override;
    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;
    function FormatSection(HL: integer; const Anchor: string;
      const Caption: string): string; override;
    function FormatAnchor(const Anchor: string): string; override;
    function FormatList(ListData: TListData): string; override;
    function FormatTable(Table: TTableData): string; override;

    { Overrides actually used. }
    function ConvertString(const s: string): string; override;
    function ConvertChar(c: char): string; override;
    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
  public
    procedure WriteDocumentation; override;
    function GetFileExtension: String; override;
  end;

implementation

uses
  PasDoc_ObjectVector, SysUtils, PasDoc_GenHtml;

{ Unused overrides ----------------------------------------------------------- }

function TPHPDocGenerator.CodeString(const s: string): string;
begin
  Result := S;
end;

function TPHPDocGenerator.ConvertString(const S: String): String;
const
  ReplacementArray: array[0..0] of TCharReplacement = (
    (cChar: '\'; sSpec: '\\')
  );
begin
  Result := StringReplaceChars(S, ReplacementArray);
end;

function TPHPDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;

procedure TPHPDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
begin
  // do nothing
end;

function TPHPDocGenerator.FormatSection(HL: integer; const Anchor: string;
  const Caption: string): string;
begin
  Result := Caption;
end;

function TPHPDocGenerator.FormatAnchor(const Anchor: string): string;
begin
  Result := Anchor;
end;

function TPHPDocGenerator.FormatTable(Table: TTableData): string;
begin
  Result := '';
end;

function TPHPDocGenerator.FormatList(ListData: TListData): string;
begin
  Result := '';
end;

{ Used overrides ------------------------------------------------------------- }

function TPHPDocGenerator.GetFileExtension: String;
begin
  Result := '.php';
end;

procedure TPHPDocGenerator.WriteDocumentation;
var
  OutputFileName: string;
begin
  inherited;

  if ProjectName <> '' then
    OutputFileName := ProjectName + GetFileExtension
  else
    OutputFileName := 'docs' + GetFileExtension;

  AllIdentifiers := TStringVector.Create;

  if not CreateStream(OutputFileName) then Exit;
  WriteDirectLine('<?php');
  WriteDirectLine('global $pasdoc;');
  WriteDirectLine('$pasdoc = array(');
  WriteUnits(1);
  WriteDirectLine(');');
  CloseStream;

  FreeAndNil(AllIdentifiers);
end;

procedure TPHPDocGenerator.WriteMap(const Identifier, HtmlFileName, ItemType: String);
begin
  { Track already output identifiers. Comment all except the first overloaded identifier.
    This allows PHP array to contain only unique keys.

    The practical advantage is that link like "Foo" using PHP map will lead
    to the *first* "Foo" overload, not last (as it would be the case if PHP map
    contained duplicate keys, then the later key overrides previous).
    The this is better, because the first overload likely contains better docs,
    at least this is the case for CGE.
  }

  if AllIdentifiers.IndexOf(Identifier) <> -1 then
    WriteDirect('  // Overloaded identifier: ')
  else
    AllIdentifiers.Add(Identifier);

  WriteDirectLine(Format('  ''%s'' => array(''html_filename'' => ''%s'', ''type'' => ''%s''),', [
    ConvertString(Identifier),
    ConvertString(HtmlFileName),
    ConvertString(ItemType)
  ]));
end;

function TPHPDocGenerator.ItemFileName(const Item: TPasItem): String;
var
  CurrentItem: TPasItem;
begin
  if Item is TPasCIO then
    Result := Item.Name + '.html'
  else
    Result := 'html#' + SignatureToHtmlId(Item.Signature);

  { now prefix Result with
    - as many as necessary "OuterClassName."
    - "UnitName." }
  CurrentItem := Item;
  while CurrentItem.MyObject <> nil do
  begin
    Result := CurrentItem.MyObject.Name + '.' + Result;
    CurrentItem := CurrentItem.MyObject;
  end;
  Result := CurrentItem.MyUnit.Name + '.' + Result;
end;

procedure TPHPDocGenerator.WriteRoutine(const Namespace: String; const Item: TPasRoutine);
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), RoutineTypeToString(Item.What));
end;

procedure TPHPDocGenerator.WriteConstant(const Namespace: String; const Item: TPasItem);
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), 'constant');
end;

procedure TPHPDocGenerator.WriteVariable(const Namespace: String; const Item: TPasItem);
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), 'variable');
end;

procedure TPHPDocGenerator.WriteType(const Namespace: String; const Item: TPasItem);
var
  I: Integer;
  EnumMember: TPasItem;
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), 'type');

  if Item is TPasEnum then
    for I := 0 to TPasEnum(Item).Members.Count - 1 do
    begin
      EnumMember := TPasEnum(Item).Members.PasItemAt[i];
      WriteMap(Namespace + EnumMember.Name, ItemFileName(EnumMember), 'enum constant');
    end;
end;

procedure TPHPDocGenerator.WriteProperty(const Namespace: String; const Item: TPasProperty);
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), 'property');
end;

procedure TPHPDocGenerator.WriteStructure(Namespace: String; const Item: TPasCIO);
var
  I: Integer;
begin
  WriteMap(Namespace + Item.Name, ItemFileName(Item), CioTypeToString(Item.MyType));

  Namespace := Namespace + Item.Name + '.';

  for I := 0 to item.Methods.count-1 do
    WriteRoutine(Namespace, item.Methods.PasItemAt[i] as TPasRoutine);

  for I := 0 to item.Fields.count-1 do
    WriteVariable(Namespace, item.fields.PasItemAt[i]);

  for I := 0 to item.Properties.count-1 do
    WriteProperty(Namespace, item.Properties.PasItemAt[i] as TPasProperty);

  for I := 0 to item.Types.count-1 do
    WriteType(Namespace, item.Types.PasItemAt[i]);

  for I := 0 to item.Cios.count-1 do
    WriteStructure(Namespace, item.Cios.PasItemAt[i] as TPasCio);
end;

procedure TPHPDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
var
  I: Integer;
begin
  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteMap(U.Name, U.Name + '.html', 'unit');

  // global functions
  for I := 0 to u.FuncsProcs.count-1 do
    WriteRoutine('', u.FuncsProcs.PasItemAt[i] as TPasRoutine);

  // global constants
  for I := 0 to u.Constants.count-1 do
    WriteConstant('', u.Constants.PasItemAt[i]);

  // global vars
  for I := 0 to u.Variables.count-1 do
    WriteVariable('', u.Variables.PasItemAt[i]);

  // global types
  for I := 0 to u.Types.count-1 do
    WriteType('', u.types.PasItemAt[i]);

  // global classes
  for I := 0 to u.CIOs.count-1 do
    WriteStructure('', u.CIOs.PasItemAt[i] as TPasCIO);
end;

end.
