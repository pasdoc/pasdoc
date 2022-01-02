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

uses PasDoc_Utils, PasDoc_Gen, PasDoc_Items, PasDoc_Types, PasDoc_Languages;

type
  { PHP output generator. }
  TPHPDocGenerator = class(TDocGenerator)
  private
    { Output a PHP line mapping given Identifier -> to given HtmlFileName, ItemType. }
    procedure WriteMap(const Identifier, HtmlFileName, ItemType: String);
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
  PasDoc_ObjectVector, SysUtils;

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

  if not CreateStream(OutputFileName) then Exit;
  WriteDirectLine('global $pasdoc;');
  WriteUnits(1);
  CloseStream;
end;

procedure TPHPDocGenerator.WriteMap(const Identifier, HtmlFileName, ItemType: String);
begin
  WriteDirectLine(Format('$pasdoc[''%s''] = array(''html_filename'' => ''%s'', ''type'' => ''%s'');', [
    ConvertString(Identifier),
    ConvertString(HtmlFileName),
    ConvertString(ItemType)
  ]));
end;

procedure TPHPDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
var
  I: Integer;
begin
  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteMap(U.Name, U.Name + '.html', 'unit');
  // TODO
end;

end.
