{
  Copyright 1998-2014 PasDoc developers.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ @abstract(Helper unit for integrating tipue [http://www.tipue.com/]
  with pasdoc HTML output.) }
unit PasDoc_Tipue;

interface

uses PasDoc_Utils, PasDoc_Items;

{ Put this in <head> of every page with search button. }
function TipueSearchButtonHead: string;

const
  { Put this inside the page contents --- it will make a form with search button.
    You will need to use Format to insert the localized word for "Search", e.g.:
    Format(TipueSearchButton, ['Search'])
    for English.}
  TipueSearchButton =
    '<form class="search-form" action="_tipue_results.html">' +
    '<div class="search-input"><input type="text" name="q" id="tipue_search_input"></div>' +
    '<div class="search-button"><input type="button" id="tipue_search_button" onclick="this.form.submit();"></div>' +
    '</form>' + LineEnding +
    '<div style="clear: both"></div>' + LineEnding +
    LineEnding +
    '<script>' + LineEnding +
    '$(document).ready(function() {' + LineEnding +
    '    $(''#tipue_search_input'').tipuesearch({' + LineEnding +
    '        /* 10 items to display seems standard */' + LineEnding +
    '        ''show'': 10,' + LineEnding +
    '        /* For PasDoc docs, showing urls is not very useful,' + LineEnding +
    '           since the page title already shows the unit and identifier. */' + LineEnding +
    '        ''showURL'': false' + LineEnding +
    '    });' + LineEnding +
    '});' + LineEnding +
    '</script>' + LineEnding;


{ Adds some additional files to html documentation, needed for tipue engine.

  OutputPath is our output path, where html output must be placed.
  Must end with PathDelim.

  Units must be non-nil. It will be used to generate index data for tipue. }
procedure TipueAddFiles(Units: TPasUnits;
  const Introduction, Conclusion: TExternalItem;
  const Head, BodyBegin, BodyEnd: string;
  const OutputPath: string);

implementation

uses Classes, SysUtils;

function TipueSearchButtonHead: string;
begin
  Result :=
    // Note that we use local jquery copy, not from google, to work offline
    '<script type="text/javascript" src="tipuesearch/jquery.min.js"></script>' + LineEnding +
    '<script type="text/javascript" src="tipuesearch/tipuesearch_data.js"></script>' + LineEnding +
    '<link rel="stylesheet" type="text/css" href="tipuesearch/tipuesearch.css">' + LineEnding +
    '<script type="text/javascript" src="tipuesearch/tipuesearch_set.js"></script>' + LineEnding +
    '<script type="text/javascript" src="tipuesearch/tipuesearch.js"></script>' + LineEnding;
end;

procedure TipueAddFiles(Units: TPasUnits;
  const Introduction, Conclusion: TExternalItem;
  const Head, BodyBegin, BodyEnd: string;
  const OutputPath: string);

  procedure WriteTipueIndexData(const FileName: string);
  var
    OutFile: TextFile;
    NeedsLeadingComma: boolean;

    { Write one line of index data.
      See http://www.tipue.com/help/search/data/.
      Note: we try hard to *not* place a comma on the final newline,
      as google suggests that IE may have problems with it.

      For now ShortDescription is not used. It was useful
      with old tipue version, we keep calculating it as it may be useful again
      in the future. }
    procedure WriteIndexData(
      const Title, URL, ShortDescription, LongDescription: string);
    begin
      if NeedsLeadingComma then
        Write(OutFile, ',');
      Writeln(OutFile);
      Write(OutFile, '     {"title": "', Title,
        '", "text": "', LongDescription,
        '", "tags": "', { no tags for now? Or maybe use here ShortDescription? }
        '", "loc": "', URL, '"}');
      NeedsLeadingComma := true;
    end;

    procedure WriteItemIndexData(Item: TBaseItem);

      function EscapeIndexEntry(const S: string): string;
      const
        { We want to avoid introducing special chars in JavaScript string,
          so \ and " and newline markers must be escaped. }
        ReplacementArray: array[0..3] of TCharReplacement = (
          (cChar: #10; sSpec: ' '),
          (cChar: #13; sSpec: ' '),
          (cChar: '"'; sSpec: '\"'),
          (cChar: '\'; sSpec: '\\')
        );
      begin
        Result := StringReplaceChars(S, ReplacementArray);
      end;

    var
      ShortDescription, LongDescription: string;
      EnumMember: TPasItem;
      i: Integer;
    begin
      { calculate ShortDescription }
      if Item is TPasItem then
        ShortDescription :=
          EscapeIndexEntry(TPasItem(Item).AbstractDescription) else
      if Item is TExternalItem then
        ShortDescription :=
          EscapeIndexEntry(TExternalItem(Item).ShortTitle) else
        ShortDescription := '';

      { calculate LongDescription.
        Note that LongDescription will not be shown to user anywhere
        (it will only be searched by tipue), so we don't care how
        things look here. We just glue some properties of Item together. }
      LongDescription := EscapeIndexEntry(Item.DetailedDescription) +
        ' ' + EscapeIndexEntry(Item.Authors.Text);
      if Item is TPasMethod then
        LongDescription := LongDescription +
          ' ' + EscapeIndexEntry(TPasMethod(Item).Params.Text(' ', ' ')) +
          ' ' + EscapeIndexEntry(TPasMethod(Item).Returns) +
          ' ' + EscapeIndexEntry(TPasMethod(Item).Raises.Text(' ', ' '));
      if Item is TPasEnum then
      begin
        for i := 0 to TPasEnum(Item).Members.Count - 1 do
        begin
          EnumMember := TPasEnum(Item).Members.PasItemAt[i];
          LongDescription := LongDescription +
            ' ' + EscapeIndexEntry(EnumMember.Name) +
            ' ' + EscapeIndexEntry(EnumMember.AbstractDescription) +
            ' ' + EscapeIndexEntry(EnumMember.DetailedDescription) +
            ' ' + EscapeIndexEntry(EnumMember.Authors.Text);
        end;
      end;
      if Item is TExternalItem then
      begin
        LongDescription := LongDescription +
          ' ' + EscapeIndexEntry(TExternalItem(Item).Title);
      end;

      WriteIndexData(Item.QualifiedName, Item.FullLink,
        ShortDescription, LongDescription);
    end;

    procedure WriteItemsIndexData(Items: TPasItems);
    var
      i: Integer;
    begin
      for i := 0 to Items.Count - 1 do
        WriteItemIndexData(Items.PasItemAt[i]);
    end;

    procedure WriteCIOsIndexData(CIOs: TPasItems);
    var
      i: Integer;
      CIO: TPasCIO;
    begin
      for i := 0 to CIOs.Count - 1 do
      begin
        CIO := CIOs[i] as TPasCIO;
        WriteItemIndexData(CIO);
        WriteItemsIndexData(CIO.Fields);
        WriteItemsIndexData(CIO.Methods);
        WriteItemsIndexData(CIO.Properties);
        WriteItemsIndexData(CIO.Types);
        if CIO.Cios.Count > 0 then
          WriteCIOsIndexData(CIO.Cios);
      end;
    end;

    procedure WriteUnitsIndexData(Units: TPasUnits);
    var
      i: Integer;
      U: TPasUnit;
    begin
      for i := 0 to Units.Count - 1 do
      begin
        U := Units[i] as TPasUnit;
        WriteItemIndexData(U);
        WriteCIOsIndexData(U.CIOs);
        WriteItemsIndexData(U.Constants);
        WriteItemsIndexData(U.FuncsProcs);
        WriteItemsIndexData(U.Types);
        WriteItemsIndexData(U.Variables);
      end;
    end;

  begin
    Assign(OutFile, FileName);
    Rewrite(OutFile);
    try
      Write(OutFile, 'var tipuesearch = {"pages": [');
      NeedsLeadingComma := false;

      if Introduction <> nil then
        WriteItemIndexData(Introduction);
      if Conclusion <> nil then
        WriteItemIndexData(Conclusion);
      WriteUnitsIndexData(Units);

      Writeln(OutFile, LineEnding + ']};');
    finally CloseFile(OutFile) end;
  end;

const
  TipueSearchCss: {$I tipuesearch.css.inc};
  TipueSearchScript: {$I tipuesearch.js.inc};
  TipueSearchSetScript: {$I tipuesearch_set.js.inc};
  JQueryScript: {$I jquery.min.js.inc};
  TipueSearchImage: {$I search.png.inc};
  TipueLoaderImage: {$I loader.gif.inc};
var
  TipueResultsPage: string;
begin
  CreateDir(OutputPath + 'tipuesearch');
  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'tipuesearch.css', TipueSearchCss);
  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'tipuesearch.js', TipueSearchScript);
  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'tipuesearch_set.js', TipueSearchSetScript);
  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'jquery.min.js', JQueryScript);

  TipueResultsPage := {$I _tipue_results.html.inc};
  TipueResultsPage := StringReplace(TipueResultsPage, '###-PASDOC-HEAD-###', Head, []);
  TipueResultsPage := StringReplace(TipueResultsPage, '###-PASDOC-BODY-BEGIN-###', BodyBegin, []);
  TipueResultsPage := StringReplace(TipueResultsPage, '###-PASDOC-BODY-END-###', BodyEnd, []);
  StringToFile(OutputPath + '_tipue_results.html', TipueResultsPage);

  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'search.png', TipueSearchImage);
  DataToFile(OutputPath + 'tipuesearch' + PathDelim + 'loader.gif', TipueLoaderImage);
  WriteTipueIndexData(OutputPath + 'tipuesearch' + PathDelim + 'tipuesearch_data.js');
end;

end.