{ @abstract(Helper unit for integrating tipue [http://www.tipue.com/]
  with pasdoc HTML output.) }

unit PasDoc_Tipue;

interface

uses PasDoc_Utils, PasDoc_Items;

{ Put this in <head> of page with search button. }
function TipueSearchButtonHead: string;

{ Put this in content of some page ---
  this will place a form with search button.
  You will need to use Format to insert the localized word for "Search", e.g.:
  Format(TipueSearchButton, ['Search'])
  for English.}
const
  TipueSearchButton =
    '<form name="tip_Form" onsubmit="search_form(tip_Form);return false">' + LineEnding +
    '<input type="text" id="search_text" name="d">' + LineEnding +
    '<input type="submit" id="search_submit_button" value="%s">' + LineEnding +
    '</form>' + LineEnding;


{ Adds some additional files to html documentation, needed for tipue engine.

  OutputPath is our output path, where html output must be placed.
  Must end with PathDelim.

  Units must be non-nil. It will be used to generate index data for tipue. }
procedure TipueAddFiles(Units: TPasUnits;
  const Introduction, Conclusion: TExternalItem;
  const MetaContentType: string;
  const OutputPath: string);

implementation

uses Classes, SysUtils;

function TipueSearchButtonHead: string;
begin
  Result := '<script language="JavaScript1.3" ' +
    'type="text/javascript" src="tip_form.js"></script>';
end;

procedure TipueAddFiles(Units: TPasUnits;
  const Introduction, Conclusion: TExternalItem;
  const MetaContentType: string;
  const OutputPath: string);

  procedure WriteTipueIndexData(const FileName: string);
  var 
    OutFile: TextFile;
    IndexDataNum: Cardinal;
    
    { Write one line of index data.
      Title, URL, ShortDescription, LongDescription 
      are 1st four parameters of data, see tipue docs. }
    procedure WriteIndexData(
      const Title, URL, ShortDescription, LongDescription: string);
    begin
      Writeln(OutFile, 's[', IndexDataNum, '] = "', Title, '^', URL, '^', 
        ShortDescription, '^', LongDescription, '^0"');
      Inc(IndexDataNum);
    end;
    
    procedure WriteItemIndexData(Item: TBaseItem);
    
      function EscapeIndexEntry(const S: string): string;
      const
        { We want to avoid introducing special chars in JavaScript string,
          so \ and " and newline markers must be escaped.
          ^ must be escaped because it's a field-marker for tipue index data
          entry. }
        ReplacementArray: array[0..4] of TCharReplacement = (
          (cChar: #10; sSpec: ' '),
          (cChar: #13; sSpec: ' '),
          (cChar: '"'; sSpec: '\"'),
          (cChar: '\'; sSpec: '\\'),
          (cChar: '^'; sSpec: '&circ;')
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
      Writeln(OutFile, 'var s = new Array()');
      Writeln(OutFile);
      
      IndexDataNum := 0;
      
      if Introduction <> nil then
      begin
        WriteItemIndexData(Introduction);
      end;
      if Conclusion <> nil then
      begin
        WriteItemIndexData(Conclusion);
      end;

      WriteUnitsIndexData(Units);
    finally CloseFile(OutFile) end;
  end;

const
  TipFormScript = {$I tip_form.js.inc};
  TipSearchScript = {$I tip_search.js.inc};
  TipResultsPage = {$I _tipue_results.html.inc};
  TipLogoImage : {$I tipue_b1.png.inc};
begin
  StringToFile(OutputPath + 'tip_search.js', TipSearchScript);
  StringToFile(OutputPath + 'tip_form.js', TipFormScript);
  StringToFile(OutputPath + '_tipue_results.html', 
    StringReplace(TipResultsPage, 
      '###-PASDOC-INSERT-HEAD-###', MetaContentType, []));
  DataToFile(OutputPath + 'tipue_b1.png', TipLogoImage);
  WriteTipueIndexData(OutputPath + 'tip_data.js');
end;

end.