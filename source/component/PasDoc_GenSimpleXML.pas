unit PasDoc_GenSimpleXML;

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
  private
    space:string;

    { Returns XML <description> element with Item's description.
      Returns '' if Item doesn't have any description. }
    function ItemDescription(Item: TPasItem): string;

    procedure writefunction(const item:TPasItem);
    procedure writeconstant(const item:TPasItem);
    procedure writevariable(const item:TPasItem);
    procedure writetypes(const item:TPasItem);
    procedure writeclass(const item:TPasCIO);
    procedure writeproperty(const item:TPasItem);
  public
    constructor Create(AOwner: TComponent); override;
    procedure WriteDocumentation; override;
    function GetFileExtension: string; override;
  end;

implementation

uses
  PasDoc_ObjectVector, SysUtils;

constructor TSimpleXMLDocGenerator.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
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
    Result := '<description>' + Item.GetDescription + '</description>' else
    Result := '';
end;

procedure TSimpleXMLDocGenerator.writefunction(const item:TPasItem);

  function writeWhat(w:TMethodType):string;
  begin
    result:='';
    case w of
      METHOD_CONSTRUCTOR:result:='constructor';
      METHOD_DESTRUCTOR:result:='destructor';
      METHOD_FUNCTION:result:='function';
      METHOD_PROCEDURE:result:='procedure';
      METHOD_OPERATOR:result:='operator';
    end;
  end;

{var
  i:cardinal;}
begin
  if item is TPasMethod then
  begin
    WriteDirectLine(space+'<function name="'+item.name+'" type="'+writeWhat(TPasMethod(item).What)+'" declaration="'+TPasMethod(item).FullDeclaration+'" result="'+TPasMethod(item).returns+'"/>');
//    if TPasMethod(item).params.count>0 then
//      for i:=0 to TPasMethod(item).params.count-1 do
//        WriteDirectLine('<param name="'+TPasMethod(item).params[i].name+'" value="'+TPasMethod(item).params[i].value+'"');
//    writeln(TPasMethod(item).FullDeclaration);
//    WriteDirectLine('</function>');
  end;
end;

procedure TSimpleXMLDocGenerator.writeproperty(const item:TPasItem);
begin
  WriteDirectLine(space+'<property name="'+item.name+'" indexdecl="'+TPasProperty(item).indexDecl+
  '" type="'+TPasProperty(item).Proptype+'" reader="'+TPasProperty(item).reader+
  '" writer="'+TPasProperty(item).writer+'" default="'+booltostr(TPasProperty(item).default)+
  '" defaultid="'+TPasProperty(item).defaultid+'" nodefault="'+booltostr(TPasProperty(item).nodefault)+
  '" storedid="'+TPasProperty(item).storedid+'"/>');
end;

procedure TSimpleXMLDocGenerator.writeconstant(const item:TPasItem);
begin
  WriteDirectLine(space+'<constant name="'+item.FullDeclaration+'" />');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</constant>');
end;

procedure TSimpleXMLDocGenerator.writevariable(const item:TPasItem);
begin
  WriteDirectLine(space+'<variable name="'+item.FullDeclaration+'" />');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</variable>');
end;

procedure TSimpleXMLDocGenerator.writetypes(const item:TPasItem);
begin
  WriteDirectLine(space+'<type name="'+item.FullDeclaration+'" />');
  if item.HasDescription then
    WriteDirectLine(space + '  ' + ItemDescription(Item));
  WriteDirectLine(space+'</type>');
end;

procedure TSimpleXMLDocGenerator.writeclass(const item:TPasCIO);

function writetype(t:TCIOType):string;
begin
  result:='unknown';
  case t of
    CIO_CLASS:result:='class';
    CIO_SPINTERFACE:result:='dispinterface';
    CIO_INTERFACE:result:='interface';
    CIO_OBJECT:result:='object';
    CIO_RECORD:result:='record';
    CIO_PACKEDRECORD:result:='packed record';
  end;
end;

var
  i:cardinal;
begin
  WriteDirectLine(space+'<structure name="'+item.name+'" type="'+writetype(item.MyType)+'">');
  space:=space+'  ';

  if item.HasDescription then
    WriteDirectLine(space + ItemDescription(Item));

  if item.ancestors.count>0 then
    for i:=0 to item.ancestors.count-1 do
      WriteDirectLine(space+'<ancestor name="'+item.ancestors[i]+'"/>');

  if item.Methods.count>0 then
    for i:=0 to item.Methods.count-1 do
      writefunction(item.Methods.PasItemAt[i]);

  if item.Fields.count>0 then
    for i:=0 to item.Fields.count-1 do
      writevariable(item.fields.PasItemAt[i]);

  if item.Properties.count>0 then
    for i:=0 to item.Properties.count-1 do
      writeproperty(item.Properties.PasItemAt[i]);
  space:=copy(space,0,length(space)-2);
  WriteDirectLine(space+'</structure>');
end;

procedure TSimpleXMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
var
  i:cardinal;
begin
  U.OutputFileName:=U.OutputFileName+'.xml';
  if not Assigned(U) then begin
    DoMessage(1, mtError, 'TGenericXMLDocGenerator.WriteUnit: ' +
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
      DoMessage(1, mtError, 'Could not create XML unit doc file for unit %s.', [U.Name]);
      Exit;
    end;
  end;

  DoMessage(2, mtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteDirectLine('<unit name="'+U.SourceFileName+'">');
  space:='  ';
  if u.HasDescription then
    WriteDirectLine(space + ItemDescription(u));
  //global uses
  if u.UsesUnits.count>0 then
    for i:=0 to u.UsesUnits.count-1 do
      WriteDirectLine(space+'<uses name="'+u.UsesUnits[i]+'"/>');
  //global functions
  if u.FuncsProcs.count>0 then
    for i:=0 to u.FuncsProcs.count-1 do
      writefunction(u.FuncsProcs.PasItemAt[i]);
  //global constants
  if u.Constants.count>0 then
    for i:=0 to u.Constants.count-1 do
      writeconstant(u.Constants.PasItemAt[i]);
  //global vars
  if u.Variables.count>0 then
    for i:=0 to u.Variables.count-1 do
      writevariable(u.Variables.PasItemAt[i]);
  //global types
  if u.Types.count>0 then
    for i:=0 to u.Types.count-1 do
      writetypes(u.types.PasItemAt[i]);
  //global classes
  if u.CIOs.count>0 then
    for i:=0 to u.CIOs.count-1 do
      writeclass(TPasCIO(u.CIOs.PasItemAt[i]));
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
    Result := Result + '<item';
    if ListData.ListType = ltDefinition then
      Result := Result + ' label="' + ListItem.ItemLabel + '"';
    Result := Result + '>' + ListItem.Text + '</item>' + LineEnding;
  end;

  Result := Result + Format('</%s>', [ListTag[ListData.ListType]]) +
    LineEnding + LineEnding;
end;

end.