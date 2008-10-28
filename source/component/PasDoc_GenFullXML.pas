unit PasDoc_GenFullXML;

interface

uses
  PasDoc_Utils,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_Types,
  Classes;

type
  TXMLDocGenerator = class(TDocGenerator)
  protected //override inherited abstract
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
  //create link (= output filename?)
    function CreateLink(const Item: TBaseItem): string; override;
  //paragraph ending in processed descriptions
    function  Paragraph: string; override;
  private
  //indentation
    indent: string;
  //open tags
    TagList: array of string;
    TagLevel: integer;

    function ArgString(const Aname, arg: string): string;
  //write tagged string
    procedure WriteTag(const Atag, args, str: string);
    procedure StartTag(const Aname, args: string; fPush, NewLine: boolean);
    procedure OpenTag(const Atag, args: string; NewLine: boolean = True);
    procedure CloseTag;
  //close all open tags - until level?
    procedure CloseTags;

  //write any item.
    procedure WriteItem(item: TDescriptionItem; tid: TTranslationID);

  {$IFDEF unused}
    function  OpenItemTag(item: TDescriptionItem): boolean;
    function  Tagged(const tag, str: string): string;
  {$ELSE}
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDocumentation; override;
    function  GetFileExtension: string; override;
  end;

implementation

uses
  PasDoc_ObjectVector, SysUtils, Math;

constructor TXMLDocGenerator.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
{$IFDEF old}
  tags := TStringList.Create;
{$ELSE}
{$ENDIF}
end;

destructor TXMLDocGenerator.Destroy;
begin
{$IFDEF old}
  FreeAndNil(tags);
{$ELSE}
{$ENDIF}
  inherited;
end;

function TXMLDocGenerator.GetFileExtension:string;
begin
  Result := '.xml';
end;

procedure TXMLDocGenerator.WriteDocumentation;
begin
  StartSpellChecking('sgml');
{$IFDEF old}
  inherited;
  WriteUnits(1);
{$ELSE}
  CreateStream(self.ProjectName + GetFileExtension, True);
  MasterFile := FCurrentFileName;
  OpenTag('project', ArgString('name', ProjectName));
    WriteIntroduction;
    WriteUnits(1);
    WriteConclusion;
  CloseTags;
{$ENDIF}
  CloseStream;
  EndSpellChecking;
end;

function TXMLDocGenerator.CodeString(const s: string): string;
begin
  Result := '<code>' + S + '</code>';
end;

function TXMLDocGenerator.ConvertString(const S: String): String;
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

function TXMLDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;

procedure TXMLDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
begin
(* Write introduction/conclusion.
  How? (add filename as reference?)
*)
  { TODO }
end;

function TXMLDocGenerator.FormatSection(HL: integer; const Anchor: string;
  const Caption: string): string;
begin
  Result := '';
  { TODO }
end;

function TXMLDocGenerator.FormatAnchor(const Anchor: string): string;
begin
  { TODO: untested, as this is used only by introduction-conclusion stuff
    and WriteExternalCore is not impl yet. }
  Result := Format('<anchor target="%s" />', [Anchor]);
end;

function TXMLDocGenerator.FormatTable(Table: TTableData): string;
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
        //Format('    <cell>%s</cell>', [Row.Cells[ColNum]]) + LineEnding;
        '    <cell>' + Row.Cells[ColNum] + '</cell>' + LineEnding;
    Result := Result + '  </' + RowElement[Row.Head] + '>' + LineEnding;
  end;

  Result := Result + '</table>' + LineEnding + LineEnding;
end;

function TXMLDocGenerator.FormatList(ListData: TListData): string;
const
  ListTag: array[TListType]of string =
  ( 'unorderedlist', 'orderedlist', 'definitionlist' );
var
  i: Integer;
  ListItem: TListItemData;
begin
  Result := LineEnding + LineEnding +
    //Format('<%s>', [ListTag[ListData.ListType]]) + LineEnding;
    '<' + ListTag[ListData.ListType] + '>' + LineEnding;

  for i := 0 to ListData.Count - 1 do
  begin
    ListItem := ListData.Items[i] as TListItemData;
    Result := Result + '<item';
    if ListData.ListType = ltDefinition then
      Result := Result + ' label="' + ListItem.ItemLabel + '"';
    Result := Result + '>' + ListItem.Text + '</item>' + LineEnding;
  end;

  Result := Result +
    //Format('</%s>', [ListTag[ListData.ListType]]) +
    '</' + ListTag[ListData.ListType] + '>' +
    LineEnding + LineEnding;
end;

function TXMLDocGenerator.CreateLink(const Item: TBaseItem): string;
begin
(* AFAIK XML has no linking/reference specification.
*)
{ TODO : CreateLink for XML }
  Result := Item.Name;
end;

function TXMLDocGenerator.Paragraph: string;
begin
  Result := LineEnding; // + LineEnding;
end;

//-------------------- new tags ----------------------

procedure TXMLDocGenerator.CloseTag;
begin
  SetLength(indent, Length(indent) - 2);
  dec(TagLevel);
  WriteDirect(indent + '</' + TagList[TagLevel] + '>', True);
end;

procedure TXMLDocGenerator.CloseTags;
begin
  while TagLevel > 0 do
    CloseTag;
end;

procedure TXMLDocGenerator.StartTag(const Aname, args: string;
  fPush, NewLine: boolean);
var
  t: string;
begin
(* write tag and args.
  Close depending on fPush.
*)
//handle tags like "See also"
  t := StringReplace(Aname, ' ', '_', [rfReplaceAll]);
  WriteDirect(indent + '<' + t);
  if args <> '' then
    WriteDirect(args);
  if fPush then begin
  //keep tag open
    if TagLevel >= Length(TagList) then
      SetLength(TagList, TagLevel + 8);
    TagList[TagLevel] := t;
    inc(TagLevel);
    indent := indent + '  ';
    WriteDirect('>', NewLine);
  end else //close tag
    WriteDirect('/>', True);
end;

procedure TXMLDocGenerator.OpenTag(const Atag, args: string; NewLine: boolean);
begin
(* write
  <tag args> EOL?
  Args must have been converted!
*)
  StartTag(Atag, args, True, NewLine);
end;

{$IFDEF unused}
function TXMLDocGenerator.Tagged(const tag, str: string): string;
begin
  if str = '' then
    Result := ''
  else
    Result := '<' + tag + '>' + str + '</' + tag + '>';
end;

function TXMLDocGenerator.OpenItemTag(item: TDescriptionItem): boolean;
var
  n, s: string;
begin
(* open tag:
  <id name=... value=...>
  Result: False if all done.
*)
  Result := (item.Count > 0) or (item is TPasItem);
  n := item.Name;
  s := ArgString('value', item.Value);
  case item.ID of
  trNoTrans: //use name as tag
    ;
  trDescription: //name=abstract, value=detailed
    begin
      OpenTag('description', ArgString('abstract', item.Name));
      WriteTag('detailed', '', item.Value);
      Result := True;
      exit;
    end;
  trDeclaration:
    WriteTag('declaration', '', item.Name);
  trAuthors: //list of strings
    n := 'authors'; //debug
  trSeeAlso:  //single string!
    n := 'SeeAlso';
  trDeprecated, trLibrary, trPlatformSpecific:
  //ignore
    exit;
  trUses: //debug - should be list!
    n := 'uses';
  else //case
    n := StringReplace(Translation(item.ID, lgEnglish), ' ', '_', [rfReplaceAll]);
    if item.Name <> '' then
      s := ArgString('name', item.Name) + s;
    if item is TPasItem then
      s := ArgString('kind', TokenName(item.PasItem.Kind)) + s;
  end;
  if Result then
    OpenTag(n, s)
  else
    WriteTag(n, s, '');
end;
{$ELSE}
{$ENDIF}

procedure TXMLDocGenerator.WriteTag(const Atag, args, str: string);
begin
(* Write tag without child nodes:
    <tag args>str</tag>
or? <tag args />
*)
  //if (args = '') and (str = '') then exit; //strip empty tags?
  if str = '' then begin
    StartTag(Atag, args, False, True);
  end else begin
    StartTag(Atag, args, True, False);
  //if str <> '' then
    WriteConverted(str);
    CloseTag;
  end;
end;

function TXMLDocGenerator.ArgString(const Aname, arg: string): string;
var
  n: string;
begin
(* return ' name= "'+arg+"', empty if arg is empty
*)
  if arg = '' then
    Result := ''
  else begin
    n := StringReplace(Aname, ' ', '_', [rfReplaceAll]);
    Result := ' ' + n + '="' + ConvertString(Trim(arg)) + '"';
  end;
end;

procedure TXMLDocGenerator.WriteItem(item: TDescriptionItem; tid: TTranslationID);
var
  i: integer;
  d: TDescriptionItem;
  hasItems: boolean;

  procedure WriteAttributes(p: TPasItem);
  var
    i: TDirectives;
  begin
    if p = nil then
      exit; //not a PasItem
    if p.Attributes = [] then
      exit; //no attributes at all
    OpenTag('directives', '');
    for i := succ(SD_INVALIDSTANDARDDIRECTIVE) to high(i) do begin
      if p.HasAttribute[i] then begin
        WriteTag(LowerCase(DirectiveNames[i]), '', '');
      end;
    end;
    CloseTag;
  end;

begin
(* Write item.
  Eventually depending on it's kind:
  TDescriptionItem
  TPasItem
  TPasScope(?)
*)
  if (item.ID <> trNoTrans) then
    tid := item.ID;
  hasItems := (item.Count > 0) or (item is TPasItem);
  case tid of
  trDeprecated, trLibrarySpecific, trPlatformSpecific:
    exit; //show as general attributes
  //trAuthor: name as text?
  //trSeeAlso: value as text?
  //trDeclaration: //as text? value of item?
  trDescription: //todo: retain paragraphs
    begin
    {$IFDEF paragraphs}
      OpenTag('Description', ArgString('abstract', item.Name), True);
      for i := 0 to item.Count - 1 do begin
        d := item.ItemAt(i);
        if d.ID = trDescription then
          WriteTag('description', '', item.Strings[i])
        else
          WriteItem(d, trDescription);
      end; //else top level list, no name/value expected
      CloseTag;
    {$ELSE}
      WriteTag('Description', ArgString('abstract', item.Name), ConvertString(item.Value));
    {$ENDIF}
      exit;
    end;
  else //case
  end;
//start item
  StartTag(Translation(tid, lgEnglish),
    ArgString('name', item.Name) + ArgString('value', item.Value),
    hasItems, hasItems);
  if not hasItems then
    exit; //all done
//write PasItem attributes etc.?
  WriteAttributes(item.PasItem);
//write descriptions
  for i := 0 to item.Count - 1 do begin
    d := item.ItemAt(i);
    WriteItem(d, tid);
  end;
//done this item tag
  CloseTag;
end;

procedure TXMLDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
begin
  WriteItem(U, trUnit);
end;

end.
