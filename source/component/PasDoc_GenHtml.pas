{ @abstract(Provides HTML document generator object.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Alexander Lisnevsky (alisnevsky@yandex.ru))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Hendy Irawan (ceefour@gauldong.net))
  @author(Wim van der Vegt (wvd_vegt@knoware.nl))
  @lastmod(2003-03-29)
  
  Implements an object to generate HTML documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenHtml;

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  PasDoc_Types,
  Classes;

type
  { @abstract(generates HTML documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in HTML (HyperText Markup Language) format.
    This type of output is well suited to be read with a web browser at the
    computer, as a reference manual that does not have to be printed.
    For printed output, use @link(Tex.TTexDocGenerator). }
  THTMLDocGenerator = class(TDocGenerator)
  protected
    FNumericFilenames: boolean;
    FWriteUses: boolean;
    FLinkCount: Integer;
    FFooter: string;
    FHeader: string;
    { Contains Name of a file to read HtmlHelp Contents from.
      If empty, create default contents file. }
    FContentsFile: string;
    { If True, generate Html Help project files. }
    FHtmlHelp: Boolean;
    { True if not to call HCC.exe if creating HtmlHelp output.
      Otherwise, PasDoc will look for HCC.exe in the registry and
      compile the project.  }
    FNoHHC: Boolean;
    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: Byte; Authors: TStringVector);
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string; const
      ItemLink: string);
    { Writes the beginning of the HTML document, including opening HTML element,
      a complete HEAD element and an opening BODY element.
      See @link(WriteEndOfDocument). }
    procedure WriteDocumentHeadline;
    { Writes an empty table cell, '&nbsp;'. }
    procedure WriteEmptyCell;

    { Writes the end of an HTML anchor, '</A>'. }
    procedure WriteEndOfAnchor;
    { See @link(WriteDocumentHeadline). }
    procedure WriteEndOfDocument;
    procedure WriteEndOfLink;
    { Finishes an HTML paragraph element by writing a closing P tag. }
    procedure WriteEndOfParagraph;
    { Finishes an HTML table cell by writing a closing TD tag. }
    procedure WriteEndOfTableCell;
    { Finishes an HTML table by writing a closing TABLE tag. }
    procedure WriteEndOfTable;
    { Finishes an HTML table row by writing a closing TR tag. }
    procedure WriteEndOfTableRow;
    procedure WriteFields(const Order: Byte; const Fields: TPasItems);
    procedure WriteFooter;
    { Writes a Hireachy list - this is more useful than the simple class list }
    procedure WriteHierachy;
    procedure WriteItemDescription(const AItem: TPasItem);
    { Writes the Item's DetailedDescription. If the Item also has Discription
      (extracted from @@abstract), this is written to a separate paragraph
      in front of the DetailedDescription. }
    procedure WriteItemDetailedDescription(const AItem: TPasItem);
    procedure WriteOverviewFiles;
    procedure WriteParagraph(HL: Byte; s: string; t: string);
    procedure WritePropertiesSummary(HL: Byte; p: TPasProperties);
    { Writes an opening A element, including a name attribute given by the
      argument. }
    procedure WriteStartOfAnchor(const Name: string);
    procedure WriteStartOfDocument(Name: string);
    procedure WriteStartOfLink(const Name: string);
    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph;
    procedure WriteStartOfTableCell;
    procedure WriteStartOfTable1Column(t: string);
    procedure WriteStartOfTable2Columns(t1, t2: string);
    procedure WriteStartOfTable3Columns(t1, t2, T3: string);
    procedure WriteStartOfTableRow;
    { Writes the topic files for Html Help Generation }
    procedure WriteHtmlHelpProject;

    { Creates an output stream that lists up all units and short descriptions. }
    procedure WriteUnitOverviewFile;
    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);

    procedure WriteUnit(const HL: Byte; const U: TPasUnit); override;
    procedure WriteUnitUses(const HL:Byte; U: TPasUnit); 
    procedure WriteUnitDescription(HL: Byte; U: TPasUnit); override;
    procedure WriteProperties(HL: Byte; const p: TPasProperties); override;

    procedure WriteSpellChecked(const AString: string);

    procedure WriteWithURLs(s: string);
    { Mark the string as a parameter, e.g. <b>TheString</b> }
    function ParameterString(const ParamType, Param: string): string; override;
    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; override;
    { Returns a link to an anchor within a document. HTML simply concatenates
      the strings with a "#" character between them. }
    function CreateLink(const Item: TPasItem): string; override;
    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: Byte; const CIO: TPasCio); override;
    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: Byte; c: TPasItems); override;
    procedure WriteCIOSummary(HL: Byte; c: TPasItems); override;
    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: Byte; const Created, LastMod: string); override;
    procedure WriteStartOfCode; override;
    procedure WriteItems(HL: Byte; Heading: string; const Anchor: string;
      const i: TPasItems); override;
    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(Level: Byte; const s: string); override;
    { Reads the default HTML Images from the PasDoc executable and writes
      them to the Output Directory. Existing files will not be overwritten. }
    procedure WriteEndOfCode; override;
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument Methods is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the FuncsProcs argument. }
    procedure WriteFuncsProcs(const HL: Byte; const Methods: Boolean; const
      FuncsProcs: TPasMethods); override;

    { output all the necessary images and the CSS }
    procedure WriteBinaryFiles;
  public
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure LoadFooterFromFile(const AFileName: string);
    procedure LoadHeaderFromFile(const AFileName: string);
  published
    property HtmlHelp: boolean read FHtmlHelp write FHtmlHelp;
    property ContentsFile: string read FContentsFile write FContentsFile;
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses;
  end;


{$INCLUDE automated.inc}
{$INCLUDE private.inc}
{$INCLUDE public.inc}
{$INCLUDE published.inc}
{$INCLUDE protected.inc}

const
  { background color of a table header row; a light gray slightly darker
    than the light gray of @link(HTML_HEADER_BACKGROUND_COLOR)  }
  HTML_ROW_BACKGROUND_COLOR: string[6] = 'efefef';
  { background color of a normal table row; a light gray slightly lighter
    than the light gray of @link(HTML_ROW_BACKGROUND_COLOR) }
  HTML_HEADER_BACKGROUND_COLOR: string[6] = 'e0e0e0';

  { HTML table padding inside each cell. }
  HTML_TABLE_CELLPADNG = '4';
  { HTML table spacing between cells. }
  HTML_TABLE_CELLSPACING = '2';

implementation

uses
  SysUtils,
  PasDoc,
  ObjectVector,
  Utils,
  PasDoc_Tokenizer,
  PasDoc_HierarchyTree,
  StreamUtils;

{ HTML things to be customized:
    - standard background color (white)
    - background color for table headings and overview list at the top of each file (light gray)
    - background color for normal table cells (light gray, slightly lighter than the above)
    - standard foreground color (black)
    - unused link color (blue)
    - used link color (purple)
    - link color while being clicked on (red)
    - normal font (Times Roman)
    - heading font (Helvetica)
    - code font (Courier New) }

function THTMLDocGenerator.CodeString(const s: string): string;
begin
  Result := '<CODE>' + s + '</CODE>';
end;

function THTMLDocGenerator.CreateLink(const Item: TPasItem): string;
begin
  Result := '';
  if (not Assigned(Item)) then Exit;
  if NumericFilenames then begin
    Result := Format('%.8d', [FLinkCount]) + GetFileExtension;
    Inc(FLinkCount);
  end else begin
    if Assigned(Item.MyUnit) then begin
      if Assigned(Item.MyObject) then begin
        { it's a method, a field or a property - only those have MyObject initialized }
        Result := Item.MyObject.FullLink + '#' + Item.Name;
      end else begin
        if Item.ClassType = TPasCio then begin
          { it's an object / a class }
          Result := Item.MyUnit.Name + '.' + Item.Name + GetFileExtension;
        end else begin
          { it's a constant, a variable, a type or a function / procedure }
          Result := Item.MyUnit.FullLink + '#' + Item.Name;
        end;
      end;
    end else begin
      { it's a unit - only units don't have a MyUnit pointer }
      Result := Item.Name + GetFileExtension;
    end;
  end;
end;

function THTMLDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result := CodeString('<A href="' + Link + '">' + ItemName + '</A>');
end;

function THTMLDocGenerator.GetFileExtension: string;
begin
  Result := '.html';
end;

procedure THTMLDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  WriteLine('<HR noshade size=1><EM>' +
    FLanguage.Translation[trGeneratedBy] +
    ' <A href="' + PASDOC_HOMEPAGE + '">' +
    PASDOC_NAME_AND_VERSION + '</A> ' + FLanguage.Translation[trOnDateTime] + ' ' +
    FormatDateTime('ddd dd/ mmm yyyy hh:mm:ss', Now) + '</EM>');
end;

procedure THTMLDocGenerator.WriteAuthors(HL: Byte; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  EmailAddress: string;
begin
  if StringVectorIsNilOrEmpty(Authors) then Exit;

  if (Authors.Count = 1) then
    WriteHeading(HL, FLanguage.Translation[trAuthor])
  else
    WriteHeading(HL, FLanguage.Translation[trAuthors]);

  for i := 0 to Authors.Count - 1 do begin
    s := Authors[i];
    WriteStartOfParagraph;

    if ExtractEmailAddress(s, S1, S2, EmailAddress) then begin
      WriteString(S1);
      WriteString('<A href="mailto:' + EmailAddress +
        '">');
      WriteString(EmailAddress);
      WriteString('</A>');
      WriteString(S2);
    end else begin
      WriteString(s);
    end;

    WriteEndOfParagraph;
  end;
end;

procedure THTMLDocGenerator.WriteCIO(HL: Byte; const CIO: TPasCio);
var
  i: Integer;
  s: string;
  Item: TPasItem;
begin
  if not Assigned(CIO) then Exit;

  CIO.SortPasItems;
  s := GetCIOTypeName(CIO.MyType) + ' ' + CIO.Name;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);
  if not HtmlHelp then WriteDocumentHeadline;

  WriteStartOfAnchor(CIO.Name);
  WriteEndOfAnchor;
  WriteHeading(HL, s);
  { write unit link }
  if Assigned(CIO.MyUnit) then begin
    WriteHeading(HL + 1, FLanguage.Translation[trUnit]);
    WriteString('<A href="' + CIO.MyUnit.FullLink + '">' + CIO.MyUnit.Name + '</A><BR>');
  end;

  { write declaration link }
  WriteHeading(HL + 1, FLanguage.Translation[trDeclaration]);
  WriteString('<P>');
  WriteStartOfCode;
  WriteString('type ' + CIO.Name + ' = ');
  case CIO.MyType of
    CIO_CLASS: WriteString('class');
    CIO_SPINTERFACE: WriteString('dispinterface');
    CIO_INTERFACE: WriteString('interface');
    CIO_RECORD: WriteString('record');
    CIO_PACKEDRECORD: WriteString('packed record');
  else
    WriteString('object');
  end;

  if not StringVectorIsNilOrEmpty(CIO.Ancestors) then begin
    WriteString('(');
    for i := 0 to CIO.Ancestors.Count - 1 do begin
      s := CIO.Ancestors[i];
      s := SearchLink(s, CIO);
      WriteString(s);
      if (i <> CIO.Ancestors.Count - 1) then
        WriteString(', ');
    end;
    WriteString(')');
  end;
  WriteEndOfCode;
  WriteString('</P>');

  { Write Description }
  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(CIO);

  { Write Hierarchy }
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then begin
    s := CIO.Ancestors.FirstName;
    Item := SearchItem(s, CIO);
    if Assigned(Item) and (Item is TPasCio) then begin
      WriteHeading(HL + 1, FLanguage.Translation[trHierarchy]);
      repeat
        s := CreateReferencedLink(Item.Name, Item.FullLink);
        WriteString(s);

        if not StringVectorIsNilOrEmpty(TPasCio(Item).Ancestors) then begin
          s := TPasCio(Item).Ancestors.FirstName;
          Item := SearchItem(s, Item);

          if (Item <> nil) and (Item is TPasCio) then begin
            WriteString('&nbsp;&gt; ');
            Continue;
          end;
        end;
        Break;
      until False;
    end;
  end;

  WriteFields(HL + 1, CIO.Fields);

  WriteFuncsProcs(HL + 1, True, CIO.Methods);

  if (CIO.MyType <> CIO_OBJECT) then begin
    WritePropertiesSummary(HL + 1, CIO.Properties);
    WriteProperties(HL + 1, CIO.Properties);
  end;

  WriteAuthors(HL + 1, CIO.Authors);
  WriteDates(HL + 1, CIO.Created, CIO.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
end;

procedure THTMLDocGenerator.WriteCIOs(HL: Byte; c: TPasItems);
var
  i: Integer;
  p: TPasCio;
begin
  if c = nil then Exit;

  for i := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[i]);
    if not CreateStream(p.OutputFileName) then begin
      DoMessage(1, mtError, 'Could not create Class/Interface/Object documentation file.', []);
      Continue;
    end;
    DoMessage(3, mtInformation, 'Creating Class/Interface/Object file for "%s"...', [p.Name]);
    WriteCIO(HL, p);
  end;
  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteCIOSummary(HL: Byte; c: TPasItems);
var
  j: Integer;
  p: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;

  if HtmlHelp then
    WriteString('<A name=@Classes></A>');

  WriteHeading(HL, FLanguage.Translation[trCio]);
  WriteStartOfTable2Columns(FLanguage.Translation[trName], FLanguage.Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteStartOfTableRow;
      { name of class/interface/object and unit }
    WriteStartOfTableCell;
    WriteString(GetCIOTypeName(p.MyType));
    WriteString('&nbsp;');
    WriteStartOfLink(p.FullLink);
    WriteString(CodeString(p.Name));
    WriteEndOfLink;
    WriteEndOfTableCell;

      { Description of class/interface/object }
    if j = 0 then
      WriteString('<TD width=100%>')
    else
      WriteStartOfTableCell;
      { Write only the description and do not opt for DetailedDescription,
        like WriteItemDescription does. }
    if p.Description <> '' then
      WriteWithURLs(p.Description)
    else
      WriteString('&nbsp;');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

procedure THTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem; const Code:
  string; const ItemLink: string);
var
  NameFound, SearchForLink: Boolean;
  FoundItem: TPasItem;
  i, j, l: Integer;
  s: string;
  pl: TStandardDirective;
  n, ncstart: Integer;
  S1: string;
  S2: string;
  S3: string;
begin
  WriteStartOfCode;
  i := 1;
  NameFound := false;
  SearchForLink := False;
  l := Length(Code);
  ncstart := i;
  while i <= l do begin
    case Code[i] of
      '_', 'A'..'Z', 'a'..'z': begin
          WriteString(Copy(Code, ncstart, i - ncstart));
          { assemble item }
          j := i;
          repeat
            Inc(i);
          until (i > l) or (not (Code[i] in ['.', '_', '0'..'9', 'A'..'Z', 'a'..'z']));
          s := Copy(Code, j, i - j);

           { Special processing for standard directives. }
          pl := StandardDirectiveByName(s);
          case pl of
            SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
              SD_FAR, SD_FORWARD, SD_NAME, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
              SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL:
              begin
                WriteString(s);
                SearchForLink := False;
                ncstart := i;
                Continue;
              end;
            SD_EXTERNAL:
              begin
                WriteString(s);
                SearchForLink := True;
                ncstart := i;
                Continue;
              end;
          end;
          if not NameFound and (s = p.Name) then begin
            if ItemLink <> '' then begin
              WriteStartOfLink(ItemLink);
              WriteString('<B>' + s + '</B>');
              WriteEndOfLink;
            end else begin
              WriteString('<B>' + s + '</B>')
            end;
            NameFound := True;
          end else begin
            { search for item of name  L }
            if SearchForLink and (SplitLink(s, S1, S2, S3, n)) then begin
              FoundItem := p.FindName(S1, S2, S3, n);
              if not Assigned(FoundItem) then
                FoundItem := FindGlobal(S1, S2, S3, n);
            end
            else
              FoundItem := nil;

            if Assigned(FoundItem) then begin
              WriteStartOfLink(FoundItem.FullLink);
              WriteString(s);
              WriteEndOfLink;
            end else begin
              WriteString(s);
            end;
          end;
          ncstart := i;
          Continue; // We don't want to miss out on any ':' or ';' for SearchForLink
        end;
      ':',
      '=': SearchForLink := True;
      ';': SearchForLink := False;
      '''': begin
          Inc(i);
          while (i<=l) and (Code[i] <> '''') do Inc(i);
        end;
    end;
    Inc(i);
  end;
  WriteString(Copy(Code, ncstart, i - ncstart));
  WriteEndOfCode;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDates(const HL: Byte; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteString(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteLine(LastMod);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDocumentation;
{$IFDEF MSWINDOWS}
var
  HhcPath: string;
{$ENDIF}
begin
  StartSpellChecking('sgml');
  FLinkCount := 1;
  inherited;
  WriteUnits(1);
  WriteHierachy;
  WriteBinaryFiles;
  WriteOverviewFiles;
{$IFDEF MSWINDOWS}
  { Registry and HCC only exists on Windows. }
  if HtmlHelp and not NoHHC then begin // Try to call HCC.exe
    HhcPath := RegReadStrDefA(HKEY_CURRENT_USER,
      'Software\Microsoft\HTML Help Workshop', 'InstallDir', '');
    if (HhcPath = '') or
      not DirectoryExistsA(HhcPath) or
      (ShellExecute(GetDeskTopWindow(), 'Open',
      PChar(HhcPath + '\hhc.exe'),
      PChar(DestinationDirectory + ProjectName + '.hhp'),
      '', SW_SHOW) <= 32) then
      DoMessage(1, mtError, 'Could not compile HtmlHelp.', []);
  end;
{$ENDIF}
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDocumentHeadline;
var
  i: Byte;
begin
  WriteLine('<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  WriteLine('<TR bgcolor="' + HTML_HEADER_BACKGROUND_COLOR
    + '">');
  for i := 0 to NUM_OVERVIEW_FILES - 1 do begin
    WriteString('<TD><A href="' + OverviewFilenames[i] +
      GetFileExtension + '"><CENTER>');
    case i of
      0: WriteString(FLanguage.Translation[trUnits]);
      1: WriteString(FLanguage.Translation[trClassHierarchy]);
      2: WriteString(FLanguage.Translation[trCio]);
      3: WriteString(FLanguage.Translation[trTypes]);
      4: WriteString(FLanguage.Translation[trVariables]);
      5: WriteString(FLanguage.Translation[trConstants]);
      6: WriteString(FLanguage.Translation[trFunctionsAndProcedures]);
      7: WriteString(FLanguage.Translation[trIdentifiers]);
    end;
    WriteLine('</CENTER></A></TD>');
  end;
  WriteLine('</TR>');
  WriteLine('</TABLE>');
end;

procedure THTMLDocGenerator.WriteEmptyCell;
begin
  WriteString('&nbsp;');
end;

procedure THTMLDocGenerator.WriteEndOfDocument;
begin
  WriteLine('</BODY>');
  WriteLine('</HTML>');
end;

procedure THTMLDocGenerator.WriteEndOfAnchor;
begin
  WriteString('</A>');
end;

procedure THTMLDocGenerator.WriteEndOfCode;
begin
  WriteString('</CODE>');
end;

procedure THTMLDocGenerator.WriteEndOfLink;
begin
  WriteString('</A>');
end;

procedure THTMLDocGenerator.WriteEndOfParagraph;
begin
  WriteLine('</P>');
end;

procedure THTMLDocGenerator.WriteEndOfTableCell;
begin
  WriteLine('</TD>');
end;

procedure THTMLDocGenerator.WriteEndOfTable;
begin
  WriteLine('</TABLE>');
end;

procedure THTMLDocGenerator.WriteEndOfTableRow;
begin
  WriteLine('</TR>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFields(const Order: Byte; const Fields:
  TPasItems);
var
  j: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(Fields) then Exit;

  WriteString('<A name=@Fields></A>');
  WriteHeading(Order, FLanguage.Translation[trFields]);

  WriteString('<TABLE cellspacing=' +
    HTML_TABLE_CELLSPACING + ' cellpadding=' + HTML_TABLE_CELLPADNG +
    ' width=100%>');
  WriteString('<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '">');
  WriteLine('<TH>&nbsp;</TH><TH>' + FLanguage.Translation[trName] +
    '</TH><TH>' + FLanguage.Translation[trDescription] + '</TH></TR>');

  for j := 0 to Fields.Count - 1 do begin
    Item := Fields.PasItemAt[j];
    WriteStartOfTableRow;

    WriteVisibilityCell(Item);

    WriteStartOfTableCell;
    WriteStartOfAnchor(Item.Name);
    WriteEndOfAnchor;
    WriteString(CodeString(Item.Name));
    WriteEndOfTableCell;

    if j = 0 then
      WriteString('<TD width=100%>')
    else
      WriteStartOfTableCell;

    WriteItemDetailedDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  WriteString('</TABLE>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFooter;
begin
  WriteString(Footer);
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFuncsProcs(const HL: Byte; const Methods:
  Boolean; const FuncsProcs: TPasMethods);
var
  i: Integer;
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;

  if Methods then begin
    if HtmlHelp then
      WriteString('<A name=@Methods></A>');
    WriteHeading(HL, FLanguage.Translation[trMethods]);
  end
  else begin
    if HtmlHelp then
      WriteString('<A name=@FuncsProcs></A>');
    WriteHeading(HL, FLanguage.Translation[trFunctionsAndProcedures]);
  end;

  FuncsProcs.SortByPasItemName;

  for i := 0 to 1 do begin
    if (i = 0) then begin
      WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
      WriteStartOfTable1Column('');
    end
    else
      WriteHeading(HL + 1, FLanguage.Translation[trDescription]);

    for j := 0 to FuncsProcs.Count - 1 do begin
      p := TPasMethod(FuncsProcs.PasItemAt[j]);
      if (i = 0) then begin
        WriteStartOfTableRow;

              { Only write visibility for methods of classes and objects. }
        if Methods then WriteVisibilityCell(p);

        if j = 0 then
          WriteString('<TD width=100%>')
        else
          WriteStartOfTableCell;

        s := p.FullLink;
        if Assigned(p.MyUnit) then
          if CompareText(p.MyUnit.FullLink, Copy(s, 1,
            Length(p.MyUnit.FullLink))) = 0 then
            Delete(s, 1, Length(p.MyUnit.FullLink));

        WriteCodeWithLinks(p, p.FullDeclaration, s);

        WriteEndOfTableCell;
        WriteEndOfTableRow;
      end
      else begin
        WriteStartOfTable1Column('');
        WriteStartOfTableRow;

        if Methods then WriteVisibilityCell(p);

        WriteString('<TD width=100%>');
        WriteStartOfAnchor(p.Name);
        WriteEndOfAnchor;

        WriteCodeWithLinks(p, p.FullDeclaration, '');
        WriteEndOfTableCell;
        WriteEndOfTableRow;
        WriteEndOfTable;

        WriteStartOfParagraph;
        WriteItemDetailedDescription(p);
        WriteEndOfParagraph;
      end;
    end;
    if (i = 0) then WriteEndOfTable;
  end;
end;

procedure THTMLDocGenerator.WriteHeading(Level: Byte; const s: string);
var
  c: string;
begin
  if (Level < 1) then Level := 1;
  if Level > 6 then begin
    DoMessage(2, mtWarning, 'HTML generator cannot write headlines of level 7 or greater; will use 6 instead.', []);
    Level := 6;
  end;
  c := IntToStr(Level);
  WriteString('<H' + c + '>');
  WriteString(s);
  WriteLine('</H' + c + '>');
end;

{ ---------- }

procedure THTMLDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;

  if AItem.Description <> '' then begin
    WriteWithURLs(AItem.Description);
  end else begin
    if AItem.DetailedDescription <> '' then begin
      WriteWithURLs(AItem.DetailedDescription)
    end else begin
      WriteString('&nbsp;');
    end;
  end;
end;

procedure THTMLDocGenerator.WriteItemDetailedDescription(const AItem:
  TPasItem);
begin
  if not Assigned(AItem) then Exit;

  if AItem.Description <> '' then begin
    WriteWithURLs(AItem.Description);

    if AItem.DetailedDescription <> '' then begin
      WriteString('<P>');
      WriteWithURLs(AItem.DetailedDescription);
    end;
  end else begin
    if AItem.DetailedDescription <> '' then begin
      WriteWithURLs(AItem.DetailedDescription)
    end else begin
      WriteString('&nbsp;');
    end;
  end;
end;

procedure THTMLDocGenerator.WriteItems(HL: Byte; Heading: string; const
  Anchor: string; const i: TPasItems);
var
  j, k: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(i) then Exit;

  if HtmlHelp and (Anchor <> '') then
    WriteString('<A name=@' + Anchor + '></A>');

  WriteHeading(HL, Heading);

  WriteString('<TABLE cellspacing=' +
    HTML_TABLE_CELLSPACING + ' cellpadding=' + HTML_TABLE_CELLPADNG +
    ' width=100%>');
  WriteString('<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '">');
  WriteLine('<TH>' + FLanguage.Translation[trName] + '</TH><TH>' +
    FLanguage.Translation[trDescription] + '</TH></TR>');

  for j := 0 to i.Count - 1 do begin
    Item := i.PasItemAt[j];
    WriteStartOfTableRow;

    WriteStartOfTableCell;
    WriteStartOfAnchor(Item.Name);
    WriteEndOfAnchor;
    WriteString(Item.Name);
    if Item is TPasVarConst then begin
      WriteCodeWithLinks(Item, TPasVarConst(Item).FullDeclaration, '');
    end;
    WriteEndOfTableCell;

    if j = 0 then
      WriteString('<TD width=100%>')
    else
      WriteStartOfTableCell;
    WriteItemDetailedDescription(Item);
    if Item is TPasEnum then begin
      WriteLine('<UL>');
      for k := 0 to TPasEnum(Item).Members.Count-1 do begin
        WriteLine('<LI>');
        WriteString(TPasItem(TPasEnum(Item).Members.ObjectAt[k]).Name);
        WriteString(': ');
        WriteWithURLs(TPasItem(TPasEnum(Item).Members.ObjectAt[k]).GetDescription);
        WriteLine('</LI>');
      end;
      WriteLine('</UL>');
    end;
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  WriteString('</TABLE>');
end;

{ ---------- }

procedure THTMLDocGenerator.WriteOverviewFiles;
var
  ItemsToCopy: TPasItems;
  PartialItems: TPasItems;
  TotalItems: TPasItems; // Collect all Items for final listing.
  i: Integer;
  Item: TPasItem;
  j: Integer;
  PU: TPasUnit;
begin
  if HtmlHelp then
    WriteHtmlHelpProject;

  WriteUnitOverviewFile;

  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  // Make sure we don't free the Itmes when we free the container.
  TotalItems := TPasItems.Create(False);

  for i := 2 to 6 do begin
    if (not CreateStream(OverviewFilenames[i] + GetFileExtension))
      then begin
      DoMessage(1, mtError, 'Error: Could not create output file "' +
        OverviewFilenames[i] + '".', []);
      Exit;
    end;
    DoMessage(3, mtInformation, 'Writing overview file ' +
      OverviewFilenames[i] + '...', []);

    case i of
      2: WriteStartOfDocument(FLanguage.Translation[trHeadlineCio]);
      3: WriteStartOfDocument(FLanguage.Translation[trHeadlineTypes]);
      4: WriteStartOfDocument(FLanguage.Translation[trHeadlineVariables]);
      5: WriteStartOfDocument(FLanguage.Translation[trHeadlineConstants]);
      6: WriteStartOfDocument(FLanguage.Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteStartOfDocument(FLanguage.Translation[trHeadlineIdentifiers]);
    end;

    if not HtmlHelp then WriteDocumentHeadline;

    case i of
      2: WriteHeading(1, FLanguage.Translation[trHeadlineCio]);
      3: WriteHeading(1, FLanguage.Translation[trHeadlineTypes]);
      4: WriteHeading(1, FLanguage.Translation[trHeadlineVariables]);
      5: WriteHeading(1, FLanguage.Translation[trHeadlineConstants]);
      6: WriteHeading(1, FLanguage.Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteHeading(1, FLanguage.Translation[trHeadlineIdentifiers]);
    end;

      // Make sure we don't free the Itmes when we free the container.
    PartialItems := TPasItems.Create(False);

    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      case i of
        2: ItemsToCopy := PU.CIOs;
        3: ItemsToCopy := PU.Types;
        4: ItemsToCopy := PU.Variables;
        5: ItemsToCopy := PU.Constants;
        6: ItemsToCopy := PU.FuncsProcs;
      else
        ItemsToCopy := nil;
      end;
      PartialItems.InsertItems(ItemsToCopy);
    end;

    if not ObjectVectorIsNilOrEmpty(PartialItems) then begin
      WriteStartOfTable3Columns(FLanguage.Translation[trName], FLanguage.Translation[trUnit],
        FLanguage.Translation[trDescription]);

      PartialItems.SortByPasItemName;

      for j := 0 to PartialItems.Count - 1 do begin
        Item := PartialItems.PasItemAt[j];
        WriteStartOfTableRow;

        WriteStartOfTableCell;
        WriteStartOfLink(Item.FullLink);
        WriteString(Item.Name);
        WriteEndOfLink;
        WriteEndOfTableCell;

        WriteStartOfTableCell;
        WriteStartOfLink(Item.MyUnit.FullLink);
        WriteString(Item.MyUnit.Name);
        WriteEndOfLink;
        WriteEndOfTableCell;

        if j = 0 then
          WriteString('<TD width=100%>')
        else
          WriteStartOfTableCell;
        WriteItemDescription(Item);
        WriteEndOfTableCell;

        WriteEndOfTableRow;
      end;
      WriteEndOfTable;
    end
    else begin
      WriteStartOfParagraph;
      WriteString(FLanguage.Translation[trNone]);
      WriteEndOfParagraph;
    end;

    TotalItems.InsertItems(PartialItems);
    PartialItems.Free;
    WriteFooter;
    WriteAppInfo;
    WriteEndOfDocument;
    CloseStream;
  end;

  if not CreateStream(OverviewFilenames[7] + GetFileExtension) then
    begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[7] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing overview file ' + OverviewFilenames[7]
    + '...', []);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineIdentifiers]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, FLanguage.Translation[trHeadlineIdentifiers]);
  WriteStartOfTable3Columns(FLanguage.Translation[trName], FLanguage.Translation[trUnit],
    FLanguage.Translation[trDescription]);

  TotalItems.SortByPasItemName;
  for j := 0 to TotalItems.Count - 1 do begin
    Item := TotalItems.PasItemAt[j];
    WriteStartOfTableRow;

    WriteStartOfTableCell;
    WriteStartOfLink(Item.FullLink);
    WriteString(Item.Name);
    WriteEndOfLink;
    WriteEndOfTableCell;

    WriteStartOfTableCell;
    WriteStartOfLink(Item.MyUnit.FullLink);
    WriteString(Item.MyUnit.Name);
    WriteEndOfLink;
    WriteEndOfTableCell;

    if j = 0 then
      WriteString('<TD width=100%>')
    else
      WriteStartOfTableCell;
    WriteItemDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;

  TotalItems.Free;

  WriteEndOfTable;
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

procedure THTMLDocGenerator.WriteParagraph(HL: Byte; s: string; t: string);
begin
  // if (not Assigned(t)) or (t.Content < 1) then Exit;
  WriteHeading(HL, s);
  WriteLine('<P>');
  WriteWithURLs(t);
  WriteLine('</P>');
end;

procedure THTMLDocGenerator.WriteProperties(HL: Byte; const p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
begin
  if ObjectVectorIsNilOrEmpty(p) then Exit;

  WriteHeading(HL + 1, FLanguage.Translation[trDescription]);
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);

    WriteStartOfTable1Column('');
    WriteStartOfTableRow;

    WriteVisibilityCell(Prop);

    WriteString('<TD width=100%>');
    WriteStartOfAnchor(Prop.Name);
    WriteEndOfAnchor;
    WriteCodeWithLinks(Prop, 'property ' + Prop.FullDeclaration, '');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
    WriteEndOfTable;

    WriteStartOfParagraph;
    WriteItemDetailedDescription(Prop);
    WriteEndOfParagraph;

  end;
end;

procedure THTMLDocGenerator.WritePropertiesSummary(HL: Byte; p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
begin
  if ObjectVectorIsNilOrEmpty(p) then Exit;

  if HtmlHelp then
    WriteString('<A name=Properties></A>');

  WriteHeading(HL, FLanguage.Translation[trProperties]);
  WriteHeading(HL + 1, FLanguage.Translation[trOverview]);

  WriteStartOfTable1Column('');
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);
    WriteStartOfTableRow;

    WriteVisibilityCell(Prop);
    if j = 0 then
      WriteString('<TD width=100%>')
    else
      WriteStartOfTableCell;

    WriteCodeWithLinks(Prop, 'property ' + Prop.FullDeclaration,
      Prop.FullLink);

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfAnchor(const Name: string);
begin
  WriteString('<A name="' + Name + '">');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfCode;
begin
  WriteString('<CODE>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfDocument(Name: string);
begin
  WriteLine('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">');
  WriteLine('<HTML>');
  WriteLine('<HEAD>');
  WriteLine('<META name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  // Check if we need to specify character sets
  if FLanguage.CharSet <> '' then begin
    WriteLine('<META http-equiv="content-type" content="text/html; charset=' + FLanguage.CharSet + '">');
  end;
  // Title
  WriteString('<TITLE>');
  if {not HtmlHelp and}(Title <> '') then begin
    WriteString(Title + ': ');
  end;
  WriteString(Name);
  WriteLine('</TITLE>');
  // StyleSheet
  WriteString('<LINK rel="StyleSheet" href="');
  WriteString(ProjectName);
  WriteLine('.css">');

  WriteLine('</HEAD>');
  WriteLine('<BODY bgcolor="#ffffff" text="#000000" link="#0000ff" vlink="#800080" alink="#FF0000">');

  if Length(Header) > 0 then begin
    WriteWithURLs(Header);
  end;
end;

procedure THTMLDocGenerator.WriteStartOfLink(const Name: string);
begin
  WriteString('<A href="' + Name + '">');
end;

procedure THTMLDocGenerator.WriteStartOfParagraph;
begin
  WriteString('<P>');
end;

procedure THTMLDocGenerator.WriteStartOfTable1Column(t: string);
begin
  WriteLine('<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
end;

procedure THTMLDocGenerator.WriteStartOfTable2Columns(t1, t2: string);
begin
  WriteLine('<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  WriteString('<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '"><TH>');
  WriteString(t1);
  WriteString('</TH><TH>');
  WriteString(t2);
  WriteLine('</TH></TR>');
end;

procedure THTMLDocGenerator.WriteStartOfTable3Columns(t1, t2, T3: string);
begin
  WriteLine('<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  WriteString('<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '"><TH>');
  WriteString(t1);
  WriteString('</TH><TH>');
  WriteString(t2);
  WriteString('</TH><TH>');
  WriteString(T3);
  WriteLine('</TH></TR> ');
end;

procedure THTMLDocGenerator.WriteStartOfTableCell;
begin
  WriteString('<TD>');
end;

procedure THTMLDocGenerator.WriteStartOfTableRow;
begin
  WriteString('<TR bgcolor=#' + HTML_ROW_BACKGROUND_COLOR
    + ' valign=top>');
end;

{ ---------------------------------------------------------------------------- }
{ HtmlHelp Content Generation inspired by Wim van der Vegt <wvd_vegt@knoware.nl>
{ ---------------------------------------------------------------------------- }

function BeforeEqualChar(const s: string): string;
var
  i: Cardinal;
begin
  Result := s;
  i := Pos('=', Result);
  if i <> 0 then
    SetLength(Result, i - 1);
end;

function AfterEqualChar(const s: string): string;
var
  i: Cardinal;
begin
  Result := s;
  i := Pos('=', Result);
  if i <> 0 then
    Delete(Result, 1, i)
  else
    Result := '';
end;

function GetLevel(var s: string): Integer;
var
  l: Cardinal;
  p: PChar;
begin
  Result := 0;
  p := Pointer(s);
  l := Length(s);
  while (l > 0) and (p^ in [' ', #9]) do begin
    Inc(Result);
    Inc(p);
    Dec(l);
  end;
  Delete(s, 1, Result);
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteHtmlHelpProject;
var
  DefaultContentsWritten: Boolean;
  DefaultTopic: string;

  procedure WriteLiObject(const Name, Local: string);
  begin
    WriteLine('<LI><OBJECT type="text/sitemap">');
    WriteLine('<PARAM name="Name" value="' + Name + '">');
    if Local <> '' then begin
      WriteLine('<PARAM name="Local" value="' + Local + '">');
      if DefaultTopic = '' then
        DefaultTopic := Local;
    end;
    WriteLine('</OBJECT>');
  end;

  { ---------- }

  procedure WriteItemCollection(const c: TPasItems);
  var
    i: Integer;
    Item: TPasItem;
  begin
    if Assigned(c) then begin
      WriteLine('<UL>');
      for i := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[i];
        WriteLiObject(Item.Name, Item.FullLink);
      end;
      WriteLine('</UL>');
    end;
  end;

  { ---------- }

  procedure WriteItemHeadingCollection(const Title, FullLink: string; const
    c: TPasItems);
  begin
    if Assigned(c) and (c.Count > 0) then begin
      WriteLiObject(Title, FullLink);
      WriteItemCollection(c);
    end;
  end;

  { ---------- }

  procedure InternalWriteCIO(const ClassItem: TPasCio);
  begin
    WriteLiObject(ClassItem.Name, ClassItem.FullLink);
    WriteLine('<UL>');

    WriteItemHeadingCollection('Fields', ClassItem.FullLink + '#@Fields', ClassItem.Fields);
    WriteItemHeadingCollection('Properties', ClassItem.FullLink + '#@Properties', ClassItem.Properties);
    WriteItemHeadingCollection('Methods', ClassItem.FullLink + '#@Methods', ClassItem.Methods);

    WriteLine('</UL>');
  end;

  { ---------- }

  procedure ContentWriteUnits(const Text: string);
  var
    c: TPasItems;
    j, k: Integer;
    PU: TPasUnit;
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[0] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trUnits], OverviewFilenames[0] +
        GetFileExtension);
    WriteLine('<UL>');

    // Iterate all Units
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      WriteLiObject(PU.Name, PU.FullLink);
      WriteLine('<UL>');

        // For each unit, write classes (if there are any).
      c := PU.CIOs;
      if Assigned(c) then begin
        WriteLiObject(FLanguage.Translation[trClasses], PU.FullLink + '#@Classes');
        WriteLine('<UL>');

        for k := 0 to c.Count - 1 do
          InternalWriteCIO(TPasCio(c.PasItemAt[k]));

        WriteLine('</UL>');
      end;

        // For each unit, write Functions & Procedures.
      WriteItemHeadingCollection(FLanguage.Translation[trFunctionsAndProcedures],
        PU.FullLink + '#@FuncsProcs', PU.FuncsProcs);
        // For each unit, write Types.
      WriteItemHeadingCollection(FLanguage.Translation[trTypes], PU.FullLink +
        '#@Types', PU.Types);
        // For each unit, write Constants.
      WriteItemHeadingCollection(FLanguage.Translation[trConstants], PU.FullLink +
        '#@Constants', PU.Constants);

      WriteLine('</UL>');
    end;
    WriteLine('</UL>');
  end;

  { ---------- }

  procedure ContentWriteClasses(const Text: string);
  var
    c: TPasItems;
    j: Integer;
    PU: TPasUnit;
  begin
    // Write Classes to Contents
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[2] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trClasses], OverviewFilenames[2] +
        GetFileExtension);
    WriteLine('<UL>');

    c := TPasItems.Create(False);
    // First collect classes
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      c.CopyItems(PU.CIOs);
    end;
    // Output sorted classes
    // TODO: Sort
    for j := 0 to c.Count - 1 do
      InternalWriteCIO(TPasCio(c.PasItemAt[j]));
    c.Free;
    WriteLine('</UL>');
  end;

  { ---------- }

  procedure ContentWriteClassHierarchy(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[1] + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trClassHierarchy], OverviewFilenames[1] +
        GetFileExtension);
  end;

  { ---------- }

  procedure ContentWriteOverview(const Text: string);
  var
    j: Integer;
  begin
    if Text <> '' then
      WriteLiObject(Text, '')
    else
      WriteLiObject(FLanguage.Translation[trOverview], '');
    WriteLine('<UL>');
    for j := 0 to NUM_OVERVIEW_FILES - 1 do begin
      WriteLine('<LI><OBJECT type="text/sitemap">');
      case j of
        0: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineUnits] + '">');
        1: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trClassHierarchy] + '">');
        2: WriteLine('<PARAM name="Name" value="' + FLanguage.Translation[trHeadlineCio]
          + '">');
        3: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineTypes] + '">');
        4: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineVariables] + '">');
        5: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineConstants] + '">');
        6: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineFunctionsAndProcedures] + '">');
        7: WriteLine('<PARAM name="Name" value="' +
          FLanguage.Translation[trHeadlineIdentifiers] + '">');
      end;
      WriteLine('<PARAM name="Local" value="' + OverviewFilenames[j] + GetFileExtension + '">');
      WriteLine('</OBJECT>');
    end;
    WriteLine('</UL>');
  end;

  { ---------- }

  procedure ContentWriteLegend(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, 'Legend'+GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trLegend], 'Legend'+GetFileExtension);
  end;

  { ---------- }

  procedure ContentWriteCustom(const Text, Link: string);
  begin
    if CompareText('@Classes', Link) = 0 then begin
      DefaultContentsWritten := True;
      ContentWriteClasses(Text);
    end
    else
      if CompareText('@ClassHierarchy', Link) = 0 then begin
        DefaultContentsWritten := True;
        ContentWriteClassHierarchy(Text);
      end
      else
        if CompareText('@Units', Link) = 0 then begin
          DefaultContentsWritten := True;
          ContentWriteUnits(Text);
        end
        else
          if CompareText('@Overview', Link) = 0 then begin
            DefaultContentsWritten := True;
            ContentWriteOverview(Text);
          end
          else
            if CompareText('@Legend', Link) = 0 then begin
              DefaultContentsWritten := True;
              ContentWriteLegend(Text);
            end
            else
              WriteLiObject(Text, Link);
  end;

  procedure IndexWriteItem(const Item, PreviousItem, NextItem: TPasItem);
    { Item is guaranteed to be assigned, i.e. not to be nil. }
  begin
    if Assigned(Item.MyObject) then begin
      if (Assigned(NextItem) and Assigned(NextItem.MyObject) and
        (CompareText(Item.MyObject.Name, NextItem.MyObject.Name) = 0)) or
        (Assigned(PreviousItem) and Assigned(PreviousItem.MyObject) and
          (CompareText(Item.MyObject.Name, PreviousItem.MyObject.Name) = 0))
          then
        WriteLiObject(Item.MyObject.Name + ' - ' + Item.MyUnit.Name + #32 +
          FLanguage.Translation[trUnit], Item.FullLink)
      else
        WriteLiObject(Item.MyObject.Name, Item.FullLink);
    end
    else begin
      WriteLiObject(Item.MyUnit.Name + #32 + FLanguage.Translation[trUnit],
        Item.FullLink);
    end;
  end;

  { ---------------------------------------------------------------------------- }

var
  j, k: Integer;
  CurrentLevel, Level: Integer;
  CIO: TPasCio;
  PU: TPasUnit;
  c: TPasItems;
  Item, NextItem, PreviousItem: TPasItem;
  s, Text, Link: string;
  SL: TStringVector;
begin
  { At this point, at least one unit has been parsed:
    Units is assigned and Units.Count > 0
    No need to test this again. }

  if not CreateStream(ProjectName + '.hhc') then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Content file "%s.hhc' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Content file "' + ProjectName
    + '"...', []);

  // File Header
  WriteLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteLine('<HTML>');
  WriteLine('<HEAD>');
  WriteLine('<META name="GENERATOR" content="' +
    PASDOC_NAME_AND_VERSION + '">');
  WriteLine('</HEAD><BODY>');
  WriteLine('<UL>');

  DefaultContentsWritten := False;
  DefaultTopic := '';
  if ContentsFile <> '' then begin
    SL := NewStringVector;
    try
      SL.LoadFromTextFileAdd(ContentsFile);
    except
      on e: Exception do
        DoMessage(1, mtError, e.Message +
          '. Writing default HtmlHelp contents.', []);
    end;

    CurrentLevel := 0;
    for j := 0 to SL.Count - 1 do begin
      s := SL[j];
      Text := BeforeEqualChar(s);
      Level := GetLevel(Text);
      Link := AfterEqualChar(s);

      if Level = CurrentLevel then
        ContentWriteCustom(Text, Link)
      else
        if CurrentLevel = (Level - 1) then begin
          WriteLine('<UL>');
          Inc(CurrentLevel);
          ContentWriteCustom(Text, Link)
        end
        else
          if CurrentLevel > Level then begin
            WriteLine('</UL>');
            Dec(CurrentLevel);
            while CurrentLevel > Level do begin
              WriteLine('</UL>');
              Dec(CurrentLevel);
            end;
            ContentWriteCustom(Text, Link)
          end

          else begin
            DoMessage(1, mtError, 'Invalid level ' + IntToStr(Level) +
              'in Content file (line ' + IntToStr(j) + ').', []);
            Exit;
          end;
    end;
    SL.Free;
  end;

  if not DefaultContentsWritten then begin
    ContentWriteUnits('');
    ContentWriteClassHierarchy(FLanguage.Translation[trClassHierarchy]);
    ContentWriteClasses('');
    ContentWriteOverview('');
    ContentWriteLegend('');
  end;

  // End of File
  WriteLine('</UL>');
  WriteLine('</BODY></HTML>');
  CloseStream;

  // Create Keyword Index
  // First collect all Items
  c := TPasItems.Create(False); // Don't free Items when freeing the container

  for j := 0 to Units.Count - 1 do begin
    PU := Units.UnitAt[j];

    if Assigned(PU.CIOs) then
      for k := 0 to PU.CIOs.Count - 1 do begin
        CIO := TPasCio(PU.CIOs.PasItemAt[k]);
        c.InsertObjectLast(CIO);
        c.CopyItems(CIO.Fields);
        c.CopyItems(CIO.Properties);
        c.CopyItems(CIO.Methods);
      end;

    c.CopyItems(PU.Types);
    c.CopyItems(PU.Variables);
    c.CopyItems(PU.Constants);
    c.CopyItems(PU.FuncsProcs);
  end;

  if not CreateStream(ProjectName + '.hhk') then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Index file "%s.hhk' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Index file "%s"...',
    [ProjectName]);

  WriteLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteLine('<HTML>');
  WriteLine('<HEAD>');
  WriteLine('<META name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION + '">');
  WriteLine('</HEAD><BODY>');
  WriteLine('<UL>');

  // Write all Items to KeyWord Index

  c.SortByPasItemName;

  if c.Count > 0 then begin
    Item := c.PasItemAt[0];
    j := 1;

    while j < c.Count do begin
      NextItem := c.PasItemAt[j];

          // Does the next Item have a different name?
      if CompareText(Item.Name, NextItem.Name) <> 0 then begin
        WriteLiObject(Item.Name, Item.FullLink);
        Item := NextItem;
      end
      else begin
              // Write the Item. It acts as a header for the subitems to follow.
        WriteLiObject(Item.Name, Item.FullLink);
              // Indent by one.
        WriteLine('<UL>');

              // No previous Item as we start.
        PreviousItem := nil;

              // Keep on writing Items with the same name as subitems.
        repeat
          IndexWriteItem(Item, PreviousItem, NextItem);

          PreviousItem := Item;
          Item := NextItem;
          Inc(j);

          if j >= c.Count then Break;
          NextItem := c.PasItemAt[j];

                // Break as soon Items' names are different.
        until CompareText(Item.Name, NextItem.Name) <> 0;

              // No NextItem as we write the last one of the same Items.
        IndexWriteItem(Item, PreviousItem, nil);

        Item := NextItem;
        WriteLine('</UL>');
      end;

      Inc(j);
    end;

      // Don't forget to write the last item. Can it ever by nil?
    WriteLiObject(Item.Name, Item.FullLink);
  end;

  c.Free;

  WriteLine('</UL>');
  WriteLine('</BODY></HTML>');
  CloseStream;

  // Create a HTML Help Project File
  if (not CreateStream(ProjectName + '.hhp')) then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Project file "%s.hhp' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing Html Help Project file "%s"...',
    [ProjectName]);

  WriteLine('[OPTIONS]');
  WriteLine('Binary TOC=Yes');
  WriteLine('Compatibility=1.1 or later');
  WriteLine('Compiled file=' + ProjectName + '.chm');
  WriteLine('Contents file=' + ProjectName + '.hhc');
  WriteLine('Default Window=Default');
  WriteLine('Default topic=' + DefaultTopic);
  WriteLine('Display compile progress=Yes');
  WriteLine('Error log file=' + ProjectName + '.log');
  WriteLine('Full-text search=Yes');
  WriteLine('Index file=' + ProjectName + '.hhk');
  if Title <> '' then
    WriteLine('Title=' + Title)
  else
    WriteLine('Title=' + ProjectName);

  WriteLine('');
  WriteLine('[WINDOWS]');
  if Title <> '' then
    WriteLine('Default="' + Title + '","' + ProjectName +
      '.hhc","' + ProjectName + '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0')
  else
    WriteLine('Default="' + ProjectName + '","' +
      ProjectName + '.hhc","' + ProjectName +
      '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0');

  WriteLine('');
  WriteLine('[FILES]');

  { HHC seems to know about the files by reading the Content and Index.
    So there is no need to specify them in the FILES section.

  WriteLine('Legend.htm');
  for k := 0 to NUM_OVERVIEW_FILES - 1 do
    WriteLine(OverviewFilenames[k] + '.htm');

  if Assigned(Units) then
    for k := 0 to units.Count - 1 do
      begin
        Item := units.PasItemAt[k);
        PU := units.PasItemAt[k);
        WriteLine(Item.FullLink);
        c := PU.CIO;
        if Assigned(c) then
          for l := 0 to c.Count - 1 do
            begin
              Item2 := c.PasItemAt[l);
              WriteLine(Item2^.FullLink);
            end;
      end;}

  WriteLine('');

  WriteLine('[INFOTYPES]');

  WriteLine('');

  WriteLine('[MERGE FILES]');

  CloseStream;

  // Create a Main Topic
  if (not CreateStream('Legend'+GetFileExtension)) then begin
    DoMessage(1, mtError, 'Could not create file "Legend'+GetFileExtension+'".', []);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing Legend...', []);

  WriteStartOfDocument(FLanguage.Translation[trLegend]);
  WriteHeading(1, FLanguage.Translation[trLegend]);
  WriteString('<TABLE cellpadding=5>');
  WriteString('<TR><TD><IMG src="private.gif" alt="' + FLanguage.Translation[trPrivate]
    + '"></TD><TD>' + FLanguage.Translation[trPrivate] + '</TD></TR>');
  WriteString('<TR><TD><IMG src="protected.gif" alt="' +
    FLanguage.Translation[trProtected] + '"></TD><TD>' + FLanguage.Translation[trProtected] +
    '</TD></TR>');
  WriteString('<TR><TD><IMG src="public.gif" alt="' + FLanguage.Translation[trPublic] +
    '"></TD><TD>' + FLanguage.Translation[trPublic] + '</TD></TR>');
  WriteString('<TR><TD><IMG src="published.gif" alt="' +
    FLanguage.Translation[trPublished] + '"></TD><TD>' + FLanguage.Translation[trPublished] +
    '</TD></TR>');
  WriteString('</TABLE>');
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteUnit(const HL: Byte; const U: TPasUnit);
begin
  if not Assigned(U) then begin
    DoMessage(1, mtError,
      'THTMLDocGenerator.WriteUnit: Unit variable has not been initialized.',
      []);
    Exit;
  end;

  if (not CreateStream(U.OutputFileName)) then begin
    DoMessage(1, mtError, 'Could not create HTML unit doc file for unit %s.',
      [U.Name]);
    Exit;
  end;

  DoMessage(2, mtInformation, 'Writing Docs for unit "%s"', [U.Name]);
  WriteStartOfDocument(U.Name);

  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(HL, FLanguage.Translation[trUnit] + ' ' + U.Name);

  WriteUnitDescription(HL + 1, U);
  WriteUnitUses(HL + 1, U);
  WriteCIOSummary(HL + 1, U.CIOs);
  WriteFuncsProcs(HL + 1, False, U.FuncsProcs);
  WriteTypes(HL + 1, U.Types);
  WriteConstants(HL + 1, U.Constants);
  WriteVariables(HL + 1, U.Variables);
  WriteAuthors(HL + 1, U.Authors);
  WriteDates(HL + 1, U.Created, U.LastMod);
  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;
  CloseStream;
  WriteCIOs(HL, U.CIOs);
end;

procedure THTMLDocGenerator.WriteUnitDescription(HL: Byte; U: TPasUnit);
begin
  WriteHeading(HL, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(U);
end;

procedure THTMLDocGenerator.WriteUnitOverviewFile;
var
  c: TPasItems;
  Item: TPasItem;
  j: Integer;
begin
  c := Units;
  if (not CreateStream(OverviewFilenames[0] + GetFileExtension))
    then begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[0] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing unit overview file "%s" ...',
    [OverviewFilenames[0]]);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineUnits]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, FLanguage.Translation[trHeadlineUnits]);
  if Assigned(c) and (c.Count > 0) then begin
    WriteStartOfTable2Columns(FLanguage.Translation[trName],
      FLanguage.Translation[trDescription]);
    for j := 0 to c.Count - 1 do begin
      Item := c.PasItemAt[j];
      WriteStartOfTableRow;
      WriteStartOfTableCell;
      WriteStartOfLink(Item.FullLink);
      WriteString(Item.Name);
      WriteEndOfLink;
      WriteEndOfTableCell;

      if j = 0 then
        WriteString('<TD width=100%>')
      else
        WriteStartOfTableCell;
      WriteItemDescription(Item);
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

procedure THTMLDocGenerator.WriteVisibilityCell(const Item: TPasItem);
begin
  WriteStartOfTableCell;
  case Item.State of
    STATE_PRIVATE:
      WriteString('<IMG src="private.gif" alt="' +
        FLanguage.Translation[trPrivate] + '">');
    STATE_PROTECTED:
      WriteString('<IMG src="protected.gif" alt="' +
        FLanguage.Translation[trProtected] + '">');
    STATE_PUBLIC:
      WriteString('<IMG src="public.gif" alt="' +
        FLanguage.Translation[trPublic] + '">');
    STATE_PUBLISHED:
      WriteString('<IMG src="published.gif" alt="' +
        FLanguage.Translation[trPublished] + '">');
    STATE_AUTOMATED:
      WriteString('<IMG src="autmated.gif" alt="' +
        FLanguage.Translation[trAutomated] + '">');
  end;
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteHierachy;
var
  Level, OldLevel: Integer;
  Node: TPasItemNode;
begin
  CreateClassHierarchy;
  
  if not CreateStream(OverviewFilenames[1] + GetFileExtension) then begin
    DoMessage(1, mtError, 'Could not create output file "%s".',
      [OverviewFilenames[1] + GetFileExtension]);
    Abort;
  end;

  WriteStartOfDocument(FLanguage.Translation[trClassHierarchy]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, FLanguage.Translation[trClassHierarchy]);

  if FClassHierarchy.IsEmpty then begin
    WriteStartOfParagraph;
    WriteString(FLanguage.Translation[trNone]);
    WriteEndOfParagraph;
  end else begin
    OldLevel := -1;
    Node := FClassHierarchy.FirstItem;
    while Node <> nil do begin
      Level := Node.Level;
      if Level > OldLevel then
        WriteString('<UL>')
      else
        while Level < OldLevel do begin
          WriteString('</UL>');
          Dec(OldLevel);
        end;
      OldLevel := Level;

      if Node.Item = nil then
        WriteString('<LI>' + Node.Name + '</LI>')
      else
        WriteString('<LI><A href="' + Node.Item.FullLink + '">' + Node.Name + '</A></LI>');

      Node := FClassHierarchy.NextItem(Node);
    end;

    while OldLevel >= 0 do begin
      WriteString('</UL>');
      Dec(OldLevel);
    end;
  end;

  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;

  CloseStream;
end;

procedure THTMLDocGenerator.LoadFooterFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FFooter);
end;

procedure THTMLDocGenerator.LoadHeaderFromFile(const AFileName: string);
begin
  LoadStrFromFileA(AFileName, FHeader);
end;

procedure THTMLDocGenerator.WriteUnitUses(const HL: Byte; U: TPasUnit);
var
  i: Integer;
  ULink: TPasItem;
begin
  if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
    WriteHeading(HL, 'uses');
    WriteString('<ul>');
    for i := 0 to U.UsesUnits.Count-1 do begin
      WriteString('<li>');
      ULink := FUnits.FindName(U.UsesUnits[i]);
      if ULink is TPasUnit then begin
        WriteStartOfLink(ULink.FullLink);
        WriteText(U.UsesUnits[i]);
        WriteEndOfLink;
      end else begin
        WriteText(U.UsesUnits[i]);
      end;
      WriteString('</li>');
    end;   
    WriteString('</ul>');
  end;
end;

procedure THTMLDocGenerator.WriteWithURLs(s: string);
var
  s1, s2, link: string;
begin
  while ExtractLink(s, s1, s2, link) do begin
    WriteSpellChecked(S1);
    WriteText('<a href="');
    WriteText(link);
    WriteText('" target="_new">');
    WriteText(link);
    WriteText('</a>');
    s := s2;
  end;
  WriteSpellChecked(s);
end;

procedure THTMLDocGenerator.WriteSpellChecked(const AString: string);
var
  LErrors: TObjectVector;
  i, temp: Integer;
  LString, s: string;
begin
  LErrors := TObjectVector.Create(True);
  CheckString(AString, LErrors);
  if LErrors.Count = 0 then begin
    WriteString(AString);
  end else begin
    // build s
    s := '';
    LString := AString;
    for i := LErrors.Count-1 downto 0 do begin
      // everything after the offending word
      temp := TSpellingError(LErrors.ObjectAt[i]).Offset+Length(TSpellingError(LErrors.ObjectAt[i]).Word) + 1;
      s := ( '">' + TSpellingError(LErrors.ObjectAt[i]).Word +  '</acronym>' + Copy(LString, temp, MaxInt)) + s; // insert into string
      if Length(TSpellingError(LErrors.ObjectAt[i]).Suggestions) > 0 then begin
        s := 'suggestions: '+TSpellingError(LErrors.ObjectAt[i]).Suggestions + s;
      end else begin
        s := 'no suggestions' + s;
      end;
      s := '<acronym style="#0000FF; border-bottom: 1px solid crimson" title="' + s;
      SetLength(LString, TSpellingError(LErrors.ObjectAt[i]).Offset);
    end;
    WriteString(LString);
    writestring(s);
  end;
  LErrors.Free;
end;

function THTMLDocGenerator.ParameterString(const ParamType,
  Param: string): string;
begin
  { TODO -cfixme -otwm :
    This should probably be something like
    <div type="parameter"> ... </div> to be used with CSS }
  Result := '<br>' + ParamType + ' <span class="parameter">' + Param + '</span>';
end;

procedure THTMLDocGenerator.WriteBinaryFiles;
begin
  CreateStream('automated.gif');
  CurrentStream.Write(img_automated[0], High(img_automated)+1);
  CloseStream;

  CreateStream('private.gif');
  CurrentStream.Write(img_private[0], High(img_private)+1);
  CloseStream;

  CreateStream('protected.gif');
  CurrentStream.Write(img_protected[0], High(img_protected)+1);
  CloseStream;

  CreateStream('public.gif');
  CurrentStream.Write(img_public[0], High(img_public)+1);
  CloseStream;

  CreateStream('published.gif');
  CurrentStream.Write(img_published[0], High(img_published)+1);
  CloseStream;

  if not FileExists(DestinationDirectory+ProjectName+'.css') then begin
    CreateStream(ProjectName + '.css');
    StreamUtils.WriteLine(CurrentStream, 'BODY { font-family:"Verdana","Arial"; }');
    StreamUtils.WriteLine(CurrentStream, 'span.parameter { color:blue; }');
    CloseStream;
  end;
end;

end.
