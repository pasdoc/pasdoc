{ @abstract(Provides HTML document generator object.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Alexander Lisnevsky (alisnevsky@yandex.ru))
  @author(Erwin Scheuch-Heilig (ScheuchHeilig@t-online.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Hendy Irawan (ceefour@gauldong.net))
  @author(Wim van der Vegt (wvd_vegt@knoware.nl))

  Implements an object to generate HTML documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenHtml;

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  Classes;

type
  { Colors for HTML elements. }
  THTMLColorIndex = (
    HTML_BACKGROUND,
    HTML_TEXT,
    HTML_LINK,
    HTML_VLINK,
    HTML_ALINK,
    HTML_TABLE_BACKGROUND,
    HTML_TABLE_TEXT);

type
  { @abstract(generates HTML documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in HTML (HyperText Markup Language) format.
    This type of output is well suited to be read with a web browser at the
    computer, as a reference manual that does not have to be printed.
    For printed output, use @link(Tex.TTexDocGenerator). }
  THTMLDocGenerator = class(TDocGenerator)
    { Contains Name of a file to read HtmlHelp Contents from.
      If empty, create default contents file. }
    ContentsFile: string;
    { If True, generate Html Help project files. }
    HtmlHelp: Boolean;
    { True if not to call HCC.exe if creating HtmlHelp output.
      Otherwise, PasDoc will look for HCC.exe in the registry and
      compile the project.  }
    NoHHC: Boolean;
    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; override;
    { Returns a link to an anchor within a document. HTML simply concatenates
      the strings with a "#" character between them. }
    function CreateLink(const Item: TPasItem): string; override;
    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;
    function ExistsFullPath(s: string): Boolean;
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: Byte; Authors: TStringVector);
    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: Byte; const CIO: TPasCio); override;
    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: Byte; c: TPasItems); override;
    procedure WriteCIOSummary(HL: Byte; c: TPasItems); override;
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string; const
      ItemLink: string);
    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: Byte; const Created, LastMod: string);
      override;
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    { Writes the beginning of the HTML document, including opening HTML element,
      a complete HEAD element and an opening BODY element.
      See @link(WriteEndOfDocument). }
    procedure WriteDocumentHeadline;
    { Writes an empty table cell, '&nbsp;'. }
    procedure WriteEmptyCell;

    procedure WriteEndOfCode; override;
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
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument Methods is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the FuncsProcs argument. }
    procedure WriteFuncsProcs(const HL: Byte; const Methods: Boolean; const
      FuncsProcs: TPasMethods); override;
    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(Level: Byte; const s: string); override;
    { Writes a Hireachy list - this is more useful than the simple class list }
    procedure WriteHierachy;
    { Reads the default HTML Images from the PasDoc executable and writes
      them to the Output Directory. Existing files will not be overwritten. }
    procedure WriteBinaryFiles; override;
    procedure WriteItemDescription(const AItem: TPasItem);
    { Writes the Item's DetailedDescription. If the Item also has Discription
      (extracted from @@abstract), this is written to a separate paragraph
      in front of the DetailedDescription. }
    procedure WriteItemDetailedDescription(const AItem: TPasItem);
    procedure WriteItems(HL: Byte; Heading: string; const Anchor: string;
      const i: TPasItems); override;
    procedure WriteOverviewFiles;
    procedure WriteParagraph(HL: Byte; s: string; t: string);
    procedure WriteProperties(HL: Byte; const p: TPasProperties); override;
    procedure WritePropertiesSummary(HL: Byte; p: TPasProperties);
    { Writes an opening A element, including a name attribute given by the
      argument. }
    procedure WriteStartOfAnchor(const Name: string);
    procedure WriteStartOfCode; override;
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

    procedure WriteUnit(const HL: Byte; const U: TPasUnit); override;
    procedure WriteUnitDescription(HL: Byte; U: TPasUnit); override;
    { Creates an output stream that lists up all units and short descriptions. }
    procedure WriteUnitOverviewFile;
    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);
  end;

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
  StringCardinalTree,
  ObjectVector,
  Types,
  Utils,
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
var
  i: Integer;
begin
  Result := '';
  if (not Assigned(Item)) then Exit;
  if Assigned(Item.MyUnit) then begin
    if Assigned(Item.MyObject) then begin
      { it's a method, a field or a property - only those have MyObject initialized }
      Result := Item.MyObject.FullLink + '#' + AnchorToString(Item.AnchorNumber);
    end
    else begin
      if Item.ClassType = TPasCio then begin
        { it's an object / a class }
        Result := Item.Name + GetFileExtension;
        i := 0;
        while ExistsFullPath(Result) do begin
          Inc(i);
          Result := Item.Name + IntToStr(i) + GetFileExtension;
        end;
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := Item.MyUnit.FullLink + '#' +
          AnchorToString(Item.AnchorNumber);
      end;
    end;
  end else begin
    { it's a unit - only units don't have a MyUnit pointer }
    Result := Item.Name + GetFileExtension;
  end;
end;

function THTMLDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result := CodeString('<A href="' + Link + '">' + ItemName + '</A>');
end;

function THTMLDocGenerator.ExistsFullPath(s: string): Boolean;
var
  i, j: Integer;
  CO: TPasCio;
  U: TPasUnit;
begin
  Result := False;

  if IsNilOrEmpty(Units) then Exit;

  for i := 0 to Units.Count - 1 do begin
    U := Units.UnitAt[i];
    Result := CompareText(U.FullLink, s) = 0;
    if Result then Exit;

    if not IsNilOrEmpty(U.CIOs) then begin
      for j := 0 to U.CIOs.Count - 1 do begin
        CO := TPasCio(U.CIOs.PasItemAt[j]);
        Result := CompareText(CO.FullLink, s) = 0;
        if Result then Exit;
      end;
    end;
  end;
end;

function THTMLDocGenerator.GetFileExtension: string;
begin
  { '.html' makes DOS version unhappy - welcome to the past ;-) }
  GetFileExtension :=
{$IFDEF OS_DOS}
  '.htm';
{$ELSE}
  '.html';
{$ENDIF}
end;

procedure THTMLDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  StreamUtils.WriteLine(Stream, '<HR noshade size=1><EM>' +
    Translation[trGeneratedBy] +
    ' <A href="' + PASDOC_HOMEPAGE + '">' +
    PASDOC_NAME_AND_VERSION + '</A> ' + Translation[trOnDateTime] + ' ' +
    FormatDateTime('ddd dd/ mmm yyyy hh:mm:ss', Now) + '</EM>');
end;

procedure THTMLDocGenerator.WriteAuthors(HL: Byte; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  EmailAddress: string;
begin
  if IsNilOrEmpty(Authors) then Exit;

  if (Authors.Count = 1) then
    WriteHeading(HL, Translation[trAuthor])
  else
    WriteHeading(HL, Translation[trAuthors]);

  for i := 0 to Authors.Count - 1 do begin
    s := Authors[i];
    WriteStartOfParagraph;

    if ExtractEmailAddress(s, S1, S2, EmailAddress) then begin
      WriteString(S1);
      StreamUtils.WriteString(Stream, '<A href="mailto:' + EmailAddress +
        '">');
      WriteString(EmailAddress);
      StreamUtils.WriteString(Stream, '</A>');
      WriteString(S2);
    end
    else begin
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

  case CIO.MyType of
    CIO_CLASS: s := Translation[trClass];
    CIO_SPINTERFACE: s := Translation[trDispInterface];
    CIO_INTERFACE: s := Translation[trInterface];
    CIO_OBJECT: s := Translation[trObject];
  else
    s := '';
  end;

  s := s + ' ' + CIO.Name;

  WriteStartOfDocument(CIO.MyUnit.Name + ': ' + s);
  if not HtmlHelp then WriteDocumentHeadline;

  WriteStartOfAnchor(AnchorToString(CIO.AnchorNumber));
  WriteEndOfAnchor;
  WriteHeading(HL, s);
  { write unit link }
  if Assigned(CIO.MyUnit) then begin
    WriteHeading(HL + 1, Translation[trUnit]);
    StreamUtils.WriteString(Stream, '<A href="' + CIO.MyUnit.FullLink + '">'
      + CIO.MyUnit.Name + '</A><BR>');
  end;

  { write declaration link }
  WriteHeading(HL + 1, Translation[trDeclaration]);
  StreamUtils.WriteString(Stream, '<P>');
  WriteStartOfCode;
  StreamUtils.WriteString(Stream, 'type ' + CIO.Name + ' = ');
  case CIO.MyType of
    CIO_CLASS: StreamUtils.WriteString(Stream, 'class');
    CIO_SPINTERFACE: StreamUtils.WriteString(Stream, 'dispinterface');
    CIO_INTERFACE: StreamUtils.WriteString(Stream, 'interface');
  else
    StreamUtils.WriteString(Stream, 'object');
  end;

  if not IsNilOrEmpty(CIO.Ancestors) then begin
    StreamUtils.WriteString(Stream, '(');
    for i := 0 to CIO.Ancestors.Count - 1 do begin
      s := CIO.Ancestors[i];
      s := SearchLink(s, CIO);
      StreamUtils.WriteString(Stream, s);
      if (i <> CIO.Ancestors.Count - 1) then
        StreamUtils.WriteString(Stream, ', ');
    end;
    StreamUtils.WriteString(Stream, ')');
  end;
  WriteEndOfCode;
  StreamUtils.WriteString(Stream, '</P>');

  { Write Description }
  WriteHeading(HL + 1, Translation[trDescription]);
  WriteItemDetailedDescription(CIO);

  { Write Hierarchy }
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then begin
    s := CIO.Ancestors.FirstName;
    Item := SearchItem(s, CIO);
    if Assigned(Item) and (Item is TPasCio) then begin
      WriteHeading(HL + 1, Translation[trHierarchy]);
      repeat
        s := CreateReferencedLink(Item.Name, Item.FullLink);
        StreamUtils.WriteString(Stream, s);

        if not IsNilOrEmpty(TPasCio(Item).Ancestors) then begin
          s := TPasCio(Item).Ancestors.FirstName;
          Item := SearchItem(s, Item);

          if (Item <> nil) and (Item is TPasCio) then begin
            StreamUtils.WriteString(Stream, '&nbsp;&gt; ');
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
      DoMessage(1, mtError,
        'Could not create Class/Interface/Object documentation file.', []);
      Continue;
    end;
    DoMessage(2, mtInformation, 'Creating Class/Interface/Object file for "'
      + p.OutputFileName + '"...', []);
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
  if IsNilOrEmpty(c) then Exit;

  if HtmlHelp then
    WriteString('<A name=Classes></A>');

  WriteHeading(HL, Translation[trCio]);
  WriteStartOfTable2Columns(Translation[trName], Translation[trDescription]);
  for j := 0 to c.Count - 1 do begin
    p := TPasCio(c.PasItemAt[j]);
    WriteStartOfTableRow;
      { name of class/interface/object and unit }
    WriteStartOfTableCell;
    WriteString(GetCIOTypeName(p.MyType));
    StreamUtils.WriteString(Stream, '&nbsp;');
    WriteStartOfLink(p.FullLink);
    WriteString(CodeString(p.Name));
    WriteEndOfLink;
    WriteEndOfTableCell;

      { Description of class/interface/object }
    if j = 0 then
      StreamUtils.WriteString(Stream, '<TD width=100%>')
    else
      WriteStartOfTableCell;
      { Write only the description and do not opt for DetailedDescription,
        like WriteItemDescription does. }
    if p.Description <> '' then
      WriteText(p.Description)
    else
      StreamUtils.WriteString(Stream, '&nbsp;');

    WriteEndOfTableCell;
    WriteEndOfTableRow;
  end;
  WriteEndOfTable;
end;

procedure THTMLDocGenerator.WriteCodeWithLinks(const p: TPasItem; const Code:
  string; const ItemLink: string);
var
  SearchForLink: Boolean;
  FoundItem: TPasItem;
  i, j, l: Integer;
  s: string;

  n, ncstart: Integer;
  S1: string;
  S2: string;
  S3: string;
begin
  WriteStartOfCode;
  i := 1;
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
          until (i > l) or (not (Code[i] in ['.', '_', '0'..'9', 'A'..'Z',
            'a'..'z']));
          s := Copy(Code, j, i - j);

          if s = p.Name then begin
            if ItemLink <> '' then begin
              WriteStartOfLink(ItemLink);
              WriteString('<B>' + s + '</B>');
              WriteEndOfLink;
            end else begin
              WriteString('<B>' + s + '</B>')
            end
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
              StreamUtils.WriteString(Stream, s);
              WriteEndOfLink;
            end else begin
              WriteString(s);
            end;
          end;
          ncstart := i;
          Continue; // We don't want to miss out on any ':' or ';'
        end;
      ':': SearchForLink := True;
      ';': SearchForLink := False;
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
    WriteHeading(HL, Translation[trCreated]);
    WriteStartOfParagraph;
    WriteString(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, Translation[trLastModified]);
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
  WriteUnits(1);
  WriteHierachy;
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
      PChar(DestDir + ProjectName + '.hhp'),
      '', SW_SHOW) <= 32) then
      DoMessage(1, mtError, 'Could not compile HtmlHelp.', []);
  end;
{$ENDIF}
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteDocumentHeadline;
var
  i: Byte;
begin
  StreamUtils.WriteLine(Stream, '<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  StreamUtils.WriteLine(Stream, '<TR bgcolor="' + HTML_HEADER_BACKGROUND_COLOR
    + '">');
  for i := 0 to NUM_OVERVIEW_FILES - 1 do begin
    StreamUtils.WriteString(Stream, '<TD><A href="' + OverviewFilenames[i] +
      GetFileExtension + '"><CENTER>');
    case i of
      0: WriteString(Translation[trUnits]);
      1: WriteString(Translation[trClassHierarchy]);
      2: WriteString(Translation[trCio]);
      3: WriteString(Translation[trTypes]);
      4: WriteString(Translation[trVariables]);
      5: WriteString(Translation[trConstants]);
      6: WriteString(Translation[trFunctionsAndProcedures]);
      7: WriteString(Translation[trIdentifiers]);
    end;
    StreamUtils.WriteLine(Stream, '</CENTER></A></TD>');
  end;
  StreamUtils.WriteLine(Stream, '</TR>');
  StreamUtils.WriteLine(Stream, '</TABLE>');
end;

procedure THTMLDocGenerator.WriteEmptyCell;
begin
  StreamUtils.WriteString(Stream, '&nbsp;');
end;

procedure THTMLDocGenerator.WriteEndOfDocument;
begin
  StreamUtils.WriteLine(Stream, '</BODY>');
  StreamUtils.WriteLine(Stream, '</HTML>');
end;

procedure THTMLDocGenerator.WriteEndOfAnchor;
begin
  StreamUtils.WriteString(Stream, '</A>');
end;

procedure THTMLDocGenerator.WriteEndOfCode;
begin
  StreamUtils.WriteString(Stream, '</CODE>');
end;

procedure THTMLDocGenerator.WriteEndOfLink;
begin
  StreamUtils.WriteString(Stream, '</A>');
end;

procedure THTMLDocGenerator.WriteEndOfParagraph;
begin
  StreamUtils.WriteLine(Stream, '</P>');
end;

procedure THTMLDocGenerator.WriteEndOfTableCell;
begin
  StreamUtils.WriteLine(Stream, '</TD>');
end;

procedure THTMLDocGenerator.WriteEndOfTable;
begin
  StreamUtils.WriteLine(Stream, '</TABLE>');
end;

procedure THTMLDocGenerator.WriteEndOfTableRow;
begin
  StreamUtils.WriteLine(Stream, '</TR>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFields(const Order: Byte; const Fields:
  TPasItems);
var
  j: Integer;
  Item: TPasItem;
begin
  if IsNilOrEmpty(Fields) then Exit;

  WriteString('<A name=Fields></A>');
  WriteHeading(Order, Translation[trFields]);

  StreamUtils.WriteString(Stream, '<TABLE cellspacing=' +
    HTML_TABLE_CELLSPACING + ' cellpadding=' + HTML_TABLE_CELLPADNG +
    ' width=100%>');
  StreamUtils.WriteString(Stream, '<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '">');
  StreamUtils.WriteLine(Stream, '<TH>&nbsp;</TH><TH>' + Translation[trName] +
    '</TH><TH>' + Translation[trDescription] + '</TH></TR>');

  for j := 0 to Fields.Count - 1 do begin
    Item := Fields.PasItemAt[j];
    WriteStartOfTableRow;

    WriteVisibilityCell(Item);

    WriteStartOfTableCell;
    WriteStartOfAnchor(AnchorToString(Item.AnchorNumber));
    WriteEndOfAnchor;
    WriteString(CodeString(Item.Name));
    WriteEndOfTableCell;

    if j = 0 then
      StreamUtils.WriteString(Stream, '<TD width=100%>')
    else
      WriteStartOfTableCell;

    WriteItemDetailedDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  StreamUtils.WriteString(Stream, '</TABLE>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteFooter;
begin
  Stream.Write(Footer[1], Length(Footer));
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
  if IsNilOrEmpty(FuncsProcs) then Exit;

  if Methods then begin
    if HtmlHelp then
      WriteString('<A name=Methods></A>');
    WriteHeading(HL, Translation[trMethods]);
  end
  else begin
    if HtmlHelp then
      WriteString('<A name=FuncsProcs></A>');
    WriteHeading(HL, Translation[trFunctionsAndProcedures]);
  end;

  FuncsProcs.SortByPasItemName;

  for i := 0 to 1 do begin
    if (i = 0) then begin
      WriteHeading(HL + 1, Translation[trOverview]);
      WriteStartOfTable1Column('');
    end
    else
      WriteHeading(HL + 1, Translation[trDescription]);

    for j := 0 to FuncsProcs.Count - 1 do begin
      p := TPasMethod(FuncsProcs.PasItemAt[j]);
      if (i = 0) then begin
        WriteStartOfTableRow;

              { Only write visibility for methods of classes and objects. }
        if Methods then WriteVisibilityCell(p);

        if j = 0 then
          StreamUtils.WriteString(Stream, '<TD width=100%>')
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

        StreamUtils.WriteString(Stream, '<TD width=100%>');
        WriteStartOfAnchor(AnchorToString(p.AnchorNumber));
        WriteEndOfAnchor;

              // s := StringReplace(s, p^.Name, '<B>' + p^.Name + '</B>', [rfIgnoreCase]);
        WriteCodeWithLinks(p, p.FullDeclaration, '');
              {StreamUtils.WriteString(Stream, '<code>');
              WriteString(P^.FullDeclaration);
              StreamUtils.WriteString(Stream, '</code>');}
        WriteEndOfTableCell;
        WriteEndOfTableRow;
        WriteEndOfTable;
              // RJ WriteEndOfParagraph;

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
    DoMessage(2, mtWarning,
      'HTML generator cannot write headlines of level 7 or greater; will use 6 instead.',
      []);
    Level := 6;
  end;
  c := IntToStr(Level);
  StreamUtils.WriteString(Stream, '<H' + c + '>');
  WriteString(s);
  StreamUtils.WriteLine(Stream, '</H' + c + '>');
end;

{ ---------- }

procedure THTMLDocGenerator.WriteBinaryFiles;
begin
  WriteResourceToFile('PRIVATE', RT_RCDATA, 'private.gif');
  WriteResourceToFile('PROTECTED', RT_RCDATA, 'protected.gif');
  WriteResourceToFile('PUBLIC', RT_RCDATA, 'public.gif');
  WriteResourceToFile('PUBLISHED', RT_RCDATA, 'published.gif');
  WriteResourceToFile('CSS', RT_RCDATA, ProjectName + '.css')
end;

{ ---------- }

procedure THTMLDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;

  if AItem.Description <> '' then
    WriteText(AItem.Description)
  else
    if AItem.DetailedDescription <> '' then
      WriteText(AItem.DetailedDescription)
    else
      StreamUtils.WriteString(Stream, '&nbsp;');
end;

procedure THTMLDocGenerator.WriteItemDetailedDescription(const AItem:
  TPasItem);
begin
  if not Assigned(AItem) then Exit;

  if AItem.Description <> '' then begin
    WriteText(AItem.Description);

    if AItem.DetailedDescription <> '' then begin
      StreamUtils.WriteString(Stream, '<P>');
      WriteText(AItem.DetailedDescription);
    end;
  end
  else
    if AItem.DetailedDescription <> '' then
      WriteText(AItem.DetailedDescription)
    else
      StreamUtils.WriteString(Stream, '&nbsp;');

end;

procedure THTMLDocGenerator.WriteItems(HL: Byte; Heading: string; const
  Anchor: string; const i: TPasItems);
var
  j: Integer;
  Item: TPasItem;
begin
  if IsNilOrEmpty(i) then Exit;

  if HtmlHelp and (Anchor <> '') then
    WriteString('<A name=' + Anchor + '></A>');

  WriteHeading(HL, Heading);

  StreamUtils.WriteString(Stream, '<TABLE cellspacing=' +
    HTML_TABLE_CELLSPACING + ' cellpadding=' + HTML_TABLE_CELLPADNG +
    ' width=100%>');
  StreamUtils.WriteString(Stream, '<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '">');
  StreamUtils.WriteLine(Stream, '<TH>' + Translation[trName] + '</TH><TH>' +
    Translation[trDescription] + '</TH></TR>');

  for j := 0 to i.Count - 1 do begin
    Item := i.PasItemAt[j];
    WriteStartOfTableRow;

    WriteStartOfTableCell;
    WriteStartOfAnchor(AnchorToString(Item.AnchorNumber));
    WriteEndOfAnchor;
    WriteString(Item.Name);
    WriteEndOfTableCell;

    if j = 0 then
      StreamUtils.WriteString(Stream, '<TD width=100%>')
    else
      WriteStartOfTableCell;
    WriteItemDetailedDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;
  StreamUtils.WriteString(Stream, '</TABLE>');
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

  if IsNilOrEmpty(Units) then Exit;

  // Make sure we don't free the Itmes when we free the container.
  TotalItems := TPasItems.Create(False);

  for i := 2 to 6 do begin
      // if (not CreateStream(OverviewFilenames[i]))
    if (not CreateStream(DestDir + OverviewFilenames[i] + GetFileExtension))
      then begin
      DoMessage(1, mtError, 'Error: Could not create output file "' +
        OverviewFilenames[i] + '".', []);
      Exit;
    end;
    DoMessage(3, mtInformation, 'Writing overview file ' +
      OverviewFilenames[i] + '...', []);

    case i of
      2: WriteStartOfDocument(Translation[trHeadlineCio]);
      3: WriteStartOfDocument(Translation[trHeadlineTypes]);
      4: WriteStartOfDocument(Translation[trHeadlineVariables]);
      5: WriteStartOfDocument(Translation[trHeadlineConstants]);
      6: WriteStartOfDocument(Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteStartOfDocument(Translation[trHeadlineIdentifiers]);
    end;

    if not HtmlHelp then WriteDocumentHeadline;

    case i of
      2: WriteHeading(1, Translation[trHeadlineCio]);
      3: WriteHeading(1, Translation[trHeadlineTypes]);
      4: WriteHeading(1, Translation[trHeadlineVariables]);
      5: WriteHeading(1, Translation[trHeadlineConstants]);
      6: WriteHeading(1, Translation[trHeadlineFunctionsAndProcedures]);
      7: WriteHeading(1, Translation[trHeadlineIdentifiers]);
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

    if not IsNilOrEmpty(PartialItems) then begin
      WriteStartOfTable3Columns(Translation[trName], Translation[trUnit],
        Translation[trDescription]);

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
          StreamUtils.WriteString(Stream, '<TD width=100%>')
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
      WriteString(Translation[trNone]);
      WriteEndOfParagraph;
    end;

    TotalItems.InsertItems(PartialItems);
    PartialItems.Free;
    WriteFooter;
    WriteAppInfo;
    WriteEndOfDocument;
    CloseStream;
  end;

  if not CreateStream(DestDir + OverviewFilenames[7] + GetFileExtension) then
    begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[7] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing overview file ' + OverviewFilenames[7]
    + '...', []);
  WriteStartOfDocument(Translation[trHeadlineIdentifiers]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, Translation[trHeadlineIdentifiers]);
  WriteStartOfTable3Columns(Translation[trName], Translation[trUnit],
    Translation[trDescription]);

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
      StreamUtils.WriteString(Stream, '<TD width=100%>')
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
  StreamUtils.WriteLine(Stream, '<P>');
  Stream.Write(t[1], Length(t));
  StreamUtils.WriteLine(Stream, '</P>');
end;

procedure THTMLDocGenerator.WriteProperties(HL: Byte; const p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
begin
  if IsNilOrEmpty(p) then Exit;

  WriteHeading(HL + 1, Translation[trDescription]);
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);

    WriteStartOfTable1Column('');
    WriteStartOfTableRow;

    WriteVisibilityCell(Prop);

    StreamUtils.WriteString(Stream, '<TD width=100%>');
    WriteStartOfAnchor(AnchorToString(Prop.AnchorNumber));
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
  if IsNilOrEmpty(p) then Exit;

  if HtmlHelp then
    WriteString('<A name=Properties></A>');

  WriteHeading(HL, Translation[trProperties]);
  WriteHeading(HL + 1, Translation[trOverview]);

  WriteStartOfTable1Column('');
  for j := 0 to p.Count - 1 do begin
    Prop := TPasProperty(p.PasItemAt[j]);
    WriteStartOfTableRow;

    WriteVisibilityCell(Prop);
    if j = 0 then
      StreamUtils.WriteString(Stream, '<TD width=100%>')
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
  StreamUtils.WriteString(Stream, '<A name="' + Name + '">');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfCode;
begin
  StreamUtils.WriteString(Stream, '<CODE>');
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteStartOfDocument(Name: string);
begin
  StreamUtils.WriteLine(Stream,
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">');
  StreamUtils.WriteLine(Stream, '<HTML>');
  StreamUtils.WriteLine(Stream, '<HEAD>');
  StreamUtils.WriteLine(Stream, '<META name="GENERATOR" content="' +
    PASDOC_NAME_AND_VERSION + '">');
  // Check if we need to specify character sets
  if LANGUAGE_ARRAY[Language].CharSet <> '' then
    StreamUtils.WriteLine(Stream,
      '<META http-equiv="content-type" content="text/html; charset=' +
      LANGUAGE_ARRAY[Language].CharSet + '">');
  // Title
  StreamUtils.WriteString(Stream, '<TITLE>');
  if {not HtmlHelp and}(Title <> '') then
    WriteString(Title + ': ');
  WriteString(Name);
  StreamUtils.WriteLine(Stream, '</TITLE>');
  // StyleSheet
  WriteString('<LINK rel="StyleSheet" href="');
  WriteString(ProjectName);
  StreamUtils.WriteLine(Stream, '.css">');

  StreamUtils.WriteLine(Stream, '</HEAD>');
  StreamUtils.WriteLine(Stream,
    '<BODY bgcolor="#ffffff" text="#000000" link="#0000ff" vlink="#800080" alink="#FF0000">');

  if Header <> '' then
    Stream.Write(Header[1], Length(Header));
end;

procedure THTMLDocGenerator.WriteStartOfLink(const Name: string);
begin
  StreamUtils.WriteString(Stream, '<A href="' + Name + '">');
end;

procedure THTMLDocGenerator.WriteStartOfParagraph;
begin
  StreamUtils.WriteString(Stream, '<P>');
end;

procedure THTMLDocGenerator.WriteStartOfTable1Column(t: string);
begin
  StreamUtils.WriteLine(Stream, '<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
end;

procedure THTMLDocGenerator.WriteStartOfTable2Columns(t1, t2: string);
begin
  StreamUtils.WriteLine(Stream, '<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  StreamUtils.WriteString(Stream, '<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '"><TH>');
  WriteString(t1);
  StreamUtils.WriteString(Stream, '</TH><TH>');
  WriteString(t2);
  StreamUtils.WriteLine(Stream, '</TH></TR>');
end;

procedure THTMLDocGenerator.WriteStartOfTable3Columns(t1, t2, T3: string);
begin
  StreamUtils.WriteLine(Stream, '<TABLE cellspacing=' + HTML_TABLE_CELLSPACING
    + ' cellpadding=' + HTML_TABLE_CELLPADNG + ' width=100%>');
  StreamUtils.WriteString(Stream, '<TR bgcolor="#' +
    HTML_HEADER_BACKGROUND_COLOR + '"><TH>');
  WriteString(t1);
  StreamUtils.WriteString(Stream, '</TH><TH>');
  WriteString(t2);
  StreamUtils.WriteString(Stream, '</TH><TH>');
  WriteString(T3);
  StreamUtils.WriteLine(Stream, '</TH></TR> ');
end;

procedure THTMLDocGenerator.WriteStartOfTableCell;
begin
  StreamUtils.WriteString(Stream, '<TD>');
end;

procedure THTMLDocGenerator.WriteStartOfTableRow;
begin
  StreamUtils.WriteString(Stream, '<TR bgcolor=#' + HTML_ROW_BACKGROUND_COLOR
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
    StreamUtils.WriteLine(Stream, '<LI><OBJECT type="text/sitemap">');
    WriteLine('<PARAM name="Name" value="' + Name + '">');
    if Local <> '' then begin
      WriteLine('<PARAM name="Local" value="' + Local + '">');
      if DefaultTopic = '' then
        DefaultTopic := Local;
    end;
    StreamUtils.WriteLine(Stream, '</OBJECT>');
  end;

  { ---------- }

  procedure WriteItemCollection(const c: TPasItems);
  var
    i: Integer;
    Item: TPasItem;
  begin
    if Assigned(c) then begin
      StreamUtils.WriteLine(Stream, '<UL>');
      for i := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[i];
        WriteLiObject(Item.Name, Item.FullLink);
      end;
      StreamUtils.WriteLine(Stream, '</UL>');
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
    StreamUtils.WriteLine(Stream, '<UL>');

    WriteItemHeadingCollection('Fields', ClassItem.FullLink + '#Fields',
      ClassItem.Fields);
    WriteItemHeadingCollection('Properties', ClassItem.FullLink +
      '#Properties', ClassItem.Properties);
    WriteItemHeadingCollection('Methods', ClassItem.FullLink + '#Methods',
      ClassItem.Methods);

    StreamUtils.WriteLine(Stream, '</UL>');
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
      WriteLiObject(Translation[trUnits], OverviewFilenames[0] +
        GetFileExtension);
    StreamUtils.WriteLine(Stream, '<UL>');

    // Iterate all Units
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      WriteLiObject(PU.Name, PU.FullLink);
      StreamUtils.WriteLine(Stream, '<UL>');

        // For each unit, write classes (if there are any).
      c := PU.CIOs;
      if Assigned(c) then begin
        WriteLiObject(Translation[trClasses], PU.FullLink + '#Classes');
        StreamUtils.WriteLine(Stream, '<UL>');

        for k := 0 to c.Count - 1 do
          InternalWriteCIO(TPasCio(c.PasItemAt[k]));

        StreamUtils.WriteLine(Stream, '</UL>');
      end;

        // For each unit, write Functions & Procedures.
      WriteItemHeadingCollection(Translation[trFunctionsAndProcedures],
        PU.FullLink + '#FuncsProcs', PU.FuncsProcs);
        // For each unit, write Types.
      WriteItemHeadingCollection(Translation[trTypes], PU.FullLink +
        '#Types', PU.Types);
        // For each unit, write Constants.
      WriteItemHeadingCollection(Translation[trConstants], PU.FullLink +
        '#Constants', PU.Constants);

      StreamUtils.WriteLine(Stream, '</UL>');
    end;
    StreamUtils.WriteLine(Stream, '</UL>');
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
      WriteLiObject(Translation[trClasses], OverviewFilenames[2] +
        GetFileExtension);
    StreamUtils.WriteLine(Stream, '<UL>');

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
    StreamUtils.WriteLine(Stream, '</UL>');
  end;

  { ---------- }

  procedure ContentWriteClassHierarchy(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilenames[1] + GetFileExtension)
    else
      WriteLiObject(Translation[trClassHierarchy], OverviewFilenames[1] +
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
      WriteLiObject(Translation[trOverview], '');
    StreamUtils.WriteLine(Stream, '<UL>');
    for j := 0 to NUM_OVERVIEW_FILES - 1 do begin
      StreamUtils.WriteLine(Stream, '<LI><OBJECT type="text/sitemap">');
      case j of
        0: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineUnits] + '">');
        1: WriteLine('<PARAM name="Name" value="' +
          Translation[trClassHierarchy] + '">');
        2: WriteLine('<PARAM name="Name" value="' + Translation[trHeadlineCio]
          + '">');
        3: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineTypes] + '">');
        4: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineVariables] + '">');
        5: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineConstants] + '">');
        6: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineFunctionsAndProcedures] + '">');
        7: WriteLine('<PARAM name="Name" value="' +
          Translation[trHeadlineIdentifiers] + '">');
      end;
      WriteLine('<PARAM name="Local" value="' + OverviewFilenames[j] +
        '.htm">');
      StreamUtils.WriteLine(Stream, '</OBJECT>');
    end;
    StreamUtils.WriteLine(Stream, '</UL>');
  end;

  { ---------- }

  procedure ContentWriteLegend(const Text: string);
  begin
    if Text <> '' then
      WriteLiObject(Text, 'Legend.htm')
    else
      WriteLiObject(Translation[trLegend], 'Legend.htm');
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
          Translation[trUnit], Item.FullLink)
      else
        WriteLiObject(Item.MyObject.Name, Item.FullLink);
    end
    else begin
      WriteLiObject(Item.MyUnit.Name + #32 + Translation[trUnit],
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

  if not CreateStream(DestDir + ProjectName + '.hhc') then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Content file "%s.hhc' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Content file "' + ProjectName
    + '"...', []);

  // File Header
  StreamUtils.WriteLine(Stream,
    '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  StreamUtils.WriteLine(Stream, '<HTML>');
  StreamUtils.WriteLine(Stream, '<HEAD>');
  StreamUtils.WriteLine(Stream, '<META name="GENERATOR" content="' +
    PASDOC_NAME_AND_VERSION + '">');
  StreamUtils.WriteLine(Stream, '</HEAD><BODY>');
  StreamUtils.WriteLine(Stream, '<UL>');

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
          StreamUtils.WriteLine(Stream, '<UL>');
          Inc(CurrentLevel);
          ContentWriteCustom(Text, Link)
        end
        else
          if CurrentLevel > Level then begin
            StreamUtils.WriteLine(Stream, '</UL>');
            Dec(CurrentLevel);
            while CurrentLevel > Level do begin
              StreamUtils.WriteLine(Stream, '</UL>');
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
    ContentWriteClassHierarchy(Translation[trClassHierarchy]);
    ContentWriteClasses('');
    ContentWriteOverview('');
    ContentWriteLegend('');
  end;

  // End of File
  StreamUtils.WriteLine(Stream, '</UL>');
  StreamUtils.WriteLine(Stream, '</BODY></HTML>');
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

  if not CreateStream(DestDir + ProjectName + '.hhk') then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Index file "%s.hhk' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing HtmlHelp Index file "%s"...',
    [ProjectName]);

  StreamUtils.WriteLine(Stream,
    '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  StreamUtils.WriteLine(Stream, '<HTML>');
  StreamUtils.WriteLine(Stream, '<HEAD>');
  WriteLine('<META name="GENERATOR" content="' + PASDOC_NAME_AND_VERSION +
    '">');
  StreamUtils.WriteLine(Stream, '</HEAD><BODY>');
  StreamUtils.WriteLine(Stream, '<UL>');

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
        StreamUtils.WriteLine(Stream, '<UL>');

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
        StreamUtils.WriteLine(Stream, '</UL>');
      end;

      Inc(j);
    end;

      // Don't forget to write the last item. Can it ever by nil?
    WriteLiObject(Item.Name, Item.FullLink);
  end;

  c.Free;

  StreamUtils.WriteLine(Stream, '</UL>');
  StreamUtils.WriteLine(Stream, '</BODY></HTML>');
  CloseStream;

  // Create a HTML Help Project File
  if (not CreateStream(DestDir + ProjectName + '.hhp')) then begin
    DoMessage(1, mtError, 'Could not create HtmlHelp Project file "%s.hhp' +
      '".', [ProjectName]);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing Html Help Project file "%s"...',
    [ProjectName]);

  StreamUtils.WriteLine(Stream, '[OPTIONS]');
  StreamUtils.WriteLine(Stream, 'Binary TOC=Yes');
  StreamUtils.WriteLine(Stream, 'Compatibility=1.1 or later');
  StreamUtils.WriteLine(Stream, 'Compiled file=' + ProjectName + '.chm');
  StreamUtils.WriteLine(Stream, 'Contents file=' + ProjectName + '.hhc');
  StreamUtils.WriteLine(Stream, 'Default Window=Default');
  StreamUtils.WriteLine(Stream, 'Default topic=' + DefaultTopic);
  StreamUtils.WriteLine(Stream, 'Display compile progress=Yes');
  StreamUtils.WriteLine(Stream, 'Error log file=' + ProjectName + '.log');
  StreamUtils.WriteLine(Stream, 'Full-text search=Yes');
  StreamUtils.WriteLine(Stream, 'Index file=' + ProjectName + '.hhk');
  if Title <> '' then
    StreamUtils.WriteLine(Stream, 'Title=' + Title)
  else
    StreamUtils.WriteLine(Stream, 'Title=' + ProjectName);

  StreamUtils.WriteLine(Stream, '');
  StreamUtils.WriteLine(Stream, '[WINDOWS]');
  if Title <> '' then
    StreamUtils.WriteLine(Stream, 'Default="' + Title + '","' + ProjectName +
      '.hhc","' + ProjectName + '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0')
  else
    StreamUtils.WriteLine(Stream, 'Default="' + ProjectName + '","' +
      ProjectName + '.hhc","' + ProjectName +
      '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0');

  StreamUtils.WriteLine(Stream, '');
  StreamUtils.WriteLine(Stream, '[FILES]');

  { HHC seems to know about the files by reading the Content and Index.
    So there is no need to specify them in the FILES section.

  StreamUtils.WriteLine(Stream, 'Legend.htm');
  for k := 0 to NUM_OVERVIEW_FILES - 1 do
    StreamUtils.WriteLine(Stream, OverviewFilenames[k] + '.htm');

  if Assigned(Units) then
    for k := 0 to units.Count - 1 do
      begin
        Item := units.PasItemAt[k);
        PU := units.PasItemAt[k);
        StreamUtils.WriteLine(Stream, Item.FullLink);
        c := PU.CIO;
        if Assigned(c) then
          for l := 0 to c.Count - 1 do
            begin
              Item2 := c.PasItemAt[l);
              StreamUtils.WriteLine(Stream, Item2^.FullLink);
            end;
      end;}

  StreamUtils.WriteLine(Stream, '');

  StreamUtils.WriteLine(Stream, '[INFOTYPES]');

  StreamUtils.WriteLine(Stream, '');

  StreamUtils.WriteLine(Stream, '[MERGE FILES]');

  CloseStream;

  // Create a Main Topic
  if (not CreateStream(DestDir + 'Legend.htm')) then begin
    DoMessage(1, mtError, 'Could not create file "Legend.htm".', []);
    Exit;
  end;
  DoMessage(2, mtInformation, 'Writing Legend.htm...', []);

  WriteStartOfDocument('Legend');
  WriteHeading(1, 'Legend');
  WriteString('<TABLE cellpadding=5>');
  WriteString('<TR><TD><IMG src="private.gif" alt="' + Translation[trPrivate]
    + '"></TD><TD>' + Translation[trPrivate] + '</TD></TR>');
  WriteString('<TR><TD><IMG src="protected.gif" alt="' +
    Translation[trProtected] + '"></TD><TD>' + Translation[trProtected] +
    '</TD></TR>');
  WriteString('<TR><TD><IMG src="public.gif" alt="' + Translation[trPublic] +
    '"></TD><TD>' + Translation[trPublic] + '</TD></TR>');
  WriteString('<TR><TD><IMG src="published.gif" alt="' +
    Translation[trPublished] + '"></TD><TD>' + Translation[trPublished] +
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
  WriteHeading(HL, Translation[trUnit] + ' ' + U.Name);

  WriteUnitDescription(HL + 1, U);
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
  WriteHeading(HL, Translation[trDescription]);
  WriteItemDetailedDescription(U);
end;

procedure THTMLDocGenerator.WriteUnitOverviewFile;
var
  c: TPasItems;
  Item: TPasItem;
  j: Integer;
begin
  c := Units;
  if (not CreateStream(DestDir + OverviewFilenames[0] + GetFileExtension))
    then begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[0] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing unit overview file "%s" ...',
    [OverviewFilenames[0]]);
  WriteStartOfDocument(Translation[trHeadlineUnits]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, Translation[trHeadlineUnits]);
  if Assigned(c) and (c.Count > 0) then begin
    WriteStartOfTable2Columns(Translation[trName],
      Translation[trDescription]);
    for j := 0 to c.Count - 1 do begin
      Item := c.PasItemAt[j];
      WriteStartOfTableRow;
      WriteStartOfTableCell;
      WriteStartOfLink(Item.FullLink);
      WriteString(Item.Name);
      WriteEndOfLink;
      WriteEndOfTableCell;

      if j = 0 then
        StreamUtils.WriteString(Stream, '<TD width=100%>')
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
      StreamUtils.WriteString(Stream, '<IMG src="private.gif" alt="' +
        Translation[trPrivate] + '">');
    STATE_PROTECTED:
      StreamUtils.WriteString(Stream, '<IMG src="protected.gif" alt="' +
        Translation[trProtected] + '">');
    STATE_PUBLIC:
      StreamUtils.WriteString(Stream, '<IMG src="public.gif" alt="' +
        Translation[trPublic] + '">');
    STATE_PUBLISHED:
      StreamUtils.WriteString(Stream, '<IMG src="published.gif" alt="' +
        Translation[trPublished] + '">');
  end;
  WriteEndOfTableCell;
end;

{ ---------------------------------------------------------------------------- }

procedure THTMLDocGenerator.WriteHierachy;
var
  unitLoop: Integer;
  classLoop: Integer;
  PU: TPasUnit;
  ACIO: TPasCio;
  ParentName: string;
  Tree: TStringCardinalTree;
  Level, OldLevel: Integer;
  Node, Parent, Child: TStringCardinalTreeNode;
begin
  { First build an internal tree of the class hierarchy.
    This is necessary since we don't know yet which class is a parent of which. }
  Tree := NewStringCardinalTree;
  for unitLoop := 0 to Units.Count - 1 do begin
    PU := Units.UnitAt[unitLoop];
    if PU.CIOs = nil then Continue;
    for classLoop := 0 to PU.CIOs.Count - 1 do begin
      ACIO := TPasCio(PU.CIOs.PasItemAt[classLoop]);

      if Assigned(ACIO.Ancestors) and (ACIO.Ancestors.Count > 0) then begin
        ParentName := ACIO.Ancestors.FirstName;
        Parent := Tree.PItemOfNameCI(ParentName);
              // Add parent if not already there.
        if Parent = nil then
          Parent := Tree.InsertNameLast(ParentName);
      end
      else
        Parent := nil;

      Child := Tree.PItemOfNameCI(ACIO.Name);
      if Child = nil then
        Tree.InsertNameNumberChildLast(Parent, ACIO.Name, 1)
      else begin
        if Parent <> nil then
          Tree.MoveChildLast(Child, Parent);
        Child.Number := 1;
      end;
    end;
  end;

  if not CreateStream(DestDir + OverviewFilenames[1] + GetFileExtension) then
    begin
    DoMessage(1, mtError, 'Could not create output file "%s".',
      [OverviewFilenames[1] + GetFileExtension]);
    Exit;
  end;

  WriteStartOfDocument(Translation[trClassHierarchy]);
  if not HtmlHelp then WriteDocumentHeadline;
  WriteHeading(1, Translation[trClassHierarchy]);

  if Tree.IsEmpty then begin
    WriteStartOfParagraph;
    WriteString(Translation[trNone]);
    WriteEndOfParagraph;
  end
  else begin
    Tree.SortRecurseByNameCI; // Make sure we sort the output.
    OldLevel := -1;
    Node := Tree.PFirstItem;
    while Node <> nil do begin
      Level := Tree.Level(Node);
      if Level > OldLevel then
        WriteString('<UL>')
      else
        while Level < OldLevel do begin
          WriteString('</UL>');
          Dec(OldLevel);
        end;
      OldLevel := Level;

      if Node.Number = 0 then
        WriteString('<LI>' + Node.Name + '</LI>')
      else
        WriteString('<LI><A href="' + Node.Name + GetFileExtension + '">' +
          Node.Name + '</A></LI>');

      Node := Tree.PNextItem(Node);
    end;

    while OldLevel >= 0 do begin
      WriteString('</UL>');
      Dec(OldLevel);
    end;
  end;

  Tree.Free;

  WriteFooter;
  WriteAppInfo;
  WriteEndOfDocument;

  CloseStream;
end;

end.
