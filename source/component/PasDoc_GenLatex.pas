{ @abstract(Provides Latex document generator object.)
  @cvs($Date$)

  Implements an object to generate latex documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenLatex;

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  StringVector,
  PasDoc_Types,
  Classes;

type
  { @abstract(generates latex documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in LaTex format. }
  TTexDocGenerator = class(TDocGenerator)
  protected
    FNumericFilenames: boolean;
    FWriteUses: boolean;
    FLinkCount: Integer;
    FFooter: string;
    FHeader: string;
    FOddTableRow: Integer;
    { number of cells (= columns) per table row }
    NumCells: Integer;
    { number of cells we've already written in current table row }
    CellCounter: LongInt;
    { Indicate if the output must be simplified for latex2rtf }
    FLatex2Rtf: Boolean;
    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: integer; Authors: TStringVector);
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string; const
      ItemLink: string);
    { Writes an empty table cell, '&nbsp;'. }
    procedure WriteEmptyCell;

    procedure WriteEndOfDocument;
    { Finishes an HTML paragraph element by writing a closing P tag. }
    procedure WriteEndOfParagraph;
    { Finishes an HTML table cell by writing a closing TD tag. }
    procedure WriteEndOfTableCell;
    { Finishes an HTML table by writing a closing TABLE tag. }
    procedure WriteEndOfTable;
    { Finishes an HTML table row by writing a closing TR tag. }
    procedure WriteEndOfTableRow;
    procedure WriteFields(const Order: integer; const Fields: TPasItems);
    procedure WriteFooter;
    procedure WriteItemDescription(const AItem: TPasItem);
    { Writes the Item's DetailedDescription. If the Item also has Discription
      (extracted from @@abstract), this is written to a separate paragraph
      in front of the DetailedDescription. }
    procedure WriteItemDetailedDescription(const AItem: TPasItem);
    procedure WriteOverviewFiles;
    procedure WritePropertiesSummary(HL: integer; p: TPasProperties);

    { Writes an opening A element, including a name attribute given by the
      argument. }
    procedure WriteStartOfDocument(AName: string);

    { Starts an HTML paragraph element by writing an opening P tag. }
    procedure WriteStartOfParagraph;

    procedure WriteStartOfTableCell; overload;
    procedure WriteStartOfTableCell(const css: string); overload;
    procedure WriteStartOfTableCell(const Params, css: string); overload;

    procedure WriteStartOfTable1Column(t: string);
    procedure WriteStartOfTable2Columns(t1, t2: string);
    procedure WriteStartOfTable3Columns(t1, t2, T3: string);
    procedure WriteStartOfTableRow(const CssClass: string);

    { Creates an output stream that lists up all units and short descriptions. }
    procedure WriteUnitOverviewFile;
    { Writes a cell into a table row with the Item's visibility image. }
    procedure WriteVisibilityCell(const Item: TPasItem);

    function ConvertString(const s: string): string; override;
    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): String; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
    procedure WriteUnitUses(const HL: integer; U: TPasUnit);
    procedure WriteUnitDescription(HL: integer; U: TPasUnit); override;
    procedure WriteProperties(HL: integer; const p: TPasProperties); override;

    procedure WriteSpellChecked(const AString: string);

    procedure WriteWithURLs(s: string);
    
    function LatexString(const S: string): string; override;

    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; override;
    { Returns a link to an anchor within a document. HTML simply concatenates
      the strings with a "-" character between them. }
    function CreateLink(const Item: TPasItem): string; override;
    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;
    { Returns HTML file extension ".htm". }
    function GetFileExtension: string; override;
    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio); override;
    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems); override;
    procedure WriteCIOSummary(HL: integer; c: TPasItems); override;
    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string); override;
    procedure WriteStartOfCode; override;
    procedure WriteItems(HL: integer; Heading: string; const Anchor: string;
      const i: TPasItems); override;
    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(Level: integer; const s: string); override;

    procedure WriteEndOfCode; override;
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument Methods is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the FuncsProcs argument. }
    procedure WriteFuncsProcs(const HL: integer; const Methods: Boolean; const FuncsProcs: TPasMethods); override;
      
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument Methods is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the FuncsProcs argument. }
    procedure WriteMethods(const HL: integer; const FuncsProcs: TPasMethods); 
    procedure WriteMethodsSummary(const HL: integer; const FuncsProcs: TPasMethods); 
    procedure WriteFuncsProcsSummary(const HL: integer; const FuncsProcs: TPasMethods);

    procedure WriteImage(const src, alt, css: string);
    procedure WriteLink(const href, caption, css: string);
    procedure WriteAnchor(ItemName, Link: string);
    
    function InsertParagraphs(const S: string): string; override;
  public
    function FormatPascalCode(const Line: string): string; override;

    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure BuildLinks; override;

    function EscapeURL(const AString: string): string; virtual;
  published
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses;
    property Latex2rtf: boolean read FLatex2rtf write FLatex2rtf;
    
   private
    procedure WriteParameter(const ParamName: string; const Desc: string);
    procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
      List: TStringVector; LinkToParamNames: boolean);
    procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
    { PDF Conditional support routines }
    procedure WriteStartFlushLeft;
    procedure WritePDFIfdef;
    procedure WriteEndFlushLeft; 
    procedure WritePDFDocInfo(LocalTitle: string); 
    procedure WriteStartList(s: string);
    procedure WriteEndList;
    function HasDescriptions(c: TPasItems):boolean;
    procedure WriteDeclarationItem(p: TPasItem; itemname: string; itemdesc: string);
    {** Returns @true if this item or its ancestor has a description, otherwise
        returns @false.
    }    
    function HasDescription(const AItem: TPasItem): boolean;
  end;

{$I automated.inc}
{$I private.inc}
{$I public.inc}
{$I published.inc}
{$I protected.inc}

const
  { HTML table padding inside each cell. }
  HTML_TABLE_CELLPADNG = '4';
  { HTML table spacing between cells. }
  HTML_TABLE_CELLSPACING = '2';

implementation

uses
  SysUtils,
  PasDoc,
  ObjectVector,
  Utils;

function TTexDocGenerator.LatexString(const S: string): string;
begin
  Result := S;
end;

function TTexDocGenerator.FormatPascalCode(const Line: string): string;
begin
  result := '\begin{verbatim}' + inherited FormatPascalCode(Line) + '\end{verbatim}';
end;

function TTexDocGenerator.CodeString(const s: string): string;
begin
  Result := '\begin{ttfamily}' + s + '\end{ttfamily}';
end;

function TTexDocGenerator.CreateLink(const Item: TPasItem): string;
  function NewLink(const AFullName: string): string;
  begin
    if NumericFilenames then begin
      Result := Format('%.8d', [FLinkCount]) + GetFileExtension;
      Inc(FLinkCount);
    end else begin
      Result := AFullName + GetFileExtension;
    end;
  end;

begin
  Result := '';
  if (not Assigned(Item)) then Exit;
  if Assigned(Item.MyUnit) then begin
    if Assigned(Item.MyObject) then begin
      { it's a method, a field or a property - only those have MyObject initialized }
      Result := Item.MyObject.FullLink + '-' + Item.Name;
    end else begin
      if Item.ClassType = TPasCio then begin
        { it's an object / a class }
        Result := NewLink(Item.MyUnit.Name + '.' + Item.Name);
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := Item.MyUnit.FullLink + '-' + Item.Name;
      end;
    end;
  end else begin
    { it's a unit - only units don't have a MyUnit pointer }
    Result := NewLink(Item.Name);
  end;
end;

function TTexDocGenerator.CreateReferencedLink(ItemName, Link: string):
  string;
begin
  Result :=  '\begin{ttfamily}'+ConvertString(ItemName) +'\end{ttfamily}(\ref{' + 
     EscapeURL(Link) + '})'; 
end;

function TTexDocGenerator.GetFileExtension: string;
begin
  Result := '.tex';
end;

procedure TTexDocGenerator.WriteAppInfo;
begin
  { check if user does not want a link to the pasdoc homepage }
  if NoGeneratorInfo then Exit;
  { write a horizontal line, pasdoc version and a link to the pasdoc homepage }
  WriteDirect('% '+FLanguage.Translation[trGeneratedBy] + ' ');
  WriteDirect(PASDOC_HOMEPAGE+ PASDOC_NAME_AND_VERSION);
  WriteDirect(' ' + FLanguage.Translation[trOnDateTime] + ' ' +
    FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
  WriteDirect('', true);
end;

procedure TTexDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
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
      WriteConverted(S1);
      WriteConverted(EmailAddress);
      WriteConverted(S2);
    end else begin
      WriteConverted(s);
    end;

    WriteEndOfParagraph;
  end;
end;


{ Returns TRUE if one of the subentries has a description
  otherwise returns FALSE
}  
function TTexDocGenerator.HasDescriptions(c: TPasItems):boolean;
var j :integer;
    Item: TPasItem;
begin
  HasDescriptions := false;
  for j := 0 to c.Count - 1 do 
    begin
      Item := TPasItem(c.PasItemAt[j]);
      if HasDescription(Item) then
        begin
          HasDescriptions:=true;
          exit;
        end;
    end;
end;


procedure TTexDocGenerator.WriteCIO(HL: integer; const CIO: TPasCio);
type
  TSections = (dsDescription, dsHierarchy, dsFields, dsMethods, dsProperties);
  TSectionSet = set of TSections;
var
  s: string;
  Item: TPasItem;
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;
begin
  if not Assigned(CIO) then Exit;
  
  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsHierarchy] := FLanguage.Translation[trHierarchy];
  SectionHeads[dsFields ]:= FLanguage.Translation[trFields];
  SectionHeads[dsMethods ]:= FLanguage.Translation[trMethods];
  SectionHeads[dsProperties ]:= FLanguage.Translation[trProperties];

  SectionsAvailable := [];
  if HasDescription(CIO) then
    Include(SectionsAvailable, dsDescription);
  if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then
    Include(SectionsAvailable, dsHierarchy);
  if not ObjectVectorIsNilOrEmpty(CIO.Fields) then
    Include(SectionsAvailable, dsFields);
  if not ObjectVectorIsNilOrEmpty(CIO.Methods) then
    Include(SectionsAvailable, dsMethods);
  if not ObjectVectorIsNilOrEmpty(CIO.Properties) then
    Include(SectionsAvailable, dsProperties);

  if SectionsAvailable = [] then exit;

  CIO.SortPasItems;
  

  WriteHeading(HL+1,CIO.Name+' '+ConvertString(GETCIOTypeName(CIO.MyType)));
  WriteAnchor(CIO.Name,CIO.FullLink);

  if dsHierarchy in SectionsAvailable then
    begin
      { Write Hierarchy }
      if Assigned(CIO.Ancestors) and (CIO.Ancestors.Count > 0) then 
        begin
          WriteHeading(HL + 2, SectionHeads[dsHierarchy]);

          WriteConverted(CIO.Name);
          WriteConverted(' > ');
          s := CIO.Ancestors.FirstName;
          Item := SearchItem(s, CIO);
          if Assigned(Item) and (Item is TPasCio) then 
            begin
              repeat
                s := CreateReferencedLink(Item.Name, Item.FullLink);
                WriteDirect(s);

                if not StringVectorIsNilOrEmpty(TPasCio(Item).Ancestors) then 
                  begin
                    s := TPasCio(Item).Ancestors.FirstName;
                    Item := SearchItem(s, Item);

                    WriteConverted(' > ');
                    if (Item <> nil) and (Item is TPasCio) then 
                      begin
                        Continue;
                      end;
                   end;
                 WriteDirect('',true);
                 Break;
                until False;
             end;
             if Item = nil then 
              begin
                WriteDirect(s,true);
              end;
          end;
      end;

  if dsDescription in SectionsAvailable then
    begin
      WriteHeading(HL + 2, SectionHeads[dsDescription]);
      WriteItemDetailedDescription(CIO);
    end
  else
      WriteDirect('%%%%' + SectionHeads[dsDescription],true);
    
        
  if (CIO.MyType in [CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE]) then 
    WriteProperties(HL + 2, CIO.Properties);
        
  WriteFields(HL + 2, CIO.Fields);

  WriteMethods(HL + 2, CIO.Methods);
        

  WriteAuthors(HL + 2, CIO.Authors);
  WriteDates(HL + 2, CIO.Created, CIO.LastMod);
  
end;

procedure TTexDocGenerator.WriteCIOs(HL: integer; c: TPasItems);
var
  j: Integer;
  CIO: TPasCio;
begin
  if c = nil then Exit;
  if c.Count = 0 then Exit;

  WriteHeading(HL, FLanguage.Translation[trCio]);

  for j := 0 to c.Count - 1 do 
    begin
      CIO := TPasCio(c.PasItemAt[j]);
      WriteCIO(HL,CIO);
    end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteCIOSummary(HL: integer; c: TPasItems);
var
  j: Integer;
  CIO: TPasCio;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;
  if c.Count = 0 then exit;
  
  WriteDirect('\begin{description}',true);
  
  for j := 0 to c.Count - 1 do 
    begin
      CIO := TPasCio(c.PasItemAt[j]);
      WriteDirect('\item[\texttt{');
      WriteLink(CIO.FullLink, CodeString(ConvertString(CIO.Name)), 'bold');
      { name of class/interface/object and unit }
      WriteDirect(' ');
      WriteConverted(GETCIOTypeName(CIO.MyType));
      WriteDirect('}]');

      { Write only the description and do not opt for DetailedDescription,
        like WriteItemDescription does. }
      if CIO.Description <> '' then
        WriteWithURLs(CIO.Description);
      WriteDirect('',true);
    end;
  WriteDirect('\end{description}',true);
end;

procedure TTexDocGenerator.WriteCodeWithLinks(const p: TPasItem; const Code:
  string; const ItemLink: string);
begin
  WriteCodeWithLinksCommon(p, Code, ItemLink, '', '', 
    {$ifdef FPC}@{$endif} WriteLink);
  WriteDirect('',true);
end;

{ ---------------------------------------------------------------------------- }
{                             PDF SUPPORT ROUTINES                             }
{ ---------------------------------------------------------------------------- }
procedure TTexDocGenerator.WriteStartFlushLeft;
  begin
    if not FLatex2rtf then
      begin
        WriteDirect('\ifpdf',true);
        WriteDirect('\begin{flushleft}',true);
        WriteDirect('\fi',true);
      end
     else
        WriteDirect('\begin{flushleft}',true);
  end;
  
procedure TTexDocGenerator.WritePDFIfdef;
  begin
    { PDF output support, create ifpdf macro 
      to be able to support extended PDF features.
    }
    if not FLatex2Rtf then
      begin
        WriteDirect('',true);
        WriteDirect('% Conditional define to determine if pdf output is used',true);
        WriteDirect('\newif\ifpdf',true);
        WriteDirect('\ifx\pdfoutput\undefined',true);
        WriteDirect('\pdffalse',true);
        WriteDirect('\else',true);
        WriteDirect('\pdfoutput=1',true);
        WriteDirect('\pdftrue',true);
        WriteDirect('\fi',true);
        WriteDirect('',true);
      end;
  end;    
  
 
 
procedure TTexDocGenerator.WriteEndFlushLeft; 
  begin
    if not FLatex2Rtf then
      begin
        WriteDirect('\ifpdf',true);
        WriteDirect('\end{flushleft}',true);
        WriteDirect('\fi',true);
      end
     else
        WriteDirect('\end{flushleft}',true);
end;
 
procedure TTexDocGenerator.WritePDFDocInfo(Localtitle: string); 
  begin
    if not FLatex2RTF then
      begin
        WriteDirect('',true);
        WriteDirect('% Write Document information for pdflatex/pdftex',true);
        WriteDirect('\ifpdf',true);
        WriteDirect('\pdfinfo{',true);
        WriteDirect(' /Author     (Pasdoc)',true);
        WriteDirect(' /Title      ('+LocalTitle+')',true);
        WriteDirect(' /CreationDate ('+FormatDateTime('yyyymmddhhmmss', Now)+')',true);
        WriteDirect('}',true);
        WriteDirect('\fi',true);
        WriteDirect('',true);
      end;
 end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteDates(const HL: integer; const Created,
  LastMod: string);
begin
  if Created <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trCreated]);
    WriteStartOfParagraph;
    WriteConverted(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteConverted(LastMod, true);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteDocumentation;
var
 OutputFileName: string;
begin
{  StartSpellChecking('sgml');}
  inherited;

  if ProjectName <> '' then
    OutputFileName := ProjectName + '.tex'
  else
    OutputFileName := 'docs.tex';
  case CreateStream(OutputFileName, true) of
    csError: begin
      DoMessage(1, mtError, 'Could not create doc file %s',[Outputfilename]);
      Exit;
    end;
    csExisted: begin
      Exit;
    end;
  end;
  WriteStartOfDocument('');
  WriteUnits(1);
  WriteEndOfDocument;
  CloseStream;
{  EndSpellChecking;}
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteEmptyCell;
begin
end;

procedure TTexDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('\end{document}',true);
end;

procedure TTexDocGenerator.WriteEndOfCode;
begin
  WriteDirect('\end{ttfamily}',true);
end;

procedure TTexDocGenerator.WriteLink(const href, caption, css: string);
begin
  WriteDirect(caption);
end;

procedure TTexDocGenerator.WriteEndOfParagraph;
begin
  WriteDirect('', true);
  WriteDirect('',true);
end;

procedure TTexDocGenerator.WriteEndOfTableCell;
begin
  Inc(CellCounter);
  if (CellCounter < NumCells)
  then WriteDirect(' & ');
end;

procedure TTexDocGenerator.WriteEndOfTable;
begin
 WriteDirect('\end{tabular}',true);
end;

procedure TTexDocGenerator.WriteEndOfTableRow;
begin
   WriteDirect('\\',true);
end;
{ ---------------------------------------------------------------------------- }
procedure TTexDocGenerator.WriteStartList(s: string);
begin
  if FLatex2rtf then
    begin
      WriteDirect('\begin{list}{}{',true);
      WriteDirect('\settowidth{\tmplength}{\textbf{'+convertstring(s)+'}}',true);
      WriteDirect('\setlength{\itemindent}{0cm}',true);
      WriteDirect('\setlength{\listparindent}{0cm}',true);
      WriteDirect('\setlength{\leftmargin}{\evensidemargin}',true);
      WriteDirect('\addtolength{\leftmargin}{\tmplength}',true);
      WriteDirect('\settowidth{\labelsep}{X}',true);
      WriteDirect('\addtolength{\leftmargin}{\labelsep}',true);
      WriteDirect('\setlength{\labelwidth}{\tmplength}',true);
      WriteDirect('}',true);
    end
  else
    begin
      WriteDirect('\begin{list}{}{',true);
      WriteDirect('\settowidth{\tmplength}{\textbf{'+convertstring(s)+'}}',true);
      WriteDirect('\setlength{\itemindent}{0cm}',true);
      WriteDirect('\setlength{\listparindent}{0cm}',true);
      WriteDirect('\setlength{\leftmargin}{\evensidemargin}',true);
      WriteDirect('\addtolength{\leftmargin}{\tmplength}',true);
      WriteDirect('\settowidth{\labelsep}{X}',true);
      WriteDirect('\addtolength{\leftmargin}{\labelsep}',true);
      WriteDirect('\setlength{\labelwidth}{\tmplength}',true);
      WriteDirect('}',true);
    end;
end;

procedure TTexDocGenerator.WriteEndList;
begin
  WriteDirect('\end{list}',true);
end;

procedure TTexDocGenerator.WriteDeclarationItem(p: TPasItem; itemname: string; itemdesc: string);
begin
  if FLatex2rtf then
    begin
      WriteStartFlushLeft;
      WriteDirect('\item[\textbf{'+convertstring(itemname)+'}\hfill]',true);
      WriteCodeWithLinks(p, itemdesc, '');
      WriteDirect('',true);
      WriteEndFlushLeft;
    end
  else
    begin
      WriteDirect('\item[\textbf{'+convertstring(itemname)+'}\hfill]',true);
      WriteStartFlushLeft;
      WriteCodeWithLinks(p, itemdesc, '');
      WriteEndFlushLeft;
      WriteDirect('',true);
    end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFields(const Order: integer; const Fields:
  TPasItems);
var
  j: Integer;
  Item: TPasItem;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(Fields) then Exit;
  
  WriteHeading(Order, FLanguage.Translation[trFields]);
  
  if FLatex2Rtf then
    begin
    { Determine the longest string used.
      This is the one we will use for determining the label width.
    }
    s:=FLanguage.Translation[trDescription];
    if length(s) < length(FLanguage.Translation[trDeclaration])  then
       s:= FLanguage.Translation[trDeclaration];
    if length(s) < length(FLanguage.Translation[trReturns])  then
       s:=FLanguage.Translation[trReturns];
    if length(s) < length(FLanguage.Translation[trParameters])  then
       s:=FLanguage.Translation[trParameters];
    if length(s) < length(FLanguage.Translation[trExceptions])  then
       s:=FLanguage.Translation[trExceptions];

    for j := 0 to Fields.Count - 1 do 
      begin
        Item := Fields.PasItemAt[j];

        WriteHeading(Order+1, Item.Name);
        WriteAnchor(Item.Name,Item.FullLink);
        WriteStartList(s);
        
        if Item is TPasVarConst then 
        begin
          WriteDeclarationItem(Item,FLanguage.Translation[trDeclaration],
            AccessibilityStr[Item.State]+' '+TPasVarConst(Item).FullDeclaration);
        end
        else
          WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration], 
          AccessibilityStr[Item.State]+' '+Item.name);
          
        if HasDescription(Item) then
        begin
          WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
          WriteStartOfParagraph;
          WriteItemDetailedDescription(Item);
          WriteEndOfParagraph;
        end;

        WriteEndList;
      end;
    end
   else
  begin  
    { get the longest string for names }
    s:='';
    for j := 0 to Fields.Count - 1 do 
      begin
        Item := Fields.PasItemAt[j];
        if length(s) < length(Item.Name) then
          s:=Item.Name;
      end;
    
    { Determine the longest string used.
      This is the one we will use for determining the label width.
    }
    WriteStartList(s);
    for j := 0 to Fields.Count - 1 do 
      begin
        Item := Fields.PasItemAt[j];
        WriteAnchor(Item.Name,Item.FullLink);
        if Item is TPasVarConst then 
        begin
          WriteDeclarationItem(Item, Item.name, AccessibilityStr[Item.State]+' '+
            TPasVarConst(Item).FullDeclaration);
        end
        else
          WriteDeclarationItem(Item, Item.name, AccessibilityStr[Item.State]+' '+Item.name);
        WriteDirect('',true);
        WriteDirect('\par ');
        WriteItemDetailedDescription(Item);
      end;
     WriteEndList; 
  end;     
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFooter;
begin
{  WriteConverted(Footer);}
end;

{ ---------------------------------------------------------------------------- }
  function ExtractFirstWord(var s: string): string;
  var
    p: integer;
    Len: integer;
  begin
    Result := '';
    Len := Length(s);
    p := 1;
    while (p <= Len) and (s[p] in [' ', #9, #13, #10]) do
      Inc(p);
    while (p <= Len) and not (s[p] in [' ', #9, #13, #10]) do
      begin
        Result := Result + s[p];
        Inc(p);
      end;
    s := Copy(s, p, Length(s));
  end;

  procedure TTexDocGenerator.WriteParameter(const ParamName: string; const Desc: string);
  begin
    WriteDirect('\item[');
    WriteDirect(ParamName);
    WriteDirect('] ');
    WriteWithURLs(Desc);
    WriteDirect('',true);
  end;

  { writes the parameters or exceptions list }
  procedure TTexDocgenerator.WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
    List: TStringVector; LinkToParamNames: boolean);
  var
    i: integer;
    s: string;
    ParamName: string;
  begin
    if StringVectorIsNilOrEmpty(List) then
      exit;

    WriteDirect('\item[\textbf{'+Caption+'}]',true);
    WriteDirect('\begin{description}',true);
    { Terrible hack : To fix and replace by a clean solution,
      we need to add an empty item so that the list starts
      at the correct margin.
    }
{    WriteDirect('\item',true);}
    for i := 0 to List.Count - 1 do begin
      s := List[i];
      
      { TODO -- splitting S to ParamName and the rest should be done
        inside TTagManager.Execute, working with raw text, instead
        of here, where the text is already expanded and converted.
        
        TPasMethod should provide properties Params and Raises 
        as a TStringPairVector, holding pairs of strings: 
        for each item, 
        a string being the name of raised exception (or paramater name)
        and and accompanying description (description that is of course 
        already expanded recursively).
        
        Actually, current approach works for now perfectly,
        but only because neighter html generator nor LaTeX generator
        change text in such way that first word of the text
        (assuming it's a normal valid Pascal identifier) is changed.
        
        E.g. '@raises(EFoo with some link @link(Blah))'
        is expanded to 'EFoo with some link <a href="...">Blah</a>'
        so the 1st word ('EFoo') is preserved.
        
        But this is obviously unclean approach. 
        It's also unclean that mechanics of splitting (ExtractFirstWord)
        must be called from each generator class. Such thing like 
        ExtractFirstWord should not only be implemented once,
        but should also be called from only one place in code. }

      ParamName := ExtractFirstWord(s);

      if LinkToParamNames then
       ParamName := SearchLinkOrWarning(ParamName, Func, 
         'Could not resolve link to "%s" from description of item "%s"');

      WriteParameter(ParamName, s);
    end;
    WriteDirect('\end{description}',true);
  end;

  procedure TTexDocGenerator.WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteDirect('\item[\textbf{'+FLanguage.Translation[trReturns]+'}]');
    WriteWithURLs(ReturnDesc);
    WriteDirect('',true);
  end;


procedure TTexDocGenerator.WriteMethodsSummary(const HL: integer; const FuncsProcs: TPasMethods); 
var
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if FuncsProcs.Count = 0 then Exit;
  
  // Sort alphabatically
  FuncsProcs.SortByPasItemName;

  WriteDirect('\begin{description}',true);

  // two passes, in the first (i=0) we write the overview
  // in the second (i=1) we write the descriptions
  WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
  for j := 0 to FuncsProcs.Count - 1 do 
  begin
    p := TPasMethod(FuncsProcs.PasItemAt[j]);

    WriteDirect('\item[\texttt{');
    { overview of functions and procedures }
    { Only write visibility for methods of classes and objects. }
    WriteVisibilityCell(p);

    s := p.FullLink;
      if Assigned(p.MyUnit) then
         if CompareText(p.MyUnit.FullLink, Copy(s, 1,
            Length(p.MyUnit.FullLink))) = 0 then
            begin
              Delete(s, 1, Length(p.MyUnit.FullLink));
              { remove - character from the link }
              Delete(s, 1,1);
            end;

    WriteConverted(p.name);
    WriteDirect('}]');

    WriteItemDescription(p);
  end;
  WriteDirect('\end{description}',true);
end;

procedure TTexDocGenerator.WriteMethods(const HL: integer; const FuncsProcs: TPasMethods);
var
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;
  if FuncsProcs.Count = 0 then exit;

  WriteHeading(HL, FLanguage.Translation[trMethods]);

  { Determine the longest string used.
    This is the one we will use for determining the label width.
  }
  s:=FLanguage.Translation[trDescription];
  if length(s) < length(FLanguage.Translation[trDeclaration])  then
     s:= FLanguage.Translation[trDeclaration];
  if length(s) < length(FLanguage.Translation[trReturns])  then
     s:=FLanguage.Translation[trReturns];
  if length(s) < length(FLanguage.Translation[trParameters])  then
     s:=FLanguage.Translation[trParameters];
  if length(s) < length(FLanguage.Translation[trExceptions])  then
     s:=FLanguage.Translation[trExceptions];


  for j := 0 to FuncsProcs.Count - 1 do 
  begin
      p := TPasMethod(FuncsProcs.PasItemAt[j]);
      
      { overview of functions and procedures }
      WriteHeading(HL+1,p.Name);
      WriteAnchor(p.Name,p.FullLink);

      WriteStartList(s);

      WriteDeclarationItem(p,FLanguage.Translation[trDeclaration],
        AccessibilityStr[p.State]+' '+p.FullDeclaration);

      if HasDescription(p) then
      begin
        WriteStartOfParagraph;
        WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
        WriteItemDetailedDescription(p);
        WriteEndOfParagraph;
      end;

      WriteParamsOrRaises(p, FLanguage.Translation[trParameters], 
        p.Params, false);
      WriteReturnDesc(p, p.Returns);
      WriteParamsOrRaises(p, FLanguage.Translation[trExceptions], 
        p.Raises, true);

      WriteEndList;
  end;
end;
 

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFuncsProcsSummary(const HL: integer; const FuncsProcs: TPasMethods);
var
  j: Integer;
  p: TPasMethod;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;
  if FuncsProcs.Count = 0 then exit;

  // Sort alphabatically
  FuncsProcs.SortByPasItemName;
  WriteDirect('\begin{description}',true);

  for j := 0 to FuncsProcs.Count - 1 do 
  begin
    p := TPasMethod(FuncsProcs.PasItemAt[j]);
    
    WriteDirect('\item[\texttt{');
    { overview of functions and procedures }
    { Only write visibility for methods of classes and objects. }
    s := p.FullLink;
      if Assigned(p.MyUnit) then
         if CompareText(p.MyUnit.FullLink, Copy(s, 1,
            Length(p.MyUnit.FullLink))) = 0 then
            begin
              Delete(s, 1, Length(p.MyUnit.FullLink));
              { remove - character from the link }
              Delete(s, 1,1);
            end;

    WriteConverted(p.name);
    WriteDirect('}]');
    WriteItemDescription(p);
  end;
  WriteDirect('\end{description}',true);
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteFuncsProcs(const HL: integer; const Methods: boolean; const FuncsProcs: TPasMethods);


var
  j: Integer;
  p: TPasMethod;
  s: string;
  procstr: string;
begin
  if ObjectVectorIsNilOrEmpty(FuncsProcs) then Exit;
  if FuncsProcs.Count = 0 then exit;

  WriteHeading(HL, FLanguage.Translation[trFunctionsAndProcedures]);

  { Determine the longest string used.
    This is the one we will use for determining the label width.
  }
  s:=FLanguage.Translation[trDescription];
  if length(s) < length(FLanguage.Translation[trDeclaration])  then
     s:= FLanguage.Translation[trDeclaration];
  if length(s) < length(FLanguage.Translation[trReturns])  then
     s:=FLanguage.Translation[trReturns];
  if length(s) < length(FLanguage.Translation[trParameters])  then
     s:=FLanguage.Translation[trParameters];


  for j := 0 to FuncsProcs.Count - 1 do
  begin
      p := TPasMethod(FuncsProcs.PasItemAt[j]);
      { overview of functions and procedures }
      begin
        { Check if this is a function or a procedure }
        { and add it as an end string                }
        procstr:=p.FullDeclaration;
        procstr:=trimleft(procstr);
        procstr:=trimright(procstr);
        if (pos('procedure',LowerCase(procstr)) >= 1) then
          procstr:='procedure'
        else
          procstr:='function';

        WriteHeading(HL+1,p.Name+' '+procstr);
        WriteAnchor(p.Name,p.FullLink);

        WriteStartList(s);
        
        WriteDeclarationItem(p,FLanguage.Translation[trDeclaration],
          p.FullDeclaration);

        if HasDescription(p) then
        begin
          WriteStartOfParagraph;
          WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
          WriteItemDetailedDescription(p);
          WriteEndOfParagraph;
        end;

        WriteParamsOrRaises(p, FLanguage.Translation[trParameters], 
          p.Params, false);
        WriteReturnDesc(p, p.Returns);
        WriteParamsOrRaises(p, FLanguage.Translation[trExceptions], 
          p.Raises, true);
        
        WriteEndList;
        
      end;
  end;
end;

procedure TTexDocGenerator.WriteHeading(Level: integer; const s: string);
begin
  if (Level < 1) then Level := 1;
  if Level > 5 then begin
    DoMessage(2, mtWarning, 'latex generator cannot write headlines of level 4 or greater; will use 4 instead.', []);
    Level := 5;
  end;
  case Level of
    1: begin
        WriteDirect('\chapter{');
        WriteConverted(s);
        WriteDirect('}', true);
       end; 
    2: begin
        WriteDirect('\section{');
        WriteConverted(s);
        WriteDirect('}', true);
       end; 
    3: begin
          if latex2rtf then
            begin
              WriteDirect('\subsection*{');
              WriteConverted(s);
              WriteDirect('}', true);
            end
          else
            begin
              WriteDirect('\ifpdf',true);
              WriteDirect('\subsection*{');
              WriteDirect('\large{\textbf{'+ConvertString(s)+'}}\normalsize\hspace{1ex}'+
                '\hrulefill');
              WriteDirect('}', true);
              WriteDirect('\else',true);
              WriteDirect('\subsection*{');
              WriteConverted(s);
              WriteDirect('}', true);
              WriteDirect('\fi',true);
            end;
       end; 
    4: begin
          WriteDirect('\subsubsection*{');
          WriteDirect('\large{\textbf{'+ConvertString(s)+'}}\normalsize\hspace{1ex}'+
          '\hfill');
          WriteDirect('}',true);
       end;
    5: begin
        WriteDirect('\paragraph*{');
        WriteConverted(s);
        WriteDirect('}\hspace*{\fill}', true);
        WriteDirect('',true);
       end; 
  end;
end;

{ ---------- }

procedure TTexDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;
  if AItem.Description <> '' then
    begin
      WriteWithURLs(AItem.Description);
    end
  else
      WriteDirect(' ');
end;


function TTexDocGenerator.HasDescription(const AItem: TPasItem): boolean;
var
  Ancestor: TPasItem;
  AncestorName: string;
begin
  HasDescription := false;
  if not Assigned(AItem) then Exit;

  if AItem.Description <> '' then 
  begin
    HasDescription := true;
    exit
  end;
  if AItem.DetailedDescription <> '' then
  begin
    HasDescription := true;
    exit;
  end;
  if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then 
  begin
    AncestorName := TPasCio(AItem).Ancestors.FirstName;
    Ancestor := SearchItem(AncestorName, AItem);
    if Assigned(Ancestor) then
      begin
        HasDescription:=HasDescription(Ancestor);
        exit;
      end;
  end;    
end;

procedure TTexDocGenerator.WriteItemDetailedDescription(const AItem: TPasItem);
var
  Ancestor: TPasItem;
  AncestorName: string;
begin
  if not Assigned(AItem) then Exit;

  if AItem.Description <> '' then 
  begin
    WriteWithURLs(AItem.Description);
    
    if AItem.DetailedDescription <> '' then 
      begin
        WriteDirect('\hfill\vspace*{1ex}',true);
        WriteDirect('',true);
        WriteWithURLs(AItem.DetailedDescription);
      end;
  end else 
  begin
    if AItem.DetailedDescription <> '' then 
    begin
      WriteWithURLs(AItem.DetailedDescription);
    end else 
    begin
      if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then 
      begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := SearchItem(AncestorName, AItem);
        if Assigned(Ancestor) then
          begin
            WriteConverted(Format('no description available, %s description follows', [AncestorName]));
            WriteItemDetailedDescription(Ancestor);
          end;
      end else
      begin
        WriteDirect(' ');
      end;
    end;
  end;
end;

procedure TTexDocGenerator.WriteItems(HL: integer; Heading: string; const
  Anchor: string; const i: TPasItems);
var
  j, k: Integer;
  Item: TPasItem;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(i) then Exit;
  if i.count = 0 then exit;

  WriteHeading(HL, Heading);
  
  s:=FLanguage.Translation[trDescription];
  if length(s) < length(FLanguage.Translation[trDeclaration])  then
     s:= FLanguage.Translation[trDeclaration];
  if length(s) < length(FLanguage.Translation[trReturns])  then
     s:=FLanguage.Translation[trReturns];
  if length(s) < length(FLanguage.Translation[trParameters])  then
     s:=FLanguage.Translation[trParameters];
  
  
  for j := 0 to i.Count - 1 do begin
    Item := i.PasItemAt[j];

    if Item is TPasEnum then
      WriteHeading(HL+1, Item.Name+' '+LowerCase(FLanguage.Translation[trEnum]))
    else
      WriteHeading(HL+1, Item.Name);
    
    WriteAnchor(Item.Name,Item.FullLink);
    
    WriteStartList(s);

    if Item is TPasVarConst then 
      begin
        WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration],
            Item.Name+TPasVarConst(Item).FullDeclaration);
      end
    else
        WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration],
            Item.Name);
      
    WriteDirect('',true);

    
    
    if not (Item is TPasEnum) then
      begin
        if HasDescription(Item) then
        begin
          WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
          WriteItemDetailedDescription(Item);
          WriteDirect('\par ',true);
        end;
      end
    else
     begin
      WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
      { We may be here if the subitems have descriptions, 
        if the main has no enumerations then simply skip it. 
      }
      if HasDescription(Item) then
        WriteItemDetailedDescription(Item);
      if TPasEnum(Item).Members.Count > 0 then begin
        WriteDirect('\begin{description}', true);
        for k := 0 to TPasEnum(Item).Members.Count-1 do begin
          WriteDirect('\item[\texttt{');
          { add the first character for enums }
          WriteConverted(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).Name);
          { add the end characters for enums }
          WriteDirect('}] ');
          WriteWithURLs(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).GetDescription);
          WriteDirect('', true);
        end;
        WriteDirect('\end{description}', true);
      end;
    end;
    WriteEndList;
  end;
end;

{ ---------- }

procedure TTexDocGenerator.WriteOverviewFiles;
var
  ItemsToCopy: TPasItems;
  PartialItems: TPasItems;
  TotalItems: TPasItems; // Collect all Items for final listing.
  i: Integer;
  Item: TPasItem;
  j: Integer;
  PU: TPasUnit;
begin
  WriteUnitOverviewFile;

  if ObjectVectorIsNilOrEmpty(Units) then Exit;

  // Make sure we don't free the Itmes when we free the container.
  TotalItems := TPasItems.Create(False);

  for i := 2 to NUM_OVERVIEW_FILES_USED - 1  do begin
    if (CreateStream(OverviewFilenames[i] + GetFileExtension, True) = csError)
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
        WriteStartOfTableRow('');

        WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
        WriteLink(Item.FullLink, Item.Name, 'bold');
        WriteEndOfTableCell;

        WriteStartOfTableCell;
        WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
        WriteEndOfTableCell;

        if j = 0 then
          WriteStartOfTableCell('width="100%"', '')
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
      WriteConverted(FLanguage.Translation[trNone]);
      WriteEndOfParagraph;
    end;

    TotalItems.InsertItems(PartialItems);
    PartialItems.Free;
    WriteFooter;
    WriteEndOfDocument;
    CloseStream;
  end;

  if CreateStream(OverviewFilenames[7] + GetFileExtension, True) = csError then
    begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[7] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing overview file ' + OverviewFilenames[7]
    + '...', []);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineIdentifiers]);
  WriteHeading(1, FLanguage.Translation[trHeadlineIdentifiers]);
  WriteStartOfTable3Columns(FLanguage.Translation[trName], FLanguage.Translation[trUnit],
    FLanguage.Translation[trDescription]);

  TotalItems.SortByPasItemName;
  for j := 0 to TotalItems.Count - 1 do begin
    Item := TotalItems.PasItemAt[j];
    WriteStartOfTableRow('');

    WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
    WriteLink(Item.FullLink, Item.Name, 'bold');
    WriteEndOfTableCell;

    WriteStartOfTableCell;
    WriteLink(Item.MyUnit.FullLink, Item.MyUnit.Name, 'bold');
    WriteEndOfTableCell;

    if j = 0 then
      WriteStartOfTableCell('width="100%"', '')
    else
      WriteStartOfTableCell;
    WriteItemDescription(Item);
    WriteEndOfTableCell;

    WriteEndOfTableRow;
  end;

  TotalItems.Free;

  WriteEndOfTable;
  WriteFooter;
  WriteEndOfDocument;
  CloseStream;
end;


procedure TTexDocGenerator.WriteProperties(HL: integer; const p:
  TPasProperties);
var
  j: Integer;
  Prop: TPasProperty;
  s: string;
begin
  if ObjectVectorIsNilOrEmpty(p) then Exit;

  WriteHeading(HL, FLanguage.Translation[trProperties]);
  
  if FLatex2Rtf then
    begin
    { Determine the longest string used.
      This is the one we will use for determining the label width.
    }
    s:=FLanguage.Translation[trDescription];
    if length(s) < length(FLanguage.Translation[trDeclaration])  then
       s:= FLanguage.Translation[trDeclaration];
    if length(s) < length(FLanguage.Translation[trReturns])  then
       s:=FLanguage.Translation[trReturns];
    if length(s) < length(FLanguage.Translation[trParameters])  then
       s:=FLanguage.Translation[trParameters];
    if length(s) < length(FLanguage.Translation[trExceptions])  then
       s:=FLanguage.Translation[trExceptions];
  

    for j := 0 to p.Count - 1 do 
      begin
        Prop := TpasProperty(p.PasItemAt[j]);

        WriteHeading(HL+1, Prop.Name);
        WriteAnchor(Prop.Name,Prop.FullLink);
        WriteStartList(s);        
        
        WriteDeclarationItem(Prop, FLanguage.Translation[trDeclaration], 
          AccessibilityStr[Prop.State]+ ' '+Prop.Fulldeclaration);
          
        if HasDescription(Prop) then
        begin
          WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
          WriteStartOfParagraph;
          WriteItemDetailedDescription(Prop);
          WriteEndOfParagraph;
        end;
        
        WriteEndList;        
      end;
    end
   else
  begin    
  
    { get the longest string for names }
    s:='';
    for j := 0 to p.Count - 1 do
      begin
        Prop := TpasProperty(p.PasItemAt[j]);
        if length(s) < length(Prop.Name) then
          s:=Prop.Name;
      end;
    
    { Determine the longest string used.
      This is the one we will use for determining the label width.
    }
    WriteStartList(s);
    
    for j := 0 to p.Count - 1 do 
      begin
        Prop := TPasProperty(p.PasItemAt[j]);
        WriteAnchor(Prop.Name,Prop.FullLink);
        WriteDeclarationItem(Prop, Prop.Name, AccessibilityStr[Prop.State]+' '+
           Prop.FullDeclaration);
        WriteDirect('',true);
        WriteDirect('\par ');
        WriteItemDetailedDescription(Prop);
      end;
    
     WriteEndList; 
  end;
end;

procedure TTexDocGenerator.WritePropertiesSummary(HL: integer; p:
  TPasProperties);
begin
end;

{ ---------------------------------------------------------------------------- }
procedure TTexDocGenerator.WriteAnchor(ItemName, Link: string);
begin
  { no links in RTF documentation }
  if FLatex2rtf then exit;
  if Link <> '' then
     WriteDirect('\label{'+Link+'}',true)
  else
     WriteDirect('\label{'+ItemName+'}',true);
  WriteDirect('\index{'+ConvertString(ItemName)+'}',true);
end;


{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteStartOfCode;
begin
  WriteDirect('\begin{ttfamily}',true);
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteStartOfDocument(AName: string);
begin
  { write basic header }
  WriteAppInfo;
  WriteDirect('\documentclass{report}',true);
  WriteDirect('\usepackage{hyperref}',true);
  WriteDirect('% WARNING: THIS SHOULD BE MODIFIED DEPENDING ON THE LETTER/A4 SIZE',true);
  WriteDirect('\oddsidemargin 0cm',true);
  WriteDirect('\evensidemargin 0cm',true);
  WriteDirect('\marginparsep 0cm',true);
  WriteDirect('\marginparwidth 0cm',true);
  WriteDirect('\parindent 0cm',true);
  if not FLatex2Rtf then
  begin
    WriteDirect('\setlength{\textwidth}{\paperwidth}',true);
    WriteDirect('\addtolength{\textwidth}{-2in}',true);
  end
  else
    WriteDirect('\textwidth 16.5cm',true);
  WriteDirect('',true);
  WritePDFIfDef;
  Title := ConvertString(Title);
  WritePDFDocInfo(Title);
  WriteDirect('',true);
  WriteDirect('\begin{document}',true);
  if not Flatex2rtf then
  begin
    if Title <> '' then
      begin
        WriteDirect('\title{'+Title+'}',true);
        WriteDirect('\author{Pasdoc}',true);      
        WriteDirect('\maketitle',true);
        WriteDirect('\newpage',true);
      end;

    WriteDirect('\label{toc}');
    WriteDirect('\tableofcontents',true);
    WriteDirect('\newpage',true);
  end;
  
  WriteDirect('% special variable used for calculating some widths.',true);
  WriteDirect('\newlength{\tmplength}',true);
  if Length(Header) > 0 then begin
    WriteWithURLs(Header);
  end;
end;


procedure TTexDocGenerator.WriteStartOfParagraph;
begin
  WriteDirect('\par',true);
end;

procedure TTexDocGenerator.WriteStartOfTable1Column(T: String);
begin
  FOddTableRow := 0;
  NumCells := 1;
  WriteDirect('\begin{tabular}{|l|}',true);
  WriteDirect('\hline',true);
  if t <> '' then
    begin
     WriteConverted(t);
     WriteDirect(' \\',true);
     WriteDirect('\hline',true);
    end;
end;

procedure TTexDocGenerator.WriteStartOfTable2Columns(T1, T2: String);
begin
  WriteDirect('\begin{tabular}{|ll|}',true);
  WriteDirect('\hline',true);
  if t1 <> '' then
  Begin
     WriteConverted(t1);
     WriteDirect(' & ');
     WriteConverted(t2);
     WriteDirect(' \\',true);
     WriteDirect('\hline',true);
  end;
  NumCells := 2;
end;

procedure TTexDocGenerator.WriteStartOfTable3Columns(T1, T2, T3: String);
begin
  WriteDirect('\begin{tabular}{|lll|}',true);
  WriteDirect('\hline',true);
  WriteConverted(T1);
  WriteDirect(' & ');
  WriteConverted(T2);
  WriteDirect(' & ');
  WriteConverted(T3);
  WriteDirect(' \\',true);
  WriteDirect('\hline',true);
  NumCells := 3;
end;


procedure TTexDocGenerator.WriteStartOfTableCell(const Params, css: string);
begin
end;

procedure TTexDocGenerator.WriteStartOfTableCell(const css: string);
begin
  WriteStartOfTableCell('', css);
end;

procedure TTexDocGenerator.WriteStartOfTableCell;
begin
  WriteStartOfTableCell('', '');
end;

procedure TTexDocGenerator.WriteStartOfTableRow(const CssClass: string);
begin
  CellCounter := 0;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);
type
  TSections = (dsDescription, dsUses, dsClasses, dsFuncsProcs,
    dsTypes, dsConstants, dsVariables);
  TSectionSet = set of TSections;
var
  SectionsAvailable: TSectionSet;
  SectionHeads: array[TSections] of string;

  procedure ConditionallyAddSection(Section: TSections; Condition: boolean);
  begin
    if Condition then
      Include(SectionsAvailable, Section);
  end;

begin

  SectionHeads[dsDescription] := FLanguage.Translation[trDescription];
  SectionHeads[dsUses] := 'uses';
  SectionHeads[dsClasses] := FLanguage.Translation[trCio];
  SectionHeads[dsFuncsProcs]:= FLanguage.Translation[trFunctionsAndProcedures];
  SectionHeads[dsTypes]:= FLanguage.Translation[trTypes];
  SectionHeads[dsConstants]:= FLanguage.Translation[trConstants];
  SectionHeads[dsVariables]:= FLanguage.Translation[trVariables];

  SectionsAvailable := [dsDescription];
  ConditionallyAddSection(dsUses, WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits));
  ConditionallyAddSection(dsClasses, not ObjectVectorIsNilOrEmpty(U.CIOs));
  ConditionallyAddSection(dsFuncsProcs, not ObjectVectorIsNilOrEmpty(U.FuncsProcs));
  ConditionallyAddSection(dsTypes, not ObjectVectorIsNilOrEmpty(U.Types));
  ConditionallyAddSection(dsConstants, not ObjectVectorIsNilOrEmpty(U.Constants));
  ConditionallyAddSection(dsVariables, not ObjectVectorIsNilOrEmpty(U.Variables));

  DoMessage(2, mtInformation, 'Writing Docs for unit "%s"', [U.Name]);

  WriteAnchor(U.Name, U.FullLink);
  WriteHeading(HL, FLanguage.Translation[trUnit] + ' ' + U.Name);

  if HasDescription(U) then
    WriteUnitDescription(HL + 1, U);

  WriteUnitUses(HL + 1, U);
  
  if (U.CIOs.count <> 0) or (U.FuncsProcs.count <> 0) then
    WriteHeading(HL + 1, FLanguage.Translation[trOverview]);
  WriteCIOSummary(HL + 1, U.CIOs);
  WriteFuncsProcsSummary(HL + 1, U.FuncsProcs);
  
  WriteCIOs(HL + 1, U.CIOs);

  WriteFuncsProcs(HL + 1, False, U.FuncsProcs);

  WriteTypes(HL + 1, U.Types);

  WriteConstants(HL + 1, U.Constants);

  WriteVariables(HL + 1, U.Variables);

  WriteAuthors(HL + 1, U.Authors);
  WriteDates(HL + 1, U.Created, U.LastMod);
end;

procedure TTexDocGenerator.WriteUnitDescription(HL: integer; U: TPasUnit);
begin
  WriteHeading(HL, FLanguage.Translation[trDescription]);
  WriteItemDetailedDescription(U);
  WriteDirect('',true);
end;

procedure TTexDocGenerator.WriteUnitOverviewFile;
var
  c: TPasItems;
  Item: TPasItem;
  j: Integer;
begin
  c := Units;
  if CreateStream(OverviewFilenames[0] + GetFileExtension, True) = csError
    then begin
    DoMessage(1, mtError, 'Could not create overview output file "' +
      OverviewFilenames[0] + '".', []);
    Exit;
  end;
  DoMessage(3, mtInformation, 'Writing unit overview file "%s" ...',
    [OverviewFilenames[0]]);
  WriteStartOfDocument(FLanguage.Translation[trHeadlineUnits]);
  WriteHeading(1, FLanguage.Translation[trHeadlineUnits]);
  if Assigned(c) and (c.Count > 0) then begin
    WriteStartOfTable2Columns(FLanguage.Translation[trName],
      FLanguage.Translation[trDescription]);
    for j := 0 to c.Count - 1 do begin
      Item := c.PasItemAt[j];
      WriteStartOfTableRow('');
      WriteStartOfTableCell('nowrap="nowrap"', 'itemname');
      WriteLink(Item.FullLink, Item.Name, 'bold');
      WriteEndOfTableCell;

      if j = 0 then
        WriteStartOfTableCell('width="100%"', '')
      else
        WriteStartOfTableCell;
      WriteItemDescription(Item);
      WriteEndOfTableCell;
      WriteEndOfTableRow;
    end;
    WriteEndOfTable;
  end;
  WriteFooter;
  WriteEndOfDocument;
  CloseStream;
end;

procedure TTexDocGenerator.WriteImage(const src, alt, css: string);
var
  s: string;
begin
  if css <> '' then
    s := Format('<img class="%s"', [css])
  else
    s := '<img border="0"';
  WriteDirect(Format('%s src="%s" alt="%s"/>', [s, src, alt]));
end;

procedure TTexDocGenerator.WriteVisibilityCell(const Item: TPasItem);

  procedure WriteVisibilityImage(const Image: string; trans: TTranslationID);
  begin
    WriteImage(Image, ConvertString(FLanguage.Translation[trans]), '');
  end;

begin
{
  case Item.State of
    STATE_PRIVATE:
      WriteVisibilityImage('private.gif', trPrivate);
    STATE_PROTECTED:
      WriteVisibilityImage('protected.gif', trProtected);
    STATE_PUBLIC:
      WriteVisibilityImage('public.gif', trPublic);
    STATE_PUBLISHED:
      WriteVisibilityImage('published.gif', trPublished);
    STATE_AUTOMATED:
      WriteVisibilityImage('automated.gif', trAutomated);
  end;
}  
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteUnitUses(const HL: integer; U: TPasUnit);
var
  i: Integer;
  ULink: TPasItem;
begin
  if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
    WriteHeading(HL, 'uses');
    WriteDirect('\begin{itemize}',true);
    for i := 0 to U.UsesUnits.Count-1 do begin
      WriteDirect('\item ');
      ULink := FUnits.FindName(U.UsesUnits[i]);
      if ULink is TPasUnit then begin
        WriteLink(ULink.FullLink, U.UsesUnits[i], 'bold');
      end else begin
        WriteDirect(U.UsesUnits[i]);
      end;
      WriteDirect('');
    end;   
    WriteDirect('\end{itemize}',true);
  end;
end;

procedure TTexDocGenerator.WriteWithURLs(s: string);
var
  s1, s2, link: string;
begin
  while ExtractLink(s, s1, s2, link) do begin
    WriteSpellChecked(S1);
    WriteLink('',link,'');
    s:=s2;
  end;
  WriteSpellChecked(s);
end;

procedure TTexDocGenerator.WriteSpellChecked(const AString: string);
var
  LErrors: TObjectVector;
  i, temp: Integer;
  LString, s: string;
begin
  LErrors := TObjectVector.Create(True);
  CheckString(AString, LErrors);
  if LErrors.Count = 0 then 
  begin
    WriteDirect(AString);
  end else 
  begin
    // build s
    s := '';
    LString := AString;
    for i := LErrors.Count-1 downto 0 do begin
      // everything after the offending word
      temp := TSpellingError(LErrors.Items[i]).Offset+Length(TSpellingError(LErrors.Items[i]).Word) + 1;
      s := ( '">' + TSpellingError(LErrors.Items[i]).Word +  '</acronym>' + Copy(LString, temp, MaxInt)) + s; // insert into string
      if Length(TSpellingError(LErrors.Items[i]).Suggestions) > 0 then begin
        s := 'suggestions: '+TSpellingError(LErrors.Items[i]).Suggestions + s;
      end else begin
        s := 'no suggestions' + s;
      end;
      s := '<acronym style="#0000FF; border-bottom: 1px solid crimson" title="' + s;
      SetLength(LString, TSpellingError(LErrors.Items[i]).Offset);
    end;
    WriteDirect(LString);
    WriteDirect(s);
  end;
  LErrors.Free;
end;

function TTexDocGenerator.ConvertString(const S: String): String;
const
  ReplacementArray: array[0..10] of TCharReplacement = (
    (cChar: '$'; sSpec: '{\$}'),
    (cChar: '&'; sSpec: '{\&}'),
    (cChar: '%'; sSpec: '{\%}'),
    (cChar: '#'; sSpec: '{\#}'),
    (cChar: '{'; sSpec: '{\{}'),
    (cChar: '}'; sSpec: '{\}}'),
    (cChar: '>'; sSpec: '{$>$}'),
    (cChar: '<'; sSpec: '{$<$}'),
    (cChar: '^'; sSpec: '{\^{}}'),
    (cChar: '\'; sSpec: '{\textbackslash}'),
    (cChar: '_'; sSpec: '{\_}')
  );
begin
  Result := StringReplaceChars(S, ReplacementArray);
end;

function TTexDocGenerator.ConvertChar(c: char): String;
begin
  ConvertChar := ConvertString(c);
end;


procedure TTexDocGenerator.BuildLinks;
begin
  FLinkCount := 1;
  inherited;
end;

function TTexDocGenerator.EscapeURL(const AString: string): string;
begin
  EscapeURL := AString;
end;

function TTexDocGenerator.InsertParagraphs(const S: string): string; 
begin
  Result := S;
  Utils.InsertParagraphs(Result, LineEnding + LineEnding);
end;

(*
  $Log$
  Revision 1.25  2005/04/12 21:25:13  kambi
  * Cleaning up THTMLGenerator.ConvertString and TTexGenerator.ConvertString:
    - code shared in Utils.StringReplaceChars
    - removed wrong conversions of chars that used to broke pasdoc's output in
      various non-ISO-8859-1 charsets. This fixes bug 1109867 and actually
      more general problems with many charsets, see my letter on pasdoc-main
      [http://sourceforge.net/mailarchive/forum.php?thread_id=7028980&forum_id=4647].
    - accidentaly this also fixes a bug in THTMLGenerator.ConvertString
      (previous check
      "if (ord(Result[i]) > 127) or (Result[i] in ['<','>','&','"']) then"
      prevented convertion of some chars like '^' to '&circ;')

  Revision 1.24  2005/04/10 06:06:16  kambi
  * Corrected autodoc output (some broken @links etc.)

  Revision 1.23  2005/04/08 17:56:17  twm
  removed some unused variable declarations
  checkt that it still compiles with Kylix

  Revision 1.22  2005/04/06 12:52:42  kambi
  * Two similiar implementations of WriteCodeWithLinks in tex and html generator
    merged to one code in TDocGenerator.WriteCodeWithLinksCommon
  * WriteLink(4 args) renamed to WriteLinkTarget,
    to be able to take address of WriteLink
  * WriteCodeWithLinksCommon does check whether S = P.Name before checking
    whether S in a Pascal directive

  Revision 1.21  2005/04/05 07:36:15  kambi
  * @html tag is ignored in non-html output, @latex tag implemented

  Revision 1.20  2005/04/04 21:14:10  kambi
  Commiting my fixes sent to pasdoc-main.
  "Trailer of my next patch" [http://sourceforge.net/mailarchive/forum.php?thread_id=6919292&forum_id=4647]
  "Fixes to TTagManager.Execute" [http://sourceforge.net/mailarchive/forum.php?thread_id=6934185&forum_id=4647]
  "Small fix for LaTeX output" [http://sourceforge.net/mailarchive/forum.php?thread_id=6946611&forum_id=4647]
  "Fix for LaTeX genetator omitting some things" [http://sourceforge.net/mailarchive/forum.php?thread_id=6948809&forum_id=4647]
  "Fix for --name parameter with Latex generator" [http://sourceforge.net/mailarchive/forum.php?thread_id=6959580&forum_id=4647]

  Revision 1.19  2005/03/29 06:55:48  johill
  patches from Michalis Kamburelis

  Revision 1.18  2005/01/22 13:03:17  twm
  moved description comment of ConvertChar from ConvertString to where it belongs

  Revision 1.17  2004/08/20 00:34:44  ccodere
    * bugfixes when writing URL's in LaTeX

  Revision 1.16  2004/07/16 16:34:16  johill
  some code cleanup, fixes from Pierre Woestyn

  Revision 1.15  2004/07/09 14:03:28  johill
  fix things, apply patches from mailing list.

  Revision 1.14  2004/06/20 18:36:26  johill
  Changes from Grzegorz Skoczylas: character set handling + updated Polish
  translation

  Revision 1.13  2004/05/07 18:13:24  ccodere
    * fixes for compilation with different compiler targets

  Revision 1.12  2004/05/07 07:14:27  johill
  fix bug: write \begin{description} only if it will be non-empty, achieved by writing it lazily only when real output comes. Something like this should possibly be done in more places.

  Revision 1.11  2004/05/06 19:50:27  johill
  clean up source a bit, fix warnings and some hints

  Revision 1.10  2004/04/20 01:58:20  ccodere
  + now all non-documented items will not be output, there is still a bug with the CIO Heading though that might appear with an empty section.

  Revision 1.9  2004/03/19 17:15:13  ccodere
    - remove description and headings with no data

  Revision 1.8  2004/03/19 15:55:09  ccodere
    * bugfix with parsing field names with _ charactersa
    + added longcode support

  Revision 1.7  2004/03/17 05:06:03  ccodere
    bugfix with return values, title was not in correct font.

  Revision 1.6  2004/03/16 07:11:57  ccodere
  + if no project is defined, the document will be called docs
  + htmlstring now works as expected

  Revision 1.5  2004/03/13 02:21:17  ccodere
  1) added latex2rtf support
  2) now independent from paper size
  3) table of contents
  4) several several bugfixes

  Revision 1.4  2004/03/12 05:01:21  ccodere
  added support for properties.
  added support for overview of functions
  some fixes regarding illegal characters
  some fixes regarding some vertical spacing

*) // GSk: commend moved before last *end* to eliminate compiler warning

end.

