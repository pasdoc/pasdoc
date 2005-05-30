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
    output in LaTex format. 
    
    TODO:
      WriteFields
      WriteMethods
      WriteItems
      WriteProperties
    are a horrible mess that look like they were copy&pasted all over,
    only to change a small detail here and there. 
    Merge them -- preferably into one thing. }
  TTexDocGenerator = class(TDocGenerator)
  private
    FNumericFilenames: boolean;
    FWriteUses: boolean;
    FLinkCount: Integer;
    FOddTableRow: Integer;
    { number of cells (= columns) per table row }
    NumCells: Integer;
    { number of cells we've already written in current table row }
    CellCounter: LongInt;
    FLatex2Rtf: Boolean;
    
    procedure WriteProperties(HL: integer; const p: TPasProperties);

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

    procedure WriteItemDescription(const AItem: TPasItem);
    (*
      Writes the Item's DetailedDescription. If the Item also has
      AbstractDescription this is written in front of the
      DetailedDescription. 
      
      TODO: this should be fixed to write 
      @longcode(#
        WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
      #)
      inside it, and to take care of writing paragraph markers inside it.
      Right now this is messy -- to many paragraphs may be written around
      (which does not hurt, but is unclean) and 
      FLanguage.Translation[trDescription] header may be written when
      there is actually no description (only e.g. Params or Raises or Returns
      information).
    *)
    procedure WriteItemDetailedDescription(const AItem: TPasItem);

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
      
    { Writes information on functions and procedures or methods of a unit or
      class, interface or object to output.
      If argument OfObject is true, they will be considered methods of a class,
      interface or object, otherwise they're considered functions or procedures
      of a unit.
      The functions are stored in the Items argument. }
    procedure WriteMethods(const HL: integer; const Items: TPasMethods;
      OfObject: boolean); 
    
    procedure WriteItemsSummary(const Items: TPasItems);

    procedure WriteLink(const href, caption, css: string);
    procedure WriteAnchor(ItemName, Link: string);
        
    procedure WriteSpellChecked(const AString: string);
    
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
  protected
  
    function ConvertString(const s: string): string; override;
    
    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): String; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;
    
    procedure WriteUnitDescription(HL: integer; U: TPasUnit); override;

    function LatexString(const S: string): string; override;

    { Makes a String look like a coded String, i.e. <CODE>TheString</CODE>
      in Html. }
    function CodeString(const s: string): string; override;
    
    { Returns a link to an anchor within a document. HTML simply concatenates
      the strings with a "-" character between them. }
    function CreateLink(const Item: TBaseItem): string; override;
    
    { Creates a valid HTML link, starting with an anchor that points to Link,
      encapsulating the text ItemName in it. }
    function CreateReferencedLink(ItemName, Link: string): string; override;
    
    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio); override;
    
    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems); override;
    
    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string); override;
    
    procedure WriteStartOfCode; override;
    procedure WriteEndOfCode; override;
    
    procedure WriteItems(HL: integer; Heading: string; const Anchor: string;
      const i: TPasItems); override;
      
    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(Level: integer; const s: string); override;
    
    function Paragraph: string; override;
    
    function LineBreak: string; override;
    
    function URLLink(const URL: string): string; override;
  public
    function FormatPascalCode(const Line: string): string; override;

    { Returns Latex file extension ".tex". }
    function GetFileExtension: string; override;
    { The method that does everything - writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    procedure BuildLinks; override;

    function EscapeURL(const AString: string): string; virtual;
  published
    property NumericFilenames: boolean read FNumericFilenames write FNumericFilenames
      default false;
    property WriteUsesClause: boolean read FWriteUses write FWriteUses
      default false;
    { Indicate if the output must be simplified for latex2rtf }
    property Latex2rtf: boolean read FLatex2rtf write FLatex2rtf default false;
  end;

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

function TTexDocGenerator.CreateLink(const Item: TBaseItem): string;

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
  
  if (Item is TPasItem) and Assigned(TPasItem(Item).MyUnit) then 
  begin
    if Assigned(TPasItem(Item).MyObject) then begin
      { it's a method, a field or a property - only those have MyObject initialized }
      Result := TPasItem(Item).MyObject.FullLink + '-' + Item.Name;
    end else begin
      if Item is TPasCio then 
      begin
        { it's an object / a class }
        Result := NewLink(TPasItem(Item).MyUnit.Name + '.' + Item.Name);
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := TPasItem(Item).MyUnit.FullLink + '-' + Item.Name;
      end;
    end;
  end else begin
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
  WriteDirectLine('');
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
  Item: TBaseItem;
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

  WriteMethods(HL + 2, CIO.Methods, true);

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
        if not NoGeneratorInfo then
          WriteDirect(' /CreationDate ('+
            FormatDateTime('yyyymmddhhmmss', Now)+')',true);
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
    WriteConvertedLine(LastMod);
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
  WriteDirectLine('');
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
        
        WriteDeclarationItem(Item,FLanguage.Translation[trDeclaration],
          AccessibilityStr[Item.State] + ' ' + Item.FullDeclaration);
          
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

        WriteDeclarationItem(Item, Item.name, 
          AccessibilityStr[Item.State] + ' ' + Item.FullDeclaration);

        WriteDirect('',true);
        WriteDirect('\par ');
        WriteItemDetailedDescription(Item);
      end;
     WriteEndList; 
  end;     
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteMethods(const HL: integer; 
  const Items: TPasMethods; OfObject: boolean);
var
  j: Integer;
  Item: TPasItem;
  s: string;
  Accessibility: string;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  if OfObject then
    WriteHeading(HL, FLanguage.Translation[trMethods]) else
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
  if length(s) < length(FLanguage.Translation[trExceptions])  then
     s:=FLanguage.Translation[trExceptions];

  for j := 0 to Items.Count - 1 do 
  begin
    Item := Items.PasItemAt[j];
      
    WriteHeading(HL+1, Item.Name);
    WriteAnchor(Item.Name, Item.FullLink);

    WriteStartList(s);

    if OfObject then
      Accessibility := AccessibilityStr[Item.State] + ' ' else
      Accessibility := '';
    WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration],
      Accessibility + Item.FullDeclaration);

    if HasDescription(Item) then
    begin
      WriteStartOfParagraph;
      WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
      WriteItemDetailedDescription(Item);
      WriteEndOfParagraph;
    end;

    WriteEndList;
  end;
end;
 

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteItemsSummary(const Items: TPasItems);
var
  i: Integer;
  Item: TPasItem;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteDirect('\begin{description}',true);

  for i := 0 to Items.Count - 1 do 
  begin
    Item := Items.PasItemAt[i];
    
    WriteDirect('\item[\texttt{');
    
    if Item is TPasCio then
    begin
      WriteDirect(CodeString(ConvertString(Item.Name)));
      WriteDirect(' ');
      WriteConverted(GETCIOTypeName(TPasCio(Item).MyType));
    end else
    begin
      WriteConverted(Item.Name);
    end;
    
    WriteDirect('}]');
    
    WriteSpellChecked(Item.AbstractDescription);
    
    WriteDirectLine('');
  end;
  WriteDirect('\end{description}', true);
end;

{ ---------------------------------------------------------------------------- }

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
        WriteDirectLine('}');
       end; 
    2: begin
        WriteDirect('\section{');
        WriteConverted(s);
        WriteDirectLine('}');
       end; 
    3: begin
          if latex2rtf then
            begin
              WriteDirect('\subsection*{');
              WriteConverted(s);
              WriteDirectLine('}');
            end
          else
            begin
              WriteDirect('\ifpdf',true);
              WriteDirect('\subsection*{');
              WriteDirect('\large{\textbf{'+ConvertString(s)+'}}\normalsize\hspace{1ex}'+
                '\hrulefill');
              WriteDirectLine('}');
              WriteDirect('\else',true);
              WriteDirect('\subsection*{');
              WriteConverted(s);
              WriteDirectLine('}');
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
        WriteDirectLine('}\hspace*{\fill}');
        WriteDirect('',true);
       end; 
  end;
end;

{ ---------- }

procedure TTexDocGenerator.WriteItemDescription(const AItem: TPasItem);
begin
  if AItem = nil then Exit;
  if AItem.AbstractDescription <> '' then
    begin
      WriteSpellChecked(AItem.AbstractDescription);
    end
  else
      WriteDirect(' ');
end;


function TTexDocGenerator.HasDescription(const AItem: TPasItem): boolean;
var
  Ancestor: TBaseItem;
  AncestorName: string;
begin
  Result := false;
  if not Assigned(AItem) then Exit;
  
  Result := AItem.HasDescription or
    { Some hint directive ? }
    AItem.IsDeprecated or AItem.IsPlatformSpecific or AItem.IsLibrarySpecific or
    { Some TPasMethod optional info ? }
    ( (AItem is TPasMethod) and
      TPasMethod(AItem).HasMethodOptionalInfo );

  if Result then Exit;

  if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then 
  begin
    AncestorName := TPasCio(AItem).Ancestors.FirstName;
    Ancestor := SearchItem(AncestorName, AItem);
    if Assigned(Ancestor) and (Ancestor is TPasItem) then
    begin
      HasDescription := HasDescription(TPasItem(Ancestor));
      exit;
    end;
  end;    
end;

procedure TTexDocGenerator.WriteItemDetailedDescription(const AItem: TPasItem);

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(Func: TPasMethod; const Caption: string;
    List: TStringVector; LinkToParamNames: boolean);
    
    procedure WriteParameter(const ParamName: string; const Desc: string);
    begin
      WriteDirect('\item[');
      WriteDirect(ParamName);
      WriteDirect('] ');
      WriteSpellChecked(Desc);
      WriteDirect('',true);
    end;    
    
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
       ParamName := SearchLinkOrWarning(ParamName, Func, '',
         'Could not resolve link to "%s" from description of item "%s"');

      WriteParameter(ParamName, s);
    end;
    WriteDirect('\end{description}',true);
  end;

  procedure WriteReturnDesc(Func: TPasMethod; ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteDirect('\item[\textbf{'+FLanguage.Translation[trReturns]+'}]');
    WriteSpellChecked(ReturnDesc);
    WriteDirect('',true);
  end;

  procedure WriteHintDirective(const S: string);
  begin
    WriteConverted('Warning: ' + S + '.' + LineEnding + LineEnding);
  end;

var
  Ancestor: TBaseItem;
  AncestorName: string;
  AItemMethod: TPasMethod;
begin
  if not Assigned(AItem) then Exit;

  if AItem.IsDeprecated then
    WriteHintDirective(FLanguage.Translation[trDeprecated]);
  if AItem.IsPlatformSpecific then
    WriteHintDirective(FLanguage.Translation[trPlatformSpecific]);
  if AItem.IsLibrarySpecific then
    WriteHintDirective(FLanguage.Translation[trLibrarySpecific]);

  if AItem.AbstractDescription <> '' then 
  begin
    WriteSpellChecked(AItem.AbstractDescription);
    
    if AItem.DetailedDescription <> '' then 
      begin
        if not AItem.AbstractDescriptionWasAutomatic then
        begin
          WriteDirect('\hfill\vspace*{1ex}',true);
          WriteDirect('',true);
        end;
        WriteSpellChecked(AItem.DetailedDescription);
      end;
  end else 
  begin
    if AItem.DetailedDescription <> '' then 
    begin
      WriteSpellChecked(AItem.DetailedDescription);
    end else 
    begin
      if (AItem is TPasCio) and not StringVectorIsNilOrEmpty(TPasCio(AItem).Ancestors) then 
      begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := SearchItem(AncestorName, AItem);
        if Assigned(Ancestor) and (Ancestor is TPasItem) then
          begin
            WriteConverted(Format('no description available, %s description follows', [AncestorName]));
            WriteItemDetailedDescription(TPasItem(Ancestor));
          end;
      end else
      begin
        WriteDirect(' ');
      end;
    end;
  end;

  if (AItem is TPasMethod) and TPasMethod(AItem).HasMethodOptionalInfo then
  begin
    WriteStartOfParagraph;
    AItemMethod := TPasMethod(AItem); 
    WriteParamsOrRaises(AItemMethod, FLanguage.Translation[trParameters], 
      AItemMethod.Params, false);
    WriteReturnDesc(AItemMethod, AItemMethod.Returns);
    WriteParamsOrRaises(AItemMethod, FLanguage.Translation[trExceptions], 
      AItemMethod.Raises, true);
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

    WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration],
      Item.FullDeclaration);
      
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
        WriteDirectLine('\begin{description}');
        for k := 0 to TPasEnum(Item).Members.Count-1 do begin
          WriteDirect('\item[\texttt{');
          { add the first character for enums }
          WriteConverted(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).Name);
          { add the end characters for enums }
          WriteDirect('}] ');
          WriteSpellChecked(TPasItem(TPasEnum(Item).Members.PasItemAt[k]).GetDescription);
          WriteDirectLine('');
        end;
        WriteDirectLine('\end{description}');
      end;
    end;
    WriteEndList;
  end;
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

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
  var
    i: Integer;
    ULink: TPasItem;
  begin
    if WriteUsesClause and not StringVectorIsNilOrEmpty(U.UsesUnits) then begin
      WriteHeading(HL, 'uses');
      WriteDirect('\begin{itemize}',true);
      for i := 0 to U.UsesUnits.Count-1 do begin
        WriteDirect('\item ');
        ULink := TPasUnit(U.UsesUnits.Objects[i]);
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
    
  WriteItemsSummary(U.CIOs);
  WriteItemsSummary(U.FuncsProcs);
  
  WriteCIOs(HL + 1, U.CIOs);

  WriteMethods(HL + 1, U.FuncsProcs, false);

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

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteSpellChecked(const AString: string);
var
  LErrors: TObjectVector;
begin
  LErrors := TObjectVector.Create(True);
  try
    CheckString(AString, LErrors);
    WriteDirect(AString);
    
    { TODO: write here LErrors, like in 
      TGenericHTMLDocGenerator.WriteSpellChecked }
  finally LErrors.Free end;
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

function TTexDocGenerator.Paragraph: string; 
begin
  Result := LineEnding + LineEnding;
end;

function TTexDocGenerator.LineBreak: string; 
begin
  Result := '\\';
end;

function TTexDocGenerator.URLLink(const URL: string): string; 
begin
  Result := '\href{' + EscapeURL(URL) + '}{' + ConvertString(URL) + '}';
end;

end.

