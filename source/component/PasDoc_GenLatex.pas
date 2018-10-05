{
  Copyright 1998-2018 PasDoc developers.

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

{ @abstract(Provides Latex document generator object.)

  Implements an object to generate latex documentation, overriding many of
  @link(TDocGenerator)'s virtual methods. }

unit PasDoc_GenLatex;

{$I pasdoc_defines.inc}

interface

uses
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_Languages,
  PasDoc_StringVector,
  PasDoc_Types,
  Classes;

type
  { @abstract(generates latex documentation)
    Extends @link(TDocGenerator) and overwrites many of its methods to generate
    output in LaTex format. }
  TTexDocGenerator = class(TDocGenerator)
  private
  {$IFDEF unused}
    FOddTableRow: Integer;
    { number of cells (= columns) per table row }
    NumCells: Integer;
    { number of cells we've already written in current table row }
    CellCounter: LongInt;
  {$ELSE}
  {$ENDIF}
    FLatex2Rtf: Boolean;
    FLatexHead: TStrings;

    FImages: TStringList;

    { Writes information on doc generator to current output stream,
      including link to pasdoc homepage. }
    procedure WriteAppInfo;
    { Writes authors to output, at heading level HL. Will not write anything
      if collection of authors is not assigned or empty. }
    procedure WriteAuthors(HL: integer; Authors: TStringVector);
    procedure WriteCodeWithLinks(const p: TPasItem; const Code: string;
      WriteItemLink: boolean);

    procedure WriteEndOfDocument;
    { Finishes a LaTeX paragraph by writing a blank line. }
    procedure WriteEndOfParagraph;
  {$IFDEF unused}
    { Finishes an Latex table cell by writing ' & '. }
    procedure WriteEndOfTableCell;
    { Finishes an HTML table by writing a closing TABLE tag. }
    procedure WriteEndOfTable;
    { Finishes a LaTeX table row by writing '\\'. }
    procedure WriteEndOfTableRow;
  {$ELSE}
  {$ENDIF}

    (*
      Writes the Item's AbstractDescription and DetailedDescription.

      TODO: this should be fixed to write
      @longcode(#
        WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
      #)
      inside it, and to take care of writing paragraph markers inside it.
      Right now this is messy --- to many paragraphs may be written around
      (which does not hurt, but is unclean) and
      FLanguage.Translation[trDescription] header may be written when
      there is actually no description (only e.g. Params or Raises or Returns
      information).
    *)
    procedure WriteItemLongDescription(const AItem: TPasItem;
       AlreadyWithinAList: boolean);

    { @name writes the preamble of a LaTeX document and the begining of the
      document itself up through the table of contents.}
    procedure WriteStartOfDocument(AName: string);

    { Starts an LaTeX paragraph element by writing '\par'. }
    procedure WriteStartOfParagraph;

  {$IFDEF unused}
    procedure WriteStartOfTable1Column(t: string);
    procedure WriteStartOfTable2Columns(t1, t2: string);
    procedure WriteStartOfTable3Columns(t1, t2, T3: string);
    procedure WriteStartOfTableRow;
  {$ELSE}
  {$ENDIF}

    procedure WriteItemsSummary(const Items: TPasItems);

    { Writes information about all Items.
      If ShowVisibility then their Visibility will also be shown. }
    procedure WriteItemsDetailed(const HL: integer;
      const Items: TPasItems; ShowVisibility: boolean;
      SectionName: TTranslationId);

    procedure WriteFieldsProperties(HL: integer;
      const Items: TPasItems; ShowVisibility: boolean;
      SectionName: TTranslationId);

    procedure WriteAnchor(ItemName, Link: string);

    { Writes a single class, interface or object CIO to output, at heading
      level HL. }
    procedure WriteCIO(HL: integer; const CIO: TPasCio);

    { Calls @link(WriteCIO) with each element in the argument collection C,
      using heading level HL. }
    procedure WriteCIOs(HL: integer; c: TPasItems);

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

    { Writes heading S to output, at heading level I.
      For HTML, only levels 1 to 6 are valid, so that values smaller
      than 1 will be set to 1 and arguments larger than 6 are set to 6.
      The String S will then be enclosed in an element from H1 to H6,
      according to the level. }
    procedure WriteHeading(HL: integer; const s: string);

    { Writes dates Created and LastMod at heading level HL to output
      (if at least one the two has a value assigned). }
    procedure WriteDates(const HL: integer; const Created, LastMod: string);
    procedure SetLatexHead(const Value: TStrings);
    function FormatHeading(HL: integer; const s: string): string;
  protected

    function ConvertString(const s: string): string; override;

    { Called by @link(ConvertString) to convert a character.
      Will convert special characters to their html escape sequence
      -> test }
    function ConvertChar(c: char): String; override;

    procedure WriteUnit(const HL: integer; const U: TPasUnit); override;

    function LatexString(const S: string): string; override;

    // Makes a String look like a coded String, i.e.
    // '\begin{ttfamily}TheString\end{ttfamily}'
    //  in LaTeX. }
    function CodeString(const s: string): string; override;

    { Returns a link to an anchor within a document. LaTeX simply concatenates
      the strings with either a "-" or "." character between them. }
    function CreateLink(const Item: TBaseItem): string; override;

    procedure WriteStartOfCode; override;
    procedure WriteEndOfCode; override;

    function Paragraph: string; override;

    function ShortDash: string; override;

    function LineBreak: string; override;

    function URLLink(const URL: string): string; override;

    procedure WriteExternalCore(const ExternalItem: TExternalItem;
      const Id: TTranslationID); override;

{ TODO : FormatKeyWord, FormatCompilerComment, FormatComment,
FormatString and FormatPascalCode are all closely related.
Maybe they should be extracted into an abstract base class.  There
could be descendents of the abstract ancestor associated with the HTML and
Latex DocGenerators.}

    // @name is called from within @link(FormatPascalCode)
    // to return AString in a bold font.
    function FormatKeyWord(AString: string): string; override;

    // @name is called from within @link(FormatPascalCode) to
    // return AString in italics.
    function FormatCompilerComment(AString: string): string; override;

    // @name is called from within @link(FormatPascalCode) to
    // return AString in italics.
    function FormatComment(AString: string): string; override;

    function FormatAnchor(const Anchor: string): string; override;

    function MakeItemLink(const Item: TBaseItem;
      const LinkCaption: string;
      const LinkContext: TLinkContext): string; override;

    function FormatBold(const Text: string): string; override;
    function FormatItalic(const Text: string): string; override;

    function FormatPreformatted(const Text: string): string; override;

    function FormatImage(FileNames: TStringList): string; override;

    function FormatList(ListData: TListData): string; override;

    function FormatTable(Table: TTableData): string; override;
  public
    // @name is intended to format Line as if it were Object Pascal
    // code in Delphi or Lazarus.  However, unlike Lazarus and Delphi,
    // colored text is not used because printing colored text tends to
    // be much more expensive than printing all black text.
    function FormatPascalCode(const Line: string): string; override;

    { Returns Latex file extension ".tex". }
    function GetFileExtension: string; override;
    { The method that does everything --- writes documentation for all units
      and creates overview files. }
    procedure WriteDocumentation; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EscapeURL(const AString: string): string; virtual;

    function FormatSection(HL: integer; const Anchor: string;
      const Caption: string): string; override;
  published
    { Indicate if the output must be simplified for latex2rtf }
    property Latex2rtf: boolean read FLatex2rtf write FLatex2rtf default false;
    // The strings in @name are inserted directly into the preamble of
    // the LaTeX document.  Therefore they must be valid LaTeX code.
    property LatexHead: TStrings read FLatexHead write SetLatexHead;
  end;

implementation

uses
  SysUtils,
  PasDoc_Base,
  PasDoc_ObjectVector,
  PasDoc_Utils,
  PasDoc_StringPairVector,
  StrUtils,
  PasDoc_Versions;

function TTexDocGenerator.LatexString(const S: string): string;
begin
  Result := S;
end;

function TTexDocGenerator.FormatPascalCode(const Line: string): string;
var
  AStringList: TStringList;
  LineIndex: integer;
  ALine: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := inherited FormatPascalCode(Line);

    for LineIndex := 0 to AStringList.Count -1 do
    begin
      ALine := AStringList[LineIndex];
      ALine := SCharsReplace(ALine, [' '], '~') + '\\';
      if LineIndex < AStringList.Count -1 then
      begin
        ALine := ALine + '\nopagebreak[3]'
      end;
      AStringList[LineIndex] := ALine;
    end;

    result := '\texttt{' + AStringList.Text + '}';

  finally
    AStringList.Free;
  end;
end;

function TTexDocGenerator.CodeString(const s: string): string;
begin
  Result := '\begin{ttfamily}' + s + '\end{ttfamily}';
end;

function TTexDocGenerator.CreateLink(const Item: TBaseItem): string;
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
        Result := TPasItem(Item).MyUnit.Name + '.' + Item.Name;
      end else begin
        { it's a constant, a variable, a type or a function / procedure }
        Result := TPasItem(Item).MyUnit.FullLink + '-' + Item.Name;
      end;
    end;
  end else begin
    Result := Item.Name;
  end;
end;

function TTexDocGenerator.MakeItemLink(const Item: TBaseItem;
  const LinkCaption: string;
  const LinkContext: TLinkContext): string;
begin
  if LinkContext = lcCode then
    { Links inside lcCode context look bad... }
    Result := ConvertString(LinkCaption) else
    Result :=  '\begin{ttfamily}' + ConvertString(LinkCaption) +
      '\end{ttfamily}(\ref{' + EscapeURL(Item.FullLink) + '})';
end;

function TTexDocGenerator.GetFileExtension: string;
begin
  Result := '.tex';
end;

procedure TTexDocGenerator.WriteAppInfo;
begin
  if not ExcludeGenerator then
    WriteDirectLine('% '+ FLanguage.Translation[trGeneratedBy] + ' ' +
      PASDOC_HOMEPAGE + PASDOC_NAME_AND_VERSION);
  if IncludeCreationTime then
    WriteDirectLine('% ' + FLanguage.Translation[trGeneratedOn] + ' ' +
      FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
end;

procedure TTexDocGenerator.WriteAuthors(HL: integer; Authors: TStringVector);
var
  i: Integer;
  s, S1, S2: string;
  EmailAddress: string;
begin
  if IsEmpty(Authors) then Exit;

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

  if CIO.ClassDirective = CT_HELPER then
    s := ' for ' + CIO.HelperTypeIdentifier else s := '';

  WriteHeading(HL+1,CIO.Name+' '+ConvertString(GETCIOTypeName(CIO.MyType))
    + ConvertString(GetClassDirectiveName(CIO.ClassDirective)) + ConvertString(s));
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
          Item := CIO.FirstAncestor;
          if Assigned(Item) and (Item is TPasCio) then
            begin
              repeat
                s := MakeItemLink(Item, Item.Name, lcNormal);
                WriteDirect(s);

                if TPasCio(Item).Ancestors.Count <> 0 then
                  begin
                    s := TPasCio(Item).Ancestors.FirstName;
                    Item := TPasCio(Item).FirstAncestor;

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
      WriteItemLongDescription(CIO, false);
    end
  else
      WriteDirect('%%%%' + SectionHeads[dsDescription],true);

  WriteFieldsProperties(HL + 2, CIO.Properties, CIO.ShowVisibility, trProperties);

  WriteFieldsProperties(HL + 2, CIO.Fields, CIO.ShowVisibility, trFields);

  WriteItemsDetailed(HL + 2, CIO.Methods, CIO.ShowVisibility, trMethods);

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

procedure TTexDocGenerator.WriteCodeWithLinks(const p: TPasItem;
  const Code: string; WriteItemLink: boolean);
begin
  WriteCodeWithLinksCommon(p, Code, WriteItemLink, '', '');
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
        if IncludeCreationTime then
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
    WriteDirectLine(Created);
    WriteEndOfParagraph;
  end;
  if LastMod <> '' then begin
    WriteHeading(HL, FLanguage.Translation[trLastModified]);
    WriteStartOfParagraph;
    WriteDirectLine(LastMod);
    WriteEndOfParagraph;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteDocumentation;
var
 OutputFileName: string;
begin
  StartSpellChecking('tex');
  inherited;

  if ProjectName <> '' then
    OutputFileName := ProjectName + '.tex'
  else
    OutputFileName := 'docs.tex';
  if not CreateStream(OutputFileName) then Exit;
  WriteStartOfDocument('');
  WriteIntroduction;
  WriteUnits(1);
  WriteAdditionalFiles;
  WriteConclusion;
  WriteEndOfDocument;
  CloseStream;
  EndSpellChecking;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteEndOfDocument;
begin
  WriteDirect('\end{document}',true);
end;

procedure TTexDocGenerator.WriteEndOfCode;
begin
  WriteDirect('\end{ttfamily}',true);
end;

procedure TTexDocGenerator.WriteEndOfParagraph;
begin
  WriteDirectLine('');
  WriteDirect('',true);
end;

{$IFDEF unused}
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
{$ELSE}
{$ENDIF}
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
      WriteCodeWithLinks(p, itemdesc, false);
      WriteDirect('',true);
      WriteEndFlushLeft;
    end
  else
    begin
      WriteDirect('\item[\textbf{'+convertstring(itemname)+'}\hfill]',true);
      WriteStartFlushLeft;
      WriteCodeWithLinks(p, itemdesc, false);
      WriteEndFlushLeft;
      WriteDirect('',true);
    end;
end;

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteItemsDetailed(const HL: integer;
  const Items: TPasItems; ShowVisibility: boolean; SectionName: TTranslationId);
var
  j: Integer;
  Item: TPasItem;
  s: string;
  Visibility: string;
begin
  if ObjectVectorIsNilOrEmpty(Items) then Exit;

  WriteHeading(HL, FLanguage.Translation[SectionName]);

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

    if ShowVisibility then
      Visibility := string(VisibilityStr[Item.Visibility]) + ' ' else
      Visibility := '';
    WriteDeclarationItem(Item, FLanguage.Translation[trDeclaration],
      Visibility + Item.FullDeclaration);

    if HasDescription(Item) then
    begin
      WriteStartOfParagraph;
      WriteDirect('\item[\textbf{'+FLanguage.Translation[trDescription]+'}]',true);
      WriteItemLongDescription(Item, true);
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

function TTexDocGenerator.FormatHeading(HL: integer; const s: string): string;
begin
  if (HL < 1) then HL := 1;
  if HL > 5 then begin
    DoMessage(2, pmtWarning, 'LaTeX generator cannot write headlines of '
      + 'level 5 or greater; will create a new paragraph instead.', []);
    HL := 5;
  end;
  case HL of
    1: begin
         result :=  '\chapter{'
           + ConvertString(s)
           + '}' + LineEnding;
       end;
    2: begin
         result :=  '\section{'
           + ConvertString(s)
           + '}' + LineEnding;
       end;
    3: begin
          if latex2rtf then
            begin
               result := '\subsection*{'
                 + ConvertString(s)
                 + '}' + LineEnding;
            end
          else
            begin
              result := '\ifpdf' + LineEnding
                + '\subsection*{'
                +'\large{\textbf{'+ConvertString(s)+'}}\normalsize\hspace{1ex}'+
                '\hrulefill'
                + '}'
                + '\else'+ LineEnding

                + '\subsection*{'
                + ConvertString(s)
                + '}'+ LineEnding
                + '\fi'+ LineEnding;
            end;
       end;
    4: begin
         result := '\subsubsection*{'
           + '\large{\textbf{'+ConvertString(s)+'}}\normalsize\hspace{1ex}'+
          '\hfill'
            + '}'+ LineEnding;
       end;
    5: begin
        result := '\paragraph*{'
          + ConvertString(s)
          + '}\hspace*{\fill}'+ LineEnding
          + ''+ LineEnding;
       end;
  end;
end;

procedure TTexDocGenerator.WriteHeading(HL: integer; const s: string);
begin
  if (HL < 1) then HL := 1;
  if HL > 5 then begin
    DoMessage(2, pmtWarning, 'LaTeX generator cannot write headlines of '
      + 'level 5 or greater; will a new paragraph instead.', []);
    HL := 5;
  end;
  case HL of
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

function TTexDocGenerator.HasDescription(const AItem: TPasItem): boolean;
var
  Ancestor: TBaseItem;
begin
  Result := false;
  if not Assigned(AItem) then Exit;

  Result := AItem.HasDescription or
    { TPasEnum always has some description: list of it's members }
    (AItem is TPasEnum) or
    { Some hint directive ? }
    (AItem.HintDirectives <> []) or
    AItem.HasOptionalInfo or
    { Seealso section ? }
    (AItem.SeeAlso.Count <> 0);

  if Result then Exit;

  if (AItem is TPasCio) and
     (TPasCio(AItem).Ancestors.Count <> 0) then
  begin
    Ancestor := TPasCio(AItem).FirstAncestor;
    if Assigned(Ancestor) and (Ancestor is TPasItem) then
    begin
      HasDescription := HasDescription(TPasItem(Ancestor));
      exit;
    end;
  end;
end;

procedure TTexDocGenerator.WriteItemLongDescription(const AItem: TPasItem;
  AlreadyWithinAList: boolean);

  { writes the parameters or exceptions list }
  procedure WriteParamsOrRaises(ItemToSearchFrom: TPasItem; const Caption: string;
    List: TStringPairVector; LinkToParamNames: boolean);

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
    ParamName: string;
  begin
    if objectVectorIsNilOrEmpty(List) then
      Exit;

    WriteDirect('\item[\textbf{'+Caption+'}]',true);
    WriteDirect('\begin{description}',true);
    { Terrible hack : To fix and replace by a clean solution,
      we need to add an empty item so that the list starts
      at the correct margin.
    }
{    WriteDirect('\item',true);}
    for i := 0 to List.Count - 1 do
    begin
      ParamName := List[i].Name;

      if LinkToParamNames then
       ParamName := SearchLink(ParamName, ItemToSearchFrom, '', true);

      WriteParameter(ParamName, List[i].Value);
    end;
    WriteDirect('\end{description}',true);
  end;

  procedure WriteSeeAlso(SeeAlso: TStringPairVector);
  var
    i: integer;
    SeeAlsoItem: TBaseItem;
    SeeAlsoLink: string;
  begin
    if ObjectVectorIsNilOrEmpty(SeeAlso) then
      Exit;

    if not AlreadyWithinAList then
      WriteStartList(FLanguage.Translation[trSeeAlso]);

    WriteDirect('\item[\textbf{' + FLanguage.Translation[trSeeAlso] + '}]',true);
    WriteDirect('\begin{description}',true);

    for i := 0 to SeeAlso.Count - 1 do
    begin
      SeeAlsoLink := SearchLink(SeeAlso[i].Name, AItem,
        SeeAlso[i].Value, true, SeeAlsoItem);
      WriteDirect('\item[');
      if SeeAlsoItem <> nil then
        WriteDirect(SeeAlsoLink) else
        WriteConverted(SeeAlso[i].Name);
      WriteDirectLine('] ');

      if (SeeAlsoItem <> nil) and (SeeAlsoItem is TPasItem) then
        WriteDirect(TPasItem(SeeAlsoItem).AbstractDescription);
      WriteDirectLine('');
    end;
    WriteDirect('\end{description}',true);

    if not AlreadyWithinAList then
      WriteEndList;
  end;

  procedure WriteReturnDesc(ReturnDesc: string);
  begin
    if ReturnDesc = '' then
      exit;
    WriteDirect('\item[\textbf{'+FLanguage.Translation[trReturns]+'}]');
    WriteSpellChecked(ReturnDesc);
    WriteDirect('',true);
  end;

  procedure WriteHintDirective(const S: string; const Note: string = '');
  var
    Text: string;
  begin
    Text := FLanguage.Translation[trWarning] + ': ' + S;
    if Note <> '' then
      Text := Text + ': ' + Note else
      Text := Text + '.';
    Text := Text + LineEnding + LineEnding;
    WriteConverted(Text);
  end;

var
  Ancestor: TBaseItem;
  AncestorName: string;
  EnumMember: TPasItem;
  i: Integer;
begin
  if not Assigned(AItem) then Exit;

  if hdDeprecated in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trDeprecated], AItem.DeprecatedNote);
  if hdPlatform in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trPlatformSpecific]);
  if hdLibrary in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trLibrarySpecific]);
  if hdExperimental in AItem.HintDirectives then
    WriteHintDirective(FLanguage.Translation[trExperimental]);

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
      if (AItem is TPasCio) and
         (TPasCio(AItem).Ancestors.Count <> 0) then
      begin
        AncestorName := TPasCio(AItem).Ancestors.FirstName;
        Ancestor := TPasCio(AItem).FirstAncestor;
        if Assigned(Ancestor) and (Ancestor is TPasItem) then
          begin
            WriteConverted(Format('no description available, %s description follows', [AncestorName]));
            WriteItemLongDescription(TPasItem(Ancestor), AlreadyWithinAList);
          end;
      end else
      begin
        WriteDirect(' ');
      end;
    end;
  end;

  if AItem.HasOptionalInfo then
  begin
    WriteStartOfParagraph;
    WriteParamsOrRaises(AItem, FLanguage.Translation[trParameters],
      AItem.Params, false);
    if AItem is TPasMethod then
      WriteReturnDesc(TPasMethod(AItem).Returns);

    { In LaTeX generator I use trExceptions, not trExceptionsRaised,
      because trExceptionsRaised is just too long and so everything
      would look too ugly. However it's preferred to use
      trExceptionsRaised in the future (then trExceptions can be simply
      removed from PasDoc_Languages), because trExceptionsRaised
      is just more understandable to the reader of documentation. }
    WriteParamsOrRaises(AItem, FLanguage.Translation[trExceptions],
      AItem.Raises, true);
  end;

  WriteSeeAlso(AItem.SeeAlso);

  if AItem is TPasEnum then
  begin
    WriteDirect('\item[\textbf{' + FLanguage.Translation[trValues] + '}]',true);
    WriteDirectLine('\begin{description}');
    for i := 0 to TPasEnum(AItem).Members.Count - 1 do
    begin
      EnumMember := TPasEnum(AItem).Members.PasItemAt[i];
      WriteDirect('\item[\texttt{');
      { Use EnumMember.FullDeclaration, not just EnumMember.Name.
        This is important for enums with explicit numeric value, like "me1 := 1". }
      WriteConverted(EnumMember.FullDeclaration);
      WriteDirect('}] ');
      { We have to place anchor outside of the \item[], otherwise it doesn't work... }
      WriteAnchor('', EnumMember.FullLink);
      WriteItemLongDescription(EnumMember, false);
      WriteDirectLine('');
    end;
    WriteDirectLine('\end{description}');
  end;
end;

procedure TTexDocGenerator.WriteFieldsProperties(HL: integer;
  const Items: TPasItems; ShowVisibility: boolean; SectionName: TTranslationId);
var
  j: Integer;
  Item: TPasItem;
  s, Visibility: string;
begin
  if FLatex2Rtf then
    WriteItemsDetailed(HL, Items, ShowVisibility, SectionName) else
  begin
    if ObjectVectorIsNilOrEmpty(Items) then Exit;

    WriteHeading(HL, FLanguage.Translation[SectionName]);

    { Determine the longest string used.
      This is the one we will use for determining the label width.
    }
    s:='';
    for j := 0 to Items.Count - 1 do
    begin
      Item := Items.PasItemAt[j];
      if length(s) < length(Item.Name) then
        s := Item.Name;
    end;

    WriteStartList(s);

    for j := 0 to Items.Count - 1 do
    begin
      Item := Items.PasItemAt[j];
      WriteAnchor(Item.Name, Item.FullLink);

      if ShowVisibility then
        Visibility := string(VisibilityStr[Item.Visibility]) + ' ' else
        Visibility := '';
      WriteDeclarationItem(Item, Item.Name,
        Visibility + Item.FullDeclaration);

      WriteDirectLine('');
      WriteDirect('\par ');
      WriteItemLongDescription(Item, true);
    end;

    WriteEndList;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TTexDocGenerator.WriteAnchor(ItemName, Link: string);
begin
  { No links in RTF documentation -- latex2rtf can't really handle them.
    docs.aux must be generated by user using some normal latex program
    (like latex or pdflatex), and then passed to latex2rtf.
    See Latex2RtfOutput page on pasdoc wiki. }
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
var
  Index: integer;
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

  for Index := 0 to LatexHead.Count -1 do
  begin
    WriteDirect(LatexHead[Index], true);
  end;

  if not FLatex2Rtf then
  begin
    WriteDirect('\setlength{\textwidth}{\paperwidth}',true);
    WriteDirect('\addtolength{\textwidth}{-2in}',true);
  end
  else
    WriteDirect('\textwidth 16.5cm',true);
  WriteDirect('',true);
  WritePDFIfDef;

  { Use graphicx package (for @image tag) }
  WriteDirectLine(
    '\ifpdf' + LineEnding +
    '  \usepackage[pdftex]{graphicx}' + LineEnding +
    '\else' + LineEnding +
    '  \usepackage[dvips]{graphicx}' + LineEnding +
    '\fi');

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

{$IFDEF unused}
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

procedure TTexDocGenerator.WriteStartOfTableRow;
begin
  CellCounter := 0;
end;
{$ELSE}
{$ENDIF}

{ ---------------------------------------------------------------------------- }

procedure TTexDocGenerator.WriteUnit(const HL: integer; const U: TPasUnit);

  procedure WriteUnitDescription(HL: integer; U: TPasUnit);
  begin
    WriteHeading(HL, FLanguage.Translation[trDescription]);
    WriteItemLongDescription(U, false);
    WriteDirect('',true);
  end;

  procedure WriteUnitUses(const HL: integer; U: TPasUnit);
  var
    i: Integer;
    ULink: TPasItem;
  begin
    if WriteUsesClause and not IsEmpty(U.UsesUnits) then begin
      WriteHeading(HL, FLanguage.Translation[trUses]);
      WriteDirect('\begin{itemize}',true);
      for i := 0 to U.UsesUnits.Count-1 do begin
        WriteDirect('\item ');
        ULink := TPasUnit(U.UsesUnits.Objects[i]);
        if ULink <> nil then
        begin
          WriteDirect(MakeItemLink(ULink, U.UsesUnits[i], lcNormal));
        end else
        begin
          { MakeItemLink writes link names in tt font, so we follow
            the convention here and also use tt font. }
          WriteDirect('\begin{ttfamily}' +
            ConvertString(U.UsesUnits[i]) + '\end{ttfamily}');
        end;
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

  procedure ConditionallyAddSection(Section: TSections; Condition: boolean);
  begin
    if Condition then
      Include(SectionsAvailable, Section);
  end;

begin
  SectionsAvailable := [dsDescription];
  ConditionallyAddSection(dsUses, WriteUsesClause and not IsEmpty(U.UsesUnits));
  ConditionallyAddSection(dsClasses, not ObjectVectorIsNilOrEmpty(U.CIOs));
  ConditionallyAddSection(dsFuncsProcs, not ObjectVectorIsNilOrEmpty(U.FuncsProcs));
  ConditionallyAddSection(dsTypes, not ObjectVectorIsNilOrEmpty(U.Types));
  ConditionallyAddSection(dsConstants, not ObjectVectorIsNilOrEmpty(U.Constants));
  ConditionallyAddSection(dsVariables, not ObjectVectorIsNilOrEmpty(U.Variables));

  DoMessage(2, pmtInformation, 'Writing Docs for unit "%s"', [U.Name]);

  if U.IsUnit then
    WriteHeading(HL, FLanguage.Translation[trUnit] + ' ' + U.Name)
  else if U.IsProgram then
    WriteHeading(HL, FLanguage.Translation[trProgram] + ' ' + U.Name)
  else
    WriteHeading(HL, FLanguage.Translation[trLibrary] + ' ' + U.Name);

  WriteAnchor(U.Name, U.FullLink);

  if HasDescription(U) then
    WriteUnitDescription(HL + 1, U);

  WriteUnitUses(HL + 1, U);

  if (U.CIOs.count <> 0) or (U.FuncsProcs.count <> 0) then
    WriteHeading(HL + 1, FLanguage.Translation[trOverview]);

  WriteItemsSummary(U.CIOs);
  WriteItemsSummary(U.FuncsProcs);

  WriteCIOs(HL + 1, U.CIOs);

  WriteItemsDetailed(HL + 1, U.FuncsProcs, false, trFunctionsAndProcedures);

  WriteItemsDetailed(HL + 1, U.Types, false, trTypes);

  WriteItemsDetailed(HL + 1, U.Constants, false, trConstants);

  WriteItemsDetailed(HL + 1, U.Variables, false, trVariables);

  WriteAuthors(HL + 1, U.Authors);
  WriteDates(HL + 1, U.Created, U.LastMod);
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

function TTexDocGenerator.EscapeURL(const AString: string): string;
begin
  EscapeURL := AString;
end;

function TTexDocGenerator.Paragraph: string;
begin
  Result := LineEnding + LineEnding;
end;

function TTexDocGenerator.ShortDash: string;
begin
  Result := '{-}';
end;

function TTexDocGenerator.LineBreak: string;
begin
  (* We add '{}' at the end. Otherwise there would be LaTeX errors
     if user will put "[" character right after @br, producing
     LaTeX code that looks like: "...\\ [...".
     In such case (even if you put newline between \\ and [)
     LaTeX would think that you want to use optional \\ argument,
     "\\[extra-space]". Putting '{}' tells LaTeX explicitly
     that this "\\" command doesn't take the "extra-space" argument. *)
  Result := '\\{}';
end;

function TTexDocGenerator.URLLink(const URL: string): string;
begin
  if Latex2Rtf then
    (* latex2rtf doesn't understand \href (well, actually it doesn't
       understand \usepackage{hyperref} at all) *)
    Result := ConvertString(URL) else
    Result := '\href{' + EscapeURL(URL) + '}{' + ConvertString(URL) + '}';
end;

procedure TTexDocGenerator.WriteExternalCore(
  const ExternalItem: TExternalItem;
  const Id: TTranslationID);
var
  HL: integer;
begin
  HL := 1;

  WriteHeading(HL, ExternalItem.Title);
  WriteAnchor(ExternalItem.Name, ExternalItem.FullLink);

  WriteSpellChecked(ExternalItem.DetailedDescription);

  WriteAuthors(HL + 1, ExternalItem.Authors);
  WriteDates(HL + 1, ExternalItem.Created, ExternalItem.LastMod);
end;

constructor TTexDocGenerator.Create(AOwner: TComponent);
begin
  inherited;
  FLatexHead := TStringList.Create;
  FImages := TStringList.Create;
end;

destructor TTexDocGenerator.Destroy;
begin
  FImages.Free;
  FLatexHead.Free;
  inherited;
end;

procedure TTexDocGenerator.SetLatexHead(const Value: TStrings);
begin
  FLatexHead.Assign(Value);
end;

function TTexDocGenerator.FormatKeyWord(AString: string): string;
begin
  result := '}\textbf{' + ConvertString(AString) + '}\texttt{';
end;

function TTexDocGenerator.FormatCompilerComment(AString: string): string;
begin
  result := '\textit{' + ConvertString(AString) + '}';
end;

function TTexDocGenerator.FormatComment(AString: string): string;
begin
  result := '\textit{' + ConvertString(AString) + '}';
end;

function TTexDocGenerator.FormatAnchor(const Anchor: string): string;
begin
  { No links in RTF documentation -- latex2rtf can't really handle them.
    docs.aux must be generated by user using some normal latex program
    (like latex or pdflatex), and then passed to latex2rtf.
    See Latex2RtfOutput page on pasdoc wiki. }
  if not FLatex2rtf then
  begin
    result := '\label{'+Anchor+'}' + LineEnding
  end
  else
  begin
    result := '';
  end;
end;

function TTexDocGenerator.FormatSection(HL: integer;
  const Anchor, Caption: string): string;
begin
  { We use `HL + 1' because user is allowed to use levels
    >= 1, and heading level 1 is reserved for section title. }
  result := FormatAnchor(Anchor)
    + FormatHeading(HL + 1, Caption);
end;

function TTexDocGenerator.FormatBold(const Text: string): string;
begin
  Result := '\textbf{' + Text + '}';
end;

function TTexDocGenerator.FormatItalic(const Text: string): string;
begin
  Result := '\textit{' + Text + '}';
end;

function TTexDocGenerator.FormatPreformatted(const Text: string): string;
begin
  Result := '\begin{verbatim}' + Text +  '\end{verbatim}';
end;

function TTexDocGenerator.FormatList(ListData: TListData): string;
const
  ListEnvironment: array[TListType]of string =
  ( 'itemize', 'enumerate', 'description' );
var
  ListItem: TListItemData;
  i: Integer;
begin
  { LaTeX doesn't allow empty lists }
  if ListData.Count <> 0 then
  begin
    Result := Format('\begin{%s}',
      [ListEnvironment[ListData.ListType]]) + LineEnding;
    for i := 0 to ListData.Count - 1 do
    begin
      ListItem := ListData.Items[i] as TListItemData;

      if ListData.ListType = ltDefinition then
      begin
        Result := Result +
          '\item[' + ListItem.ItemLabel + '] ' + ListItem.Text + LineEnding;
      end else
      begin
        if ListData.ListType = ltOrdered then
          Result := Result +
            { We don't know here which counter we should set to Index.
              So we just set *all* four counters. Simple, and works. }
            Format(
              '\setcounter{enumi}{%d} ' +
              '\setcounter{enumii}{%0:d} ' +
              '\setcounter{enumiii}{%0:d} ' +
              '\setcounter{enumiv}{%0:d} ' + LineEnding,
              [ { Note that we set ListItem.Index - 1, so that resulting
                  document will correctly display ListItem.Index.
                  That's how LaTeX  works. }
                ListItem.Index - 1 ]);

        Result := Result +
          '\item ' + ListItem.Text + LineEnding;
      end;
    end;
    Result := Result +
      Format('\end{%s}', [ListEnvironment[ListData.ListType]]);
  end;
end;

function TTexDocGenerator.FormatTable(Table: TTableData): string;
var
  RowNum, ColNum: Integer;
  Row: TRowData;

  function CellContent(Row: TRowData; ColNum: Integer): string;
  begin
    Result := Row.Cells[ColNum];
    if Row.Head then
      Result := FormatBold(Result);
  end;

begin
  Result := Paragraph + '\begin{tabular}{' +
    DupeString('|l', Table.MaxCellCount) + '|}' + LineEnding +
    '\hline' + LineEnding;
  for RowNum := 0 to Table.Count - 1 do
  begin
    Row := Table.Items[RowNum] as TRowData;

    for ColNum := 0 to Row.Cells.Count - 2 do
      Result := Result + CellContent(Row, ColNum) + ' & ';

    { No '&' after the last cell of a row. }
    Result := Result + CellContent(Row, Row.Cells.Count - 1) +
      ' \\ \hline' + LineEnding;
  end;
  Result := Result + '\end{tabular}' + Paragraph;
end;

function TTexDocGenerator.FormatImage(FileNames: TStringList): string;

  { This behaves like FormatImage would behave if we would
    take only pdflatex into account. Returns the filename that
    you should insert into the output. }
  function FormatImagePdf: string;
  var
    ChosenFileName: string;
    ImageId, I: Integer;
    CopyNeeded: boolean;
  begin
    { Calculate ChosenFileName, i.e. choose right image format for pdf.
      pdf extension is preferred, otherwise png or jpg. }
    ChosenFileName := '';
    for I := 0 to FileNames.Count - 1 do
      if LowerCase(ExtractFileExt(FileNames[I])) = '.pdf' then
      begin
        ChosenFileName := FileNames[I];
        Break;
      end;
    if ChosenFileName = '' then
    begin
      for I := 0 to FileNames.Count - 1 do
        if (LowerCase(ExtractFileExt(FileNames[I])) = '.jpg') or
           (LowerCase(ExtractFileExt(FileNames[I])) = '.jpeg') or
           (LowerCase(ExtractFileExt(FileNames[I])) = '.png') then
        begin
          ChosenFileName := FileNames[I];
          Break;
        end;

      if ChosenFileName = '' then
        ChosenFileName := FileNames[0];
    end;

    { Calculate ImageId and CopyNeeded }
    ImageId := FImages.IndexOf(ChosenFileName);
    CopyNeeded := ImageId = -1;
    if CopyNeeded then
      ImageId := FImages.Add(ChosenFileName);

    Result := 'image_' + IntToStr(ImageId) + ExtractFileExt(ChosenFileName);

    if CopyNeeded then
      CopyFile(ChosenFileName, DestinationDirectory + Result);
  end;

  { This behaves like FormatImage would behave if we would
    take only dvi output (i.e. normal latex) into account.
    Returns the filename that you should insert into the output. }
  function FormatImageDvi: string;
  var
    ChosenFileName: string;
    ImageId, I: Integer;
    CopyNeeded: boolean;
  begin
    { Calculate ChosenFileName, i.e. choose right image format for dvi.
      eps extension is preferred. }
    ChosenFileName := '';
    for I := 0 to FileNames.Count - 1 do
      if LowerCase(ExtractFileExt(FileNames[I])) = '.eps' then
      begin
        ChosenFileName := FileNames[I];
        Break;
      end;
    if ChosenFileName = '' then
      ChosenFileName := FileNames[0];

    { Calculate ImageId and CopyNeeded }
    ImageId := FImages.IndexOf(ChosenFileName);
    CopyNeeded := ImageId = -1;
    if CopyNeeded then
      ImageId := FImages.Add(ChosenFileName);

    Result := 'image_' + IntToStr(ImageId) + ExtractFileExt(ChosenFileName);

    if CopyNeeded then
      CopyFile(ChosenFileName, DestinationDirectory + Result);
  end;

var
  SImagePdf, SImageDvi: string;
begin
  { I call FormatImageXxx funcs first, before calculating Result,
    to make sure that FormatImageXxx are called in determined order.

    Otherwise e.g. FPC 2.0.4 calculated S := S1 + S2 by calling
    S2 function first, while FPC 2.1.3 (fixes_2_2 branch) and 2.3.1
    (trunk) call S1 first. The determined order is not needed for
    pasdoc correctness (they actually *can* be called in any order),
    but it's needed to make results determined --- e.g. for comparing
    two test results, like with our ../../tests/. }
  SImageDvi := FormatImageDvi;
  SImagePdf := FormatImagePdf;

  Result :=
    '\begin{figure}' + LineEnding +
    '  \ifpdf' + LineEnding +
    '    \includegraphics{' + EscapeURL(SImagePdf) + '}' + LineEnding +
    '  \else' + LineEnding +
    '    \includegraphics{' + EscapeURL(SImageDvi) + '}' + LineEnding +
    '  \fi' + LineEnding +
    '\end{figure}' + LineEnding;
end;

end.
