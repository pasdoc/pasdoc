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

{ @abstract(Generate HtmlHelp output.) }
unit PasDoc_GenHtmlHelp;

{$I pasdoc_defines.inc}

interface

uses PasDoc_GenHtml, PasDoc_Utils, PasDoc_SortSettings;

type
  THTMLHelpDocGenerator = class(TGenericHTMLDocGenerator)
  private
    FContentsFile: string;

    { Writes the topic files for Html Help Generation }
    procedure WriteHtmlHelpProject;
  public
    procedure WriteDocumentation; override;
  published
    { Contains Name of a file to read HtmlHelp Contents from.
      If empty, create default contents file. }
    property ContentsFile: string read FContentsFile write FContentsFile;
  end;

implementation

uses SysUtils, PasDoc_Types, PasDoc_StringVector, PasDoc_Base, PasDoc_Items,
  PasDoc_Languages, PasDoc_Gen, PasDoc_Versions;

{ HtmlHelp Content Generation inspired by Wim van der Vegt <wvd_vegt@knoware.nl> }

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
  while (l > 0) and IsCharInSet(p^, [' ', #9]) do begin
    Inc(Result);
    Inc(p);
    Dec(l);
  end;
  Delete(s, 1, Result);
end;

{ THTMLHelpDocGenerator ------------------------------------------------------ }

procedure THTMLHelpDocGenerator.WriteDocumentation;
begin
  inherited;
  WriteHtmlHelpProject;
end;

procedure THTMLHelpDocGenerator.WriteHtmlHelpProject;
var
  DefaultContentsWritten: Boolean;
  DefaultTopic: string;

  procedure WriteLiObject(const Name, Local: string);
  begin
    WriteDirectLine('<li><object type="text/sitemap">');
    WriteDirectLine('<param name="Name" value="' + Name + '">');
    if Local <> '' then begin
      WriteDirectLine('<param name="Local" value="' + Local + '">');
      if DefaultTopic = '' then
        DefaultTopic := Local;
    end;
    WriteDirectLine('</object>');
  end;

  { ---------- }

  procedure WriteItemCollection(const _Filename: string; const c: TPasItems);
  var
    i: Integer;
    Item: TPasItem;
  begin
    if Assigned(c) then begin
      WriteDirectLine('<ul>');
      for i := 0 to c.Count - 1 do begin
        Item := c.PasItemAt[i];
        WriteLiObject(Item.Name, _Filename + '#' + Item.Name);
      end;
      WriteDirectLine('</ul>');
    end;
  end;

  { ---------- }

  procedure WriteItemHeadingCollection(const Title, ParentLink, Anchor: string; const
    c: TPasItems);
  begin
    if Assigned(c) and (c.Count > 0) then begin
      WriteLiObject(Title, ParentLink + '#' + Anchor);
      WriteItemCollection(ParentLink, c);
    end;
  end;

  { ---------- }

  procedure NestedWriteCIO(const ClassItem: TPasCio);
  var
    I: Integer;
  begin
    WriteLiObject(ClassItem.Name, ClassItem.FullLink);
    WriteDirectLine('<ul>');

    if ClassItem.Cios.Count > 0 then
    begin
      WriteLiObject(FLanguage.Translation[trNestedCR], ClassItem.FullLink + '#@NestedCRs');
      ClassItem.Cios.SortShallow;
      WriteDirectLine('<ul>');
      for I := 0 to ClassItem.Cios.Count - 1 do
      begin
        TPasCio(ClassItem.Cios.PasItemAt[I]).Sort([ssRecordFields,
          ssNonRecordFields, ssMethods, ssProperties]);
        NestedWriteCIO(TPasCio(ClassItem.Cios.PasItemAt[I]));
      end;
      WriteDirectLine('</ul>');
    end;

    WriteItemHeadingCollection(fLanguage.Translation[trNestedTypes], ClassItem.FullLink, '@NestedTypes', ClassItem.Types);
    WriteItemHeadingCollection(fLanguage.Translation[trFields], ClassItem.FullLink, '@Fields', ClassItem.Fields);
    WriteItemHeadingCollection(fLanguage.Translation[trProperties], ClassItem.FullLink, '@Properties', ClassItem.Properties);
    WriteItemHeadingCollection(fLanguage.Translation[trMethods], ClassItem.FullLink, '@Methods', ClassItem.Methods);

    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteUnits(const Text: string);
  var
    c: TPasItems;
    j, k: Integer;
    PU: TPasUnit;
  begin
    if Text <> '' then
      WriteLiObject(Text, OverviewFilesInfo[ofUnits].BaseFileName + GetFileExtension)
    else
      WriteLiObject(FLanguage.Translation[trUnits], OverviewFilesInfo[ofUnits].BaseFileName +
        GetFileExtension);
    WriteDirectLine('<ul>');

    // Iterate all Units
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      WriteLiObject(PU.Name, PU.FullLink);
      WriteDirectLine('<ul>');

        // For each unit, write classes (if there are any).
      c := PU.CIOs;
      if Assigned(c) then begin
        WriteLiObject(FLanguage.Translation[trClasses], PU.FullLink + '#@Classes');
        WriteDirectLine('<ul>');

        for k := 0 to c.Count - 1 do
          NestedWriteCIO(TPasCio(c.PasItemAt[k]));

        WriteDirectLine('</ul>');
      end;

        // For each unit, write Functions & Procedures.
      WriteItemHeadingCollection(FLanguage.Translation[trFunctionsAndProcedures],
        PU.FullLink, '@FuncsProcs', PU.FuncsProcs);
        // For each unit, write Types.
      WriteItemHeadingCollection(FLanguage.Translation[trTypes], PU.FullLink,
        '@Types', PU.Types);
        // For each unit, write Constants.
      WriteItemHeadingCollection(FLanguage.Translation[trConstants], PU.FullLink,
        '@Constants', PU.Constants);

      WriteDirectLine('</ul>');
    end;
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteClasses(const Text: string);
  var
    c: TPasItems;
    j: Integer;
    PU: TPasUnit;
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofCios].BaseFileName + GetFileExtension;

    // Write Classes to Contents
    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trClasses], FileName);
    WriteDirectLine('<ul>');

    c := TPasItems.Create(False);
    // First collect classes
    for j := 0 to Units.Count - 1 do begin
      PU := Units.UnitAt[j];
      c.CopyItems(PU.CIOs);
    end;
    // Output sorted classes
    // TODO: Sort by sort settings rather than const values
    c.SortShallow;
    for j := 0 to c.Count - 1 do
    begin
      TPasCio(c.PasItemAt[j]).Sort([ssRecordFields, ssNonRecordFields,
                                    ssMethods, ssProperties]);
      NestedWriteCIO(TPasCio(c.PasItemAt[j]));
    end;
    c.Free;
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteClassHierarchy(const Text: string);
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofClassHierarchy].BaseFileName +
      GetFileExtension;

    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trClassHierarchy], FileName);
  end;

  { ---------- }

  procedure ContentWriteOverview(const Text: string);

    procedure WriteParam(Id: TTranslationId);
    begin
      WriteDirect('<param name="Name" value="');
      WriteConverted(FLanguage.Translation[Id]);
      WriteDirectLine('">');
    end;

  var
    Overview: TCreatedOverviewFile;
  begin
    if Text <> '' then
      WriteLiObject(Text, '')
    else
      WriteLiObject(FLanguage.Translation[trOverview], '');
    WriteDirectLine('<ul>');
    for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    begin
      WriteDirectLine('<li><object type="text/sitemap">');
      WriteParam(OverviewFilesInfo[Overview].TranslationHeadlineId);
      WriteDirect('<param name="Local" value="');
      WriteConverted(OverviewFilesInfo[Overview].BaseFileName + GetFileExtension);
      WriteDirectLine('">');
      WriteDirectLine('</object>');
    end;
    WriteDirectLine('</ul>');
  end;

  { ---------- }

  procedure ContentWriteLegend(const Text: string);
  var
    FileName: string;
  begin
    FileName := 'Legend' + GetFileExtension;
    if Text <> '' then
      WriteLiObject(Text, FileName) else
      WriteLiObject(FLanguage.Translation[trLegend], FileName);
  end;

  { ---------- }

  procedure ContentWriteGVUses();
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofGraphVizUses].BaseFileName +
      '.' + LinkGraphVizUses;

    if LinkGraphVizUses <> '' then
      WriteLiObject(FLanguage.Translation[trGvUses], FileName);
  end;

  { ---------- }

  procedure ContentWriteGVClasses();
  var
    FileName: string;
  begin
    FileName := OverviewFilesInfo[ofGraphVizClasses].BaseFileName +
      '.' + LinkGraphVizClasses;

    if LinkGraphVizClasses <> '' then
      WriteLiObject(FLanguage.Translation[trGvClasses], FileName);
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

  { ---------- }

  Procedure ContentWriteIntroduction;
  begin
    if Introduction <> nil then
    begin
      WriteLiObject(Introduction.ShortTitle, Introduction.FullLink);
    end;
  end;

  { ---------- }

  Procedure ContentWriteConclusion;
  begin
    if Conclusion <> nil then
    begin
      WriteLiObject(Conclusion.ShortTitle, Conclusion.FullLink);
    end;
  end;

  { ---------- }

  Procedure ContentWriteAdditionalFiles;
  var
    i: Integer;
  begin
    if (AdditionalFiles <> nil) and (AdditionalFiles.Count > 0) then
    begin
      for i := 0 to AdditionalFiles.Count - 1 do
      begin
        WriteLiObject(AdditionalFiles.Get(i).ShortTitle, AdditionalFiles.Get(i).FullLink);
      end;
    end;
  end;

  { ---------- }

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

  { ---------- }

  procedure CopyCiosRecursively(ADst: TPasItems; ACios: TPasItems);
    procedure AddRecursive(ACio: TPasCio);
    begin
      ADst.Add(ACio);
      ADst.CopyItems(ACio.Fields);
      ADst.CopyItems(ACio.Properties);
      ADst.CopyItems(ACio.Methods);
      ADst.CopyItems(ACio.Types);
      if ACio.Cios.Count > 0 then
        CopyCiosRecursively(ADst, ACio.Cios);
    end;
  var
    I: Integer;
  begin
    for I := 0 to ACios.Count - 1 do
      AddRecursive(TPasCio(ACios.PasItemAt[I]));
  end;

  { -------------------------------------------------------------------------- }
var
  i, j, k, l: Integer;
  CurrentLevel, Level: Integer;
  PU: TPasUnit;
  c: TPasItems;
  Item, NextItem, PreviousItem: TPasItem;
  Item2: TPasCio;
  s, Text, Link: string;
  SL: TStringVector;
  Overview: TCreatedOverviewFile;
begin
  { At this point, at least one unit has been parsed:
    Units is assigned and Units.Count > 0
    No need to test this again. }

  if not CreateStream(ProjectName + '.hhc') then Exit;
  DoMessage(2, pmtInformation, 'Writing HtmlHelp Content file "' + ProjectName
    + '"...', []);

  // File Header
  WriteDirectLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  if not ExcludeGenerator then
    WriteDirect('<meta name="GENERATOR" content="' +
      PASDOC_NAME_AND_VERSION + '">', true);
  WriteDirectLine('</head><body>');
  WriteDirectLine('<ul>');

  DefaultContentsWritten := False;
  DefaultTopic := '';
  if ContentsFile <> '' then begin
    SL := NewStringVector;
    try
      SL.LoadFromTextFileAdd(ContentsFile);
    except
      on e: Exception do
        DoMessage(1, pmtError, e.Message +
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
          WriteDirectLine('<ul>');
          Inc(CurrentLevel);
          ContentWriteCustom(Text, Link)
        end
        else
          if CurrentLevel > Level then begin
            WriteDirectLine('</ul>');
            Dec(CurrentLevel);
            while CurrentLevel > Level do begin
              WriteDirectLine('</ul>');
              Dec(CurrentLevel);
            end;
            ContentWriteCustom(Text, Link)
          end

          else begin
            DoMessage(1, pmtError, 'Invalid level ' + IntToStr(Level) +
              'in Content file (line ' + IntToStr(j) + ').', []);
            Exit;
          end;
    end;
    SL.Free;
  end;

  if not DefaultContentsWritten then begin
    ContentWriteIntroduction;
    ContentWriteUnits('');
    ContentWriteClassHierarchy(FLanguage.Translation[trClassHierarchy]);
    ContentWriteClasses('');
    ContentWriteOverview('');
    ContentWriteLegend('');
    ContentWriteGVClasses();
    ContentWriteGVUses();
    ContentWriteAdditionalFiles;
    ContentWriteConclusion;
  end;

  // End of File
  WriteDirectLine('</ul>');
  WriteDirectLine('</body></html>');
  CloseStream;

  // Create Keyword Index
  // First collect all Items
  c := TPasItems.Create(False); // Don't free Items when freeing the container

  for j := 0 to Units.Count - 1 do begin
    PU := Units.UnitAt[j];

    if Assigned(PU.CIOs) then
      CopyCiosRecursively(c, PU.CIOs);

    c.CopyItems(PU.Types);
    c.CopyItems(PU.Variables);
    c.CopyItems(PU.Constants);
    c.CopyItems(PU.FuncsProcs);
  end;

  if not CreateStream(ProjectName + '.hhk') then Exit;
  DoMessage(2, pmtInformation, 'Writing HtmlHelp Index file "%s"...',
    [ProjectName]);

  WriteDirectLine('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  WriteDirectLine('<html>');
  WriteDirectLine('<head>');
  if not ExcludeGenerator then
    WriteDirectLine('<meta name="GENERATOR" content="' +
      PASDOC_NAME_AND_VERSION + '">');
  WriteDirectLine('</head><body>');
  WriteDirectLine('<ul>');

  // Write all Items to KeyWord Index

  c.SortShallow;

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
        WriteDirectLine('<ul>');

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
        WriteDirectLine('</ul>');
      end;

      Inc(j);
    end;

      // Don't forget to write the last item. Can it ever by nil?
    WriteLiObject(Item.Name, Item.FullLink);
  end;

  c.Free;

  WriteDirectLine('</ul>');
  WriteDirectLine('</body></html>');
  CloseStream;

  // Create a HTML Help Project File
  if not CreateStream(ProjectName + '.hhp') then Exit;
  DoMessage(3, pmtInformation, 'Writing Html Help Project file "%s"...',
    [ProjectName]);

  WriteDirectLine('[OPTIONS]');
  WriteDirectLine('Binary TOC=Yes');
  WriteDirectLine('Compatibility=1.1 or later');
  WriteDirectLine('Compiled file=' + ProjectName + '.chm');
  WriteDirectLine('Contents file=' + ProjectName + '.hhc');
  WriteDirectLine('Default Window=Default');
  WriteDirectLine('Default topic=' + DefaultTopic);
  WriteDirectLine('Display compile progress=Yes');
  WriteDirectLine('Error log file=' + ProjectName + '.log');
  WriteDirectLine('Full-text search=Yes');
  WriteDirectLine('Index file=' + ProjectName + '.hhk');
  if Title <> '' then
    WriteDirectLine('Title=' + Title)
  else
    WriteDirectLine('Title=' + ProjectName);

  WriteDirectLine('');
  WriteDirectLine('[WINDOWS]');
  if Title <> '' then
    WriteDirect('Default="' + Title + '","' + ProjectName +
      '.hhc","' + ProjectName + '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true)
  else
    WriteDirect('Default="' + ProjectName + '","' +
      ProjectName + '.hhc","' + ProjectName +
      '.hhk",,,,,,,0x23520,,0x300e,,,,,,,,0', true);

  WriteDirectLine('');
  WriteDirectLine('[FILES]');

  { HHC seems to know about the files by reading the Content and Index.
    So there is no need to specify them in the FILES section. }

  WriteDirectLine('Legend.html');

  If Introduction <> nil then
  begin
    WriteDirectLine(Introduction.FullLink);
  end;

  if (LinkGraphVizClasses <> '') then
    WriteDirectLine(OverviewFilesInfo[ofGraphVizClasses].BaseFileName + '.' +
      LinkGraphVizClasses);

  if LinkGraphVizUses <> '' then
    WriteDirectLine(OverviewFilesInfo[ofGraphVizUses].BaseFileName + '.' +
      LinkGraphVizUses);

  for Overview := LowCreatedOverviewFile to HighCreatedOverviewFile do
    WriteDirectLine(OverviewFilesInfo[Overview].BaseFileName + '.html');

  if Assigned(Units) then
    for k := 0 to units.Count - 1 do
      begin
        Item := units.PasItemAt[k];
        PU := TPasUnit(units.PasItemAt[k]);
        WriteDirectLine(Item.FullLink);
        c := PU.CIOs;
        if Assigned(c) then
          for l := 0 to c.Count - 1 do
            begin
              Item2 := TPasCio(c.PasItemAt[l]);
              WriteDirectLine(Item2.OutputFilename);
            end;
      end;

  If (AdditionalFiles <> nil) and (AdditionalFiles.Count > 0) then
  begin
    for i := 0 to AdditionalFiles.Count - 1 do
    begin
      WriteDirectLine(AdditionalFiles.Get(i).FullLink);
    end;
  end;

  If Conclusion <> nil then
  begin
    WriteDirectLine(Conclusion.FullLink);
  end;

  WriteDirectLine('');

  WriteDirectLine('[INFOTYPES]');

  WriteDirectLine('');

  WriteDirectLine('[MERGE FILES]');

  CloseStream;
end;

end.
