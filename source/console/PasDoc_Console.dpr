{ ------------------------------------------------------------------------------

  PasDoc

  * generates documentation from comments in Pascal unit source code files
  * command line program
  * written in Delphi
  * output formats Html and HtmlHelp
  * try PasDoc on its own source code

  * distributed under the GNU General Public License (GPL)

  * copyright (C) 1998-2000 by Marco Schmidt
  * copyright (C) 2001-2003 by Ralf Junker <delphi@zeitungsjunge.de>
  * Copyright (C) 2003 by Johannes Berg <johannes@sipsolutions.de>

  Hint:

  Whenever you use PasDoc for documentations, make sure the program file
  contains no code except for a call to a main routine in another unit or
  the instantiation of an object / class that does all the work
  (usually TApplication).

  Pasdoc is restricted to work on unit files only, that's why the program file
  should contain no actual program-specific code - it would not become part of
  the documentation.

------------------------------------------------------------------------------ }

program PasDoc_Console;

{$APPTYPE CONSOLE}

uses
  PasDoc,
  PasDoc_Languages,
  SysUtils,
  Utils,
  PasDoc_HierarchyTree,
  StreamUtils,
  ObjectVector,
  PasDoc_GenHtml,
  PasDoc_Gen,
  PasDoc_Items,
  OptionParser,
  PasDoc_Types,
  PasDoc_RunHelp;

var
  GPasDoc: TPasDoc;
  GOptionParser: TOptionParser;
  GOption_Verbosity: TIntegerOption;
  GOption_Define: TStringOptionList;
  GOption_Help: TBoolOption;
  GOption_Descriptions,
  GOption_ConditionalFile,
  GOption_IncludePaths,
  GOption_SourceList,
  GOption_AbbrevFiles: TStringOptionList;
  GOption_ContentFile,
  GOption_Footer,
  GOption_Header,
  GOption_Name,
  GOption_Title,
  GOption_Format,
  GOption_OutputPath,
  GOption_Language,
  GOption_ASPELL,
  GOption_IgnoreWords: TStringOption;
  GOption_StarOnly,
  GOption_Generator,
  GOption_NumericFilenames,
  GOption_WriteUsesList,
  GOption_WriteGVUses,
  GOption_WriteGVClasses: TBoolOption;
  GOption_VisibleMembers: TSetOption;

  { ---------------------------------------------------------------------------- }

procedure CreateOptions; 
var
  l: TLanguageID;
begin
  GOptionParser := TOptionParser.Create;

  GOption_Help := TBoolOption.Create('?', 'help', True, False);
  GOption_Help.Explanation := 'show this help';
  GOptionParser.AddOption(GOption_Help);

  GOption_Verbosity := TIntegerOption.Create('v', 'verbosity', True, False);
  GOption_Verbosity.Value := DEFAULT_VERBOSITY_LEVEL;
  GOption_Verbosity.Explanation := 'set log verbosity (0-5) ['+IntToStr(DEFAULT_VERBOSITY_LEVEL)+']';
  GOptionParser.AddOption(GOption_Verbosity);

  GOption_Define := TStringOptionList.Create('D', 'define', True, False);
  GOption_Define.Explanation := 'define conditional';
  GOptionParser.AddOption(GOption_Define);

  GOption_Descriptions := TStringOptionList.Create('R', 'description', True, False);
  GOption_Descriptions.Explanation := 'read description from this file';
  GOptionParser.AddOption(GOption_Descriptions);

  GOption_ConditionalFile := TStringOptionList.Create('d', 'conditionals', True, False);
  GOption_ConditionalFile.Explanation := 'read conditionals from this file';
  GOptionParser.AddOption(GOption_ConditionalFile);

  GOption_IncludePaths := TStringOptionList.Create('I', 'include', True, False);
  GOption_IncludePaths.Explanation := 'includes search path';
  GOptionParser.AddOption(GOption_IncludePaths);

  GOption_SourceList := TStringOptionList.Create('S', 'source', True, False);
  GOption_SourceList.Explanation := 'read source filenames from file';
  GOptionParser.AddOption(GOption_SourceList);

  GOption_ContentFile := TStringOption.Create('C', 'content', True, False);
  GOption_ContentFile.Explanation := 'Read Contents for HtmlHelp from file';
  GOptionParser.AddOption(GOption_ContentFile);

  GOption_Footer := TStringOption.Create('F', 'footer', True, False);
  GOption_Footer.Explanation := 'include file as footer';
  GOptionParser.AddOption(GOption_Footer);

  GOption_Header := TStringOption.Create('H', 'header', True, False);
  GOption_Header.Explanation := 'include file as header';
  GOptionParser.AddOption(GOption_Header);

  GOption_Name := TStringOption.Create('N', 'name', True, False);
  GOption_Name.Explanation := 'Name for documentation';
  GOptionParser.AddOption(GOption_Name);

  GOption_Title := TStringOption.Create('T', 'title', True, False);
  GOption_Title.Explanation := 'Documentation title';
  GOptionParser.AddOption(GOption_Title);

  GOption_Format := TStringOption.Create('O', 'format', True, False);
  GOption_Format.Explanation := 'output format: html or htmlhelp';
  GOption_Format.Value := 'html';
  GOptionParser.AddOption(GOption_Format);

  GOption_OutputPath := TStringOption.Create('E', 'output', True, False);
  GOption_OutputPath.Explanation := 'output path';
  GOptionParser.AddOption(GOption_OutputPath);

  GOption_Generator := TBoolOption.Create('X', 'exclude-generator', True, False);
  GOption_Generator.Explanation := 'exclude generator information';
  GOptionParser.AddOption(GOption_Generator);

  GOption_Language := TStringOption.Create('L', 'language', True, False);
  GOption_Language.Explanation := 'Output language. Valid languages are: ' + #10;
  for l := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do
    GOption_Language.Explanation := GOption_Language.Explanation + '  ' +
      LANGUAGE_ARRAY[l].Syntax + ': ' + LANGUAGE_ARRAY[l].Name + #10;
  GOptionParser.AddOption(GOption_Language);

  GOption_StarOnly := TBoolOption.Create(#0, 'staronly', True, False);
  GOption_StarOnly.Explanation :=
    'Parse only {**, (*** and //** style comments';
  GOptionParser.AddOption(GOption_StarOnly);

  GOption_NumericFilenames := TBoolOption.Create(#0, 'numericfilenames', True, False);
  GOption_NumericFilenames.Explanation := 'Causes the html generator to create numeric filenames';
  GOptionParser.AddOption(GOption_NumericFilenames);

  GOption_VisibleMembers := TSetOption.Create('M','visible-members', True, False);
  GOption_VisibleMembers.Explanation := 'Include / Exclude class Members by visiblity';
  GOption_VisibleMembers.PossibleValues := 'private,protected,public,published,automated';
  GOption_VisibleMembers.Values := 'protected,public,published,automated';
  GOptionParser.AddOption(GOption_VisibleMembers);

  GOption_WriteUsesList := TBoolOption.Create(#0, 'write-uses-list', True, False);
  GOption_WriteUsesList.Explanation := 'put uses list into output';
  GOptionParser.AddOption(GOption_WriteUsesList);

  GOption_WriteGVUses := TBoolOption.Create(#0, 'graphviz-uses', True, False);
  GOption_WriteGVUses.Explanation := 'write a GVUses.gviz file that can be used for the `dot` program from GraphViz to generate a unit dependency graph';
  GOptionParser.AddOption(GOption_WriteGVUses);

  GOption_WriteGVClasses := TBoolOption.Create(#0, 'graphviz-classes', True, False);
  GOption_WriteGVClasses.Explanation := 'write a GVClasses.gviz file that can be used for the `dot` program from GraphViz to generate a class hierarchy graph';
  GOptionParser.AddOption(GOption_WriteGVClasses);

  GOption_AbbrevFiles := TStringOptionList.Create(#0, 'abbreviations', True, False);
  GOption_AbbrevFiles.Explanation := 'abbreviation file, format is "[name]  value", value is trimmed, lines that do not start with ''['' (or whitespace before that) are ignored';
  GOptionParser.AddOption(GOption_AbbrevFiles);

  GOption_ASPELL := TStringOption.Create(#0, 'aspell', True, False);
  GOption_ASPELL.Explanation := 'enable aspell, giving language as parameter, currently only done in HTML output';
  GOptionParser.AddOption(GOption_ASPELL);

  GOption_IgnoreWords := TStringOption.Create(#0, 'ignore-words', True, False);
  GOption_IgnoreWords.Explanation := 'When spell-checking, ignore the words in that file. The file should contain one word on every line, no comments allowed';
  GOptionParser.AddOption(GOption_IgnoreWords);
end;

procedure PrintUsage;
begin
  WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [options] [files]');
  WriteLn('Valid options are: ');
  GOptionParser.WriteExplanations;
end;

function ParseCommandLine: boolean;
var
  i: Integer;
  lng: TLanguageID;
begin
  Result := false;
  GOptionParser.ParseOptions;
  if GOption_Help.TurnedOn then begin
    PrintUsage;
    exit;
  end;

  GOption_Format.Value := LowerCase(GOption_Format.Value);
  if GOption_Format.Value = 'html' then begin
    GPasDoc.Generator := THTMLDocGenerator.Create(GPasDoc);
  end else begin
    if GOption_Format.Value = 'htmlhelp' then begin
      GPasDoc.Generator := THTMLDocGenerator.Create(GPasDoc);
    end else begin
      GPasDoc.DoMessage(1, mtWarning, 'Unknown output format (%s), skipping.',
        [GOption_Format.Value]);
    end;
  end;

  GPasDoc.HtmlHelpContentsFileName := GOption_ContentFile.Value;
  
  GPasDoc.Directives.Assign(GOption_Define.Values);
  for i := 0 to GOption_ConditionalFile.Values.Count - 1 do begin
    GPasDoc.Directives.LoadFromTextFileAdd(GOption_ConditionalFile.Values[i]);
  end;

  GPasDoc.Generator.DestinationDirectory := GOption_OutputPath.Value;
  GPasDoc.IncludeDirectories.Assign(GOption_IncludePaths.Values);

  GOption_Language.Value := lowercase(GOption_Language.Value);
  for lng := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do begin
    if LowerCase(LANGUAGE_ARRAY[lng].Syntax) = GOption_Language.Value then
      begin
      GPasDoc.Generator.Language := lng;
      break;
    end;
  end;

  GPasDoc.ProjectName := GOption_Name.Value;

  GPasDoc.DescriptionFileNames.Assign(GOption_Descriptions.Values);

  for i := 0 to GOption_SourceList.Values.Count - 1 do begin
    GPasDoc.AddSourceFileNamesFromFile(GOption_SourceList.Values[i]);
  end;

  GPasDoc.Title := GOption_Title.Value;

  GPasDoc.Verbosity := GOption_Verbosity.Value;

  if GPasDoc.Generator is THTMLDocGenerator then begin
    THTMLDocGenerator(GPasDoc.Generator).NoGeneratorInfo := GOption_Generator.TurnedOn;

    if GOption_Footer.WasSpecified then begin
      THTMLDocGenerator(GPasDoc.Generator).LoadFooterFromFile(GOption_Footer.Value);
    end;

    if GOption_Header.WasSpecified then begin
      THTMLDocGenerator(GPasDoc.Generator).LoadHeaderFromFile(GOption_Header.Value);
    end;

    THTMLDocGenerator(GPasDoc.Generator).NumericFilenames := GOption_NumericFilenames.TurnedOn;
    THTMLDocGenerator(GPasDoc.Generator).WriteUsesClause := GOption_WriteUsesList.TurnedOn;
  end;

  GPasDoc.StarStyleOnly := GOption_StarOnly.TurnedOn;

  GPasDoc.AddSourceFileNames(GOptionParser.LeftList);

  GPasDoc.ClassMembers := [];
  if GOption_VisibleMembers.HasValue('private') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PRIVATE];
  if GOption_VisibleMembers.HasValue('protected') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PROTECTED];
  if GOption_VisibleMembers.HasValue('public') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PUBLIC];
  if GOption_VisibleMembers.HasValue('published') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PUBLISHED];
  if GOption_VisibleMembers.HasValue('automated') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_AUTOMATED];

  GPasDoc.Generator.OutputGraphVizUses := GOption_WriteGVUses.TurnedOn;
  GPasDoc.Generator.OutputGraphVizClassHierarchy := GOption_WriteGVClasses.TurnedOn;

  for i := 0 to GOption_AbbrevFiles.Values.Count-1 do begin
    GPasDoc.Generator.ParseAbbreviationsFile(GOption_AbbrevFiles.Values[i]);
  end;

  GPasDoc.Generator.CheckSpelling := GOption_ASPELL.WasSpecified;
  GPasDoc.Generator.AspellLanguage := GOption_ASPELL.Value;
  GPasDoc.Generator.IgnoreWordsFile := GOption_IgnoreWords.Value;

  Result := True;
end;

{ ---------------------------------------------------------------------------- }

procedure WriteWarning(const Dummy: Pointer; const MessageType: TMessageType;
  const AMessage: AnsiString; const AVerbosity: Cardinal);
begin
  case MessageType of
    mtInformation: WriteLn('Info[', AVerbosity, ']:    ', AMessage);
    mtWarning: WriteLn('Warning[', AVerbosity, ']: ', AMessage);
    mtError: WriteLn('Error[', AVerbosity, ']:   ', AMessage);
  else
    WriteLn(AMessage);
  end;
end;

{ ---------------------------------------------------------------------------- }

begin
  WriteLn(PASDOC_FULL_INFO);
  WriteLn('Documentation generator for Pascal source');
  WriteLn;
  WriteLn('This is free software; see the source for copying conditions.  There is NO');
  WriteLn('warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  WriteLn;
  CreateOptions;
  try
    GPasDoc := TPasDoc.Create(nil);
    try
      GPasDoc.OnWarning := TPasDocMessageEvent(MakeMethod(nil,
        @WriteWarning));
      if ParseCommandLine then begin
        GPasDoc.Execute;
      end;
    finally
      GPasDoc.Free;
    end;
  except
    on e: EPasDoc do
      with e do
        WriteLn('Fatal Error: ', Message);
  end;
  GOptionParser.Free;
end.
