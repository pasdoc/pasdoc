unit PasDoc_Main;

interface

procedure Main;

implementation

uses
  PasDoc,
  PasDoc_Languages,
  SysUtils,
  Utils,
  PasDoc_HierarchyTree,
  StreamUtils,
  StringVector,
  ObjectVector,
  PasDoc_GenHtml,
  PasDoc_GenLatex,
  PasDoc_GenHtmlHelp,
  PasDoc_Gen,
  PasDoc_Items,
  OptionParser,
  PasDoc_Types,
  PasDoc_RunHelp,
  Hashes,
  PasDoc_Parser,
  PasDoc_Tokenizer,
  PasDoc_Serialize,
  PasDoc_Scanner,
  PasDoc_TagManager,
  PasDoc_SortSettings;

var
  GPasDoc: TPasDoc;
  GOptionParser: TOptionParser;
  GOption_Verbosity: TIntegerOption;
  GOption_Define: TStringOptionList;
  GOption_Help: TBoolOption;
  GOption_Version: TBoolOption;
  GOption_IncludePaths: TPathListOption;
  GOption_Descriptions,
  GOption_ConditionalFile,
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
  Goption_LinkGVUses: TStringOption;
  Goption_LinkGVClasses: TStringOption;
  GOption_VisibleMembers: TSetOption;
  GOption_CommentMarker: TStringOptionList;
  GOption_MarkerOptional: TBoolOption;
  GOption_CacheDir: TStringOption;
  GOption_FullLink: TBoolOption;
  GOption_CSS: TStringOption; { Using external CSS file for HTML output }
  GOption_AutoAbstract: TBoolOption;
  GOption_LinkLook: TStringOption;
  GOption_UseTipueSearch: TBoolOption;
  GOption_Sort: TSetOption;
  GOption_Introduction: TStringOption;
  GOption_Conclusion: TStringOption;

  { ---------------------------------------------------------------------------- }

procedure CreateOptions; 
var
  l: TLanguageID;
begin
  GOptionParser := TOptionParser.Create;

  GOption_Help := TBoolOption.Create('?', 'help');
  GOption_Help.Explanation := 'Show this help';
  GOptionParser.AddOption(GOption_Help);

  GOption_Version := TBoolOption.Create(#0, 'version');
  GOption_Version.Explanation := 'Show pasdoc version (and related info)';
  GOptionParser.AddOption(GOption_Version);

  GOption_Verbosity := TIntegerOption.Create('v', 'verbosity');
  GOption_Verbosity.Value := DEFAULT_VERBOSITY_LEVEL;
  GOption_Verbosity.Explanation := 'Set log verbosity (0-6) ['+IntToStr(DEFAULT_VERBOSITY_LEVEL)+']';
  GOptionParser.AddOption(GOption_Verbosity);

  GOption_Define := TStringOptionList.Create('D', 'define');
  GOption_Define.Explanation := 'Define conditional';
  GOptionParser.AddOption(GOption_Define);

  GOption_Descriptions := TStringOptionList.Create('R', 'description');
  GOption_Descriptions.Explanation := 'Read description from this file';
  GOptionParser.AddOption(GOption_Descriptions);

  GOption_ConditionalFile := TStringOptionList.Create('d', 'conditionals');
  GOption_ConditionalFile.Explanation := 'Read conditionals from this file';
  GOptionParser.AddOption(GOption_ConditionalFile);

  GOption_IncludePaths := TPathListOption.Create('I', 'include');
  GOption_IncludePaths.Explanation := 'Includes search path';
  GOptionParser.AddOption(GOption_IncludePaths);

  GOption_SourceList := TStringOptionList.Create('S', 'source');
  GOption_SourceList.Explanation := 'Read source filenames from file';
  GOptionParser.AddOption(GOption_SourceList);

  GOption_ContentFile := TStringOption.Create('C', 'content');
  GOption_ContentFile.Explanation := 'Read Contents for HtmlHelp from file';
  GOptionParser.AddOption(GOption_ContentFile);

  GOption_Footer := TStringOption.Create('F', 'footer');
  GOption_Footer.Explanation := 'Include file as footer';
  GOptionParser.AddOption(GOption_Footer);

  GOption_Header := TStringOption.Create('H', 'header');
  GOption_Header.Explanation := 'Include file as header';
  GOptionParser.AddOption(GOption_Header);

  GOption_Name := TStringOption.Create('N', 'name');
  GOption_Name.Explanation := 'Name for documentation';
  GOptionParser.AddOption(GOption_Name);

  GOption_Title := TStringOption.Create('T', 'title');
  GOption_Title.Explanation := 'Documentation title';
  GOptionParser.AddOption(GOption_Title);

  GOption_Format := TStringOption.Create('O', 'format');
  GOption_Format.Explanation := 'Output format: html, latex, latex2rtf or htmlhelp';
  GOption_Format.Value := 'html';
  GOptionParser.AddOption(GOption_Format);

  GOption_OutputPath := TStringOption.Create('E', 'output');
  GOption_OutputPath.Explanation := 'Output path';
  GOptionParser.AddOption(GOption_OutputPath);

  GOption_Generator := TBoolOption.Create('X', 'exclude-generator');
  GOption_Generator.Explanation := 'Exclude generator information';
  GOptionParser.AddOption(GOption_Generator);

  GOption_Language := TStringOption.Create('L', 'language');
  GOption_Language.Explanation := 'Output language. Valid languages are: ' + #10;
  for l := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do
    GOption_Language.Explanation := GOption_Language.Explanation + '  ' +
      LANGUAGE_ARRAY[l].Syntax + ': ' + LANGUAGE_ARRAY[l].Name + #10;
  GOptionParser.AddOption(GOption_Language);

  GOption_StarOnly := TBoolOption.Create(#0, 'staronly');
  GOption_StarOnly.Explanation := 'Parse only {**, (*** and //** style comments';
  GOptionParser.AddOption(GOption_StarOnly);

  GOption_CommentMarker := TStringOptionList.Create(#0, 'marker');
  GOption_CommentMarker.Explanation := 'Parse only {<marker>, (*<marker> and //<marker> comments. Overrides the staronly option, which is a shortcut for ''--marker=**''';
  GOptionParser.AddOption(GOption_CommentMarker);

  GOption_MarkerOptional := TBoolOption.Create(#0, 'marker-optional');
  GOption_MarkerOptional.Explanation := 'Do not require the markers given in --marker but remove them from the comment if they exist.';
  GOptionParser.AddOption(GOption_MarkerOptional);

  GOption_NumericFilenames := TBoolOption.Create(#0, 'numericfilenames');
  GOption_NumericFilenames.Explanation := 'Causes the html generator to create numeric filenames';
  GOptionParser.AddOption(GOption_NumericFilenames);

  GOption_VisibleMembers := TSetOption.Create('M','visible-members');
  GOption_VisibleMembers.Explanation := 'Include / Exclude class Members by visiblity';
  GOption_VisibleMembers.PossibleValues := 'private,protected,public,published,automated';
  GOption_VisibleMembers.Values := 'protected,public,published,automated';
  GOptionParser.AddOption(GOption_VisibleMembers);

  GOption_WriteUsesList := TBoolOption.Create(#0, 'write-uses-list');
  GOption_WriteUsesList.Explanation := 'Put uses list into output';
  GOptionParser.AddOption(GOption_WriteUsesList);

  GOption_WriteGVUses := TBoolOption.Create(#0, 'graphviz-uses');
  GOption_WriteGVUses.Explanation := 'Write a GVUses.dot file that can be used for the `dot` program from GraphViz to generate a unit dependency graph';
  GOptionParser.AddOption(GOption_WriteGVUses);

  GOption_WriteGVClasses := TBoolOption.Create(#0, 'graphviz-classes');
  GOption_WriteGVClasses.Explanation := 'Write a GVClasses.dot file that can be used for the `dot` program from GraphViz to generate a class hierarchy graph';
  GOptionParser.AddOption(GOption_WriteGVClasses);

  Goption_LinkGVUses := TStringOption.Create(#0, 'link-gv-uses');
  Goption_LinkGVUses.Explanation := 'Add a link to a GVUses.<format> file generated by the `dot` program where <format> is any extension that `dot` can generate (e.g. jpg). (currently only for HTML output)';
  GOptionParser.AddOption(Goption_LinkGVUses);

  Goption_LinkGVClasses := TStringOption.Create(#0, 'link-gv-classes');
  Goption_LinkGVClasses.Explanation := 'Add a link to a GVClasses.<format> file generated by the `dot` program where <format> is any extension that `dot` can generate (e.g. jpg). (currently only for HTML output)';
  GOptionParser.AddOption(Goption_LinkGVClasses);

  GOption_AbbrevFiles := TStringOptionList.Create(#0, 'abbreviations');
  GOption_AbbrevFiles.Explanation := 'Abbreviation file, format is "[name]  value", value is trimmed, lines that do not start with ''['' (or whitespace before that) are ignored';
  GOptionParser.AddOption(GOption_AbbrevFiles);

  GOption_ASPELL := TStringOption.Create(#0, 'aspell');
  GOption_ASPELL.Explanation := 'Enable aspell, giving language as parameter, currently only done in HTML output';
  GOptionParser.AddOption(GOption_ASPELL);

  GOption_IgnoreWords := TStringOption.Create(#0, 'ignore-words');
  GOption_IgnoreWords.Explanation := 'When spell-checking, ignore the words in that file. The file should contain one word on every line, no comments allowed';
  GOptionParser.AddOption(GOption_IgnoreWords);

  GOption_CacheDir := TStringOption.Create(#0, 'cache-dir');
  GOption_CacheDir.Explanation := 'Cache directory for parsed files (default not set)';
  GOptionParser.AddOption(GOption_CacheDir);

  GOption_LinkLook := TStringOption.Create(#0, 'link-look');
  GOption_LinkLook.Explanation := 'How links are displayed in documentation: "default" (show the complete link name, as specified by @link), "full" (show the complete link name, and try to make each part of it a link), or "stripped" (show only last part of the link)';
  GOption_LinkLook.Value := 'default'; { default value is 'default' }
  GOptionParser.AddOption(GOption_LinkLook);

  GOption_FullLink := TBoolOption.Create(#0, 'full-link');
  GOption_FullLink.Explanation := 'Obsolete name for --link-look=full';
  GOptionParser.AddOption(GOption_FullLink);

  { Using external CSS file for HTML output. }
  GOption_CSS := TStringOption.Create(#0, 'css');
  GOption_CSS.Explanation := 'CSS file for HTML files (copied into output tree)';
  GOptionParser.AddOption(GOption_CSS);

  GOption_AutoAbstract := TBoolOption.Create(#0, 'auto-abstract');
  GOption_AutoAbstract.Explanation := 'If set, pasdoc will automatically make abstract description of every item from the first sentence of description of this item';
  GOptionParser.AddOption(GOption_AutoAbstract);

  GOption_UseTipueSearch := TBoolOption.Create(#0, 'use-tipue-search');
  GOption_UseTipueSearch.Explanation := 'Ise tipue search engine in HTML output';
  GOptionParser.AddOption(GOption_UseTipueSearch);

  GOption_Sort := TSetOption.Create(#0, 'sort');
  GOption_Sort.Explanation := 'Specifies what groups of items are sorted (the rest is presented in the same order they were declared in your source files)';
  GOption_Sort.PossibleValues := SortSettingsToName(AllSortSettings);
  GOption_Sort.Values := '';
  GOptionParser.AddOption(GOption_Sort);

  GOption_Introduction := TStringOption.Create(#0, 'introduction');
  GOption_Introduction.Explanation := 'The name of a text file with introductory materials for the project';
  GOption_Introduction.Value := '';
  GOptionParser.AddOption(GOption_Introduction);

  GOption_Conclusion := TStringOption.Create(#0, 'conclusion');
  GOption_Conclusion.Explanation := 'The name of a text file with concluding materials for the project';
  GOption_Conclusion.Value := '';
  GOptionParser.AddOption(GOption_Conclusion);
end;

procedure PrintHeader;
begin
  WriteLn(PASDOC_FULL_INFO);
  WriteLn('Documentation generator for Pascal source');
  WriteLn;
  WriteLn('This is free software; see the source for copying conditions.  There is NO');
  WriteLn('warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  WriteLn;
end;

procedure PrintUsage;
begin
  PrintHeader;
  WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [options] [files]');
  WriteLn('Valid options are: ');
  GOptionParser.WriteExplanations;
end;

procedure PrintVersion; 
begin
  Writeln(PASDOC_FULL_INFO);
end;

type
  EInvalidCommandLine = class(Exception);

procedure ParseCommandLine;
var
  i: Integer;
  lng: TLanguageID;
  SS: TSortSetting;
  HtmlGen: TGenericHTMLDocGenerator;
begin
  GOption_Format.Value := LowerCase(GOption_Format.Value);
  if GOption_Format.Value = 'html' then begin
    GPasDoc.Generator := THTMLDocGenerator.Create(GPasDoc);
  end else 
  if GOption_Format.Value = 'latex' then 
  begin
    GPasDoc.Generator := TTexDocGenerator.Create(GPasDoc);
  end else 
  if GOption_Format.Value = 'latex2rtf' then 
  begin
    GPasDoc.Generator := TTexDocGenerator.Create(GPasDoc);
    TTexDocGenerator(GPasDoc.Generator).Latex2rtf := True;
  end else
  if GOption_Format.Value = 'htmlhelp' then 
  begin
    GPasDoc.Generator := THTMLHelpDocGenerator.Create(GPasDoc);
    TGenericHTMLDocGenerator(GPasDoc.Generator).NumericFilenames := True;
  end else
  begin
    raise EInvalidCommandLine.CreateFmt(
      'Unknown output format "%s"', [GOption_Format.Value]);
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
    GPasDoc.AddSourceFileNamesFromFile(GOption_SourceList.Values[i], true);
  end;

  GPasDoc.Title := GOption_Title.Value;

  GPasDoc.Verbosity := GOption_Verbosity.Value;
  
  GPasDoc.Generator.NoGeneratorInfo := GOption_Generator.TurnedOn;

  if GPasDoc.Generator is TGenericHTMLDocGenerator then
  begin    
    HtmlGen := TGenericHTMLDocGenerator(GPasDoc.Generator);

    if GOption_Footer.WasSpecified then
      HtmlGen.LoadFooterFromFile(GOption_Footer.Value);

    if GOption_Header.WasSpecified then
      HtmlGen.LoadHeaderFromFile(GOption_Header.Value);

    { If external CSS file was specified }
    if GOption_CSS.WasSpecified then
     HtmlGen.CSS := GOption_CSS.Value;

    HtmlGen.NumericFilenames := GOption_NumericFilenames.TurnedOn;
    HtmlGen.WriteUsesClause := GOption_WriteUsesList.TurnedOn;
    
    HtmlGen.UseTipueSearch := GOption_UseTipueSearch.TurnedOn;
  end else
  begin
    if GOption_UseTipueSearch.TurnedOn then
      raise EInvalidCommandLine.Create(
        'You can''t specify --use-tipue-search option for non-html output formats');
  end;

  if GOption_CommentMarker.WasSpecified then begin
    GPasDoc.CommentMarkers.Assign(GOption_CommentMarker.Values);
  end;
  if GOption_StarOnly.TurnedOn then
    GPasDoc.StarStyleOnly := true;
  GPasDoc.MarkerOptional := GOption_MarkerOptional.TurnedOn;

  GPasDoc.AddSourceFileNames(GOptionParser.LeftList);

  GPasDoc.ClassMembers := [];
  if GOption_VisibleMembers.HasValue('private') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PRIVATE];
  if GOption_VisibleMembers.HasValue('protected') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PROTECTED];
  if GOption_VisibleMembers.HasValue('public') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PUBLIC];
  if GOption_VisibleMembers.HasValue('published') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_PUBLISHED];
  if GOption_VisibleMembers.HasValue('automated') then GPasDoc.ClassMembers :=  GPasDoc.ClassMembers+[STATE_AUTOMATED];

  GPasDoc.Generator.OutputGraphVizUses := GOption_WriteGVUses.TurnedOn;
  GPasDoc.Generator.OutputGraphVizClassHierarchy := GOption_WriteGVClasses.TurnedOn;
  GPasDoc.Generator.LinkGraphVizUses := Goption_LinkGVUses.Value;
  GPasDoc.Generator.LinkGraphVizClasses := Goption_LinkGVClasses.Value;

  for i := 0 to GOption_AbbrevFiles.Values.Count-1 do begin
    GPasDoc.Generator.ParseAbbreviationsFile(GOption_AbbrevFiles.Values[i]);
  end;

  GPasDoc.Generator.CheckSpelling := GOption_ASPELL.WasSpecified;
  GPasDoc.Generator.AspellLanguage := GOption_ASPELL.Value;
  GPasDoc.Generator.IgnoreWordsFile := GOption_IgnoreWords.Value;
  GPasDoc.CacheDir := GOption_CacheDir.Value;

  GPasDoc.Generator.AutoAbstract := GOption_AutoAbstract.TurnedOn;

  if SameText(GOption_LinkLook.Value, 'default') then
    GPasDoc.Generator.LinkLook := llDefault else
  if SameText(GOption_LinkLook.Value, 'full') then
    GPasDoc.Generator.LinkLook := llFull else
  if SameText(GOption_LinkLook.Value, 'stripped') then
    GPasDoc.Generator.LinkLook := llStripped else
    raise EInvalidCommandLine.CreateFmt(
      'Invalid argument for "--link-look" option : "%s"', 
      [GOption_LinkLook.Value]);

  if GOption_FullLink.TurnedOn then
    GPasDoc.Generator.LinkLook := llFull;

  { interpret GOption_Sort value }
  GPasDoc.SortSettings := [];
  for SS := Low(SS) to High(SS) do
    if GOption_Sort.HasValue(SortSettingNames[SS]) then
      GPasDoc.SortSettings := GPasDoc.SortSettings + [SS];

  GPasDoc.IntroductionFileName := GOption_Introduction.Value;
  GPasDoc.ConclusionFileName := GOption_Conclusion.Value;
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
procedure Main;
begin
  CreateOptions;
  try
    GOptionParser.ParseOptions;

    if GOption_Help.TurnedOn then begin PrintUsage; Exit; end;

    if GOption_Version.TurnedOn then begin PrintVersion; Exit; end;
    
    if not GOption_Generator.TurnedOn then PrintHeader;

    try
      GPasDoc := TPasDoc.Create(nil);
      try
        GPasDoc.OnMessage := 
          TPasDocMessageEvent(MakeMethod(nil, @WriteWarning));
        ParseCommandLine;
        GPasDoc.Execute;
      finally
        GPasDoc.Free;
      end;
    except
      on e: Exception do
        with e do
          WriteLn('Fatal Error: ', Message);
    end;
  finally
    GOptionParser.Free;
  end;
end;

end.
