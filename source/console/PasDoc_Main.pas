{ @abstract(Provides the Main procedure.) }
unit PasDoc_Main;

{ Define this to see the backtrace of crashes (when compiled with FPC).
  Easier for debugging. }
{ $define LET_EXCEPTIONS_THROUGH}

interface

{ This is the main procedure of PasDoc, it does everything. }
procedure Main;

implementation

uses
  PasDoc_Base,
  PasDoc_Languages,
  SysUtils, StrUtils,
  PasDoc_Utils,
  PasDoc_GenHtml,
  PasDoc_GenSimpleXML,
  PasDoc_GenLatex,
  PasDoc_GenHtmlHelp,
  PasDoc_GenPHP,
  PasDoc_GenMarkdown,
  PasDoc_Gen,
  PasDoc_Items,
  PasDoc_OptionParser,
  PasDoc_Types,
  PasDoc_Tokenizer,
  PasDoc_Serialize,
  PasDoc_SortSettings,
  PasDoc_Versions;

type
  TPasdocOptions = class(TOptionParser)
  public
    OptionVerbosity: TIntegerOption;
    OptionDefine: TStringOptionList;
    OptionHelp: TBoolOption;
    OptionVersion: TBoolOption;
    OptionIncludePaths: TPathListOption;
    OptionDescriptions,
    OptionConditionalFile,
    OptionSourceList,
    OptionAbbrevFiles: TStringOptionList;
    OptionHtmlHelpContents,
    OptionHeader,
    OptionFooter,
    OptionHtmlHead,
    OptionHtmlBodyBegin,
    OptionHtmlBodyEnd,
    OptionName,
    OptionTitle,
    OptionFormat,
    OptionOutputPath,
    OptionLanguage,
    OptionAspell: TStringOption;
    OptionSpellCheck: TBoolOption;
    OptionSpellCheckIgnoreWords: TStringOption;
    OptionStarOnly,
    OptionExcludeGenerator,
    OptionIncludeCreationTime,
    OptionUseLowercaseKeywords,
    OptionNumericFilenames,
    OptionWriteUsesList,
    OptionWriteGVUses,
    OptionWriteGVClasses: TBoolOption;
    OptionLinkGVUses: TStringOption;
    OptionLinkGVClasses: TStringOption;
    OptionVisibleMembers: TSetOption;
    OptionCommentMarker: TStringOptionList;
    OptionIgnoreMarker: TStringOptionList;
    OptionMarkerOptional: TBoolOption;
    OptionIgnoreLeading: TStringOption;
    OptionCacheDir: TStringOption;
    OptionFullLink: TBoolOption;
    OptionCSS: TStringOption; {< Using external CSS file for HTML output }
    OptionAutoAbstract: TBoolOption;
    OptionLinkLook: TStringOption;
    OptionUseTipueSearch: TBoolOption;
    OptionSort: TSetOption;
    OptionIntroduction: TStringOption;
    OptionConclusion: TStringOption;
    OptionAdditionalFiles: TPathListOption;
    OptionLatexHead: TStringOption;
    OptionImplicitVisibility: TStringOption;
    OptionNoMacro: TBoolOption;
    OptionAutoLink: TBoolOption;
    OptionAutoLinkExclude: TStringOption;
    OptionExternalClassHierarchy: TStringOption;
    OptionMarkdown: TBoolOption;
    OptionAutoBackComments: TBoolOption;
    OptionInfoMergeMode: TStringOption;
  public
    constructor Create; override;
    procedure InterpretCommandline(PasDoc: TPasDoc);
  end;

type
  TPasdocMain = class
  private
    procedure WriteWarning(const MessageType: TPasDocMessageType;
      const AMessage: string; const AVerbosity: Cardinal);
    procedure PrintHeader;
    procedure PrintUsage(OptionParser: TOptionParser);
    procedure PrintVersion;
  public
    procedure Execute;
  end;

{ ---------------------------------------------------------------------------- }

constructor TPasdocOptions.Create;
var
  l: TLanguageID;
  mt: TInfoMergeType;
const
  InfoMergeTypeExplanation: array[TInfoMergeType] of string = (
    'Do not read implementation comments.',
    'Read both interface and implementation comments. Use whichever comment is non-empty. If they are both non-empty, use the interface comment.',
    'Read both interface and implementation comments. Use whichever comment is non-empty. If they are both non-empty, use the implementation comment.',
    'Read both interface and implementation comments and concatenate them. If the interface comment is also present (repeated) at the beginning of the implementation comment, it will be ignored (to not repeat 2x the same text in the concatenated result)'
  );
begin
  inherited;

  Self.IncludeFileOptionName := '@<path to config file>';
  Self.IncludeFileOptionExpl := 'Read options from specified file and insert them at current position. File must contain one option per line.'+LineEnding+
    'Format is "name[=value]", options are named just like in the command line but without leading dashes. Option values with spaces must NOT be quoted';

  OptionHelp := TBoolOption.Create('?', 'help');
  OptionHelp.Explanation := 'Show this help';
  AddOption(OptionHelp);

  OptionVersion := TBoolOption.Create(#0, 'version');
  OptionVersion.Explanation := 'Show pasdoc version (and related info)';
  AddOption(OptionVersion);

  OptionVerbosity := TIntegerOption.Create('v', 'verbosity');
  OptionVerbosity.Value := DEFAULT_VERBOSITY_LEVEL;
  OptionVerbosity.Explanation := 'Set log verbosity (0-6) ['+IntToStr(DEFAULT_VERBOSITY_LEVEL)+']';
  AddOption(OptionVerbosity);

  OptionDefine := TStringOptionList.Create('D', 'define');
  OptionDefine.Explanation := 'Define conditional';
  AddOption(OptionDefine);

  OptionDescriptions := TStringOptionList.Create('R', 'description');
  OptionDescriptions.Explanation := 'Read description from this file';
  AddOption(OptionDescriptions);

  OptionConditionalFile := TStringOptionList.Create('d', 'conditionals');
  OptionConditionalFile.Explanation := 'Read conditionals from this file';
  AddOption(OptionConditionalFile);

  OptionIncludePaths := TPathListOption.Create('I', 'include');
  OptionIncludePaths.Explanation := 'Includes search path';
  AddOption(OptionIncludePaths);

  OptionSourceList := TStringOptionList.Create('S', 'source');
  OptionSourceList.Explanation := 'Read source filenames from file';
  AddOption(OptionSourceList);

  OptionHtmlHelpContents := TStringOption.Create(#0, 'html-help-contents');
  OptionHtmlHelpContents.Explanation := 'Read Contents for HtmlHelp from file';
  AddOption(OptionHtmlHelpContents);

  OptionHeader := TStringOption.Create('H', 'header');
  OptionHeader.Explanation := 'Include file as header for HTML output';
  AddOption(OptionHeader);

  OptionFooter := TStringOption.Create('F', 'footer');
  OptionFooter.Explanation := 'Include file as footer for HTML output';
  AddOption(OptionFooter);

  OptionHtmlHead := TStringOption.Create(#0, 'html-head');
  OptionHtmlHead.Explanation := 'Include file to use inside HTML <head>';
  AddOption(OptionHtmlHead);

  OptionHtmlBodyBegin := TStringOption.Create(#0, 'html-body-begin');
  OptionHtmlBodyBegin.Explanation := 'Include file to use right after HTML <body>';
  AddOption(OptionHtmlBodyBegin);

  OptionHtmlBodyEnd := TStringOption.Create(#0, 'html-body-end');
  OptionHtmlBodyEnd.Explanation := 'Include file to use right before HTML </body>';
  AddOption(OptionHtmlBodyEnd);

  OptionName := TStringOption.Create('N', 'name');
  OptionName.Explanation := 'Name for documentation';
  AddOption(OptionName);

  OptionTitle := TStringOption.Create('T', 'title');
  OptionTitle.Explanation := 'Documentation title';
  AddOption(OptionTitle);

  OptionFormat := TStringOption.Create('O', 'format');
  OptionFormat.Explanation := 'Output format: html, simplexml, latex, latex2rtf, htmlhelp, php, markdown';
  OptionFormat.Value := 'html';
  AddOption(OptionFormat);

  OptionOutputPath := TStringOption.Create('E', 'output');
  OptionOutputPath.Explanation := 'Output path';
  AddOption(OptionOutputPath);

  OptionExcludeGenerator := TBoolOption.Create('X', 'exclude-generator');
  OptionExcludeGenerator.Explanation := 'Exclude generator information';
  AddOption(OptionExcludeGenerator);

  OptionIncludeCreationTime := TBoolOption.Create(#0, 'include-creation-time');
  OptionIncludeCreationTime.Explanation := 'Include creation time in the docs';
  AddOption(OptionIncludeCreationTime);

  OptionUseLowercaseKeywords := TBoolOption.Create(#0, 'lowercase-keywords');
  OptionUseLowercaseKeywords.Explanation := 'Lowercase all literal tag keywords (@nil, @false, @true)';
  AddOption(OptionUseLowercaseKeywords);

  OptionLanguage := TStringOption.Create('L', 'language');
  OptionLanguage.Explanation := 'Output language. Valid languages are: ' + LineEnding;
  for l := Low(l) to High(l) do
    OptionLanguage.Explanation := OptionLanguage.Explanation + '  ' +
      LanguageDescriptor(l)^.Syntax + ': ' + LanguageDescriptor(l)^.Name + LineEnding;
  AddOption(OptionLanguage);

  OptionStarOnly := TBoolOption.Create(#0, 'staronly');
  OptionStarOnly.Explanation := 'Parse only {**, (*** and //** style comments';
  AddOption(OptionStarOnly);

  OptionCommentMarker := TStringOptionList.Create(#0, 'marker');
  OptionCommentMarker.Explanation := 'Parse only {<marker>, (*<marker> and //<marker> comments. Overrides the staronly option, which is a shortcut for ''--marker=**''';
  AddOption(OptionCommentMarker);

  OptionIgnoreMarker := TStringOptionList.Create(#0, 'ignore-marker');
  OptionIgnoreMarker.Explanation := 'Skip comments starting with <marker> (that is, {<marker>, (*<marker> and //<marker> comments)';
  AddOption(OptionIgnoreMarker);

  OptionMarkerOptional := TBoolOption.Create(#0, 'marker-optional');
  OptionMarkerOptional.Explanation := 'Do not require the markers given in --marker but remove them from the comment if they exist.';
  AddOption(OptionMarkerOptional);

  OptionIgnoreLeading := TStringOption.Create(#0, 'ignore-leading');
  OptionIgnoreLeading.Explanation := 'Ignore leading <ignore-leading> characters in comments.';
  AddOption(OptionIgnoreLeading);

  OptionNumericFilenames := TBoolOption.Create(#0, 'numericfilenames');
  OptionNumericFilenames.Explanation := 'Causes the html generator to create numeric filenames';
  AddOption(OptionNumericFilenames);

  OptionVisibleMembers := TSetOption.Create('M','visible-members');
  OptionVisibleMembers.Explanation := 'Include / Exclude class Members by visiblity';
  OptionVisibleMembers.PossibleValues := VisibilitiesToStr(AllVisibilities);
  OptionVisibleMembers.Values := VisibilitiesToStr(DefaultVisibilities);
  AddOption(OptionVisibleMembers);

  OptionWriteUsesList := TBoolOption.Create(#0, 'write-uses-list');
  OptionWriteUsesList.Explanation := 'Put uses list into output';
  AddOption(OptionWriteUsesList);

  OptionWriteGVUses := TBoolOption.Create(#0, 'graphviz-uses');
  OptionWriteGVUses.Explanation := 'Write a GVUses.dot file that can be used for the `dot` program from GraphViz to generate a unit dependency graph';
  AddOption(OptionWriteGVUses);

  OptionWriteGVClasses := TBoolOption.Create(#0, 'graphviz-classes');
  OptionWriteGVClasses.Explanation := 'Write a GVClasses.dot file that can be used for the `dot` program from GraphViz to generate a class hierarchy graph';
  AddOption(OptionWriteGVClasses);

  OptionLinkGVUses := TStringOption.Create(#0, 'link-gv-uses');
  OptionLinkGVUses.Explanation := 'Add a link to a GVUses.<format> file generated by the `dot` program where <format> is any extension that `dot` can generate (e.g. jpg). (currently only for HTML output)';
  AddOption(OptionLinkGVUses);

  OptionLinkGVClasses := TStringOption.Create(#0, 'link-gv-classes');
  OptionLinkGVClasses.Explanation := 'Add a link to a GVClasses.<format> file generated by the `dot` program where <format> is any extension that `dot` can generate (e.g. jpg). (currently only for HTML output)';
  AddOption(OptionLinkGVClasses);

  OptionAbbrevFiles := TStringOptionList.Create(#0, 'abbreviations');
  OptionAbbrevFiles.Explanation := 'Abbreviation file, format is "[name]  value", value is trimmed, lines that do not start with ''['' (or whitespace before that) are ignored';
  AddOption(OptionAbbrevFiles);

  OptionSpellCheck := TBoolOption.Create(#0, 'spell-check');
  OptionSpellCheck.Explanation := 'Enable spell-checking by Aspell, specify language by the --language option';
  AddOption(OptionSpellCheck);

  OptionSpellCheckIgnoreWords := TStringOption.Create(#0, 'spell-check-ignore-words');
  OptionSpellCheckIgnoreWords.Explanation := 'When spell-checking, ignore the words in that file. The file should contain one word on every line';
  AddOption(OptionSpellCheckIgnoreWords);

  OptionASPELL := TStringOption.Create(#0, 'aspell');
  OptionASPELL.Explanation := 'Deprecated, use --spell-check. Enable spell-checking by Aspell, giving language as parameter';
  AddOption(OptionASPELL);

  OptionCacheDir := TStringOption.Create(#0, 'cache-dir');
  OptionCacheDir.Explanation := 'Cache directory for parsed files (default not set)';
  AddOption(OptionCacheDir);

  OptionLinkLook := TStringOption.Create(#0, 'link-look');
  OptionLinkLook.Explanation := 'How links are displayed in documentation: "default" (show the complete link name, as specified by @link), "full" (show the complete link name, and try to make each part of it a link), or "stripped" (show only last part of the link)';
  OptionLinkLook.Value := 'default'; { default value is 'default' }
  AddOption(OptionLinkLook);

  OptionFullLink := TBoolOption.Create(#0, 'full-link');
  OptionFullLink.Explanation := 'Obsolete name for --link-look=full';
  AddOption(OptionFullLink);

  { Using external CSS file for HTML output. }
  OptionCSS := TStringOption.Create(#0, 'css');
  OptionCSS.Explanation := 'CSS file for HTML files (copied into output tree)';
  AddOption(OptionCSS);

  OptionAutoAbstract := TBoolOption.Create(#0, 'auto-abstract');
  OptionAutoAbstract.Explanation := 'If set, pasdoc will automatically make abstract description of every item from the first sentence of description of this item';
  AddOption(OptionAutoAbstract);

  OptionUseTipueSearch := TBoolOption.Create(#0, 'use-tipue-search');
  OptionUseTipueSearch.Explanation := 'Use tipue search engine in HTML output';
  AddOption(OptionUseTipueSearch);

  OptionSort := TSetOption.Create(#0, 'sort');
  OptionSort.Explanation := 'Specifies what groups of items are sorted (the rest is presented in the same order they were declared in your source files)';
  OptionSort.PossibleValues := SortSettingsToName(AllSortSettings);
  OptionSort.Values := '';
  AddOption(OptionSort);

  OptionIntroduction := TStringOption.Create(#0, 'introduction');
  OptionIntroduction.Explanation := 'The name of a text file with introductory materials for the project';
  OptionIntroduction.Value := '';
  AddOption(OptionIntroduction);

  OptionConclusion := TStringOption.Create(#0, 'conclusion');
  OptionConclusion.Explanation := 'The name of a text file with concluding materials for the project';
  OptionConclusion.Value := '';
  AddOption(OptionConclusion);

  OptionAdditionalFiles := TPathListOption.Create('A', 'additional');
  OptionAdditionalFiles.Explanation := 'The name of a text file with addition materials for the project';
  AddOption(OptionAdditionalFiles);

  OptionLatexHead := TStringOption.Create(#0, 'latex-head');
  OptionLatexHead.Explanation := 'The name of a text file that includes lines to be inserted into the preamble of a LaTeX file';
  OptionLatexHead.Value := '';
  AddOption(OptionLatexHead);

  OptionImplicitVisibility := TStringOption.Create(#0, 'implicit-visibility');
  OptionImplicitVisibility.Explanation := 'How pasdoc should handle class members within default class visibility';
  OptionImplicitVisibility.Value := 'public';
  AddOption(OptionImplicitVisibility);

  OptionNoMacro := TBoolOption.Create(#0, 'no-macro');
  OptionNoMacro.Explanation := 'Turn FPC macro support off';
  AddOption(OptionNoMacro);

  OptionAutoLink := TBoolOption.Create(#0, 'auto-link');
  OptionAutoLink.Explanation := 'Automatically create links, without the need to explicitly use @link tags';
  AddOption(OptionAutoLink);

  OptionAutoBackComments := TBoolOption.Create(#0, 'auto-back-comments');
  OptionAutoBackComments.Explanation := 'Consider //-style comments after an identifier in the same line as description of that identifier.';
  AddOption(OptionAutoBackComments);

  OptionAutoLinkExclude := TStringOption.Create(#0, 'auto-link-exclude');
  OptionAutoLinkExclude.Explanation := 'Even when --auto-link is on, never automatically create links to identifiers in the specified file. The file should contain one identifier on every line';
  AddOption(OptionAutoLinkExclude);

  OptionExternalClassHierarchy := TStringOption.Create(#0, 'external-class-hierarchy');
  OptionExternalClassHierarchy.Explanation := 'File defining hierarchy of classes not included in your source code, for more complete class tree diagrams';
  AddOption(OptionExternalClassHierarchy);

  OptionMarkdown := TBoolOption.Create(#0, 'markdown');
  OptionMarkdown.Explanation := 'Decode Markdown syntax';
  AddOption(OptionMarkdown);

  OptionInfoMergeMode := TStringOption.Create(#0, 'implementation-comments');
  OptionInfoMergeMode.Explanation := 'Read implementation section of units and merge info to that taken from interface section. Option value determines how info is merged:' + LineEnding;
  for mt := Low(TInfoMergeType) to High(TInfoMergeType) do
    OptionInfoMergeMode.Explanation := OptionInfoMergeMode.Explanation + LineEnding +
      '  ' + InfoMergeTypeStr[mt] + ' - ' + InfoMergeTypeExplanation[mt] + LineEnding;
  AddOption(OptionInfoMergeMode);
end;

procedure TPasdocMain.PrintHeader;
begin
  WriteLn(PASDOC_FULL_INFO);
  WriteLn('Documentation generator for Pascal source');
  WriteLn;
  WriteLn('This is free software; see the source for copying conditions.  There is NO');
  WriteLn('warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  WriteLn;
end;

procedure TPasdocMain.PrintUsage(OptionParser: TOptionParser);
begin
  PrintHeader;
  WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [options] [files]');
  WriteLn('Valid options are: ');
  OptionParser.WriteExplanations;
end;

procedure TPasdocMain.PrintVersion;
begin
  Writeln(PASDOC_FULL_INFO);
end;

type
  EInvalidCommandLine = class(Exception);

procedure TPasdocOptions.InterpretCommandline(PasDoc: TPasDoc);

  { sets the html specific options and returns its parameter as TDocGenerator }
  function SetHtmlOptions(Generator: TGenericHTMLDocGenerator): TDocGenerator;
  begin
    if OptionHeader.WasSpecified then
      Generator.Header := FileToString(OptionHeader.Value);
    if OptionFooter.WasSpecified then
      Generator.Footer := FileToString(OptionFooter.Value);
    if OptionHtmlHead.WasSpecified then
      Generator.HtmlHead := FileToString(OptionHtmlHead.Value);
    if OptionHtmlBodyBegin.WasSpecified then
      Generator.HtmlBodyBegin := FileToString(OptionHtmlBodyBegin.Value);
    if OptionHtmlBodyEnd.WasSpecified then
      Generator.HtmlBodyEnd := FileToString(OptionHtmlBodyEnd.Value);
    if OptionCSS.WasSpecified then
      Generator.CSS := FileToString(OptionCSS.Value);

    Generator.NumericFilenames := OptionNumericFilenames.TurnedOn;

    Generator.UseTipueSearch := OptionUseTipueSearch.TurnedOn;

    Result := Generator;
  end;

  function SetSimpleXMLOptions(Generator: TSimpleXMLDocGenerator): TDocGenerator;
  begin
    Result := Generator;
  end;

  function SetPHPOptions(Generator: TPHPDocGenerator): TDocGenerator;
  begin
    Result := Generator;
  end;

  function SetMarkdownOptions(Generator: TMarkdownDocGenerator): TDocGenerator;
  begin
    Result := Generator;
  end;

  { Sets HTML-help specific options and returns its parameter as TDocGenerator }
  function SetHtmlHelpOptions(Generator: THTMLHelpDocGenerator): TDocGenerator;
  begin
    Generator.ContentsFile := OptionHtmlHelpContents.Value;

    Result := SetHtmlOptions(Generator);
  end;

  { Sets Latex specific options and returns its parameter as TDocGenerator }
  function SetLatexOptions(Generator: TTexDocGenerator): TDocGenerator;
  begin
    if OptionLatexHead.Value <> '' then
    try
      Generator.LatexHead.LoadFromFile(OptionLatexHead.Value);
    except
      on E: Exception do
      begin
        E.Message :=
          'Error when opening file for "--'+OptionLatexHead.LongForm+'" option: ' + E.Message;
        raise;
      end;
    end;

    Result := Generator;
  end;

  { Sets Latex and Latex2rtf specific options and returns its parameter as TDocGenerator }
  function SetRtfOptions(Generator: TTexDocGenerator): TDocGenerator;
  begin
    Generator.Latex2rtf := True;
    Result := SetLatexOptions(Generator);
  end;

  function GetLanguageFromStr(S: string): TLanguageID;
  begin
    if not LanguageFromStr(S, Result) then
      raise EInvalidCommandLine.CreateFmt('Unknown language code "%s"', [S]);
  end;

var
  i: Integer;
  SS: TSortSetting;
  Vis: TVisibility;
begin
  OptionFormat.Value := LowerCase(OptionFormat.Value);
  if OptionFormat.Value = 'html' then begin
    PasDoc.Generator := SetHtmlOptions(THTMLDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'simplexml' then
  begin
    PasDoc.Generator := SetSimpleXMLOptions(TSimpleXMLDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'latex' then
  begin
    PasDoc.Generator := SetLatexOptions(TTexDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'latex2rtf' then
  begin
    PasDoc.Generator := SetRtfOptions(TTexDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'htmlhelp' then
  begin
    PasDoc.Generator := SetHtmlHelpOptions(THTMLHelpDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'php' then
  begin
    PasDoc.Generator := SetPHPOptions(TPHPDocGenerator.Create(PasDoc));
  end else
  if OptionFormat.Value = 'markdown' then
  begin
    PasDoc.Generator := SetMarkdownOptions(TMarkdownDocGenerator.Create(PasDoc));
  end else
  begin
    raise EInvalidCommandLine.CreateFmt(
      'Unknown output format "%s"', [OptionFormat.Value]);
  end;

  PasDoc.Directives.Assign(OptionDefine.Values);
  for i := 0 to OptionConditionalFile.Values.Count - 1 do begin
    PasDoc.Directives.LoadFromTextFileAdd(OptionConditionalFile.Values[i]);
  end;

  PasDoc.Generator.DestinationDirectory := OptionOutputPath.Value;
  PasDoc.IncludeDirectories.Assign(OptionIncludePaths.Values);

  if OptionLanguage.WasSpecified then
    PasDoc.Generator.Language := GetLanguageFromStr(OptionLanguage.Value);

  PasDoc.ProjectName := OptionName.Value;

  PasDoc.DescriptionFileNames.Assign(OptionDescriptions.Values);

  for i := 0 to OptionSourceList.Values.Count - 1 do begin
    PasDoc.AddSourceFileNamesFromFile(OptionSourceList.Values[i], true);
  end;

  PasDoc.Title := OptionTitle.Value;

  PasDoc.Verbosity := OptionVerbosity.Value;

  PasDoc.Generator.ExcludeGenerator := OptionExcludeGenerator.TurnedOn;
  PasDoc.Generator.IncludeCreationTime := OptionIncludeCreationTime.TurnedOn;
  PasDoc.Generator.UseLowercaseKeywords := OptionUseLowercaseKeywords.TurnedOn;
  PasDoc.Generator.WriteUsesClause := OptionWriteUsesList.TurnedOn;

  if OptionUseTipueSearch.TurnedOn then begin
    if not (PasDoc.Generator is TGenericHTMLDocGenerator) then begin
      raise EInvalidCommandLine.Create(
        'You can''t specify "--'+OptionUseTipueSearch.LongForm+'" option for non-html output formats');
    end;
  end;

  if OptionHtmlHelpContents.Value <> '' then begin
    if not (PasDoc.Generator is THTMLHelpDocGenerator) then begin
      raise EInvalidCommandLine.Create('You can specify "--'+OptionHtmlHelpContents.LongForm+'"' +
        ' option only for HTMLHelp output format');
    end;
  end;

  if OptionCommentMarker.WasSpecified then begin
    PasDoc.CommentMarkers.Assign(OptionCommentMarker.Values);
  end;
  if OptionIgnoreMarker.WasSpecified then begin
    PasDoc.IgnoreMarkers.Assign(OptionIgnoreMarker.Values);
  end;
  if OptionStarOnly.TurnedOn then
    PasDoc.StarOnly := true;
  PasDoc.MarkerOptional := OptionMarkerOptional.TurnedOn;

  PasDoc.IgnoreLeading := OptionIgnoreLeading.Value;

  PasDoc.AddSourceFileNames(LeftList);

  PasDoc.ShowVisibilities := [];
  for Vis := Low(Vis) to High(Vis) do
    if OptionVisibleMembers.HasValue(VisToStr(Vis)) then
      PasDoc.ShowVisibilities :=  PasDoc.ShowVisibilities + [Vis];

  PasDoc.Generator.OutputGraphVizUses := OptionWriteGVUses.TurnedOn;
  PasDoc.Generator.OutputGraphVizClassHierarchy := OptionWriteGVClasses.TurnedOn;
  PasDoc.Generator.LinkGraphVizUses := OptionLinkGVUses.Value;
  PasDoc.Generator.LinkGraphVizClasses := OptionLinkGVClasses.Value;

  for i := 0 to OptionAbbrevFiles.Values.Count-1 do begin
    PasDoc.Generator.ParseAbbreviationsFile(OptionAbbrevFiles.Values[i]);
  end;

  PasDoc.Generator.CheckSpelling :=
    OptionASPELL.WasSpecified or OptionSpellCheck.WasSpecified;
  if OptionSpellCheck.WasSpecified then
    PasDoc.Generator.AspellLanguage := LanguageCode(PasDoc.Generator.Language) else
  if OptionASPELL.Value = '' then
    PasDoc.Generator.AspellLanguage := LanguageCode(PasDoc.Generator.Language) else
    PasDoc.Generator.AspellLanguage := OptionASPELL.Value;
  if OptionSpellCheckIgnoreWords.Value <> '' then
    PasDoc.Generator.SpellCheckIgnoreWords.LoadFromFile(
      OptionSpellCheckIgnoreWords.Value);

  PasDoc.CacheDir := OptionCacheDir.Value;

  PasDoc.Generator.AutoAbstract := OptionAutoAbstract.TurnedOn;

  if SameText(OptionLinkLook.Value, 'default') then
    PasDoc.Generator.LinkLook := llDefault else
  if SameText(OptionLinkLook.Value, 'full') then
    PasDoc.Generator.LinkLook := llFull else
  if SameText(OptionLinkLook.Value, 'stripped') then
    PasDoc.Generator.LinkLook := llStripped else
    raise EInvalidCommandLine.CreateFmt(
      'Invalid argument for "--'+OptionLinkLook.LongForm+'" option : "%s"',
      [OptionLinkLook.Value]);

  if OptionFullLink.TurnedOn then
    PasDoc.Generator.LinkLook := llFull;

  { interpret OptionSort value }
  PasDoc.SortSettings := [];
  for SS := Low(SS) to High(SS) do
    if OptionSort.HasValue(SortSettingNames[SS]) then
      PasDoc.SortSettings := PasDoc.SortSettings + [SS];

  PasDoc.IntroductionFileName := OptionIntroduction.Value;
  PasDoc.ConclusionFileName := OptionConclusion.Value;
  PasDoc.AdditionalFilesNames.Assign(OptionAdditionalFiles.Values);

  if OptionLatexHead.Value <> '' then begin
    if not (PasDoc.Generator is TTexDocGenerator) then begin
      raise EInvalidCommandLine.Create(
        'You can only use the "--'+OptionLatexHead.LongForm+'" option with LaTeX output.');
    end;
  end;

  if SameText(OptionImplicitVisibility.Value, 'public') then
    PasDoc.ImplicitVisibility := ivPublic else
  if SameText(OptionImplicitVisibility.Value, 'published') then
    PasDoc.ImplicitVisibility := ivPublished else
  if SameText(OptionImplicitVisibility.Value, 'implicit') then
    PasDoc.ImplicitVisibility := ivImplicit else
    raise EInvalidCommandLine.CreateFmt(
      'Invalid argument for "--'+OptionImplicitVisibility.LongForm+'" option : "%s"',
      [OptionImplicitVisibility.Value]);

  PasDoc.HandleMacros := not OptionNoMacro.TurnedOn;
  PasDoc.AutoLink := OptionAutoLink.TurnedOn;

  if OptionAutoLinkExclude.Value <> '' then
  begin
    PasDoc.Generator.AutoLinkExclude.LoadFromFile(OptionAutoLinkExclude.Value);
    { Sorted makes searching AutoLinkExclude.IndexOf (used heavily when
      auto-linking to respect this option) obviously much faster.
      The speed improvement can be literally felt when you specified
      large file like /usr/share/dict/american-english for this option. }
    PasDoc.Generator.AutoLinkExclude.Sorted := true;
  end;

  if OptionExternalClassHierarchy.WasSpecified then
    PasDoc.Generator.ExternalClassHierarchy.LoadFromFile(
      OptionExternalClassHierarchy.Value);

  PasDoc.Generator.Markdown := OptionMarkdown.TurnedOn;
  PasDoc.AutoBackComments := OptionAutoBackComments.TurnedOn;
  if OptionInfoMergeMode.Value <> '' then
    {$ifdef FPC} {$push} {$notes off} {$endif} // do not make a note about IndexText not inlined
    PasDoc.InfoMergeType := TInfoMergeType(IndexText(OptionInfoMergeMode.Value, InfoMergeTypeStr));
    {$ifdef FPC} {$pop} {$endif}
end;

{ ---------------------------------------------------------------------------- }

procedure TPasdocMain.WriteWarning(const MessageType: TPasDocMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  case MessageType of
    pmtInformation: WriteLn('Info[', AVerbosity, ']:    ', AMessage);
    pmtWarning: WriteLn('Warning[', AVerbosity, ']: ', AMessage);
    pmtError: WriteLn('Error[', AVerbosity, ']:   ', AMessage);
  else
    WriteLn(AMessage);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TPasdocMain.Execute;
var
  PasDoc: TPasDoc;
  OptionParser: TPasdocOptions;
begin
  OptionParser := TPasdocOptions.Create;
  try
    OptionParser.ParseOptions;

    if OptionParser.OptionHelp.TurnedOn then begin PrintUsage(OptionParser); Exit; end;

    if OptionParser.OptionVersion.TurnedOn then begin PrintVersion; Exit; end;

    if not OptionParser.OptionExcludeGenerator.TurnedOn then PrintHeader;

    PasDoc := TPasDoc.Create(nil);
    try
      PasDoc.OnMessage := {$ifdef FPC}@{$endif} WriteWarning;
      OptionParser.InterpretCommandline(PasDoc);
      PasDoc.Execute;
    finally
      PasDoc.Free;
    end;
  finally
    OptionParser.Free;
  end;
end;

procedure Main;
var
  PasdocMain: TPasdocMain;
begin
{$IFNDEF FPC}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion > 17}
      ReportMemoryLeaksOnShutdown := DebugHook <> 0;
    {$IFEND}
  {$ENDIF}
{$ENDIF}
  {$ifndef LET_EXCEPTIONS_THROUGH}
  try
  {$endif not LET_EXCEPTIONS_THROUGH}
    PasdocMain := TPasdocMain.Create;
    try
      PasdocMain.Execute;
      if ExitCode <> 0 then
        WriteLn('The documentation was generated, but some problems during parsing or generation occurred. (Consult the output above for details.) Therefore exiting with non-zero exit status.');
    finally
      PasdocMain.Free;
    end;
  {$ifndef LET_EXCEPTIONS_THROUGH}
  except
    on E: Exception do
    begin
      WriteLn('Fatal Error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1; // exit with non-zero status
      Exit;
    end;
  end;
  {$endif not LET_EXCEPTIONS_THROUGH}
{$IFNDEF FPC}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion > 14}
      {$WARN SYMBOL_PLATFORM OFF}
    {$IFEND}
  {$ENDIF}
  if DebugHook <> 0 then
    ReadLn;
{$ENDIF}
end;

end.
