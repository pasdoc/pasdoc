{ ------------------------------------------------------------------------------

  PasDoc

  * generates documentation from comments in Pascal unit source code files
  * command line program
  * written in Delphi
  * output formats Html and HtmlHelp
  * try PasDoc on its own source code

  * distributed under the GNU General Public License (GPL)

  * copyright (C) 1998-2000 by Marco Schmidt
  * copyright (C) 2001-2002 by Ralf Junker <delphi@zeitungsjunge.de>
  * Copyright (C) 2003 by Johannes Berg <johannes@sipsolutions.de>

  Hint:

  Whenever you use PasDoc for documentations, make sure the program file
  - like this is one - contains no code except for a call to a main routine in
  another unit or the instantiation of an object / class that does all the work
  (usually TApplication).

  Pasdoc is restricted to work on unit files only, that's why the program file
  should contain no actual program-specific code - it would not become part of
  the documentation.

------------------------------------------------------------------------------ }

program PasDoc_Console;

{$APPTYPE CONSOLE}

uses
  PasDoc in '../component/PasDoc.pas',
  PasDoc_Languages in '../component/PasDoc_Languages.pas',
  SysUtils,
  Utils in '../component/Utils.pas',
  Types in '../component/Types.pas',
  StringCardinalTree in '../component/StringCardinalTree.pas',
  StreamUtils in '../component/StreamUtils.pas',
  ObjectVector in '../component/ObjectVector.pas',
  PasDoc_GenHtml in '../component/PasDoc_GenHtml.pas',
  PasDoc_Gen in '../component/PasDoc_Gen.pas',
  PasDoc_Items in '../component/PasDoc_Items.pas',
  OptionParser in '../OptionParser/OptionParser.pas';

var
  GPasDoc: TPasDoc;
  GOptionParser: TOptionParser;
  GOption_Verbosity: TIntegerOption;
  GOption_Define: TStringOptionList;
  GOption_Help,
  GOption_Private: TBoolOption;
  GOption_Descriptions,
  GOption_ConditionalFile,
  GOption_IncludePaths,
  GOption_SourceList: TStringOptionList;
  GOption_ContentFile,
  GOption_Footer,
  GOption_Header,
  GOption_Name,
  GOption_Title,
  GOption_Format,
  GOption_OutputPath,
  GOption_Language: TStringOption;
  GOption_StarOnly,
  GOption_Generator: TBoolOption;

  { ---------------------------------------------------------------------------- }

procedure CreateOptions;
var
  l: TLanguageID;
begin
  GOptionParser := TOptionParser.Create;

  GOption_Help := TBoolOption.Create('?', 'help');
  GOption_Help.Explanation := 'show this help';
  GOptionParser.AddOption(GOption_Help);

  GOption_Verbosity := TIntegerOption.Create('v', 'verbosity');
  GOption_Verbosity.Value := DEFAULT_VERBOSITY_LEVEL;
  GOption_Verbosity.Explanation := 'set log verbosity (0-5) ['+IntToStr(DEFAULT_VERBOSITY_LEVEL)+']';
  GOptionParser.AddOption(GOption_Verbosity);

  GOption_Define := TStringOptionList.Create('D', 'define');
  GOption_Define.Explanation := 'define conditional';
  GOptionParser.AddOption(GOption_Define);

  GOption_Descriptions := TStringOptionList.Create('R', 'description');
  GOption_Descriptions.Explanation := 'read description from this file';
  GOptionParser.AddOption(GOption_Descriptions);

  GOption_ConditionalFile := TStringOptionList.Create('d', 'conditionals');
  GOption_ConditionalFile.Explanation := 'read conditionals from this file';
  GOptionParser.AddOption(GOption_ConditionalFile);

  GOption_IncludePaths := TStringOptionList.Create('I', 'include');
  GOption_IncludePaths.Explanation := 'includes search path';
  GOptionParser.AddOption(GOption_IncludePaths);

  GOption_Private := TBoolOption.Create('p', 'private', False);
  GOption_Private.Explanation := 'include private declarations';
  GOptionParser.AddOption(GOption_Private);

  GOption_SourceList := TStringOptionList.Create('S', 'source');
  GOption_SourceList.Explanation := 'read source filenames from file';
  GOptionParser.AddOption(GOption_SourceList);

  GOption_ContentFile := TStringOption.Create('C', 'content');
  GOption_ContentFile.Explanation := 'Read Contents for HtmlHelp from file';
  GOptionParser.AddOption(GOption_ContentFile);

  GOption_Footer := TStringOption.Create('F', 'footer');
  GOption_Footer.Explanation := 'include file as footer';
  GOptionParser.AddOption(GOption_Footer);

  GOption_Header := TStringOption.Create('H', 'header');
  GOption_Header.Explanation := 'include file as header';
  GOptionParser.AddOption(GOption_Header);

  GOption_Name := TStringOption.Create('N', 'name');
  GOption_Name.Explanation := 'Name for documentation';
  GOptionParser.AddOption(GOption_Name);

  GOption_Title := TStringOption.Create('T', 'title');
  GOption_Title.Explanation := 'Documentation title';
  GOptionParser.AddOption(GOption_Title);

  GOption_Format := TStringOption.Create('O', 'format');
  GOption_Format.Explanation := 'output format: html or htmlhelp';
  GOption_Format.Value := 'html';
  GOptionParser.AddOption(GOption_Format);

  GOption_OutputPath := TStringOption.Create('E', 'output');
  GOption_OutputPath.Explanation := 'output path';
  GOptionParser.AddOption(GOption_OutputPath);

  GOption_Generator := TBoolOption.Create('X', 'exclude-generator');
  GOption_Generator.Explanation := 'exclude generator information';
  GOptionParser.AddOption(GOption_Generator);

  GOption_Language := TStringOption.Create('L', 'language');
  GOption_Language.Explanation := 'Output language. Valid languages are: ' +
    #10;
  for l := Succ(lgDefault) to High(LANGUAGE_ARRAY) do
    GOption_Language.Explanation := GOption_Language.Explanation + '  ' +
      LANGUAGE_ARRAY[l].Syntax + ': ' + LANGUAGE_ARRAY[l].Name + #10;
  GOptionParser.AddOption(GOption_Language);

  GOption_StarOnly := TBoolOption.Create(#0, 'staronly');
  GOption_StarOnly.Explanation :=
    'Parse only {**, (**** and //** style comments';
  GOptionParser.AddOption(GOption_StarOnly);
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

  GPasDoc.HtmlHelpContentsFileName := GOption_ContentFile.Value;
  GPasDoc.Directives.Assign(GOption_Define.Values);
  for i := 0 to GOption_ConditionalFile.Values.Count - 1 do begin
    GPasDoc.Directives.LoadFromTextFileAdd(GOption_ConditionalFile.Values[i]);
  end;

  GPasDoc.OutputFolder := GOption_OutputPath.Value;
  if GOption_Footer.WasSpecified then begin
    GPasDoc.LoadFooterFromFile(GOption_Footer.Value);
  end;
  if GOption_Header.WasSpecified then begin
    GPasDoc.LoadHeaderFromFile(GOption_Header.Value);
  end;

  GPasDoc.IncludeDirectories.Assign(GOption_IncludePaths.Values);

  GOption_Language.Value := lowercase(GOption_Language.Value);
  for lng := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do begin
    if LowerCase(LANGUAGE_ARRAY[lng].Syntax) = GOption_Language.Value then
      begin
      GPasDoc.Language := lng;
      break;
    end;
  end;

  GPasDoc.ProjectName := GOption_Name.Value;

  GOption_Format.Value := LowerCase(GOption_Format.Value);
  if GOption_Format.Value = 'html' then begin
    GPasDoc.OutputFormat := ofHtml;
  end else begin
    if GOption_Format.Value = 'html' then begin
      GPasDoc.OutputFormat := ofHtmlHelp;
    end else begin
      GPasDoc.DoMessage(1, mtWarning, 'Unknown output format (%s), skipping.',
        [GOption_Format.Value]);
    end;
  end;

  GPasDoc.IncludePrivate := GOption_Private.TurnedOn;
  GPasDoc.DescriptionFileNames.Assign(GOption_Descriptions.Values);

  for i := 0 to GOption_SourceList.Values.Count - 1 do begin
    GPasDoc.AddSourceFileNamesFromFile(GOption_SourceList.Values[i]);
  end;

  GPasDoc.Title := GOption_Title.Value;

  GPasDoc.Verbosity := GOption_Verbosity.Value;

  GPasDoc.GeneratorInfo := not GOption_Generator.TurnedOn;

  GPasDoc.StarStyleOnly := GOption_StarOnly.TurnedOn;

  GPasDoc.SourceFileNames.AddStrings(GOptionParser.LeftList);

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
