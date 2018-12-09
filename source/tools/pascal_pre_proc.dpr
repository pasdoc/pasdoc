{ -*- compile-command: "make -C ../../ build-pascal_pre_proc" -*- }

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

{ A Pascal preprocessor.
  See [https://github.com/pasdoc/pasdoc/wiki/OtherTools] for
  documentation.

  TODO: should be made configurable to preserve some directives.
  Conditional compilation, macros, includes are handled fully
  by this preprocessor, so they should be stripped, but some
  directives should be usually left in the source for the actual compiler.
  Things like $mode, or $H+.
}

program pascal_pre_proc;

uses SysUtils, Classes, PasDoc_OptionParser, PasDoc_StringVector, PasDoc_Base,
  PasDoc_Tokenizer, PasDoc_Scanner, PasDoc_Types, PasDoc_Versions;

{ TPascalPreProcessor -------------------------------------------------------- }

type
  TPascalPreProcessor = class
  private
    procedure MessageEvent(const MessageType: TPasDocMessageType;
      const AMessage: string; const AVerbosity: Cardinal);
  public
    Verbosity: Cardinal;
    HandleMacros: boolean;
    Directives: TStringVector;
    IncludeDirectories: TStringVector;
    constructor Create;
    destructor Destroy; override;
    procedure Process(const InputFileName: string);
  end;

constructor TPascalPreProcessor.Create;
begin
  inherited;
  Directives := TStringVector.Create;
  IncludeDirectories := TStringVector.Create;
end;

destructor TPascalPreProcessor.Destroy;
begin
  FreeAndNil(Directives);
  FreeAndNil(IncludeDirectories);
  inherited;
end;

procedure TPascalPreProcessor.MessageEvent(
  const MessageType: TPasDocMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  if (AVerbosity <= Verbosity) then
    case MessageType of
      pmtInformation: WriteLn(ErrOutput, 'Info[', AVerbosity, ']:    ', AMessage);
      pmtWarning: WriteLn(ErrOutput, 'Warning[', AVerbosity, ']: ', AMessage);
      pmtError: WriteLn(ErrOutput, 'Error[', AVerbosity, ']:   ', AMessage);
    else
      WriteLn(ErrOutput, AMessage);
    end;
end;

procedure TPascalPreProcessor.Process(const InputFileName: string);
var
  Scanner: TScanner;
  InputStream: TStream;
  T: TToken;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead);
  { We do not free InputStream anywhere in this procedure,
    Scanner will take care of this. }

  Scanner := TScanner.Create(InputStream,
    {$ifdef FPC_OBJFPC}@{$endif} MessageEvent ,
    Verbosity, InputFileName, ExtractFilePath(InputFileName),
    HandleMacros);
  try
    Scanner.IncludeFilePaths := IncludeDirectories;
    Scanner.AddSymbols(Directives);
    try
      repeat
        T := Scanner.GetToken;
        Write(T.Data);
      until false;
    except
      on ETokenizerStreamEnd do
        { silence exception, as this is the normal end };
    end;
  finally FreeAndNil(Scanner) end;
end;

{ TPascalPreProcessorOptions ------------------------------------------------- }

type
  TPascalPreProcessorOptions = class(TOptionParser)
    OptionVerbosity: TIntegerOption;
    OptionDefine: TStringOptionList;
    OptionHelp: TBoolOption;
    OptionIncludePaths: TPathListOption;
    OptionConditionalFile: TStringOptionList;
    OptionNoMacro: TBoolOption;
  public
    constructor Create; override;
    procedure InterpretCommandline(PreProcessor: TPascalPreProcessor);
  end;

constructor TPascalPreProcessorOptions.Create;
begin
  inherited;

  OptionHelp := TBoolOption.Create('?', 'help');
  OptionHelp.Explanation := 'Show this help';
  AddOption(OptionHelp);

  OptionVerbosity := TIntegerOption.Create('v', 'verbosity');
  OptionVerbosity.Value := DEFAULT_VERBOSITY_LEVEL;
  OptionVerbosity.Explanation := 'Set log verbosity (0-6) ['+IntToStr(DEFAULT_VERBOSITY_LEVEL)+']';
  AddOption(OptionVerbosity);

  OptionDefine := TStringOptionList.Create('D', 'define');
  OptionDefine.Explanation := 'Define conditional';
  AddOption(OptionDefine);

  OptionConditionalFile := TStringOptionList.Create('d', 'conditionals');
  OptionConditionalFile.Explanation := 'Read conditionals from this file';
  AddOption(OptionConditionalFile);

  OptionIncludePaths := TPathListOption.Create('I', 'include');
  OptionIncludePaths.Explanation := 'Includes search path';
  AddOption(OptionIncludePaths);

  OptionNoMacro := TBoolOption.Create(#0, 'no-macro');
  OptionNoMacro.Explanation := 'Turn FPC macro support off';
  AddOption(OptionNoMacro);
end;

procedure TPascalPreProcessorOptions.InterpretCommandline(
  PreProcessor: TPascalPreProcessor);
var
  I: Integer;
begin
  PreProcessor.Directives.Assign(OptionDefine.Values);

  for I := 0 to OptionConditionalFile.Values.Count - 1 do
    PreProcessor.Directives.LoadFromTextFileAdd(OptionConditionalFile.Values[I]);

  PreProcessor.IncludeDirectories.Assign(OptionIncludePaths.Values);

  PreProcessor.Verbosity := OptionVerbosity.Value;

  PreProcessor.HandleMacros := not OptionNoMacro.TurnedOn;
end;

{ Main program --------------------------------------------------------------- }

{ If CATCH_EXCEPTIONS then exceptions will be catched and displayed
  nicely using MessageEvent, which means that exceptions will be
  reported like other errors of pasdoc. This is what users most
  definitely want.

  For debug purposes, it's sometimes useful to undefine this,
  to let the default FPC exception handler do it's work and
  output nice backtrace of the exception. }
{$define CATCH_EXCEPTIONS}

var
  PreProcessor: TPascalPreProcessor;
  InputFileName: string;
  Options: TPascalPreProcessorOptions;
begin
  PreProcessor := TPascalPreProcessor.Create;
  try

    {$ifdef CATCH_EXCEPTIONS}
    try
    {$endif}

      Options := TPascalPreProcessorOptions.Create;
      try
        Options.ParseOptions;
        if Options.OptionHelp.TurnedOn then
        begin
          Writeln('pascal_pre_proc: Pascal preprocessor.');
          Writeln('Using PasDoc parser:');
          Writeln(PASDOC_FULL_INFO);
          Writeln;
          WriteLn('Usage:');
          WriteLn('  pascal_pre_proc [options] file-name');
          Writeln('This will parse file-name, interpreting all compiler directives');
          Writeln('(like $define, $ifdef, $include, handling also FPC macros)');
          Writeln('and output the result on standard output.');
          Writeln;
          WriteLn('Valid options are: ');
          Options.WriteExplanations;
          Exit;
        end;

        Options.InterpretCommandline(PreProcessor);

        if Options.LeftList.Count = 0 then
          raise Exception.Create('You must provide a filename to parse');
        if Options.LeftList.Count > 1 then
          raise Exception.Create('You cannot provide more than one filename to parse');

        InputFileName := Options.LeftList[0];

      finally FreeAndNil(Options) end;

      PreProcessor.Process(InputFileName);

    {$ifdef CATCH_EXCEPTIONS}
    except
      on E: Exception do
      begin
        PreProcessor.MessageEvent(pmtError, E.Message, 1);
        ExitCode := 1;
        Exit;
      end;
    end;
    {$endif CATCH_EXCEPTIONS}

  finally FreeAndNil(PreProcessor) end;
end.