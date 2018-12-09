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

{ @abstract(Spellchecking using Aspell.) }
unit PasDoc_Aspell;

{$I pasdoc_defines.inc}

interface

uses SysUtils, Classes, PasDoc_ProcessLineTalk, PasDoc_ObjectVector, PasDoc_Types;

type
  TSpellingError = class
  public
    { the mis-spelled word }
    Word: string;
    { offset inside the checked string }
    Offset: Integer;
    { comma-separated list of suggestions }
    Suggestions: string;
  end;

  { This is a class to interface with aspell through pipe.
    It uses underlying @link(TProcessLineTalk) to execute and
    "talk" with aspell. }
  TAspellProcess = class
  private
    FProcess: TProcessLineTalk;
    FAspellMode: string;
    FAspellLanguage: string;
    FOnMessage: TPasDocMessageEvent;

    procedure DoMessage(const AVerbosity: Cardinal;
      const MessageType: TPasDocMessageType; const AMessage: string);
  public
    { Constructor.
      Values for AspellMode and AspellLanguage are the same as for
      aspell @--mode and @--lang command-line options.
      You can pass here '', then we will not pass appropriate
      command-line option to aspell. }
    constructor Create(const AAspellMode, AAspellLanguage: string;
      AOnMessage: TPasDocMessageEvent);
    destructor Destroy; override;

    property AspellMode: string read FAspellMode;

    property AspellLanguage: string read FAspellLanguage;

    procedure SetIgnoreWords(Value: TStringList);

    { Spellchecks AString and returns result.
      Will create an array of TSpellingError objects,
      one entry for each misspelled word.
      Offsets of TSpellingErrors will be relative to AString. }
    procedure CheckString(const AString: string; const AErrors: TObjectVector);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses PasDoc_Utils;

function StringsJoin(const List: TStrings; const Glue: string): string;
var
  I: Integer;
begin
  if List.Count <> 0 then
  begin
    Result := List[0];
    for I := 1 to List.Count - 1 do
      Result := Result + Glue + List[I];
  end else
    Result := '';
end;

constructor TAspellProcess.Create(const AAspellMode, AAspellLanguage: string;
  AOnMessage: TPasDocMessageEvent);
var FirstAspellLine: string;
begin
  inherited Create;

  FAspellMode := AAspellMode;
  FAspellLanguage := AAspellLanguage;
  FOnMessage := AOnMessage;

  FProcess := TProcessLineTalk.Create(nil);

  { calculate FProcess.Executable / Parameters }
  FProcess.Executable := 'aspell';
  FProcess.Parameters.Add('-a');
  if AspellMode <> '' then
    FProcess.Parameters.Add(' --mode=' + AspellMode);
  if AspellLanguage <> '' then
    FProcess.Parameters.Add(' --lang=' + AspellLanguage);

  DoMessage(3, pmtInformation, 'Calling aspell process: "' +
    FProcess.Executable + ' ' +
    StringsJoin(FProcess.Parameters, ' ') + '"');

  { execute }
  FProcess.Execute;

  { read and check 1st aspell output line }
  FirstAspellLine := FProcess.ReadLine;
  if Copy(FirstAspellLine, 1, 4) <> '@(#)' then
    raise Exception.CreateFmt('Wrong introduction from aspell: "%s"',
      [FirstAspellLine]);

  { switch to aspell terse mode (do not report about correct words;
    report only mispellings) }
  FProcess.WriteLine('!');
end;

destructor TAspellProcess.Destroy;
begin
  FProcess.Free;
  inherited;
end;

procedure TAspellProcess.SetIgnoreWords(Value: TStringList);
var
  i: Integer;
begin
  for i := 0 to Value.Count - 1 do
    FProcess.WriteLine('@' + Value[i]);
end;

procedure TAspellProcess.CheckString(const AString: string;
  const AErrors: TObjectVector);
var
  s: string;
  p, p2: Integer;
  LError: TSpellingError;
begin
  AErrors.Clear;

  { make sure that FAspellMode is set -- should be removed, since it's
    passed to aspell command-line ? TODO. }
  if AspellMode <> '' then
  begin
    FProcess.WriteLine('-');
    FProcess.WriteLine('+' + AspellMode);
  end;

  { request spell-checking AString }
  FProcess.WriteLine('^' + SCharsReplace(AString, WhiteSpaceNL, ' '));

  repeat
    s := FProcess.ReadLine;
    { aspell returns empty line when it finished spell-checking AString }
    if s = '' then break;

    case s[1] of
      '*': Continue; // no error
      '#': begin
             LError := TSpellingError.Create;
             s := copy(s, 3, MaxInt); // get rid of '# '
             p := Pos(' ', s);
             LError.Word := copy(s, 1, p-1); // get word
             LError.Suggestions := '';
             s := copy(s, p+1, MaxInt);
             LError.Offset := StrToIntDef(s, 0)-1;
             AErrors.Add(LError);
           end;
      '&': begin
             LError := TSpellingError.Create;
             s := copy(s, 3, MaxInt); // get rid of '& '
             p := Pos(' ', s);
             LError.Word := copy(s, 1, p-1); // get word
             s := copy(s, p+1, MaxInt);
             p := Pos(' ', s);
             s := copy(s, p+1, MaxInt);
             p2 := Pos(':', s);
             LError.Suggestions := Copy(s, Pos(':', s)+2, MaxInt);
             SetLength(s, p2-1);
             LError.Offset := StrToIntDef(s, 0)-1;
             AErrors.Add(LError);
           end;
      else
        { Actually, it's nowhere formally specified that aspell error
          messages start with "Error:". So we can possibly accidentaly
          skip some error messages from aspell. }
        if IsPrefix('Error:', S) then
          DoMessage(2, pmtWarning, 'Aspell error: ' + S);
    end;
  until false;
end;

procedure TAspellProcess.DoMessage(const AVerbosity: Cardinal;
  const MessageType: TPasDocMessageType;  const AMessage: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(MessageType, AMessage, AVerbosity);
end;

end.
