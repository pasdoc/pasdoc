{
  Copyright 1998-2014 PasDoc developers.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ @abstract(PasDoc language definitions and translations.)
  @author(Johannes Berg <johannes AT sipsolutions.de>)
  @author(Ralf Junker <delphi AT zeitungsjunge.de>)
  @author(Andrew Andreev <andrew AT alteragate.net> (Bulgarian translation))
  @author(Alexander Lisnevsky <alisnevsky AT yandex.ru> (Russian translation))
  @author(Hendy Irawan <ceefour AT gauldong.net> (Indonesian and Javanese translation))
  @author(Ivan Montes Velencoso (Catalan and Spanish translations))
  @author(Javi (Spanish translation))
  @author(Jean Dit Bailleul (Frensh translation))
  @author(Marc Weustinks (Dutch translation))
  @author(Martin Hansen <mh AT geus.dk> (Danish translation))
  @author(Michele Bersini <michele.bersini AT smartit.it> (Italian translation))
  @author(Peter Šimkoviè <simkovic_jr AT manal.sk> (Slovak translation))
  @author(Peter Thörnqvist <pt AT timemetrics.se> (Swedish translation))
  @author(Rodrigo Urubatan Ferreira Jardim <rodrigo AT netscape.net> (Brasilian translation))
  @author(Alexandre da Silva <simpsomboy AT gmail.com> (Brasilian translation - Update))
  @author(Alexsander da Rosa <alex AT rednaxel.com> (Brasilian translation - UTF8))
  @author(Vitaly Kovalenko <v_l_kovalenko AT alsy.by> (Russian translation))
  @author(Grzegorz Skoczylas <gskoczylas AT rekord.pl> (corrected Polish translation))
  @author(Jónás Gergõ <jonas.gergo AT ch...> (Hungarian translation))
  @author(Michalis Kamburelis)
  @author(Ascanio Pressato (Some Italian translation))
  @author(JBarbero Quiter (updated Spanish translation))
  @author(Liu Chuanjun <1000copy AT gmail.com> (Chinese gb2312 translation))
  @author(Liu Da <xmacmail AT gmail.com> (Chinese gb2312 translation))
  @author(DoDi)
  @author(René Mihula <rene.mihula@gmail.com> (Czech translation))  
  @author(Yann Merignac (French translation))
  @author(Arno Garrels <first name.name@nospamgmx.de>)
}

unit PasDoc_Languages;

{$I pasdoc_defines.inc}

interface

type
  { An enumeration type of all supported languages }
{$IFDEF STRING_UNICODE}
  TLanguageID = (
    lgBosnian,
    lgBrazilian,
    lgBulgarian,
    lgCatalan,
    lgChinese,
    lgCroatian,
    lgDanish,
    lgDutch,
    lgEnglish,
    lgFrench,
    lgGerman,
    lgIndonesian,
    lgItalian,
    lgJavanese,
    lgPolish,
    lgRussian,
    lgSlovak,
    lgSpanish,
    lgSwedish,
    lgHungarian,
    lgCzech
  );
{$ELSE}
  TLanguageID = (
    lgBosnian,
    lgBrazilian_1252,
    lgBrazilian_utf8,
    lgBulgarian,
    lgCatalan,
    lgChinese_gb2312,
    lgCroatian,    
    lgDanish,
    lgDutch,
    lgEnglish,
    lgFrench_ISO_8859_15,
    lgFrench_UTF_8,    
    lgGerman,
    lgIndonesian,
    lgItalian,
    lgJavanese,
    lgPolish_CP1250,
    lgPolish_ISO_8859_2,
    lgRussian_1251,
    lgRussian_utf8,
    lgRussian_866,
    lgRussian_koi8,
    lgSlovak,
    lgSpanish,
    lgSwedish,
    lgHungarian_1250,
    lgCzech_CP1250,
    lgCzech_ISO_8859_2
   );
{$ENDIF}
  { An enumeration type of all static output texts.
    Warning: count and order changed!
  }
  TTranslationID = (
  //no translation ID assigned, so far
    trNoTrans,
  //the language name (English, ASCII), e.g. for file names.
    trLanguage,
  //map
    trUnits,
    trClassHierarchy,
    trCio,
    trInternalCR,
    trInternalTypes,
    trIdentifiers,
    trGvUses,
    trGvClasses,
  //tables and members
    trClasses,
      trClass,
      trDispInterface,
      trInterface,
    trObjects,
      trObject,
      trRecord,
        trHierarchy,
        trFields,
        trMethods,
        trProperties,
    trLibrary,
    trPackage,
    trProgram,
    trUnit,
      trUses,
      trConstants,
      trFunctionsAndProcedures,
      trTypes,
        trType,
      trVariables,
      trAuthors,
        trAuthor,
      trCreated,
      trLastModified,
    trSubroutine,
      trParameters,
      trReturns,
      trExceptionsRaised,
    trExceptions,
      trException,
    trEnum,

  //visibilities
    trVisibility,
      trPrivate,
      trStrictPrivate,
      trProtected,
      trStrictProtected,
      trPublic,
      trPublished,
      trAutomated,
      trImplicit,
  //hints
    trDeprecated,
    trPlatformSpecific,
    trLibrarySpecific,

  //headings
    trOverview,
    trIntroduction,
    trConclusion,
    trEnclosingClass,
    trHeadlineCio,
    trHeadlineConstants,
    trHeadlineFunctionsAndProcedures,
    trHeadlineIdentifiers,
    trHeadlineTypes,
    trHeadlineUnits,
    trHeadlineVariables,
    trSummaryCio,
  //column headings
    trDeclaration,
    trDescription, //<as column OR section heading!
    trDescriptions, //<section heading for detailed descriptions
    trName,
    trValues,

  //empty tables
    trNone,
    trNoCIOs,
    trNoCIOsForHierarchy,
    trNoTypes,
    trNoVariables,
    trNoConstants,
    trNoFunctions,
    trNoIdentifiers,

  //misc
    trHelp,
    trLegend,
    trMarker,

    trWarningOverwrite,
    trWarning,

    trGeneratedBy,
    trGeneratedOn,
    trOnDateTime,

    trSearch,
    trSeeAlso,
    trInternal,
  //add more here
    trAttributes,
    trDummy
  );

//array holding the translated strings, or empty for default (English) text.
  RTransTable = array[TTranslationID] of string;
  PTransTable = ^RTransTable;

//language descriptor
  PLanguageRecord = ^TLanguageRecord;
  TLanguageRecord = record
  {$IFDEF STRING_UNICODE}
    Table: PTransTable;
    Name: string;
    Syntax: string;
    AspellLanguage: string;
  {$ELSE}
    Table: PTransTable;
    Name: string;
    Syntax: string;
    CharSet: string;
    { Name of this language as used by Aspell, see
      http://aspell.net/man-html/Supported.html .
      
      Set this to empty string if it's the same as our Syntax up to a dot.
      So a Syntax = 'pl' or Syntax = 'pl.iso-8859-2' already indicates
      AspellLanguage = 'pl'.
      
      TODO: In the future, it would be nice if all language names used by PasDoc
      and Aspell matched. Aspell language naming follows the standard
      http://en.wikipedia.org/wiki/ISO_639-1 as far as I see,
      and we should probably follow it too (currently, we deviate for
      some languages).
      
      So in the future, we'll probably replace Syntax and AspellLanguage
      by LanguageCode and CharsetCode. LanguageCode = code (suitable for both
      PasDoc and Aspell command-line; the thing currently up to a dot in Syntax),
      CharsetCode = the short representation of CharSet (the thing currently
      after a dot in Syntax). }
    AspellLanguage: string;
  {$ENDIF}
  end;

const
  DEFAULT_LANGUAGE = lgEnglish;
  lgDefault = lgEnglish;

type
  { Language class to hold all translated strings }
  TPasDocLanguages = class
  private
    FLanguage: TLanguageID;
  {$IFDEF STRING_UNICODE}
    FCodePage: LongWord;
  {$ENDIF}
    procedure SetLanguage(const Value: TLanguageID);
  protected
  //the table of the selected language
    pTable: PTransTable;
    FCharSet: string;
    { @abstract(gets a translation token) }
    function GetTranslation(ATranslationID: TTranslationID): string;
    procedure SetTranslation(id: TTranslationID; const into: string);
    property FTranslation[id: TTranslationID]: string
      read GetTranslation write SetTranslation;
  public
    { Charset for current language }
    property CharSet: string read FCharSet;
  {$IFDEF STRING_UNICODE}
    property CodePage: LongWord read FCodePage;
  {$ENDIF}
    property Translation[ATranslationID: TTranslationID]: string read GetTranslation;
    constructor Create;
    property Language: TLanguageID read FLanguage write SetLanguage
      default DEFAULT_LANGUAGE;
  end;

//Some GUI helpers
{}

//Full language name
function LanguageFromIndex(i: integer): string;
function LanguageFromID(i: TLanguageID): string;

//Language abbreviation
function SyntaxFromIndex(i: integer): string;
function SyntaxFromID(i: TLanguageID): string;

//Search for language by short or long name
function IDfromLanguage(const s: string): TLanguageID;

//Manual translation of id into lang
function Translation(id: TTranslationID; lang: TLanguageID): string;

{ Find a language with Syntax = S (case ignored).
  Returns @true and sets LanguageId if found, otherwise returns @false. }
function LanguageFromStr(S: string; out LanguageId: TLanguageID): boolean;

//access LANGUAGE_ARRAY
function LanguageDescriptor(id: TLanguageID): PLanguageRecord;

{ Language code suitable for Aspell. }
function LanguageAspellCode(const Language: TLanguageID): string;

implementation

{$IFDEF fpc}
{$ELSE}
//Delphi
uses
  SysUtils;
{$ENDIF}

const
  { Translation markers.
    For ease of finding missing translations, special markers can be used:
    strToDo should be obvious ;-)
    strKeep means to keep the English (default language) wording. }
  strKeep = {$IFDEF debug} '=' {$else} '' {$endif};
  strToDo = {$IFDEF debug} '?' {$else} '' {$endif};

  { NewLanguageTemplate value is not actually used. We include it just to 
    force developers to keep PasDoc_Languages_Template_New_Language.inc
    in compileable state. }
  NewLanguageTemplate: {$I lang\PasDoc_Languages_Template_New_Language.inc}

{$IFDEF STRING_UNICODE}
  aEnglish            : {$I lang\PasDoc_Languages_English_utf8_bom.inc}
  aBosnian            : {$I lang\PasDoc_Languages_Bosnia_utf8_bom.inc}
  aBrazilian          : {$I lang\PasDoc_Languages_Brasilian_utf8_bom.inc}
  aBulgarian          : {$I lang\PasDoc_Languages_Bulgarian_utf8_bom.inc}
  aCatalan            : {$I lang\PasDoc_Languages_Catalan_utf8_bom.inc}
  aChinese            : {$I lang\PasDoc_Languages_Chinese_utf8_bom.inc}
  aCroatian           : {$I lang\PasDoc_Languages_Croatia_utf8_bom.inc}
  aDanish             : {$I lang\PasDoc_Languages_Danish_utf8_bom.inc}
  aDutch              : {$I lang\PasDoc_Languages_Dutch_utf8_bom.inc}
  aFrench             : {$I lang\PasDoc_Languages_French_utf8_bom.inc}
  aGerman             : {$I lang\PasDoc_Languages_German_utf8_bom.inc}
  aIndonesian         : {$I lang\PasDoc_Languages_Indonesian_utf8_bom.inc}
  aItalian            : {$I lang\PasDoc_Languages_Italian_utf8_bom.inc}
  aJavanese           : {$I lang\PasDoc_Languages_Javanese_utf8_bom.inc}
  aPolish             : {$I lang\PasDoc_Languages_Polish_utf8_bom.inc}
  aRussian            : {$I lang\PasDoc_Languages_Russian_utf8_bom.inc}
  aSlovak             : {$I lang\PasDoc_Languages_Slovak_utf8_bom.inc}
  aSpanish            : {$I lang\PasDoc_Languages_Spanish_utf8_bom.inc}
  aSwedish            : {$I lang\PasDoc_Languages_Swedish_utf8_bom.inc}
  aHungarian          : {$I lang\PasDoc_Languages_Hungarian_utf8_bom.inc}
  aCzech              : {$I lang\PasDoc_Languages_Czech_utf8_bom.inc}
{$ELSE}
  aEnglish            : {$I lang\PasDoc_Languages_English_utf8.inc}
  aBosnian            : {$I lang\PasDoc_Languages_Bosnia_1250.inc}
  aBrazilian_1252     : {$I lang\PasDoc_Languages_Brasilian_1252.inc}
  aBrazilian_utf8     : {$I lang\PasDoc_Languages_Brasilian_utf8.inc}
  aBulgarian          : {$I lang\PasDoc_Languages_Bulgarian_utf8.inc}
  aCatalan            : {$I lang\PasDoc_Languages_Catalan_1252.inc}
  aChinese_gb2312     : {$I lang\PasDoc_Languages_Chinese_gb2312.inc}
  aCroatian           : {$I lang\PasDoc_Languages_Croatia_1250.inc}
  aDanish             : {$I lang\PasDoc_Languages_Danish_1252.inc}
  aDutch              : {$I lang\PasDoc_Languages_Dutch_1252.inc}
  aFrench_ISO_8859_15 : {$I lang\PasDoc_Languages_French_ISO_8859_15.inc}
  aFrench_UTF_8       : {$I lang\PasDoc_Languages_French_utf8.inc}
  aGerman             : {$I lang\PasDoc_Languages_German_1252.inc}
  aIndonesian         : {$I lang\PasDoc_Languages_Indonesian_1252.inc}
  aItalian            : {$I lang\PasDoc_Languages_Italian_1252.inc}
  aJavanese           : {$I lang\PasDoc_Languages_Javanese_1250.inc}
  aPolish1250         : {$I lang\PasDoc_Languages_Polish_1250.inc}
  aPolish_ISO_8859_2  : {$I lang\PasDoc_Languages_Polish_iso_8859_2.inc}
  aRussian_1251       : {$I lang\PasDoc_Languages_Russian_1251.inc}
  aRussian_utf8       : {$I lang\PasDoc_Languages_Russian_utf8.inc}
  aRussian_866        : {$I lang\PasDoc_Languages_Russian_866.inc}
  aRussian_koi8       : {$I lang\PasDoc_Languages_Russian_koi8r.inc}
  aSlovak             : {$I lang\PasDoc_Languages_Slovak_1250.inc}
  aSpanish            : {$I lang\PasDoc_Languages_Spanish_1252.inc}
  aSwedish            : {$I lang\PasDoc_Languages_Swedish_1252.inc}
  aHungarian_1250     : {$I lang\PasDoc_Languages_Hungarian_1250.inc}
  aCzech_ISO_8859_2   : {$I lang\PasDoc_Languages_Czech_iso_8859_2.inc}
  aCzech_CP1250       : {$I lang\PasDoc_Languages_Czech_1250.inc}
{$ENDIF}

{$IFDEF STRING_UNICODE}
  LANGUAGE_ARRAY: array[TLanguageID] of TLanguageRecord = (
    (Table: @aBosnian; Name: 'Bosnian'; Syntax: 'ba'; AspellLanguage: 'bs'),
    (Table: @aBrazilian; Name: 'Brazilian'; Syntax: 'br'; AspellLanguage: 'pt'),
    (Table: @aBulgarian; Name: 'Bulgarian'; Syntax: 'bg'; AspellLanguage: ''),
    (Table: @aCatalan; Name: 'Catalan'; Syntax: 'ct'; AspellLanguage: 'ca'),
    (Table: @aChinese; Name: 'Chinese'; Syntax: 'zh'; AspellLanguage: 'zh'),
    (Table: @aCroatian; Name: 'Croatian'; Syntax: 'hr'; AspellLanguage: 'hr'),
    (Table: @aDanish; Name: 'Danish'; Syntax: 'dk'; AspellLanguage: 'da'),
    (Table: @aDutch; Name: 'Dutch'; Syntax: 'nl'; AspellLanguage: ''),
    (Table: @aEnglish; Name: 'English'; Syntax: 'en'; AspellLanguage: ''),
    (Table: @aFrench; Name: 'French'; Syntax: 'fr'; AspellLanguage: ''),
    (Table: @aGerman; Name: 'German'; Syntax: 'de'; AspellLanguage: ''),
    (Table: @aIndonesian; Name: 'Indonesian'; Syntax: 'id'; AspellLanguage: ''),
    (Table: @aItalian; Name: 'Italian'; Syntax: 'it'; AspellLanguage: ''),
    (Table: @aJavanese; Name: 'Javanese'; Syntax: 'jv'; AspellLanguage: ''),
    (Table: @aPolish; Name: 'Polish'; Syntax: 'pl'; AspellLanguage: ''),
    (Table: @aRussian;  Name: 'Russian'; Syntax: 'ru'; AspellLanguage: ''),
    (Table: @aSlovak; Name: 'Slovak'; Syntax: 'sk'; AspellLanguage: ''),
    (Table: @aSpanish; Name: 'Spanish'; Syntax: 'es'; AspellLanguage: ''),
    (Table: @aSwedish; Name: 'Swedish'; Syntax: 'se'; AspellLanguage: 'sv'),
    (Table: @aHungarian; Name: 'Hungarian'; Syntax: 'hu'; AspellLanguage: ''),
    (Table: @aCzech; Name: 'Czech'; Syntax: 'cz'; AspellLanguage: 'cs')
  );
{$ELSE}
  LANGUAGE_ARRAY: array[TLanguageID] of TLanguageRecord = (
    (Table: @aBosnian; Name: 'Bosnian (Codepage 1250)'; Syntax: 'ba'; CharSet: 'windows-1250'; AspellLanguage: 'bs'),
    (Table: @aBrazilian_1252; Name: 'Brazilian (Codepage 1252)'; Syntax: 'br.1252'; CharSet: 'windows-1252'; AspellLanguage: 'pt'),
    (Table: @aBrazilian_utf8; Name: 'Brazilian (Codepage UTF-8)'; Syntax: 'br.utf8'; CharSet: 'utf-8'; AspellLanguage: 'pt'),
    (Table: @aBulgarian; Name: 'Bulgarian (Codepage UTF-8)'; Syntax: 'bg'; CharSet: 'utf-8'; AspellLanguage: ''),
    (Table: @aCatalan; Name: 'Catalan'; Syntax: 'ct'; CharSet: 'windows-1252'; AspellLanguage: 'ca'),
    (Table: @aChinese_gb2312; Name: 'Chinese (Simple, gb2312)'; Syntax: 'gb2312'; CharSet: 'gb2312'; AspellLanguage: 'zh'),
    (Table: @aCroatian; Name: 'Croatian'; Syntax: 'hr'; CharSet: 'windows-1250'; AspellLanguage: 'hr'),    
    (Table: @aDanish; Name: 'Danish'; Syntax: 'dk'; CharSet: 'iso-8859-15'; AspellLanguage: 'da'),
    (Table: @aDutch; Name: 'Dutch'; Syntax: 'nl'; CharSet: 'iso-8859-15'; AspellLanguage: ''),
    (Table: @aEnglish; Name: 'English'; Syntax: 'en'; CharSet: 'utf-8'; AspellLanguage: ''),
    (Table: @aFrench_ISO_8859_15; Name: 'French (iso-8859-15)'; Syntax: 'fr'; CharSet: 'iso-8859-15'; AspellLanguage: ''),
    (Table: @aFrench_UTF_8; Name: 'French (UTF-8)'; Syntax: 'fr.utf8'; CharSet: 'utf-8'; AspellLanguage: ''),    
    (Table: @aGerman; Name: 'German'; Syntax: 'de'; CharSet: 'iso-8859-15'; AspellLanguage: ''),
    (Table: @aIndonesian; Name: 'Indonesian'; Syntax: 'id'; CharSet: 'windows-1252'; AspellLanguage: ''),
    (Table: @aItalian; Name: 'Italian'; Syntax: 'it'; CharSet: 'iso-8859-15'; AspellLanguage: ''),
    (Table: @aJavanese; Name: 'Javanese'; Syntax: 'jv'; CharSet: 'windows-1252'; AspellLanguage: ''),
    (Table: @aPolish1250; Name: 'Polish (Codepage CP1250)'; Syntax: 'pl.cp1250'; CharSet: 'windows-1250'; AspellLanguage: ''),
    (Table: @aPolish_ISO_8859_2; Name: 'Polish (Codepage ISO 8859-2)'; Syntax: 'pl.iso-8859-2'; CharSet: 'iso-8859-2'; AspellLanguage: ''),
    (Table: @aRussian_1251; Name: 'Russian (Codepage 1251)'; Syntax: 'ru.1251'; CharSet: 'windows-1251'; AspellLanguage: ''),
    (Table: @aRussian_utf8; Name: 'Russian (Codepage UTF-8)'; Syntax: 'ru.utf8'; CharSet: 'utf-8'; AspellLanguage: ''),
    (Table: @aRussian_866;  Name: 'Russian (Codepage 866)'; Syntax: 'ru.866'; CharSet: 'IBM866'; AspellLanguage: ''),
    (Table: @aRussian_koi8;  Name: 'Russian (KOI-8)'; Syntax: 'ru.koi8r'; CharSet: 'koi8-r'; AspellLanguage: ''),
    (Table: @aSlovak; Name: 'Slovak (Codepage 1250)'; Syntax: 'sk'; CharSet: 'windows-1250'; AspellLanguage: ''),
    (Table: @aSpanish; Name: 'Spanish'; Syntax: 'es'; CharSet: 'iso-8859-15'; AspellLanguage: ''),
    (Table: @aSwedish; Name: 'Swedish'; Syntax: 'se'; CharSet: 'iso-8859-15'; AspellLanguage: 'sv'),
    (Table: @aHungarian_1250; Name: 'Hungarian (Codepage 1250)'; Syntax: 'hu.1250'; CharSet: 'windows-1250'; AspellLanguage: ''),
    (Table: @aCzech_CP1250; Name: 'Czech (Codepage 1250)'; Syntax: 'cz'; CharSet: 'windows-1250'; AspellLanguage: ''),
    (Table: @aCzech_ISO_8859_2; Name: 'Czech (Codepage ISO 8859-2)'; Syntax: 'cz.iso-8859-2'; CharSet: 'iso-8859-2'; AspellLanguage: 'cs')
  );
{$ENDIF}

function TPasDocLanguages.GetTranslation(
  ATranslationID: TTranslationID): string;
begin
  Result := pTable^[ATranslationID];
  if Result <= strKeep then
    Result := aEnglish[ATranslationID];
end;

procedure TPasDocLanguages.SetTranslation(id: TTranslationID;
  const into: string);
begin
  pTable^[id] := into;
end;

constructor TPasDocLanguages.Create;
begin
  inherited;
  SetLanguage(DEFAULT_LANGUAGE);
end;

procedure TPasDocLanguages.SetLanguage(const Value: TLanguageID);
begin
  FLanguage := Value;
{$IFNDEF STRING_UNICODE}
  FCharSet  := LANGUAGE_ARRAY[Value].Charset;
{$ELSE} // String is UTF-16 so get rid of this ANSI stuff.
  FCharSet  := 'UTF-8';
  FCodePage := 65001;
{$ENDIF}

//get table
  pTable := LANGUAGE_ARRAY[Value].Table;
  Assert(Assigned(pTable));
end;

function LanguageFromStr(S: string; out LanguageId: TLanguageID): boolean;
var
  I: TLanguageID;
begin
  S := LowerCase(S);
  for I := Low(LANGUAGE_ARRAY) to High(LANGUAGE_ARRAY) do
  begin
    if LowerCase(LANGUAGE_ARRAY[I].Syntax) = S then
    begin
      Result := true;
      LanguageId := I;
      Exit;
    end;
  end;

  Result := false;
end;

//------------- language helpers, for PasDoc_gui -----------------

function LanguageFromIndex(i: integer): string;
begin
  Result := language_array[TLanguageID(i)].Name;
end;

function LanguageFromID(i: TLanguageID): string;
begin
  Result := language_array[i].Name;
end;

function SyntaxFromIndex(i: integer): string;
var
  l: TLanguageID absolute i;
begin
  Result := Language_array[l].Syntax;
end;

function SyntaxFromID(i: TLanguageID): string;
begin
  Result := Language_array[i].Syntax;
end;

function IDfromLanguage(const s: string): TLanguageID;
var
  i: TLanguageID;
begin
  for i := low(i) to high(i) do begin
    if (LANGUAGE_ARRAY[i].Name = s)
    or (LANGUAGE_ARRAY[i].Syntax = s) then begin
      Result := i;
      exit;
    end;
  end;
  Result := DEFAULT_LANGUAGE;
end;

function LanguageDescriptor(id: TLanguageID): PLanguageRecord;
begin
  Result := @Language_array[id];
end;

function  Translation(id: TTranslationID; lang: TLanguageID): string;
var
  tbl: PTransTable;
begin
  tbl := LANGUAGE_ARRAY[lang].Table;
  if not assigned(tbl) then
    tbl := @aEnglish;
  Result := tbl^[id];
end;

function LanguageAspellCode(const Language: TLanguageID): string;
var
  Dot: Integer;
begin
  Result := LANGUAGE_ARRAY[Language].AspellLanguage;
  if Result = '' then
  begin
    Result := LANGUAGE_ARRAY[Language].Syntax;
    Dot := Pos('.', Result);
    if Dot <> 0 then SetLength(Result, Dot - 1); { cut stuff after '.' }
  end;
end;

end.

