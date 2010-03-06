{ @abstract(PasDoc language definitions and translations.)
  @author(Johannes Berg <johannes AT sipsolutions.de>)
  @author(Ralf Junker <delphi AT zeitungsjunge.de>)
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
}

unit PasDoc_Languages;

interface

type
  { An enumeration type of all supported languages }
  TLanguageID = (
    lgBosnian,
    lgBrasilian,
    lgCatalan,
    lgChinese_gb2312,
    lgDanish,
    lgDutch,
    lgEnglish,
    lgFrench,
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
    trOnDateTime,

    trSearch,
    trSeeAlso,
  //add more here
    trDummy
  );

//array holding the translated strings, or empty for default (English) text.
  RTransTable = array[TTranslationID] of string;
  PTransTable = ^RTransTable;

//language descriptor
  PLanguageRecord = ^TLanguageRecord;
  TLanguageRecord = record
    Table: PTransTable;
    Name: string;
    Syntax: string;
    CharSet: string;
  end;

const
  DEFAULT_LANGUAGE = lgEnglish;
  lgDefault = lgEnglish;

type
  { Language class to hold all translated strings }
  TPasDocLanguages = class
  private
    FLanguage: TLanguageID;
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
  //following languages/codepages need transformation into tables
    { Defines translations for Russian (Codepage 866). }
    procedure SetLanguageRussian_866;
    { Defines translations for Russian (KOI-8). }
    procedure SetLanguageRussian_koi8;
  public
    { Charset for current language }
    property CharSet: string read FCharSet;
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

implementation

{$IFDEF fpc}
{$ELSE}
//Delphi
uses
  SysUtils;
{$ENDIF}

const
(* Translation markers
  For ease of finding missing translations, special markers can be used:
  strToDo should be obvious ;-)
  strKeep means to keep the English (default language) wording.
*)
{$IFDEF debug}
  strKeep = '='; //keep English wording
  strToDo = '?'; //to be translated
{$ELSE}
  strKeep = ''; //'='? keep English wording
  strToDo = ''; //'?'? to be translated
{$ENDIF}

(* New language template. To add a new language or encoding:

1)  Copy aNewLanguage into a const section and rename it to the new language name.

2)  Put a reference to the new const array into LANGUAGE_ARRAY[...].Table.

3)  Then replace all occurences of strToDo by your translation of the text in the comment,
  or rename them into strKeep for all strings that need no translation,
  or leave the strToDo in place, it will be replaced by the default (English) text.
*)
var //writeable, for old (explicit) setup
  aNewLanguage: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} strToDo, //<<<<<< replace with the name of the new language
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} strToDo, //'Classes, Interfaces, Objects and Records',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} strToDo, //'Classes',
      {trClass} strToDo, //'Class',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} strToDo, //'Objects',
      {trObject} strToDo, //'Object',
      {trRecord} strToDo, //'Record',
        {trHierarchy} strToDo, //'Hierarchy',
        {trFields} strToDo, //'Fields',
        {trMethods} strToDo, //'Methods',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} strToDo, //'Constants',
      {trFunctionsAndProcedures} strToDo, //'Functions and Procedures',
      {trTypes} strToDo, //'Types',
        {trType} strToDo, //'Type',
      {trVariables} strToDo, //'Variables',
      {trAuthors} strToDo, //'Authors',
        {trAuthor} strToDo, //'Author',
      {trCreated} strToDo, //'Created',
      {trLastModified} strToDo, //'Last Modified',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} strToDo, //'Overview',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} strToDo, //'All Classes, Interfaces, Objects and Records',
    {trHeadlineConstants} strToDo, //'All Constants',
    {trHeadlineFunctionsAndProcedures} strToDo, //'All Functions and Procedures',
    {trHeadlineIdentifiers} strToDo, //'All Identifiers',
    {trHeadlineTypes} strToDo, //'All Types',
    {trHeadlineUnits} strToDo, //'All Units',
    {trHeadlineVariables} strToDo, //'All Variables',
    {trSummaryCio} strToDo, //'Summary of Classes, Interfaces, Objects and Records',
  //column headings
    {trDeclaration} strToDo, //'Declaration',
    {trDescription} strToDo, //'Description',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} strToDo, //'Name',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} strToDo, //'None',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} strToDo, //'Legend',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} strToDo, //'Warning: Do not edit - this file has been created automatically and is likely be overwritten',
    {trWarning} strToDo, //'Warning',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

const
  aEnglish: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'English',
  //map
    {trUnits} 'Units',
    {trClassHierarchy} 'Class Hierarchy',
    {trCio} 'Classes, Interfaces, Objects and Records',
    {trIdentifiers} 'Identifiers',
    {trGvUses} 'Unit dependency graph',
    {trGvClasses} 'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Classes',
      {trClass} 'Class',
      {trDispInterface} 'DispInterface',
      {trInterface} 'Interface',
    {trObjects} 'Objects',
      {trObject} 'Object',
      {trRecord} 'Record',
        {trHierarchy} 'Hierarchy',
        {trFields} 'Fields',
        {trMethods} 'Methods',
        {trProperties} 'Properties',
    {trLibrary} 'Library',
    {trPackage} 'Package',
    {trProgram} 'Program',
    {trUnit} 'Unit',
      {trUses} 'Uses',
      {trConstants} 'Constants',
      {trFunctionsAndProcedures} 'Functions and Procedures',
      {trTypes} 'Types',
        {trType} 'Type',
      {trVariables} 'Variables',
      {trAuthors} 'Authors',
        {trAuthor} 'Author',
      {trCreated} 'Created',
      {trLastModified} 'Last Modified',
    {trSubroutine} 'Subroutine',
      {trParameters} 'Parameters',
      {trReturns} 'Returns',
      {trExceptionsRaised} 'Exceptions raised',
    {trExceptions} 'Exceptions',
      {trException} 'Exception',
    {trEnum} 'Enumeration',
  //visibilities
    {trVisibility} 'Visibility',
      {trPrivate} 'Private',
      {trStrictPrivate} 'Strict Private',
      {trProtected} 'Protected',
      {trStrictProtected} 'Strict Protected',
      {trPublic} 'Public',
      {trPublished} 'Published',
      {trAutomated} 'Automated',
      {trImplicit} 'Implicit',
  //hints
    {trDeprecated} 'this symbol is deprecated',
    {trPlatformSpecific} 'this symbol is specific to some platform',
    {trLibrarySpecific} 'this symbol is specific to some library',
  //headings
    {trOverview} 'Overview',
    {trIntroduction} 'Introduction',
    {trConclusion} 'Conclusion',
    {trHeadlineCio} 'All Classes, Interfaces, Objects and Records',
    {trHeadlineConstants} 'All Constants',
    {trHeadlineFunctionsAndProcedures} 'All Functions and Procedures',
    {trHeadlineIdentifiers} 'All Identifiers',
    {trHeadlineTypes} 'All Types',
    {trHeadlineUnits} 'All Units',
    {trHeadlineVariables} 'All Variables',
    {trSummaryCio} 'Summary of Classes, Interfaces, Objects and Records',
  //column headings
    {trDeclaration} 'Declaration',
    {trDescription} 'Description',
    {trDescriptions} 'Detailed Descriptions',
    {trName} 'Name',
    {trValues} 'Values',
  //empty
    {trNone} 'None',
    {trNoCIOs} 'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} 'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} 'The units do not contain any types.',
    {trNoVariables} 'The units do not contain any variables.',
    {trNoConstants} 'The units do not contain any constants.',
    {trNoFunctions} 'The units do not contain any functions or procedures.',
    {trNoIdentifiers} 'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Help',
    {trLegend} 'Legend',
    {trMarker} 'Marker',
    {trWarningOverwrite} 'Warning: Do not edit - this file has been created automatically and is likely be overwritten',
    {trWarning} 'Warning',
    {trGeneratedBy} 'Generated by',
    {trOnDateTime} 'on',
    {trSearch} 'Search',
    {trSeeAlso} 'See also',
    ''  //dummy
  );

  aBosnian: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Bosnian',
  //map
    {trUnits} 'Fajlovi',
    {trClassHierarchy} 'Klasna hijerarhija',
    {trCio} 'Klase, Interfejsi i Objekti',
    {trIdentifiers} 'Identifikatori',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Klase',
      {trClass} 'Klasa',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} 'Interfejs',
    {trObjects} 'Objekti',
      {trObject} 'Objekt',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hijerarhija',
        {trFields} 'Polja',
        {trMethods} 'Metode',
        {trProperties} 'Osibine',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Fajl',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konstante',
      {trFunctionsAndProcedures} 'Funkcije i Procedure',
      {trTypes} 'Tipovi',
        {trType} 'Tip',
      {trVariables} 'Promjenjive',
      {trAuthors} 'Autori',
        {trAuthor} 'Autor',
      {trCreated} 'Kreirano',
      {trLastModified} 'Zadnja promjena',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} 'Privatni',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} 'Zaštiæen',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} 'Publikovan',
      {trPublished} 'Javan',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Pregled',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Sve Klase, Interfejsi i Objekti',
    {trHeadlineConstants} 'Sve Konstante',
    {trHeadlineFunctionsAndProcedures} 'Sve Funkcije i Procedure',
    {trHeadlineIdentifiers} 'Svi Identifikatoti',
    {trHeadlineTypes} 'Svi Tipovi',
    {trHeadlineUnits} 'Svi Fajlovi',
    {trHeadlineVariables} 'Sve Varijable',
    {trSummaryCio} 'Zbirno od Klasa, Interfejsa i Objekata',
  //column headings
    {trDeclaration} 'Deklaracija',
    {trDescription} 'Opis',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Ime',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Ništa',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Pomoæ',
    {trLegend} 'Legenda',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Upozorenje: Ne mjenjajte fajl - ovaj fajl je kreiran automatski i velika je vjerovatnoæa da æe biti prepisan',
    {trWarning} strToDo, //'Warning',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aBrasilian: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Brasilian',
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} 'Hierarquia de Classes',
    {trCio} 'Classes, Interfaces, Objetos e Registros',
    {trIdentifiers} 'Identificadores',
    {trGvUses} 'Diagrama de dependências de units',
    {trGvClasses} 'Diagrama de hierarquia de Classes',
  //tables and members
    {trClasses} strKeep, //'Classes',
      {trClass} 'Classe',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strKeep, //'Interface',
    {trObjects} 'Objetos',
      {trObject} 'Objeto',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hierarquia',
        {trFields} 'Campos',
        {trMethods} 'Métodos',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Constantes',
      {trFunctionsAndProcedures} 'Funções e Procedimentos',
      {trTypes} 'Tipos',
        {trType} 'Tipo',
      {trVariables} 'Variáveis',
      {trAuthors} 'Autores',
        {trAuthor} 'Autor',
      {trCreated} 'Criada',
      {trLastModified} 'Última modificação',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Parâmetros',
      {trReturns} 'Retornos', //???
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} 'Exceções',
      {trException} strToDo, //'Exception',
    {trEnum} 'Enumerações', //???
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} 'este símbolo está depreciado',
    {trPlatformSpecific} 'este símbolo é específico para alguma plataforma',
    {trLibrarySpecific} 'este símbolo é específico para alguma biblioteca',
  //headings
    {trOverview} 'Visão Geral',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Todas as Classes, Interfaces, Objetos e Registros',
    {trHeadlineConstants} 'Todas as Constantes',
    {trHeadlineFunctionsAndProcedures} 'Todas as funções e procedimentos',
    {trHeadlineIdentifiers} 'Todos os Identificadores',
    {trHeadlineTypes} 'Todos os Tipos',
    {trHeadlineUnits} 'Todas as Units',
    {trHeadlineVariables} 'Todas as Variáveis',
    {trSummaryCio} 'Lista das Classes, Interfaces, Objetos e Registros',
  //column headings
    {trDeclaration} 'Declaração',
    {trDescription} 'Descrição',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nome',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Nenhum',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Ajuda',
    {trLegend} 'Legenda',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Aviso, não altere - este arquivo foi gerado automaticamente e será sobrescrito',
    {trWarning} strToDo, //'Warning',
    {trGeneratedBy} 'Gerado por',
    {trOnDateTime} 'as',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aCatalan: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Catalan',
  //map
    {trUnits} 'Unitats',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Clases, interfaces i objectes',
    {trIdentifiers} 'Identificadors',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Clases',
      {trClass} 'Clase',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objectes',
      {trObject} 'Objecte',
      {trRecord} strToDo, //'Record',
        {trHierarchy} strToDo, //'Hierarchy',
        {trFields} 'Camps',
        {trMethods} 'MŠtodes',
        {trProperties} 'Propietats',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Unitat',
      {trUses} strToDo, //'Uses',
      {trConstants} strToDo, //'Constants',
      {trFunctionsAndProcedures} 'Funcions i procediments',
      {trTypes} 'Tipus',
        {trType} 'Tipus',
      {trVariables} strToDo, //'Variables',
      {trAuthors} 'Autors',
        {trAuthor} 'Autor',
      {trCreated} 'Creat',
      {trLastModified} 'Éltima modificaci¢',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Resum',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Totes les clases, interfaces i objectes',
    {trHeadlineConstants} 'Totes les constants',
    {trHeadlineFunctionsAndProcedures} 'Totes les funcions i procediments',
    {trHeadlineIdentifiers} 'Tot els indentificadors',
    {trHeadlineTypes} 'Tots els tipus',
    {trHeadlineUnits} 'Totes les unitats',
    {trHeadlineVariables} 'Totes les variables',
    {trSummaryCio} 'Llista de clases, interfaces i objectes',
  //column headings
    {trDeclaration} 'Declaraci¢',
    {trDescription} 'Descripci¢',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} strToDo, //'Nom',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Ningu',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} strToDo, //'Legend',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Atenci¢, no editar - aquest fitxer ha estat creat automaticament i ser… sobrescrit',
    {trWarning} 'Atenci¢',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

{ ---------------------------------------------------------------------------- }

{$I PasDoc_Languages_Chinese_gb2312.inc}

{ ---------------------------------------------------------------------------- }

const
  aDanish: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Danish',
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Klasser, interfaces og objekter',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Klasser',
      {trClass} 'Klasse',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objekter',
      {trObject} 'Objekt',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Herarki',
        {trFields} 'Felter',
        {trMethods} 'Metoder',
        {trProperties} 'Egenskaber',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konstanter',
      {trFunctionsAndProcedures} 'Funktioner og prosedurer',
      {trTypes} 'Typer',
        {trType} strToDo, //'Type',
      {trVariables} 'Variable',
      {trAuthors} 'Forfatre',
        {trAuthor} 'Forfatter',
      {trCreated} 'Udført',
      {trLastModified} 'Sidst Modificieret',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Sammendrag',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Alle Klasesr, Interfaces og Objekter',
    {trHeadlineConstants} 'Alle Konstanter',
    {trHeadlineFunctionsAndProcedures} 'Alle Functioner and Procedurer',
    {trHeadlineIdentifiers} 'Alle Identifiers',
    {trHeadlineTypes} 'Alle Typer',
    {trHeadlineUnits} 'Alle Units',
    {trHeadlineVariables} 'Alle Variable',
    {trSummaryCio} 'Oversigt over klasser, interfaces & objekter',
  //column headings
    {trDeclaration} strToDo, //'Declaration',
    {trDescription} 'Beskrivelse',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Navn',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Ingen',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Hjælp',
    {trLegend} 'Legende',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Advarsel: Editer ikke denne fil, den er autogeneret og vil sansylgvis blive overskret',
    {trWarning} strToDo, //'Warning',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aDutch: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Dutch',
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Classes, interfaces and objecten',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} strToDo, //'Classes',
      {trClass} strToDo, //'Class',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objecten',
      {trObject} strToDo, //'Object',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hierarchie',
        {trFields} 'Velden',
        {trMethods} strToDo, //'Methods',
        {trProperties} 'Eigenschappen',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} 'Programma',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Constanten',
      {trFunctionsAndProcedures} 'Functies en procedures',
      {trTypes} 'Typen',
        {trType} strToDo, //'Type',
      {trVariables} 'Variabelen',
      {trAuthors} 'Auteurs',
        {trAuthor} 'Auteur',
      {trCreated} 'Gemaakt',
      {trLastModified} 'Laatste wijziging',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Overzicht',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Alle classes, interfaces en objecten',
    {trHeadlineConstants} 'Alle constanten',
    {trHeadlineFunctionsAndProcedures} 'Alle functies en procedures',
    {trHeadlineIdentifiers} 'Alle identifiers',
    {trHeadlineTypes} 'Alle typen',
    {trHeadlineUnits} 'Alle units',
    {trHeadlineVariables} 'Alle variabelen',
    {trSummaryCio} 'Overzicht van classes, interfaces & objecten',
  //column headings
    {trDeclaration} 'Declaratie',
    {trDescription} 'Omschrijving',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Naam',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Geen',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} strToDo, //'Legend',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Waarschuwing, wijzig niets - dit bestand is automatisch gegenereerd en zal worden overschreven',
    {trWarning} 'Waarschuwing',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aFrench: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'French',
  //map
    {trUnits} 'Unités',
    {trClassHierarchy} 'Hiérarchie des classes',
    {trCio} 'Classes, interfaces, structures et objets',
    {trIdentifiers} 'Identificateurs',
    {trGvUses} 'Graphique de dépendance d''unités',
    {trGvClasses} 'Graphique de hiérarchie des classes',
  //tables and members
    {trClasses} strKeep, //'Classes',
      {trClass} 'Classe',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objets',
      {trObject} 'Objet',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hiérarchie',
        {trFields} 'Champs',
        {trMethods} 'Méthodes',
        {trProperties} 'Propriétés',
    {trLibrary} 'Bibliothèque', //?
    {trPackage} strToDo,  //'Package',
    {trProgram} 'Logiciel', //? 'Program',
    {trUnit} 'Unité',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Constantes',
      {trFunctionsAndProcedures} 'Fonctions et procédures',
      {trTypes} strKeep, //'Types',
        {trType} strKeep, //'Type',
      {trVariables} strKeep, //'Variables',
      {trAuthors} 'Auteurs',
        {trAuthor} 'Auteur',
      {trCreated} 'Crée',
      {trLastModified} 'Dernière modification',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Paramètres',
      {trReturns} 'Retourne',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} 'Visibilité',
      {trPrivate} 'Privé',
      {trStrictPrivate} 'Strictement Privé', //?
      {trProtected} 'Protégé',
      {trStrictProtected} 'Strictement Protégé', //?
      {trPublic} strKeep, //'Public',
      {trPublished} 'Publiés',
      {trAutomated} 'Automatisé',
      {trImplicit} strKeep, //'Implicit',
  //hints
    {trDeprecated} 'ce symbole est désapprouvé',
    {trPlatformSpecific} 'ce symbole est spécifique à une plateforme d''exécution',
    {trLibrarySpecific} 'ce symbole est spécifique à une certaine bibliothèque',
  //headings
    {trOverview} 'Aperçu',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Toutes les classes, interfaces, objets et enregistrements',
    {trHeadlineConstants} 'Toutes les constants',
    {trHeadlineFunctionsAndProcedures} 'Toutes les fonctions et procédures',
    {trHeadlineIdentifiers} 'Tous les identificateurs',
    {trHeadlineTypes} 'Tous les types',
    {trHeadlineUnits} 'Toutes les unités',
    {trHeadlineVariables} 'Toutes les variables',
    {trSummaryCio} 'Classes, interfaces, objets et enregistrements',
  //column headings
    {trDeclaration} 'Déclaration',
    {trDescription} strKeep, //'Description',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nom',
    {trValues} 'Valeurs', //?
  //empty
    {trNone} 'Aucun(e)(s)', //'Rien'?
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Aide',
    {trLegend} 'Légende',
    {trMarker} 'Marquage',
    {trWarningOverwrite} 'Attention, ne pas édtier - ce fichier est créé automatiquement et va être écrasé',
    {trWarning} 'Attention',
    {trGeneratedBy} 'Produit par',
    {trOnDateTime} 'le',
    {trSearch} 'Cherche', //? 'Recherche'
    {trSeeAlso} 'Voir aussi', //?
    ''  //dummy
  );

  aGerman: RTransTable = (
    {trNoTrans} '<häh?>', //no ID assigned, so far
    {trLanguage} 'German',
  //map
    {trUnits} strKeep, //'Units',
    {trClassHierarchy} 'Klassenhierarchie',
    {trCio} 'Klassen, Interfaces und Objects',
    {trIdentifiers} 'Bezeichner',
    {trGvUses} 'Graph der Unit-Abhängigkeiten',
    {trGvClasses} 'Graph der Klassenhierarchie',
  //tables and members
    {trClasses} 'Klassen',
      {trClass} 'Klasse',
      {trDispInterface} strKeep, //'DispInterface',
      {trInterface} strKeep, //'Interface', 'Schnittstelle'?
    {trObjects} strKeep, //'Objects',
      {trObject} strKeep, //'Object',
      {trRecord} strKeep, //'Record',
        {trHierarchy} 'Hierarchie',
        {trFields} 'Felder',
        {trMethods} 'Methoden',
        {trProperties} 'Eigenschaften',
    {trLibrary} 'Bibliothek',
    {trPackage} strKeep, //'Package',
    {trProgram} 'Programm',
    {trUnit} strKeep, //'Unit',
      {trUses} strKeep, //'Uses',
      {trConstants} 'Konstanten',
      {trFunctionsAndProcedures} 'Funktionen und Prozeduren',
      {trTypes} 'Datentypen',
        {trType} strKeep, //'Type', 'Typ'?
      {trVariables} 'Variablen',
      {trAuthors} 'Autoren',
        {trAuthor} 'Autor',
      {trCreated} 'Erstellt',
      {trLastModified} 'Letzte Änderung',
    {trSubroutine} 'Unterprogramm',
      {trParameters} 'Parameter',
      {trReturns} 'Result',
      {trExceptionsRaised} 'Wirft Ausnahmen', //'Exceptions raised',
    {trExceptions} 'Ausnahmen',
      {trException} strKeep, //'Exception',
    {trEnum} strKeep, //'Enumeration',
  //visibilities
    {trVisibility} 'Sichtbarkeit',
      {trPrivate} strKeep, //'Private',
      {trStrictPrivate} strKeep, //'Strict Private',
      {trProtected} strKeep, //'Protected',
      {trStrictProtected} strKeep, //'Strict Protected',
      {trPublic} strKeep, //'Public',
      {trPublished} strKeep, //'Published',
      {trAutomated} strKeep, //'Automated',
      {trImplicit} strKeep, //'Implicit',
  //hints
    {trDeprecated} 'Dieses Symbol sollte nicht (mehr) verwendet werden.',
    {trPlatformSpecific} 'Dieses Symbol ist plattformspezifisch.',
    {trLibrarySpecific} 'Dieses Symbol ist spezifisch für eine bestimmte Bibliothek.',
  //headings
    {trOverview} 'Übersicht',
    {trIntroduction} 'Einführung',
    {trConclusion} 'Fazit',
    {trHeadlineCio} 'Alle Klassen, Schnittstellen, Objekte und Records',
    {trHeadlineConstants} 'Alle Konstanten',
    {trHeadlineFunctionsAndProcedures} 'Alle Funktionen und Prozeduren',
    {trHeadlineIdentifiers} 'Alle Bezeichner',
    {trHeadlineTypes} 'Alle Typen',
    {trHeadlineUnits} 'Alle Units',
    {trHeadlineVariables} 'Alle Variablen',
    {trSummaryCio} 'Zusammenfassung aller Klassen, Schnittstellen, Objekte und Records',
  //column headings
    {trDeclaration} 'Deklaration',
    {trDescription} 'Beschreibung',
    {trDescriptions} 'Ausführliche Beschreibungen',
    {trName} strKeep, //'Name',
    {trValues} 'Werte',
  //empty
    {trNone} 'Keine',
    {trNoCIOs} 'Die Units enthalten keine Klassen, Interfaces, Objects oder Records.',
    {trNoCIOsForHierarchy} 'Die Units enthalten keine Klassen, Interfaces oder Objects.',
    {trNoTypes} 'Die Units enthalten keine Typen.',
    {trNoVariables} 'Die Units enthalten keine Variablen.',
    {trNoConstants} 'Die Units enthalten keine Konstanten.',
    {trNoFunctions} 'Die Units enthalten keine Funktionen oder Prozeduren.',
    {trNoIdentifiers} 'Die Units enthalten keine Bezeichner.',
  //misc
    {trHelp} 'Hilfe',
    {trLegend} 'Legende',
    {trMarker} 'Markierung',
    {trWarningOverwrite} 'Achtung: Nicht ändern - diese Datei wurde automatisch erstellt und wird möglicherweise überschrieben',
    {trWarning} 'Warnung',
    {trGeneratedBy} 'Erstellt mit',
    {trOnDateTime} 'am',
    {trSearch} 'Suchen',
    {trSeeAlso} 'Siehe auch',
    ''  //dummy
  );

  aIndonesian: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Indonesian',
  //map
    {trUnits} 'Unit',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Kelas, Interface, dan Objek',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Kelas',
      {trClass} 'Kelas',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objek',
      {trObject} 'Objek',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hirarki',
        {trFields} strToDo, //'Fields',
        {trMethods} strToDo, //'Methods',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strKeep, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konstanta',
      {trFunctionsAndProcedures} 'Fungsi dan Prosedur',
      {trTypes} 'Tipe Bentukan',
        {trType} 'Tipe Bentukan',
      {trVariables} 'Variabel',
      {trAuthors} 'Pembuat',
        {trAuthor} 'Pembuat',
      {trCreated} 'Dibuat',
      {trLastModified} 'Terakhir Dimodifikasi',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Sekilas',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Semua Kelas, Interface, dan Objek',
    {trHeadlineConstants} 'Semua Konstanta',
    {trHeadlineFunctionsAndProcedures} 'Semua Fungsi dan Prosedur',
    {trHeadlineIdentifiers} 'Semua Identifier',
    {trHeadlineTypes} 'Semua Tipe Bentukan',
    {trHeadlineUnits} 'Semua Unit',
    {trHeadlineVariables} 'Semua Variabel',
    {trSummaryCio} 'Ringkasan Kelas, Interface, dan Objek',
  //column headings
    {trDeclaration} 'Deklarasi',
    {trDescription} 'Definisi',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nama',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Tidak Ada',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Bantuan',
    {trLegend} 'Legenda',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Perhatian: Jangan dimodifikasi - '
      + 'file ini dihasilkan secara otomatis dan mungkin saja ditimpa ulang',
    {trWarning} 'Perhatian', //?
    {trGeneratedBy} 'Dihasilkan oleh',
    {trOnDateTime} 'pada',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aItalian: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Italian',
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Classi, Interfacce ed Oggetti',
    {trIdentifiers} 'Identificatori',
    {trGvUses} 'Grafico dipendenze Unit',
    {trGvClasses} 'Grafico gerarchia Classi',
  //tables and members
    {trClasses} 'Classi',
      {trClass} 'Classe',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} 'Interfacce',
    {trObjects} 'Oggetti',
      {trObject} 'Oggetto',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Gerarchia',
        {trFields} 'Campi',
        {trMethods} 'Metodi',
        {trProperties} 'Proprietà',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Costanti',
      {trFunctionsAndProcedures} 'Funzioni e Procedure',
      {trTypes} 'Tipi',
        {trType} 'Tipo',
      {trVariables} 'Variabili',
      {trAuthors} 'Autori',
        {trAuthor} 'Autore',
      {trCreated} 'Creato',
      {trLastModified} 'Ultima Variazione',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Parametri',
      {trReturns} 'Ritorni',
      {trExceptionsRaised} 'Eccezioni sollevate',
    {trExceptions} 'Eccezione',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Sommario',
    {trIntroduction} 'Introduczione',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Tutte le Classi, Interfacce ed Oggetti',
    {trHeadlineConstants} 'Tutte le Costanti',
    {trHeadlineFunctionsAndProcedures} 'Tutte le Funzioni e Procedure',
    {trHeadlineIdentifiers} 'Tutti gli Identificatori',
    {trHeadlineTypes} 'Tutti i Tipi',
    {trHeadlineUnits} 'Tutte le Units',
    {trHeadlineVariables} 'Tutte le Variabili',
    {trSummaryCio} 'Sommario di Classi, Interfacce ed Oggetti',
  //column headings
    {trDeclaration} 'Dichiarazione',
    {trDescription} 'Descrizione',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nome',
    {trValues} 'Valori',
  //empty
    {trNone} 'Nessuno',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} 'Legenda',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Attenzione: Non modificare - questo file è stato generato automaticamente e verrà probabilmente sovrascritto',
    {trWarning} 'Attenzione',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} 'Cerca',
    {trSeeAlso} 'Vedere Anche',
    ''  //dummy
  );

  aJavanese: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Javanese',
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Kelas, Interface, lan Objek',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Kelas',
      {trClass} 'Kelas',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objek',
      {trObject} 'Objek',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hirarki',
        {trFields} strToDo, //'Fields',
        {trMethods} strToDo, //'Methods',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} strToDo, //'Unit',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konstanta',
      {trFunctionsAndProcedures} 'Fungsi lan Prosedur',
      {trTypes} 'Macem Gawean',
        {trType} 'Macem Gawean',
      {trVariables} 'Variabel',
      {trAuthors} 'Sing Nggawe',
        {trAuthor} 'Sing Nggawe',
      {trCreated} 'Digawe',
      {trLastModified} 'Terakhir Diowahi',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} strToDo, //'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Pambuka',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Kabeh Kelas, Interface, lan Objek',
    {trHeadlineConstants} 'Kabeh Konstanta',
    {trHeadlineFunctionsAndProcedures} 'Kabeh Fungsi lan Prosedur',
    {trHeadlineIdentifiers} 'Kabeh Identifier',
    {trHeadlineTypes} 'Kabeh Macem Gawean',
    {trHeadlineUnits} 'Kabeh Unit',
    {trHeadlineVariables} 'Kabeh Variabel',
    {trSummaryCio} 'Ringkesan Kelas, Interface, lan Objek',
  //column headings
    {trDeclaration} 'Deklarasi',
    {trDescription} 'Katrangan',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Jeneng',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Mboten Wonten',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} 'Tulung',
    {trLegend} 'Katrangan',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Ati-ati: Ojo diowahi - '
      + 'file iki digawe otomatis dadi iso ilang owahanmu',
    {trWarning} 'Ati-ati', //?
    {trGeneratedBy} 'Dihasilne karo',
    {trOnDateTime} 'ing',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aPolish1250: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Polish',
  //map
    {trUnits} 'Modu³y',
    {trClassHierarchy} 'Hierarchia klas',
    {trCio} 'Klasy, interfejsy, obiekty i rekordy',
    {trIdentifiers} 'Identyfikatory',
    {trGvUses} 'Graf zale¿noœci modu³ów',
    {trGvClasses} 'Graf dziedziczenia klas',
  //tables and members
    {trClasses} 'Klasy',
      {trClass} 'Klasa',
      {trDispInterface} 'DispInterface', //'DispInterface',
      {trInterface} 'Interfejs',
    {trObjects} 'Obiekty',
      {trObject} 'Obiekt',
      {trRecord} 'Rekord', //'Record',
        {trHierarchy} 'Hierarchia',
        {trFields} 'Pola',
        {trMethods} 'Metody',
        {trProperties} 'W³aœciwoœci',
    {trLibrary} 'Biblioteka',  //'Library',
    {trPackage} 'Pakiet',  //'Package',
    {trProgram} 'Program',  //'Program',
    {trUnit} 'Modu³',
      {trUses} 'U¿ywa', //'Uses',
      {trConstants} 'Sta³e',
      {trFunctionsAndProcedures} 'Podprogramy',
      {trTypes} 'Typy',
        {trType} 'Typ',
      {trVariables} 'Zmienne',
      {trAuthors} 'Autorzy',
        {trAuthor} 'Autor',
      {trCreated} 'Utworzony',
      {trLastModified} 'Ostatnia modyfikacja',
    {trSubroutine} 'Podprograma', //?
      {trParameters} 'Parametry',
      {trReturns} 'Wynik',
      {trExceptionsRaised} 'Generowane wyj¹tki',
    {trExceptions} 'Wyj¹tki',
      {trException} 'Wyj¹tek', //'Exception',
    {trEnum} 'Wyliczenie',
  //visibilities
    {trVisibility} 'Widocznoœæ',
      {trPrivate} 'Prywatne',
      {trStrictPrivate} 'Œciœle prywatne', //'Strict Private',
      {trProtected} 'Chronione',
      {trStrictProtected} 'Œciœle chronione', //'Strict Protected',
      {trPublic} 'Publiczne',
      {trPublished} 'Publikowane',
      {trAutomated} 'Automated', //'Automated',
      {trImplicit} 'Domyœlne',
  //hints
    {trDeprecated} 'odradza siê u¿ywania tego identyfikatora',
    {trPlatformSpecific} 'ten identyfikator jest zale¿ny od platformy',
    {trLibrarySpecific} 'ten identyfikator jest zale¿ny od biblioteki',
  //headings
    {trOverview} 'Przegl¹d',
    {trIntroduction} 'Wstêp',
    {trConclusion} 'Podsumowanie',
    {trHeadlineCio} 'Wszystkie klasy, interfejsy, obiekty i rekordy',
    {trHeadlineConstants} 'Wszystkie sta³e',
    {trHeadlineFunctionsAndProcedures} 'Wszystkie podprogramy',
    {trHeadlineIdentifiers} 'Wszystkie identyfikatory',
    {trHeadlineTypes} 'Wszystkie typy',
    {trHeadlineUnits} 'Wszystkie modu³y',
    {trHeadlineVariables} 'Wszystkie zmienne',
    {trSummaryCio} 'Podsumowanie klas, interfejsów, obiektów i rekordów',
  //column headings
    {trDeclaration} 'Deklaracja',
    {trDescription} 'Opis',
    {trDescriptions} 'Szczegó³y', //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nazwa',
    {trValues} 'Wartoœci',
  //empty
    {trNone} 'Brak',
    {trNoCIOs} 'Modu³ nie zawiera ¿adnych klas, interfejsów, obiektów ani rekordów.',
    {trNoCIOsForHierarchy} 'Modu³ nie zawiera ¿adnych klas, interfejsów ani obiektów.',
    {trNoTypes} 'Modu³ nie zawiera ¿adnych typów.',
    {trNoVariables} 'Modu³ nie zawiera ¿adnych zmiennych.',
    {trNoConstants} 'Modu³ nie zawiera ¿adnych sta³ych.',
    {trNoFunctions} 'Modu³ nie zawiera ¿adnych funkcji ani podprogramów.',
    {trNoIdentifiers} 'Modu³ nie zawiera ¿adnych identyfikatorów.',
  //misc
    {trHelp} 'Pomoc',
    {trLegend} 'Legenda',
    {trMarker} 'Kolor',
    {trWarningOverwrite} 'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany',
    {trWarning} 'Uwaga',
    {trGeneratedBy} 'Wygenerowane przez',
    {trOnDateTime} ' - ',
    {trSearch} 'Szukaj',
    {trSeeAlso} 'Zobacz tak¿e',
    ''  //dummy
  );

  aPolish_ISO_8859_2: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Polish',
  //map
    {trUnits} 'Modu³y',
    {trClassHierarchy} 'Hierarchia klas',
    {trCio} 'Klasy, interfejsy, obiekty i rekordy',
    {trIdentifiers} 'Identyfikatory',
    {trGvUses} 'Graf zale¿no¶ci modu³ów',
    {trGvClasses} 'Graf dziedziczenia klas',
  //tables and members
    {trClasses} 'Klasy',
      {trClass} 'Klasa',
      {trDispInterface} 'DispInterface', //'DispInterface',
      {trInterface} 'Interfejs',
    {trObjects} 'Obiekty',
      {trObject} 'Obiekt',
      {trRecord} 'Rokord', //'Record',
        {trHierarchy} 'Hierarchia',
        {trFields} 'Pola',
        {trMethods} 'Metody',
        {trProperties} 'W³a¶ciwo¶ci',
    {trLibrary} 'Biblioteka',  //'Library',
    {trPackage} 'Pakiet',  //'Package',
    {trProgram} 'Program',  //'Program',
    {trUnit} 'Modu³',
      {trUses} 'U¿ywa', //'Uses',
      {trConstants} 'Sta³e',
      {trFunctionsAndProcedures} 'Podprogramy',
      {trTypes} 'Typy',
        {trType} 'Typ',
      {trVariables} 'Zmienne',
      {trAuthors} 'Autorzy',
        {trAuthor} 'Autor',
      {trCreated} 'Utworzony',
      {trLastModified} 'Ostatnia modyfikacja',
    {trSubroutine} 'Podprogram',
      {trParameters} 'Parametry',
      {trReturns} 'Wynik',
      {trExceptionsRaised} 'Generowane wyj±tki',
    {trExceptions} 'Wyj±tki',
      {trException} 'Wyj±tek',
    {trEnum} 'Wyliczenie',
  //visibilities
    {trVisibility} 'Widoczno¶æ',
      {trPrivate} 'Prywatne',
      {trStrictPrivate} '¦ci¶le prywatne', //'Strict Private',
      {trProtected} 'Chronione',
      {trStrictProtected} '¦ci¶le chronione', //'Strict Protected',
      {trPublic} 'Publiczne',
      {trPublished} 'Publikowane',
      {trAutomated} 'Automated', //'Automated',
      {trImplicit} 'Domy¶lne',
  //hints
    {trDeprecated} 'odradza siê u¿ywania tego identyfikatora',
    {trPlatformSpecific} 'ten identyfikator jest zale¿ny od platformy',
    {trLibrarySpecific} 'ten identyfikator jest zale¿ny od biblioteki',
  //headings
    {trOverview} 'Przegl±d',
    {trIntroduction} 'Wstêp',
    {trConclusion} 'Podsumowanie',
    {trHeadlineCio} 'Wszystkie klasy, interfejsy, obiekty i rekordy',
    {trHeadlineConstants} 'Wszystkie sta³e',
    {trHeadlineFunctionsAndProcedures} 'Wszystkie podprogramy',
    {trHeadlineIdentifiers} 'Wszystkie identyfikatory',
    {trHeadlineTypes} 'Wszystkie typy',
    {trHeadlineUnits} 'Wszystkie modu³y',
    {trHeadlineVariables} 'Wszystkie zmienne',
    {trSummaryCio} 'Podsumowanie klas, interfejsów, obiektów i rekordów',
  //column headings
    {trDeclaration} 'Deklaracja',
    {trDescription} 'Opis',
    {trDescriptions} 'Szczegó³y', //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nazwa',
    {trValues} 'Warto¶ci',
  //empty
    {trNone} 'Brak',
    {trNoCIOs} 'Modu³ nie zawiera ¿adnych klas, interfejsów, obiektów ani rekordów.',
    {trNoCIOsForHierarchy} 'Modu³ nie zawiera ¿adnych klas, interfejsów ani obiektów.',
    {trNoTypes} 'Modu³ nie zawiera ¿adnych typów.',
    {trNoVariables} 'Modu³ nie zawiera ¿adnych zmiennych.',
    {trNoConstants} 'Modu³ nie zawiera ¿adnych sta³ych.',
    {trNoFunctions} 'Modu³ nie zawiera ¿adnych funkcji ani podprogramów.',
    {trNoIdentifiers} 'Modu³ nie zawiera ¿adnych identyfikatorów.',
  //misc
    {trHelp} 'Pomoc',
    {trLegend} 'Legenda',
    {trMarker} 'Kolor',
    {trWarningOverwrite} 'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany',
    {trWarning} 'Uwaga',
    {trGeneratedBy} 'Wygenerowane przez',
    {trOnDateTime} ' - ',
    {trSearch} 'Szukaj',
    {trSeeAlso} 'Zobacz tak¿e',
    ''  //dummy
  );

  aRussian_1251: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Russian',
  //map
    {trUnits} 'Ìîäóëè',
    {trClassHierarchy} 'Èåğàğõèÿ êëàññîâ',
    {trCio} 'Êëàññû, èíòåğôåéñû è îáúåêòû',
    {trIdentifiers} 'Èäåíòèôèêàòîğû',
    {trGvUses} 'Ãğàôèê çàâèñèìîñòè ìîäóëåé',
    {trGvClasses} 'Ãğàôèê èåğàğõèè êëàññîâ',
  //tables and members
    {trClasses} 'Êëàññû',
      {trClass} 'Êëàññ',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} 'Èíòåğôåéñ',
    {trObjects} 'Îáúåêòû',
      {trObject} 'Îáúåêò',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Èåğàğõèÿ',
        {trFields} 'Ïîëÿ',
        {trMethods} 'Ìåòîäû',
        {trProperties} 'Ñâîéñòâà',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} 'Ïğîãğàììà',
    {trUnit} 'Ìîäóëü',
      {trUses} 'Èñïîëüçóåìûå ìîäóëè',
      {trConstants} 'Êîíñòàíòû',
      {trFunctionsAndProcedures} 'Ïğîöåäóğû è ôóíêöèè',
      {trTypes} 'Òèïû',
        {trType} 'Òèï',
      {trVariables} 'Ïåğåìåííûå',
      {trAuthors} 'Àâòîğû',
        {trAuthor} 'Àâòîğ',
      {trCreated} 'Ñîçäàíî',
      {trLastModified} 'Ïîñëåäíåå èçìåíåíèå',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Ïàğàìåòğû',
      {trReturns} 'Âîçâğàùàåìûå çíà÷åíèÿ',
      {trExceptionsRaised} 'Âûçûâàåò èñêëş÷åíèÿ',
    {trExceptions} 'Èñêëş÷åíèÿ',
      {trException} strToDo, //'Exception',
    {trEnum} 'Ïåğå÷èñëåíèå',
  //visibilities
    {trVisibility} 'Çîíà âèäèìîñòè',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} 'ıòîò ñèìâîë áîëüøå íå èñïîëüçóåòñÿ',
    {trPlatformSpecific} 'ıòîò ñèìâîë çàâèñèò îò ïëàòôîğìû',
    {trLibrarySpecific} 'ıòîò ñèìâîë çàâèñèò îò áèáëèîòåêè',
  //headings
    {trOverview} 'Îáçîğ',
    {trIntroduction} 'Ââåäåíèå',
    {trConclusion} 'Çàêëş÷åíèå',
    {trHeadlineCio} 'Âñå êëàññû, èíòåğôåéñû è îáúåêòû',
    {trHeadlineConstants} 'Âñå êîíñòàíòû',
    {trHeadlineFunctionsAndProcedures} 'Âñå ïğîöåäóğû è ôóíêöèè',
    {trHeadlineIdentifiers} 'Âñå èäåíòèôèêàòîğû',
    {trHeadlineTypes} 'Âñå òèïû',
    {trHeadlineUnits} 'Âñå ìîäóëè',
    {trHeadlineVariables} 'Âñå ïåğåìåííûå',
    {trSummaryCio} 'Ñïèñîê êëàññîâ, èíòåğôåéñîâ è îáúåêòîâ',
  //column headings
    {trDeclaration} 'Îáúÿâëåíèÿ',
    {trDescription} 'Îïèñàíèå',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Èìÿ',
    {trValues} 'Çíà÷åíèå',
  //empty
    {trNone} 'Íåò',
    {trNoCIOs} 'Ìîäóëè íå ñîäåğæàò êëàññîâ, èíòåğôåéñîâ, îáúåêòîâ è çàïèñåé.',
    {trNoCIOsForHierarchy} 'Ìîäóëè íå ñîäåğæàò êëàññîâ, èíòåğôåéñîâ è îáúåêòîâ.',
    {trNoTypes} 'Ìîäóëè íå ñîäåğæàò òèïîâ.',
    {trNoVariables} 'Ìîäóëè íå ñîäåğæàò ïåğåìåííûõ.',
    {trNoConstants} 'Ìîäóëè íå ñîäåğæàò êîíñòàíò.',
    {trNoFunctions} 'Ìîäóëè íå ñîäåğæàò ôóíêöèè è ïğîöåäóğû.',
    {trNoIdentifiers} 'Ìîäóëè íå ñîäåğæàò íè îäíîãî èäåíòèôèêàòîğà.',
  //misc
    {trHelp} strKeep, //'Help', // Untranslated to avoid Russian file name for css
      { TODO : how does "Help" interfere with file names? }
    {trLegend} 'Îáîçíà÷åíèÿ',
    {trMarker} 'Ìàğêåğ',
    {trWarningOverwrite} 'Ïğåäóïğåæäåíèå: íå ğåäàêòèğîâàòü - ıòîò ôàéë ñîçäàí àâòîìàòè÷åñêè è ìîæåò áûòü èçìåí¸í áåç ïğåäóïğåæäåíèÿ',
    {trWarning} 'Ïğåäóïğåæäåíèå',
    {trGeneratedBy} 'Ñãåíåğèğîâàë', // + ' '?
    {trOnDateTime} 'äàòà/âğåìÿ', //really???
    {trSearch} 'Íàéòè',
    {trSeeAlso} 'Ìàòåğèàëû ïî òåìå',
    ''  //dummy
  );

{$I PasDoc_Languages_Russian_utf8.inc}

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_866;
begin
  FTranslation[trAuthor] := '€¢â®à';
  FTranslation[trAuthors] := '€¢â®àë';
  FTranslation[trCio] := 'Š« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trClass] := 'Š« áá';
  FTranslation[trClasses] := 'Š« ááë';
  FTranslation[trClassHierarchy] := 'ˆ¥à àå¨ï ª« áá®¢';
  FTranslation[trConstants] := 'Š®­áâ ­âë';
  FTranslation[trCreated] := '‘®§¤ ­®';
  FTranslation[trDeclaration] := '¡êï¢«¥­¨ï';
  FTranslation[trParameters] := ' à ¬¥âàë';
  FTranslation[trReturns] := '‚®§¢à é ¥¬ë¥ §­ ç¥­¨ï';
  FTranslation[trExceptions] := 'ˆáª«îç¥­¨ï';
  FTranslation[trExceptionsRaised] := '‚ë§ë¢ ¥â ¨áª«îç¥­¨ï';
  FTranslation[trEnum] := '¥à¥ç¨á«¥­¨¥';
  FTranslation[trDescription] := '¯¨á ­¨¥';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := '®«ï';
  FTranslation[trFunctionsAndProcedures] := 'à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'ˆ¥à àå¨ï';
  FTranslation[trIdentifiers] := 'ˆ¤¥­â¨ä¨ª â®àë';
  FTranslation[trInterface] := 'ˆ­â¥àä¥©á';
  FTranslation[trLegend] := '¡®§­ ç¥­¨ï';
  FTranslation[trMarker] := 'Œ àª¥à';
  FTranslation[trVisibility] := '‡®­  ¢¨¤¨¬®áâ¨';
  FTranslation[trLastModified] := '®á«¥¤­¥¥ ¨§¬¥­¥­¨¥';
  FTranslation[trMethods] := 'Œ¥â®¤ë';
  FTranslation[trName] := 'ˆ¬ï';
  FTranslation[trNone] := '¥â';
  FTranslation[trObject] := '¡ê¥ªâ';
  FTranslation[trObjects] := '¡ê¥ªâë';
  FTranslation[trOverview] := '¡§®à';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := '‘¢®©áâ¢ ';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := '’¨¯';
  FTranslation[trTypes] := '’¨¯ë';
  FTranslation[trUnit] := 'Œ®¤ã«ì';
  FTranslation[trUnits] := 'Œ®¤ã«¨';
  FTranslation[trVariables] := '¥à¥¬¥­­ë¥';
  FTranslation[trGvUses] := 'ƒà ä¨ª § ¢¨á¨¬®áâ¨ ¬®¤ã«¥©';
  FTranslation[trGvClasses] := 'ƒà ä¨ª ¨¥à àå¨¨ ª« áá®¢';
  FTranslation[trWarningOverwrite] :=
    'à¥¤ã¯à¥¦¤¥­¨¥: ­¥ à¥¤ ªâ¨à®¢ âì - íâ®â ä ©« á®§¤ ­  ¢â®¬ â¨ç¥áª¨ ¨ ¬®¦¥â ¡ëâì ¨§¬¥­ñ­ ¡¥§ ¯à¥¤ã¯à¥¦¤¥­¨ï';
  FTranslation[trWarning] := 'à¥¤ã¯à¥¦¤¥­¨¥';
  FTranslation[trHeadlineCio] := '‚á¥ ª« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trHeadlineConstants] := '‚á¥ ª®­áâ ­âë';
  FTranslation[trHeadlineFunctionsAndProcedures] := '‚á¥ ¯à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHeadlineIdentifiers] := '‚á¥ ¨¤¥­â¨ä¨ª â®àë';
  FTranslation[trHeadlineTypes] := '‚á¥ â¨¯ë';
  FTranslation[trHeadlineUnits] := '‚á¥ ¬®¤ã«¨';
  FTranslation[trHeadlineVariables] := '‚á¥ ¯¥à¥¬¥­­ë¥';
  FTranslation[trSummaryCio] := '‘¯¨á®ª ª« áá®¢, ¨­â¥àä¥©á®¢ ¨ ®¡ê¥ªâ®¢';
  FTranslation[trGeneratedBy] := '‘£¥­¥à¨à®¢ « ';
  FTranslation[trOnDateTime] := '¤ â /¢à¥¬ï';
  FTranslation[trDeprecated] := 'íâ®â á¨¬¢®« ¡®«ìè¥ ­¥ ¨á¯®«ì§ã¥âáï';
  FTranslation[trPlatformSpecific] := 'íâ®â á¨¬¢®« § ¢¨á¨â ®â ¯« âä®à¬ë';
  FTranslation[trLibrarySpecific] := 'íâ®â á¨¬¢®« § ¢¨á¨â ®â ¡¨¡«¨®â¥ª¨';
  FTranslation[trIntroduction] := '‚¢¥¤¥­¨¥';
  FTranslation[trConclusion] := '‡ ª«îç¥­¨¥';
  FTranslation[trSearch] := ' ©â¨';
  FTranslation[trSeeAlso] := 'Œ â¥à¨ «ë ¯® â¥¬¥';
  FTranslation[trValues] := '‡­ ç¥­¨¥';
  FTranslation[trNoCIOs] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª« áá®¢, ¨­â¥àä¥©á®¢, ®¡ê¥ªâ®¢ ¨ § ¯¨á¥©.';
  FTranslation[trNoCIOsForHierarchy] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª« áá®¢, ¨­â¥àä¥©á®¢ ¨ ®¡ê¥ªâ®¢.';
  FTranslation[trNoTypes] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â â¨¯®¢.';
  FTranslation[trNoVariables] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ¯¥à¥¬¥­­ëå.';
  FTranslation[trNoConstants] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ª®­áâ ­â.';
  FTranslation[trNoFunctions] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â äã­ªæ¨¨ ¨ ¯à®æ¥¤ãàë.';
  FTranslation[trNoIdentifiers] := 'Œ®¤ã«¨ ­¥ á®¤¥à¦ â ­¨ ®¤­®£® ¨¤¥­â¨ä¨ª â®à .';
  FTranslation[trProgram] := 'à®£à ¬¬ ';
  FTranslation[trUses] := 'L¸ÿşû¹÷ºõüvõ üşôºûø';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_koi8;
begin
  FTranslation[trAuthor] := 'á×ÔÏÒ';
  FTranslation[trAuthors] := 'á×ÔÏÒÙ';
  FTranslation[trCio] := 'ëÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trClass] := 'ëÌÁÓÓ';
  FTranslation[trClasses] := 'ëÌÁÓÓÙ';
  FTranslation[trClassHierarchy] := 'éÅÒÁÒÈÉÑ ËÌÁÓÓÏ×';
  FTranslation[trConstants] := 'ëÏÎÓÔÁÎÔÙ';
  FTranslation[trCreated] := 'óÏÚÄÁÎÏ';
  FTranslation[trDeclaration] := 'ïÂßÑ×ÌÅÎÉÑ';
  FTranslation[trParameters] := 'ğÁÒÁÍÅÔÒÙ';
  FTranslation[trReturns] := '÷ÏÚ×ÒÁİÁÅÍÙÅ ÚÎÁŞÅÎÉÑ';
  FTranslation[trExceptions] := 'éÓËÌÀŞÅÎÉÑ';
  FTranslation[trExceptionsRaised] := '÷ÙÚÙ×ÁÅÔ ÉÓËÌÀŞÅÎÉÑ';
  FTranslation[trEnum] := 'ğÅÒÅŞÉÓÌÅÎÉÅ';
  FTranslation[trDescription] := 'ïĞÉÓÁÎÉÅ';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'ğÏÌÑ';
  FTranslation[trFunctionsAndProcedures] := 'ğÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'éÅÒÁÒÈÉÑ';
  FTranslation[trIdentifiers] := 'éÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trInterface] := 'éÎÔÅÒÆÅÊÓ';
  FTranslation[trLegend] := 'ïÂÏÚÎÁŞÅÎÉÑ';
  FTranslation[trMarker] := 'íÁÒËÅÒ';
  FTranslation[trVisibility] := 'úÏÎÁ ×ÉÄÉÍÏÓÔÉ';
  FTranslation[trLastModified] := 'ğÏÓÌÅÄÎÅÅ ÉÚÍÅÎÅÎÉÅ';
  FTranslation[trMethods] := 'íÅÔÏÄÙ';
  FTranslation[trName] := 'éÍÑ';
  FTranslation[trNone] := 'îÅÔ';
  FTranslation[trObject] := 'ïÂßÅËÔ';
  FTranslation[trObjects] := 'ïÂßÅËÔÙ';
  FTranslation[trOverview] := 'ïÂÚÏÒ';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'ó×ÏÊÓÔ×Á';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trImplicit] := 'Implicit';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'ôÉĞ';
  FTranslation[trTypes] := 'ôÉĞÙ';
  FTranslation[trUnit] := 'íÏÄÕÌØ';
  FTranslation[trUnits] := 'íÏÄÕÌÉ';
  FTranslation[trVariables] := 'ğÅÒÅÍÅÎÎÙÅ';
  FTranslation[trGvUses] := 'çÒÁÆÉË ÚÁ×ÉÓÉÍÏÓÔÉ ÍÏÄÕÌÅÊ';
  FTranslation[trGvClasses] := 'çÒÁÆÉË ÉÅÒÁÒÈÉÉ ËÌÁÓÓÏ×';
  FTranslation[trWarningOverwrite] :=
    'ğÒÅÄÕĞÒÅÖÄÅÎÉÅ: ÎÅ ÒÅÄÁËÔÉÒÏ×ÁÔØ - ÜÔÏÔ ÆÁÊÌ ÓÏÚÄÁÎ Á×ÔÏÍÁÔÉŞÅÓËÉ É ÍÏÖÅÔ ÂÙÔØ ÉÚÍÅÎ£Î ÂÅÚ ĞÒÅÄÕĞÒÅÖÄÅÎÉÑ';
  FTranslation[trWarning] := 'ğÒÅÄÕĞÒÅÖÄÅÎÉÅ';
  FTranslation[trHeadlineCio] := '÷ÓÅ ËÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trHeadlineConstants] := '÷ÓÅ ËÏÎÓÔÁÎÔÙ';
  FTranslation[trHeadlineFunctionsAndProcedures] := '÷ÓÅ ĞÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHeadlineIdentifiers] := '÷ÓÅ ÉÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trHeadlineTypes] := '÷ÓÅ ÔÉĞÙ';
  FTranslation[trHeadlineUnits] := '÷ÓÅ ÍÏÄÕÌÉ';
  FTranslation[trHeadlineVariables] := '÷ÓÅ ĞÅÒÅÍÅÎÎÙÅ';
  FTranslation[trSummaryCio] := 'óĞÉÓÏË ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ× É ÏÂßÅËÔÏ×';
  FTranslation[trGeneratedBy] := 'óÇÅÎÅÒÉÒÏ×ÁÌ ';
  FTranslation[trOnDateTime] := 'ÄÁÔÁ/×ÒÅÍÑ';
  FTranslation[trDeprecated] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÂÏÌØÛÅ ÎÅ ÉÓĞÏÌØÚÕÅÔÓÑ';
  FTranslation[trPlatformSpecific] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÚÁ×ÉÓÉÔ ÏÔ ĞÌÁÔÆÏÒÍÙ';
  FTranslation[trLibrarySpecific] := 'ÜÔÏÔ ÓÉÍ×ÏÌ ÚÁ×ÉÓÉÔ ÏÔ ÂÉÂÌÉÏÔÅËÉ';
  FTranslation[trIntroduction] := '÷×ÅÄÅÎÉÅ';
  FTranslation[trConclusion] := 'úÁËÌÀŞÅÎÉÅ';
  FTranslation[trSearch] := 'îÁÊÔÉ';
  FTranslation[trSeeAlso] := 'íÁÔÅÒÉÁÌÙ ĞÏ ÔÅÍÅ';
  FTranslation[trValues] := 'úÎÁŞÅÎÉÅ';
  FTranslation[trNoCIOs] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ×, ÏÂßÅËÔÏ× É ÚÁĞÉÓÅÊ.';
  FTranslation[trNoCIOsForHierarchy] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ× É ÏÂßÅËÔÏ×.';
  FTranslation[trNoTypes] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÔÉĞÏ×.';
  FTranslation[trNoVariables] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ĞÅÒÅÍÅÎÎÙÈ.';
  FTranslation[trNoConstants] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ËÏÎÓÔÁÎÔ.';
  FTranslation[trNoFunctions] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÆÕÎËÃÉÉ É ĞÒÏÃÅÄÕÒÙ.';
  FTranslation[trNoIdentifiers] := 'íÏÄÕÌÉ ÎÅ ÓÏÄÅÒÖÁÔ ÎÉ ÏÄÎÏÇÏ ÉÄÅÎÔÉÆÉËÁÔÏÒÁ.';
  FTranslation[trProgram] := 'ğÒÏÇÒÁÍÍÁ'; 
  FTranslation[trUses] := 'õßÎÍÊİÃÑÅËØÅ ËÍÄÑÊÕ';
end;

{ ---------------------------------------------------------------------------- }

const
  aSlovak: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Slovak',
  //map
    {trUnits} 'Jednotky',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Triedy, interfejsy a objekty',
    {trIdentifiers} 'Identifikátory',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Triedy',
      {trClass} 'Trieda',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} 'Interfejs',
    {trObjects} 'Objekty',
      {trObject} 'Objekt',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hierarchia',
        {trFields} 'Poloky',
        {trMethods} 'Metódy',
        {trProperties} 'Monosti',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Jednotka',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konštanty',
      {trFunctionsAndProcedures} 'Funkcie a procedúry',
      {trTypes} 'Typy',
        {trType} 'Typ',
      {trVariables} 'Premenné',
      {trAuthors} 'Autori',
        {trAuthor} 'Autor',
      {trCreated} 'Vytvorené',
      {trLastModified} 'Posledná zmena',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Parameters',
      {trReturns} strToDo, //'Returns',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} strToDo, //'Overview',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Všetky triedy, interfejsy a objekty',
    {trHeadlineConstants} 'Všetky konštanty',
    {trHeadlineFunctionsAndProcedures} 'Všetky funkcie a procedúry',
    {trHeadlineIdentifiers} 'Všetky identifikátory',
    {trHeadlineTypes} 'Všetky typy',
    {trHeadlineUnits} 'Všetky jednotky',
    {trHeadlineVariables} 'Všetky premenné',
    {trSummaryCio} 'Zoznam tried, interfejsov a objektov',
  //column headings
    {trDeclaration} 'Deklarácie',
    {trDescription} 'Popis',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Meno',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Niè',
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help',
    {trLegend} strToDo, //'Legend',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Upozornenie: Needitujte - tento súbor bol vytvorenı automaticky a je pravdepodobné, e bude prepísanı',
    {trWarning} 'Upozornenie',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aSpanish: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Spanish',
  //map
    {trUnits} 'Unidades',
    {trClassHierarchy} 'Jerarquía de clases',
    {trCio} 'Clases, interfaces y objetos',
    {trIdentifiers} 'Identificadores',
    {trGvUses} 'Gráfico de las dependencias de unidades',
    {trGvClasses} 'Gráfico de la jerarquía de clases',
  //tables and members
    {trClasses} 'Clases',
      {trClass} 'Clase',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objetos',
      {trObject} 'Objeto',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Jerarquía',
        {trFields} 'Campos',
        {trMethods} 'Métodos',
        {trProperties} 'Propiedades',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Unidad',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Constantes',
      {trFunctionsAndProcedures} 'Funciones y procedimientos',
      {trTypes} 'Tipos',
        {trType} 'Tipo',
      {trVariables} strKeep, //'Variables', //???
      {trAuthors} 'Autores',
        {trAuthor} 'Autor',
      {trCreated} 'Creado',
      {trLastModified} 'Última modificación',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Parámetros',
      {trReturns} 'Retorno', //strToDo??? solo uno!
      {trExceptionsRaised} 'Excepciones lanzadas',
    {trExceptions} 'Excepciones',
      {trException} 'Excepcion', //?
    {trEnum} strKeep, //'Enumeration',
  //visibilities
    {trVisibility} 'Visibilidad',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} 'Este símbolo está obsoleto',
    {trPlatformSpecific} 'Este símbolo es específico para alguna plataforma',
    {trLibrarySpecific} 'Este símbolo es específico para alguna librería',
  //headings
    {trOverview} 'Resumen',
    {trIntroduction} 'Introducción',
    {trConclusion} 'Conclusión',
    {trHeadlineCio} 'Todas las clases, interfaces y objetos',
    {trHeadlineConstants} 'Todas las constantes',
    {trHeadlineFunctionsAndProcedures} 'Todos las funciones y procedimientos',
    {trHeadlineIdentifiers} 'Todos los indentificadores',
    {trHeadlineTypes} 'Todos los tipos',
    {trHeadlineUnits} 'Todas las unidades',
    {trHeadlineVariables} 'Todas las variables',
    {trSummaryCio} 'Lista de clases, interfaces y objetos',
  //column headings
    {trDeclaration} 'Declaración',
    {trDescription} 'Descripción',
    {trDescriptions} 'Descripciónes', //? 'Descriptions', 'Detailed Descriptions'?
    {trName} 'Nombre',
    {trValues} 'Valores',
  //empty
    {trNone} 'Ninguno',
    {trNoCIOs} 'Las unidades no contienen ni clases ni interfaces ni objetos ni registros.',
    {trNoCIOsForHierarchy} 'Las unidades no contienen ni clases ni interfaces ni objetos.',
    {trNoTypes} 'Las unidades no contienen ningún tipo.',
    {trNoVariables} 'Las unidades no contienen ningunas variables.',
    {trNoConstants} 'Las unidades no contienen ningunas constantes.',
    {trNoFunctions} 'Las unidades no contienen ni funciones ni procedimientos',
      //??? strToDo, //'Las unidades no contienen ni variables ni procedimientos',
    {trNoIdentifiers} 'Las unidades no contienen ningún Identificador.',
  //misc
    {trHelp} 'Ayuda',
    {trLegend} 'Leyenda',
    {trMarker} 'Marcador',
    {trWarningOverwrite} 'Atención, no editar - este fichero ha sido creado automaticamente y puede ser sobrescrito',
    {trWarning} 'Atención',
    {trGeneratedBy} 'Generado por', //??? strToDo, //'Generador por',
    {trOnDateTime} 'a',
    {trSearch} 'Buscar',
    {trSeeAlso} 'Ver',
    ''  //dummy
  );

  aSwedish: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Swedish',
  //map
    {trUnits} 'Enheter',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} 'Klasser, interface och objekt',
    {trIdentifiers} strToDo, //'Identifiers',
    {trGvUses} strToDo, //'Unit dependency graph',
    {trGvClasses} strToDo, //'Classes hierarchy graph',
  //tables and members
    {trClasses} 'Klasser',
      {trClass} 'Klass',
      {trDispInterface} strToDo, //'DispInterface',
      {trInterface} strToDo, //'Interface',
    {trObjects} 'Objekt', //-er ???
      {trObject} 'Objekt',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hierarki',
        {trFields} 'Fält', //-er ???
        {trMethods} 'Metoder',
        {trProperties} strToDo, //'Properties',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Enhet',
      {trUses} strToDo, //'Uses',
      {trConstants} strToDo, //'Constants',
      {trFunctionsAndProcedures} strToDo, //'Functions and Procedures',
      {trTypes} 'Typer',
        {trType} 'Typer',
      {trVariables} 'Variabler',
      {trAuthors} 'Författare',
        {trAuthor} 'Författare',
      {trCreated} 'Skapad',
      {trLastModified} 'Senast ändrad',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Se parameter',
      {trReturns} 'Retur',
      {trExceptionsRaised} strToDo, //'Exceptions raised',
    {trExceptions} strToDo, //'Exceptions',
      {trException} strToDo, //'Exception',
    {trEnum} strToDo, //'Enumeration',
  //visibilities
    {trVisibility} strToDo, //'Visibility',
      {trPrivate} strToDo, //'Private',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} strToDo, //'Protected',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} strToDo, //'Public',
      {trPublished} strToDo, //'Published',
      {trAutomated} strToDo, //'Automated',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} strToDo, //'this symbol is deprecated',
    {trPlatformSpecific} strToDo, //'this symbol is specific to some platform',
    {trLibrarySpecific} strToDo, //'this symbol is specific to some library',
  //headings
    {trOverview} 'Översikt',
    {trIntroduction} strToDo, //'Introduction',
    {trConclusion} strToDo, //'Conclusion',
    {trHeadlineCio} 'Alla klasser, interface och objekt',
    {trHeadlineConstants} strToDo, //'All Constants',
    {trHeadlineFunctionsAndProcedures} 'Alla funktioner och procedurer',
    {trHeadlineIdentifiers} 'Alla identifierare',
    {trHeadlineTypes} 'Alla typer',
    {trHeadlineUnits} 'Alla enheter',
    {trHeadlineVariables} 'Alla variabler',
    {trSummaryCio} 'Sammanfattning av Klasser, Interface, Objekt',
  //column headings
    {trDeclaration} 'Deklarationer',
    {trDescription} 'Beskrivning',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Namn',
    {trValues} strToDo, //'Values',
  //empty
    {trNone} 'Ingen/inget.', //???
    {trNoCIOs} strToDo, //'The units do not contain any classes, interfaces, objects or records.',
    {trNoCIOsForHierarchy} strToDo, //'The units do not contain any classes, interfaces or objects.',
    {trNoTypes} strToDo, //'The units do not contain any types.',
    {trNoVariables} strToDo, //'The units do not contain any variables.',
    {trNoConstants} strToDo, //'The units do not contain any constants.',
    {trNoFunctions} strToDo, //'The units do not contain any functions or procedures.',
    {trNoIdentifiers} strToDo, //'The units do not contain any identifiers.',
  //misc
    {trHelp} strToDo, //'Help', // Untranslated to avoid Swedish file name for css
    {trLegend} 'Förklaring',
    {trMarker} strToDo, //'Marker',
    {trWarningOverwrite} 'Varning: ändra inte denna fil manuellt - filen har skapats automatiskt och kommer troligen att skrivas över vid ett senare tilfälle',
    {trWarning} 'Varning',
    {trGeneratedBy} strToDo, //'Generated by',
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    ''  //dummy
  );

  aHungarian_1250: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Hungarian',
  //map
    {trUnits} 'Egységek',
    {trClassHierarchy} 'Osztály hierarchia',
    {trCio} 'Osztályok, Kapcsolódási felületek és Objektumok',
    {trIdentifiers} 'Azonosítók',
    {trGvUses} 'Egység függõségi gráf',
    {trGvClasses} 'Osztály hierarchia gráf',
  //tables and members
    {trClasses} 'Osztályok',
      {trClass} 'Osztály',
      {trDispInterface} 'Képernyõ felületek',
      {trInterface} 'Kapcsolódási felület',
    {trObjects} 'Objektumok',
      {trObject} 'Objektum',
      {trRecord} strToDo, //'Record',
        {trHierarchy} 'Hierarchia',
        {trFields} 'Mezõk',
        {trMethods} 'Metódusok',
        {trProperties} 'Tulajdonságok',
    {trLibrary} strToDo,  //'Library',
    {trPackage} strToDo,  //'Package',
    {trProgram} strToDo,  //'Program',
    {trUnit} 'Egység',
      {trUses} strToDo, //'Uses',
      {trConstants} 'Konstansok',
      {trFunctionsAndProcedures} 'Függvények és Eljárások',
      {trTypes} 'Típusok',
        {trType} 'Típus',
      {trVariables} 'Változók',
      {trAuthors} 'Szerzõk',
        {trAuthor} 'Szerzõ',
      {trCreated} 'Készült',
      {trLastModified} 'Utolsó módosítás',
    {trSubroutine} strToDo, //'Subroutine',
      {trParameters} 'Paraméterek',
      {trReturns} 'Visszatérési értékek',
      {trExceptionsRaised} 'Kivételek kiemelése',
    {trExceptions} 'Kivételek',
      {trException} strToDo, //'Exception',
    {trEnum} 'Felsorolások',
  //visibilities
    {trVisibility} 'Láthatóság',
      {trPrivate} 'Privát',
      {trStrictPrivate} strToDo, //'Strict Private',
      {trProtected} 'Védett',
      {trStrictProtected} strToDo, //'Strict Protected',
      {trPublic} 'Publikus',
      {trPublished} 'Publikált',
      {trAutomated} 'Automatikus',
      {trImplicit} strToDo, //'Implicit',
  //hints
    {trDeprecated} 'ez az azonosító érték nélküli',
    {trPlatformSpecific} 'ez az azonosító szükséges némely platform számára',
    {trLibrarySpecific} 'ez az azonosító szükséges némely library számára',
  //headings
    {trOverview} 'Áttekintés',
    {trIntroduction} 'Bevezetõ',
    {trConclusion} 'Összefoglaló',
    {trHeadlineCio} 'Összes Osztály, Kapcsolódási felület és Objektumok',
    {trHeadlineConstants} 'Összes Kontans',
    {trHeadlineFunctionsAndProcedures} 'Összes Függvény és Eljárás',
    {trHeadlineIdentifiers} 'Összes Azonosító',
    {trHeadlineTypes} 'Összes Típus',
    {trHeadlineUnits} 'Összes Egység',
    {trHeadlineVariables} 'Összes Változó',
    {trSummaryCio} 'Öszefoglaló az Osztályokról, Kapcsoldási felületekrõl és Objektumokról',
  //column headings
    {trDeclaration} 'Deklaráció',
    {trDescription} 'Megjegyzés',
    {trDescriptions} strToDo, //'Descriptions', 'Detailed Descriptions'?
    {trName} 'Név',
    {trValues} 'Értékek',
  //empty
    {trNone} 'Nincs',
    {trNoCIOs} 'Az egység nem tartalmaz osztályt, interfészt, objektumot, vagy rekordot.',
    {trNoCIOsForHierarchy} 'Az egység nem tartalmaz osztályt, interfészt vagy objektumot.',
    {trNoTypes} 'Az egység nem tartalmaz típusokat',
    {trNoVariables} 'Az egység nem tartalmaz változókat.',
    {trNoConstants} 'Az egység nem tartalmaz konstansokat.',
    {trNoFunctions} 'Az egység nem tartalmaz függvényeket vagy eljárásokat.',
    {trNoIdentifiers} 'Az egység nem tartalmaz azonosítókat.',
  //misc
    {trHelp} 'Súgó',
    {trLegend} 'Történet',
    {trMarker} 'Jelzõ',
    {trWarningOverwrite} 'Vigyázat: Nem szerkesztendõ file - ez a file automatikusan készült, valószínûleg felülírásra kerülne',
    {trWarning} 'Vigyázat',
    {trGeneratedBy} 'Készítette',
    {trOnDateTime} ' ', //none in Hungarian language
    {trSearch} 'Keresés',
    {trSeeAlso} 'Lásd még',
    ''  //dummy
  );
  
  aCzech_ISO_8859_2: {$I PasDoc_Languages_Czech_ISO_8859_2.inc}
  aCzech_CP1250: {$I PasDoc_Languages_Czech_CP1250.inc}

const
  LANGUAGE_ARRAY: array[TLanguageID] of TLanguageRecord = (
    (Table: @aBosnian; Name: 'Bosnian (Codepage 1250)'; Syntax: 'ba'; CharSet: 'windows-1250'),
    (Table: @aBrasilian; Name: 'Brasilian'; Syntax: 'br'; CharSet: ''),
    (Table: @aCatalan; Name: 'Catalan'; Syntax: 'ct'; CharSet: ''),
    (Table: @aChinese_gb2312; Name: 'Chinese (Simple, gb2312)'; Syntax: 'gb2312'; CharSet: 'gb2312'),
    (Table: @aDanish; Name: 'Danish'; Syntax: 'dk'; CharSet: 'iso-8859-15'),
    (Table: @aDutch; Name: 'Dutch'; Syntax: 'nl'; CharSet: 'iso-8859-15'),
    (Table: @aEnglish; Name: 'English'; Syntax: 'en'; CharSet: 'iso-8859-1'),
    (Table: @aFrench; Name: 'French'; Syntax: 'fr'; CharSet: 'iso-8859-15'),
    (Table: @aGerman; Name: 'German'; Syntax: 'de'; CharSet: 'iso-8859-15'),
    (Table: @aIndonesian; Name: 'Indonesian'; Syntax: 'id'; CharSet: ''),
    (Table: @aItalian; Name: 'Italian'; Syntax: 'it'; CharSet: 'iso-8859-15'),
    (Table: @aJavanese; Name: 'Javanese'; Syntax: 'jv'; CharSet: ''),
    (Table: @aPolish1250; Name: 'Polish (Codepage CP1250)'; Syntax: 'pl.cp1250'; CharSet: 'windows-1250'),
    (Table: @aPolish_ISO_8859_2; Name: 'Polish (Codepage ISO 8859-2)'; Syntax: 'pl.iso-8859-2'; CharSet: 'iso-8859-2'),
    (Table: @aRussian_1251; Name: 'Russian (Codepage 1251)'; Syntax: 'ru.1251'; CharSet: 'windows-1251'),
    (Table: @aRussian_utf8; Name: 'Russian (Codepage utf8)'; Syntax: 'ru.utf8'; CharSet: 'utf-8'),
    (Table: nil;  Name: 'Russian (Codepage 866)'; Syntax: 'ru.866'; CharSet: 'IBM866'),
    (Table: nil;  Name: 'Russian (KOI-8)'; Syntax: 'ru.KOI8'; CharSet: 'koi8-r'),
    (Table: @aSlovak; Name: 'Slovak'; Syntax: 'sk'; CharSet: ''),
    (Table: @aSpanish; Name: 'Spanish'; Syntax: 'es'; CharSet: 'iso-8859-15'),
    (Table: @aSwedish; Name: 'Swedish'; Syntax: 'se'; CharSet: 'iso-8859-15'),
    (Table: @aHungarian_1250; Name: 'Hungarian (Codepage 1250)'; Syntax: 'hu.1250'; CharSet: 'windows-1250'),
    (Table: @aCzech_CP1250; Name: 'Czech (Codepage CP1250)'; Syntax: 'cz'; CharSet: 'windows-1250'),
    (Table: @aCzech_ISO_8859_2; Name: 'Czech (Codepage ISO 8859-2)'; Syntax: 'cz.iso-8859-2'; CharSet: 'iso-8859-2')
  );

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
  FCharSet := LANGUAGE_ARRAY[Value].Charset;
//get table
  pTable := LANGUAGE_ARRAY[Value].Table;
  if assigned(pTable) then
    exit; //array already exists

//use writeable table
  pTable := addr(aNewLanguage);
  case Value of
  //no array yet
    lgRussian_866: SetLanguageRussian_866;
    lgRussian_koi8: SetLanguageRussian_koi8;
  else  //this should never be reached
    pTable := addr(aEnglish);
  end;
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

end.

 	  	 
