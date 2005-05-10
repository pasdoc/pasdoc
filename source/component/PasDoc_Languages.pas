{ @abstract(PasDoc language definitions and translations.)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker <delphi@zeitungsjunge.de>)
  @author(Alexander Lisnevsky <alisnevsky@yandex.ru> (Russian translation))
  @author(Hendy Irawan <ceefour@gauldong.net> (Indonesian and Javanese translation))
  @author(Ivan Montes Velencoso (Catalan and Spanish translations))
  @author(Javi (Spanish translation))
  @author(Jean Dit Bailleul (Frensh translation))
  @author(Marc Weustinks (Dutch translation))
  @author(Martin Hansen <mh@geus.dk> (Danish translation))
  @author(Michele Bersini <michele.bersini@smartit.it> (Italian translation))
  @author(Peter Šimkoviè <simkovic_jr@manal.sk> (Slovak translation))
  @author(Peter Thörnqvist <pt@timemetrics.se> (Swedish translation))
  @author(Rodrigo Urubatan Ferreira Jardim <rodrigo@netscape.net> (Brasilian translation))
  @author(Alexandre da Silva <simpsomboy@gmail.com> (Brasilian translation - Update))
  @author(Vitaly Kovalenko <v_l_kovalenko@alsy.by> (Russian translation))
  @author(Grzegorz Skoczylas <gskoczylas@program.z.pl> (corrected Polish translation))
  @author(Jónás Gergõ <jonas.gergo@ch...> (Hungarian translation))
  }

unit PasDoc_Languages;

interface

type
  { An enumeration type of all supported languages }
  TLanguageID = (
    lgBosnian,
    lgBrasilian,
    lgCatalan,
    lgChinese_950,
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
    lgRussian_866,
    lgRussian_koi8,
    lgSlovak,
    lgSpanish,
    lgSwedish,
    lgHungarian_1250
   );

  { An enumeration type of all static output texts. }
  TTranslationID = (
    trAuthor,
    trAuthors,
    trAutomated,
    trCio,
    trClass,
    trClasses,
    trClassHierarchy,
    trConstants,
    trCreated,
    trDeclaration,
    trDescription,
    trParameters,
    trReturns,
    trExceptions,
    trEnum,
    trDispInterface,
    trFields,
    trFunctionsAndProcedures,
    trHelp,
    trHierarchy,
    trIdentifiers,
    trInterface,
    trLegend,
    trMethods,
    trLastModified,
    trName,
    trNone,
    trObject,
    trObjects,
    trOverview,
    trPrivate,
    trProperties,
    trProtected,
    trPublic,
    trPublished,
    trType,
    trTypes,
    trUnit,
    trUnits,
    trVariables,
    trGvUses,
    trGvClasses,

    trHeadlineCio,
    trHeadlineConstants,
    trHeadlineFunctionsAndProcedures,
    trHeadlineIdentifiers,
    trHeadlineTypes,
    trHeadlineUnits,
    trHeadlineVariables,

    trSummaryCio,

    trWarningOverwrite,

    trGeneratedBy,
    trOnDateTime,
    
    trDeprecated,
    trPlatformSpecific,
    trLibrarySpecific);

const
  DEFAULT_LANGUAGE = lgEnglish;

type
  { Language class to hold all translated strings }
  TPasDocLanguages = class
  private
    FLanguage: TLanguageID;
    procedure SetLanguage(const Value: TLanguageID);
  protected
    FTranslation: array[TTranslationID] of string;
    FCharSet: string;
    { @abstract(gets a translation token) }
    function GetTranslation(const ATranslationID: TTranslationID): string;
    { Defines translations for English (the default). }
    procedure SetLanguageEnglish;
    { Defines translations for Bosnian. }
    procedure SetLanguageBosnian;
    { Defines translations for Brasilian. }
    procedure SetLanguageBrasilian;
    { Defines translations for Catalan. }
    procedure SetLanguageCatalan;
    { Defines translations for Chinese (Codepage 950). }
    procedure SetLanguageChinese_950;
    { Defines translations for Danish. }
    procedure SetLanguageDanish;
    { Defines translations for Dutch. }
    procedure SetLanguageDutch;
    { Defines translations for French. }
    procedure SetLanguageFrench;
    { Defines translations for German }
    procedure SetLanguageGerman;
    { Defines translations for Indonesian. }
    procedure SetLanguageIndonesian;
    { Defines translations for Italian. }
    procedure SetLanguageItalian;
    { Defines translations for Javanese. }
    procedure SetLanguageJavanese;
    { Defines translations for Polish (Codepage 1250. }
    procedure SetLanguagePolish_CP1250;           
    { Defines translations for Polish (Codepage ISO 8859-2. }
    procedure SetLanguagePolish_ISO_8859_2;       
    { Defines translations for Russian (Codepage 1251). }
    procedure SetLanguageRussian_1251;
    { Defines translations for Russian (Codepage 866). }
    procedure SetLanguageRussian_866;
    { Defines translations for Russian (KOI-8). }
    procedure SetLanguageRussian_koi8;
    { Defines translations for Slovak. }
    procedure SetLanguageSlovak;
    { Defines translations for Spanish. }
    procedure SetLanguageSpanish;
    { Defines translations for Swedish. }
    procedure SetLanguageSwedish;
    { Defines translations for Hungarian (Codepage 1250). }
    procedure SetLanguageHungarian_1250;


  public
    { Charset for current language }
    property CharSet: string read FCharSet;
    property Translation[const ATranslationID: TTranslationID]: string read GetTranslation;
    constructor Create;
    property Language: TLanguageID read FLanguage write SetLanguage
      default DEFAULT_LANGUAGE;
  end;

type
  TLanguageRecord = record
    Name: string;
    Syntax: string;
    CharSet: string;
  end;

const
  LANGUAGE_ARRAY: array[TLanguageID] of TLanguageRecord = (
    (Name: 'Bosnian (Codepage 1250)'; Syntax: 'ba'; CharSet: 'windows-1250'),
    (Name: 'Brasilian'; Syntax: 'br'; CharSet: ''),
    (Name: 'Catalan'; Syntax: 'ct'; CharSet: ''),
    (Name: 'Chinese (Codepage 950)'; Syntax: 'big5'; CharSet: 'big5'),
    (Name: 'Danish'; Syntax: 'dk'; CharSet: 'iso-8859-15'),
    (Name: 'Dutch'; Syntax: 'nl'; CharSet: 'iso-8859-15'),
    (Name: 'English'; Syntax: 'en'; CharSet: 'iso-8859-1'),
    (Name: 'French'; Syntax: 'fr'; CharSet: 'iso-8859-15'),
    (Name: 'German'; Syntax: 'de'; CharSet: 'iso-8859-15'),
    (Name: 'Indonesian'; Syntax: 'id'; CharSet: ''),
    (Name: 'Italian'; Syntax: 'it'; CharSet: 'iso-8859-15'),
    (Name: 'Javanese'; Syntax: 'jv'; CharSet: ''),
    (Name: 'Polish (Codepage CP1250)'; Syntax: 'pl.cp1250'; CharSet: 'windows-1250'),       
    (Name: 'Polish (Codepage ISO 8859-2)'; Syntax: 'pl.iso-8859-2'; CharSet: 'iso-8859-2'), 
    (Name: 'Russian (Codepage 1251)'; Syntax: 'ru.1251'; CharSet: 'windows-1251'),
    (Name: 'Russian (Codepage 866)'; Syntax: 'ru.866'; CharSet: 'IBM866'),
    (Name: 'Russian (KOI-8)'; Syntax: 'ru.KOI8'; CharSet: 'koi8-r'),
    (Name: 'Slovak'; Syntax: 'sk'; CharSet: ''),
    (Name: 'Spanish'; Syntax: 'es'; CharSet: 'iso-8859-15'),
    (Name: 'Swedish'; Syntax: 'se'; CharSet: 'iso-8859-15'),
    (Name: 'Hungarian (Codepage 1250)'; Syntax: 'hu.1250'; CharSet: 'windows-1250')
    
    );

implementation

procedure TPasDocLanguages.SetLanguageEnglish;
begin
  FTranslation[trAuthor] := 'Author';
  FTranslation[trAuthors] := 'Authors';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Classes, Interfaces, Objects and Records';
  FTranslation[trClass] := 'Class';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trClassHierarchy] := 'Class Hierarchy';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Created';
  FTranslation[trDeclaration] := 'Declaration';
  FTranslation[trDescription] := 'Description';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Fields';
  FTranslation[trFunctionsAndProcedures] := 'Functions and Procedures';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchy';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMethods] := 'Methods';
  FTranslation[trLastModified] := 'Last Modified';
  FTranslation[trName] := 'Name';
  FTranslation[trNone] := 'None';
  FTranslation[trObject] := 'Object';
  FTranslation[trObjects] := 'Objects';
  FTranslation[trOverview] := 'Overview';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Types';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variables';
  FTranslation[trGvUses] := 'Unit dependency graph';
  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'All Classes, Interfaces, Objects and Records';
  FTranslation[trHeadlineConstants] := 'All Constants';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'All Functions and Procedures';
  FTranslation[trHeadlineIdentifiers] := 'All Identifiers';
  FTranslation[trHeadlineTypes] := 'All Types';
  FTranslation[trHeadlineUnits] := 'All Units';
  FTranslation[trHeadlineVariables] := 'All Variables';

  FTranslation[trSummaryCio] := 
    'Summary of Classes, Interfaces, Objects and Records';

  FTranslation[trWarningOverwrite] :=
    'Warning: Do not edit - this file has been created automatically and is likely be overwritten';

  FTranslation[trGeneratedBy] := 'Generated by';
  FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageBosnian;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Klase, Interfejsi i Objekti';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klase';
  FTranslation[trClassHierarchy] := 'Klasna hijerarhija';
  FTranslation[trConstants] := 'Konstante';
  FTranslation[trCreated] := 'Kreirano';
  FTranslation[trDeclaration] := 'Deklaracija';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Polja';
  FTranslation[trFunctionsAndProcedures] := 'Funkcije i Procedure';
  FTranslation[trHelp] := 'Pomoæ';
  FTranslation[trHierarchy] := 'Hijerarhija';
  FTranslation[trIdentifiers] := 'Identifikatori';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Metode';
  FTranslation[trLastModified] := 'Zadnja promjena';
  FTranslation[trName] := 'Ime';
  FTranslation[trNone] := 'Ništa';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekti';
  FTranslation[trOverview] := 'Pregled';
  FTranslation[trPrivate] := 'Privatni';
  FTranslation[trProperties] := 'Osibine';
  FTranslation[trProtected] := 'Zaštiæen';
  FTranslation[trPublic] := 'Publikovan';
  FTranslation[trPublished] := 'Javan';
  FTranslation[trType] := 'Tip';
  FTranslation[trTypes] := 'Tipovi';
  FTranslation[trUnit] := 'Fajl';
  FTranslation[trUnits] := 'Fajlovi';
  FTranslation[trVariables] := 'Promjenjive';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'Sve Klase, Interfejsi i Objekti';
  FTranslation[trHeadlineConstants] := 'Sve Konstante';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Sve Funkcije i Procedure';
  FTranslation[trHeadlineIdentifiers] := 'Svi Identifikatoti';
  FTranslation[trHeadlineTypes] := 'Svi Tipovi';
  FTranslation[trHeadlineUnits] := 'Svi Fajlovi';
  FTranslation[trHeadlineVariables] := 'Sve Varijable';

  FTranslation[trSummaryCio] := 'Zbirno od Klasa, Interfejsa i Objekata';

  FTranslation[trWarningOverwrite] :=
    'Upozorenje: Ne mjenjajte fajl - ovaj fajl je kreiran automatski i velika je vjerovatnoæa da æe biti prepisan';

  FTranslation[trGeneratedBy] := 'Generated by (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trOnDateTime] := 'on (PLEASE TRANSLATE THIS STRING)';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageBrasilian;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autores';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Classes, Interfaces, Objetos e Registros';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trClassHierarchy] := 'Hierarquia de Classes';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Criada';
  FTranslation[trDeclaration] := 'Declaração';
  FTranslation[trDescription] := 'Descrição';
  FTranslation[trParameters] := 'Parâmetros';
  FTranslation[trReturns] := 'Retornos';
  FTranslation[trExceptions] := 'Exceções';
  FTranslation[trEnum] := 'Enumerações';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campos';
  FTranslation[trFunctionsAndProcedures] := 'Funções e Procedimentos';
  FTranslation[trHelp] := 'Ajuda';
  FTranslation[trHierarchy] := 'Hierarquia';
  FTranslation[trIdentifiers] := 'Identificadores';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Métodos';
  FTranslation[trLastModified] := 'Última modificação';
  FTranslation[trName] := 'Nome';
  FTranslation[trNone] := 'Nenhum';
  FTranslation[trObject] := 'Objeto';
  FTranslation[trObjects] := 'Objetos';
  FTranslation[trOverview] := 'Visão Geral';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipos';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variáveis';
  FTranslation[trGvUses] := 'Diagrama de dependências de units';
  FTranslation[trGvClasses] := 'Diagrama de hierarquia de Classes';

  FTranslation[trHeadlineCio] := 'Todas as Classes, Interfaces, Objetos e Registros';
  FTranslation[trHeadlineConstants] := 'Todas as Constantes';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Todas as funções e procedimentos';
  FTranslation[trHeadlineIdentifiers] := 'Todos os Identificadores';
  FTranslation[trHeadlineTypes] := 'Todos os Tipos';
  FTranslation[trHeadlineUnits] := 'Todas as Units';
  FTranslation[trHeadlineVariables] := 'Todas as Variáveis';

  FTranslation[trSummaryCio] :=
    'Lista das Classes, Interfaces, Objetos e Registros';

  FTranslation[trWarningOverwrite] :=
    'Aviso, não altere - este arquivo foi gerado automaticamente e será sobrescrito';

  FTranslation[trGeneratedBy] := 'Gerado por';
  FTranslation[trOnDateTime] := 'as';

  FTranslation[trDeprecated] := 'este símbolo está depreciado';
  FTranslation[trPlatformSpecific] := 'este símbolo é específico para alguma plataforma';
  FTranslation[trLibrarySpecific] := 'este símbolo é específico para alguma biblioteca';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageCatalan;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autors';
  FTranslation[trCio] := 'Clases, interfaces i objectes';
  FTranslation[trClass] := 'Clase';
  FTranslation[trClasses] := 'Clases';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Creat';
  FTranslation[trDeclaration] := 'Declaraci¢';
  FTranslation[trDescription] := 'Descripci¢';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Camps';
  FTranslation[trFunctionsAndProcedures] := 'Funcions i procediments';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchy';
  FTranslation[trIdentifiers] := 'Identificadors';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLastModified] := 'Éltima modificaci¢';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMethods] := 'MŠtodes';
  FTranslation[trName] := 'Nom';
  FTranslation[trNone] := 'Ningu';
  FTranslation[trObject] := 'Objecte';
  FTranslation[trObjects] := 'Objectes';
  FTranslation[trOverview] := 'Resum';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Propietats';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipus';
  FTranslation[trTypes] := 'Tipus';
  FTranslation[trUnit] := 'Unitat';
  FTranslation[trUnits] := 'Unitats';
  FTranslation[trVariables] := 'Variables';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trWarningOverwrite] :=
    'Atenci¢, no editar - aquest fitxer ha estat creat automaticament i ser… sobrescrit';

  FTranslation[trHeadlineCio] := 'Totes les clases, interfaces i objectes';
  FTranslation[trHeadlineConstants] := 'Totes les constants';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Totes les funcions i procediments';
  FTranslation[trHeadlineIdentifiers] := 'Tot els indentificadors';
  FTranslation[trHeadlineTypes] := 'Tots els tipus';
  FTranslation[trHeadlineUnits] := 'Totes les unitats';
  FTranslation[trHeadlineVariables] := 'Totes les variables';

  FTranslation[trSummaryCio] := 'Llista de clases, interfaces i objectes';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageChinese_950;
begin
  FTranslation[trAuthor] := '§@ªÌ';
  FTranslation[trAuthors] := '§@ªÌ¸s';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageDanish;
begin
  FTranslation[trAuthor] := 'Forfatter';
  FTranslation[trAuthors] := 'Forfatre';
  FTranslation[trCio] := 'Klasser, interfaces og objekter';
  FTranslation[trClass] := 'Klasse';
  FTranslation[trClasses] := 'Klasser';
  FTranslation[trConstants] := 'Konstanter';
  FTranslation[trCreated] := 'Udført';
  FTranslation[trDeclaration] := 'Declaration';
  FTranslation[trDescription] := 'Beskrivelse';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Felter';
  FTranslation[trFunctionsAndProcedures] := 'Funktioner og prosedurer';
  FTranslation[trHelp] := 'Hjælp';
  FTranslation[trHierarchy] := 'Herarki';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legende';
  FTranslation[trLastModified] := 'Sidst Modificieret';
  FTranslation[trMethods] := 'Metoder';
  FTranslation[trName] := 'Navn';
  FTranslation[trNone] := 'Ingen';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekter';
  FTranslation[trOverview] := 'Sammendrag';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Egenskaber';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Typer';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variable';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trWarningOverwrite] :=
    'Advarsel: Editer ikke denne fil, den er autogeneret og vil sansylgvis blive overskret';

  FTranslation[trHeadlineCio] := 'Alle Klasesr, Interfaces og Objekter';
  FTranslation[trHeadlineConstants] := 'Alle Konstanter';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alle Functioner and Procedurer';
  FTranslation[trHeadlineIdentifiers] := 'Alle Identifiers';
  FTranslation[trHeadlineTypes] := 'Alle Typer';
  FTranslation[trHeadlineUnits] := 'Alle Units';
  FTranslation[trHeadlineVariables] := 'Alle Variable';

  FTranslation[trSummaryCio] :=
    'Oversigt over klasser, interfaces & objekter';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageDutch;
begin
  FTranslation[trAuthor] := 'Auteur';
  FTranslation[trAuthors] := 'Auteurs';
  FTranslation[trCio] := 'Classes, interfaces and objecten';
  FTranslation[trClass] := 'Class';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trConstants] := 'Constanten';
  FTranslation[trCreated] := 'Gemaakt';
  FTranslation[trDeclaration] := 'Declaratie';
  FTranslation[trDescription] := 'Omschrijving';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Velden';
  FTranslation[trFunctionsAndProcedures] := 'Functies en procedures';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Hierarchie';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLastModified] := 'Laatste wijziging';
  FTranslation[trLegend] := 'Legend';
  FTranslation[trMethods] := 'Methods';
  FTranslation[trName] := 'Naam';
  FTranslation[trNone] := 'Geen';
  FTranslation[trObject] := 'Object';
  FTranslation[trObjects] := 'Objecten';
  FTranslation[trOverview] := 'Overzicht';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Eigenschappen';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Typen';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variabelen';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trWarningOverwrite] :=
    'Waarschuwing, wijzig niets - dit bestand is automatisch gegenereerd en zal worden overschreven';

  FTranslation[trHeadlineCio] := 'Alle classes, interfaces en objecten';
  FTranslation[trHeadlineConstants] := 'Alle constanten';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alle functies en procedures';
  FTranslation[trHeadlineIdentifiers] := 'Alle identifiers';
  FTranslation[trHeadlineTypes] := 'Alle typen';
  FTranslation[trHeadlineUnits] := 'Alle units';
  FTranslation[trHeadlineVariables] := 'Alle variabelen';

  FTranslation[trSummaryCio] :=
    'Overzicht van classes, interfaces & objecten';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageFrench;
begin
  FTranslation[trAuthor] := 'Auteur';
  FTranslation[trAuthors] := 'Auteurs';
  FTranslation[trCio] := 'Classes, interfaces, structures et objets';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classes';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Crée';
  FTranslation[trDeclaration] := 'Déclaration';
  FTranslation[trDescription] := 'Description';
  FTranslation[trParameters] := 'Paramètres';
  FTranslation[trReturns] := 'Retourne';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Champs';
  FTranslation[trFunctionsAndProcedures] := 'Fonctions et procédures';
  FTranslation[trHelp] := 'Aide';
  FTranslation[trHierarchy] := 'Hierarchie';
  FTranslation[trIdentifiers] := 'Identificateurs';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLastModified] := 'Dernière modification';
  FTranslation[trLegend] := 'Légende';
  FTranslation[trMethods] := 'Méthodes';
  FTranslation[trName] := 'Nom';
  FTranslation[trNone] := 'Aucun(e)(s)';
  FTranslation[trObject] := 'Objet';
  FTranslation[trObjects] := 'Objets';
  FTranslation[trOverview] := 'Aperçu';
  FTranslation[trPrivate] := 'Privé';
  FTranslation[trProperties] := 'Propriétés';
  FTranslation[trProtected] := 'Protégé';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Publiés';
  FTranslation[trType] := 'Type';
  FTranslation[trTypes] := 'Types';
  FTranslation[trUnit] := 'Unité';
  FTranslation[trUnits] := 'Unités';
  FTranslation[trVariables] := 'Variables';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trWarningOverwrite] :=
    'Attention, ne pas édtier - ce fichier est créé automatiquement et va être écrasé';

  FTranslation[trHeadlineCio] := 'Toutes les classes, interfaces et objets';
  FTranslation[trHeadlineConstants] := 'Toutes les constants';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Toutes les fonctions et procédures';
  FTranslation[trHeadlineIdentifiers] := 'Tous les identificateurs';
  FTranslation[trHeadlineTypes] := 'Tous les types';
  FTranslation[trHeadlineUnits] := 'Toutes les unités';
  FTranslation[trHeadlineVariables] := 'Toutes les variables';

  FTranslation[trSummaryCio] := 'Classes, interfaces & objets';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageGerman;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autoren';
  FTranslation[trCio] := 'Klassen, Schnittstellen und Objekte';
  FTranslation[trClass] := 'Klasse';
  FTranslation[trClasses] := 'Klassen';
  FTranslation[trClassHierarchy] := 'Klassenhierarchie';
  FTranslation[trConstants] := 'Konstanten';
  FTranslation[trCreated] := 'Erstellt';
  FTranslation[trDeclaration] := 'Deklaration';
  FTranslation[trDescription] := 'Beschreibung';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Felder';
  FTranslation[trFunctionsAndProcedures] := 'Funktionen und Prozeduren';
  FTranslation[trHelp] := 'Hilfe';
  FTranslation[trHierarchy] := 'Hierarchie';
  FTranslation[trIdentifiers] := 'Bezeichner';
  FTranslation[trInterface] := 'Schnittstelle';
  FTranslation[trLastModified] := 'Letzte Änderung';
  FTranslation[trLegend] := 'Legende';
  FTranslation[trMethods] := 'Methoden';
  FTranslation[trName] := 'Name';
  FTranslation[trNone] := 'Keine';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekte';
  FTranslation[trOverview] := 'Übersicht';
  FTranslation[trPrivate] := 'Privat';
  FTranslation[trProperties] := 'Eigenschaften';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trTypes] := 'Typen';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variablen';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typen';
  FTranslation[trGvUses] := 'Unit Abhängigkeitsgraph';
  FTranslation[trGvClasses] := 'Klassenhierarchie Graph';

  FTranslation[trHeadlineCio] := 'Alle Klassen, Schnittstellen und Objekte';
  FTranslation[trHeadlineConstants] := 'Alle Konstanten';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alle Funktionen und Prozeduren';
  FTranslation[trHeadlineIdentifiers] := 'Alle Bezeichner';
  FTranslation[trHeadlineTypes] := 'Alle Typen';
  FTranslation[trHeadlineUnits] := 'Alle Units';
  FTranslation[trHeadlineVariables] := 'Alle Variablen';

  FTranslation[trWarningOverwrite] :=
    'Achtung: Nicht ändern - diese Datei wurde automatisch erstellt und wird möglicherweise überschrieben';

  FTranslation[trGeneratedBy] := 'Erstellt von';
  FTranslation[trOnDateTime] := 'am';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageIndonesian;
begin
  FTranslation[trAuthor] := 'Pembuat';
  FTranslation[trAuthors] := 'Pembuat';
  FTranslation[trCio] := 'Kelas, Interface, dan Objek';
  FTranslation[trClass] := 'Kelas';
  FTranslation[trClasses] := 'Kelas';
  FTranslation[trConstants] := 'Konstanta';
  FTranslation[trCreated] := 'Dibuat';
  FTranslation[trDeclaration] := 'Deklarasi';
  FTranslation[trDescription] := 'Definisi';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Field';
  FTranslation[trFunctionsAndProcedures] := 'Fungsi dan Prosedur';
  FTranslation[trHelp] := 'Bantuan';
  FTranslation[trHierarchy] := 'Hirarki';
  FTranslation[trIdentifiers] := 'Identifier';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Method';
  FTranslation[trLastModified] := 'Terakhir Dimodifikasi';
  FTranslation[trName] := 'Nama';
  FTranslation[trNone] := 'Tidak Ada';
  FTranslation[trObject] := 'Objek';
  FTranslation[trObjects] := 'Objek';
  FTranslation[trOverview] := 'Sekilas';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Property';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipe Bentukan';
  FTranslation[trTypes] := 'Tipe Bentukan';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Unit';
  FTranslation[trVariables] := 'Variabel';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'Semua Kelas, Interface, dan Objek';
  FTranslation[trHeadlineConstants] := 'Semua Konstanta';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Semua Fungsi dan Prosedur';
  FTranslation[trHeadlineIdentifiers] := 'Semua Identifier';
  FTranslation[trHeadlineTypes] := 'Semua Tipe Bentukan';
  FTranslation[trHeadlineUnits] := 'Semua Unit';
  FTranslation[trHeadlineVariables] := 'Semua Variabel';

  FTranslation[trSummaryCio] := 'Ringkasan Kelas, Interface, dan Objek';

  FTranslation[trWarningOverwrite] := 'Perhatian: Jangan dimodifikasi - '
    + 'file ini dihasilkan secara otomatis dan mungkin saja ditimpa ulang';

  FTranslation[trGeneratedBy] := 'Dihasilkan oleh';
  FTranslation[trOnDateTime] := 'pada';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageItalian;
begin
  FTranslation[trAuthor] := 'Autore';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Classi, Interfacce ed Oggetti';
  FTranslation[trClass] := 'Classe';
  FTranslation[trClasses] := 'Classi';
  FTranslation[trConstants] := 'Costanti';
  FTranslation[trCreated] := 'Creato';
  FTranslation[trDeclaration] := 'Dichiarazione';
  FTranslation[trDescription] := 'Descrizione';
  FTranslation[trParameters] := 'Parametri';
  FTranslation[trReturns] := 'Ritorni';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campi';
  FTranslation[trFunctionsAndProcedures] := 'Funzioni e Procedure';
  FTranslation[trHelp] := 'Help';
  FTranslation[trHierarchy] := 'Gerarchia';
  FTranslation[trIdentifiers] := 'Identificatori';
  FTranslation[trInterface] := 'Interfacce';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Metodi';
  FTranslation[trLastModified] := 'Ultima Variazione';
  FTranslation[trName] := 'Nome';
  FTranslation[trNone] := 'Nessuno';
  FTranslation[trObject] := 'Oggetto';
  FTranslation[trObjects] := 'Oggetti';
  FTranslation[trOverview] := 'Sommario';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Proprietà';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipi';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Units';
  FTranslation[trVariables] := 'Variabili';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'Tutte le Classi, Interfacce ed Oggetti';
  FTranslation[trHeadlineConstants] := 'Tutte le Costanti';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Tutte le Funzioni e Procedure';
  FTranslation[trHeadlineIdentifiers] := 'Tutti gli Identificatori';
  FTranslation[trHeadlineTypes] := 'Tutti i Tipi';
  FTranslation[trHeadlineUnits] := 'Tutte le Units';
  FTranslation[trHeadlineVariables] := 'Tutte le Variabili';

  FTranslation[trSummaryCio] := 'Sommario di Classi, Interfacce ed Oggetti';

  FTranslation[trWarningOverwrite] :=
    'Attenzione: Non modificare - questo file è stato generato automaticamente e verrà probabilmente sovrascritto';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageJavanese;
begin
  FTranslation[trAuthor] := 'Sing Nggawe';
  FTranslation[trAuthors] := 'Sing Nggawe';
  FTranslation[trCio] := 'Kelas, Interface, lan Objek';
  FTranslation[trClass] := 'Kelas';
  FTranslation[trClasses] := 'Kelas';
  FTranslation[trConstants] := 'Konstanta';
  FTranslation[trCreated] := 'Digawe';
  FTranslation[trDeclaration] := 'Deklarasi';
  FTranslation[trDescription] := 'Katrangan';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Field';
  FTranslation[trFunctionsAndProcedures] := 'Fungsi lan Prosedur';
  FTranslation[trHelp] := 'Tulung';
  FTranslation[trHierarchy] := 'Hirarki';
  FTranslation[trIdentifiers] := 'Identifier';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Katrangan';
  FTranslation[trMethods] := 'Method';
  FTranslation[trLastModified] := 'Terakhir Diowahi';
  FTranslation[trName] := 'Jeneng';
  FTranslation[trNone] := 'Mboten Wonten';
  FTranslation[trObject] := 'Objek';
  FTranslation[trObjects] := 'Objek';
  FTranslation[trOverview] := 'Pambuka';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Property';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Macem Gawean';
  FTranslation[trTypes] := 'Macem Gawean';
  FTranslation[trUnit] := 'Unit';
  FTranslation[trUnits] := 'Unit';
  FTranslation[trVariables] := 'Variabel';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'Kabeh Kelas, Interface, lan Objek';
  FTranslation[trHeadlineConstants] := 'Kabeh Konstanta';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Kabeh Fungsi lan Prosedur';
  FTranslation[trHeadlineIdentifiers] := 'Kabeh Identifier';
  FTranslation[trHeadlineTypes] := 'Kabeh Macem Gawean';
  FTranslation[trHeadlineUnits] := 'Kabeh Unit';
  FTranslation[trHeadlineVariables] := 'Kabeh Variabel';

  FTranslation[trSummaryCio] := 'Ringkesan Kelas, Interface, lan Objek';

  FTranslation[trWarningOverwrite] := 'Ati-ati: Ojo diowahi - '
    + 'file iki digawe otomatis dadi iso ilang owahanmu';

  FTranslation[trGeneratedBy] := 'Dihasilne karo';
  FTranslation[trOnDateTime] := 'ing';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguagePolish_CP1250;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autorzy';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Klasy, interfejsy, obiekty i rekordy';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klasy';
  FTranslation[trClassHierarchy] := 'Hierarchia klas';
  FTranslation[trConstants] := 'Sta³e';
  FTranslation[trCreated] := 'Utworzony';
  FTranslation[trDeclaration] := 'Deklaracja';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parametry';
  FTranslation[trReturns] := 'Wynik';
  FTranslation[trExceptions] := 'Wyj¹tki';
  FTranslation[trEnum] := 'Wyliczenie';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Pola';
  FTranslation[trFunctionsAndProcedures] := 'Podprogramy';
  FTranslation[trHelp] := 'Pomoc';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identyfikatory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Metody';
  FTranslation[trLastModified] := 'Ostatnia modyfikacja';
  FTranslation[trName] := 'Nazwa';
  FTranslation[trNone] := 'Brak';
  FTranslation[trObject] := 'Obiekt';
  FTranslation[trObjects] := 'Obiekty';
  FTranslation[trOverview] := 'Przegl¹d';
  FTranslation[trPrivate] := 'Prywatne';
  FTranslation[trProperties] := 'W³aœciwoœci';
  FTranslation[trProtected] := 'Chronione';
  FTranslation[trPublic] := 'Publiczne';
  FTranslation[trPublished] := 'Publikowane';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Modu³';
  FTranslation[trUnits] := 'Modu³y';
  FTranslation[trVariables] := 'Zmienne';
  FTranslation[trGvUses] := 'Graf zale¿noœci modu³ów';
  FTranslation[trGvClasses] := 'Graf dziedziczenia klas';

  FTranslation[trHeadlineCio] := 'Wszystkie klasy, interfejsy, obiekty i rekordy';
  FTranslation[trHeadlineConstants] := 'Wszystkie sta³e';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Wszystkie podprogramy';
  FTranslation[trHeadlineIdentifiers] := 'Wszystkie identyfikatory';
  FTranslation[trHeadlineTypes] := 'Wszystkie typy';
  FTranslation[trHeadlineUnits] := 'Wszystkie modu³y';
  FTranslation[trHeadlineVariables] := 'Wszystkie zmienne';

  FTranslation[trSummaryCio] := 
    'Podsumowanie klas, interfejsów, obiektów i rekordów';

  FTranslation[trWarningOverwrite] :=
    'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany';

  FTranslation[trGeneratedBy] := 'Wygenerowane przez';
  FTranslation[trOnDateTime] := ' - ';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

procedure TPasDocLanguages.SetLanguagePolish_ISO_8859_2;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autorzy';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Klasy, interfejsy, obiekty i rekordy';
  FTranslation[trClass] := 'Klasa';
  FTranslation[trClasses] := 'Klasy';
  FTranslation[trClassHierarchy] := 'Hierarchia klas';
  FTranslation[trConstants] := 'Sta³e';
  FTranslation[trCreated] := 'Utworzony';
  FTranslation[trDeclaration] := 'Deklaracja';
  FTranslation[trDescription] := 'Opis';
  FTranslation[trParameters] := 'Parametry';
  FTranslation[trReturns] := 'Wynik';
  FTranslation[trExceptions] := 'Wyj±tki';
  FTranslation[trEnum] := 'Wyliczenie';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Pola';
  FTranslation[trFunctionsAndProcedures] := 'Podprogramy';
  FTranslation[trHelp] := 'Pomoc';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identyfikatory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLegend] := 'Legenda';
  FTranslation[trMethods] := 'Metody';
  FTranslation[trLastModified] := 'Ostatnia modyfikacja';
  FTranslation[trName] := 'Nazwa';
  FTranslation[trNone] := 'Brak';
  FTranslation[trObject] := 'Obiekt';
  FTranslation[trObjects] := 'Obiekty';
  FTranslation[trOverview] := 'Przegl±d';
  FTranslation[trPrivate] := 'Prywatne';
  FTranslation[trProperties] := 'W³a¶ciwo¶ci';
  FTranslation[trProtected] := 'Chronione';
  FTranslation[trPublic] := 'Publiczne';
  FTranslation[trPublished] := 'Publikowane';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Modu³';
  FTranslation[trUnits] := 'Modu³y';
  FTranslation[trVariables] := 'Zmienne';
  FTranslation[trGvUses] := 'Graf zale¿no¶ci modu³ów';
  FTranslation[trGvClasses] := 'Graf dziedziczenia klas';

  FTranslation[trHeadlineCio] := 'Wszystkie klasy, interfejsy, obiekty i rekordy';
  FTranslation[trHeadlineConstants] := 'Wszystkie sta³e';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Wszystkie podprogramy';
  FTranslation[trHeadlineIdentifiers] := 'Wszystkie identyfikatory';
  FTranslation[trHeadlineTypes] := 'Wszystkie typy';
  FTranslation[trHeadlineUnits] := 'Wszystkie modu³y';
  FTranslation[trHeadlineVariables] := 'Wszystkie zmienne';

  FTranslation[trSummaryCio] := 
    'Podsumowanie klas, interfejsów, obiektów i rekordów';

  FTranslation[trWarningOverwrite] :=
    'Uwaga, nie modyfikuj - ten plik zosta³ wygenerowany automatycznie i mo¿e zostaæ nadpisany';

  FTranslation[trGeneratedBy] := 'Wygenerowane przez';
  FTranslation[trOnDateTime] := ' - ';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_1251;
begin
  FTranslation[trAuthor] := 'Àâòîð';
  FTranslation[trAuthors] := 'Àâòîðû';
  FTranslation[trCio] := 'Êëàññû, èíòåðôåéñû è îáúåêòû';
  FTranslation[trClass] := 'Êëàññ';
  FTranslation[trClasses] := 'Êëàññû';
  FTranslation[trConstants] := 'Êîíñòàíòû';
  FTranslation[trCreated] := 'Ñîçäàíî';
  FTranslation[trDeclaration] := 'Îáúÿâëåíèÿ';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDescription] := 'Îïèñàíèå';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Ïîëÿ';
  FTranslation[trFunctionsAndProcedures] := 'Ïðîöåäóðû è ôóíêöèè';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'Èåðàðõèÿ';
  FTranslation[trIdentifiers] := 'Èäåíòèôèêàòîðû';
  FTranslation[trInterface] := 'Èíòåðôåéñ';
  FTranslation[trLegend] := 'Îáîçíà÷åíèÿ';
  FTranslation[trLastModified] := 'Ïîñëåäíåå èçìåíåíèå';
  FTranslation[trMethods] := 'Ìåòîäû';
  FTranslation[trName] := 'Èìÿ';
  FTranslation[trNone] := 'Íåò';
  FTranslation[trObject] := 'Îáúåêò';
  FTranslation[trObjects] := 'Îáúåêòû';
  FTranslation[trOverview] := 'Îáçîð';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Ñâîéñòâà';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Òèï';
  FTranslation[trTypes] := 'Òèïû';
  FTranslation[trUnit] := 'Ìîäóëü';
  FTranslation[trUnits] := 'Ìîäóëè';
  FTranslation[trVariables] := 'Ïåðåìåííûå';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trWarningOverwrite] :=
    'Ïðåäóïðåæäåíèå: íå ðåäàêòèðîâàòü - ýòîò ôàéë ñîçäàí àâòîìàòè÷åñêè è ìîæåò áûòü èçìåí¸í áåç ïðåäóïðåæäåíèÿ';

  FTranslation[trHeadlineCio] := 'Âñå êëàññû, èíòåðôåéñû è îáúåêòû';
  FTranslation[trHeadlineConstants] := 'Âñå êîíñòàíòû';
  FTranslation[trHeadlineFunctionsAndProcedures] := 'Âñå ïðîöåäóðû è ôóíêöèè';
  FTranslation[trHeadlineIdentifiers] := 'Âñå èäåíòèôèêàòîðû';
  FTranslation[trHeadlineTypes] := 'Âñå òèïû';
  FTranslation[trHeadlineUnits] := 'Âñå ìîäóëè';
  FTranslation[trHeadlineVariables] := 'Âñå ïåðåìåííûå';

  FTranslation[trSummaryCio] := 'Ñïèñîê êëàññîâ, èíòåðôåéñîâ è îáúåêòîâ';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_866;
begin
  FTranslation[trAuthor] := '€¢â®à';
  FTranslation[trAuthors] := '€¢â®àë';
  FTranslation[trCio] := 'Š« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trClass] := 'Š« áá';
  FTranslation[trClasses] := 'Š« ááë';
  FTranslation[trConstants] := 'Š®­áâ ­âë';
  FTranslation[trCreated] := '‘®§¤ ­®';
  FTranslation[trDeclaration] := 'Ž¡êï¢«¥­¨ï';
  FTranslation[trDescription] := 'Ž¯¨á ­¨¥';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := '®«ï';
  FTranslation[trFunctionsAndProcedures] := 'à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'ˆ¥à àå¨ï';
  FTranslation[trIdentifiers] := 'ˆ¤¥­â¨ä¨ª â®àë';
  FTranslation[trInterface] := 'ˆ­â¥àä¥©á';
  FTranslation[trLegend] := 'Ž¡®§­ ç¥­¨ï';
  FTranslation[trLastModified] := '®á«¥¤­¥¥ ¨§¬¥­¥­¨¥';
  FTranslation[trMethods] := 'Œ¥â®¤ë';
  FTranslation[trName] := 'ˆ¬ï';
  FTranslation[trNone] := '¥â';
  FTranslation[trObject] := 'Ž¡ê¥ªâ';
  FTranslation[trObjects] := 'Ž¡ê¥ªâë';
  FTranslation[trOverview] := 'Ž¡§®à';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := '‘¢®©áâ¢ ';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := '’¨¯';
  FTranslation[trTypes] := '’¨¯ë';
  FTranslation[trUnit] := 'Œ®¤ã«ì';
  FTranslation[trUnits] := 'Œ®¤ã«¨';
  FTranslation[trVariables] := '¥à¥¬¥­­ë¥';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trWarningOverwrite] :=
    'à¥¤ã¯à¥¦¤¥­¨¥: ­¥ à¥¤ ªâ¨à®¢ âì - íâ®â ä ©« á®§¤ ­  ¢â®¬ â¨ç¥áª¨ ¨ ¬®¦¥â ¡ëâì ¨§¬¥­ñ­ ¡¥§ ¯à¥¤ã¯à¥¦¤¥­¨ï';

  FTranslation[trHeadlineCio] := '‚á¥ ª« ááë, ¨­â¥àä¥©áë ¨ ®¡ê¥ªâë';
  FTranslation[trHeadlineConstants] := '‚á¥ ª®­áâ ­âë';
  FTranslation[trHeadlineFunctionsAndProcedures] := '‚á¥ ¯à®æ¥¤ãàë ¨ äã­ªæ¨¨';
  FTranslation[trHeadlineIdentifiers] := '‚á¥ ¨¤¥­â¨ä¨ª â®àë';
  FTranslation[trHeadlineTypes] := '‚á¥ â¨¯ë';
  FTranslation[trHeadlineUnits] := '‚á¥ ¬®¤ã«¨';
  FTranslation[trHeadlineVariables] := '‚á¥ ¯¥à¥¬¥­­ë¥';

  FTranslation[trSummaryCio] := '‘¯¨á®ª ª« áá®¢, ¨­â¥àä¥©á®¢ ¨ ®¡ê¥ªâ®¢';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';

  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageRussian_koi8;
begin
  FTranslation[trAuthor] := 'á×ÔÏÒ';
  FTranslation[trAuthors] := 'á×ÔÏÒÙ';
  FTranslation[trCio] := 'ëÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trClass] := 'ëÌÁÓÓ';
  FTranslation[trClasses] := 'ëÌÁÓÓÙ';
  FTranslation[trConstants] := 'ëÏÎÓÔÁÎÔÙ';
  FTranslation[trCreated] := 'óÏÚÄÁÎÏ';
  FTranslation[trDeclaration] := 'ïÂßÑ×ÌÅÎÉÑ';
  FTranslation[trDescription] := 'ïÐÉÓÁÎÉÅ';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trFields] := 'ðÏÌÑ';
  FTranslation[trFunctionsAndProcedures] := 'ðÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Russian file name for css
  FTranslation[trHierarchy] := 'éÅÒÁÒÈÉÑ';
  FTranslation[trIdentifiers] := 'éÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trInterface] := 'éÎÔÅÒÆÅÊÓ';
  FTranslation[trLegend] := 'ïÂÏÚÎÁÞÅÎÉÑ';
  FTranslation[trLastModified] := 'ðÏÓÌÅÄÎÅÅ ÉÚÍÅÎÅÎÉÅ';
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
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'ôÉÐ';
  FTranslation[trTypes] := 'ôÉÐÙ';
  FTranslation[trUnit] := 'íÏÄÕÌØ';
  FTranslation[trUnits] := 'íÏÄÕÌÉ';
  FTranslation[trVariables] := 'ðÅÒÅÍÅÎÎÙÅ';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trWarningOverwrite] :=
    'ðÒÅÄÕÐÒÅÖÄÅÎÉÅ: ÎÅ ÒÅÄÁËÔÉÒÏ×ÁÔØ - ÜÔÏÔ ÆÁÊÌ ÓÏÚÄÁÎ Á×ÔÏÍÁÔÉÞÅÓËÉ É ÍÏÖÅÔ ÂÙÔØ ÉÚÍÅÎ£Î ÂÅÚ ÐÒÅÄÕÐÒÅÖÄÅÎÉÑ';

  FTranslation[trHeadlineCio] := '÷ÓÅ ËÌÁÓÓÙ, ÉÎÔÅÒÆÅÊÓÙ É ÏÂßÅËÔÙ';
  FTranslation[trHeadlineConstants] := '÷ÓÅ ËÏÎÓÔÁÎÔÙ';
  FTranslation[trHeadlineFunctionsAndProcedures] := '÷ÓÅ ÐÒÏÃÅÄÕÒÙ É ÆÕÎËÃÉÉ';
  FTranslation[trHeadlineIdentifiers] := '÷ÓÅ ÉÄÅÎÔÉÆÉËÁÔÏÒÙ';
  FTranslation[trHeadlineTypes] := '÷ÓÅ ÔÉÐÙ';
  FTranslation[trHeadlineUnits] := '÷ÓÅ ÍÏÄÕÌÉ';
  FTranslation[trHeadlineVariables] := '÷ÓÅ ÐÅÒÅÍÅÎÎÙÅ';

  FTranslation[trSummaryCio] := 'óÐÉÓÏË ËÌÁÓÓÏ×, ÉÎÔÅÒÆÅÊÓÏ× É ÏÂßÅËÔÏ×';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSlovak;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autori';
  FTranslation[trCio] := 'Triedy, interfejsy a objekty';
  FTranslation[trClass] := 'Trieda';
  FTranslation[trClasses] := 'Triedy';
  FTranslation[trConstants] := 'Konštanty';
  FTranslation[trCreated] := 'Vytvorené';
  FTranslation[trDeclaration] := 'Deklarácie';
  FTranslation[trDescription] := 'Popis';
  FTranslation[trParameters] := 'Parameters';
  FTranslation[trReturns] := 'Returns';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trFields] := 'Položky';
  FTranslation[trFunctionsAndProcedures] := 'Funkcie a procedúry';
  FTranslation[trHierarchy] := 'Hierarchia';
  FTranslation[trIdentifiers] := 'Identifikátory';
  FTranslation[trInterface] := 'Interfejs';
  FTranslation[trLastModified] := 'Posledná zmena';
  FTranslation[trMethods] := 'Metódy';
  FTranslation[trName] := 'Meno';
  FTranslation[trNone] := 'Niè';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekty';
  FTranslation[trOverview] := 'Overview';
  FTranslation[trProperties] := 'Možnosti';
  FTranslation[trType] := 'Typ';
  FTranslation[trTypes] := 'Typy';
  FTranslation[trUnit] := 'Jednotka';
  FTranslation[trUnits] := 'Jednotky';
  FTranslation[trVariables] := 'Premenné';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trWarningOverwrite] :=
    'Upozornenie: Needitujte - tento súbor bol vytvorený automaticky a je pravdepodobné, že bude prepísaný';

  FTranslation[trHeadlineCio] := 'Všetky triedy, interfejsy a objekty';
  FTranslation[trHeadlineConstants] := 'Všetky konštanty';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Všetky funkcie a procedúry';
  FTranslation[trHeadlineIdentifiers] := 'Všetky identifikátory';
  FTranslation[trHeadlineTypes] := 'Všetky typy';
  FTranslation[trHeadlineUnits] := 'Všetky jednotky';
  FTranslation[trHeadlineVariables] := 'Všetky premenné';

  FTranslation[trSummaryCio] := 'Zoznam tried, interfejsov a objektov';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSpanish;
begin
  FTranslation[trAuthor] := 'Autor';
  FTranslation[trAuthors] := 'Autores';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := 'Clases, interfaces y objetos';
  FTranslation[trClass] := 'Clase';
  FTranslation[trClasses] := 'Clases';
  FTranslation[trClassHierarchy] := 'Jerarquía de clases';
  FTranslation[trConstants] := 'Constantes';
  FTranslation[trCreated] := 'Creado';
  FTranslation[trDeclaration] := 'Declaración';
  FTranslation[trDescription] := 'Descripción';
  FTranslation[trParameters] := 'Parámetros';
  FTranslation[trReturns] := 'Vueltas';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Campos';
  FTranslation[trFunctionsAndProcedures] := 'Funciones y procedimientos';
  FTranslation[trHelp] := 'Ayuda';
  FTranslation[trHierarchy] := 'Jerarquía';
  FTranslation[trIdentifiers] := 'Identificadores';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Leyenda';
  FTranslation[trMethods] := 'Métodos';
  FTranslation[trLastModified] := 'Última modificación';
  FTranslation[trName] := 'Nombre';
  FTranslation[trNone] := 'Ninguno';
  FTranslation[trObject] := 'Objeto';
  FTranslation[trObjects] := 'Objetos';
  FTranslation[trOverview] := 'Resumen';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Propiedades';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Tipo';
  FTranslation[trTypes] := 'Tipos';
  FTranslation[trUnit] := 'Unidad';
  FTranslation[trUnits] := 'Unidades';
  FTranslation[trVariables] := 'Variables';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';
  FTranslation[trHeadlineCio] := 'Todas las clases, interfaces y objetos';
  FTranslation[trHeadlineConstants] := 'Todas las constantes';
  FTranslation[trHeadlineFunctionsAndProcedures] :=    'Todos las funciones y procedimientos';
  FTranslation[trHeadlineIdentifiers] := 'Todos los indentificadores';
  FTranslation[trHeadlineTypes] := 'Todos los tipos';
  FTranslation[trHeadlineUnits] := 'Todas las unidades';
  FTranslation[trHeadlineVariables] := 'Todas las variables';
  FTranslation[trSummaryCio] := 'Lista de clases, interfaces y objetos';
  FTranslation[trWarningOverwrite] :=    'Atención, no editar - este fichero ha sido creado automaticamente y puede ser sobrescrito';
  FTranslation[trGeneratedBy] := 'Generador por';
  FTranslation[trOnDateTime] := 'a';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageSwedish;
begin
  FTranslation[trAuthor] := 'Författare';
  FTranslation[trAuthors] := 'Författare';
  FTranslation[trCio] := 'Klasser, interface och objekt';
  FTranslation[trClass] := 'Klass';
  FTranslation[trClasses] := 'Klasser';
  FTranslation[trConstants] := 'Constants';
  FTranslation[trCreated] := 'Skapad';
  FTranslation[trDeclaration] := 'Deklarationer';
  FTranslation[trDescription] := 'Beskrivning';
  FTranslation[trParameters] := 'Se parameter';
  FTranslation[trReturns] := 'Retur';
  FTranslation[trExceptions] := 'Exceptions';
  FTranslation[trEnum] := 'Enumeration';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := 'Fält';
  FTranslation[trFunctionsAndProcedures] := 'Functions and Procedures';
  FTranslation[trHelp] := 'Help';
    // Untranslated to avoid Swedish file name for css
  FTranslation[trHierarchy] := 'Hierarki';
  FTranslation[trIdentifiers] := 'Identifiers';
  FTranslation[trInterface] := 'Interface';
  FTranslation[trLegend] := 'Förklaring';
  FTranslation[trMethods] := 'Metoder';
  FTranslation[trLastModified] := 'Senast ändrad';
  FTranslation[trName] := 'Namn';
  FTranslation[trNone] := 'Ingen/inget.';
  FTranslation[trObject] := 'Objekt';
  FTranslation[trObjects] := 'Objekt';
  FTranslation[trOverview] := 'Översikt';
  FTranslation[trPrivate] := 'Private';
  FTranslation[trProperties] := 'Properties';
  FTranslation[trProtected] := 'Protected';
  FTranslation[trPublic] := 'Public';
  FTranslation[trPublished] := 'Published';
  FTranslation[trType] := 'Typer';
  FTranslation[trTypes] := 'Typer';
  FTranslation[trUnit] := 'Enhet';
  FTranslation[trUnits] := 'Enheter';
  FTranslation[trVariables] := 'Variabler';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

  FTranslation[trHeadlineCio] := 'Alla klasser, interface och objekt';
  FTranslation[trHeadlineConstants] := 'All Constants';
  FTranslation[trHeadlineFunctionsAndProcedures] :=
    'Alla funktioner och procedurer';
  FTranslation[trHeadlineIdentifiers] := 'Alla identifierare';
  FTranslation[trHeadlineTypes] := 'Alla typer';
  FTranslation[trHeadlineUnits] := 'Alla enheter';
  FTranslation[trHeadlineVariables] := 'Alla variabler';

  FTranslation[trSummaryCio] :=
    'Sammanfattning av Klasser, Interface, Objekt';

  FTranslation[trWarningOverwrite] :=
    'Varning: ändra inte denna fil manuellt - filen har skapats automatiskt och kommer troligen att skrivas över vid ett senare tilfälle';

  // Please translate
  // FTranslation[trGeneratedBy] := 'Generated by';
  // FTranslation[trOnDateTime] := 'on';
  
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

{ ---------------------------------------------------------------------------- }

procedure TPasDocLanguages.SetLanguageHungarian_1250;
 begin
   FTranslation[trAuthor] := 'Szerzõ';
   FTranslation[trAuthors] := 'Szerzõk';
   FTranslation[trAutomated] := 'Automatikus';
   FTranslation[trCio] := 'Osztályok, Kapcsolódási felületek és Objektumok';
   FTranslation[trClass] := 'Osztály';
   FTranslation[trClasses] := 'Osztályok';
   FTranslation[trClassHierarchy] := 'Osztály hierarchia';
   FTranslation[trConstants] := 'Konstansok';
   FTranslation[trCreated] := 'Készült';
   FTranslation[trDeclaration] := 'Deklaráció';
   FTranslation[trDescription] := 'Megjegyzés';
   FTranslation[trParameters] := 'Paraméterek';
   FTranslation[trReturns] := 'Visszatérési értékek';
   FTranslation[trExceptions] := 'Kivételek';
   FTranslation[trEnum] := 'Felsorolások';
   FTranslation[trDispInterface] := 'Képernyõ felületek';
   FTranslation[trFields] := 'Mezõk';
   FTranslation[trFunctionsAndProcedures] := 'Függvények és Eljárások';
   FTranslation[trHelp] := 'Súgó';
   FTranslation[trHierarchy] := 'Hierarchia';
   FTranslation[trIdentifiers] := 'Azonosítók';
   FTranslation[trInterface] := 'Kapcsolódási felület';
   FTranslation[trLegend] := 'Történet';
   FTranslation[trMethods] := 'Metódusok';
   FTranslation[trLastModified] := 'Utolsó módosítás';
   FTranslation[trName] := 'Név';
   FTranslation[trNone] := 'Nincs';
   FTranslation[trObject] := 'Objektum';
   FTranslation[trObjects] := 'Objektumok';
   FTranslation[trOverview] := 'Áttekintés';
   FTranslation[trPrivate] := 'Privát';
   FTranslation[trProperties] := 'Tulajdonságok';
   FTranslation[trProtected] := 'Védett';
   FTranslation[trPublic] := 'Publikus';
   FTranslation[trPublished] := 'Publikált';
   FTranslation[trType] := 'Típus';
   FTranslation[trTypes] := 'Típusok';
   FTranslation[trUnit] := 'Egység';
   FTranslation[trUnits] := 'Egységek';
   FTranslation[trVariables] := 'Változók';
  { TODO : add translation }
//  FTranslation[trGvUses] := 'Unit dependency graph';
//  FTranslation[trGvClasses] := 'Classes hierarchy graph';

   FTranslation[trHeadlineCio] := 'Összes Osztály, Kapcsolódási felület és Objektumok';
   FTranslation[trHeadlineConstants] := 'Összes Kontans';
   FTranslation[trHeadlineFunctionsAndProcedures] := 'Összes Függvény és Eljárás';
   FTranslation[trHeadlineIdentifiers] := 'Összes Azonosító';
   FTranslation[trHeadlineTypes] := 'Összes Típus';
   FTranslation[trHeadlineUnits] := 'Összes Egység';
   FTranslation[trHeadlineVariables] := 'Összes Változó';
  
   FTranslation[trSummaryCio] := 'Öszefoglaló az Osztályokról, Kapcsoldási felületekrõl és Objektumokról';
  
   FTranslation[trWarningOverwrite] :=
     'Vigyázat: Nem szerkesztendõ file - ez a file automatikusan készült, valószínûleg felülírásra kerülne';
  
   FTranslation[trGeneratedBy] := 'Készítette';
   FTranslation[trOnDateTime] := ''; //none in Hungarian language
   
  FTranslation[trDeprecated] := 'this symbol is deprecated (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trPlatformSpecific] := 'this symbol is specific to some platform (PLEASE TRANSLATE THIS STRING)';
  FTranslation[trLibrarySpecific] := 'this symbol is specific to some library (PLEASE TRANSLATE THIS STRING)';
end;

function TPasDocLanguages.GetTranslation(
  const ATranslationID: TTranslationID): string;
begin
  Result := FTranslation[ATranslationID];
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
  case Value of
    lgBosnian: SetLanguageBosnian;
    lgBrasilian: SetLanguageBrasilian;
    lgCatalan: SetLanguageCatalan;
    lgChinese_950: SetLanguageChinese_950;
    lgDanish: SetLanguageDanish;
    lgDutch: SetLanguageDutch;
    lgEnglish: SetLanguageEnglish;
    lgFrench: SetLanguageFrench;
    lgGerman: SetLanguageGerman;
    lgIndonesian: SetLanguageIndonesian;
    lgItalian: SetLanguageItalian;
    lgJavanese: SetLanguageJavanese;
    lgPolish_CP1250: SetLanguagePolish_CP1250;         
    lgPolish_ISO_8859_2: SetLanguagePolish_ISO_8859_2; 
    lgRussian_1251: SetLanguageRussian_1251;
    lgRussian_866: SetLanguageRussian_866;
    lgRussian_koi8: SetLanguageRussian_koi8;
    lgSlovak: SetLanguageSlovak;
    lgSpanish: SetLanguageSpanish;
    lgSwedish: SetLanguageSwedish;
    
    lgHungarian_1250: SetLanguageHungarian_1250;

  end;
end;

{
  $Log$
  Revision 1.20  2005/05/10 07:28:27  kambi
  * Updates to Brasilian translation from Alexandre da Silva

  Revision 1.19  2005/05/07 19:03:41  kambi
  * Displaying hint directives in html and latex output

  Revision 1.18  2005/05/06 23:30:49  kambi
  * Updates to Brasilian translation from Alexandre da Silva

  Revision 1.17  2005/04/17 03:46:53  kambi
  * typo fix: Frensh -> French

  Revision 1.16  2005/04/14 10:21:51  kambi
  * Specified default values for many properties. This means that code is better self-documenting,
    and also component is better shown in object inspector and saved to dfm/xfm/lfm files.

  Revision 1.15  2005/04/08 02:51:47  ccodere
    + corrected french language

  Revision 1.14  2005/04/06 13:34:36  kambi
  * Added "and Records" to FTranslation[trCio], FTranslation[trHeadlineCio],
    FTranslation[trSummaryCio] for English and two Polish versions
  * trGvUses/Classes in Polish translation and minor fixes for Polish translation,
    removed "//GSk" (this was a leftover from patch of Grzegorz Skoczylas,
    but these comments belonged to cvs logs, not to clutter source code)

  Revision 1.13  2005/03/30 10:18:59  johill
  add charsets to languages

  Revision 1.12  2004/08/10 20:43:26  twm
  patches from Ricardo Pardini:
  * Include graphics into htmlhelp
  * Check whether --link-gv-* are actually given before creating the links
  * Brazilian Portuguese translations

  Revision 1.11  2004/08/08 14:56:33  twm
  Bugfixes:
  * In several places the return value of CreateStream wasn't checked leading a access violations if a file could not be created (e.g. if the output directory didn't exist)
  * Missing field declarations for FLinkGraphVizUses and FLinkGraphVizClasses
  * missing declaration and translations for trGvUses and trGvClasses
  * Main program did not handle all exceptions (e.g. EAbort)

  Revision 1.10  2004/07/16 16:34:16  johill
  some code cleanup, fixes from Pierre Woestyn

  Revision 1.9  2004/06/20 18:36:26  johill
  Changes from Grzegorz Skoczylas: character set handling + updated Polish
  translation

  Revision 1.8  2004/06/13 17:58:07  ccodere
    + added hungarian translation from Jonas (thanks!)

}
end.
