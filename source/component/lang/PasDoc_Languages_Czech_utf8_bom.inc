{
  This is translation for Czech language.
  Pasted into separate file, to minimize chance of messing character codes
  (and also, to ease iconv call).

  PasDoc_Languages_Czech_ISO_8859_2.inc contains ISO-8859-2 version.

  PasDoc_Languages_Czech_CP1250.inc contans Windows CP1250 version,
  which should be automatically generated from *_ISO_8859_2.inc using
  iconv ("make PasDoc_Languages_Czech_CP1250.inc" in this dir takes
  care of that).
}

RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Czech',
  //map
    {trUnits} 'Unity',
    {trClassHierarchy} 'Hierarchie tříd',
    {trCio} 'Třídy, rozhraní a objekty',
    {trInternalCR} strToDo, // 'Internal Classes and Records',
    {trInternalTypes} strToDo, // 'Internal Types',
    {trIdentifiers} 'Identifikátory',
    {trGvUses} 'Graf závislostí unit',
    {trGvClasses} 'Graf závislostí tříd',
  //tables and members
    {trClasses} 'Třídy',
      {trClass} 'Třída',
      {trDispInterface} 'DispInterface',
      {trInterface} 'Rozhraní',
    {trObjects} 'Objekty',
      {trObject} 'Objekt',
      {trRecord} strToDo,
        {trHierarchy} 'Hierarchie',
        {trFields} 'Položky',
        {trMethods} 'Metody',
        {trProperties} 'Vlastnosti',
    {trLibrary} 'Knihovna',
    {trPackage} strToDo,
    {trProgram} 'Aplikace',
    {trUnit} 'Unita',
      {trUses} strToDo,
      {trConstants} 'Konstanty',
      {trFunctionsAndProcedures} 'Funkce a procedury',
      {trTypes} 'Typy',
        {trType} 'Typ',
      {trVariables} 'Proměnné',
      {trAuthors} 'Autoři',
        {trAuthor} 'Autor',
      {trCreated} 'Vytvořeno',
      {trLastModified} 'Poslední změna',
    {trSubroutine} strToDo,
      {trParameters} 'Parametery',
      {trReturns} 'Vrací',
      {trExceptionsRaised} 'Vyhazuje vyjímku',
    {trExceptions} 'Vyjímky',
      {trExceptions} strToDo,
    {trEnum} 'Výčtové typy',
  //visibilities
    {trVisibility} 'Viditelnost',
      {trPrivate} 'Private',
      {trStrictPrivate} 'Strict Private',
      {trProtected} 'Protected',
      {trStrictProtected} 'Strict Protected',
      {trPublic} 'Public',
      {trPublished} 'Published',
      {trAutomated} 'Automated',
      {trImplicit} 'Implicit',
  //hints
    {trDeprecated} 'tato konstrukce je zastaralá (deprecated)',
    {trPlatformSpecific} 'tato konstrukce je závislá na platformě',
    {trLibrarySpecific} 'tato konstrukce je závislá na konkrétní knihovně',
  //headings
    {trOverview} 'Přehled',
    {trIntroduction} 'Úvod',
    {trConclusion} 'Závěr',
    {trEnclosingClass} strToDo, //'Enclosing Class',
    {trHeadlineCio} 'Všechny třídy, rozhraní a objekty',
    {trHeadlineConstants} 'Seznam konstant',
    {trHeadlineFunctionsAndProcedures} 'Seznam funkcí a procedur',
    {trHeadlineIdentifiers} 'Seznam identifikátorů',
    {trHeadlineTypes} 'Seznam typů',
    {trHeadlineUnits} 'Seznam unit',
    {trHeadlineVariables} 'Seznam proměnných',
    {trSummaryCio} 'Seznam tříd, rozhraní a objektů',
  //column headings
    {trDeclaration} 'Deklarace',
    {trDescription} 'Popis',
    {trDescriptions} strToDo,
    {trName} 'Název',
    {trValues} 'Hodnoty',
  //empty
    {trNone} 'Nic',
    {trNoCIOs} 'Unity neobsahují žádné třídy, rozhraní, objekty nebo recordy.',
    {trNoCIOsForHierarchy} 'Unity neobsahují žádné třídy, rozhraní nebo objekty.',
    {trNoTypes} 'Unity neobsahují žádné typy.',
    {trNoVariables} 'Unity neobsahují žádné proměnné.',
    {trNoConstants} 'Unity neobsahují žádné konstanty.',
    {trNoFunctions} 'Unity neobsahují žádné funkce nebo procedury.',
    {trNoIdentifiers} 'Unity neobsahují žádné identifikátory.',
  //misc
    {trHelp} 'Nápověda',
    {trLegend} 'Legenda',
    {trMarker} 'Značka',
    {trWarningOverwrite} 'Varování: Tento soubor není učený k editaci. Byl automaticky vygenerován a může být opět přepsán.',
    {trWarning} 'Varování',
    {trGeneratedBy} 'Vygenerováno pomocí',
    {trGeneratedOn} strToDo, //'Generated on'
    {trOnDateTime} 'v',
    {trSearch} 'Hledat',
    {trSeeAlso} 'Viz také',
    {trInternal} strToDo, //'internal',
    {trAttributes} 'Atributy',
    '' //dummy
  );
