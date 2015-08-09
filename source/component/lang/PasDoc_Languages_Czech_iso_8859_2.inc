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
    {trClassHierarchy} 'Hierarchie tøíd',
    {trCio} 'Tøídy, rozhraní a objekty',
    {trInternalCR} strToDo, // 'Internal Classes and Records',
    {trInternalTypes} strToDo, // 'Internal Types',
    {trIdentifiers} 'Identifikátory',
    {trGvUses} 'Graf závislostí unit',
    {trGvClasses} 'Graf závislostí tøíd',
  //tables and members
    {trClasses} 'Tøídy',
      {trClass} 'Tøída',
      {trDispInterface} 'DispInterface',
      {trInterface} 'Rozhraní',
    {trObjects} 'Objekty',
      {trObject} 'Objekt',
      {trRecord} strToDo,
        {trHierarchy} 'Hierarchie',
        {trFields} 'Polo¾ky',
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
      {trVariables} 'Promìnné',
      {trAuthors} 'Autoøi',
        {trAuthor} 'Autor',
      {trCreated} 'Vytvoøeno',
      {trLastModified} 'Poslední zmìna',
    {trSubroutine} strToDo,
      {trParameters} 'Parametery',
      {trReturns} 'Vrací',
      {trExceptionsRaised} 'Vyhazuje vyjímku',
    {trExceptions} 'Vyjímky',
      {trExceptions} strToDo,
    {trEnum} 'Výètové typy',
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
    {trPlatformSpecific} 'tato konstrukce je závislá na platformì',
    {trLibrarySpecific} 'tato konstrukce je závislá na konkrétní knihovnì',
  //headings
    {trOverview} 'Pøehled',
    {trIntroduction} 'Úvod',
    {trConclusion} 'Závìr',
    {trEnclosingClass} strToDo, //'Enclosing Class',
    {trHeadlineCio} 'V¹echny tøídy, rozhraní a objekty',
    {trHeadlineConstants} 'Seznam konstant',
    {trHeadlineFunctionsAndProcedures} 'Seznam funkcí a procedur',
    {trHeadlineIdentifiers} 'Seznam identifikátorù',
    {trHeadlineTypes} 'Seznam typù',
    {trHeadlineUnits} 'Seznam unit',
    {trHeadlineVariables} 'Seznam promìnných',
    {trSummaryCio} 'Seznam tøíd, rozhraní a objektù',
  //column headings
    {trDeclaration} 'Deklarace',
    {trDescription} 'Popis',
    {trDescriptions} strToDo,
    {trName} 'Název',
    {trValues} 'Hodnoty',
  //empty
    {trNone} 'Nic',
    {trNoCIOs} 'Unity neobsahují ¾ádné tøídy, rozhraní, objekty nebo recordy.',
    {trNoCIOsForHierarchy} 'Unity neobsahují ¾ádné tøídy, rozhraní nebo objekty.',
    {trNoTypes} 'Unity neobsahují ¾ádné typy.',
    {trNoVariables} 'Unity neobsahují ¾ádné promìnné.',
    {trNoConstants} 'Unity neobsahují ¾ádné konstanty.',
    {trNoFunctions} 'Unity neobsahují ¾ádné funkce nebo procedury.',
    {trNoIdentifiers} 'Unity neobsahují ¾ádné identifikátory.',
  //misc
    {trHelp} 'Nápovìda',
    {trLegend} 'Legenda',
    {trMarker} 'Znaèka',
    {trWarningOverwrite} 'Varování: Tento soubor není uèený k editaci. Byl automaticky vygenerován a mù¾e být opìt pøepsán.',
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
