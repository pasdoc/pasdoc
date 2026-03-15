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
    {trClassHierarchy} 'Hierarchie t��d',
    {trCio} 'T��dy, rozhran� a objekty',
    {trNestedCR} strToDo, // 'Nested Classes and Records',
    {trNestedTypes} strToDo, // 'Nested Types',
    {trIdentifiers} 'Identifik�tory',
    {trGvUses} 'Graf z�vislost� unit',
    {trGvClasses} 'Graf z�vislost� t��d',
  //tables and members
    {trClasses} 'T��dy',
      {trClass} 'T��da',
      {trObjcClass} strToDo,
      {trDispInterface} 'DispInterface',
      {trInterface} 'Rozhran�',
    {trObjects} 'Objekty',
      {trObject} 'Objekt',
      {trRecord} strToDo,
      {trPacked} strToDo, //'Packed',
        {trHierarchy} 'Hierarchie',
        {trFields} 'Polo�ky',
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
      {trVariables} 'Prom�nn�',
      {trAuthors} 'Auto�i',
        {trAuthor} 'Autor',
      {trCreated} 'Vytvo�eno',
      {trLastModified} 'Posledn� zm�na',
    {trSubroutine} strToDo,
      {trParameters} 'Parametery',
      {trReturns} 'Vrac�',
      {trExceptionsRaised} 'Vyhazuje vyj�mku',
    {trExceptions} 'Vyj�mky',
      {trExceptions} strToDo,
    {trEnum} 'V��tov� typy',
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
    {trDeprecated} 'tato konstrukce je zastaral� (deprecated)',
    {trPlatformSpecific} 'tato konstrukce je z�visl� na platform�',
    {trLibrarySpecific} 'tato konstrukce je z�visl� na konkr�tn� knihovn�',
    {trExperimental} strToDo, //'this symbol is experimental',
    {trUnimplemented} strToDo, //'this symbol is unimplemented',
  //headings
    {trOverview} 'P�ehled',
    {trIntroduction} '�vod',
    {trConclusion} 'Z�v�r',
    {trAdditionalFile} strToDo, //'additional file',
    {trEnclosingClass} strToDo, //'Enclosing Class',
    {trHeadlineCio} 'V�echny t��dy, rozhran� a objekty',
    {trHeadlineConstants} 'Seznam konstant',
    {trHeadlineFunctionsAndProcedures} 'Seznam funkc� a procedur',
    {trHeadlineIdentifiers} 'Seznam identifik�tor�',
    {trHeadlineTypes} 'Seznam typ�',
    {trHeadlineUnits} 'Seznam unit',
    {trHeadlineVariables} 'Seznam prom�nn�ch',
    {trSummaryCio} 'Seznam t��d, rozhran� a objekt�',
  //column headings
    {trDeclaration} 'Deklarace',
    {trDescription} 'Popis',
    {trDescriptions} strToDo,
    {trName} 'N�zev',
    {trValues} 'Hodnoty',
  //tags with inbuilt heading
    {trWarningTag} strToDo,
    {trNoteTag} strToDo,
  //empty
    {trNone} 'Nic',
    {trNoCIOs} 'Unity neobsahuj� ��dn� t��dy, rozhran�, objekty nebo recordy.',
    {trNoCIOsForHierarchy} 'Unity neobsahuj� ��dn� t��dy, rozhran� nebo objekty.',
    {trNoTypes} 'Unity neobsahuj� ��dn� typy.',
    {trNoVariables} 'Unity neobsahuj� ��dn� prom�nn�.',
    {trNoConstants} 'Unity neobsahuj� ��dn� konstanty.',
    {trNoFunctions} 'Unity neobsahuj� ��dn� funkce nebo procedury.',
    {trNoIdentifiers} 'Unity neobsahuj� ��dn� identifik�tory.',
  //misc
    {trHelp} 'N�pov�da',
    {trLegend} 'Legenda',
    {trMarker} 'Zna�ka',
    {trWarningOverwrite} 'Varov�n�: Tento soubor nen� u�en� k editaci. Byl automaticky vygenerov�n a m��e b�t op�t p�eps�n.',
    {trWarning} 'Varov�n�',
    {trGeneratedBy} 'Vygenerov�no pomoc�',
    {trGeneratedOn} strToDo, //'Generated on'
    {trOnDateTime} 'v',
    {trSearch} 'Hledat',
    {trSeeAlso} 'Viz tak�',
    {trNested} strToDo, //'nested',
    {trAttributes} 'Atributy',
    {trSourcePosition} strToDo, //'Source: %s (line %d).',
    {trShowAdditionalMembers} strToDo, //'Show Additional Members',
    '' //dummy
  );
