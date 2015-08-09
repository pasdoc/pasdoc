(* New language template. To add a new language or encoding:

  1) Copy this file (PasDoc_Languages_Template_New_Language.inc)
     into more approprate filename.

  2) Add your language to PasDoc_Languages.pas array. See other languages
     for examples. You want to
     - add a new value to TLanguageID type,
     - define a const named aXxx,
     - and add a new row to LANGUAGE_ARRAY table describing your language.

  3) Then, of course, do the actual translation :)
     Replace all occurences of strToDo by
     - your translation of the text in the comment,
     - or rename them into strKeep for all strings that need no translation,
     - or leave the strToDo in place, if you don't know the translation.

     For "normal" PasDoc builds, both strToDo and strKeep are equal
     (in fact, they are just empty strings) and mean
     "use the English version". The distinction between strToDo and strKeep
     is mainly for you, and other devs/translators, so that we know
     when you really want to show English version and when you just can't
     decide what is the proper translation.
*)
RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} strToDo, //<<<<<< replace with the name of the new language
  //map
    {trUnits} strToDo, //'Units',
    {trClassHierarchy} strToDo, //'Class Hierarchy',
    {trCio} strToDo, //'Classes, Interfaces, Objects and Records',
    {trInternalCR} strToDo, // 'Internal Classes and Records',
    {trInternalTypes} strToDo, // 'Internal Types',
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
    {trEnclosingClass} strToDo, //'Enclosing Class',
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
    {trGeneratedOn} strToDo, //'Generated on'
    {trOnDateTime} strToDo, //'on',
    {trSearch} strToDo, //'Search',
    {trSeeAlso} strToDo, //'See also',
    {trInternal} strToDo, //'internal',
    {trAttributes} strToDo, //'Attributes',
    ''  //dummy
  );
