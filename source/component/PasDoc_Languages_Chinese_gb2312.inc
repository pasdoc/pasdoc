{ Simplified Chinese (Codepage 936, GB2312) Translation updated by Liu Da <xmacmail AT gmail.com>
  Based on translation made by Liu Chuanjun <1000copy AT gmail.com> ( Thank you )
  Thanks to the authors and all contributors of Pasdoc. :-D
}

{$IFDEF old}
procedure TPasDocLanguages.SetLanguageChinese_gb2312;
begin
  FTranslation[trAuthor] := '作者';
  FTranslation[trAuthors] := '作者';
  FTranslation[trAutomated] := 'Automated';
  FTranslation[trCio] := '类、接口、对象和记录';
  FTranslation[trClass] := '类';
  FTranslation[trClasses] := '类';
  FTranslation[trClassHierarchy] := '类继承';
  FTranslation[trConstants] := '常量';
  FTranslation[trCreated] := '创建时间';
  FTranslation[trDeclaration] := '声明';
  FTranslation[trDescription] := '描述';
  FTranslation[trParameters] := '参数列表';
  FTranslation[trReturns] := '返回值';
  FTranslation[trExceptions] := '异常';
  FTranslation[trExceptionsRaised] := '抛出的异常';
  FTranslation[trEnum] := '枚举';
  FTranslation[trDispInterface] := 'DispInterface';
  FTranslation[trFields] := '字段';
  FTranslation[trFunctionsAndProcedures] := '函数与过程';
  FTranslation[trHelp] := '帮助';
  FTranslation[trHierarchy] := '继承';
  FTranslation[trIdentifiers] := '标识符';
  FTranslation[trInterface] := '接口';
  FTranslation[trLegend] := '图例';
  FTranslation[trMarker] := '标记';
  FTranslation[trVisibility] := '可见性';
  FTranslation[trMethods] := '方法';
  FTranslation[trLastModified] := '最近修改';
  FTranslation[trName] := '名字';
  FTranslation[trNone] := '无';
  FTranslation[trObject] := '对象';
  FTranslation[trObjects] := '对象';
  FTranslation[trOverview] := '概要';
  FTranslation[trPrivate] := '私有';
  FTranslation[trStrictPrivate] := '严格私有';
  FTranslation[trProperties] := '属性';
  FTranslation[trProtected] := '保护';
  FTranslation[trStrictProtected] := '严格保护';
  FTranslation[trPublic] := '公共';
  FTranslation[trImplicit] := '内含';
  FTranslation[trPublished] := '发布';
  FTranslation[trType] := '数据类型';
  FTranslation[trTypes] := '数据类型';
  FTranslation[trUnit] := '单元';
  FTranslation[trUnits] := '单元';
  FTranslation[trVariables] := '变量';
  FTranslation[trGvUses] := '单元依赖图';
  FTranslation[trGvClasses] := '类继承图';
  FTranslation[trHeadlineCio] := '全部类、接口、对象和记录';
  FTranslation[trHeadlineConstants] := '全部常量';
  FTranslation[trHeadlineFunctionsAndProcedures] := '全部函数';
  FTranslation[trHeadlineIdentifiers] := '全部标识符';
  FTranslation[trHeadlineTypes] := '全部数据类型';
  FTranslation[trHeadlineUnits] := '全部单元';
  FTranslation[trHeadlineVariables] := '全部变量';
  FTranslation[trSummaryCio] := '类、接口、对象和记录的摘要';
  FTranslation[trWarningOverwrite] :=
    '警告：不要编辑本文件 - 这是一个自动生成的文件，而且它很可能被覆盖掉。';
  FTranslation[trWarning] := '警告';
  FTranslation[trGeneratedBy] := '由';
  FTranslation[trOnDateTime] := '生成于';
  FTranslation[trDeprecated] := '这个符号已经过期';
  FTranslation[trPlatformSpecific] := '这个符号仅仅用于特定平台';
  FTranslation[trLibrarySpecific] := '这个符号仅仅用于特定的库';
  FTranslation[trIntroduction] := '前言';
  FTranslation[trConclusion] := '后记';
  FTranslation[trSearch] := '搜索';
  FTranslation[trSeeAlso] := '参见';
  FTranslation[trValues] := '值';
  FTranslation[trNoCIOs] := '本单元没有包括任何类、接口、对象和记录';
  FTranslation[trNoCIOsForHierarchy] := '本单元没有包括任何类、接口和对象';
  FTranslation[trNoTypes] := '本单元没有包括任何数据类型';
  FTranslation[trNoVariables] := '本单元没有任何变量';
  FTranslation[trNoConstants] := '本单元没有任何常量';
  FTranslation[trNoFunctions] := '本单元没有任何函数和过程';
  FTranslation[trNoIdentifiers] := '本单元没有任何标识符';
  FTranslation[trProgram] := '程序';
  FTranslation[trLibrary] := '库';
end;
{$ELSE}
const
  aChinese_gb2312: RTransTable = (
    {trNoTrans} '<what?>', //no ID assigned, so far
    {trLanguage} 'Chinese_gb2312',
  //map
    {trUnits} '单元',
    {trClassHierarchy} '类继承',
    {trCio} '类、接口、对象和记录',
    {trIdentifiers} '标识符',
    {trGvUses} '单元依赖关系图',
    {trGvClasses} '类继承关系图',
  //tables and members
    {trClasses} '类',
      {trClass} '类',
      {trDispInterface} '调度接口',
      {trInterface} '接口',
    {trObjects} '对象',
      {trObject} '对象',
      {trRecord} '记录',
        {trHierarchy} '层次结构',
        {trFields} '字段',
        {trMethods} '方法',
        {trProperties} '属性',
    {trLibrary} '库',
    {trPackage} '包',
    {trProgram} '程序',
    {trUnit} '单元',
      {trUses} '引用',
      {trConstants} '常量',
      {trFunctionsAndProcedures} '函数与过程',
      {trTypes} '类型',
        {trType} '类型',
      {trVariables} '变量',
      {trAuthors} '作者',
        {trAuthor} '作者',
      {trCreated} '创建于',
      {trLastModified} '最后更新',
    {trSubroutine} '子程序',
      {trParameters} '参数',
      {trReturns} '返回值',
      {trExceptionsRaised} '抛出的异常',
    {trExceptions} '异常',
      {trException} '异常',
    {trEnum} '枚举',
  //visibilities
    {trVisibility} '可见性',
      {trPrivate} '私有',
      {trStrictPrivate} '严格私有',
      {trProtected} '保护',
      {trStrictProtected} '严格保护',
      {trPublic} '公有',
      {trPublished} '发布',
      {trAutomated} '自动',
      {trImplicit} '隐式',
  //hints
    {trDeprecated} '这个符号是不建议使用的',
    {trPlatformSpecific} '这个符号仅用于特定的平台',
    {trLibrarySpecific} '这个符号仅用于特定的库',
  //headings
    {trOverview} '综述',
    {trIntroduction} '前言',
    {trConclusion} '后记',
    {trHeadlineCio} '所有的类、接口、对象和记录',
    {trHeadlineConstants} '所有常量',
    {trHeadlineFunctionsAndProcedures} '所有函数和过程',
    {trHeadlineIdentifiers} '所有标识符',
    {trHeadlineTypes} '所有类型',
    {trHeadlineUnits} '所有单元',
    {trHeadlineVariables} '所有变量',
    {trSummaryCio} '类、接口、对象和记录的摘要',
  //column headings
    {trDeclaration} '声明',
    {trDescription} '描述',
    {trDescriptions} '描述',
    {trName} '名称',
    {trValues} '值',
  //empty
    {trNone} '无',
    {trNoCIOs} '不包含任何类、接口、对象和记录的单元.',
    {trNoCIOsForHierarchy} '不包含任何类、接口、对象的单元.',
    {trNoTypes} '不包含任何类型的单元.',
    {trNoVariables} '不包含任何变量的单元.',
    {trNoConstants} '不包含任何常量的单元.',
    {trNoFunctions} '不包含任何函数与过程的单元.',
    {trNoIdentifiers} '不包含任何标识符的单元.',
  //misc
    {trHelp} '帮助',
    {trLegend} '图例',
    {trMarker} '标记',
    {trWarningOverwrite} '警告：不要编辑此文件 - 这是一个自动生成的文件，而且它很可能被覆盖掉',
    {trWarning} '警告',
    {trGeneratedBy} '由',
    {trOnDateTime} '生成于',
    {trSearch} '搜索',
    {trSeeAlso} '参见',
    ''  //dummy
  );

{$ENDIF}



