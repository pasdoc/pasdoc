  {$IFDEF LINUX}
    {$IF RTLVersion = 14.0}
      {$DEFINE KYLIX_1}
      {$DEFINE KYLIX_1_UP}
    {$IFEND}
    {$IF RTLVersion = 14.2}
      {$DEFINE KYLIX_2}
      {$DEFINE KYLIX_1_UP}
      {$DEFINE KYLIX_2_UP}
    {$IFEND}
    {$IF RTLVersion = 14.5}
      {$DEFINE KYLIX_3}
      {$DEFINE KYLIX_1_UP}
      {$DEFINE KYLIX_2_UP}
      {$DEFINE KYLIX_3_UP}
    {$IFEND}
    {$DEFINE PPC_KYLIX}
  {$ENDIF}
