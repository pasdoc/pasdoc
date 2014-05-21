object frmHelpGenerator: TfrmHelpGenerator
  Left = 691
  Top = 277
  Width = 674
  Height = 470
  HelpType = htKeyword
  HelpKeyword = 'PasDocGui'
  Caption = 'pasdoc gui'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 165
    Height = 416
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'PanelLeft'
    TabOrder = 0
    object lbNavigation: TListBox
      Left = 0
      Top = 38
      Width = 165
      Height = 378
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbNavigationClick
    end
    object PanelLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 165
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object ButtonGenerate: TButton
        Left = 10
        Top = 7
        Width = 147
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = MenuGenerateRunClick
      end
    end
  end
  object NotebookMain: TNotebook
    Left = 165
    Top = 0
    Width = 501
    Height = 416
    Align = alClient
    PageIndex = 7
    TabOrder = 1
    object pageOptions: TPage
      Left = 0
      Top = 0
      Caption = 'Options'
      DesignSize = (
        501
        416)
      object Label2: TLabel
        Left = 10
        Top = 14
        Width = 27
        Height = 13
        HelpType = htKeyword
        Caption = 'Title :'
        Color = clBtnFace
        FocusControl = edTitle
        ParentColor = False
      end
      object Label6: TLabel
        Left = 10
        Top = 102
        Width = 68
        Height = 13
        HelpType = htKeyword
        Caption = 'Output Type :'
        Color = clBtnFace
        FocusControl = comboGenerateFormat
        ParentColor = False
      end
      object Label11: TLabel
        Left = 10
        Top = 58
        Width = 71
        Height = 13
        HelpType = htKeyword
        Caption = 'Project Name :'
        Color = clBtnFace
        FocusControl = edProjectName
        ParentColor = False
      end
      object Label19: TLabel
        Left = 10
        Top = 146
        Width = 218
        Height = 13
        HelpType = htKeyword
        Caption = 'Language used in generated documentation :'
        Color = clBtnFace
        FocusControl = comboLanguages
        ParentColor = False
      end
      object Label3: TLabel
        Left = 10
        Top = 194
        Width = 375
        Height = 13
        HelpType = htKeyword
        Caption = 
          'Output directory (This is the directory where the web pages will' +
          ' be created.) :'
        Color = clBtnFace
        ParentColor = False
      end
      object CheckAutoAbstract: TCheckBox
        Left = 10
        Top = 294
        Width = 605
        Height = 24
        Hint = 
          'If this is checked, the 1st sentence of each description'#10'will be' +
          ' treated as the abstract of that description'#10'(unless you overrid' +
          'e it by using the @abstract tag).'
        HelpType = htKeyword
        HelpKeyword = 'AutoAbstractOption'
        Caption = 
          'Automatically deduce @abstract description from the 1st sentence' +
          ' of description'
        TabOrder = 8
      end
      object CheckUseTipueSearch: TCheckBox
        Left = 10
        Top = 270
        Width = 316
        Height = 24
        Hint = 
          'Check this to get working "Search" button in your HTML documenta' +
          'tion.'
        HelpType = htKeyword
        HelpKeyword = 'UseTipueSearchOption'
        Caption = 'Use tipue search engine in HTML output'
        TabOrder = 7
      end
      object edTitle: TEdit
        Left = 10
        Top = 30
        Width = 177
        Height = 21
        Hint = 
          'Title for your documentation. In HTML output, this appears in th' +
          'e web browser caption.'
        HelpType = htKeyword
        HelpKeyword = 'DocumentationTitle'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = SomethingChanged
      end
      object comboGenerateFormat: TComboBox
        Left = 10
        Top = 116
        Width = 176
        Height = 21
        HelpType = htKeyword
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = comboGenerateFormatChange
        Items.Strings = (
          'HTML'
          'HTML Help Workshop'
          'LaTeX'
          'LaTeX for latex2rtf')
      end
      object edOutput: TEdit
        Left = 10
        Top = 211
        Width = 436
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = SomethingChanged
      end
      object ButtonOutPutPathName: TButton
        Left = 451
        Top = 211
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '..'
        TabOrder = 5
        OnClick = LocationsButtonsClick
      end
      object edProjectName: TEdit
        Left = 10
        Top = 74
        Width = 176
        Height = 21
        Hint = 
          'The project name is used to specify the main part of '#10'the output' +
          ' file name for HtmlHelp or LaTeX output.'
        HelpType = htKeyword
        HelpKeyword = 'NameOption'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = SomethingChanged
      end
      object comboLanguages: TComboBox
        Left = 10
        Top = 162
        Width = 237
        Height = 21
        HelpType = htKeyword
        HelpKeyword = 'OutputLanguage'
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
        OnChange = comboLanguagesChange
      end
      object CheckAutoLink: TCheckBox
        Left = 10
        Top = 318
        Width = 520
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'AutoLinkOption'
        Caption = 
          'Automatically turn identifiers into links, without the need for ' +
          '@link tag'
        TabOrder = 9
      end
      object CheckHandleMacros: TCheckBox
        Left = 10
        Top = 342
        Width = 349
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'NoMacroOption'
        Caption = 'Recognize FPC macros syntax when parsing'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object CheckStoreRelativePaths: TCheckBox
        Left = 10
        Top = 374
        Width = 294
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'PasDocGui/StoreRelativePaths'
        Caption = 'Store only relative paths in project file'
        Checked = True
        State = cbChecked
        TabOrder = 11
      end
      object CheckWriteUsesList: TCheckBox
        Left = 10
        Top = 246
        Width = 167
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'WriteUsesList'
        Caption = 'Show units uses list'
        TabOrder = 6
      end
    end
    object pageSourceFiles: TPage
      Left = 0
      Top = 0
      Caption = 'Source Files'
      object PanelSourceFilesTop: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        DesignSize = (
          501
          416)
        object Label8: TLabel
          Left = 10
          Top = 10
          Width = 481
          Height = 39
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'Add the filenames of source files you wish to include in your pr' +
            'oject. The directories for each file will be automatically added' +
            ' to the "Include" directories if you use the "Browse" button to ' +
            'add the source files.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object btnBrowseSourceFiles: TButton
          Left = 10
          Top = 54
          Width = 481
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Browse'
          TabOrder = 0
          OnClick = btnBrowseSourceFilesClick
        end
        object memoFiles: TMemo
          Left = 10
          Top = 86
          Width = 481
          Height = 320
          HelpType = htKeyword
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnChange = SomethingChanged
        end
      end
    end
    object pageIncludeDirectories: TPage
      Left = 0
      Top = 0
      Caption = 'Include Directories'
      object PanelIncludeDirectoriesTop: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        DesignSize = (
          501
          416)
        object Label9: TLabel
          Left = 10
          Top = 10
          Width = 481
          Height = 26
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'The directories where PasDoc can find include files.'#10'(If you use' +
            ' $I, $INCLUDE compiler directives.)'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object btnBrowseIncludeDirectory: TButton
          Left = 10
          Top = 41
          Width = 481
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Browse'
          TabOrder = 0
          OnClick = btnBrowseIncludeDirectoryClick
        end
        object memoIncludeDirectories: TMemo
          Left = 10
          Top = 72
          Width = 481
          Height = 334
          HelpType = htKeyword
          HelpKeyword = 'IncludeInSearchPath'
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnChange = SomethingChanged
        end
      end
    end
    object pageDefines: TPage
      Left = 0
      Top = 0
      Caption = 'Defines'
      object PanelDefinesTop: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        object Label12: TLabel
          Left = 10
          Top = 10
          Width = 472
          Height = 26
          Align = alTop
          Caption = 
            'Put here any symbols that you want to have defined at the start,' +
            ' just as if they would be defined by $DEFINE at the beginning of' +
            ' each unit.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label4: TLabel
          Left = 10
          Top = 36
          Width = 465
          Height = 39
          Align = alTop
          Caption = 
            'Note that your compiler may define some symbols by default (for ' +
            'example, "FPC" by FreePascal, "VER150" by Delphi 7, target OS an' +
            'd architecture like "UNIX", "MSWINDOWS" etc.) --- you may want t' +
            'o define some of these for pasdoc too.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object MemoDefines: TMemo
          Left = 10
          Top = 75
          Width = 481
          Height = 331
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object PageVisibleMembers: TPage
      Left = 0
      Top = 0
      Caption = 'Visible members'
      object LabelImplicitVisibility: TLabel
        Left = 8
        Top = 211
        Width = 3
        Height = 13
        Color = clBtnFace
        ParentColor = False
      end
      object PanelVisibleMembers: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        object LabelVisibleMembers: TLabel
          Left = 10
          Top = 10
          Width = 481
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'Structures (classes etc.) members (properties, methods, events, ' +
            'fields) to show in documentation :'
          Color = clBtnFace
          FocusControl = CheckListVisibleMembers
          ParentColor = False
          WordWrap = True
        end
        object RadioImplicitVisibility: TRadioGroup
          Left = 10
          Top = 139
          Width = 481
          Height = 73
          HelpType = htKeyword
          HelpKeyword = 'ImplicitVisibilityOption'
          Align = alTop
          Caption = 'Default visibility of members'
          ItemIndex = 0
          Items.Strings = (
            
              '"Public", unless the class is declared within {$M+} state, then ' +
              'it'#39's "published"'
            'Always "published"'
            'Always "implicit"')
          TabOrder = 0
          OnClick = SomethingChanged
        end
        object CheckListVisibleMembers: TCheckListBox
          Left = 10
          Top = 23
          Width = 481
          Height = 116
          HelpType = htKeyword
          HelpKeyword = 'IncludeByVisibility'
          Align = alTop
          ItemHeight = 13
          Items.Strings = (
            'Published'
            'Public'
            'Protected'
            'Private'
            'Automated'
            'Implicit')
          TabOrder = 1
          OnClick = CheckListVisibleMembersClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Sort'
      object PanelSort: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        object Label1: TLabel
          Left = 10
          Top = 10
          Width = 481
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = 'Items to sort alphabetically'
          Color = clBtnFace
          FocusControl = clbSorting
          ParentColor = False
        end
        object clbSorting: TCheckListBox
          Left = 10
          Top = 23
          Width = 481
          Height = 146
          Hint = 
            'Which items will be sorted alphabetically '#10'and which will be dis' +
            'played in their declared order.'
          HelpType = htKeyword
          HelpKeyword = 'SortOption'
          Align = alTop
          ItemHeight = 13
          Items.Strings = (
            'structures'
            'constants'
            'global functions'
            'types'
            'variables'
            'uses-clauses'
            'record-fields'
            'non-record-fields'
            'methods'
            'properties')
          TabOrder = 0
          OnClick = SomethingChanged
        end
      end
    end
    object pageMarkers: TPage
      Left = 0
      Top = 0
      Caption = 'Markers'
      object PanelMarkers: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 416
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        object Label18: TLabel
          Left = 10
          Top = 97
          Width = 481
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = 'Special comment markers'
          Color = clBtnFace
          ParentColor = False
        end
        object rgCommentMarkers: TRadioGroup
          Left = 10
          Top = 10
          Width = 481
          Height = 87
          HelpType = htKeyword
          HelpKeyword = 'CommentMarker'
          Align = alTop
          Caption = 'Comment marker treatment'
          ItemIndex = 1
          Items.Strings = (
            'Ignore special comment markers'
            'Include all comments but remove special comment markers'
            'Include only comments with special comment markers')
          TabOrder = 0
          OnClick = rgCommentMarkersClick
        end
        object memoCommentMarkers: TMemo
          Left = 10
          Top = 112
          Width = 481
          Height = 294
          HelpType = htKeyword
          HelpKeyword = 'CommentMarker'
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 1
          OnChange = SomethingChanged
        end
      end
    end
    object pageLocations: TPage
      Left = 0
      Top = 0
      Caption = 'CustomFiles'
      DesignSize = (
        501
        416)
      object Label14: TLabel
        Left = 8
        Top = 121
        Width = 75
        Height = 13
        HelpType = htKeyword
        Caption = 'Conclusion file :'
        Color = clBtnFace
        ParentColor = False
      end
      object Label15: TLabel
        Left = 8
        Top = 75
        Width = 83
        Height = 13
        HelpType = htKeyword
        Caption = 'Introduction file :'
        Color = clBtnFace
        ParentColor = False
      end
      object Label16: TLabel
        Left = 8
        Top = 10
        Width = 384
        Height = 13
        HelpType = htKeyword
        Caption = 
          'Use custom CSS file with HTML output (leave empty to use default' +
          ' pasdoc.css) :'
        Color = clBtnFace
        ParentColor = False
      end
      object EditCssFileName: TEdit
        Left = 8
        Top = 25
        Width = 436
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'EditCssFileName'
        OnChange = SomethingChanged
      end
      object EditIntroductionFileName: TEdit
        Left = 8
        Top = 90
        Width = 436
        Height = 21
        Hint = 'Optional file used as an introduction to your project.'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'EditIntroductionFileName'
        OnChange = SomethingChanged
      end
      object EditConclusionFileName: TEdit
        Left = 8
        Top = 136
        Width = 436
        Height = 21
        Hint = 'Optional file used as a conclusion to your project.'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'EditConclusionFileName'
        OnChange = SomethingChanged
      end
      object ButtonIntroFileName: TButton
        Left = 449
        Top = 90
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '..'
        TabOrder = 3
        OnClick = LocationsButtonsClick
      end
      object ButtonConclusionFileName: TButton
        Left = 449
        Top = 136
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '..'
        TabOrder = 5
        OnClick = LocationsButtonsClick
      end
      object ButtonCssFileName: TButton
        Left = 449
        Top = 25
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '..'
        TabOrder = 1
        OnClick = LocationsButtonsClick
      end
    end
    object pageHeadFoot: TPage
      Left = 0
      Top = 0
      Caption = 'Header / Footer'
      object Splitter2: TSplitter
        Left = 0
        Top = 179
        Width = 501
        Height = 9
        Cursor = crVSplit
        Align = alTop
      end
      object PanelHeaderHidden: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 179
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        object LabelHeader: TLabel
          Left = 10
          Top = 10
          Width = 275
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = '&Header (This text will appear at the top of the web page)'
          Color = clBtnFace
          FocusControl = memoHeader
          ParentColor = False
        end
        object memoHeader: TMemo
          Left = 10
          Top = 23
          Width = 481
          Height = 146
          HelpType = htKeyword
          HelpKeyword = 'FileAsHeaderOrFooter'
          Align = alClient
          TabOrder = 0
          WordWrap = False
          OnChange = SomethingChanged
        end
      end
      object PanelFooterHidden: TPanel
        Left = 0
        Top = 188
        Width = 501
        Height = 228
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        object LabelFooter: TLabel
          Left = 10
          Top = 10
          Width = 290
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = '&Footer (This text will appear at the bottom of the web page)'
          Color = clBtnFace
          FocusControl = memoFooter
          ParentColor = False
        end
        object memoFooter: TMemo
          Left = 10
          Top = 23
          Width = 481
          Height = 195
          HelpType = htKeyword
          HelpKeyword = 'FileAsHeaderOrFooter'
          Align = alClient
          TabOrder = 0
          WordWrap = False
          OnChange = SomethingChanged
        end
      end
    end
    object pageLatexOptions: TPage
      Left = 0
      Top = 0
      Caption = 'LaTeX Options'
      object Label24: TLabel
        Left = 12
        Top = 110
        Width = 113
        Height = 13
        Caption = 'LateX graphics package'
        Color = clBtnFace
        FocusControl = comboLatexGraphicsPackage
        ParentColor = False
      end
      object rgLineBreakQuality: TRadioGroup
        Left = 12
        Top = 7
        Width = 95
        Height = 81
        HelpType = htKeyword
        HelpKeyword = 'PasDocGui/LatexLineBreaks'
        Caption = 'Line Breaks'
        ItemIndex = 0
        Items.Strings = (
          'strict'
          'sloppy')
        TabOrder = 1
        OnClick = SomethingChanged
      end
      object comboLatexGraphicsPackage: TComboBox
        Left = 12
        Top = 126
        Width = 148
        Height = 21
        Hint = 
          'If you use graphics in LaTeX, you have to specify '#10'the graphics ' +
          'package in the header for the LaTeX file.'#10'This option allows you' +
          ' to specify which one to use.'
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'None'
        OnChange = SomethingChanged
        Items.Strings = (
          'None'
          'PDF'
          'DVI')
      end
      object PanelLatexHyphenation: TPanel
        Left = 154
        Top = 0
        Width = 347
        Height = 416
        Align = alRight
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 2
        object Label17: TLabel
          Left = 10
          Top = 10
          Width = 322
          Height = 39
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'You can specify how you want words to be hyphenated here. Just e' +
            'nter the word (one per line) with hyphens in the correct places.' +
            ' Only English letters are allowed.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object memoHyphenatedWords: TMemo
          Left = 10
          Top = 49
          Width = 327
          Height = 357
          Align = alClient
          TabOrder = 0
          OnChange = SomethingChanged
        end
      end
    end
    object pageGraphViz: TPage
      Left = 0
      Top = 0
      Caption = 'Graphs'
      object Label22: TLabel
        Left = 14
        Top = 70
        Width = 386
        Height = 13
        Caption = 
          'You will have to generate graphs yourself using the dot program ' +
          'from GraphViz :'
        Color = clBtnFace
        ParentColor = False
      end
      object cbVizGraphUses: TCheckBox
        Left = 10
        Top = 34
        Width = 243
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'GraphVizSupport'
        Caption = 'Generate and use Uses graph'
        TabOrder = 1
      end
      object cbVizGraphClasses: TCheckBox
        Left = 10
        Top = 10
        Width = 263
        Height = 24
        HelpType = htKeyword
        HelpKeyword = 'GraphVizSupport'
        Caption = 'Generate and use Classes graph'
        TabOrder = 0
      end
      object ButtonGraphVizURL: TButton
        Left = 14
        Top = 96
        Width = 188
        Height = 25
        Caption = 'http://www.graphviz.org/'
        TabOrder = 2
        OnClick = ButtonURLClick
      end
    end
    object pageSpellChecking: TPage
      Left = 0
      Top = 0
      Caption = 'Spell Checking'
      object PanelSpellCheckingTop1: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 133
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        object Label20: TLabel
          Left = 10
          Top = 97
          Width = 302
          Height = 26
          HelpType = htKeyword
          Align = alBottom
          Caption = 
            'Enter words that should be ignored when spell-checking below.'#10'On' +
            'e word per line.'
          Color = clBtnFace
          ParentColor = False
        end
        object Label23: TLabel
          Left = 10
          Top = 40
          Width = 343
          Height = 13
          Caption = 
            'GNU Aspell must be installed and available on $PATH for spell ch' +
            'ecking :'
          Color = clBtnFace
          ParentColor = False
        end
        object cbCheckSpelling: TCheckBox
          Left = 10
          Top = 10
          Width = 131
          Height = 24
          HelpType = htKeyword
          HelpKeyword = 'SpellChecking'
          Caption = 'Check Spelling'
          TabOrder = 0
        end
        object ButtonAspellURL: TButton
          Left = 10
          Top = 64
          Width = 218
          Height = 25
          Caption = 'http://aspell.sourceforge.net/'
          TabOrder = 1
          OnClick = ButtonURLClick
        end
      end
      object PanelSpellCheckingBottom: TPanel
        Left = 0
        Top = 133
        Width = 501
        Height = 283
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        object memoSpellCheckingIgnore: TMemo
          Left = 10
          Top = 10
          Width = 481
          Height = 263
          HelpType = htKeyword
          HelpKeyword = 'SpellChecking'
          Align = alClient
          TabOrder = 0
          WordWrap = False
        end
      end
    end
    object pageGenerate: TPage
      Left = 0
      Top = 0
      Caption = 'Generate'
      object PanelGenerateTop: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 103
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        BorderWidth = 10
        FullRepaint = False
        TabOrder = 0
        DesignSize = (
          501
          103)
        object Label10: TLabel
          Left = 10
          Top = 10
          Width = 481
          Height = 26
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'While generating documentation, messages describing what is happ' +
            'ening will appear in the area below.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
        end
        object Label7: TLabel
          Left = 80
          Top = 49
          Width = 134
          Height = 13
          HelpType = htKeyword
          Caption = 'Verbosity level (default is 2)'
          Color = clBtnFace
          ParentColor = False
        end
        object seVerbosity: TSpinEdit
          Left = 10
          Top = 44
          Width = 64
          Height = 22
          Hint = 'The higher the message level, the more messages will be shown.'
          HelpType = htKeyword
          MaxValue = 6
          MinValue = 0
          TabOrder = 0
          Value = 2
          OnChange = SomethingChanged
        end
        object ButtonGenerateDocs: TButton
          Left = 10
          Top = 75
          Width = 481
          Height = 25
          Anchors = [akLeft, akRight]
          Caption = 'Generate documentation'
          TabOrder = 1
          OnClick = ButtonGenerateDocsClick
        end
      end
      object PanelGenerateBottom: TPanel
        Left = 0
        Top = 103
        Width = 501
        Height = 313
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        object memoMessages: TMemo
          Left = 10
          Top = 10
          Width = 481
          Height = 293
          HelpType = htKeyword
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
    object pageEdit: TPage
      Left = 0
      Top = 0
      Caption = 'Display Comments'
      object Splitter1: TSplitter
        Left = 0
        Top = 187
        Width = 501
        Height = 5
        Cursor = crVSplit
        Align = alTop
      end
      object pnlEditCommentInstructions: TPanel
        Left = 0
        Top = 0
        Width = 501
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Click on an item in the tree view to see its comment.'
        FullRepaint = False
        TabOrder = 0
      end
      object PanelDisplayCommentsMid: TPanel
        Left = 0
        Top = 28
        Width = 501
        Height = 159
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        object tvUnits: TTreeView
          Left = 10
          Top = 10
          Width = 481
          Height = 139
          Align = alClient
          Indent = 19
          TabOrder = 0
          OnClick = tvUnitsClick
        end
      end
      object PanelDisplayCommentsBottom: TPanel
        Left = 0
        Top = 192
        Width = 501
        Height = 224
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 2
        object seComment: TMemo
          Left = 10
          Top = 10
          Width = 481
          Height = 204
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Delphi source files *.pas|*.pas|Free Pascal source files *.pp|*.' +
      'pp|All Pascal source files *.pas, *.pp|*.pas;*.pp|All Files *.*|' +
      '*.*'
    FilterIndex = 3
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Title = 'Open existing file'
    Top = 528
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.pds'
    Filter = 'PasDoc GUI Settings (*.pds)|*.pds'
    FilterIndex = 0
    Title = 'Save file as'
    Left = 96
    Top = 528
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.pds'
    Filter = 'PasDoc GUI Settings (*.pds)|*.pds'
    FilterIndex = 0
    Title = 'Open existing file'
    Left = 64
    Top = 528
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 528
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuNew: TMenuItem
        Caption = '&New'
        OnClick = MenuNewClick
      end
      object MenuOpen: TMenuItem
        Caption = '&Open ...'
        OnClick = btnOpenClick
      end
      object MenuSave: TMenuItem
        Caption = 'Save'
        OnClick = MenuSaveClick
      end
      object MenuSaveAs: TMenuItem
        Caption = '&Save as...'
        OnClick = MenuSaveAsClick
      end
      object MenuExit: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object MenuEdit: TMenuItem
      Caption = 'Edit'
      object MenuPreferences: TMenuItem
        Caption = 'Preferences'
        OnClick = MenuPreferencesClick
      end
    end
    object MenuGenerate: TMenuItem
      Caption = 'Generate'
      object MenuGenerateRun: TMenuItem
        Caption = 'Generate documentation'
        OnClick = MenuGenerateRunClick
      end
    end
    object MenuHelp: TMenuItem
      Caption = '&Help'
      object MenuContextHelp: TMenuItem
        Caption = 'Help'
        OnClick = MenuContextHelpClick
      end
      object MenuAbout: TMenuItem
        Caption = '&About'
        OnClick = MenuAboutClick
      end
    end
  end
  object OpenDialog3: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 288
  end
  object PasDoc1: TPasDoc
    ShowVisibilities = []
    Left = 48
    Top = 100
  end
  object HTMLDocGenerator: THTMLDocGenerator
    CSS = 
      'body { font-family: Verdana,Arial; '#13#10'  color: black; background-' +
      'color: white; '#13#10'  font-size: 12px; }'#13#10'body.navigationframe { fon' +
      't-family: Verdana,Arial; '#13#10'  color: white; background-color: #78' +
      '7878; '#13#10'  font-size: 12px; }'#13#10#13#10'img { border:0px; }'#13#10#13#10'a:link {c' +
      'olor:#C91E0C; text-decoration: none; }'#13#10'a:visited {color:#7E5C31' +
      '; text-decoration: none; }'#13#10'a:hover {text-decoration: underline;' +
      ' }'#13#10'a:active {text-decoration: underline; }'#13#10#13#10'a.navigation:link' +
      ' { color: white; text-decoration: none; font-size: 12px;}'#13#10'a.nav' +
      'igation:visited { color: white; text-decoration: none; font-size' +
      ': 12px;}'#13#10'a.navigation:hover { color: white; font-weight: bold; ' +
      #13#10'  text-decoration: none; font-size: 12px; }'#13#10'a.navigation:acti' +
      've { color: white; text-decoration: none; font-size: 12px;}'#13#10#13#10'a' +
      '.bold:link {color:#C91E0C; text-decoration: none; font-weight:bo' +
      'ld; }'#13#10'a.bold:visited {color:#7E5C31; text-decoration: none; fon' +
      't-weight:bold; }'#13#10'a.bold:hover {text-decoration: underline; font' +
      '-weight:bold; }'#13#10'a.bold:active {text-decoration: underline; font' +
      '-weight:bold; }'#13#10#13#10'a.section {color: green; text-decoration: non' +
      'e; font-weight: bold; }'#13#10'a.section:hover {color: green; text-dec' +
      'oration: underline; font-weight: bold; }'#13#10#13#10'ul.useslist a:link {' +
      'color:#C91E0C; text-decoration: none; font-weight:bold; }'#13#10'ul.us' +
      'eslist a:visited {color:#7E5C31; text-decoration: none; font-wei' +
      'ght:bold; }'#13#10'ul.useslist a:hover {text-decoration: underline; fo' +
      'nt-weight:bold; }'#13#10'ul.useslist a:active {text-decoration: underl' +
      'ine; font-weight:bold; }'#13#10#13#10'ul.hierarchy { list-style-type:none;' +
      ' }'#13#10'ul.hierarchylevel { list-style-type:none; }'#13#10#13#10'p.unitlink a:' +
      'link {color:#C91E0C; text-decoration: none; font-weight:bold; }'#13 +
      #10'p.unitlink a:visited {color:#7E5C31; text-decoration: none; fon' +
      't-weight:bold; }'#13#10'p.unitlink a:hover {text-decoration: underline' +
      '; font-weight:bold; }'#13#10'p.unitlink a:active {text-decoration: und' +
      'erline; font-weight:bold; }'#13#10#13#10'tr.list { background: #FFBF44; }'#13 +
      #10'tr.list2 { background: #FFC982; }'#13#10'tr.listheader { background: ' +
      '#C91E0C; color: white; }'#13#10#13#10'table.wide_list { border-spacing:2px' +
      '; width:100%; }'#13#10'table.wide_list td { vertical-align:top; paddin' +
      'g:4px; }'#13#10#13#10'table.markerlegend { width:auto; }'#13#10'table.markerlege' +
      'nd td.legendmarker { text-align:center; }'#13#10#13#10'table.sections { ba' +
      'ckground:white; }'#13#10'table.sections td {background:lightgray; }'#13#10#13 +
      #10'table.summary td.itemcode { width:100%; }'#13#10'table.detail td.item' +
      'code { width:100%; }'#13#10#13#10'td.itemname {white-space:nowrap; }'#13#10'td.i' +
      'temunit {white-space:nowrap; }'#13#10'td.itemdesc { width:100%; }'#13#10#13#10'd' +
      'iv.nodescription { color:red; }'#13#10'dl.parameters dt { color:blue; ' +
      '}'#13#10#13#10'/* Various browsers have various default styles for <h6>,'#13#10 +
      '   sometimes ugly for our purposes, so it'#39's best to set things'#13#10 +
      '   like font-size and font-weight in out pasdoc.css explicitly. ' +
      '*/'#13#10'h6.description_section { '#13#10'  /* font-size 100% means that it' +
      ' has the same font size as the '#13#10'     parent element, i.e. norma' +
      'l description text */'#13#10'  font-size: 100%;'#13#10'  font-weight: bold; ' +
      #13#10'  /* By default browsers usually have some large margin-bottom' +
      ' and '#13#10'     margin-top for <h1-6> tags. In our case, margin-bott' +
      'om is'#13#10'     unnecessary, we want to visually show that descripti' +
      'on_section'#13#10'     is closely related to content below. In this si' +
      'tuation'#13#10'     (where the font size is just as a normal text), sm' +
      'aller bottom'#13#10'     margin seems to look good. */'#13#10'  margin-botto' +
      'm: 0em;'#13#10'}'#13#10#13#10'/* Style applied to Pascal code in documentation '#13 +
      #10'   (e.g. produced by @longcode tag) } */'#13#10'span.pascal_string { ' +
      'color: #000080; }'#13#10'span.pascal_keyword { font-weight: bolder; }'#13 +
      #10'span.pascal_comment { color: #000080; font-style: italic; }'#13#10'sp' +
      'an.pascal_compiler_comment { color: #008000; }'#13#10'span.pascal_nume' +
      'ric { }'#13#10'span.pascal_hex { }'#13#10#13#10'p.hint_directive { color: red; }' +
      #13#10#13#10'input#search_text { }'#13#10'input#search_submit_button { }'#13#10#13#10'acr' +
      'onym.mispelling { background-color: #ffa; }'#13#10#13#10'/* Actually this ' +
      'reduces vertical space between *every* paragraph'#13#10'   inside list' +
      ' with @itemSpacing(compact). '#13#10'   While we would like to reduce ' +
      'this space only for the'#13#10'   top of 1st and bottom of last paragr' +
      'aph within each list item.'#13#10'   But, well, user probably will not' +
      ' do any paragraph breaks'#13#10'   within a list with @itemSpacing(com' +
      'pact) anyway, so it'#39's'#13#10'   acceptable solution. */'#13#10'ul.compact_sp' +
      'acing p { margin-top: 0em; margin-bottom: 0em; }'#13#10'ol.compact_spa' +
      'cing p { margin-top: 0em; margin-bottom: 0em; }'#13#10'dl.compact_spac' +
      'ing p { margin-top: 0em; margin-bottom: 0em; }'#13#10#13#10'/* Style for t' +
      'able created by @table tags:'#13#10'   just some thin border.'#13#10'   '#13#10'  ' +
      ' This way we have some borders around the cells'#13#10'   (so cells ar' +
      'e visibly separated), but the border '#13#10'   "blends with the backg' +
      'round" so it doesn'#39't look too ugly.'#13#10'   Hopefully it looks satis' +
      'factory in most cases and for most'#13#10'   people. '#13#10'   '#13#10'   We add ' +
      'padding for cells, otherwise they look too close.'#13#10'   This is no' +
      'rmal thing to do when border-collapse is set to'#13#10'   collapse (be' +
      'cause this eliminates spacing between cells). '#13#10'*/'#13#10'table.table_' +
      'tag { border-collapse: collapse; }'#13#10'table.table_tag td { border:' +
      ' 1pt solid gray; padding: 0.3em; }'#13#10'table.table_tag th { border:' +
      ' 1pt solid gray; padding: 0.3em; }'#13#10#13#10'table.detail {'#13#10'  border: ' +
      '1pt solid gray;'#13#10'  margin-top: 0.3em;'#13#10'  margin-bottom: 0.3em;'#13#10 +
      '}'#13#10
    Left = 48
    Top = 148
  end
  object TexDocGenerator: TTexDocGenerator
    Left = 48
    Top = 190
  end
  object HTMLHelpDocGenerator: THTMLHelpDocGenerator
    CSS = 
      'body { font-family: Verdana,Arial; '#13#10'  color: black; background-' +
      'color: white; '#13#10'  font-size: 12px; }'#13#10'body.navigationframe { fon' +
      't-family: Verdana,Arial; '#13#10'  color: white; background-color: #78' +
      '7878; '#13#10'  font-size: 12px; }'#13#10#13#10'img { border:0px; }'#13#10#13#10'a:link {c' +
      'olor:#C91E0C; text-decoration: none; }'#13#10'a:visited {color:#7E5C31' +
      '; text-decoration: none; }'#13#10'a:hover {text-decoration: underline;' +
      ' }'#13#10'a:active {text-decoration: underline; }'#13#10#13#10'a.navigation:link' +
      ' { color: white; text-decoration: none; font-size: 12px;}'#13#10'a.nav' +
      'igation:visited { color: white; text-decoration: none; font-size' +
      ': 12px;}'#13#10'a.navigation:hover { color: white; font-weight: bold; ' +
      #13#10'  text-decoration: none; font-size: 12px; }'#13#10'a.navigation:acti' +
      've { color: white; text-decoration: none; font-size: 12px;}'#13#10#13#10'a' +
      '.bold:link {color:#C91E0C; text-decoration: none; font-weight:bo' +
      'ld; }'#13#10'a.bold:visited {color:#7E5C31; text-decoration: none; fon' +
      't-weight:bold; }'#13#10'a.bold:hover {text-decoration: underline; font' +
      '-weight:bold; }'#13#10'a.bold:active {text-decoration: underline; font' +
      '-weight:bold; }'#13#10#13#10'a.section {color: green; text-decoration: non' +
      'e; font-weight: bold; }'#13#10'a.section:hover {color: green; text-dec' +
      'oration: underline; font-weight: bold; }'#13#10#13#10'ul.useslist a:link {' +
      'color:#C91E0C; text-decoration: none; font-weight:bold; }'#13#10'ul.us' +
      'eslist a:visited {color:#7E5C31; text-decoration: none; font-wei' +
      'ght:bold; }'#13#10'ul.useslist a:hover {text-decoration: underline; fo' +
      'nt-weight:bold; }'#13#10'ul.useslist a:active {text-decoration: underl' +
      'ine; font-weight:bold; }'#13#10#13#10'ul.hierarchy { list-style-type:none;' +
      ' }'#13#10'ul.hierarchylevel { list-style-type:none; }'#13#10#13#10'p.unitlink a:' +
      'link {color:#C91E0C; text-decoration: none; font-weight:bold; }'#13 +
      #10'p.unitlink a:visited {color:#7E5C31; text-decoration: none; fon' +
      't-weight:bold; }'#13#10'p.unitlink a:hover {text-decoration: underline' +
      '; font-weight:bold; }'#13#10'p.unitlink a:active {text-decoration: und' +
      'erline; font-weight:bold; }'#13#10#13#10'tr.list { background: #FFBF44; }'#13 +
      #10'tr.list2 { background: #FFC982; }'#13#10'tr.listheader { background: ' +
      '#C91E0C; color: white; }'#13#10#13#10'table.wide_list { border-spacing:2px' +
      '; width:100%; }'#13#10'table.wide_list td { vertical-align:top; paddin' +
      'g:4px; }'#13#10#13#10'table.markerlegend { width:auto; }'#13#10'table.markerlege' +
      'nd td.legendmarker { text-align:center; }'#13#10#13#10'table.sections { ba' +
      'ckground:white; }'#13#10'table.sections td {background:lightgray; }'#13#10#13 +
      #10'table.summary td.itemcode { width:100%; }'#13#10'table.detail td.item' +
      'code { width:100%; }'#13#10#13#10'td.itemname {white-space:nowrap; }'#13#10'td.i' +
      'temunit {white-space:nowrap; }'#13#10'td.itemdesc { width:100%; }'#13#10#13#10'd' +
      'iv.nodescription { color:red; }'#13#10'dl.parameters dt { color:blue; ' +
      '}'#13#10#13#10'/* Various browsers have various default styles for <h6>,'#13#10 +
      '   sometimes ugly for our purposes, so it'#39's best to set things'#13#10 +
      '   like font-size and font-weight in out pasdoc.css explicitly. ' +
      '*/'#13#10'h6.description_section { '#13#10'  /* font-size 100% means that it' +
      ' has the same font size as the '#13#10'     parent element, i.e. norma' +
      'l description text */'#13#10'  font-size: 100%;'#13#10'  font-weight: bold; ' +
      #13#10'  /* By default browsers usually have some large margin-bottom' +
      ' and '#13#10'     margin-top for <h1-6> tags. In our case, margin-bott' +
      'om is'#13#10'     unnecessary, we want to visually show that descripti' +
      'on_section'#13#10'     is closely related to content below. In this si' +
      'tuation'#13#10'     (where the font size is just as a normal text), sm' +
      'aller bottom'#13#10'     margin seems to look good. */'#13#10'  margin-botto' +
      'm: 0em;'#13#10'}'#13#10#13#10'/* Style applied to Pascal code in documentation '#13 +
      #10'   (e.g. produced by @longcode tag) } */'#13#10'span.pascal_string { ' +
      'color: #000080; }'#13#10'span.pascal_keyword { font-weight: bolder; }'#13 +
      #10'span.pascal_comment { color: #000080; font-style: italic; }'#13#10'sp' +
      'an.pascal_compiler_comment { color: #008000; }'#13#10'span.pascal_nume' +
      'ric { }'#13#10'span.pascal_hex { }'#13#10#13#10'p.hint_directive { color: red; }' +
      #13#10#13#10'input#search_text { }'#13#10'input#search_submit_button { }'#13#10#13#10'acr' +
      'onym.mispelling { background-color: #ffa; }'#13#10#13#10'/* Actually this ' +
      'reduces vertical space between *every* paragraph'#13#10'   inside list' +
      ' with @itemSpacing(compact). '#13#10'   While we would like to reduce ' +
      'this space only for the'#13#10'   top of 1st and bottom of last paragr' +
      'aph within each list item.'#13#10'   But, well, user probably will not' +
      ' do any paragraph breaks'#13#10'   within a list with @itemSpacing(com' +
      'pact) anyway, so it'#39's'#13#10'   acceptable solution. */'#13#10'ul.compact_sp' +
      'acing p { margin-top: 0em; margin-bottom: 0em; }'#13#10'ol.compact_spa' +
      'cing p { margin-top: 0em; margin-bottom: 0em; }'#13#10'dl.compact_spac' +
      'ing p { margin-top: 0em; margin-bottom: 0em; }'#13#10#13#10'/* Style for t' +
      'able created by @table tags:'#13#10'   just some thin border.'#13#10'   '#13#10'  ' +
      ' This way we have some borders around the cells'#13#10'   (so cells ar' +
      'e visibly separated), but the border '#13#10'   "blends with the backg' +
      'round" so it doesn'#39't look too ugly.'#13#10'   Hopefully it looks satis' +
      'factory in most cases and for most'#13#10'   people. '#13#10'   '#13#10'   We add ' +
      'padding for cells, otherwise they look too close.'#13#10'   This is no' +
      'rmal thing to do when border-collapse is set to'#13#10'   collapse (be' +
      'cause this eliminates spacing between cells). '#13#10'*/'#13#10'table.table_' +
      'tag { border-collapse: collapse; }'#13#10'table.table_tag td { border:' +
      ' 1pt solid gray; padding: 0.3em; }'#13#10'table.table_tag th { border:' +
      ' 1pt solid gray; padding: 0.3em; }'#13#10#13#10'table.detail {'#13#10'  border: ' +
      '1pt solid gray;'#13#10'  margin-top: 0.3em;'#13#10'  margin-bottom: 0.3em;'#13#10 +
      '}'#13#10
    Left = 48
    Top = 242
  end
end
