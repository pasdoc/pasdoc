object frmHelpGenerator: TfrmHelpGenerator
  Left = 691
  Top = 277
  HelpType = htKeyword
  HelpKeyword = 'PasDocGui'
  Caption = 'pasdoc gui'
  ClientHeight = 411
  ClientWidth = 658
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
    Height = 411
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'PanelLeft'
    TabOrder = 0
    object lbNavigation: TListBox
      Left = 0
      Top = 38
      Width = 165
      Height = 373
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
    Width = 493
    Height = 411
    Align = alClient
    PageIndex = 7
    TabOrder = 1
    object pageOptions: TPage
      Left = 0
      Top = 0
      Caption = 'Options'
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        493
        411)
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          493
          411)
        object Label8: TLabel
          Left = 10
          Top = 10
          Width = 473
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
          ExplicitWidth = 481
        end
        object btnBrowseSourceFiles: TButton
          Left = 10
          Top = 54
          Width = 473
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Browse'
          TabOrder = 0
          OnClick = btnBrowseSourceFilesClick
          ExplicitWidth = 481
        end
        object memoFiles: TMemo
          Left = 10
          Top = 86
          Width = 473
          Height = 315
          HelpType = htKeyword
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnChange = SomethingChanged
          ExplicitWidth = 481
          ExplicitHeight = 320
        end
      end
    end
    object pageIncludeDirectories: TPage
      Left = 0
      Top = 0
      Caption = 'Include Directories'
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          493
          411)
        object Label9: TLabel
          Left = 10
          Top = 10
          Width = 473
          Height = 26
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'The directories where PasDoc can find include files.'#10'(If you use' +
            ' $I, $INCLUDE compiler directives.)'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
          ExplicitWidth = 245
        end
        object btnBrowseIncludeDirectory: TButton
          Left = 10
          Top = 41
          Width = 473
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Browse'
          TabOrder = 0
          OnClick = btnBrowseIncludeDirectoryClick
          ExplicitWidth = 481
        end
        object memoIncludeDirectories: TMemo
          Left = 10
          Top = 72
          Width = 473
          Height = 329
          HelpType = htKeyword
          HelpKeyword = 'IncludeInSearchPath'
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnChange = SomethingChanged
          ExplicitWidth = 481
          ExplicitHeight = 334
        end
      end
    end
    object pageDefines: TPage
      Left = 0
      Top = 0
      Caption = 'Defines'
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Width = 493
        Height = 431
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        ExplicitWidth = 501
        ExplicitHeight = 416
        object LabelVisibleMembers: TLabel
          Left = 10
          Top = 10
          Width = 473
          Height = 26
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'Structures (classes etc.) members (properties, methods, events, ' +
            'fields) to show in documentation :'
          Color = clBtnFace
          FocusControl = CheckListVisibleMembers
          ParentColor = False
          WordWrap = True
          ExplicitWidth = 472
        end
        object RadioImplicitVisibility: TRadioGroup
          Left = 10
          Top = 152
          Width = 473
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
          ExplicitTop = 139
          ExplicitWidth = 481
        end
        object CheckListVisibleMembers: TCheckListBox
          Left = 10
          Top = 36
          Width = 473
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
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PanelSort: TPanel
        Left = 0
        Top = 0
        Width = 493
        Height = 431
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        ExplicitWidth = 501
        ExplicitHeight = 416
        object Label1: TLabel
          Left = 10
          Top = 10
          Width = 473
          Height = 13
          HelpType = htKeyword
          Align = alTop
          Caption = 'Items to sort alphabetically'
          Color = clBtnFace
          FocusControl = clbSorting
          ParentColor = False
          ExplicitWidth = 130
        end
        object clbSorting: TCheckListBox
          Left = 10
          Top = 23
          Width = 473
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Width = 120
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
        493
        411)
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Left = 146
        Top = 0
        Width = 347
        Height = 411
        Align = alRight
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 2
        ExplicitLeft = 154
        ExplicitHeight = 416
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          493
          103)
        object Label10: TLabel
          Left = 10
          Top = 10
          Width = 473
          Height = 26
          HelpType = htKeyword
          Align = alTop
          Caption = 
            'While generating documentation, messages describing what is happ' +
            'ening will appear in the area below.'
          Color = clBtnFace
          ParentColor = False
          WordWrap = True
          ExplicitWidth = 462
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
          Width = 473
          Height = 25
          Anchors = [akLeft, akRight]
          Caption = 'Generate documentation'
          TabOrder = 1
          OnClick = ButtonGenerateDocsClick
          ExplicitWidth = 481
        end
      end
      object PanelGenerateBottom: TPanel
        Left = 0
        Top = 103
        Width = 493
        Height = 308
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        ExplicitWidth = 501
        ExplicitHeight = 313
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
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 187
        Width = 493
        Height = 5
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 501
      end
      object pnlEditCommentInstructions: TPanel
        Left = 0
        Top = 0
        Width = 493
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Click on an item in the tree view to see its comment.'
        FullRepaint = False
        TabOrder = 0
        ExplicitWidth = 501
      end
      object PanelDisplayCommentsMid: TPanel
        Left = 0
        Top = 28
        Width = 493
        Height = 159
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 1
        ExplicitWidth = 501
        object tvUnits: TTreeView
          Left = 10
          Top = 10
          Width = 473
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
        Width = 493
        Height = 219
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 2
        ExplicitWidth = 501
        ExplicitHeight = 224
        object seComment: TMemo
          Left = 10
          Top = 10
          Width = 473
          Height = 219
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
end
