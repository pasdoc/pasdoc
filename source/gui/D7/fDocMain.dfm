object DocMain: TDocMain
  Left = 341
  Top = 111
  Width = 615
  Height = 471
  Caption = 'PasDoc GUI'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tabPages: TPageControl
    Left = 0
    Top = 0
    Width = 607
    Height = 425
    ActivePage = tabGenerate
    Align = alClient
    TabOrder = 0
    object tabOpts: TTabSheet
      Caption = 'Options'
      DesignSize = (
        599
        397)
      object Label2: TLabel
        Left = 8
        Top = 44
        Width = 20
        Height = 13
        Caption = 'Title'
      end
      object Label1: TLabel
        Left = 8
        Top = 96
        Width = 59
        Height = 13
        Caption = 'Output Type'
      end
      object Label3: TLabel
        Left = 408
        Top = 88
        Width = 115
        Height = 13
        Caption = 'Members only of visibility'
      end
      object Label4: TLabel
        Left = 272
        Top = 92
        Width = 19
        Height = 13
        Caption = 'Sort'
      end
      object Label5: TLabel
        Left = 8
        Top = 124
        Width = 83
        Height = 13
        Caption = 'Output Language'
      end
      object Label10: TLabel
        Left = 4
        Top = 180
        Width = 85
        Height = 13
        Caption = 'Comment Markers'
      end
      object Label12: TLabel
        Left = 408
        Top = 252
        Width = 71
        Height = 13
        Caption = 'Implicit Visibility'
      end
      object edTitle: TEdit
        Left = 48
        Top = 40
        Width = 548
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = AnyChange
      end
      object lbOutType: TComboBox
        Left = 104
        Top = 92
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = lbOutTypeChange
        Items.Strings = (
          'HTML'
          'HTML (full)'
          'HTML Help Workshop'
          'LaTeX'
          'LaTeX for latex2rtf'
          'XML (simple)'
          'XML (full)')
      end
      object swSort: TCheckListBox
        Left = 272
        Top = 112
        Width = 121
        Height = 153
        ItemHeight = 13
        Items.Strings = (
          'structures'
          'constants'
          'global functions'
          'types'
          'variables'
          'uses'
          'record fields'
          'non-record fields'
          'methods'
          'properties'
          'events')
        TabOrder = 2
        OnClick = AnyChange
      end
      object swAutoAbstract: TCheckBox
        Left = 8
        Top = 344
        Width = 409
        Height = 17
        Caption = 
          'Automatically deduce @abstract description from the 1st sentence' +
          ' of description'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = AnyChange
      end
      object lbOutLang: TComboBox
        Left = 104
        Top = 120
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = 'English'
        OnChange = lbOutLangChange
        Items.Strings = (
          'English'
          'German')
      end
      object swVisible: TCheckListBox
        Left = 408
        Top = 112
        Width = 121
        Height = 133
        ItemHeight = 13
        Items.Strings = (
          '<dummy>'
          'published'
          'public'
          'protected'
          'strict protected'
          'private'
          'strict private'
          'automated'
          'implicit')
        TabOrder = 5
        OnClick = AnyChange
      end
      object swBackRef: TCheckBox
        Left = 8
        Top = 276
        Width = 161
        Height = 17
        Caption = 'Default BackReferences'
        Checked = True
        State = cbChecked
        TabOrder = 6
        Visible = False
        OnClick = AnyChange
      end
      object lbMarkers: TMemo
        Left = 100
        Top = 208
        Width = 97
        Height = 57
        TabOrder = 7
        OnChange = AnyChange
      end
      object swMarkers: TComboBox
        Left = 100
        Top = 180
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 8
        OnChange = AnyChange
        Items.Strings = (
          'ignore'
          'optional'
          'mandatory'
          'single char')
      end
      inline edProjectFile: TDirBox
        Left = 0
        Top = 0
        Width = 599
        Height = 29
        Align = alTop
        TabOrder = 9
        inherited Label1: TLabel
          Width = 33
          Caption = 'Project'
        end
        inherited edFile: TEdit
          Left = 48
          Width = 460
        end
        inherited buSelect: TButton
          Left = 516
          Hint = 'Open project'
          OnClick = Open1Click
        end
        inherited dlgOpen: TOpenDialog
          DefaultExt = 'pds'
          FilterIndex = 2
          Options = [ofFileMustExist, ofEnableSizing]
          Title = 'Open Project file'
        end
      end
      object buSave: TButton
        Left = 564
        Top = 4
        Width = 31
        Height = 21
        Hint = 'Save project'
        Anchors = [akTop, akRight]
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 10
        OnClick = Save1Click
      end
      object swSpellCheck: TCheckBox
        Left = 8
        Top = 148
        Width = 109
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Check Spelling'
        TabOrder = 11
        OnClick = swSpellCheckClick
      end
      object swImplicitVisibility: TComboBox
        Left = 408
        Top = 276
        Width = 125
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 12
        Text = 'public'
        OnChange = AnyChange
        Items.Strings = (
          'public'
          'published'
          'implicit')
      end
      object swShowUses: TCheckBox
        Left = 8
        Top = 316
        Width = 97
        Height = 17
        Caption = 'Show Uses list'
        TabOrder = 13
        OnClick = AnyChange
      end
    end
    object tabFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 6
      inline lbFiles: TDirList
        Left = 0
        Top = 29
        Width = 599
        Height = 368
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 599
          Height = 368
          Caption = 'Source Files'
          inherited buAddAll: TButton
            OnClick = lbFilesbuAddAllClick
          end
          inherited buRemove: TButton
            Left = 563
            Top = 16
          end
          inherited lbFiles: TListBox
            Top = 63
            Width = 595
            Height = 303
          end
        end
        inherited dlgAdd: TOpenDialog
          FilterIndex = 2
          Options = [ofNoValidate, ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
          Title = 'Add source files'
          Left = 140
          Top = 24
        end
      end
      inline edRoot: TDirBox
        Left = 0
        Top = 0
        Width = 599
        Height = 29
        Align = alTop
        TabOrder = 1
        inherited Label1: TLabel
          Width = 68
          Caption = 'Root Directory'
        end
        inherited edFile: TEdit
          Left = 88
          Width = 469
        end
        inherited buSelect: TButton
          Left = 564
          OnClick = edRootbuSelectClick
        end
      end
    end
    object tabLocs: TTabSheet
      Caption = 'Locations'
      ImageIndex = 1
      inline lbInclude: TDirList
        Left = 0
        Top = 58
        Width = 599
        Height = 339
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 599
          Height = 339
          Caption = 'Include Directories'
          inherited buAddAll: TButton
            OnClick = lbIncludebuAddAllClick
          end
          inherited buRemove: TButton
            Left = 567
          end
          inherited lbFiles: TListBox
            Width = 595
            Height = 287
          end
        end
        inherited dlgAdd: TOpenDialog
          FilterIndex = 2
        end
      end
      inline edIntro: TDirBox
        Left = 0
        Top = 0
        Width = 599
        Height = 29
        Align = alTop
        TabOrder = 1
        inherited Label1: TLabel
          Width = 56
          Caption = 'Introduction'
        end
        inherited edFile: TEdit
          Left = 80
          Width = 477
        end
        inherited buSelect: TButton
          Left = 564
        end
        inherited dlgOpen: TOpenDialog
          DefaultExt = 'txt'
          Title = 'Select Introduction file'
        end
      end
      inline edConclusion: TDirBox
        Left = 0
        Top = 29
        Width = 599
        Height = 29
        Align = alTop
        TabOrder = 2
        inherited Label1: TLabel
          Width = 52
          Caption = 'Conclusion'
        end
        inherited edFile: TEdit
          Left = 80
          Width = 477
        end
        inherited buSelect: TButton
          Left = 564
        end
        inherited dlgOpen: TOpenDialog
          Title = 'Select Conclusion file'
        end
      end
    end
    object tabGraph: TTabSheet
      Caption = 'GraphWiz'
      ImageIndex = 2
      object Label11: TLabel
        Left = 176
        Top = 12
        Width = 75
        Height = 13
        Caption = 'Graph Package'
      end
      object Label8: TLabel
        Left = 176
        Top = 40
        Width = 56
        Height = 13
        Caption = 'Line Breaks'
      end
      object swClassDiagram: TCheckBox
        Left = 8
        Top = 8
        Width = 157
        Height = 17
        Caption = 'Create Classes Diagram'
        TabOrder = 0
        OnClick = AnyChange
      end
      object swUsesDiagram: TCheckBox
        Left = 8
        Top = 31
        Width = 157
        Height = 17
        Caption = 'Create Uses Diagram'
        TabOrder = 1
        OnClick = AnyChange
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 56
        Width = 595
        Height = 315
        Caption = 'Options'
        TabOrder = 2
        object Label9: TLabel
          Left = 260
          Top = 16
          Width = 92
          Height = 13
          Caption = 'Hyphenated Words'
        end
        object lbHyphenate: TMemo
          Left = 260
          Top = 40
          Width = 333
          Height = 269
          TabOrder = 0
          OnChange = AnyChange
        end
      end
      object lbGraphPackage: TComboBox
        Left = 260
        Top = 8
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = AnyChange
        Items.Strings = (
          'none'
          'PFD'
          'DVI')
      end
      object lbLineBreaks: TComboBox
        Left = 260
        Top = 36
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = 'strict'
        OnChange = AnyChange
        Items.Strings = (
          'strict'
          'sloppy')
      end
    end
    object tabHTML: TTabSheet
      Caption = 'HTML'
      ImageIndex = 3
      object GroupBox2: TGroupBox
        Left = 8
        Top = 88
        Width = 553
        Height = 137
        Caption = 'Header'
        TabOrder = 0
        object edHeader: TMemo
          Left = 2
          Top = 15
          Width = 549
          Height = 120
          Align = alClient
          TabOrder = 0
          OnChange = AnyChange
        end
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 232
        Width = 553
        Height = 177
        Caption = 'Footer'
        TabOrder = 1
        object edFooter: TMemo
          Left = 2
          Top = 15
          Width = 549
          Height = 160
          Align = alClient
          TabOrder = 0
          OnChange = AnyChange
        end
      end
      object swTipue: TCheckBox
        Left = 8
        Top = 4
        Width = 249
        Height = 17
        Caption = 'Use tipue search engine in HTML output'
        TabOrder = 2
        OnClick = AnyChange
      end
      inline edCSS: TDirBox
        Left = 0
        Top = 32
        Width = 586
        Height = 29
        TabOrder = 3
        inherited Label1: TLabel
          Width = 21
          Caption = 'CSS'
        end
        inherited edFile: TEdit
          Width = 480
        end
        inherited buSelect: TButton
          Left = 551
        end
        inherited dlgOpen: TOpenDialog
          FilterIndex = 3
          Title = 'Select StyleSheet file'
        end
      end
    end
    object tabSpelling: TTabSheet
      Caption = 'Spelling'
      ImageIndex = 4
      object GroupBox8: TGroupBox
        Left = 0
        Top = 0
        Width = 273
        Height = 397
        Align = alLeft
        Caption = 'Ignored Words'
        TabOrder = 0
        object lbIgnored: TMemo
          Left = 2
          Top = 15
          Width = 269
          Height = 380
          Align = alClient
          TabOrder = 0
          OnChange = AnyChange
        end
      end
      object GroupBox9: TGroupBox
        Left = 273
        Top = 0
        Width = 326
        Height = 397
        Align = alClient
        Caption = 'Misspelled Words'
        TabOrder = 1
        object edMisspelledWords: TMemo
          Left = 2
          Top = 15
          Width = 322
          Height = 380
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object tabMarkers: TTabSheet
      Caption = 'Markers'
      ImageIndex = 5
      TabVisible = False
    end
    object tabDirectories: TTabSheet
      Caption = 'Directories'
      ImageIndex = 7
      TabVisible = False
    end
    object tabDefines: TTabSheet
      Caption = 'Defines'
      ImageIndex = 8
      object GroupBox6: TGroupBox
        Left = 0
        Top = 0
        Width = 217
        Height = 397
        Align = alLeft
        Caption = 'Compiler specific'
        TabOrder = 0
        object Label13: TLabel
          Left = 8
          Top = 24
          Width = 35
          Height = 13
          Caption = 'Version'
        end
        object Label14: TLabel
          Left = 8
          Top = 48
          Width = 38
          Height = 13
          Caption = 'Platform'
        end
        object cbVersion: TComboBox
          Left = 64
          Top = 20
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = 'unknown'
          Items.Strings = (
            'unknown'
            'FreePascal'
            'mode Delphi'
            'Delphi 2'
            'Delphi 3'
            'Delphi 4'
            'Delphi 5'
            'Delphi 6'
            'Delphi 7'
            'BDS 2005'
            'BDS 2006'
            'RAD 2007')
        end
        object swConsole: TCheckBox
          Left = 8
          Top = 80
          Width = 69
          Height = 17
          Caption = 'Console'
          TabOrder = 1
        end
        object cbPlatform: TComboBox
          Left = 64
          Top = 48
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 2
          Text = 'any'
          Items.Strings = (
            'any'
            'Linux'
            'Unix'
            'Win32')
        end
        object swDebug: TCheckBox
          Left = 8
          Top = 104
          Width = 69
          Height = 17
          Caption = 'Debug'
          TabOrder = 3
        end
        object buCreate: TButton
          Left = 132
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Create'
          TabOrder = 4
          OnClick = buCreateClick
        end
        object edSysDefs: TMemo
          Left = 2
          Top = 136
          Width = 213
          Height = 259
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ReadOnly = True
          TabOrder = 5
          OnChange = AnyChange
        end
      end
      object GroupBox7: TGroupBox
        Left = 217
        Top = 0
        Width = 382
        Height = 397
        Align = alClient
        Caption = 'User defined'
        TabOrder = 1
        object memoDefines: TMemo
          Left = 2
          Top = 15
          Width = 378
          Height = 380
          Align = alClient
          TabOrder = 0
          OnChange = AnyChange
        end
      end
    end
    object tabGenerate: TTabSheet
      Caption = 'Generate'
      ImageIndex = 9
      object GroupBox1: TGroupBox
        Left = 0
        Top = 81
        Width = 599
        Height = 316
        Align = alClient
        Caption = 'Log'
        TabOrder = 0
        object lbLog: TListBox
          Left = 2
          Top = 15
          Width = 595
          Height = 299
          Align = alClient
          ItemHeight = 13
          PopupMenu = mnuLog
          TabOrder = 0
          OnKeyUp = lbLogKeyUp
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 599
        Height = 81
        Align = alTop
        Caption = 'Output Directory'
        TabOrder = 1
        DesignSize = (
          599
          81)
        object Label7: TLabel
          Left = 352
          Top = 24
          Width = 44
          Height = 13
          Caption = 'FileName'
        end
        object Label6: TLabel
          Left = 240
          Top = 56
          Width = 72
          Height = 13
          Caption = 'Message Level'
        end
        object buClearOutput: TButton
          Left = 8
          Top = 48
          Width = 89
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Clear directory'
          TabOrder = 0
          OnClick = buClearOutputClick
        end
        object buGenerate: TButton
          Left = 479
          Top = 48
          Width = 109
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Documentation'
          TabOrder = 1
          OnClick = buGenerateClick
        end
        inline edOutput: TDirBox
          Left = 2
          Top = 15
          Width = 351
          Height = 29
          TabOrder = 2
          inherited Label1: TLabel
            Width = 3
            Caption = ''
          end
          inherited edFile: TEdit
            Left = 8
            Width = 297
          end
          inherited buSelect: TButton
            Left = 309
            OnClick = edOutputbuSelectClick
          end
          inherited dlgOpen: TOpenDialog
            Title = 'Select Output directory'
          end
        end
        object edProjectName: TEdit
          Left = 408
          Top = 20
          Width = 177
          Height = 21
          Enabled = False
          TabOrder = 3
          OnChange = AnyChange
        end
        object swLevel: TUpDown
          Left = 341
          Top = 52
          Width = 15
          Height = 21
          Associate = edMsgLvl
          Max = 6
          Position = 2
          TabOrder = 4
          OnClick = swLevelClick
        end
        object edMsgLvl: TEdit
          Left = 320
          Top = 52
          Width = 21
          Height = 21
          ReadOnly = True
          TabOrder = 5
          Text = '2'
        end
        object buAnalyze: TButton
          Left = 367
          Top = 48
          Width = 85
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Analyze'
          TabOrder = 6
          OnClick = buGenerateClick
        end
        object buCmdFile: TButton
          Left = 136
          Top = 48
          Width = 85
          Height = 25
          Caption = 'Command file'
          TabOrder = 7
          OnClick = buCmdFileClick
        end
      end
    end
    object tabDoc: TTabSheet
      Caption = 'Docs'
      ImageIndex = 10
      object Splitter1: TSplitter
        Left = 241
        Top = 0
        Height = 397
      end
      object tvUnits: TTreeView
        Left = 0
        Top = 0
        Width = 241
        Height = 397
        Align = alLeft
        Indent = 19
        TabOrder = 0
        OnClick = tvUnitsClick
      end
      object Panel1: TPanel
        Left = 244
        Top = 0
        Width = 355
        Height = 397
        Align = alClient
        TabOrder = 1
        object edRem: TMemo
          Left = 1
          Top = 185
          Width = 353
          Height = 211
          Align = alClient
          Lines.Strings = (
            'edRem')
          ParentShowHint = False
          ReadOnly = True
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 0
        end
        object GroupBox10: TGroupBox
          Left = 1
          Top = 1
          Width = 353
          Height = 184
          Align = alTop
          Caption = 'Source'
          TabOrder = 1
          DesignSize = (
            353
            184)
          object cbRem: TComboBox
            Left = 12
            Top = 72
            Width = 329
            Height = 104
            Style = csSimple
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnClick = cbRemClick
          end
          object edName: TEdit
            Left = 8
            Top = 16
            Width = 333
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = 'Name'
          end
          object edValue: TEdit
            Left = 8
            Top = 40
            Width = 333
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'Value'
          end
        end
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 568
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        RadioItem = True
        object mnuDelphi: TMenuItem
          Caption = 'Delphi'
          OnClick = mnuDelphiClick
        end
        object mnuFPC: TMenuItem
          Caption = 'FreePascal'
          OnClick = mnuFPCClick
        end
      end
      object Open1: TMenuItem
        Caption = '&Open'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Preferences1: TMenuItem
        Caption = '&Preferences'
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Projects (*.pds)|*.pds|Source files (*.pp, *.pas, *.dpr)|*.pp;*.' +
      'pas;*.dpr|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 528
  end
  object PasDoc1: TPasDoc
    OnWarning = PasDocWarning
    OnMessage = PasDocWarning
    StarStyleOnly = False
    SingleCharMarkers = False
    ShowVisibilities = []
    Left = 492
    Top = 360
  end
  object HTMLDocGenerator: THTMLDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
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
    Left = 456
    Top = 360
  end
  object TexDocGenerator: TTexDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
    Left = 416
    Top = 360
  end
  object HTMLHelpDocGenerator: THTMLHelpDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
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
    Left = 380
    Top = 360
  end
  object OpenDialog2: TOpenDialog
    Left = 340
    Top = 328
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Text File|*.txt|Project File|*.pds|All Files|*.*'
    Left = 300
    Top = 328
  end
  object mnuLog: TPopupMenu
    Left = 480
    object mnCleardiagnostics: TMenuItem
      Caption = 'Clear diagnostics'
      OnClick = mnCleardiagnosticsClick
    end
    object SaveAs1: TMenuItem
      Caption = 'Save &As...'
      OnClick = SaveLog
    end
  end
  object SimpleXMLDocGenerator: TSimpleXMLDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
    Left = 540
    Top = 360
  end
  object XMLDocGenerator: TXMLDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
    Left = 544
    Top = 312
  end
  object FullHTMLDocGenerator: TFullHTMLDocGenerator
    AutoAbstract = False
    LinkLook = llDefault
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
    Left = 492
    Top = 312
  end
end
