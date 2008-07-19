object DirList: TDirList
  Left = 0
  Top = 0
  Width = 320
  Height = 255
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 320
    Height = 255
    Align = alClient
    Caption = 'GroupBox1'
    TabOrder = 0
    DesignSize = (
      320
      255)
    object buAdd: TButton
      Left = 4
      Top = 20
      Width = 29
      Height = 25
      Hint = 'Add an file'
      Caption = '3'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = buAddClick
    end
    object buAddAll: TButton
      Left = 44
      Top = 20
      Width = 29
      Height = 25
      Hint = 'Add files like...'
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object buRemove: TButton
      Left = 288
      Top = 20
      Width = 27
      Height = 25
      Hint = 'Remove selected items'
      Anchors = [akTop, akRight]
      Caption = 'x'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Wingdings'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = buRemoveClick
    end
    object lbFiles: TListBox
      Left = 2
      Top = 50
      Width = 316
      Height = 203
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 3
    end
  end
  object dlgAdd: TOpenDialog
    Filter = 'Include Files (*.inc)|*.inc|All Files|*.*'
    Options = [ofHideReadOnly, ofNoValidate, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Collect directories containing files like pattern'
    Left = 144
    Top = 8
  end
end
