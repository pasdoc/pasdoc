object DirBox: TDirBox
  Left = 0
  Top = 0
  Width = 331
  Height = 29
  TabOrder = 0
  DesignSize = (
    331
    29)
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Directory'
  end
  object edFile: TEdit
    Left = 64
    Top = 4
    Width = 225
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object buSelect: TButton
    Left = 296
    Top = 4
    Width = 31
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = buSelFileClick
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'Text Files|*.txt|Project Files|*.pds|Style Sheets|*.css|HTML Fil' +
      'es|*.htm?|All Files|*.*'
    Title = 'Select file'
    Left = 152
  end
end
