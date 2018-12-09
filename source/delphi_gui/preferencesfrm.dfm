object Preferences: TPreferences
  Left = 287
  Top = 206
  Width = 432
  Height = 226
  Caption = 'Preferences'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    424
    192)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelWWWHelpServer: TLabel
    Left = 8
    Top = 104
    Width = 151
    Height = 13
    Caption = '&URL of server with help pages :'
    Color = clBtnFace
    FocusControl = EditWWWHelpServer
    ParentColor = False
  end
  object EditWWWHelpServer: TEdit
    Left = 8
    Top = 120
    Width = 392
    Height = 21
    TabOrder = 1
    Text = 'EditWWWHelpServer'
  end
  object BtnOK: TButton
    Left = 244
    Top = 157
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 332
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BtnResetDefaults: TButton
    Left = 8
    Top = 156
    Width = 139
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Reset to defaults'
    TabOrder = 2
    OnClick = BtnResetDefaultsClick
  end
  object cbLoadLastProject: TCheckBox
    Left = 8
    Top = 74
    Width = 345
    Height = 17
    Caption = 'Auto load last project'
    TabOrder = 5
  end
end
