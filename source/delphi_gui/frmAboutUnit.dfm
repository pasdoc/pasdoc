object frmAbout: TfrmAbout
  Left = 167
  Top = 62
  Caption = 'About'
  ClientHeight = 347
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = frmAboutCreate
  DesignSize = (
    408
    347)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTitle: TLabel
    Left = 13
    Top = 8
    Width = 109
    Height = 24
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 'pasdoc_gui'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object MemoInformation: TMemo
    Left = 13
    Top = 38
    Width = 387
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object ButtonPasDocURL: TButton
    Left = 13
    Top = 314
    Width = 225
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'http://pasdoc.sourceforge.net/'
    TabOrder = 1
    OnClick = ButtonPasDocURLClick
  end
  object ButtonClose: TButton
    Left = 318
    Top = 314
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
  end
end
