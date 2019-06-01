object FrmEmissionPoints: TFrmEmissionPoints
  Left = 0
  Top = 0
  Caption = 'Emission points'
  ClientHeight = 278
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    527
    278)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 11
    Width = 90
    Height = 13
    Alignment = taRightJustify
    Caption = '&Number of points:'
  end
  object Label3: TLabel
    Left = 8
    Top = 38
    Width = 90
    Height = 13
    Alignment = taRightJustify
    Caption = '&Template type:'
  end
  object ButtonOK: TButton
    Left = 363
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 480
    ExplicitTop = 272
  end
  object ButtonCancel: TButton
    Left = 444
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 561
    ExplicitTop = 272
  end
  object cbTemplateType: TComboBox
    Left = 104
    Top = 35
    Width = 138
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbTemplateTypeChange
    Items.Strings = (
      ''
      'Line'
      'Circle'
      'Box'
      'Bitmap')
  end
  object edCount: TJvSpinEdit
    Left = 104
    Top = 8
    Width = 138
    Height = 21
    Value = 100.000000000000000000
    TabOrder = 3
  end
  object pnEmittorTemplate: TPanel
    Left = 0
    Top = 62
    Width = 529
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 4
  end
end
