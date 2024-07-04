object FrmPatternSort: TFrmPatternSort
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sort patterns'
  ClientHeight = 62
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    303
    62)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = '&Sort by:'
  end
  object cbSortType: TComboBox
    Left = 53
    Top = 5
    Width = 242
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Items.Strings = (
      'Name'
      'Name (Reversed)')
  end
  object btnOk: TButton
    Left = 139
    Top = 30
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Okey'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 43
    ExplicitTop = 32
  end
  object btnCancel: TButton
    Left = 220
    Top = 30
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 124
    ExplicitTop = 32
  end
end
