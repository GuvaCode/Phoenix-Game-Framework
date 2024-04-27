object FrmPatternSort: TFrmPatternSort
  Left = 345
  Height = 88
  Top = 210
  Width = 303
  BorderStyle = bsDialog
  Caption = 'Sort patterns'
  ClientHeight = 88
  ClientWidth = 303
  OnCreate = FormCreate
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Label1: TLabel
    Left = 8
    Height = 16
    Top = 8
    Width = 46
    Caption = '&Sort by:'
  end
  object cbSortType: TComboBox
    Left = 72
    Height = 26
    Top = 8
    Width = 223
    Anchors = [akTop, akLeft, akRight]
    ItemHeight = 0
    Items.Strings = (
      'Name'
      'Name (Reversed)'
    )
    Style = csDropDownList
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 139
    Height = 25
    Top = 47
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Okey'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 220
    Height = 25
    Top = 47
    Width = 75
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
