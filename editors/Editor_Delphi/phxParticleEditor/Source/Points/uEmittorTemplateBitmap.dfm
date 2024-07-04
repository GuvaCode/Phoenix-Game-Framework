object FrmEmittorTemplateBitmap: TFrmEmittorTemplateBitmap
  Left = 0
  Top = 0
  Width = 270
  Height = 240
  TabOrder = 0
  DesignSize = (
    270
    240)
  object Label1: TLabel
    Left = 8
    Top = 34
    Width = 42
    Height = 13
    Caption = '&Direction'
  end
  object Label3: TLabel
    Left = 8
    Top = 61
    Width = 77
    Height = 13
    Caption = '&Sample interval:'
  end
  object Label4: TLabel
    Left = 8
    Top = 7
    Width = 42
    Height = 13
    Caption = '&Filename'
  end
  object btnImport: TButton
    Left = 190
    Top = 85
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Import'
    Enabled = False
    TabOrder = 0
  end
  object cbDirection: TComboBox
    Left = 104
    Top = 31
    Width = 161
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Horisontal, Left to right'
    Items.Strings = (
      'Horisontal, Left to right'
      'Horisontal, Right to left'
      'Vertical, Top to bottom'
      'Vertical, Bottom to top')
  end
  object edInterval: TJvSpinEdit
    Left = 104
    Top = 58
    Width = 161
    Height = 21
    Value = 1.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edFilename: TJvComboEdit
    Left = 104
    Top = 4
    Width = 161
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnButtonClick = edFilenameButtonClick
    OnChange = edFilenameChange
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Bitmap (*.bmp)|*.bmp|XML File (*.xml)|*.xml'
    Left = 12
    Top = 92
  end
end
