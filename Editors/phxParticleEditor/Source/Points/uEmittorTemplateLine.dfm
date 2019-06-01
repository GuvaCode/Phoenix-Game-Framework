object FrmEmittorTemplateLine: TFrmEmittorTemplateLine
  Left = 0
  Top = 0
  Width = 270
  Height = 183
  TabOrder = 0
  DesignSize = (
    270
    183)
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 25
    Height = 13
    Caption = '&Min X'
  end
  object Label2: TLabel
    Left = 8
    Top = 31
    Width = 25
    Height = 13
    Caption = '&Min Y'
  end
  object Label3: TLabel
    Left = 8
    Top = 59
    Width = 29
    Height = 13
    Caption = '&Max X'
  end
  object Label4: TLabel
    Left = 8
    Top = 87
    Width = 29
    Height = 13
    Caption = '&Max X'
  end
  object btnCreate: TButton
    Left = 190
    Top = 100
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Create'
    TabOrder = 0
  end
  object edMinX: TJvSpinEdit
    Left = 105
    Top = 4
    Width = 161
    Height = 21
    ValueType = vtFloat
    Value = -50.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edMinY: TJvSpinEdit
    Left = 105
    Top = 28
    Width = 161
    Height = 21
    ValueType = vtFloat
    Value = -50.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edMaxX: TJvSpinEdit
    Left = 104
    Top = 52
    Width = 161
    Height = 21
    ValueType = vtFloat
    Value = 50.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edMaxY: TJvSpinEdit
    Left = 104
    Top = 76
    Width = 161
    Height = 21
    ValueType = vtFloat
    Value = 50.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
end
