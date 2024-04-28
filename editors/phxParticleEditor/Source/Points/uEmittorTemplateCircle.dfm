object FrmEmittorTemplateCircle: TFrmEmittorTemplateCircle
  Left = 0
  Top = 0
  Width = 270
  Height = 227
  TabOrder = 0
  DesignSize = (
    270
    227)
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 32
    Height = 13
    Caption = '&Radius'
  end
  object edRadius: TJvSpinEdit
    Left = 104
    Top = 4
    Width = 161
    Height = 21
    ValueType = vtFloat
    Value = 50.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnCreate: TButton
    Left = 190
    Top = 46
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Create'
    TabOrder = 1
  end
  object cbConstrainToSurface: TCheckBox
    Left = 104
    Top = 27
    Width = 145
    Height = 17
    Caption = 'Constrain To Surface'
    TabOrder = 2
  end
end
