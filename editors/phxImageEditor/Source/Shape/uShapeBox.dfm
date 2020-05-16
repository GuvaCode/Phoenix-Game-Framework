object FrmShapeBox: TFrmShapeBox
  Left = 0
  Top = 0
  Width = 357
  Height = 157
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  DesignSize = (
    357
    157)
  object Label1: TLabel
    Left = 4
    Top = 11
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&X:'
  end
  object Label6: TLabel
    Left = 4
    Top = 37
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Y:'
  end
  object Label7: TLabel
    Left = 4
    Top = 64
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Width:'
  end
  object Label8: TLabel
    Left = 4
    Top = 91
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Height:'
  end
  object edCenterX: TJvSpinEdit
    Left = 60
    Top = 7
    Width = 290
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edCenterXChange
  end
  object edCenterY: TJvSpinEdit
    Left = 60
    Top = 34
    Width = 290
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edCenterYChange
  end
  object edWidth: TJvSpinEdit
    Left = 60
    Top = 61
    Width = 290
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = edWidthChange
  end
  object edHeight: TJvSpinEdit
    Left = 60
    Top = 88
    Width = 290
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edHeightChange
  end
end
