object FrmShapePoint: TFrmShapePoint
  Left = 0
  Top = 0
  Width = 232
  Height = 110
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  DesignSize = (
    232
    110)
  object Label1: TLabel
    Left = 4
    Top = 10
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&X:'
  end
  object Label4: TLabel
    Left = 4
    Top = 37
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Y:'
  end
  object edPositionX: TJvSpinEdit
    Left = 60
    Top = 7
    Width = 165
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edPositionXChange
  end
  object edPositionY: TJvSpinEdit
    Left = 60
    Top = 34
    Width = 165
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edPositionYChange
  end
end
