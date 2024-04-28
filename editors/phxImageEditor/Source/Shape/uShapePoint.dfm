object FrmShapePoint: TFrmShapePoint
  Left = 0
  Height = 110
  Top = 0
  Width = 232
  ClientHeight = 110
  ClientWidth = 232
  TabOrder = 0
  object Label1: TLabel
    Left = 4
    Height = 13
    Top = 7
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&X:'
  end
  object Label4: TLabel
    Left = 4
    Height = 13
    Top = 40
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Y:'
  end
  object edPositionX: TSpinEdit
    Left = 60
    Height = 28
    Top = 7
    Width = 165
    Anchors = [akTop, akLeft, akRight]
    OnChange = edPositionXChange
    TabOrder = 0
  end
  object edPositionY: TSpinEdit
    Left = 60
    Height = 28
    Top = 40
    Width = 165
    Anchors = [akTop, akLeft, akRight]
    OnChange = edPositionYChange
    TabOrder = 1
  end
end
