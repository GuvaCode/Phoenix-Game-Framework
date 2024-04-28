object FrmShapeBox: TFrmShapeBox
  Left = 0
  Height = 157
  Top = 0
  Width = 235
  ClientHeight = 157
  ClientWidth = 235
  TabOrder = 0
  DesignLeft = 196
  DesignTop = 195
  object Label1: TLabel
    Left = 14
    Height = 13
    Top = 7
    Width = 36
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&X:'
  end
  object Label6: TLabel
    Left = 14
    Height = 13
    Top = 40
    Width = 36
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Y:'
  end
  object Label7: TLabel
    Left = 8
    Height = 13
    Top = 72
    Width = 42
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Width:'
  end
  object Label8: TLabel
    Left = 0
    Height = 13
    Top = 104
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Height:'
  end
  object edCenterX: TSpinEdit
    Left = 64
    Height = 28
    Top = 7
    Width = 161
    Anchors = [akTop, akLeft, akRight]
    OnChange = edCenterXChange
    TabOrder = 0
  end
  object edCenterY: TSpinEdit
    Left = 64
    Height = 28
    Top = 40
    Width = 161
    Anchors = [akTop, akLeft, akRight]
    OnChange = edCenterYChange
    TabOrder = 1
  end
  object edWidth: TSpinEdit
    Left = 64
    Height = 28
    Top = 72
    Width = 161
    Anchors = [akTop, akLeft, akRight]
    OnChange = edWidthChange
    TabOrder = 2
  end
  object edHeight: TSpinEdit
    Left = 64
    Height = 28
    Top = 104
    Width = 161
    Anchors = [akTop, akLeft, akRight]
    OnChange = edHeightChange
    TabOrder = 3
  end
end
