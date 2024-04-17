object FrmShapeCircle: TFrmShapeCircle
  Left = 0
  Height = 123
  Top = 0
  Width = 256
  ClientHeight = 123
  ClientWidth = 256
  TabOrder = 0
  TabStop = True
  DesignLeft = 175
  DesignTop = 256
  object Label4: TLabel
    Left = 0
    Height = 13
    Top = 7
    Width = 58
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Center X:'
  end
  object Label5: TLabel
    Left = 0
    Height = 13
    Top = 40
    Width = 58
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Center Y:'
  end
  object Label6: TLabel
    Left = 0
    Height = 13
    Top = 72
    Width = 58
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Radius:'
  end
  object edCenterX: TSpinEdit
    Left = 64
    Height = 28
    Top = 7
    Width = 185
    Anchors = [akTop, akLeft, akRight]
    OnChange = edCenterXChange
    TabOrder = 0
  end
  object edCenterY: TSpinEdit
    Left = 64
    Height = 28
    Top = 40
    Width = 185
    Anchors = [akTop, akLeft, akRight]
    OnChange = edCenterYChange
    TabOrder = 1
  end
  object edRadius: TSpinEdit
    Left = 64
    Height = 28
    Top = 72
    Width = 185
    Anchors = [akTop, akLeft, akRight]
    OnChange = edRadiusChange
    TabOrder = 2
  end
end
