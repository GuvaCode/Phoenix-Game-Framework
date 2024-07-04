object FrmShapeCircle: TFrmShapeCircle
  Left = 0
  Top = 0
  Width = 256
  Height = 123
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  TabStop = True
  DesignSize = (
    256
    123)
  object Label4: TLabel
    Left = 4
    Top = 10
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Center X:'
  end
  object Label5: TLabel
    Left = 4
    Top = 37
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Center Y:'
  end
  object Label6: TLabel
    Left = 4
    Top = 64
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Radius:'
  end
  object edCenterX: TJvSpinEdit
    Left = 60
    Top = 7
    Width = 189
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edCenterXChange
    ExplicitWidth = 508
  end
  object edCenterY: TJvSpinEdit
    Left = 60
    Top = 34
    Width = 189
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edCenterYChange
    ExplicitWidth = 508
  end
  object edRadius: TJvSpinEdit
    Left = 60
    Top = 61
    Width = 189
    Height = 21
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = edRadiusChange
    ExplicitWidth = 508
  end
end
