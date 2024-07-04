object FrmShapeBox: TFrmShapeBox
  Left = 0
  Top = 0
  Width = 320
  Height = 200
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 312
    Height = 133
    Align = alTop
    Caption = 'Box'
    TabOrder = 0
    DesignSize = (
      312
      133)
    object Label2: TLabel
      Left = 8
      Top = 21
      Width = 10
      Height = 13
      Caption = '&X:'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = '&Y:'
    end
    object Label4: TLabel
      Left = 8
      Top = 75
      Width = 32
      Height = 13
      Caption = '&Width:'
    end
    object Label5: TLabel
      Left = 8
      Top = 102
      Width = 35
      Height = 13
      Caption = '&Height:'
    end
    object edCenterX: TJvSpinEdit
      Left = 49
      Top = 18
      Width = 254
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edCenterXChange
    end
    object edCenterY: TJvSpinEdit
      Left = 49
      Top = 45
      Width = 254
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edCenterYChange
    end
    object edWidth: TJvSpinEdit
      Left = 49
      Top = 72
      Width = 254
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edWidthChange
    end
    object edHeight: TJvSpinEdit
      Left = 49
      Top = 99
      Width = 254
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edHeightChange
    end
  end
end
