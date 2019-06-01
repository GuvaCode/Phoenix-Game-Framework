object FrmEffectPhysics: TFrmEffectPhysics
  Left = 0
  Top = 0
  Width = 266
  Height = 572
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox2: TGroupBox
    Left = 4
    Top = 129
    Width = 258
    Height = 128
    Align = alTop
    Caption = 'Particle Movement'
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 157
    ExplicitWidth = 266
    object Label1: TLabel
      Left = 8
      Top = 23
      Width = 84
      Height = 13
      Caption = 'Minimum velocity:'
    end
    object Label2: TLabel
      Left = 8
      Top = 50
      Width = 88
      Height = 13
      Caption = 'Maximum velocity:'
    end
    object Label3: TLabel
      Left = 8
      Top = 77
      Width = 66
      Height = 13
      Caption = '&Minimum spin:'
    end
    object Label10: TLabel
      Left = 8
      Top = 104
      Width = 70
      Height = 13
      Caption = '&Maximum spin:'
    end
    object edVelocityMin: TJvSpinEdit
      Left = 111
      Top = 20
      Width = 132
      Height = 21
      TabOrder = 0
    end
    object edVelocityMax: TJvSpinEdit
      Left = 110
      Top = 47
      Width = 133
      Height = 21
      TabOrder = 1
    end
    object edSpinMax: TJvSpinEdit
      Left = 110
      Top = 101
      Width = 133
      Height = 21
      TabOrder = 2
    end
    object edSpinMin: TJvSpinEdit
      Left = 110
      Top = 74
      Width = 133
      Height = 21
      TabOrder = 3
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 258
    Height = 125
    Align = alTop
    Caption = 'Initial direction'
    TabOrder = 1
    OnClick = GroupBox1Click
    object Label7: TLabel
      Left = 8
      Top = 23
      Width = 56
      Height = 13
      Caption = '&Direction.X:'
    end
    object Label5: TLabel
      Left = 8
      Top = 50
      Width = 56
      Height = 13
      Caption = '&Direction.Y:'
    end
    object Label6: TLabel
      Left = 8
      Top = 77
      Width = 56
      Height = 13
      Caption = '&Direction.Z:'
    end
    object Label8: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = '&Spread:'
    end
    object edDirectionX: TJvSpinEdit
      Left = 111
      Top = 20
      Width = 132
      Height = 21
      TabOrder = 0
      OnChange = edDirectionXChange
    end
    object edDirectionY: TJvSpinEdit
      Left = 111
      Top = 47
      Width = 132
      Height = 21
      TabOrder = 1
      OnChange = edDirectionYChange
    end
    object edDirectionZ: TJvSpinEdit
      Left = 111
      Top = 74
      Width = 132
      Height = 21
      TabOrder = 2
      OnChange = edDirectionZChange
    end
    object edSpread: TJvSpinEdit
      Left = 111
      Top = 100
      Width = 132
      Height = 21
      TabOrder = 3
      OnChange = edSpreadChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 257
    Width = 258
    Height = 108
    Align = alTop
    Caption = 'World movement'
    TabOrder = 2
    ExplicitLeft = 0
    ExplicitTop = 285
    ExplicitWidth = 266
    object Label9: TLabel
      Left = 8
      Top = 23
      Width = 73
      Height = 13
      Caption = '&Acceleration.X:'
    end
    object Label4: TLabel
      Left = 8
      Top = 50
      Width = 73
      Height = 13
      Caption = '&Acceleration.Y:'
    end
    object Label12: TLabel
      Left = 8
      Top = 77
      Width = 69
      Height = 13
      Caption = '&Acceleration.Z'
    end
    object edWorldAccelerationX: TJvSpinEdit
      Left = 111
      Top = 20
      Width = 132
      Height = 21
      ValueType = vtFloat
      TabOrder = 0
    end
    object edWorldAccelerationY: TJvSpinEdit
      Left = 111
      Top = 47
      Width = 132
      Height = 21
      ValueType = vtFloat
      TabOrder = 1
    end
    object edWorldAccelerationZ: TJvSpinEdit
      Left = 110
      Top = 74
      Width = 133
      Height = 21
      ValueType = vtFloat
      TabOrder = 2
    end
  end
  object GroupBox4: TGroupBox
    Left = 4
    Top = 365
    Width = 258
    Height = 132
    Align = alTop
    Caption = 'Direction'
    TabOrder = 3
    object PaintBox1: TPaintBox
      Left = 113
      Top = 15
      Width = 143
      Height = 115
      Align = alClient
      OnPaint = PaintBox1Paint
      ExplicitLeft = 72
      ExplicitTop = 0
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object Panel1: TPanel
      Left = 2
      Top = 15
      Width = 111
      Height = 115
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 91
      object RadioXY: TRadioButton
        Left = 8
        Top = 8
        Width = 45
        Height = 17
        Caption = 'XY'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioXYClick
      end
      object RadioButton2: TRadioButton
        Left = 8
        Top = 31
        Width = 45
        Height = 17
        Caption = 'XY'
        TabOrder = 1
      end
    end
  end
end
