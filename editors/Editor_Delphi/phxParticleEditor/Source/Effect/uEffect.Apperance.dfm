object FrmEffectApperance: TFrmEffectApperance
  Left = 0
  Top = 0
  Width = 316
  Height = 628
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox2: TGroupBox
    Left = 4
    Top = 4
    Width = 308
    Height = 129
    Align = alTop
    Caption = 'Texture'
    TabOrder = 0
    DesignSize = (
      308
      129)
    object Label1: TLabel
      Left = 8
      Top = 23
      Width = 42
      Height = 13
      Caption = '&Texture:'
    end
    object Label2: TLabel
      Left = 8
      Top = 50
      Width = 66
      Height = 13
      Caption = '&Pattern count'
    end
    object Label3: TLabel
      Left = 8
      Top = 77
      Width = 69
      Height = 13
      Caption = '&Pattern index:'
    end
    object Label15: TLabel
      Left = 8
      Top = 104
      Width = 84
      Height = 13
      Caption = '&Pattern variance:'
    end
    object edPatternSize: TComboBox
      Left = 111
      Top = 47
      Width = 188
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = '1x1'
      OnChange = edPatternSizeChange
      Items.Strings = (
        '1x1'
        '2x2'
        '4x4'
        '8x8')
    end
    object edPatternVariance: TJvSpinEdit
      Left = 111
      Top = 101
      Width = 188
      Height = 21
      Hint = 'Random value to add to the pattern index'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = edPatternVarianceChange
    end
    object edTexture: TJvComboEdit
      Left = 111
      Top = 20
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ButtonHint = 'Select texture'
      DisabledColor = clBtnFace
      TabOrder = 2
      OnButtonClick = edTextureClick
      OnChange = edTextureChange
    end
    object edPatternIndex: TJvComboEdit
      Left = 111
      Top = 74
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DisabledColor = clBtnFace
      TabOrder = 3
      OnButtonClick = edPatternIndexButtonClick
      OnChange = edPatternIndexChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 133
    Width = 308
    Height = 182
    Align = alTop
    Caption = 'Apperance'
    TabOrder = 1
    DesignSize = (
      308
      182)
    object Label4: TLabel
      Left = 8
      Top = 21
      Width = 34
      Height = 13
      Caption = '&Shape:'
    end
    object Label5: TLabel
      Left = 8
      Top = 75
      Width = 63
      Height = 13
      Caption = '&Mininum size:'
    end
    object Label6: TLabel
      Left = 8
      Top = 102
      Width = 69
      Height = 13
      Caption = '&Maximum size:'
    end
    object Label7: TLabel
      Left = 8
      Top = 48
      Width = 40
      Height = 13
      Caption = '&Blending'
    end
    object Label9: TLabel
      Left = 8
      Top = 129
      Width = 79
      Height = 13
      Caption = '&Mininum growth:'
    end
    object Label10: TLabel
      Left = 8
      Top = 156
      Width = 85
      Height = 13
      Caption = '&Maximum growth:'
    end
    object edShape: TComboBox
      Left = 111
      Top = 18
      Width = 188
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edShapeChange
      Items.Strings = (
        'Point,'
        'Billboard'
        'Billboard (Rotated)'
        'AlignX'
        'AlignX (Rotated)'
        'AlignY'
        'AlignY (Rotated)'
        'AlignZ'
        'AlignZ (Rotated)'
        'Trail'
        'TrailZ')
    end
    object edSizeMin: TJvSpinEdit
      Left = 111
      Top = 99
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edSizeMinChange
    end
    object edSizeMax: TJvSpinEdit
      Left = 111
      Top = 72
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edSizeMaxChange
    end
    object edBlending: TComboBox
      Left = 111
      Top = 45
      Width = 188
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edBlendingChange
      Items.Strings = (
        'Normal'
        'Add'
        'Sub'
        'Alpha')
    end
    object edGrowthMax: TJvSpinEdit
      Left = 111
      Top = 153
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object edGrowthMin: TJvSpinEdit
      Left = 111
      Top = 126
      Width = 188
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
  end
  object GroupBox4: TGroupBox
    Left = 4
    Top = 315
    Width = 308
    Height = 98
    Align = alTop
    Caption = '&Color'
    TabOrder = 2
    object Label8: TLabel
      Left = 8
      Top = 21
      Width = 44
      Height = 13
      Caption = '&Minumum'
    end
    object Label13: TLabel
      Left = 8
      Top = 50
      Width = 48
      Height = 13
      Caption = '&Maximum:'
    end
    object edGrayscale: TCheckBox
      Left = 111
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 0
    end
    object edColorMin: TColorBox
      Left = 111
      Top = 18
      Width = 145
      Height = 22
      TabOrder = 1
      OnChange = edColorMinChange
    end
    object edColorMax: TColorBox
      Left = 111
      Top = 44
      Width = 145
      Height = 22
      TabOrder = 2
      OnChange = edColorMaxChange
    end
  end
end
