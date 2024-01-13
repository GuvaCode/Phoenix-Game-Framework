object FrmCharacter: TFrmCharacter
  Left = 0
  Top = 0
  Width = 250
  Height = 565
  TabOrder = 0
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 117
    Align = alTop
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    object pbPreview: TPaintBox
      Left = 0
      Top = 0
      Width = 246
      Height = 113
      Align = alClient
      OnPaint = pbPreviewPaint
      ExplicitLeft = -1
      ExplicitTop = 2
      ExplicitWidth = 188
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 425
    Width = 250
    Height = 76
    Align = alTop
    Caption = 'Flags'
    TabOrder = 1
    ExplicitTop = 469
    object edFlagWrap: TCheckBox
      Left = 64
      Top = 16
      Width = 97
      Height = 17
      Caption = 'edFlagWrap'
      TabOrder = 0
      OnClick = edFlagWrapClick
    end
    object edFlagNotStart: TCheckBox
      Left = 64
      Top = 39
      Width = 97
      Height = 17
      Caption = 'edFlagWrap'
      TabOrder = 1
      OnClick = edFlagNotStartClick
    end
    object edFlagNotEnd: TCheckBox
      Left = 64
      Top = 56
      Width = 97
      Height = 17
      Caption = 'edFlagWrap'
      TabOrder = 2
      OnClick = edFlagNotEndClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 193
    Width = 250
    Height = 132
    Align = alTop
    Caption = 'Texture coordinates'
    TabOrder = 2
    DesignSize = (
      250
      132)
    object Label2: TLabel
      Left = 8
      Top = 23
      Width = 10
      Height = 13
      Caption = '&X:'
    end
    object Label3: TLabel
      Left = 8
      Top = 50
      Width = 10
      Height = 13
      Caption = '&Y:'
    end
    object Label4: TLabel
      Left = 8
      Top = 77
      Width = 28
      Height = 13
      Caption = '&Width'
    end
    object Label5: TLabel
      Left = 8
      Top = 104
      Width = 35
      Height = 13
      Caption = '&Height:'
    end
    object edWidth: TSpinEdit
      Left = 84
      Top = 74
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edY: TSpinEdit
      Left = 84
      Top = 47
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edX: TSpinEdit
      Left = 84
      Top = 20
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edHeight: TSpinEdit
      Left = 84
      Top = 101
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 325
    Width = 250
    Height = 100
    Align = alTop
    Caption = 'Character offsets'
    TabOrder = 3
    DesignSize = (
      250
      100)
    object Label6: TLabel
      Left = 8
      Top = 22
      Width = 40
      Height = 13
      Caption = 'X Offset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 76
      Width = 40
      Height = 13
      Caption = 'Y Offset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Top = 49
      Width = 55
      Height = 13
      Caption = 'X Advance:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edXOffset: TSpinEdit
      Left = 84
      Top = 19
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edYOffset: TSpinEdit
      Left = 84
      Top = 73
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edYOffsetChange
    end
    object edXAdvance: TSpinEdit
      Left = 84
      Top = 46
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object GroupBox4: TGroupBox
    Left = 0
    Top = 117
    Width = 250
    Height = 76
    Align = alTop
    Caption = 'Character'
    TabOrder = 4
    ExplicitTop = 125
    DesignSize = (
      250
      76)
    object Label10: TLabel
      Left = 8
      Top = 23
      Width = 38
      Height = 13
      Caption = '&Unicode'
    end
    object Label11: TLabel
      Left = 8
      Top = 50
      Width = 26
      Height = 13
      Caption = '&Text:'
    end
    object JvSpinEdit1: TSpinEdit
      Left = 84
      Top = 20
      Width = 160
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edCharacterChange
    end
    object Edit1: TEdit
      Left = 84
      Top = 47
      Width = 156
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
end
