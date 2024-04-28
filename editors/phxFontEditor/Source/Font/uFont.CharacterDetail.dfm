object FrmCharacter: TFrmCharacter
  Left = 0
  Height = 565
  Top = 0
  Width = 250
  ClientHeight = 565
  ClientWidth = 250
  TabOrder = 0
  object Panel6: TPanel
    Left = 0
    Height = 117
    Top = 0
    Width = 250
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 117
    ClientWidth = 250
    TabOrder = 0
    object pbPreview: TPaintBox
      Left = 0
      Height = 117
      Top = 0
      Width = 250
      Align = alClient
      OnPaint = pbPreviewPaint
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Height = 76
    Top = 425
    Width = 250
    Align = alTop
    Caption = 'Flags'
    ClientHeight = 59
    ClientWidth = 248
    TabOrder = 1
    object edFlagWrap: TCheckBox
      Left = 64
      Height = 23
      Top = 16
      Width = 99
      Caption = 'edFlagWrap'
      TabOrder = 0
      OnClick = edFlagWrapClick
    end
    object edFlagNotStart: TCheckBox
      Left = 64
      Height = 23
      Top = 39
      Width = 99
      Caption = 'edFlagWrap'
      TabOrder = 1
      OnClick = edFlagNotStartClick
    end
    object edFlagNotEnd: TCheckBox
      Left = 64
      Height = 23
      Top = 56
      Width = 99
      Caption = 'edFlagWrap'
      TabOrder = 2
      OnClick = edFlagNotEndClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Height = 132
    Top = 193
    Width = 250
    Align = alTop
    Caption = 'Texture coordinates'
    ClientHeight = 115
    ClientWidth = 248
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Height = 16
      Top = 23
      Width = 11
      Caption = '&X:'
    end
    object Label3: TLabel
      Left = 8
      Height = 16
      Top = 50
      Width = 10
      Caption = '&Y:'
    end
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 77
      Width = 36
      Caption = '&Width'
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 104
      Width = 43
      Caption = '&Height:'
    end
    object edWidth: TSpinEdit
      Left = 84
      Height = 28
      Top = 74
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
    end
    object edY: TSpinEdit
      Left = 84
      Height = 28
      Top = 47
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 1
    end
    object edX: TSpinEdit
      Left = 84
      Height = 28
      Top = 20
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 2
    end
    object edHeight: TSpinEdit
      Left = 84
      Height = 28
      Top = 101
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 3
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Height = 100
    Top = 325
    Width = 250
    Align = alTop
    Caption = 'Character offsets'
    ClientHeight = 83
    ClientWidth = 248
    TabOrder = 3
    object Label6: TLabel
      Left = 8
      Height = 14
      Top = 22
      Width = 45
      Caption = 'X Offset'
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Height = 14
      Top = 76
      Width = 44
      Caption = 'Y Offset'
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'Tahoma'
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Height = 14
      Top = 49
      Width = 63
      Caption = 'X Advance:'
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      ParentFont = False
    end
    object edXOffset: TSpinEdit
      Left = 84
      Height = 28
      Top = 19
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
    end
    object edYOffset: TSpinEdit
      Left = 84
      Height = 28
      Top = 73
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edYOffsetChange
      TabOrder = 1
    end
    object edXAdvance: TSpinEdit
      Left = 84
      Height = 28
      Top = 46
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 2
    end
  end
  object GroupBox4: TGroupBox
    Left = 0
    Height = 76
    Top = 117
    Width = 250
    Align = alTop
    Caption = 'Character'
    ClientHeight = 59
    ClientWidth = 248
    TabOrder = 4
    object Label10: TLabel
      Left = 8
      Height = 16
      Top = 23
      Width = 49
      Caption = '&Unicode'
    end
    object Label11: TLabel
      Left = 8
      Height = 16
      Top = 50
      Width = 29
      Caption = '&Text:'
    end
    object JvSpinEdit1: TSpinEdit
      Left = 84
      Height = 28
      Top = 20
      Width = 160
      Anchors = [akTop, akLeft, akRight]
      OnChange = edCharacterChange
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 84
      Height = 28
      Top = 47
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
end
