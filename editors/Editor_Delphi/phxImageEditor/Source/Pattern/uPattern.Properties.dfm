object FrmPatternProperties: TFrmPatternProperties
  Left = 0
  Top = 0
  Width = 202
  Height = 282
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 202
    Height = 282
    Align = alClient
    Caption = 'Pattern properties'
    TabOrder = 0
    DesignSize = (
      202
      282)
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = '&X:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 75
      Width = 10
      Height = 13
      Caption = '&Y:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 102
      Width = 31
      Height = 13
      Caption = '&Width:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 129
      Width = 34
      Height = 13
      Caption = '&Height:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 156
      Width = 37
      Height = 13
      Caption = '&Pivot X:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 183
      Width = 37
      Height = 13
      Caption = '&Pivot Y:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edPatternName: TEdit
      Left = 54
      Top = 18
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edPatternNameChange
    end
    object edPatternX: TJvSpinEdit
      Left = 54
      Top = 45
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edPatternXChange
    end
    object edPatternY: TJvSpinEdit
      Left = 54
      Top = 72
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edPatternYChange
    end
    object edPatternWidth: TJvSpinEdit
      Left = 54
      Top = 99
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edPatternWidthChange
    end
    object edPatternHeight: TJvSpinEdit
      Left = 54
      Top = 126
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edPatternHeightChange
    end
    object edPatternPivotX: TJvSpinEdit
      Left = 54
      Top = 153
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edPatternPivotXChange
    end
    object edPatternPivotY: TJvSpinEdit
      Left = 54
      Top = 180
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = edPatternPivotYChange
    end
    object edPatternMirror: TCheckBox
      Left = 54
      Top = 207
      Width = 97
      Height = 17
      Caption = 'Mirror'
      TabOrder = 7
      OnClick = edPatternMirrorClick
    end
    object edPatternFlip: TCheckBox
      Left = 54
      Top = 230
      Width = 97
      Height = 17
      Caption = 'Flip'
      TabOrder = 8
      OnClick = edPatternFlipClick
    end
    object btnCenterPivot: TButton
      Left = 54
      Top = 253
      Width = 103
      Height = 25
      Caption = 'Center pivot'
      TabOrder = 9
      OnClick = btnCenterPivotClick
    end
  end
end
