object FrmPatternProperties: TFrmPatternProperties
  Left = 0
  Height = 329
  Top = 0
  Width = 225
  BorderSpacing.Around = 4
  ClientHeight = 329
  ClientWidth = 225
  TabOrder = 0
  DesignLeft = 481
  DesignTop = 259
  object GroupBox1: TGroupBox
    Left = 0
    Height = 329
    Top = 0
    Width = 225
    Align = alClient
    Caption = 'Pattern properties'
    ClientHeight = 312
    ClientWidth = 223
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Height = 16
      Top = 7
      Width = 38
      Caption = '&Name:'
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 40
      Width = 11
      Caption = '&X:'
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Height = 14
      Top = 75
      Width = 9
      Caption = '&Y:'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Height = 16
      Top = 102
      Width = 39
      Caption = '&Width:'
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 136
      Width = 43
      Caption = '&Height:'
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Height = 16
      Top = 168
      Width = 45
      Caption = '&Pivot X:'
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Height = 16
      Top = 200
      Width = 44
      Caption = '&Pivot Y:'
      ParentFont = False
    end
    object edPatternName: TEdit
      Left = 62
      Height = 28
      Top = 7
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
      OnChange = edPatternNameChange
    end
    object edPatternX: TSpinEdit
      Left = 62
      Height = 28
      Top = 40
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternXChange
      TabOrder = 1
    end
    object edPatternY: TSpinEdit
      Left = 62
      Height = 28
      Top = 72
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternYChange
      TabOrder = 2
    end
    object edPatternWidth: TSpinEdit
      Left = 62
      Height = 28
      Top = 104
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternWidthChange
      TabOrder = 3
    end
    object edPatternHeight: TSpinEdit
      Left = 62
      Height = 28
      Top = 136
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternHeightChange
      TabOrder = 4
    end
    object edPatternPivotX: TSpinEdit
      Left = 62
      Height = 28
      Top = 168
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternPivotXChange
      TabOrder = 5
    end
    object edPatternPivotY: TSpinEdit
      Left = 62
      Height = 28
      Top = 200
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      OnChange = edPatternPivotYChange
      TabOrder = 6
    end
    object edPatternMirror: TCheckBox
      Left = 8
      Height = 23
      Top = 232
      Width = 64
      Caption = 'Mirror'
      TabOrder = 7
      OnClick = edPatternMirrorClick
    end
    object edPatternFlip: TCheckBox
      Left = 80
      Height = 23
      Top = 232
      Width = 48
      Caption = 'Flip'
      TabOrder = 8
      OnClick = edPatternFlipClick
    end
    object btnCenterPivot: TButton
      Left = 62
      Height = 25
      Top = 264
      Width = 156
      Anchors = [akTop, akLeft, akRight]
      Caption = 'Center pivot'
      TabOrder = 9
      OnClick = btnCenterPivotClick
    end
  end
end
