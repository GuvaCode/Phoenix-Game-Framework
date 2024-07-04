object FrmEffectEmittor: TFrmEffectEmittor
  Left = 0
  Top = 0
  Width = 400
  Height = 390
  Padding.Left = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  DesignSize = (
    400
    390)
  object Label10: TLabel
    Left = 8
    Top = 52
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Delay:'
  end
  object Label11: TLabel
    Left = 8
    Top = 80
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Count:'
  end
  object Label12: TLabel
    Left = 8
    Top = 107
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Repeats'
  end
  object Label13: TLabel
    Left = 8
    Top = 25
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Mode:'
  end
  object Label2: TLabel
    Left = 8
    Top = 153
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Shape'
  end
  object Label5: TLabel
    Left = 8
    Top = 3
    Width = 302
    Height = 13
    AutoSize = False
    Caption = 'Basic'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 130
    Width = 302
    Height = 13
    AutoSize = False
    Caption = 'Points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PanelPoints: TPanel
    Left = 74
    Top = 198
    Width = 319
    Height = 164
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 0
    ExplicitWidth = 350
    object PaintBox1: TPaintBox
      Left = 4
      Top = 4
      Width = 311
      Height = 156
      Align = alClient
      OnPaint = PaintBox1Paint
      ExplicitWidth = 347
      ExplicitHeight = 181
    end
  end
  object edPointsInOrder: TCheckBox
    Left = 74
    Top = 177
    Width = 97
    Height = 15
    Caption = 'Points in order'
    TabOrder = 1
    OnClick = edPointsInOrderClick
  end
  object edRepeats: TJvSpinEdit
    Left = 74
    Top = 103
    Width = 319
    Height = 21
    Hint = 'Number of emissions before the system dies'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnChange = edRepeatsChange
    ExplicitWidth = 350
  end
  object edCount: TJvSpinEdit
    Left = 74
    Top = 77
    Width = 319
    Height = 21
    Hint = 'Number of particles to emit each burst'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnChange = edCountChange
    ExplicitWidth = 350
  end
  object edDelay: TJvSpinEdit
    Left = 74
    Top = 49
    Width = 319
    Height = 21
    Hint = 'Delay between each emission'
    Increment = 0.100000000000000000
    ValueType = vtFloat
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnChange = edDelayChange
    ExplicitWidth = 350
  end
  object edMode: TComboBox
    Left = 74
    Top = 22
    Width = 319
    Height = 21
    Hint = 
      'Toggles if the particles should be emitted over time or over dis' +
      'tance traveled'
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
    OnChange = edModeChange
    Items.Strings = (
      'Time'
      'Distance')
    ExplicitWidth = 350
  end
  object edShape: TJvComboEdit
    Left = 74
    Top = 150
    Width = 319
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DisabledColor = clBtnFace
    TabOrder = 6
    OnButtonClick = edShapeButtonClick
    OnChange = edShapeChange
    ExplicitWidth = 350
  end
end
