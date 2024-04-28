object FrmInspector: TFrmInspector
  Left = 0
  Top = 0
  Width = 236
  Height = 407
  TabOrder = 0
  object cbControls: TComboBox
    Left = 0
    Top = 0
    Width = 236
    Height = 22
    Align = alTop
    Style = csOwnerDrawFixed
    TabOrder = 0
    OnChange = cbControlsChange
    OnDrawItem = cbControlsDrawItem
  end
  object Inspector: TJvInspector
    Left = 0
    Top = 26
    Width = 236
    Height = 315
    Style = isItemPainter
    Align = alClient
    ItemHeight = 16
    Painter = InspectorPainter
    TabStop = True
    TabOrder = 1
    OnDataValueChanged = InspectorDataValueChanged
    OnItemSelected = InspectorItemSelected
  end
  object Panel1: TPanel
    Left = 0
    Top = 22
    Width = 236
    Height = 4
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 0
    Top = 341
    Width = 236
    Height = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel3: TPanel
    Left = 0
    Top = 345
    Width = 236
    Height = 62
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 4
    object lblProperty: TLabel
      Left = 6
      Top = 6
      Width = 224
      Height = 16
      Align = alTop
      AutoSize = False
      Caption = 'lblProperty'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 2
      ExplicitTop = 2
      ExplicitWidth = 308
    end
    object lblDescription: TLabel
      Left = 6
      Top = 22
      Width = 224
      Height = 34
      Align = alClient
      Caption = 'lblDescription'
      WordWrap = True
      ExplicitWidth = 63
      ExplicitHeight = 13
    end
  end
  object InspectorPainter: TJvInspectorDotNETPainter
    CategoryFont.Charset = DEFAULT_CHARSET
    CategoryFont.Color = clBtnText
    CategoryFont.Height = -11
    CategoryFont.Name = 'Tahoma'
    CategoryFont.Style = []
    NameFont.Charset = DEFAULT_CHARSET
    NameFont.Color = clWindowText
    NameFont.Height = -11
    NameFont.Name = 'Tahoma'
    NameFont.Style = []
    ValueFont.Charset = DEFAULT_CHARSET
    ValueFont.Color = clWindowText
    ValueFont.Height = -11
    ValueFont.Name = 'Tahoma'
    ValueFont.Style = []
    DrawNameEndEllipsis = False
    HideSelectFont.Charset = DEFAULT_CHARSET
    HideSelectFont.Color = clHighlightText
    HideSelectFont.Height = -11
    HideSelectFont.Name = 'Tahoma'
    HideSelectFont.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clHighlightText
    SelectedFont.Height = -11
    SelectedFont.Name = 'Tahoma'
    SelectedFont.Style = []
    Left = 120
    Top = 240
  end
end
