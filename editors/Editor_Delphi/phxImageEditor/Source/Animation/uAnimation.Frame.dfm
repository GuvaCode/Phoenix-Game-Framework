object FrmAnimationFrame: TFrmAnimationFrame
  Left = 0
  Top = 0
  Width = 322
  Height = 482
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 314
    Height = 189
    Align = alTop
    Caption = 'Frame properties'
    TabOrder = 0
    DesignSize = (
      314
      189)
    object Label5: TLabel
      Left = 8
      Top = 21
      Width = 29
      Height = 13
      Caption = '&Index:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 102
      Width = 36
      Height = 13
      Caption = '&Pattern'
    end
    object Label7: TLabel
      Left = 8
      Top = 75
      Width = 22
      Height = 13
      Caption = '&Time'
    end
    object Label1: TLabel
      Left = 8
      Top = 48
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
    object edFrameIndex: TEdit
      Left = 64
      Top = 18
      Width = 232
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object edFrameTime: TJvSpinEdit
      Left = 64
      Top = 72
      Width = 232
      Height = 21
      Value = -1.000000000000000000
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edFrameTimeChange
    end
    object edFramePatternName: TJvComboBox
      Left = 64
      Top = 136
      Width = 232
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edFrameName: TEdit
      Left = 64
      Top = 45
      Width = 232
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edFrameNameChange
    end
    object edFramePattern: TJvSpinEdit
      Left = 64
      Top = 99
      Width = 232
      Height = 21
      Value = -1.000000000000000000
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edFramePatternChange
    end
  end
end
