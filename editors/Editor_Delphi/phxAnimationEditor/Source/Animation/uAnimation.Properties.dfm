object FrmAnimationProperties: TFrmAnimationProperties
  Left = 0
  Top = 0
  Width = 298
  Height = 283
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 290
    Height = 275
    Align = alClient
    Caption = 'Animation properties'
    TabOrder = 0
    ExplicitHeight = 277
    DesignSize = (
      290
      275)
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
      Top = 224
      Width = 53
      Height = 13
      Caption = '&Frame rate'
    end
    object Label2: TLabel
      Left = 8
      Top = 197
      Width = 30
      Height = 13
      Caption = '&Image'
    end
    object Label6: TLabel
      Left = 8
      Top = 102
      Width = 49
      Height = 13
      Caption = '&Comment:'
    end
    object Label3: TLabel
      Left = 8
      Top = 75
      Width = 39
      Height = 13
      Caption = '&Version:'
    end
    object Label5: TLabel
      Left = 8
      Top = 48
      Width = 37
      Height = 13
      Caption = '&Author:'
    end
    object edAnimationName: TEdit
      Left = 75
      Top = 18
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edAnimationNameChange
      ExplicitWidth = 230
    end
    object edAnimationImage: TComboBox
      Left = 75
      Top = 194
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edAnimationImageChange
    end
    object edAnimationFrameRate: TJvSpinEdit
      Left = 75
      Top = 221
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edAnimationFrameRateChange
    end
    object edAnimationLooped: TCheckBox
      Left = 75
      Top = 248
      Width = 97
      Height = 17
      Caption = 'Looped'
      TabOrder = 3
      OnClick = edAnimationLoopedClick
    end
    object edAnimationAuthor: TEdit
      Left = 75
      Top = 45
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edAnimationAuthorChange
    end
    object edAnimationVersion: TEdit
      Left = 75
      Top = 72
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edAnimationVersionChange
    end
    object edAnimationComment: TMemo
      Left = 75
      Top = 99
      Width = 208
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = edAnimationCommentChange
    end
  end
end
