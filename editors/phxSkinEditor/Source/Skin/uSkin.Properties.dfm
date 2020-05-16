object FrmSkinProperties: TFrmSkinProperties
  Left = 0
  Top = 0
  Width = 320
  Height = 520
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox2: TGroupBox
    Left = 4
    Top = 4
    Width = 312
    Height = 195
    Align = alTop
    Caption = 'Skin properties'
    TabOrder = 0
    DesignSize = (
      312
      195)
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 37
      Height = 13
      Caption = '&Author:'
    end
    object Label3: TLabel
      Left = 8
      Top = 75
      Width = 39
      Height = 13
      Caption = '&Version:'
    end
    object Label8: TLabel
      Left = 8
      Top = 102
      Width = 49
      Height = 13
      Caption = '&Comment:'
    end
    object edName: TEdit
      Left = 68
      Top = 18
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edAuthor: TEdit
      Left = 68
      Top = 45
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edAuthorChange
    end
    object edComment: TMemo
      Left = 68
      Top = 99
      Width = 237
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      WordWrap = False
      OnChange = edCommentChange
    end
    object edVersion: TEdit
      Left = 68
      Top = 72
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edVersionChange
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 199
    Width = 312
    Height = 106
    Align = alTop
    Caption = 'Texture'
    TabOrder = 1
    DesignSize = (
      312
      106)
    object Label5: TLabel
      Left = 8
      Top = 21
      Width = 28
      Height = 13
      Caption = '&Width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 48
      Width = 31
      Height = 13
      Caption = '&Height'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 75
      Width = 34
      Height = 13
      Caption = '&Format'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edTextureWidth: TEdit
      Left = 68
      Top = 18
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object edTextureHeight: TEdit
      Left = 68
      Top = 45
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edTextureFormat: TEdit
      Left = 68
      Top = 72
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
  end
end
