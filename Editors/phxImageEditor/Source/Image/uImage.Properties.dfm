object FrmImageProperties: TFrmImageProperties
  Left = 0
  Top = 0
  Width = 250
  Height = 412
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 0
    Top = 195
    Width = 250
    Height = 126
    Align = alTop
    Caption = 'Texture'
    TabOrder = 0
    DesignSize = (
      250
      126)
    object Label4: TLabel
      Left = 12
      Top = 22
      Width = 32
      Height = 13
      Caption = '&Width:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 12
      Top = 44
      Width = 35
      Height = 13
      Caption = '&Height:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 12
      Top = 76
      Width = 38
      Height = 13
      Caption = '&Format:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edTextureWidth: TEdit
      Left = 68
      Top = 14
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object edTextureHeight: TEdit
      Left = 68
      Top = 41
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edTextureFormat: TEdit
      Left = 68
      Top = 68
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object btnTexture: TButton
      Left = 138
      Top = 95
      Width = 105
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Texture settings'
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 250
    Height = 195
    Align = alTop
    Caption = 'Image properties'
    TabOrder = 1
    DesignSize = (
      250
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
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edAuthor: TEdit
      Left = 68
      Top = 45
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edAuthorChange
    end
    object edComment: TMemo
      Left = 68
      Top = 99
      Width = 175
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edCommentChange
    end
    object edVersion: TEdit
      Left = 68
      Top = 72
      Width = 175
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edVersionChange
    end
  end
end
