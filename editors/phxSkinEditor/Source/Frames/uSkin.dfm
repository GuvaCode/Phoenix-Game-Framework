object FrmSkin: TFrmSkin
  Left = 0
  Top = 0
  Width = 205
  Height = 240
  TabOrder = 0
  DesignSize = (
    205
    240)
  object Label1: TLabel
    Left = 4
    Top = 7
    Width = 27
    Height = 13
    Caption = '&Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 4
    Top = 34
    Width = 57
    Height = 13
    Caption = '&Compressor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 4
    Top = 61
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
  object Label4: TLabel
    Left = 4
    Top = 88
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
  object Label5: TLabel
    Left = 4
    Top = 115
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
  object btnTexture: TButton
    Left = 92
    Top = 139
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Texture settings'
    TabOrder = 0
    OnClick = btnTextureClick
  end
  object edName: TEdit
    Left = 69
    Top = 4
    Width = 128
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edNameChange
  end
  object edCompressor: TComboBox
    Left = 69
    Top = 31
    Width = 128
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edWidth: TEdit
    Left = 69
    Top = 58
    Width = 128
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
  end
  object edHeight: TEdit
    Left = 69
    Top = 85
    Width = 128
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object edFormat: TEdit
    Left = 69
    Top = 112
    Width = 128
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
end
