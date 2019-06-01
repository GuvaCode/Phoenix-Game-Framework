object FrmTexture: TFrmTexture
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Texture settings'
  ClientHeight = 179
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 95
    Width = 36
    Height = 13
    Caption = 'WrapS:'
  end
  object Label2: TLabel
    Left = 8
    Top = 122
    Width = 36
    Height = 13
    Caption = 'WrapT:'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 84
    Height = 13
    Caption = 'Minifying function'
  end
  object Label4: TLabel
    Left = 8
    Top = 66
    Width = 105
    Height = 13
    Caption = 'Magnification function'
  end
  object edWrapS: TComboBox
    Left = 126
    Top = 92
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 0
    Text = 'Repeat'
    Items.Strings = (
      'Repeat'
      'Clamp')
  end
  object edWrapT: TComboBox
    Left = 126
    Top = 119
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 1
    Text = 'Repeat'
    Items.Strings = (
      'Repeat'
      'Clamp')
  end
  object edFilterMin: TComboBox
    Left = 126
    Top = 36
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 2
    Text = 'Repeat'
    Items.Strings = (
      'Repeat'
      'Clamp')
  end
  object edFilterMag: TComboBox
    Left = 126
    Top = 63
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 3
    Text = 'Repeat'
    Items.Strings = (
      'Repeat'
      'Clamp')
  end
  object cbMipmaps: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Mipmapped'
    TabOrder = 4
  end
  object Panel2: TPanel
    Left = 0
    Top = 147
    Width = 276
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
    object Panel1: TPanel
      Left = 115
      Top = 0
      Width = 161
      Height = 32
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOk: TButton
        Left = 0
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object Button4: TButton
        Left = 81
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
