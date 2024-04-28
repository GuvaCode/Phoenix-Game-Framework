object FrmDialogTexture: TFrmDialogTexture
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Texture'
  ClientHeight = 96
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    329
    96)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 24
    Height = 13
    Caption = '&Kind:'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 31
    Height = 13
    Caption = '&Name:'
  end
  object btnCancel: TButton
    Left = 246
    Top = 63
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    ExplicitLeft = 552
    ExplicitTop = 267
  end
  object btnOk: TButton
    Left = 165
    Top = 63
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 471
    ExplicitTop = 267
  end
  object edTextureKind: TComboBox
    Left = 45
    Top = 8
    Width = 276
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Items.Strings = (
      'Custom'
      'Diffuse'
      'Normal'
      'Team'
      'Alphamap'
      'Displacement')
    ExplicitWidth = 207
  end
  object edTextureName: TEdit
    Left = 45
    Top = 35
    Width = 276
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'edTextureName'
    OnButtonClick = edTextureNameButtonClick
    ExplicitWidth = 207
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 56
  end
end
