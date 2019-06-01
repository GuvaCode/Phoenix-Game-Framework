object FrmImageResize: TFrmImageResize
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Resize image'
  ClientHeight = 121
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    234
    121)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 11
    Width = 28
    Height = 13
    Caption = '&Width'
  end
  object Label2: TLabel
    Left = 6
    Top = 38
    Width = 31
    Height = 13
    Caption = '&Height'
  end
  object Label3: TLabel
    Left = 6
    Top = 65
    Width = 34
    Height = 13
    Caption = '&Format'
  end
  object edheight: TComboBox
    Left = 70
    Top = 35
    Width = 156
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edHeight'
    Items.Strings = (
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object edWidth: TComboBox
    Left = 70
    Top = 8
    Width = 156
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'edWidth'
    Items.Strings = (
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object edFormat: TComboBox
    Left = 70
    Top = 62
    Width = 156
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'edFormat'
  end
  object Button2: TButton
    Left = 74
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button1: TButton
    Left = 155
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
