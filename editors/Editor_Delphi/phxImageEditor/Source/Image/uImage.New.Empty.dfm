object FrmImageEmpty: TFrmImageEmpty
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Create a empty image'
  ClientHeight = 192
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    374
    192)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 11
    Width = 27
    Height = 13
    Caption = '&Name'
  end
  object Label2: TLabel
    Left = 6
    Top = 38
    Width = 28
    Height = 13
    Caption = '&Width'
  end
  object Label3: TLabel
    Left = 6
    Top = 65
    Width = 31
    Height = 13
    Caption = '&Height'
  end
  object Label4: TLabel
    Left = 6
    Top = 92
    Width = 34
    Height = 13
    Caption = '&Format'
  end
  object edName: TEdit
    Left = 70
    Top = 8
    Width = 296
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edName'
    ExplicitWidth = 156
  end
  object Button1: TButton
    Left = 295
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 155
    ExplicitTop = 111
  end
  object edWidth: TComboBox
    Left = 70
    Top = 35
    Width = 296
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
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
    ExplicitWidth = 156
  end
  object edheight: TComboBox
    Left = 70
    Top = 62
    Width = 296
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
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
    ExplicitWidth = 156
  end
  object edFormat: TComboBox
    Left = 70
    Top = 89
    Width = 296
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'edFormat'
    ExplicitWidth = 156
  end
  object Button2: TButton
    Left = 214
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 5
    ExplicitLeft = 74
    ExplicitTop = 111
  end
end
