object FrmImageResize: TFrmImageResize
  Left = 475
  Height = 165
  Top = 210
  Width = 234
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Resize image'
  ClientHeight = 165
  ClientWidth = 234
  Color = clBtnFace
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Label1: TLabel
    Left = 6
    Height = 16
    Top = 8
    Width = 36
    Caption = '&Width'
  end
  object Label2: TLabel
    Left = 6
    Height = 16
    Top = 40
    Width = 40
    Caption = '&Height'
  end
  object Label3: TLabel
    Left = 6
    Height = 16
    Top = 72
    Width = 43
    Caption = '&Format'
  end
  object edheight: TComboBox
    Left = 70
    Height = 28
    Top = 40
    Width = 156
    Anchors = [akTop, akLeft, akRight]
    ItemHeight = 0
    Items.Strings = (
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192'
    )
    TabOrder = 0
    Text = 'edHeight'
  end
  object edWidth: TComboBox
    Left = 70
    Height = 28
    Top = 8
    Width = 156
    Anchors = [akTop, akLeft, akRight]
    ItemHeight = 0
    Items.Strings = (
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192'
    )
    TabOrder = 1
    Text = 'edWidth'
  end
  object edFormat: TComboBox
    Left = 70
    Height = 28
    Top = 72
    Width = 156
    Anchors = [akTop, akLeft, akRight]
    ItemHeight = 0
    TabOrder = 2
    Text = 'edFormat'
  end
  object Button2: TButton
    Left = 74
    Height = 25
    Top = 132
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button1: TButton
    Left = 155
    Height = 25
    Top = 132
    Width = 75
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
