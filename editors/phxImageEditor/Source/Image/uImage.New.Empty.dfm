object FrmImageEmpty: TFrmImageEmpty
  Left = 245
  Height = 192
  Top = 203
  Width = 374
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Create a empty image'
  ClientHeight = 192
  ClientWidth = 374
  Color = clBtnFace
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Label1: TLabel
    Left = 6
    Height = 16
    Top = 8
    Width = 35
    Caption = '&Name'
  end
  object Label2: TLabel
    Left = 6
    Height = 16
    Top = 40
    Width = 36
    Caption = '&Width'
  end
  object Label3: TLabel
    Left = 6
    Height = 16
    Top = 72
    Width = 40
    Caption = '&Height'
  end
  object Label4: TLabel
    Left = 6
    Height = 16
    Top = 104
    Width = 43
    Caption = '&Format'
  end
  object edName: TEdit
    Left = 70
    Height = 28
    Top = 8
    Width = 296
    Anchors = [akTop, akLeft, akRight]
    TabOrder = 0
    Text = 'edName'
  end
  object Button1: TButton
    Left = 295
    Height = 25
    Top = 159
    Width = 75
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edWidth: TComboBox
    Left = 70
    Height = 28
    Top = 40
    Width = 296
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
    TabOrder = 2
    Text = 'edWidth'
  end
  object edheight: TComboBox
    Left = 70
    Height = 28
    Top = 72
    Width = 296
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
    TabOrder = 3
    Text = 'edHeight'
  end
  object edFormat: TComboBox
    Left = 70
    Height = 28
    Top = 104
    Width = 296
    Anchors = [akTop, akLeft, akRight]
    ItemHeight = 0
    TabOrder = 4
    Text = 'edFormat'
  end
  object Button2: TButton
    Left = 214
    Height = 25
    Top = 159
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
end
