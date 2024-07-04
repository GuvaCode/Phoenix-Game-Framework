object TextureDialog: TTextureDialog
  Left = 0
  Top = 0
  Caption = 'TextureDialog'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 618
    Height = 265
    Align = alClient
    ExplicitLeft = 194
    ExplicitTop = 90
    ExplicitWidth = 345
    ExplicitHeight = 233
  end
  object ScrollBar1: TScrollBar
    Left = 618
    Top = 0
    Width = 17
    Height = 265
    Align = alRight
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    ExplicitLeft = 567
    ExplicitHeight = 296
  end
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 635
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 584
    DesignSize = (
      635
      35)
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 79
      Height = 13
      Cursor = crHandPoint
      Caption = 'Load &textures...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object btnOkey: TButton
      Left = 471
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Okey'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 420
    end
    object btnCancel: TButton
      Left = 552
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 501
    end
    object cbTransparent: TCheckBox
      Left = 332
      Top = 10
      Width = 136
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Transparent background'
      Checked = True
      State = cbChecked
      TabOrder = 2
      ExplicitLeft = 281
    end
  end
  object OpenImageDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 268
    Top = 172
  end
end
