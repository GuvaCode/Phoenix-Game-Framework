object FrmTextureDialog: TFrmTextureDialog
  Left = 0
  Top = 0
  Caption = 'Select texture'
  ClientHeight = 331
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 567
    Height = 296
    Align = alClient
    OnDblClick = PaintBox1DblClick
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
    ExplicitLeft = 194
    ExplicitTop = 90
    ExplicitWidth = 345
    ExplicitHeight = 233
  end
  object ScrollBar1: TScrollBar
    Left = 567
    Top = 0
    Width = 17
    Height = 296
    Align = alRight
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = ScrollBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 584
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      584
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
      OnClick = Label1Click
    end
    object btnOkey: TButton
      Left = 420
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Okey'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 501
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object cbTransparent: TCheckBox
      Left = 281
      Top = 10
      Width = 136
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Transparent background'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbTransparentClick
    end
  end
  object OpenImageDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 268
    Top = 172
  end
end
