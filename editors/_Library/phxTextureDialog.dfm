object FrmTextureDialog: TFrmTextureDialog
  Left = 274
  Height = 331
  Top = 201
  Width = 584
  Caption = 'Select texture'
  ClientHeight = 331
  ClientWidth = 584
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnResize = FormResize
  Position = poOwnerFormCenter
  LCLVersion = '3.0.0.3'
  object PaintBox1: TPaintBox
    Left = 0
    Height = 296
    Top = 0
    Width = 571
    Align = alClient
    OnDblClick = PaintBox1DblClick
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object ScrollBar1: TScrollBar
    Left = 571
    Height = 296
    Top = 0
    Width = 13
    Align = alRight
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = ScrollBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Height = 35
    Top = 296
    Width = 584
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 35
    ClientWidth = 584
    ParentBackground = False
    TabOrder = 1
    object Label1: TLabel
      Cursor = crHandPoint
      Left = 8
      Height = 14
      Top = 12
      Width = 85
      Caption = 'Load &textures...'
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label1Click
    end
    object btnOkey: TButton
      Left = 420
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'Okey'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 501
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object cbTransparent: TCheckBox
      Left = 257
      Height = 23
      Top = 10
      Width = 160
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
