object FrmIconSelector: TFrmIconSelector
  Left = 357
  Height = 562
  Top = 219
  Width = 784
  Caption = 'Select Image'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poMainFormCenter
  LCLVersion = '3.0.0.3'
  object Panel1: TPanel
    Left = 0
    Height = 35
    Top = 527
    Width = 784
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 35
    ClientWidth = 784
    ParentBackground = False
    TabOrder = 0
    object lblLoadImage: TLabel
      Cursor = crHandPoint
      Left = 8
      Height = 14
      Top = 12
      Width = 74
      Caption = 'Load &image...'
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblLoadImageClick
    end
    object btnOkey: TButton
      Left = 620
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 701
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
      Left = 370
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
    object btnNone: TButton
      Left = 539
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'Select none'
      ModalResult = 1
      TabOrder = 3
      OnClick = btnNoneClick
    end
  end
  object TabControl1: TTabControl
    Left = 185
    Height = 527
    Top = 0
    Width = 599
    OnChange = TabControl1Change
    TabIndex = 0
    Tabs.Strings = (
      'Patterns'
      'Image'
    )
    Align = alClient
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 2
      Height = 497
      Top = 28
      Width = 582
      Align = alClient
      OnMouseUp = PaintBox1MouseUp
      OnPaint = PaintBox1Paint
    end
    object ScrollBar1: TScrollBar
      Left = 584
      Height = 497
      Top = 28
      Width = 13
      Align = alRight
      Kind = sbVertical
      PageSize = 0
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Height = 527
    Top = 0
    Width = 185
    Align = alLeft
    Caption = 'Images'
    ClientHeight = 512
    ClientWidth = 183
    ParentBackground = False
    TabOrder = 2
    object lwImages: TListBox
      Left = 0
      Height = 512
      Top = 0
      Width = 183
      Align = alClient
      ItemHeight = 0
      TabOrder = 0
      TopIndex = -1
      OnClick = lwImagesClick
      OnDblClick = lwImagesDblClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.phximg'
    Filter = 'Phoenix image file (*.phximg)|*.phximg|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 54
    Top = 206
  end
end
