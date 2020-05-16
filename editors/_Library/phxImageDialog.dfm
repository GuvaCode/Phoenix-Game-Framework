object FrmIconSelector: TFrmIconSelector
  Left = 582
  Top = 378
  Caption = 'Select Image'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 4
    Top = 527
    Width = 776
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      776
      35)
    object lblLoadImage: TLabel
      Left = 8
      Top = 12
      Width = 66
      Height = 13
      Cursor = crHandPoint
      Caption = 'Load &image...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblLoadImageClick
    end
    object btnOkey: TButton
      Left = 612
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 693
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
      Left = 386
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
    object btnNone: TButton
      Left = 531
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Select none'
      ModalResult = 1
      TabOrder = 3
      OnClick = btnNoneClick
    end
  end
  object TabControl1: TTabControl
    Left = 189
    Top = 4
    Width = 591
    Height = 523
    Align = alClient
    TabOrder = 1
    Tabs.Strings = (
      'Patterns'
      'Image')
    TabIndex = 0
    OnChange = TabControl1Change
    object PaintBox1: TPaintBox
      Left = 4
      Top = 24
      Width = 566
      Height = 495
      Align = alClient
      OnMouseUp = PaintBox1MouseUp
      OnPaint = PaintBox1Paint
      ExplicitLeft = 6
      ExplicitTop = 25
      ExplicitWidth = 390
      ExplicitHeight = 290
    end
    object ScrollBar1: TScrollBar
      Left = 570
      Top = 24
      Width = 17
      Height = 495
      Align = alRight
      Kind = sbVertical
      PageSize = 0
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 185
    Height = 523
    Align = alLeft
    Caption = 'Images'
    Padding.Left = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 2
    object lwImages: TListBox
      Left = 6
      Top = 15
      Width = 173
      Height = 502
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
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
