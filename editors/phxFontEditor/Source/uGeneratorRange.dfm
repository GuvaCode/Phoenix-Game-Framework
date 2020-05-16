object FrmGeneratorRange: TFrmGeneratorRange
  Left = 0
  Top = 0
  Caption = 'Select character range'
  ClientHeight = 430
  ClientWidth = 676
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnResize = FormResize
  DesignSize = (
    676
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 8
    Top = 8
    Width = 443
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnPaint = PaintBox1Paint
    ExplicitWidth = 415
    ExplicitHeight = 353
  end
  object ScrollBar1: TScrollBar
    Left = 457
    Top = 8
    Width = 17
    Height = 366
    Anchors = [akTop, akRight, akBottom]
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = ScrollBar1Change
    ExplicitLeft = 429
    ExplicitHeight = 353
  end
  object lwCodebase: TListBox
    Left = 480
    Top = 8
    Width = 188
    Height = 366
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitLeft = 452
    ExplicitHeight = 353
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 411
    Width = 676
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
    ExplicitTop = 398
    ExplicitWidth = 648
  end
  object btnImport: TButton
    Left = 89
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Import'
    TabOrder = 3
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 593
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ExplicitLeft = 565
    ExplicitTop = 367
  end
  object btnOk: TButton
    Left = 512
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 5
    ExplicitLeft = 484
    ExplicitTop = 367
  end
  object btnClear: TButton
    Left = 8
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 6
    OnClick = btnClearClick
    ExplicitTop = 367
  end
  object OpenTextFileDialog1: TOpenDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 24
    Top = 40
  end
end
