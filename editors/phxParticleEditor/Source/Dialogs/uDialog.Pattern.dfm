object PatternDialog: TPatternDialog
  Left = 0
  Top = 0
  Caption = 'Select pattern'
  ClientHeight = 323
  ClientWidth = 648
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
    648
    323)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 4
    Top = 4
    Width = 615
    Height = 280
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnDblClick = PaintBox1DblClick
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
    ExplicitWidth = 603
  end
  object btnOkey: TButton
    Left = 484
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Okey'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 471
    ExplicitTop = 267
  end
  object btnCancel: TButton
    Left = 565
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 552
    ExplicitTop = 267
  end
  object ScrollBar1: TScrollBar
    Left = 625
    Top = 4
    Width = 17
    Height = 280
    Anchors = [akTop, akRight, akBottom]
    Kind = sbVertical
    PageSize = 0
    TabOrder = 2
    OnChange = ScrollBar1Change
    ExplicitLeft = 613
  end
  object cbTransparent: TCheckBox
    Left = 342
    Top = 293
    Width = 136
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Transparent background'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbTransparentClick
    ExplicitLeft = 329
    ExplicitTop = 270
  end
end
