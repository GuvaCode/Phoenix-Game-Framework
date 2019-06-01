object FrmTextures: TFrmTextures
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Textures'
  ClientHeight = 519
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 4
    Top = 110
    Width = 580
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 173
    ExplicitWidth = 242
  end
  object lwTextures: TListView
    Left = 4
    Top = 4
    Width = 580
    Height = 106
    Align = alClient
    Columns = <
      item
        AutoSize = True
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lwTexturesClick
    OnKeyUp = lwTexturesKeyUp
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 112
    Width = 580
    Height = 403
    Align = alBottom
    Caption = 'Preview'
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 2
      Top = 15
      Width = 576
      Height = 386
      Align = alClient
      OnPaint = PaintBox1Paint
      ExplicitLeft = 1
      ExplicitTop = 173
      ExplicitWidth = 242
      ExplicitHeight = 169
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 28
    Top = 132
    object Loadtexture1: TMenuItem
      Caption = '&Load...'
      OnClick = Loadtexture1Click
    end
  end
end
