object FrmPatternSearch: TFrmPatternSearch
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Pattern Search'
  ClientHeight = 150
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    282
    150)
  PixelsPerInch = 96
  TextHeight = 13
  object edSearch: TEdit
    Left = 8
    Top = 8
    Width = 266
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edSearchChange
    OnKeyDown = edSearchKeyDown
  end
  object lwPatterns: TListBox
    Left = 8
    Top = 35
    Width = 266
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = lwPatternsClick
    OnDblClick = lwPatternsDblClick
    OnKeyUp = lwPatternsKeyUp
  end
end
