object FrmPatternSearch: TFrmPatternSearch
  Left = 0
  Height = 150
  Top = 0
  Width = 282
  BorderStyle = bsSizeToolWin
  Caption = 'Pattern Search'
  ClientHeight = 150
  ClientWidth = 282
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object edSearch: TEdit
    Left = 8
    Height = 28
    Top = 8
    Width = 266
    Anchors = [akTop, akLeft, akRight]
    TabOrder = 0
    OnChange = edSearchChange
    OnKeyDown = edSearchKeyDown
  end
  object lwPatterns: TListBox
    Left = 8
    Height = 107
    Top = 35
    Width = 266
    Anchors = [akTop, akLeft, akRight, akBottom]
    ItemHeight = 0
    TabOrder = 1
    TopIndex = -1
    OnClick = lwPatternsClick
    OnDblClick = lwPatternsDblClick
    OnKeyUp = lwPatternsKeyUp
  end
end
