object FrmFontKernings: TFrmFontKernings
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object lwKernings: TListView
    Left = 4
    Top = 4
    Width = 312
    Height = 232
    Align = alClient
    Columns = <
      item
        Caption = 'First'
        Width = 75
      end
      item
        Caption = 'Second'
        Width = 75
      end
      item
        Caption = 'Amount'
        Width = 75
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
