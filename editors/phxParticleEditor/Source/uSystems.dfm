object FrmSystems: TFrmSystems
  Left = 0
  Top = 0
  Width = 610
  Height = 122
  TabOrder = 0
  object lwSystems: TListView
    Left = 0
    Top = 0
    Width = 610
    Height = 122
    Align = alClient
    Columns = <
      item
        Caption = '#'
        Width = 20
      end
      item
        Caption = 'Effect'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Count'
        Width = 64
      end
      item
        Alignment = taRightJustify
        Caption = 'Time'
        Width = 64
      end
      item
        Caption = 'Active'
        Width = 60
      end
      item
        Caption = 'Alive'
        Width = 60
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PopupMenu1: TPopupMenu
    Left = 288
    Top = 48
    object Showparticles1: TMenuItem
      Caption = 'Show particles'
      OnClick = Showparticles1Click
    end
    object EditShowInactive: TMenuItem
      AutoCheck = True
      Caption = 'Show inactive'
      OnClick = EditShowInactiveClick
    end
  end
end
