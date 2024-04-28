object FrmParticles: TFrmParticles
  Left = 0
  Top = 0
  Caption = 'FrmParticles'
  ClientHeight = 269
  ClientWidth = 606
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lwParticles: TListView
    Left = 0
    Top = 0
    Width = 606
    Height = 269
    Align = alClient
    Columns = <
      item
        Caption = '#'
        Width = 20
      end
      item
        Caption = 'Time'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Life'
        Width = 64
      end
      item
        Alignment = taRightJustify
        Caption = 'Energy'
        Width = 64
      end
      item
        Caption = 'Alpha'
        Width = 60
      end
      item
        Caption = 'Alive'
        Width = 60
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 610
    ExplicitHeight = 122
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 296
    Top = 136
  end
end
