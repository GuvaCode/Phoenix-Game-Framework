object FrmGeneratorDebug: TFrmGeneratorDebug
  Left = 0
  Height = 160
  Top = 0
  Width = 287
  Caption = 'FrmGeneratorDebug'
  ClientHeight = 160
  ClientWidth = 287
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object GroupBox5: TGroupBox
    Left = 8
    Height = 130
    Top = 8
    Width = 274
    Caption = 'Character information'
    ClientHeight = 115
    ClientWidth = 272
    ParentBackground = False
    TabOrder = 0
    object Label3: TLabel
      Left = 175
      Height = 14
      Top = 23
      Width = 37
      Caption = 'Label3'
    end
    object PaintBox1: TPaintBox
      Left = 8
      Height = 79
      Top = 42
      Width = 161
      OnPaint = PaintBox1Paint
    end
    object cbCharacter: TComboBox
      Left = 8
      Height = 27
      Top = 15
      Width = 161
      ItemHeight = 0
      TabOrder = 0
      Text = 'cbCharacter'
      OnChange = cbCharacterChange
    end
  end
end
