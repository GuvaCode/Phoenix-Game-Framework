object FrmGeneratorDebug: TFrmGeneratorDebug
  Left = 0
  Top = 0
  Caption = 'FrmGeneratorDebug'
  ClientHeight = 160
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox5: TGroupBox
    Left = 8
    Top = 8
    Width = 274
    Height = 130
    Caption = 'Character information'
    TabOrder = 0
    object Label3: TLabel
      Left = 175
      Top = 23
      Width = 31
      Height = 13
      Caption = 'Label3'
    end
    object PaintBox1: TPaintBox
      Left = 8
      Top = 42
      Width = 161
      Height = 79
      OnPaint = PaintBox1Paint
    end
    object cbCharacter: TComboBox
      Left = 8
      Top = 15
      Width = 161
      Height = 21
      TabOrder = 0
      Text = 'cbCharacter'
      OnChange = cbCharacterChange
    end
  end
end
