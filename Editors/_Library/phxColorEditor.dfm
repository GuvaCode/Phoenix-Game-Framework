object frmColorDialog: TfrmColorDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Color'
  ClientHeight = 269
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 257
    Height = 133
    Caption = 'Color components'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 19
      Height = 13
      Caption = '&Red'
    end
    object Label2: TLabel
      Left = 8
      Top = 46
      Width = 29
      Height = 13
      Caption = '&Green'
    end
    object Label3: TLabel
      Left = 8
      Top = 73
      Width = 20
      Height = 13
      Caption = '&Blue'
    end
    object Label4: TLabel
      Left = 8
      Top = 100
      Width = 27
      Height = 13
      Caption = '&Alpha'
    end
    object cbRed: TScrollBar
      Left = 48
      Top = 16
      Width = 137
      Height = 21
      Max = 255
      PageSize = 0
      TabOrder = 0
      OnChange = cbAlphaChange
    end
    object cbGreen: TScrollBar
      Left = 48
      Top = 43
      Width = 137
      Height = 21
      Max = 255
      PageSize = 0
      TabOrder = 1
      OnChange = cbAlphaChange
    end
    object cbBlue: TScrollBar
      Left = 48
      Top = 70
      Width = 137
      Height = 21
      Max = 255
      PageSize = 0
      TabOrder = 2
      OnChange = cbAlphaChange
    end
    object cbAlpha: TScrollBar
      Left = 48
      Top = 97
      Width = 137
      Height = 21
      Max = 255
      PageSize = 0
      TabOrder = 3
      OnChange = cbAlphaChange
    end
    object edRed: TEdit
      Left = 191
      Top = 16
      Width = 58
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object edGreen: TEdit
      Left = 191
      Top = 43
      Width = 58
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object edBlue: TEdit
      Left = 191
      Top = 70
      Width = 58
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object edAlpha: TEdit
      Left = 191
      Top = 97
      Width = 58
      Height = 21
      TabOrder = 7
      Text = '0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 143
    Width = 257
    Height = 50
    Caption = 'Standard colors'
    TabOrder = 1
    object cbStandardColors: TColorBox
      Left = 8
      Top = 17
      Width = 241
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      TabOrder = 0
      OnGetColors = cbStandardColorsGetColors
    end
  end
  object btnOkey: TButton
    Left = 388
    Top = 236
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkeyClick
  end
  object btnCancel: TButton
    Left = 469
    Top = 236
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 271
    Top = 8
    Width = 273
    Height = 216
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 4
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 199
    Width = 257
    Height = 50
    Caption = 'Lua color'
    TabOrder = 5
    object edScriptColor: TEdit
      Left = 8
      Top = 19
      Width = 241
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = 'edScriptColor'
    end
  end
end
