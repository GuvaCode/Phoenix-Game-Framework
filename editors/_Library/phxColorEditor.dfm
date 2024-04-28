object frmColorDialog: TfrmColorDialog
  Left = 404
  Height = 269
  Top = 408
  Width = 552
  BorderStyle = bsDialog
  Caption = 'Color'
  ClientHeight = 269
  ClientWidth = 552
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  Position = poOwnerFormCenter
  LCLVersion = '3.0.0.3'
  object GroupBox1: TGroupBox
    Left = 8
    Height = 148
    Top = 4
    Width = 257
    Caption = 'Color components'
    ClientHeight = 133
    ClientWidth = 255
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Height = 14
      Top = 16
      Width = 21
      Caption = '&Red'
    end
    object Label2: TLabel
      Left = 8
      Height = 14
      Top = 46
      Width = 34
      Caption = '&Green'
    end
    object Label3: TLabel
      Left = 8
      Height = 14
      Top = 73
      Width = 25
      Caption = '&Blue'
    end
    object Label4: TLabel
      Left = 8
      Height = 14
      Top = 100
      Width = 32
      Caption = '&Alpha'
    end
    object cbRed: TScrollBar
      Left = 48
      Height = 13
      Top = 16
      Width = 137
      Max = 255
      PageSize = 0
      TabOrder = 0
      OnChange = cbAlphaChange
    end
    object cbGreen: TScrollBar
      Left = 48
      Height = 13
      Top = 43
      Width = 137
      Max = 255
      PageSize = 0
      TabOrder = 1
      OnChange = cbAlphaChange
    end
    object cbBlue: TScrollBar
      Left = 48
      Height = 13
      Top = 70
      Width = 137
      Max = 255
      PageSize = 0
      TabOrder = 2
      OnChange = cbAlphaChange
    end
    object cbAlpha: TScrollBar
      Left = 48
      Height = 13
      Top = 97
      Width = 137
      Max = 255
      PageSize = 0
      TabOrder = 3
      OnChange = cbAlphaChange
    end
    object edRed: TEdit
      Left = 191
      Height = 26
      Top = 16
      Width = 58
      TabOrder = 4
      Text = '0'
    end
    object edGreen: TEdit
      Left = 191
      Height = 26
      Top = 43
      Width = 58
      TabOrder = 5
      Text = '0'
    end
    object edBlue: TEdit
      Left = 191
      Height = 26
      Top = 70
      Width = 58
      TabOrder = 6
      Text = '0'
    end
    object edAlpha: TEdit
      Left = 191
      Height = 26
      Top = 97
      Width = 58
      TabOrder = 7
      Text = '0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Height = 50
    Top = 152
    Width = 257
    BorderSpacing.Around = 6
    Caption = 'Standard colors'
    ClientHeight = 35
    ClientWidth = 255
    ParentBackground = False
    TabOrder = 1
    object cbStandardColors: TColorBox
      Left = 8
      Height = 25
      Top = 8
      Width = 241
      ColorRectWidth = 8
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      OnGetColors = cbStandardColorsGetColors
      BorderSpacing.Around = 8
      ItemHeight = 0
      TabOrder = 0
    end
  end
  object btnOkey: TButton
    Left = 388
    Height = 25
    Top = 236
    Width = 75
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkeyClick
  end
  object btnCancel: TButton
    Left = 469
    Height = 25
    Top = 236
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 271
    Height = 216
    Top = 4
    Width = 273
    BevelOuter = bvNone
    Caption = 'Panel1'
    Color = clInfoBk
    ParentBackground = False
    ParentColor = False
    TabOrder = 4
    OnClick = JvFullColorPanel1ColorChange
  end
  object GroupBox3: TGroupBox
    Left = 8
    Height = 50
    Top = 200
    Width = 257
    Caption = 'Lua color'
    ClientHeight = 35
    ClientWidth = 255
    ParentBackground = False
    TabOrder = 5
    object edScriptColor: TEdit
      Left = 6
      Height = 23
      Top = 6
      Width = 243
      Align = alClient
      BorderSpacing.Around = 6
      ReadOnly = True
      TabOrder = 0
      Text = 'edScriptColor'
    end
  end
end
