object FrmDialogRotate: TFrmDialogRotate
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Rotate model'
  ClientHeight = 155
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    219
    155)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 136
    Top = 122
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 55
    Top = 122
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 203
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Rotation'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 10
      Height = 13
      Caption = '&X:'
    end
    object Label2: TLabel
      Left = 16
      Top = 46
      Width = 10
      Height = 13
      Caption = '&Y:'
    end
    object Label3: TLabel
      Left = 16
      Top = 73
      Width = 10
      Height = 13
      Caption = '&Z:'
    end
    object Label4: TLabel
      Left = 167
      Top = 19
      Width = 18
      Height = 13
      Caption = 'deg'
    end
    object Label5: TLabel
      Left = 167
      Top = 46
      Width = 18
      Height = 13
      Caption = 'deg'
    end
    object Label6: TLabel
      Left = 167
      Top = 73
      Width = 18
      Height = 13
      Caption = 'deg'
    end
    object edRotationX: TJvSpinEdit
      Left = 40
      Top = 16
      Width = 121
      Height = 21
      ValueType = vtFloat
      TabOrder = 0
    end
    object edRotationY: TJvSpinEdit
      Left = 40
      Top = 43
      Width = 121
      Height = 21
      ValueType = vtFloat
      TabOrder = 1
    end
    object edRotationZ: TJvSpinEdit
      Left = 40
      Top = 70
      Width = 121
      Height = 21
      ValueType = vtFloat
      TabOrder = 2
    end
  end
end
