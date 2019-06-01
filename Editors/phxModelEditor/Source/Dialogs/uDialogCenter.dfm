object FrmDialogCenter: TFrmDialogCenter
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Center model'
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
    Caption = 'Center along axes'
    TabOrder = 2
    object edCenterY: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Y'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object edCenterX: TCheckBox
      Left = 16
      Top = 25
      Width = 97
      Height = 17
      Caption = 'X'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object edCenterZ: TCheckBox
    Left = 24
    Top = 79
    Width = 97
    Height = 17
    Caption = 'Z'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
