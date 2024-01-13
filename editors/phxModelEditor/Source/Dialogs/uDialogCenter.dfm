object FrmDialogCenter: TFrmDialogCenter
  Left = 848
  Height = 155
  Top = 416
  Width = 219
  BorderStyle = bsToolWindow
  Caption = 'Center model'
  ClientHeight = 155
  ClientWidth = 219
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Position = poMainFormCenter
  LCLVersion = '3.0.0.3'
  object btnCancel: TButton
    Left = 136
    Height = 25
    Top = 122
    Width = 75
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 55
    Height = 25
    Top = 122
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Height = 105
    Top = 8
    Width = 203
    Anchors = [akTop, akLeft, akRight]
    Caption = 'Center along axes'
    ClientHeight = 90
    ClientWidth = 201
    ParentBackground = False
    TabOrder = 2
    object edCenterY: TCheckBox
      Left = 16
      Height = 23
      Top = 48
      Width = 33
      Caption = 'Y'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object edCenterX: TCheckBox
      Left = 16
      Height = 23
      Top = 25
      Width = 34
      Caption = 'X'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object edCenterZ: TCheckBox
    Left = 24
    Height = 23
    Top = 79
    Width = 34
    Caption = 'Z'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
