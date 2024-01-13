object FrmDialogScale: TFrmDialogScale
  Left = 0
  Height = 155
  Top = 0
  Width = 219
  BorderStyle = bsToolWindow
  Caption = 'Scale model'
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
    Caption = 'Scale'
    ClientHeight = 90
    ClientWidth = 201
    ParentBackground = False
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Height = 14
      Top = 19
      Width = 12
      Caption = '&X:'
    end
    object Label2: TLabel
      Left = 16
      Height = 14
      Top = 46
      Width = 9
      Caption = '&Y:'
    end
    object Label3: TLabel
      Left = 16
      Height = 14
      Top = 73
      Width = 12
      Caption = '&Z:'
    end
    object Label4: TLabel
      Left = 167
      Height = 14
      Top = 19
      Width = 10
      Caption = '%'
    end
    object Label5: TLabel
      Left = 167
      Height = 14
      Top = 46
      Width = 10
      Caption = '%'
    end
    object Label6: TLabel
      Left = 167
      Height = 14
      Top = 73
      Width = 10
      Caption = '%'
    end
    object edScaleX: TSpinEdit
      Left = 40
      Height = 26
      Top = 16
      Width = 121
      TabOrder = 0
    end
    object edScaleY: TSpinEdit
      Left = 40
      Height = 26
      Top = 43
      Width = 121
      TabOrder = 1
    end
    object edScaleZ: TSpinEdit
      Left = 40
      Height = 26
      Top = 70
      Width = 121
      TabOrder = 2
    end
  end
end
