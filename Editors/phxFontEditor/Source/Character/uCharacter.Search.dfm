object FrmCharacterSearch: TFrmCharacterSearch
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 92
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    307
    92)
  PixelsPerInch = 96
  TextHeight = 13
  object edChar: TRadioButton
    Left = 8
    Top = 8
    Width = 113
    Height = 17
    Caption = 'Search by text'
    TabOrder = 0
    OnClick = UpdateButtons
  end
  object edNumber: TRadioButton
    Left = 127
    Top = 8
    Width = 113
    Height = 17
    BiDiMode = bdLeftToRight
    Caption = 'Search by number'
    ParentBiDiMode = False
    TabOrder = 1
    OnClick = UpdateButtons
  end
  object edSearch: TEdit
    Left = 8
    Top = 32
    Width = 291
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    OnChange = UpdateButtons
    ExplicitTop = 56
  end
  object btnCancel: TButton
    Left = 224
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitTop = 82
  end
  object btnOk: TButton
    Left = 143
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    ExplicitTop = 82
  end
end
