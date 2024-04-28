object FrmElementName: TFrmElementName
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Skin Element'
  ClientHeight = 122
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    255
    122)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 39
    Height = 13
    Caption = '&Control:'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 24
    Height = 13
    Caption = '&Part:'
  end
  object Label3: TLabel
    Left = 8
    Top = 65
    Width = 31
    Height = 13
    Caption = '&Name:'
  end
  object edElementControl: TComboBox
    Left = 53
    Top = 8
    Width = 194
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edElementControlChange
    ExplicitWidth = 235
  end
  object btnCancel: TButton
    Left = 172
    Top = 89
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 123
    ExplicitTop = 70
  end
  object btnOkey: TButton
    Left = 91
    Top = 89
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    ExplicitLeft = 42
    ExplicitTop = 70
  end
  object edElementPart: TComboBox
    Left = 53
    Top = 35
    Width = 194
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edElementPartChange
    OnDropDown = edElementPartDropDown
    ExplicitWidth = 235
  end
  object edElementName: TEdit
    Left = 53
    Top = 62
    Width = 194
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    ExplicitWidth = 235
  end
end
