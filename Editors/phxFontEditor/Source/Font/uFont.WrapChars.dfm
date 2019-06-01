object FrmWrapCharacters: TFrmWrapCharacters
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Select characters'
  ClientHeight = 271
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    246
    271)
  PixelsPerInch = 96
  TextHeight = 13
  object sgCharacters: TStringGrid
    Left = 8
    Top = 8
    Width = 230
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 100
    DefaultRowHeight = 16
    FixedCols = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentFont = False
    TabOrder = 0
    OnExit = sgCharactersExit
    OnKeyDown = sgCharactersKeyDown
    OnSelectCell = sgCharactersSelectCell
    OnSetEditText = sgCharactersSetEditText
    ExplicitWidth = 619
    ExplicitHeight = 253
  end
  object btnOk: TButton
    Left = 82
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    ExplicitLeft = 471
    ExplicitTop = 267
  end
  object btnCancel: TButton
    Left = 163
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 552
    ExplicitTop = 267
  end
end
