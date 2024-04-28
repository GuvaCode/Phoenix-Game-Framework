object FrmShape: TFrmShape
  Left = 0
  Height = 241
  Top = 0
  Width = 274
  ClientHeight = 241
  ClientWidth = 274
  TabOrder = 0
  DesignLeft = 537
  DesignTop = 262
  object Panel1: TPanel
    Left = 0
    Height = 72
    Top = 0
    Width = 274
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 72
    ClientWidth = 274
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Height = 13
      Top = 8
      Width = 50
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Name:'
    end
    object Label4: TLabel
      Left = 4
      Height = 13
      Top = 40
      Width = 50
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Kind:'
    end
    object edName: TEdit
      Left = 60
      Height = 28
      Top = 8
      Width = 203
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edKind: TEdit
      Left = 60
      Height = 28
      Top = 40
      Width = 203
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
end
