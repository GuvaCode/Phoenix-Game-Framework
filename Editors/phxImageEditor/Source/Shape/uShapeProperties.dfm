object FrmShape: TFrmShape
  Left = 0
  Top = 0
  Width = 274
  Height = 241
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 274
    Height = 55
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      274
      55)
    object Label1: TLabel
      Left = 4
      Top = 11
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Name:'
    end
    object Label4: TLabel
      Left = 4
      Top = 41
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Kind:'
    end
    object edName: TEdit
      Left = 60
      Top = 8
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edKind: TEdit
      Left = 60
      Top = 35
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
end
