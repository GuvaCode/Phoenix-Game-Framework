object FrmShapeLine: TFrmShapeLine
  Left = 0
  Top = 0
  Width = 281
  Height = 189
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  TabStop = True
  DesignSize = (
    281
    189)
  object Label1: TLabel
    Left = 4
    Top = 10
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Start X:'
  end
  object Label2: TLabel
    Left = 4
    Top = 37
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Start Y:'
  end
  object Label3: TLabel
    Left = 4
    Top = 91
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&End Y:'
  end
  object Label4: TLabel
    Left = 4
    Top = 64
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&End X:'
  end
  object edMinY: TJvSpinEdit
    Left = 60
    Top = 7
    Width = 214
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edMinYChange
  end
  object edMinX: TJvSpinEdit
    Left = 60
    Top = 34
    Width = 214
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edMinXChange
  end
  object edMaxX: TJvSpinEdit
    Left = 60
    Top = 61
    Width = 214
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = edMaxXChange
  end
  object edMaxY: TJvSpinEdit
    Left = 60
    Top = 88
    Width = 214
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edMaxYChange
  end
end
