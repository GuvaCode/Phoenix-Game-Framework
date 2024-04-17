object FrmShapeLine: TFrmShapeLine
  Left = 0
  Height = 189
  Top = 0
  Width = 281
  ClientHeight = 189
  ClientWidth = 281
  TabOrder = 0
  TabStop = True
  DesignLeft = 176
  DesignTop = 240
  object Label1: TLabel
    Left = 4
    Height = 13
    Top = 7
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Start X:'
  end
  object Label2: TLabel
    Left = 4
    Height = 13
    Top = 40
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Start Y:'
  end
  object Label3: TLabel
    Left = 4
    Height = 13
    Top = 104
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&End Y:'
  end
  object Label4: TLabel
    Left = 4
    Height = 13
    Top = 72
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&End X:'
  end
  object edMinY: TSpinEdit
    Left = 60
    Height = 28
    Top = 7
    Width = 214
    Anchors = [akTop, akLeft, akRight]
    OnChange = edMinYChange
    TabOrder = 0
  end
  object edMinX: TSpinEdit
    Left = 60
    Height = 28
    Top = 40
    Width = 214
    Anchors = [akTop, akLeft, akRight]
    OnChange = edMinXChange
    TabOrder = 1
  end
  object edMaxX: TSpinEdit
    Left = 60
    Height = 28
    Top = 72
    Width = 214
    Anchors = [akTop, akLeft, akRight]
    OnChange = edMaxXChange
    TabOrder = 2
  end
  object edMaxY: TSpinEdit
    Left = 60
    Height = 28
    Top = 104
    Width = 214
    Anchors = [akTop, akLeft, akRight]
    OnChange = edMaxYChange
    TabOrder = 3
  end
end
