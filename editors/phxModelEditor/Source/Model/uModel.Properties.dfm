object FrmModelProperties: TFrmModelProperties
  Left = 0
  Top = 0
  Width = 250
  Height = 493
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 242
    Height = 195
    Align = alTop
    Caption = 'Model properties'
    TabOrder = 0
    DesignSize = (
      242
      195)
    object Label1: TLabel
      Left = 8
      Top = 19
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Name:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Author:'
    end
    object Label3: TLabel
      Left = 8
      Top = 75
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Version:'
    end
    object Label4: TLabel
      Left = 8
      Top = 102
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Description:'
    end
    object edName: TEdit
      Left = 94
      Top = 16
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edAuthor: TEdit
      Left = 94
      Top = 45
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edAuthorChange
    end
    object edDescription: TMemo
      Left = 94
      Top = 99
      Width = 141
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      WordWrap = False
      OnChange = edDescriptionChange
    end
    object edVersion: TEdit
      Left = 94
      Top = 72
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edVersionChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 283
    Width = 242
    Height = 126
    Align = alTop
    Caption = 'Bounding box'
    TabOrder = 1
    DesignSize = (
      242
      126)
    object Label5: TLabel
      Left = 8
      Top = 21
      Width = 10
      Height = 13
      Caption = '&X:'
    end
    object Label6: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = '&Y:'
    end
    object Label7: TLabel
      Left = 8
      Top = 75
      Width = 10
      Height = 13
      Caption = '&Z:'
    end
    object edMinX: TSpinEdit
      Left = 39
      Top = 18
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      TabOrder = 0
    end
    object edMaxX: TSpinEdit
      Left = 140
      Top = 18
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edMinY: TSpinEdit
      Left = 39
      Top = 45
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      TabOrder = 2
    end
    object edMaxY: TSpinEdit
      Left = 140
      Top = 45
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object edMinZ: TSpinEdit
      Left = 39
      Top = 72
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      TabOrder = 4
    end
    object edMaxZ: TSpinEdit
      Left = 140
      Top = 72
      Width = 95
      Height = 21
      Decimal = 3
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 199
    Width = 242
    Height = 84
    Align = alTop
    Caption = 'Model &information'
    TabOrder = 2
    ExplicitLeft = 1
    ExplicitTop = 184
    DesignSize = (
      242
      84)
    object Label8: TLabel
      Left = 8
      Top = 29
      Width = 66
      Height = 13
      Caption = '&Vertex count:'
    end
    object Label9: TLabel
      Left = 8
      Top = 56
      Width = 72
      Height = 13
      Caption = '&Triangle count:'
    end
    object edVertexCount: TEdit
      Left = 100
      Top = 26
      Width = 139
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      OnChange = edNameChange
    end
    object edTriangleCount: TEdit
      Left = 100
      Top = 53
      Width = 139
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      OnChange = edAuthorChange
    end
  end
end
