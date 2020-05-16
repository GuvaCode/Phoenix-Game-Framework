object FrmModelTags: TFrmModelTags
  Left = 0
  Top = 0
  Width = 250
  Height = 634
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox2: TGroupBox
    Left = 4
    Top = 230
    Width = 242
    Height = 195
    Align = alTop
    Caption = 'Position'
    TabOrder = 0
    DesignSize = (
      242
      195)
    object Label3: TLabel
      Left = 8
      Top = 21
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object Label5: TLabel
      Left = 8
      Top = 75
      Width = 10
      Height = 13
      Caption = 'Z:'
    end
    object edPositionX: TJvSpinEdit
      Left = 68
      Top = 18
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edPositionXChange
    end
    object edPositionY: TJvSpinEdit
      Left = 68
      Top = 45
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edPositionXChange
    end
    object edPositionZ: TJvSpinEdit
      Left = 68
      Top = 72
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edPositionXChange
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 154
    Width = 242
    Height = 76
    Align = alTop
    Caption = 'Tag properties'
    TabOrder = 1
    DesignSize = (
      242
      76)
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 27
      Height = 13
      Caption = '&Joint:'
    end
    object edName: TEdit
      Left = 68
      Top = 18
      Width = 167
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edJoint: TComboBox
      Left = 68
      Top = 45
      Width = 167
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object lwTags: TListView
    Left = 4
    Top = 4
    Width = 242
    Height = 150
    Align = alTop
    Columns = <
      item
        AutoSize = True
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 2
    ViewStyle = vsReport
    OnClick = lwTagsClick
    OnKeyUp = lwTagsKeyUp
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 425
    Width = 242
    Height = 107
    Align = alTop
    Caption = 'Rotation'
    TabOrder = 3
    DesignSize = (
      242
      107)
    object Label6: TLabel
      Left = 8
      Top = 21
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object Label7: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object Label8: TLabel
      Left = 8
      Top = 75
      Width = 10
      Height = 13
      Caption = 'Z:'
    end
    object edRotationX: TJvSpinEdit
      Left = 68
      Top = 18
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edRotationY: TJvSpinEdit
      Left = 68
      Top = 45
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edRotationZ: TJvSpinEdit
      Left = 68
      Top = 72
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
end
