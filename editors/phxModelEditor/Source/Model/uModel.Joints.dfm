object FrmModelJoints: TFrmModelJoints
  Left = 0
  Top = 0
  Width = 250
  Height = 526
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object lwJoints: TListView
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
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lwJointsClick
    OnKeyUp = lwJointsKeyUp
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 154
    Width = 242
    Height = 76
    Align = alTop
    Caption = 'Joint properties'
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
      Width = 36
      Height = 13
      Caption = '&Parent:'
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
  object GroupBox3: TGroupBox
    Left = 4
    Top = 331
    Width = 242
    Height = 107
    Align = alTop
    Caption = 'Rotation'
    TabOrder = 2
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
    object edRotationX: TSpinEdit
      Left = 68
      Top = 18
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edRotationY: TSpinEdit
      Left = 68
      Top = 45
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edRotationZ: TSpinEdit
      Left = 68
      Top = 72
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 230
    Width = 242
    Height = 101
    Align = alTop
    Caption = 'Position'
    TabOrder = 3
    DesignSize = (
      242
      101)
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
    object edPositionX: TSpinEdit
      Left = 68
      Top = 18
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edPositionY: TSpinEdit
      Left = 68
      Top = 45
      Width = 167
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edPositionZ: TSpinEdit
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
