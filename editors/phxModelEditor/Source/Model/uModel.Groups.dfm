object FrmModelGroups: TFrmModelGroups
  Left = 0
  Top = 0
  Width = 334
  Height = 483
  TabOrder = 0
  object lwGroups: TListView
    Left = 4
    Top = 4
    Width = 326
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
    OnClick = lwGroupsClick
    OnKeyUp = lwGroupsKeyUp
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 154
    Width = 326
    Height = 103
    Align = alTop
    Caption = 'Group properties'
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 19
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Name:'
    end
    object Label1: TLabel
      Left = 8
      Top = 73
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Triangle &Count:'
    end
    object Label2: TLabel
      Left = 8
      Top = 46
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Triangle &Offset:'
    end
    object edName: TEdit
      Left = 96
      Top = 16
      Width = 217
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edVertexCount: TEdit
      Left = 96
      Top = 70
      Width = 217
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edVertexOffset: TEdit
      Left = 94
      Top = 43
      Width = 219
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
  end
end
