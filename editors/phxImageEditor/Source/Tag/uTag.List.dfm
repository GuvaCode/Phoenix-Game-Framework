object FrmTagList: TFrmTagList
  Left = 0
  Top = 0
  Width = 250
  Height = 334
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object lwTags: TListView
    Left = 4
    Top = 4
    Width = 242
    Height = 145
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lwTagsClick
    OnDblClick = lwTagsDblClick
    OnKeyUp = lwTagsKeyUp
    OnSelectItem = lwTagsSelectItem
    ExplicitWidth = 286
    ExplicitHeight = 250
  end
  object ToolBar1: TToolBar
    Left = 4
    Top = 149
    Width = 242
    Height = 22
    Align = alBottom
    Caption = 'ToolBar2'
    Images = ModActions.ListImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    ExplicitLeft = 7
    ExplicitTop = 221
    object PatternAdd: TToolButton
      Left = 0
      Top = 0
      Action = ModTags.actTagAdd
    end
    object PatternDel: TToolButton
      Left = 23
      Top = 0
      Action = ModTags.actTagDel
    end
    object ToolButton1: TToolButton
      Left = 46
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object PatternDown: TToolButton
      Left = 54
      Top = 0
      Action = ModTags.actTagDown
    end
    object PatternUp: TToolButton
      Left = 77
      Top = 0
      Action = ModTags.actTagUp
    end
    object ToolButton3: TToolButton
      Left = 100
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 108
      Top = 0
      Action = ModTags.actTagZoom
    end
    object btnTagSort: TToolButton
      Left = 131
      Top = 0
      Action = ModTags.actTagSort
    end
  end
  object Panel1: TPanel
    Left = 4
    Top = 171
    Width = 242
    Height = 159
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 128
  end
end
