object FrmPatternList: TFrmPatternList
  Left = 0
  Top = 0
  Width = 250
  Height = 250
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object lwPatterns: TListView
    Left = 4
    Top = 4
    Width = 242
    Height = 220
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lwPatternsClick
    OnDblClick = lwPatternsDblClick
    OnKeyUp = lwPatternsKeyUp
    OnSelectItem = lwPatternsSelectItem
    ExplicitWidth = 239
    ExplicitHeight = 228
  end
  object ToolBar1: TToolBar
    Left = 4
    Top = 224
    Width = 242
    Height = 22
    Align = alBottom
    Caption = 'ToolBar2'
    Images = ModActions.ListImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    ExplicitTop = 232
    ExplicitWidth = 239
    object PatternAdd: TToolButton
      Left = 0
      Top = 0
      Action = actPatternAdd
    end
    object PatternDel: TToolButton
      Left = 23
      Top = 0
      Action = actPatternDel
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
      Action = actPatternDown
    end
    object PatternUp: TToolButton
      Left = 77
      Top = 0
      Action = actPatternUp
    end
    object ToolButton3: TToolButton
      Left = 100
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object btnTagSort: TToolButton
      Left = 108
      Top = 0
      Action = actPatternSort
    end
    object btnTagSearch: TToolButton
      Left = 131
      Top = 0
      Action = actPatternSearch
    end
    object ToolButton2: TToolButton
      Left = 154
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 162
      Top = 0
      Action = actPatternZoom
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 48
    Top = 32
    object actPatternAdd1: TMenuItem
      Action = actPatternAdd
    end
    object actPatternDel1: TMenuItem
      Action = actPatternDel
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object actPatternClear1: TMenuItem
      Action = actPatternClear
    end
  end
  object actPattern: TActionList
    Images = ModActions.ListImages
    Left = 48
    Top = 96
    object actPatternAdd: TAction
      Category = 'Pattern'
      Caption = '&Add'
      Hint = 'Add a new pattern'
      ImageIndex = 1
      OnExecute = actPatternAddExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternDel: TAction
      Category = 'Pattern'
      Caption = '&Del'
      Hint = 'Delete the selected pattern'
      ImageIndex = 0
      OnExecute = actPatternDelExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternUp: TAction
      Category = 'Pattern'
      Caption = 'actPatternUp'
      Hint = 'Move the selected pattern up'
      ImageIndex = 2
      OnExecute = actPatternUpExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternDown: TAction
      Category = 'Pattern'
      Caption = 'actPatternDown'
      Hint = 'Move the selected pattern down'
      ImageIndex = 3
      OnExecute = actPatternDownExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternSort: TAction
      Category = 'Pattern'
      Caption = 'actPatternSort'
      Hint = 'Sort the patterns'
      ImageIndex = 8
      OnExecute = actPatternSortExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternSearch: TAction
      Category = 'Pattern'
      Caption = 'actPatternSearch'
      Hint = 'Search for patterns by name'
      ImageIndex = 7
      OnExecute = actPatternSearchExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternZoom: TAction
      Category = 'Pattern'
      Caption = 'actPatternZoom'
      Hint = 'Zoom to the selected pattern'
      ImageIndex = 10
      OnExecute = actPatternZoomExecute
      OnUpdate = actPatternUpdate
    end
    object actPatternClear: TAction
      Category = 'Pattern'
      Caption = '&Clear'
      OnExecute = actPatternClearExecute
    end
  end
end
