object FrmPatternList: TFrmPatternList
  Left = 0
  Height = 469
  Top = 0
  Width = 308
  ClientHeight = 469
  ClientWidth = 308
  TabOrder = 0
  DesignLeft = 540
  DesignTop = 178
  object Panel1: TPanel
    Left = 0
    Height = 144
    Top = 0
    Width = 308
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ClientHeight = 144
    ClientWidth = 308
    TabOrder = 0
    object ToolBar1: TToolBar
      Left = 0
      Height = 22
      Top = 0
      Width = 308
      Caption = 'ToolBar2'
      EdgeBorders = []
      Images = ModActions.ListImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object PatternAdd: TToolButton
        Left = 1
        Top = 0
        Action = actPatternAdd
      end
      object PatternDel: TToolButton
        Left = 24
        Top = 0
        Action = actPatternDel
      end
      object ToolButton1: TToolButton
        Left = 47
        Height = 22
        Top = 0
        Caption = 'ToolButton1'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object PatternDown: TToolButton
        Left = 55
        Top = 0
        Action = actPatternDown
      end
      object PatternUp: TToolButton
        Left = 78
        Top = 0
        Action = actPatternUp
      end
      object ToolButton3: TToolButton
        Left = 101
        Height = 22
        Top = 0
        Caption = 'ToolButton3'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object btnTagSort: TToolButton
        Left = 109
        Top = 0
        Action = actPatternSort
      end
      object btnTagSearch: TToolButton
        Left = 132
        Top = 0
        Action = actPatternSearch
      end
      object ToolButton2: TToolButton
        Left = 155
        Height = 22
        Top = 0
        Caption = 'ToolButton2'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 163
        Top = 0
        Action = actPatternZoom
      end
    end
    object lwPatterns: TListView
      Left = 4
      Height = 114
      Top = 26
      Width = 300
      Align = alClient
      BorderSpacing.Around = 4
      Columns = <      
        item
          Caption = 'Name'
          Width = 285
        end>
      HideSelection = False
      PopupMenu = PopupMenu1
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 1
      ViewStyle = vsReport
      OnClick = lwPatternsClick
      OnDblClick = lwPatternsDblClick
      OnKeyUp = lwPatternsKeyUp
      OnSelectItem = lwPatternsSelectItem
    end
  end
  object Splitter1: TSplitter
    Cursor = crVSplit
    Left = 0
    Height = 5
    Top = 144
    Width = 308
    Align = alTop
    ResizeAnchor = akTop
  end
  object Panel2: TPanel
    Left = 0
    Height = 320
    Top = 149
    Width = 308
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 2
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
