object FrmTagList: TFrmTagList
  Left = 0
  Height = 372
  Top = 0
  Width = 327
  ClientHeight = 372
  ClientWidth = 327
  TabOrder = 0
  DesignLeft = 406
  DesignTop = 195
  object Panel1: TPanel
    Left = 0
    Height = 217
    Top = 155
    Width = 327
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Height = 150
    Top = 0
    Width = 327
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    ClientHeight = 150
    ClientWidth = 327
    TabOrder = 1
    object lwTags: TListView
      Left = 4
      Height = 120
      Top = 26
      Width = 319
      Align = alClient
      BorderSpacing.Around = 4
      Columns = <      
        item
          AutoSize = True
          Caption = 'Name'
          Width = 304
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
    end
    object ToolBar1: TToolBar
      Left = 0
      Height = 22
      Top = 0
      Width = 327
      Caption = 'ToolBar2'
      EdgeBorders = []
      Images = ModActions.ListImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object PatternAdd: TToolButton
        Left = 1
        Top = 0
        Action = ModTags.actTagAdd
      end
      object PatternDel: TToolButton
        Left = 24
        Top = 0
        Action = ModTags.actTagDel
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
        Action = ModTags.actTagDown
      end
      object PatternUp: TToolButton
        Left = 78
        Top = 0
        Action = ModTags.actTagUp
      end
      object ToolButton3: TToolButton
        Left = 101
        Height = 22
        Top = 0
        Caption = 'ToolButton3'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 109
        Top = 0
        Action = ModTags.actTagZoom
      end
      object btnTagSort: TToolButton
        Left = 132
        Top = 0
        Action = ModTags.actTagSort
      end
    end
  end
  object Splitter1: TSplitter
    Cursor = crVSplit
    Left = 0
    Height = 5
    Top = 150
    Width = 327
    Align = alBottom
    ResizeAnchor = akBottom
  end
end
