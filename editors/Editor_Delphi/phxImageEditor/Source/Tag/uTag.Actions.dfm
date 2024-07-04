object ModTags: TModTags
  OldCreateOrder = False
  Height = 150
  Width = 215
  object actTags: TActionList
    Images = ModActions.ListImages
    Left = 24
    Top = 16
    object actTagAdd: TAction
      Category = 'Tag'
      Caption = '&Add'
      Hint = 'Add a new tag'
      ImageIndex = 1
      OnExecute = actTagAddExecute
      OnUpdate = actTagAddUpdate
    end
    object actTagDel: TAction
      Category = 'Tag'
      Caption = '&Del'
      Hint = 'Delete the selected tag'
      ImageIndex = 0
      OnExecute = actTagDelExecute
      OnUpdate = actTagAddUpdate
    end
    object actTagUp: TAction
      Category = 'Tag'
      Caption = 'actTagUp'
      Hint = 'Move the selected tag up'
      ImageIndex = 2
      OnExecute = actTagUpExecute
    end
    object actTagDown: TAction
      Category = 'Tag'
      Caption = 'actTagDown'
      Hint = 'Move the selected tag down'
      ImageIndex = 3
      OnExecute = actTagDownExecute
    end
    object actTagSort: TAction
      Category = 'Tag'
      Caption = 'actTagSort'
      Hint = 'Sort the tags'
      ImageIndex = 8
    end
    object actTagZoom: TAction
      Category = 'Tag'
      Caption = 'actTagZoom'
      ImageIndex = 10
      OnExecute = actTagZoomExecute
    end
  end
end
