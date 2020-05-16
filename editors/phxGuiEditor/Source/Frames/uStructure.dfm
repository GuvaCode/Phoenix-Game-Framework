object FrmStructure: TFrmStructure
  Left = 0
  Top = 0
  Width = 187
  Height = 199
  TabOrder = 0
  object twStructure: TTreeView
    Left = 0
    Top = 0
    Width = 187
    Height = 199
    Align = alClient
    HideSelection = False
    Images = ModActions.ControlImages
    Indent = 19
    ReadOnly = True
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 0
    OnChange = twStructureChange
  end
end
