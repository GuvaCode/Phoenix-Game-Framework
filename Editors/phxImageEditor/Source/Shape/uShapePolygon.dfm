object FrmShapePolygon: TFrmShapePolygon
  Left = 0
  Top = 0
  Width = 310
  Height = 173
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  DesignSize = (
    310
    173)
  object Label3: TLabel
    Left = 4
    Top = 10
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Size:'
  end
  object Label4: TLabel
    Left = 4
    Top = 37
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Points:'
  end
  object sgPoints: TStringGrid
    Left = 60
    Top = 34
    Width = 243
    Height = 104
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    DefaultColWidth = 40
    DefaultRowHeight = 16
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
    ScrollBars = ssVertical
    TabOrder = 0
    OnExit = sgPointsExit
    OnSelectCell = sgPointsSelectCell
    RowHeights = (
      16
      16)
  end
  object edSize: TJvSpinEdit
    Left = 60
    Top = 7
    Width = 243
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edSizeChange
  end
  object btnCreateFromImage: TButton
    Left = 60
    Top = 144
    Width = 243
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Create from image'
    TabOrder = 2
    OnClick = btnCreateFromImageClick
  end
end
