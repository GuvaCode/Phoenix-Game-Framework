object FrmShapePolygon: TFrmShapePolygon
  Left = 0
  Height = 228
  Top = 0
  Width = 310
  ClientHeight = 228
  ClientWidth = 310
  OnResize = FrameResize
  TabOrder = 0
  TabStop = True
  object Label3: TLabel
    Left = 4
    Height = 13
    Top = 7
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Size:'
  end
  object Label4: TLabel
    Left = 4
    Height = 13
    Top = 42
    Width = 50
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Points:'
  end
  object sgPoints: TStringGrid
    Left = 60
    Height = 151
    Top = 42
    Width = 243
    Anchors = [akTop, akLeft, akRight, akBottom]
    ColCount = 3
    DefaultColWidth = 40
    DefaultRowHeight = 16
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
    RowCount = 2
    ScrollBars = ssVertical
    TabOrder = 0
    OnExit = sgPointsExit
    OnSelectCell = sgPointsSelectCell
  end
  object edSize: TSpinEdit
    Left = 60
    Height = 28
    Top = 7
    Width = 243
    Anchors = [akTop, akLeft, akRight]
    OnChange = edSizeChange
    TabOrder = 1
  end
  object btnCreateFromImage: TButton
    Left = 60
    Height = 25
    Top = 199
    Width = 243
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Create from image'
    TabOrder = 2
    OnClick = btnCreateFromImageClick
  end
end
