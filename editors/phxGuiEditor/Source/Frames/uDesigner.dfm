object FrmDesigner: TFrmDesigner
  Left = 0
  Top = 0
  Width = 634
  Height = 408
  Padding.Top = 4
  Padding.Bottom = 4
  TabOrder = 0
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 4
    Width = 634
    Height = 400
    Align = alClient
    TabOrder = 0
    object PHXDraw1: TPHXDraw
      Left = 0
      Top = 0
      Width = 200
      Height = 200
      OnInit = PHXDraw1Init
      OnRender = PHXDraw1Render
      OnMouseDown = PHXDraw1MouseDown
      OnMouseMove = PHXDraw1MouseMove
      OnMouseUp = PHXDraw1MouseUp
      OnDragOver = PHXDraw1DragOver
      OnDragDrop = PHXDraw1DragDrop
    end
  end
end
