object FrmViewer: TFrmViewer
  Left = 0
  Top = 0
  Width = 660
  Height = 455
  TabOrder = 0
  object PHXDraw1: TPHXDraw
    Left = 0
    Top = 0
    Width = 660
    Height = 455
    OnInit = PHXDraw1Init
    OnUpdate = PHXDraw1Update
    OnRender = PHXDraw1Render
    Align = alClient
    OnMouseWheel = PHXDraw1MouseWheel
    OnMouseDown = PHXDraw1MouseDown
    OnMouseMove = PHXDraw1MouseMove
    OnKeyDown = PHXDraw1KeyDown
    OnKeyUp = PHXDraw1KeyUp
  end
end
