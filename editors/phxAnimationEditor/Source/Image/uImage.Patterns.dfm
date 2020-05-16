object FrmPatterns: TFrmPatterns
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    OnResize = ScrollBox1Resize
    object PaintBox1: TPaintBox
      Left = 0
      Top = 0
      Width = 320
      Height = 189
      Align = alTop
      OnMouseDown = PaintBox1MouseDown
      OnMouseMove = PaintBox1MouseMove
      OnPaint = PaintBox1Paint
      ExplicitLeft = 127
      ExplicitWidth = 236
    end
  end
end
