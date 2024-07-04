object FrmEffectPreview: TFrmEffectPreview
  Left = 0
  Top = 0
  Width = 830
  Height = 610
  TabOrder = 0
  object PHXDraw1: TPHXDraw
    Left = 0
    Top = 0
    Width = 830
    Height = 610
    Interval = 50
    OnInit = PHXDraw1Init
    OnUpdate = PHXDraw1Update
    OnRender = PHXDraw1Render
    Popupmenu = PopupMenu1
    Align = alClient
    OnDblClick = PHXDraw1DblClick
    OnMouseDown = PHXDraw1MouseDown
    OnMouseMove = PHXDraw1MouseMove
  end
  object PopupMenu1: TPopupMenu
    Left = 152
    Top = 248
    object Addsystem1: TMenuItem
      Caption = '&Add system'
      Default = True
      OnClick = Addsystem1Click
    end
    object Clearsystems1: TMenuItem
      Caption = 'Clear systems'
      OnClick = Clearsystems1Click
    end
  end
end
