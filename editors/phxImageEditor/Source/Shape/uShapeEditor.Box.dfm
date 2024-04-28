object FrmShapeBox: TFrmShapeBox
  Left = 0
  Height = 210
  Top = 0
  Width = 252
  ClientHeight = 210
  ClientWidth = 252
  TabOrder = 0
  DesignLeft = 155
  DesignTop = 162
  object GroupBox1: TGroupBox
    Left = 0
    Height = 152
    Top = 0
    Width = 252
    Align = alTop
    Caption = 'Box'
    ClientHeight = 135
    ClientWidth = 250
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Height = 16
      Top = 8
      Width = 11
      Caption = '&X:'
    end
    object Label3: TLabel
      Left = 8
      Height = 16
      Top = 40
      Width = 10
      Caption = '&Y:'
    end
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 72
      Width = 39
      Caption = '&Width:'
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 104
      Width = 43
      Caption = '&Height:'
    end
    object edCenterX: TSpinEdit
      Left = 57
      Height = 28
      Top = 8
      Width = 186
      Anchors = [akTop, akLeft, akRight]
      OnChange = edCenterXChange
      TabOrder = 0
    end
    object edCenterY: TSpinEdit
      Left = 57
      Height = 28
      Top = 40
      Width = 186
      Anchors = [akTop, akLeft, akRight]
      OnChange = edCenterYChange
      TabOrder = 1
    end
    object edWidth: TSpinEdit
      Left = 57
      Height = 28
      Top = 72
      Width = 186
      Anchors = [akTop, akLeft, akRight]
      OnChange = edWidthChange
      TabOrder = 2
    end
    object edHeight: TSpinEdit
      Left = 57
      Height = 28
      Top = 104
      Width = 186
      Anchors = [akTop, akLeft, akRight]
      OnChange = edHeightChange
      TabOrder = 3
    end
  end
end
