object FrmTagProperties: TFrmTagProperties
  Left = 0
  Height = 213
  Top = 0
  Width = 212
  BorderSpacing.Around = 4
  ClientHeight = 213
  ClientWidth = 212
  TabOrder = 0
  DesignLeft = 501
  DesignTop = 153
  object GroupBox1: TGroupBox
    Left = 0
    Height = 213
    Top = 0
    Width = 212
    Align = alClient
    Caption = 'Tag properties'
    ClientHeight = 196
    ClientWidth = 210
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Height = 16
      Top = 7
      Width = 38
      Caption = '&Name:'
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 40
      Width = 48
      Caption = '&Pattern:'
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Height = 16
      Top = 75
      Width = 11
      Caption = '&X:'
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Height = 16
      Top = 112
      Width = 10
      Caption = '&Y:'
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 144
      Width = 52
      Caption = '&Rotation'
      ParentFont = False
    end
    object edName: TEdit
      Left = 70
      Height = 28
      Top = 7
      Width = 135
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edPattern: TComboBox
      Left = 70
      Height = 30
      Top = 40
      Width = 135
      Anchors = [akTop, akLeft, akRight]
      ItemHeight = 0
      Style = csDropDownList
      TabOrder = 1
      OnChange = edPatternChange
    end
    object edX: TSpinEdit
      Left = 70
      Height = 28
      Top = 75
      Width = 135
      Anchors = [akTop, akLeft, akRight]
      OnChange = edXChange
      TabOrder = 2
    end
    object edY: TSpinEdit
      Left = 70
      Height = 28
      Top = 110
      Width = 135
      Anchors = [akTop, akLeft, akRight]
      OnChange = edYChange
      TabOrder = 3
    end
    object edRotation: TSpinEdit
      Left = 70
      Height = 28
      Top = 144
      Width = 135
      Anchors = [akTop, akLeft, akRight]
      OnChange = edRotationChange
      TabOrder = 4
    end
  end
end
