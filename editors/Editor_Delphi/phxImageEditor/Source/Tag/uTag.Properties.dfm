object FrmTagProperties: TFrmTagProperties
  Left = 0
  Top = 0
  Width = 202
  Height = 159
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 202
    Height = 159
    Align = alClient
    Caption = 'Tag properties'
    TabOrder = 0
    DesignSize = (
      202
      159)
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 37
      Height = 13
      Caption = '&Pattern:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 75
      Width = 10
      Height = 13
      Caption = '&X:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 102
      Width = 10
      Height = 13
      Caption = '&Y:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 129
      Width = 40
      Height = 13
      Caption = '&Rotation'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edName: TEdit
      Left = 54
      Top = 18
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edPattern: TComboBox
      Left = 54
      Top = 45
      Width = 141
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edPatternChange
    end
    object edX: TJvSpinEdit
      Left = 54
      Top = 72
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edXChange
    end
    object edY: TJvSpinEdit
      Left = 54
      Top = 99
      Width = 141
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edYChange
    end
    object edRotation: TJvSpinEdit
      Left = 54
      Top = 126
      Width = 141
      Height = 21
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edRotationChange
    end
  end
end
