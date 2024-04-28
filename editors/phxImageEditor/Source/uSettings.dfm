object FrmSettings: TFrmSettings
  Left = 118
  Height = 267
  Top = 289
  Width = 441
  Caption = 'Editor Options'
  ClientHeight = 267
  ClientWidth = 441
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Panel2: TPanel
    Left = 0
    Height = 32
    Top = 235
    Width = 441
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 32
    ClientWidth = 441
    ParentBackground = False
    TabOrder = 0
    object Panel1: TPanel
      Left = 280
      Height = 32
      Top = 0
      Width = 161
      Align = alRight
      BevelOuter = bvNone
      ClientHeight = 32
      ClientWidth = 161
      ParentBackground = False
      TabOrder = 0
      object btnOk: TButton
        Left = 0
        Height = 25
        Top = 2
        Width = 75
        Caption = 'Ok'
        ModalResult = 1
        TabOrder = 0
      end
      object Button4: TButton
        Left = 81
        Height = 25
        Top = 2
        Width = 75
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object cbCenterPivots: TCheckBox
    Left = 8
    Height = 23
    Top = 160
    Width = 159
    Caption = 'Center pivots as default'
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Height = 145
    Top = 8
    Width = 353
    Caption = 'Grid settings'
    ClientHeight = 130
    ClientWidth = 351
    ParentBackground = False
    TabOrder = 2
    object Label3: TLabel
      Left = 14
      Height = 14
      Top = 55
      Width = 92
      Caption = '&Vertical spacing:'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentFont = False
    end
    object Label11: TLabel
      Left = 14
      Height = 14
      Top = 27
      Width = 107
      Caption = '&Horisontal spacing:'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentFont = False
    end
    object Label1: TLabel
      Left = 197
      Height = 14
      Top = 27
      Width = 33
      Caption = '&Color:'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentFont = False
    end
    object Label2: TLabel
      Left = 198
      Height = 14
      Top = 55
      Width = 32
      Caption = '&Style:'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      ParentFont = False
    end
    object cbSnapToGrid: TCheckBox
      Left = 14
      Height = 23
      Top = 103
      Width = 93
      Caption = '&Snap to grid'
      TabOrder = 0
    end
    object cbShowGrid: TCheckBox
      Left = 14
      Height = 23
      Top = 80
      Width = 81
      Caption = 'Show &grid'
      TabOrder = 1
    end
    object edGridColor: TColorBox
      Left = 237
      Height = 25
      Top = 24
      Width = 104
      Anchors = [akTop, akLeft, akRight]
      ItemHeight = 0
      TabOrder = 2
    end
    object edGridStyle: TComboBox
      Left = 237
      Height = 25
      Top = 52
      Width = 104
      Anchors = [akTop, akLeft, akRight]
      ItemHeight = 0
      ItemIndex = 0
      Items.Strings = (
        'Solid Lines'
        'Dotted Lines'
        'Dots'
      )
      Style = csDropDownList
      TabOrder = 3
      Text = 'Solid Lines'
    end
    object edGridSizeY: TSpinEdit
      Left = 120
      Height = 26
      Top = 52
      Width = 65
      TabOrder = 4
    end
    object edGridSizeX: TSpinEdit
      Left = 120
      Height = 26
      Top = 24
      Width = 65
      TabOrder = 5
    end
  end
end
