object FrmSettings: TFrmSettings
  Left = 0
  Top = 0
  Caption = 'Editor Options'
  ClientHeight = 267
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 235
    Width = 441
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 280
      Top = 0
      Width = 161
      Height = 32
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOk: TButton
        Left = 0
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object Button4: TButton
        Left = 81
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 353
    Height = 129
    Caption = 'Grid settings'
    TabOrder = 1
    DesignSize = (
      353
      129)
    object Label3: TLabel
      Left = 14
      Top = 55
      Width = 78
      Height = 13
      Caption = '&Vertical spacing:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 14
      Top = 27
      Width = 90
      Height = 13
      Caption = '&Horisontal spacing:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 197
      Top = 27
      Width = 27
      Height = 13
      Caption = '&Color:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 198
      Top = 55
      Width = 26
      Height = 13
      Caption = '&Style:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbSnapToGrid: TCheckBox
      Left = 14
      Top = 103
      Width = 97
      Height = 17
      Caption = '&Snap to grid'
      TabOrder = 0
    end
    object cbShowGrid: TCheckBox
      Left = 14
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Show &grid'
      TabOrder = 1
    end
    object edGridColor: TColorBox
      Left = 237
      Top = 24
      Width = 104
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edGridStyle: TComboBox
      Left = 237
      Top = 52
      Width = 104
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 3
      Text = 'Solid Lines'
      Items.Strings = (
        'Solid Lines'
        'Dotted Lines'
        'Dots')
    end
    object edGridSizeY: TJvSpinEdit
      Left = 120
      Top = 52
      Width = 65
      Height = 21
      TabOrder = 4
    end
    object edGridSizeX: TJvSpinEdit
      Left = 120
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 5
    end
  end
end
