object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'phxGuiEditor'
  ClientHeight = 710
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 240
    Top = 22
    Width = 4
    Height = 669
    ExplicitLeft = 161
    ExplicitTop = 0
    ExplicitHeight = 523
  end
  object Splitter2: TSplitter
    Left = 854
    Top = 22
    Width = 4
    Height = 669
    Align = alRight
    ExplicitLeft = 169
    ExplicitTop = 8
    ExplicitHeight = 523
  end
  object PanelRight: TPanel
    Left = 858
    Top = 22
    Width = 150
    Height = 669
    Align = alRight
    BevelOuter = bvNone
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 22
    Width = 240
    Height = 669
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Bottom = 4
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 4
      Top = 165
      Width = 236
      Height = 4
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 157
    end
    object PanelStructure: TPanel
      Left = 4
      Top = 4
      Width = 236
      Height = 161
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 691
    Width = 1008
    Height = 19
    Panels = <>
  end
  object ToolBarStandard: TToolBar
    Left = 0
    Top = 0
    Width = 1008
    Height = 22
    Caption = 'Standard'
    DrawingStyle = dsGradient
    Images = ModActions.ActionImages
    TabOrder = 3
    object ToolButton6: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 10
      Style = tbsSeparator
    end
    object btnFileNew: TToolButton
      Left = 8
      Top = 0
      Action = ModActions.actFileNew
      ParentShowHint = False
      ShowHint = True
    end
    object btnFileOpen: TToolButton
      Left = 31
      Top = 0
      Action = ModActions.actFileOpen
      ParentShowHint = False
      ShowHint = True
    end
    object btnFileSave: TToolButton
      Left = 54
      Top = 0
      Action = ModActions.actFileSave
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton1: TToolButton
      Left = 77
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 3
      Style = tbsSeparator
    end
  end
  object MainMenu1: TMainMenu
    Left = 384
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
      object actFileNew1: TMenuItem
        Action = ModActions.actFileNew
      end
      object actFileOpen1: TMenuItem
        Action = ModActions.actFileOpen
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object actFileSave1: TMenuItem
        Action = ModActions.actFileSave
      end
      object actFileSaveAs1: TMenuItem
        Action = ModActions.actFileSaveAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object actFileNew2: TMenuItem
        Action = ModActions.actFileExit
      end
    end
    object Project1: TMenuItem
      Caption = '&Project'
      object actCompile1: TMenuItem
        Action = ModActions.actProjectCompile
      end
    end
    object Assets1: TMenuItem
      Caption = '&Assets'
      object actAssetBackground1: TMenuItem
        Action = ModActions.actAssetBackground
      end
      object Loadskin1: TMenuItem
        Action = ModActions.actAssetSkin
      end
      object Loadfont1: TMenuItem
        Action = ModActions.actAssetFont
      end
    end
  end
end
