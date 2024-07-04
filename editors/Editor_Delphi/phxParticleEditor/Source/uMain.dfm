object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'phxParticleEditor'
  ClientHeight = 600
  ClientWidth = 1001
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
    Left = 308
    Top = 26
    Height = 555
    ExplicitLeft = 504
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 1001
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 1001
    Height = 26
    Align = alTop
    BevelKind = bkNone
    DrawingStyle = dsGradient
    TabOrder = 1
    object ToolBarStandard: TToolBar
      Left = 11
      Top = 2
      Width = 182
      Height = 22
      AutoSize = True
      Caption = 'Standard'
      DrawingStyle = dsGradient
      Images = ModActions.ActionImages
      TabOrder = 0
      object ToolButton3: TToolButton
        Left = 0
        Top = 0
        Action = ModActions.actFileNew
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton4: TToolButton
        Left = 23
        Top = 0
        Action = ModActions.actFileOpen
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton5: TToolButton
        Left = 46
        Top = 0
        Action = ModActions.actFileSave
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton1: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 77
        Top = 0
      end
      object ToolButton9: TToolButton
        Left = 100
        Top = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 26
    Width = 308
    Height = 555
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
  end
  object PanelClient: TPanel
    Left = 311
    Top = 26
    Width = 690
    Height = 555
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 0
      Top = 401
      Width = 690
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 399
      ExplicitWidth = 704
    end
    object PageControl2: TPageControl
      Left = 0
      Top = 403
      Width = 690
      Height = 152
      ActivePage = TabGraphScale
      Align = alBottom
      Images = ModActions.ToolImages
      TabOrder = 0
      object TabSystems: TTabSheet
        Caption = 'System& List'
        ImageIndex = 13
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
      object TabGraphAlpha: TTabSheet
        Caption = 'Alpha'
        ImageIndex = 14
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
      object TabGraphScale: TTabSheet
        Caption = 'Scale'
        ImageIndex = 14
      end
      object TabGraphColor: TTabSheet
        Caption = 'Color'
        ImageIndex = 14
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
      object TabGraphVelocity: TTabSheet
        Caption = 'Velocity'
        ImageIndex = 14
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
      object TabGraphSpin: TTabSheet
        Caption = 'Spin'
        ImageIndex = 14
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
      object TabGraphEmissionCount: TTabSheet
        Caption = 'EmissionCount'
        ImageIndex = 14
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 693
        ExplicitHeight = 0
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 460
    Top = 201
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = ModActions.actFileNew
      end
      object Open2: TMenuItem
        Action = ModActions.actFileOpen
      end
      object menuRecent: TMenuItem
        Caption = '&Reopen'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = ModActions.actFileSave
      end
      object Save2: TMenuItem
        Action = ModActions.actFileSaveAs
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ModActions.actFileExit
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Settings1: TMenuItem
      end
    end
    object Presets1: TMenuItem
      Caption = '&Presets'
      object Fire1: TMenuItem
        Action = ModActions.actPresetFire
      end
      object actPresetJumpgate1: TMenuItem
        Action = ModActions.actPresetJumpgate
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object Setsystemquota1: TMenuItem
        Action = ModActions.actSystemQuota
      end
    end
    object Debug1: TMenuItem
      Caption = '&Debug'
      object Showgraphvalues1: TMenuItem
        Action = ModActions.actDebugGraphs
      end
    end
    object About1: TMenuItem
      Caption = '&About'
      object PhoenixImageEditor1: TMenuItem
        Caption = 'Phoenix Particle Editor'
        Enabled = False
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object lblVersion: TMenuItem
        Caption = 'lblVersion'
        Enabled = False
      end
    end
  end
  object TimerUpdate: TTimer
    Interval = 250
    OnTimer = TimerUpdateTimer
    Left = 584
    Top = 220
  end
end
