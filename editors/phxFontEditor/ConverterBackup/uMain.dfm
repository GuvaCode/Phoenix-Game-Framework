object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'phxFontEditor'
  ClientHeight = 616
  ClientWidth = 952
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 26
    Height = 571
    ExplicitLeft = 4
    ExplicitTop = 4
    ExplicitHeight = 401
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 597
    Width = 952
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
  object PageControl1: TPageControl
    Left = 3
    Top = 26
    Width = 949
    Height = 571
    ActivePage = TabFont
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
    object TabFont: TTabSheet
      Caption = '&Font'
      ImageIndex = 3
    end
    object TabTexture: TTabSheet
      Caption = 'Texture'
    end
    object TabCharacters: TTabSheet
      Caption = '&Characters'
      ImageIndex = 1
    end
    object TabKernings: TTabSheet
      Caption = '&Kernings'
      ImageIndex = 4
    end
    object TabPreview: TTabSheet
      Caption = '&Preview'
      ImageIndex = 5
    end
  end
  object ControlBar2: TControlBar
    Left = 0
    Top = 0
    Width = 952
    Height = 26
    Align = alTop
    BevelKind = bkNone
    DrawingStyle = dsGradient
    TabOrder = 2
    object ToolBarStandard: TToolBar
      Left = 11
      Top = 2
      Width = 254
      Height = 22
      AutoSize = True
      Caption = 'Standard'
      DrawingStyle = dsGradient
      Images = ModActions.ActionImages
      TabOrder = 0
      object btnFileNew: TToolButton
        Left = 0
        Top = 0
        Action = ModActions.actFileNew
        ParentShowHint = False
        ShowHint = True
      end
      object btnFileOpen: TToolButton
        Left = 23
        Top = 0
        Action = ModActions.actFileOpen
        ParentShowHint = False
        ShowHint = True
      end
      object btnFileSave: TToolButton
        Left = 46
        Top = 0
        Action = ModActions.actFileSave
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton10: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object btnImportTexture: TToolButton
        Left = 77
        Top = 0
        Action = ModActions.actToolImportTexture
      end
      object btnExportTexture: TToolButton
        Left = 100
        Top = 0
        Action = ModActions.actToolExportTexture
      end
      object ToolButton13: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object ToolButton14: TToolButton
        Left = 131
        Top = 0
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 196
    Top = 113
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = ModActions.actFileNew
      end
      object Open2: TMenuItem
        Action = ModActions.actFileOpen
      end
      object Save1: TMenuItem
        Action = ModActions.actFileSave
      end
      object Save2: TMenuItem
        Action = ModActions.actFileSaveAs
      end
      object Close1: TMenuItem
        Action = ModActions.actFileClose
      end
      object menuRecent: TMenuItem
        Caption = '&Recent files'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Import1: TMenuItem
        Caption = 'Import'
        object FontStudio41: TMenuItem
          Action = ModActions.actImportFontStudio4
        end
        object AngelcodeBitmapFontXML1: TMenuItem
          Action = ModActions.actImportBMFontXML
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ModActions.actFileExit
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object Export1: TMenuItem
        Caption = 'Export'
        object actFileSaveXML1: TMenuItem
          Action = ModActions.actFileSaveXML
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object actToolExportTexture1: TMenuItem
          Action = ModActions.actToolExportTexture
        end
        object extureColors1: TMenuItem
          Action = ModActions.actToolExportColors
        end
        object extureAlpha1: TMenuItem
          Action = ModActions.actToolExportMask
        end
      end
      object Import2: TMenuItem
        Caption = 'Import'
        object actFileLoadXML1: TMenuItem
          Action = ModActions.actFileLoadXML
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object exture1: TMenuItem
          Action = ModActions.actToolImportTexture
        end
      end
    end
    object About1: TMenuItem
      Caption = '&About'
      object PhoenixImageEditor1: TMenuItem
        Caption = 'Phoenix Font Editor'
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
end
