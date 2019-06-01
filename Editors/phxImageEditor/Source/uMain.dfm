object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Phoenix Image Editor'
  ClientHeight = 542
  ClientWidth = 784
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
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 250
    Top = 26
    Width = 4
    Height = 497
    ExplicitLeft = 213
    ExplicitHeight = 522
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 784
    Height = 26
    Align = alTop
    BevelKind = bkNone
    DrawingStyle = dsGradient
    TabOrder = 0
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
      object btnImageTextureLoad: TToolButton
        Left = 77
        Top = 0
        Action = ModActions.actImageTextureLoad
      end
      object btnImageTextureSave: TToolButton
        Left = 100
        Top = 0
        Action = ModActions.actImageTextureSave
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 26
    Width = 250
    Height = 497
    ActivePage = TabProperties
    Align = alLeft
    TabOrder = 1
    OnChange = PageControl1Change
    object TabProperties: TTabSheet
      Caption = '&Image'
    end
    object TabPatterns: TTabSheet
      Caption = '&Patterns'
      ImageIndex = 1
    end
    object TabTags: TTabSheet
      Caption = '&Tags'
      ImageIndex = 2
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 523
    Width = 784
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
  object pnlClient: TPanel
    Left = 254
    Top = 26
    Width = 530
    Height = 497
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Panel1: TPanel
      Left = 0
      Top = 22
      Width = 530
      Height = 475
      Align = alClient
      BevelKind = bkTile
      BevelOuter = bvLowered
      TabOrder = 0
      Visible = False
    end
    object pnlTools: TPanel
      Left = 0
      Top = 0
      Width = 530
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object ToolBarZoom: TToolBar
        Left = 0
        Top = 0
        Width = 77
        Height = 22
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBarZoom'
        Images = ModActions.ToolImages
        TabOrder = 0
        object btnZoomIn: TToolButton
          Left = 0
          Top = 0
          Action = ModActions.actEditorZoomIn
        end
        object btnZoomOut: TToolButton
          Left = 23
          Top = 0
          Action = ModActions.actEditorZoomOriginal
        end
        object btnZoom100: TToolButton
          Left = 46
          Top = 0
          Action = ModActions.actEditorZoomOut
        end
        object ToolButton8: TToolButton
          Left = 69
          Top = 0
          Width = 8
          Caption = 'ToolButton8'
          ImageIndex = 6
          Style = tbsSeparator
        end
      end
      object ToolBarPattern: TToolBar
        Left = 77
        Top = 0
        Width = 198
        Height = 22
        Align = alLeft
        ButtonHeight = 24
        Caption = 'ToolBarPattern'
        Images = ModActions.ToolImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object btnPatternAdd: TToolButton
          Left = 0
          Top = 0
          Hint = 'Add a new pattern by selecting a region in the image'
          Caption = 'TileSelect'
          ImageIndex = 1
          Style = tbsCheck
        end
        object btnPatternImport: TToolButton
          Left = 23
          Top = 0
          Hint = 'Add a new pattern by opening a external image'
          Caption = 'PatternAddImage'
          ImageIndex = 9
          Style = tbsCheck
        end
        object ToolButton7: TToolButton
          Left = 46
          Top = 0
          Width = 8
          Caption = 'ToolButton7'
          ImageIndex = 7
          Style = tbsSeparator
        end
        object btnPatternPick: TToolButton
          Left = 54
          Top = 0
          Hint = 'Add a pattern by selecting a non transparent pixel'
          Caption = 'btnPatternPick'
          ImageIndex = 10
        end
        object btnPatternSelect: TToolButton
          Left = 77
          Top = 0
          Hint = 'Select the pattern bounds in the image'
          AllowAllUp = True
          Caption = 'TileSelect'
          ImageIndex = 7
        end
        object btnPatternMove: TToolButton
          Left = 100
          Top = 0
          Hint = 'Move the selected pattern'
          ImageIndex = 0
        end
      end
      object ToolBarTag: TToolBar
        Left = 275
        Top = 0
        Width = 112
        Height = 22
        Align = alLeft
        ButtonHeight = 23
        Caption = 'ToolBarTag'
        Images = ModActions.ToolImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object btnTagMove: TToolButton
          Left = 0
          Top = 0
          Hint = 'Move the selected tag'
          ImageIndex = 0
        end
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 284
    Top = 89
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
      object Close1: TMenuItem
        Action = ModActions.actFileClose
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
      object actEditCut1: TMenuItem
        Action = ModActions.actEditCut
      end
      object actEditCopy1: TMenuItem
        Action = ModActions.actEditCopy
      end
      object actEditPaste1: TMenuItem
        Action = ModActions.actEditPaste
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Settings1: TMenuItem
        Action = ModActions.actEditSettings
      end
    end
    object Image1: TMenuItem
      Caption = '&Image'
      object Resize1: TMenuItem
        Action = ModActions.actImageResize
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object exture3: TMenuItem
        Action = ModActions.actImageTextureSave
      end
      object exture4: TMenuItem
        Action = ModActions.actImageTextureLoad
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ileWizard1: TMenuItem
        Action = ModActions.actTileWizard
      end
      object actTileEditor1: TMenuItem
        Action = ModActions.actTileEditor
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object Export1: TMenuItem
        Caption = 'Export'
        object Patternsasxml1: TMenuItem
          Action = ModActions.actExportPatternXML
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object agstoxml1: TMenuItem
          Action = ModActions.actExportTagXML
        end
      end
      object Import1: TMenuItem
        Caption = 'Import'
        object Patternsasxml2: TMenuItem
          Action = ModActions.actImportPatternXML
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object agsfromxmldocument1: TMenuItem
          Action = ModActions.actImportTagXML
        end
      end
      object Patternsasimages1: TMenuItem
        Action = ModActions.actExportPatternImages
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object actToolShapes1: TMenuItem
        Action = ModActions.actToolShapeEditor
      end
    end
    object About1: TMenuItem
      Caption = '&About'
      object PhoenixImageEditor1: TMenuItem
        Caption = 'Phoenix Image Editor'
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
