object FrmEffect: TFrmEffect
  Left = 0
  Top = 0
  Width = 308
  Height = 571
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 308
    Height = 571
    ActivePage = TabAffector
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 300
    object TabBasic: TTabSheet
      Caption = '&Basic'
      ExplicitWidth = 292
    end
    object TabEmittor: TTabSheet
      Caption = '&Emittor'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 28
      ExplicitWidth = 292
    end
    object TabApperance: TTabSheet
      Caption = '&Apperance'
      ImageIndex = 1
      ExplicitWidth = 292
    end
    object TabPhysics: TTabSheet
      Caption = '&Physics'
      ImageIndex = 2
      ExplicitWidth = 292
    end
    object TabAffector: TTabSheet
      Caption = 'Affectors'
      ImageIndex = 4
      ExplicitLeft = 8
      ExplicitTop = 28
      ExplicitWidth = 292
    end
  end
end
