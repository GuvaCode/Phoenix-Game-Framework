object FrmEffectAffector: TFrmEffectAffector
  Left = 0
  Top = 0
  Width = 315
  Height = 320
  Padding.Left = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 4
    Top = 145
    Width = 307
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 153
  end
  object Panel1: TPanel
    Left = 4
    Top = 0
    Width = 307
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lwAffectors: TListView
      Left = 0
      Top = 24
      Width = 307
      Height = 121
      Align = alClient
      Columns = <
        item
          Caption = 'Index'
          Width = 20
        end
        item
          AutoSize = True
          Caption = 'Name'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lwAffectorsSelectItem
    end
    object ToolBar3: TToolBar
      Left = 0
      Top = 0
      Width = 307
      Height = 24
      Caption = 'ToolBar2'
      Images = ImageList1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object btnAdd: TToolButton
        Left = 0
        Top = 0
        Action = actAffectorAdd
        DropdownMenu = MenuAffectors
      end
      object btnDelete: TToolButton
        Left = 23
        Top = 0
        Action = actAffectorDel
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 151
    Width = 307
    Height = 165
    Align = alClient
    Caption = 'Affector properties'
    TabOrder = 1
  end
  object ImageList1: TImageList
    Left = 100
    Top = 36
    Bitmap = {
      494C010104000900780110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008089C0008089C000000
      000000000000000000000000000008089C0008089C0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000086B
      0800086B0800086B0800086B0800000000000000000000000000000000000000
      000000000000000000000000000000000000086B9C00086B9C00086B9C00086B
      9C00086B9C00086B9C00086B9C00086B9C00086B0800086B0800086B0800086B
      0800086B08000000000000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B3939006B3939000000
      0000000000000000000000000000000000001818A5000818C6000829DE000808
      9C00AD4A0800A54A080008089C000821CE000818BD0018108C00A5420800A542
      08009C3908009C3908009C39080000000000000000000000000000000000086B
      080042D6730031BD5A00086B0800A54A0800A5420800A5420800A5420800A542
      08009C3908009C3908009C39080000000000086B9C00D6F7FF00C6F7FF00B5EF
      FF00A5EFFF0094E7FF0084E7FF0073E7FF00086B080031C6420029BD390021B5
      3100086B08000000000000000000000000006B393900FFF7F700FFF7EF00FFEF
      E700FFEFDE00FFE7CE00FFE7C600FFDEBD00FFD6B500FFD6AD006B3939000000
      0000000000000000000000000000000000000000000008089C000829DE000829
      DE0008089C0008089C000821D6000821CE0008089C00FFEFDE00FFEFD600FFEF
      D600FFEFCE00FFE7CE009C39080000000000000000000000000000000000086B
      08007BF7A50042D67300086B0800FFF7E700FFF7DE00FFEFDE00FFEFD600FFEF
      D600FFEFCE00FFE7CE009C39080000000000086B9C00DEF7FF00CEF7FF00BDEF
      FF00ADEFFF009CEFFF008CE7FF007BE7FF00086B080031C64A0031C6420029BD
      3900086B08000000000000000000000000006B393900FFFFF700FFF7EF00FFF7
      E700FFEFDE00FFE7D600FFE7CE00FFDEBD00FFDEB500FFD6AD006B3939000000
      000000000000000000000000000000000000000000000000000008089C000829
      DE000829DE000829DE000829DE0008089C00FFF7E700FFF7E700FFEFDE00FFEF
      D600FFEFD600FFEFCE009C39080000000000086B0800086B0800086B0800086B
      08007BF7A50042D67300086B0800086B0800086B0800086B0800FFEFDE00FFEF
      D600FFEFD600FFEFCE009C39080000000000086B9C00086B9C00086B9C00086B
      9C00086B9C00086B9C00086B9C00086B9C00086B080039CE520031C64A0031C6
      4200086B08000000000000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B3939006B3939000000
      0000000000000000000000000000000000000000000000000000000000000808
      9C000829E7000829DE0008089C00B55A0800B55A0800AD520800AD520800AD4A
      0800FFEFD600FFEFD6009C42080000000000086B08007BF7A50052E784004AE7
      84004ADE7B004ADE7B0042D6730042D6730031BD5A00086B0800AD520800AD4A
      0800FFEFD600FFEFD6009C420800000000000000000000000000000000000000
      000000000000086B0800086B0800086B0800086B080042D65A0039CE520039CE
      4A00086B0800086B0800086B0800086B08000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000008089C000829
      E7000829E7000829DE000829DE0008089C00FFF7EF00FFF7EF00FFF7E700FFF7
      E700FFEFDE00FFEFD600A542080000000000086B08007BF7A5007BF7A5007BF7
      A5007BF7A5004ADE7B007BF7A5007BF7A50042D67300086B0800FFF7E700FFF7
      E700FFEFDE00FFEFD600A5420800000000000000000000000000000000000000
      00000000000000000000086B080052E773004ADE6B004ADE630042D6630039CE
      520039CE4A0031C64200086B0800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008089C000829E7000829
      E70008089C0008089C000829DE000829DE0008089C00FFF7EF00FFF7EF00FFF7
      E700FFF7E700FFE7D600A542080000000000086B0800086B0800086B0800086B
      08007BF7A50052E78400086B0800086B0800086B0800086B0800FFF7EF00FFF7
      E700FFF7E700FFE7D600A5420800000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B393900086B080052E773004ADE73004ADE6B0042D6
      630039CE5A00086B080000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B393900086B08000000
      0000000000000000000000000000000000000000000008089C000821CE000808
      9C00FFFFFF00FFFFFF0008089C000818C60008089C00BD630800B55A0800B55A
      0800F7E7CE00F7DEC600A54A080000000000000000000000000000000000086B
      08007BF7A50052E78400086B0800BD630800BD630800BD630800B55A0800B55A
      0800F7E7CE00F7DEC600A54A0800000000006B393900FFF7F700FFF7EF00FFEF
      E700FFEFDE00FFE7CE00FFE7C600FFDEBD00086B080052E773004AE773004ADE
      6B00086B08000000000000000000000000006B393900FFF7F700FFF7EF00FFEF
      E700FFEFDE00FFE7CE00FFE7C600FFDEBD00FFD6B500086B080018AD2900086B
      08000000000000000000000000000000000000000000000000001818A500B55A
      0800FFFFFF00FFFFFF00FFFFFF002121A500FFFFF700FFEFE700F7E7D600F7DE
      C600F7D6B500F7D6B500A54A080000000000000000000000000000000000086B
      08007BF7A5007BF7A500086B0800FFFFFF00FFFFF700FFEFE700F7E7D600F7DE
      C600F7D6B500F7D6B500A54A0800000000006B393900FFFFF700FFF7EF00FFF7
      E700FFEFDE00FFE7D600FFE7CE00FFDEBD00FFDEB500086B080052E77B00086B
      0800000000000000000000000000000000006B393900FFFFF700FFF7EF00FFF7
      E700FFEFDE00FFE7D600FFE7CE00FFDEBD00086B080029BD390021B5310018B5
      2900086B0800000000000000000000000000000000000000000000000000B563
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AD520800AD520800AD52
      0800AD4A0800AD4A0800AD4A080000000000000000000000000000000000086B
      0800086B0800086B0800086B0800FFFFFF00FFFFFF00AD520800AD520800AD52
      0800AD4A0800AD4A0800AD4A0800000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B393900086B08000000
      0000000000000000000000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B393900086B080039CE4A0031C6420029BD390021B5
      310021B52900086B08000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00CE730800C6730800C6730800B5520800FFFFFF00FFF7
      EF00F7DEBD00AD4A08000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00CE730800C6730800C6730800B5520800FFFFFF00FFF7
      EF00F7DEBD00AD4A080000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000086B080042D6630039D65A0039CE520031C64A0029BD
      390029BD310021B52900086B080000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800FFF7EF00F7DE
      BD00AD520800000000000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800FFF7EF00F7DE
      BD00AD5208000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000086B0800086B0800086B0800086B080042D65A0039CE520031C6
      4A00086B0800086B0800086B0800086B0800000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800F7DEBD00B55A
      080000000000000000000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800F7DEBD00B55A
      0800000000000000000000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B3939006B3939000000
      000000000000000000000000000000000000086B9C00086B9C00086B9C00086B
      9C00086B9C00086B9C00086B9C00086B9C00086B080042DE630042D65A0039CE
      5200086B0800000000000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800B55A08000000
      000000000000000000000000000000000000000000000000000000000000BD63
      0800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B55A0800B55A08000000
      0000000000000000000000000000000000006B393900FFF7F700FFF7EF00FFEF
      E700FFEFDE00FFE7CE00FFE7C600FFDEBD00FFD6B500FFD6AD006B3939000000
      000000000000000000000000000000000000086B9C00D6F7FF00C6F7FF00B5EF
      FF00A5EFFF0094E7FF0084E7FF0073E7FF00086B08004ADE73004ADE6B0042D6
      6300086B0800000000000000000000000000000000000000000000000000BD63
      0800BD630800BD630800BD630800BD630800BD630800B5630800000000000000
      000000000000000000000000000000000000000000000000000000000000BD63
      0800BD630800BD630800BD630800BD630800BD630800B5630800000000000000
      0000000000000000000000000000000000006B393900FFFFF700FFF7EF00FFF7
      E700FFEFDE00FFE7D600FFE7CE00FFDEBD00FFDEB500FFD6AD006B3939000000
      000000000000000000000000000000000000086B9C00DEF7FF00CEF7FF00BDEF
      FF00ADEFFF009CEFFF008CE7FF007BE7FF00086B080052E77B0052E773004ADE
      6B00086B08000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006B3939006B3939006B3939006B39
      39006B3939006B3939006B3939006B3939006B3939006B3939006B3939000000
      000000000000000000000000000000000000086B9C00086B9C00086B9C00086B
      9C00086B9C00086B9C00086B9C00086B9C00086B0800086B0800086B0800086B
      0800086B0800000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000009E7FE1FF0007001F0001E0010007001F
      8001E0010007001FC00100010007001FE0010001F800FFFFC0010001FC01FFFF
      800100010003001F8001E0010007000FC001E001000F0007E001E001001F0003
      E003E003FFFFFC01E007E007FFFFF800E00FE00F001F0007E01FE01F001F0007
      E03FE03F001F0007FFFFFFFF001F000700000000000000000000000000000000
      000000000000}
  end
  object MenuAffectors: TPopupMenu
    OnPopup = MenuAffectorsPopup
    Left = 184
    Top = 40
    object MenuAffectorsItem: TMenuItem
      OnClick = MenuAffectorsItemClick
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 40
    Top = 48
    object actAffectorAdd: TAction
      Caption = 'actAffectorAdd'
      ImageIndex = 1
      OnExecute = actAffectorAddExecute
      OnUpdate = actAffectorAddUpdate
    end
    object actAffectorDel: TAction
      Caption = 'actAffectorDel'
      ImageIndex = 0
      OnExecute = actAffectorDelExecute
      OnUpdate = actAffectorDelUpdate
    end
  end
end