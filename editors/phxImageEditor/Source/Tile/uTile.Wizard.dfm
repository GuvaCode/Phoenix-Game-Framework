object frmTileWizard: TfrmTileWizard
  Left = 411
  Height = 579
  Top = 182
  Width = 705
  Caption = 'Tile Wizard'
  ClientHeight = 579
  ClientWidth = 705
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Panel1: TPanel
    Left = 233
    Height = 541
    Top = 0
    Width = 472
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ClientHeight = 541
    ClientWidth = 472
    ParentBackground = False
    TabOrder = 0
    object ScrollBox1: TScrollBox
      Left = 0
      Height = 518
      Top = 23
      Width = 472
      HorzScrollBar.Page = 109
      VertScrollBar.Page = 109
      Align = alClient
      ClientHeight = 516
      ClientWidth = 470
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      object PaintBox1: TPaintBox
        Left = 4
        Height = 105
        Top = 4
        Width = 105
        OnPaint = PaintBox1Paint
      end
    end
    object ToolBar1: TToolBar
      Left = 0
      Height = 23
      Top = 0
      Width = 472
      Caption = 'ToolBar1'
      Images = ImageList2
      ParentFont = False
      TabOrder = 1
      object btnZoomIn: TToolButton
        Left = 1
        Top = 2
        Caption = 'btnZoomIn'
        ImageIndex = 3
        OnClick = btnZoomInClick
      end
      object btnZoom100: TToolButton
        Left = 24
        Top = 2
        Caption = 'btnZoom100'
        ImageIndex = 4
        OnClick = btnZoom100Click
      end
      object btnZoomOut: TToolButton
        Left = 47
        Top = 2
        Caption = 'btnZoomOut'
        ImageIndex = 5
        OnClick = btnZoomOutClick
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Height = 38
    Top = 541
    Width = 705
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 38
    ClientWidth = 705
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    object brnOkey: TButton
      Left = 540
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 621
      Height = 25
      Top = 6
      Width = 75
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 0
    Height = 541
    Top = 0
    Width = 233
    Align = alLeft
    BevelOuter = bvNone
    ClientHeight = 541
    ClientWidth = 233
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
    object Label11: TLabel
      Left = 9
      Height = 16
      Top = 40
      Width = 62
      Caption = 'Tile &width:'
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Height = 16
      Top = 72
      Width = 67
      Caption = 'Tile &height:'
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Height = 16
      Top = 296
      Width = 83
      Caption = 'Max columns:'
      ParentFont = False
    end
    object Label3: TLabel
      Left = 9
      Height = 16
      Top = 328
      Width = 61
      Caption = 'Max rows:'
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 168
      Width = 119
      Caption = 'Horisontal padding:'
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 200
      Width = 101
      Caption = 'Vertical padding:'
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Height = 16
      Top = 264
      Width = 57
      Caption = 'Max tiles:'
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Height = 16
      Top = 8
      Width = 95
      Caption = '&Name template:'
      ParentFont = False
    end
    object Label8: TLabel
      Left = 9
      Height = 16
      Top = 232
      Width = 66
      Caption = 'Skip count:'
      ParentFont = False
    end
    object Label9: TLabel
      Left = 9
      Height = 16
      Top = 104
      Width = 105
      Caption = 'Horisontal &offset:'
      ParentFont = False
    end
    object Label10: TLabel
      Left = 9
      Height = 16
      Top = 136
      Width = 87
      Caption = 'Vertical &offset:'
      ParentFont = False
    end
    object GroupBox1: TGroupBox
      Left = 9
      Height = 88
      Top = 368
      Width = 217
      Caption = 'Existing patterns'
      ClientHeight = 71
      ClientWidth = 215
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      object cbAppend: TRadioButton
        Left = 8
        Height = 23
        Top = 8
        Width = 74
        Caption = 'Append'
        TabOrder = 0
      end
      object RadioButton2: TRadioButton
        Left = 8
        Height = 23
        Top = 40
        Width = 73
        Caption = 'Replace'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object edTileName: TEdit
      Left = 136
      Height = 28
      Top = 8
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      ParentFont = False
      TabOrder = 1
      Text = 'Tile%d'
      OnChange = EditChange
    end
    object cbDrawIndex: TCheckBox
      Left = 9
      Height = 23
      Top = 488
      Width = 116
      Caption = 'Draw tile index'
      Checked = True
      ParentFont = False
      State = cbChecked
      TabOrder = 2
      OnClick = cbDrawIndexClick
    end
    object edOffsetX: TSpinEdit
      Left = 136
      Height = 28
      Top = 104
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 3
    end
    object edOffsetY: TSpinEdit
      Left = 136
      Height = 28
      Top = 136
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 4
    end
    object edWidth: TSpinEdit
      Left = 136
      Height = 28
      Top = 40
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 5
      Value = 32
    end
    object edHeight: TSpinEdit
      Left = 136
      Height = 28
      Top = 72
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 6
      Value = 32
    end
    object edHorzSpace: TSpinEdit
      Left = 136
      Height = 28
      Top = 168
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 7
    end
    object edVertSpace: TSpinEdit
      Left = 136
      Height = 28
      Top = 200
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 8
    end
    object edSkip: TSpinEdit
      Left = 136
      Height = 28
      Top = 232
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 9
    end
    object edMaxTiles: TSpinEdit
      Left = 136
      Height = 28
      Top = 264
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 10
      Value = -1
    end
    object edMaxCols: TSpinEdit
      Left = 136
      Height = 28
      Top = 296
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 11
      Value = -1
    end
    object edMaxRows: TSpinEdit
      Left = 136
      Height = 28
      Top = 328
      Width = 90
      Anchors = [akTop, akLeft, akRight]
      OnChange = EditChange
      ParentFont = False
      TabOrder = 12
      Value = -1
    end
    object edCenterPivots: TCheckBox
      Left = 9
      Height = 23
      Top = 464
      Width = 106
      Caption = 'Center Pivots'
      ParentFont = False
      TabOrder = 13
      OnClick = cbDrawIndexClick
    end
  end
  object ImageList2: TImageList
    Left = 364
    Top = 118
    Bitmap = {
      4C7A0B0000001000000010000000530700000000000078DAED997B6C53551CC7
      F9B3E930804452A7261427C98846EB8B9461841A22CC18A406C16274381E6EC5
      217640A49B8A14065B794C8B8651E603A7A254035A9EEBD8B31899458C961043
      13D1A4461747D4D8189B7C3DBFD39DF6F6F6F6F6B693876627F9E69EDD9ECFF9
      FDCEEF9CDFCEBDE78E1A95BD18753A8C2AB0105B6ED0C150401FC41A383F965F
      C716E8876118FE53C9C7AE52AC94789DC23D315EADBCB40F116769AC84CCEC9E
      682F643218B8A47D18B2D8D769B09FEF587579C45437CCF92B9497C62ADB78B5
      F4218FF3E5F23F9F12F5592157A44D26AF1561B93C56843C89F6DA4B2CA5582C
      8DB79BCD5CB9D8643BE2DD997C3496AA47583D3298FA3B24A92336C8791A9F28
      C40A09363C246283A42838ABC40BFB4A6C688815B64941573AAFB3EBD22467D3
      F9A822EF8CB571515DCE068644AC1A6F8DBA92BC18B36013F6596530C1CB7D96
      F25211EB17FCE010EFB4A6F92C541E7126EB96B083B7F10FF1240C46B802129E
      6C9693184B228E640AD8781BB22BEC277C48F172FF854DA9123E47247C98F394
      07B41649725EC449B0E94AF021CE27D60331529F95D97042D1144FEB9024F799
      E68744ED32E4486824FFAF9EFC97FAACC4AAE5BFD1D896C1D23D2DF9AFD379A1
      3386F855B0FC9E21C8AF6AF94F6B81B72D8D264475FE7724215617AC52FE134F
      F39AD647165629FF054FFDA6FA4867C59C2BE53FE763B1749F25F645ACD4F25F
      3EDE443CC3C998AAE5BFD1E049C439692F31B7C9393104F83CAAE5BFB049EB31
      E8B626F35EDC57CB7FE9B340201040797939CC26534A6CAC1E8F27E77388CFE7
      83A9B4144E9B0D7ED63ECCFEF6BBDDF0D8EDB0B03E1CECAAC69B982D576525A2
      A1101B660411E64BC8EBE5F2B95CB0B0DFBDACAEC4927F56CA1DC60C46A388F8
      FD08B7B5711F789DC9CDEC975B2C8A7C25B34B7E131B0D87B94D62A2C120F787
      4463A278A8F1C25FB2CD7D617DC5060779BF01762F1B2FFCCFE0591C626C6D52
      1F6D2C96D978BF9FAD11F6ACD9E67024FBA0712763C9AE363676A7D3894C36C8
      D69785C796FAF00EF5C17D607D04D9D5CED6846568DD93D2E7CD07A7DBCF7EB7
      C06434A29489E6CACEECD9585B5A13562B5B8F2C964ABC3F1C83D307188C2ED8
      ED09FFDA984DF2D5C5E65DDA568927D66866E3B3F915636397F8AD2435560B6F
      ABF46B7E4657F23F9F72A9F991FC1FC97FB5FC579E37EDF9AFE8BB2CFFE99D57
      E43FC555BC7F8B354857E93D69FECBDFD595EAF27BF2FCCF9797E77FBEBCDA3B
      7FBEBCF49C472956D9EE8DE4FF48FE5F2DF9AFB6FF2B95FFD2FE7FB59791FC1F
      C97F79FE8BBDC2C2DFAF537B4BF28C3C47FE539B6CF94FBF5DEAFC573B4357DA
      FFF3E52F75FEE7B2AFF55B46B6FAC8F9DFFFE3FC8F4A2E36D7F93FF59DC1E638
      FF4F9CBF2139376AACDAF91F29179BEBFC4F3EC7F99CFF49D7454A514DE77FE2
      FC5F91CDE3FC5F1B9B7EFE47B92CCEFFE5E77F5ACEFFD3F621F3D88CFF276EB3
      21E39ECB6C54DEC7AE004F8C903B2943522E2E639A9C5CA55CE9B6C62AD8CAB4
      2FE7F21DEB95E61D123E335686ACB1223992327115122B3997EF582F27EFC8C1
      E7132B213B97C259A6465FED59FCD7CEA7EC0E67FF177CA1FBBF94FF37F67FCB
      E67E3C5ED78A85EB76E1B1351ECC77EC80F5B946CCABD984B9D5EBF1F0F23ADC
      B67467D6FD7F597D333A96E9D1536BC4C997EEE275B91E5BF24CD6FDBF7A4333
      BE6EBC173FF5F830D0DF89B36FCCC329E718841A6E45FFCB37E1D40B455854B5
      4CF3F7FF1F0FBE8F2F77CC4557F353E8DEFA08429B4BF064CD72CDDFFFCFB478
      D0DBFC32DED8F604B6ECAB8267D7423C54BD46F3F7FFD5871DD878BA0955175F
      C5D2DF9AB164602BEE6C6C2AE8FBFF83C1A578A0AB0237D7BD50D0F7FF894D53
      70E3A65B307EC9E282BEFF5F5B518D314F54E29A47575F55DFFF290786F3FD5F
      BAFF17F2FD5FCB3BC8821D1FE3C9D73FC9DAF644CF49C5DFB6EF3F8671ABDE82
      FEB51E98BC3D59F9770E9D80DEDE02FDDECF51B4FF0CC6EF3C81E2577C286AF8
      1473D9039DBE3F8E292DDDAABEDEBBFE3D8CA9DF8789E18B28FE3E8E4991389A
      7E8EE37C2C8ED15FC631D9DBABCA1FEEECC5E8155EDC76E81B2CFE258E865FE3
      38F05B1CDFFE19C7843371E80F4751BCBD1D2D073A14FB69FDAC0345AB5A51F2
      C50F983FC06C5F8C633FE3438C3FF5471C36E60F8D437FE0028AB71D4FEBE7C3
      A3DD18FD5C2BF49DE73091D9B5323530BE95A9FDF738DE1EF81B4FF55CC00D3B
      BB70CDBA7DB8BDFE5DACDCF329E78F77F7625CCD1EE81B0F40BFAB1DD7ED6EC7
      FDFB7AB1A4EF3B6C19F80B4FFF4436CFA162E727203B197BDE7B4761DF7D1074
      AD5EBD012B16D870706B254A56B56051C759949C8FE1FA5D9D39D749E7B163A8
      9D654157FD03F8E8E9BB51396B368FA77E4F0F6ADF3F9E937FF1E1D9F0CC9F8E
      AF363F847533EE8167ED5A504C273CFF664E766FE316AC993513A71B66E18385
      93B172FA34F4F5F571AEA7EFA42A4FED6A6796E1E3AAA9E85B7D0FEA66DC01F7
      8A6ACDEFF41BAC73B071CE54F4AF33E1DDB9C578B6CC8CEE8E0ECD7CCDF432F8
      2A26E1C8A209A89F3E051B1757E4759EB0A3B6162BEF9B067B59196AD8B80347
      8EE4C5FF03CC4CAA0C
    }
  end
end
