object FrmShapeEditor: TFrmShapeEditor
  Left = 289
  Height = 470
  Top = 160
  Width = 784
  Caption = 'Shape editor'
  ClientHeight = 470
  ClientWidth = 784
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Menu = MainMenu1
  OnShow = FormShow
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object Splitter1: TSplitter
    Left = 214
    Height = 452
    Top = 0
    Width = 4
  end
  object PanelLeft: TPanel
    Left = 0
    Height = 452
    Top = 0
    Width = 214
    Align = alLeft
    BevelOuter = bvNone
    ClientHeight = 452
    ClientWidth = 214
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    object GroupProperties: TGroupBox
      Left = 4
      Height = 171
      Top = 277
      Width = 206
      Align = alClient
      BorderSpacing.Around = 4
      Caption = 'Shape Properties'
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
    object GroupShapes: TGroupBox
      Left = 4
      Height = 200
      Top = 73
      Width = 206
      Align = alTop
      BorderSpacing.Around = 4
      Caption = 'Shape List'
      ClientHeight = 183
      ClientWidth = 204
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
      object lwShapes: TListView
        Left = 0
        Height = 160
        Top = 0
        Width = 204
        Align = alClient
        Columns = <        
          item
            AutoSize = True
            Caption = 'Name'
          end        
          item
            Caption = 'Kind'
            Width = 147
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = ShapeImages
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lwShapesSelectItem
      end
      object ToolBar1: TToolBar
        Left = 0
        Height = 23
        Top = 160
        Width = 204
        Align = alBottom
        Caption = 'ToolBar1'
        Images = ActionImages
        TabOrder = 1
        object btnShapeAdd: TToolButton
          Left = 1
          Hint = 'Add shape'
          Top = 2
          DropdownMenu = PopupMenu1
          ImageIndex = 0
        end
        object btnShapeDelete: TToolButton
          Left = 24
          Top = 2
          Action = actDelete
        end
      end
    end
    object Panel1: TPanel
      Left = 0
      Height = 45
      Top = 24
      Width = 214
      Align = alTop
      BevelOuter = bvNone
      ClientHeight = 45
      ClientWidth = 214
      ParentBackground = False
      TabOrder = 2
      object Label1: TLabel
        Left = 2
        Height = 16
        Top = 0
        Width = 112
        Caption = 'Relative to pattern'
        ParentFont = False
      end
      object cbPatterns: TComboBox
        Left = 2
        Height = 28
        Top = 19
        Width = 210
        Anchors = [akTop, akLeft, akRight]
        ItemHeight = 0
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbPatternsChange
      end
    end
    object ToolBar2: TToolBar
      Left = 0
      Height = 24
      Top = 0
      Width = 214
      AutoSize = True
      Caption = 'ToolBarZoom'
      Images = ActionImages
      TabOrder = 3
      object ToolButton1: TToolButton
        Left = 1
        Height = 22
        Top = 2
        Caption = 'ToolButton1'
        ImageIndex = 10
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 9
        Top = 2
        Action = actFileOpen
      end
      object ToolButton3: TToolButton
        Left = 32
        Top = 2
        Action = actFileSave
      end
      object ToolButton4: TToolButton
        Left = 55
        Height = 22
        Top = 2
        Caption = 'ToolButton4'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton7: TToolButton
        Left = 63
        Top = 2
        Action = actFileOpenList
      end
      object ToolButton9: TToolButton
        Left = 86
        Top = 2
        Action = actFileSaveList
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Height = 18
    Top = 452
    Width = 784
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
  object PanelClient: TPanel
    Left = 218
    Height = 452
    Top = 0
    Width = 566
    Align = alClient
    BevelOuter = bvNone
    ClientHeight = 452
    ClientWidth = 566
    ParentBackground = False
    TabOrder = 2
    object PanelTools: TPanel
      Left = 0
      Height = 24
      Top = 0
      Width = 566
      Align = alTop
      BevelOuter = bvNone
      ClientHeight = 24
      ClientWidth = 566
      ParentBackground = False
      TabOrder = 0
      object ToolBarZoom: TToolBar
        Left = 0
        Height = 24
        Top = 0
        Width = 566
        Align = alClient
        AutoSize = True
        ButtonHeight = 23
        ButtonWidth = 23
        Caption = 'ToolBarZoom'
        EdgeBorders = []
        Images = ModActions.ToolImages
        TabOrder = 0
        object btnZoomIn: TToolButton
          Left = 1
          Top = 0
          Grouped = True
          ImageIndex = 3
          OnClick = btnZoomInClick
        end
        object btnZoomOut: TToolButton
          Left = 24
          Top = 0
          ImageIndex = 5
          OnClick = btnZoomOutClick
        end
        object btnZoom100: TToolButton
          Left = 47
          Top = 0
          ImageIndex = 4
          OnClick = btnZoom100Click
        end
        object ToolButton8: TToolButton
          Left = 70
          Height = 23
          Top = 0
          Caption = 'ToolButton8'
          ImageIndex = 6
          Style = tbsSeparator
        end
      end
    end
    object PanelEditor: TPanel
      Left = 0
      Height = 428
      Top = 24
      Width = 566
      Align = alClient
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 1
    end
  end
  object ActionImages: TImageList
    Left = 340
    Top = 56
    Bitmap = {
      4C7A0B0000001000000010000000BC0900000000000078DAED9A7F5014E719C7
      C97F74ED74ECB41D49DAC948A735B1AD89B69914D4A9064DAA6834526B8CA55A
      C138283432C106221651501451144F620971880545140A7287277A70FC3A1018
      7E2607221EA2C80911387EC822B7EDB7EFBBC71EF76B614FCC683BBE33CF1CEC
      EDE7F93EEFF3EE776FEF051797273754FEAE70140A1252794743B1C1958FA9F0
      AC4E35698E8978B07D7C8E4C1FF11C967C4D882B3463A1F0193B2EE4F0769CC3
      35D01596210CAA690EEFF170C487B3697C8CF32CD0A707ABD7126D0D58AD828F
      B409781F7D34CF5BADE1D81AB0FA1A3E8FC0DBD66CC95BCD85CC9DC0A620B5A4
      798DF342CD4278EBC2CD3F7B6943C6F831B64F47C29EA79ADE34084B837234E6
      A936F0BCD03BCAD248F612AF5FD0B4AA7F4CD7143548F614EF9F75CD3A3B167A
      71DEB266DA633A4FCBA09C10935D3F2E4F79D4D4906B85651FAB8ECCCC4CB8B9
      B9C1CBCBCBE91C94757525DED368101212024F4F4FC9392C59E198D41C8E58A9
      39787692F513CB21E8DAFADF918E6D0EBD5E8FD9B367432693D9F9576C9E94A7
      799CF1BF98BE54FF8BCD5FAAFFC57A27D5FF52D6DF9677F6FAB3F53F5D5367AE
      7FDB9AA9FEE3FAEF69DF37020303F9B9D3D8B06103140A85A47984878763DEBC
      79FCF5AC52A9F8484B23D794B737FCFDFD27EC076585EBDAD1484E4EE6F388D5
      4C7527AB8FE68F8B8BB33B8FD626E5FEADD56A79BFDA1E77777797E4FFB161F7
      1EBDD74AF5BF25EFACFFFBFAFA307DFA74387A6699CCFF74D0DE59AE9154FFD3
      73753A1DEF457ACF73D6FF74ED294B7348F5FF589F781FD3F5B5D495E27F677D
      F3AC7DFE3F1F535F3F5797EF808654DED6FF2FB8BC001ACEF012FCEFF2B8FE9F
      A86629FE9FA8E6C9FC2FB57E47FE77A67F93D5FCDCFFDFCE983B77AEF99E2B16
      63E748FE5C74F29C29F112D70FCEF8FF71F849FC8F29FA1F53F43FA6E87F3CA6
      FFF9B07C7679EEFF2737E8331D7D369CCA7706FA5DE17F3987EDFA399BC3D6FF
      CED661EB5F297391EA7FB11C52FD2F568754FF8BCDC599CFFFC9FA21C5FF3487
      8F8F0FFE1FFDFF7CFF7F6AFBFF0A117D29FBFF6E6E69A63A2D86BB7B9AA4FD7F
      57D764B8BAD780BE0A833FE6A6E15F27DAFF37F3B3F5A6A03FF3BFEB4C4173F2
      7BFF8EF7FF1DE69888B5D9FF779CC3C40A355BF6CE76FFDF9AD55907AF6FADEB
      68FFDF76BEA67E6AC77B3AC1FE3F5D37BECF632CAD91F6D6BC266E2AB84D8F9B
      70FF5FD074D80F07C7A58E8B5F7C813DEBD62274D9DBF8DBD2B710B66C29A237
      6D824AA9E473062E58209A3B66CB16042E5C007ACE11DFD5B812E587EAE37EF8
      DCDF1B61EF7AF3C769149596F339A61D2B8410CBFF14C4BFB76BD50A1C0E0A42
      F4E6CDD8B964114EF8BE833B496BF9D7E24FE743D0A74C21C799233A30009E01
      FBAC6A2B292AC2EE35AB79F666AC27AA3E9DC3F3027B98E5103FC2E1C4230E32
      76940FCB9A68D0F3CBC33D5019FA2BC83F9C65C5470C72881AE21033CC2196E4
      3A4A2281E4FA6C944312795F98AF6D08FC8E1E0E217D1C420D1CC20738440E9A
      7EDFDEDE0FDFAADB564119A5BA0C2565E5669E9E6B1B026B3B0F213C12F3F957
      7AAEAD86A55638ADC926A615F4E237094AFEFD998985A21AF45CE6FA233065C3
      608A07C1141AC05C7B8069B977B1F694823F6746EC65871A3C5BCE82297D08A6
      68000CD164AE7E0346791F3FF8FC3AD2946A9E7F3DA9183FAE3582A9E6C6836A
      6AD8714D550F98FC6E30799D60723B30E7689EF9DA7933AD7A9CAB22792A46C6
      350BFBCC9A8CE21E989C3B602EB662E549B9997F2DAFCDC4568E9A344B86C0A8
      FBC7352FEB794D26BB1D4CA60EDF4D2C47624E81999F51FED0A4497B2468921E
      99352FDD0593D5C6EB32E76FE0475139E66B5BACEF13C56B31FF7AA2CF06C2B5
      3CE79772CCFE851CB35E91C3FDE772BCFC53395E9C29C78C97E5F8E14FE4F8FE
      4B727CEF45B9F9DAB7E4D9440617338C4839338A93498F70E8C40822E258ECDC
      3F8C6D7B1E6273D820D6EF1CC09A8F0CA8F988B1E3874E30E04659702383E0D8
      5E70C35DE0063BC0F5EBC0F5DE80F14123B8EE5A70FA0A5CDF66CFF71F63D0DF
      F7003DDD9DE8BAD78E7BED3770A7B501BAA66AB43468D0545388AF2AF351AF51
      A06C8B3DDF7B84E88FF413ED07E01EEAC10DDC0167B805634F138CDFD483EBAA
      06D7A981F15E118A36DBF3DD8718A27D8F68B711ED66B4DF6CC0ADA62AB4D497
      8E695F21DA72D4166741F5677B5E1F43F487BB611CA273BE0D636F0BB89EAFC8
      9CEB60BC5F49744B61EC50C378F71AAEACB7E7EF463168DBC3E0E62E064D9F30
      680861504BFA5C497AA5F99041911FC3EB5EF9808162AD3DDF1641D87013DB48
      D91D26B68CB0C564BE0502FB4706396BECF956C26A09DB5395068EDCCFE8DF69
      E873766767275A6FB5A2A1BE1EEAD351C8798F41D62A7BBE29D4A44BD9AAEDA4
      E6AD0CBABABBD0DEDE8EAFB55A54575541AD562393B017BCED79CAD6919A8D46
      A379BE1D1D1D6869B981BABA5A94969621FF6A3E2EAC6070EEF7F63C9D2FD5A5
      751713B66023C3FFADA0B1B111151515282828803C3717E9844D5D6ACD5B7AE8
      F8F1E3888F8FE7FF4674E8D021C4C4C4202A2A0A91917B11111161F5D9F12CEF
      5BCF896EC264F1A4BEB73C6DFD67AD7F6955F9389C918293FF4CC13F525391A4
      CE8533FAB24B2AA4777423D33088941E4056D58E2FAF9648D697E5AA91DA3B84
      333DFF467227B0BB15D8597213C1CA5A04E7D521585E097FD95ED19A64972E23
      A58B45CA7D2041558783972B112D2F455816890B05084B5722F46822FCFF7EC2
      E1FF204467C891787704B1956D38AC2CC72EF2F81D46E316F009ADA505F0AF19
      C5FB39CD0EF97D67E538D86CC051328F7D357A0490C7F6BF7C0D6C6C00DEAB34
      6259318B37D26F635144B2433EF24B390E146911AF2C437033E0570FBC5F4DD8
      620E0B95037823CB8097F65EC3E628C7F547C84EE3408E1ADB4B3AF076C12016
      E6F7E34DB91EAF9EB98D9F253563C6E152CCDB122ADABF8F6365084E4885C7FE
      74CC0A3D8D99C1C730D37F1FDCD605C36D6500DCBCD6E2B9FF9F6DFF0F29E560
      CF9DC588B6096C4323FAAB6B61209F237D9A0A180AAE41131000F5D60088F12C
      F98EC69E4AB6DF4CE01E811DEA47E64A2F242C5F26CEE766E37EFC112BF43FC6
      110C0FF4C2D0791B57D72F4181FF56517E282B1B7762A2AD7405B69B3C535D5A
      E501E5465FD15EF79DCF406B64B855CD02DB5E5D84F34B5FC1C575EB45F50DA7
      5381BC5C34EF084275C0465CDFF807BEE64BABE613F65514FA7E80B3ABC5E7FF
      40760A203574C61F45F6C741C8F86B00CE066C456AC016E46FF2459ECF2A2493
      EFC362BCFE481CD8CF4EA12E2214EFFCF6D7583CFF75D4E567233DE920CEAD5B
      81CC654B90F0D66251BE73DF01186263A0DCB6097EEBBC71246227EEB734434E
      EEEBA7DEFD1D52167BE2E8028F09AFD5EAE020242DF7C0313F5F64EC8D44F6FE
      48C491FA0F2C5AC8B3BB3D3CF05FD69D2FDB
    }
  end
  object ActionList1: TActionList
    Images = ActionImages
    Left = 272
    Top = 56
    object actAddPolygon: TAction
      Category = 'Shape'
      Caption = 'Polygon'
      ImageIndex = 1
      OnExecute = actAddPolygonExecute
    end
    object actAddCircle: TAction
      Category = 'Shape'
      Caption = 'Circle'
      ImageIndex = 2
      OnExecute = actAddCircleExecute
    end
    object actAddPoint: TAction
      Category = 'Shape'
      Caption = 'Point'
      ImageIndex = 3
      OnExecute = actAddPointExecute
    end
    object actAddBox: TAction
      Category = 'Shape'
      Caption = 'Box'
      ImageIndex = 4
      OnExecute = actAddBoxExecute
    end
    object actAddLine: TAction
      Category = 'Shape'
      Caption = 'Line'
      ImageIndex = 5
      OnExecute = actAddLineExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open'
      ImageIndex = 7
      OnExecute = actFileOpenExecute
      ShortCut = 16463
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      ImageIndex = 8
      OnExecute = actFileSaveExecute
      OnUpdate = actFileUpdate
      ShortCut = 16467
    end
    object actFileOpenList: TAction
      Category = 'File'
      Caption = 'Open &List'
      ImageIndex = 9
      OnExecute = actFileOpenListExecute
      ShortCut = 24655
    end
    object actFileSaveList: TAction
      Category = 'File'
      Caption = 'Save &List'
      ImageIndex = 10
      OnExecute = actFileSaveListExecute
      ShortCut = 24659
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = actFileExitExecute
    end
    object actDelete: TAction
      Category = 'Shape'
      Caption = 'Delete'
      ImageIndex = 6
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
  end
  object PopupMenu1: TPopupMenu
    Images = ActionImages
    Left = 272
    Top = 120
    object actAddPolygon1: TMenuItem
      Action = actAddPolygon
    end
    object actAddCircle1: TMenuItem
      Action = actAddCircle
    end
    object actAddPoint1: TMenuItem
      Action = actAddPoint
    end
    object actAddLine1: TMenuItem
      Action = actAddLine
    end
    object actAddBox1: TMenuItem
      Action = actAddBox
    end
  end
  object MainMenu1: TMainMenu
    Left = 344
    Top = 120
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        SubMenuImages = ActionImages
        object Polygon2: TMenuItem
          Action = actAddPolygon
        end
        object Circle2: TMenuItem
          Action = actAddCircle
        end
        object Line2: TMenuItem
          Action = actAddLine
        end
        object Box2: TMenuItem
          Action = actAddBox
        end
        object Point2: TMenuItem
          Action = actAddPoint
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object Save1: TMenuItem
        Action = actFileSave
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Openall1: TMenuItem
        Action = actFileOpenList
      end
      object Saveall1: TMenuItem
        Action = actFileSaveList
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actFileExit
      end
    end
    object Shape1: TMenuItem
      Caption = '&Shape'
      object Polygon1: TMenuItem
        Action = actAddPolygon
      end
      object Circle1: TMenuItem
        Action = actAddCircle
      end
      object Point1: TMenuItem
        Action = actAddPoint
      end
      object Box1: TMenuItem
        Action = actAddBox
      end
      object Line1: TMenuItem
        Action = actAddLine
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.phxshp'
    Filter = 'Phoenix shape files (*.phxshp)|*.phxshp|All files (*.*)|*.*'
    Left = 270
    Top = 178
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.phxshp'
    Filter = 'Phoenix shape files (*.phxshp)|*.phxshp|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 342
    Top = 178
  end
  object OpenDialogList: TOpenDialog
    DefaultExt = '.shapes'
    Filter = 'Phoenix shape list (*.shapes)|*.shapes|All files (*.*)|*.*'
    Left = 270
    Top = 234
  end
  object SaveDialogList: TSaveDialog
    DefaultExt = '.shapes'
    Filter = 'Phoenix shape list (*.shapes)|*.shapes|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 342
    Top = 234
  end
  object ShapeImages: TImageList
    Left = 412
    Top = 56
    Bitmap = {
      4C7A060000001000000010000000960000000000000078DAED58410AC0200CEB
      D3FB73771244B6599BC03A4CC0D39AACA4EDD4990945D1A6751A5FF8A6CF320B
      7DB7F8E28B5F6C76DD3D9D5FE7663466CE8EC6536C446315F3F63C9AE35DDCAE
      47637CB646030FE142B933BC63D48ED13B8CDE65CC0E6376B3E7B7454F95ACCF
      901BE26DF67C6CA6FD0BF50FAE5FC1FDA511BC8ECE6FFFCE4077086948431AFF
      D3D0EF0F41381717A8002D36
    }
  end
end
