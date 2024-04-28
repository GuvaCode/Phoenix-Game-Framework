object frmPacker: TfrmPacker
  Left = 218
  Height = 481
  Top = 115
  Width = 702
  Caption = 'Texture packer'
  ClientHeight = 481
  ClientWidth = 702
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poScreenCenter
  LCLVersion = '3.2.0.0'
  object Splitter1: TSplitter
    Left = 225
    Height = 449
    Top = 0
    Width = 5
  end
  object ScrollBox1: TScrollBox
    Left = 230
    Height = 449
    Top = 0
    Width = 472
    HorzScrollBar.Page = 107
    VertScrollBar.Page = 107
    Align = alClient
    ClientHeight = 447
    ClientWidth = 470
    ParentBackground = False
    TabOrder = 0
    object PaintBox1: TPaintBox
      Left = 2
      Height = 105
      Top = 2
      Width = 105
      OnPaint = PaintBox1Paint
    end
  end
  object Panel2: TPanel
    Left = 0
    Height = 32
    Top = 449
    Width = 702
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 32
    ClientWidth = 702
    ParentBackground = False
    TabOrder = 1
    object Panel1: TPanel
      Left = 541
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
  object Panel3: TPanel
    Left = 0
    Height = 449
    Top = 0
    Width = 225
    Align = alLeft
    BevelOuter = bvNone
    ClientHeight = 449
    ClientWidth = 225
    ParentBackground = False
    TabOrder = 2
    object GroupBox2: TGroupBox
      Left = 0
      Height = 153
      Top = 296
      Width = 225
      Align = alBottom
      Caption = 'Packer settings'
      ClientHeight = 138
      ClientWidth = 223
      ParentBackground = False
      TabOrder = 0
      object Label1: TLabel
        Left = 11
        Height = 14
        Top = 44
        Width = 72
        Caption = 'Image width:'
      end
      object Label2: TLabel
        Left = 11
        Height = 14
        Top = 71
        Width = 77
        Caption = 'Image height:'
      end
      object Label3: TLabel
        Left = 11
        Height = 14
        Top = 97
        Width = 91
        Caption = 'Pattern padding:'
      end
      object btnPack: TButton
        Left = 140
        Height = 25
        Top = 121
        Width = 75
        Caption = 'Pack'
        TabOrder = 0
        OnClick = btnPackClick
      end
      object cbWidth: TComboBox
        Left = 98
        Height = 27
        Top = 40
        Width = 121
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        ItemHeight = 0
        Items.Strings = (
          '32'
          '64'
          '128'
          '256'
          '512'
          '1024'
          '2048'
          '4096'
          ''
        )
        ParentFont = False
        TabOrder = 1
        Text = '256'
      end
      object cbHeight: TComboBox
        Left = 98
        Height = 27
        Top = 67
        Width = 121
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        ItemHeight = 0
        Items.Strings = (
          '32'
          '64'
          '128'
          '256'
          '512'
          '1024'
          '2048'
          '4096'
          ''
        )
        ParentFont = False
        TabOrder = 2
        Text = '256'
      end
      object cbAutoSize: TCheckBox
        Left = 11
        Height = 23
        Top = 17
        Width = 188
        Caption = 'Determine size automatically'
        TabOrder = 3
        OnClick = cbAutoSizeClick
      end
      object edPadding: TSpinEdit
        Left = 98
        Height = 26
        Top = 94
        Width = 121
        TabOrder = 4
      end
    end
    object GroupBox1: TGroupBox
      Left = 0
      Height = 296
      Top = 0
      Width = 225
      Align = alClient
      Caption = 'Source images'
      ClientHeight = 281
      ClientWidth = 223
      ParentBackground = False
      TabOrder = 1
      object lwImages: TListView
        Left = 0
        Height = 257
        Top = 0
        Width = 223
        Align = alClient
        Columns = <        
          item
            Caption = 'Name'
            Width = 100
          end        
          item
            Caption = 'X'
          end        
          item
            Caption = 'Y'
            Width = 58
          end>
        PopupMenu = PopupMenu1
        SmallImages = ImageList1
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lwImagesSelectItem
      end
      object ToolBar3: TToolBar
        Left = 0
        Height = 24
        Top = 257
        Width = 223
        Align = alBottom
        Caption = 'ToolBar2'
        Images = ImageList2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object ButtonAdd: TToolButton
          Left = 1
          Hint = 'Add a image to pack'
          Top = 2
          Caption = 'ButtonAdd'
          ImageIndex = 1
          OnClick = ButtonAddClick
        end
        object ButtonDel: TToolButton
          Left = 24
          Hint = 'Remove the selected image'
          Top = 2
          Caption = 'ButtonDel'
          ImageIndex = 0
          OnClick = ButtonDelClick
        end
      end
    end
  end
  object ImageList1: TImageList
    Left = 352
    Top = 156
    Bitmap = {
      4C7A010000001000000010000000900100000000000078DA7D532D90842014DE
      B871E3D58DC68D5623D168351A8D5622914825128956A2D168DDB8F1E2BBF7A1
      30ACB777CC30E0C8F7F3FE2E97DFCBFA99A471D44B4BED64685096E639D0E58F
      659CA766D074EF24DD5A495F9D8AE7BDD7F4184DFE069763EE331E38E9579AE6
      8D46BFD1E0566AA4CB1CB8D7938BF7AB98488CFA8D07EF5478925B5F649617C9
      F9491378ECC2FA36F2C087509E5ADE0DFB806689C77BC558BF7D93671E155EA4
      9953B327E902C720A9636CA77DF4031F257E64CFD04D1CCBC65EE695461BA2EE
      ADDB3D208E7AB2EF78BE578389B14B682EDF112BE4AE9572B1E32DD5E33B1E77
      E4061B1CF0AEFC92B119CF1AC0E23CE33B1372AD46AE05624E5868028F8DB769
      9778C4D89B90B504F70EFC941870234EE89478F405FEB57A668E25D6E8DEABDC
      3FF551C36AD8BFAB93FF1002550737E240DD51AB94ABD40315F3E1CD198F857E
      BAB553AC0FE21047DC8F8837B917E10BF79ABD9CFB1833731532E6A229B0F09D
      728039C13B78FE344BE2C8057049EBC1E7A8FF9FC13217980FF437E6F6D3BC95
      EB074965DA90
    }
  end
  object OpenImageDialog: TOpenDialog
    Filter = 'All supported|*.bmp;*.jpg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 258
    Top = 150
  end
  object ImageList2: TImageList
    Left = 424
    Top = 160
    Bitmap = {
      4C7A0400000010000000100000004D0200000000000078DAED952D90A33014C7
      57469EC4562291582412595B89ACAC5DB9F224165989C4229191B1C84AE4BBF7
      1202499A4076BB3737777399F94FBB2CBFF7FDD2B7B7EF3BFD85814F1D2A96F7
      9DEECCA45EE167D11FDAD8E3617E481BF72A6CC3E4C72B836151572DCFB58DD2
      6FA30BF8279FABCA4D269B24AD8AD338A7534B4E011E13CC1347DF03CCBC936A
      0D9EB106D86904FAD4473E4B06F9297BB8F4609E4669E7894F2725FA2EFF164A
      6413F3569A943096B6B0E3B76CECB10F817AE66D1B8AD5319BB52396D478FD0B
      5BD2BFED5769842677F3DF18554FBED5D46161DA78EA9BACF3C2528C54DBB527
      490FC98F0F992F89382DCB3FCA5B0FCFF3DF75FEEFFF6BFBCF6AECBBA1CFECBF
      E66F732BB5F1C7FBEFF2D5F42E79AB8781FD776336792B97C0FE9B316B95E2B6
      7E2FF875E1FDFB6FFA2C49C892882365FD59F2A1FDF7C5AF7D5AF1EFECBF5B3F
      3B66B1BBFF2E6FC64C35D67BBFB7FFA1F989D9B93ACF2156215ECE98EE2FE58A
      FDA51C01671D4407C0EFFBFCBCF00FCD538D06C5F363FE95F8FFF4898D3DD4CF
      98FA135B4D37AF8DA3FA2BF68ABB8512F5938DA398252B145BF00BE4E3396A36
      37B6465D16B64295900DC5AE0D770F0A7E5E59F77FAC6E0EC4249B0D7EBFF48E
      C0F28DA81E4B784735A80F2CE50D45EF6B36ED732FCF911D501D722DEA2772EF
      A8EBCA2BF6D4655EFE28FEBD7BE13BF8A3FC69F6AAC0FCC4E46FCECF57E22FE4
      FCA819889D3DDFFC501F63EF58C5E1CC1A2CF530ED33EC63BAB247FBA3E75DF7
      DF6463F69FDED56CE2B0B1F72F31C93D0DDE1FAFDC5F7FCBFDFBAFFEFEFD0290
      EEA599
    }
  end
  object PopupMenu1: TPopupMenu
    Left = 104
    Top = 148
    object Insert1: TMenuItem
      Caption = '&Insert'
      OnClick = ButtonAddClick
    end
    object btnClear: TMenuItem
      Caption = 'Clear'
      OnClick = btnClearClick
    end
  end
end
