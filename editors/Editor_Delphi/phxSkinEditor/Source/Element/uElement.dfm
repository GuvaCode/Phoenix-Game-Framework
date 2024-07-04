object FrmElement: TFrmElement
  Left = 0
  Top = 0
  Width = 227
  Height = 854
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 219
    Height = 103
    Align = alTop
    Caption = 'Element properties'
    TabOrder = 0
    DesignSize = (
      219
      103)
    object Label5: TLabel
      Left = 8
      Top = 23
      Width = 20
      Height = 13
      Caption = '&Part'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 8
      Top = 49
      Width = 31
      Height = 13
      Caption = '&Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnPickForeground: TSpeedButton
      Left = 186
      Top = 73
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        08000000000000010000C40E0000C40E000000010000000100007D3C2A006B3B
        3B006D3B39006E3C3A006F3C3A007C4030007A40350074413B00634548007445
        4200764541007E4D48006C575A00A44F1A00B85B16008549370094553C00A858
        2600AD602200AB623400AE693700C5753500CC7A310083524900856160008C6C
        6A009074740094777700C17D4300B28D6300CF874500D0884B000A6B9A000C6C
        9C00116E9D000B719E000C709E0016729F0024719A000D7AA9000F7DA9001171
        A0002F7DA4006A758300FF00FF001A82AD000B87B3000A8AB1001A8EBB001A8F
        BC000A99BC001395BF00159BBC003681A6005790AE005E96AF006095AF006B9B
        B2006F9DB30074A7BB000A93C3000A9DC3001B95C1001999C000269CC4002A9A
        C200319CC3000AA3C8000BACD1000AB1D9003FA9C4002DB6D4000AB1E2000AB7
        E7005BBED7000AC0D9000ACCFC000CCDFC0016D1FC0024CAE30020D6F00024DD
        F70022DEFC001AE1FC0020EEFC0064C5DB004FCEEF005FC3E2005ADEFC0052EE
        FC0055ECFC0062E8FC0061EBFC00A38A890084A6B9008FACBA009AB1BE009EB4
        BE00A2B6BF00BDB8B800C0BABA00C2BEBE00AAB9C200B7C0C400BBC2C400BFC3
        C70087EEFC0089ECF9008BEAFC0087F0FC008BF0FC009EF0FC00A1ECFB00ACF2
        FC00B1F2FC00BCF3FC00C3C5C700C5C5C500CBF4FC00CEF6FC00D5F6FC00DDF7
        FC00000000000000000000000000000000000000000000000000000000000000
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
        00000000000000000000000000000000000000000000000000002C1A04010363
        2C2C2C2C2C2C2C2C2C2C2C03120E00182C2C2C2C2C2C2C2C2C2C2C170F160D02
        652C2C2C2C2C2C2C2C2C2C7507141505192C2C2C2C2C2C2C2C2C2C2C5D0A1E11
        04752C742C2C2C2C2C2C2C2C2C0B101F061B61252C2C2C2C2C2C2C2C2C64091C
        1308213D3B2C2C2C2C2C2C5E395E2B0C1D264A513F2C2C2C2C2C2C68222F3234
        46556B5B4538662C2C2C2C2C66244B54596D6F7370573E272A3A2C2C2C61234F
        5A6E717776584C4830602C2C2C6721475C6A72784E4931602C2C2C2C2C352850
        53526C794D402C2C2C2C2C2C62204344332D2E564C412C2C2C2C2C2C692C2C2C
        2C2C5F293C422C2C2C2C2C2C2C2C2C2C2C2C2C2C36372C2C2C2C}
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      OnClick = btnPickForegroundClick
    end
    object Label11: TLabel
      Left = 8
      Top = 77
      Width = 74
      Height = 13
      Caption = 'Color:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edName: TJvComboEdit
      Left = 64
      Top = 47
      Width = 143
      Height = 21
      Hint = 'Name of the skin element'
      Anchors = [akLeft, akTop, akRight]
      DisabledColor = clBtnFace
      TabOrder = 1
      OnButtonClick = edNameButtonClick
      OnChange = edNameChange
    end
    object edPart: TEdit
      Left = 64
      Top = 20
      Width = 143
      Height = 21
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 64
      Top = 74
      Width = 121
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Color = clWhite
      Padding.Left = 1
      Padding.Top = 1
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      TabOrder = 2
      object edColor: TPanel
        Left = 3
        Top = 3
        Width = 114
        Height = 14
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        ParentBackground = False
        TabOrder = 0
        OnClick = edColorClick
        ExplicitLeft = 24
        ExplicitTop = 0
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 107
    Width = 219
    Height = 130
    Align = alTop
    Caption = 'Bounds'
    TabOrder = 1
    ExplicitTop = 81
    DesignSize = (
      219
      130)
    object Label10: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = 'Bottom:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Top = 23
      Width = 23
      Height = 13
      Caption = 'Left:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 50
      Width = 22
      Height = 13
      Caption = 'Top:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 8
      Top = 77
      Width = 29
      Height = 13
      Caption = 'Right:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edBoundsLeft: TJvSpinEdit
      Left = 64
      Top = 20
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edBoundsLeftChange
    end
    object edBoundsTop: TJvSpinEdit
      Left = 64
      Top = 47
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edBoundsTopChange
    end
    object edBoundsRight: TJvSpinEdit
      Left = 64
      Top = 74
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edBoundsRightChange
    end
    object edBoundsBottom: TJvSpinEdit
      Left = 64
      Top = 101
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edBoundsBottomChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 237
    Width = 219
    Height = 126
    Align = alTop
    Caption = 'Fixed (resize) margins'
    TabOrder = 2
    ExplicitTop = 211
    DesignSize = (
      219
      126)
    object Label4: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = 'Bottom:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 77
      Width = 29
      Height = 13
      Caption = 'Right:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 23
      Width = 23
      Height = 13
      Caption = 'Left:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 50
      Width = 22
      Height = 13
      Caption = 'Top:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edMarginLeft: TJvSpinEdit
      Left = 64
      Top = 20
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edMarginLeftChange
    end
    object edMarginTop: TJvSpinEdit
      Left = 64
      Top = 47
      Width = 144
      Height = 21
      Value = 2.000000000000000000
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edMarginTopChange
    end
    object edMarginRight: TJvSpinEdit
      Left = 64
      Top = 74
      Width = 143
      Height = 21
      Value = 3.000000000000000000
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edMarginRightChange
    end
    object edMarginBottom: TJvSpinEdit
      Left = 64
      Top = 101
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edMarginBottomChange
    end
  end
  object GroupBox4: TGroupBox
    Left = 4
    Top = 363
    Width = 219
    Height = 132
    Align = alTop
    Caption = 'Outer (shadow) margins'
    TabOrder = 3
    ExplicitTop = 337
    DesignSize = (
      219
      132)
    object Label6: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = 'Bottom:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 8
      Top = 77
      Width = 29
      Height = 13
      Caption = 'Right:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label19: TLabel
      Left = 8
      Top = 23
      Width = 23
      Height = 13
      Caption = 'Left:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label20: TLabel
      Left = 8
      Top = 50
      Width = 22
      Height = 13
      Caption = 'Top:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edShadowLeft: TJvSpinEdit
      Left = 64
      Top = 20
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edShadowLeftChange
    end
    object edShadowTop: TJvSpinEdit
      Left = 64
      Top = 47
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edShadowTopChange
    end
    object edShadowRight: TJvSpinEdit
      Left = 64
      Top = 74
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edShadowRightChange
    end
    object edShadowBottom: TJvSpinEdit
      Left = 64
      Top = 101
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edShadowBottomChange
    end
  end
  object GroupBox6: TGroupBox
    Left = 4
    Top = 495
    Width = 219
    Height = 132
    Align = alTop
    Caption = 'Text margins'
    TabOrder = 4
    ExplicitTop = 469
    DesignSize = (
      219
      132)
    object Label17: TLabel
      Left = 8
      Top = 104
      Width = 38
      Height = 13
      Caption = 'Bottom:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label21: TLabel
      Left = 8
      Top = 77
      Width = 29
      Height = 13
      Caption = 'Right:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label22: TLabel
      Left = 8
      Top = 23
      Width = 23
      Height = 13
      Caption = 'Left:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label23: TLabel
      Left = 8
      Top = 50
      Width = 22
      Height = 13
      Caption = 'Top:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edContentMarginLeft: TJvSpinEdit
      Left = 64
      Top = 20
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edContentMarginTop: TJvSpinEdit
      Left = 64
      Top = 47
      Width = 144
      Height = 21
      Value = 8.000000000000000000
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edContentMarginRight: TJvSpinEdit
      Left = 64
      Top = 74
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edContentMarginBottom: TJvSpinEdit
      Left = 64
      Top = 101
      Width = 144
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object GroupBox5: TGroupBox
    Left = 4
    Top = 627
    Width = 219
    Height = 56
    Align = alTop
    Caption = 'Text'
    TabOrder = 5
    ExplicitTop = 601
    DesignSize = (
      219
      56)
    object Label16: TLabel
      Left = 8
      Top = 21
      Width = 29
      Height = 13
      Caption = 'Color:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnPickBackground: TSpeedButton
      Left = 185
      Top = 18
      Width = 23
      Height = 22
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        08000000000000010000C40E0000C40E000000010000000100007D3C2A006B3B
        3B006D3B39006E3C3A006F3C3A007C4030007A40350074413B00634548007445
        4200764541007E4D48006C575A00A44F1A00B85B16008549370094553C00A858
        2600AD602200AB623400AE693700C5753500CC7A310083524900856160008C6C
        6A009074740094777700C17D4300B28D6300CF874500D0884B000A6B9A000C6C
        9C00116E9D000B719E000C709E0016729F0024719A000D7AA9000F7DA9001171
        A0002F7DA4006A758300FF00FF001A82AD000B87B3000A8AB1001A8EBB001A8F
        BC000A99BC001395BF00159BBC003681A6005790AE005E96AF006095AF006B9B
        B2006F9DB30074A7BB000A93C3000A9DC3001B95C1001999C000269CC4002A9A
        C200319CC3000AA3C8000BACD1000AB1D9003FA9C4002DB6D4000AB1E2000AB7
        E7005BBED7000AC0D9000ACCFC000CCDFC0016D1FC0024CAE30020D6F00024DD
        F70022DEFC001AE1FC0020EEFC0064C5DB004FCEEF005FC3E2005ADEFC0052EE
        FC0055ECFC0062E8FC0061EBFC00A38A890084A6B9008FACBA009AB1BE009EB4
        BE00A2B6BF00BDB8B800C0BABA00C2BEBE00AAB9C200B7C0C400BBC2C400BFC3
        C70087EEFC0089ECF9008BEAFC0087F0FC008BF0FC009EF0FC00A1ECFB00ACF2
        FC00B1F2FC00BCF3FC00C3C5C700C5C5C500CBF4FC00CEF6FC00D5F6FC00DDF7
        FC00000000000000000000000000000000000000000000000000000000000000
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
        00000000000000000000000000000000000000000000000000002C1A04010363
        2C2C2C2C2C2C2C2C2C2C2C03120E00182C2C2C2C2C2C2C2C2C2C2C170F160D02
        652C2C2C2C2C2C2C2C2C2C7507141505192C2C2C2C2C2C2C2C2C2C2C5D0A1E11
        04752C742C2C2C2C2C2C2C2C2C0B101F061B61252C2C2C2C2C2C2C2C2C64091C
        1308213D3B2C2C2C2C2C2C5E395E2B0C1D264A513F2C2C2C2C2C2C68222F3234
        46556B5B4538662C2C2C2C2C66244B54596D6F7370573E272A3A2C2C2C61234F
        5A6E717776584C4830602C2C2C6721475C6A72784E4931602C2C2C2C2C352850
        53526C794D402C2C2C2C2C2C62204344332D2E564C412C2C2C2C2C2C692C2C2C
        2C2C5F293C422C2C2C2C2C2C2C2C2C2C2C2C2C2C36372C2C2C2C}
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      OnClick = btnPickBackgroundClick
      ExplicitLeft = 174
    end
    object edContentColor: TColorBox
      Left = 64
      Top = 18
      Width = 115
      Height = 22
      Hint = 'Background (skin) color'
      Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 4
    Top = 700
  end
  object ColorDialog1: TColorDialog
    Left = 152
    Top = 680
  end
end
