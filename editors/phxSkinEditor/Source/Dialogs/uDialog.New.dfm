object NewDialog: TNewDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'New skin'
  ClientHeight = 194
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    366
    194)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 283
    Top = 161
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 202
    Top = 161
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 350
    Height = 147
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    Items.ItemData = {
      058A000000030000000000000000000000FFFFFFFF00000000FFFFFFFF000000
      000445006D00740079000100000001000000FFFFFFFF00000000FFFFFFFF0000
      00000C49006D0070006F0072007400200049006D006100670065000200000002
      000000FFFFFFFF00000000FFFFFFFF000000000E540065007800740075007200
      650020007000610063006B0065007200}
    LargeImages = ToolBarImages
    TabOrder = 2
    OnDblClick = ListView1DblClick
    OnSelectItem = ListView1SelectItem
    ExplicitWidth = 475
    ExplicitHeight = 241
  end
  object ToolBarImages: TImageList
    Height = 24
    Width = 24
    Left = 80
    Top = 112
    Bitmap = {
      494C010103000E003C0118001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD000894BD000894BD000894BD000894
      BD000894BD002984AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B55A
      1000AD5A1000AD521000AD521000AD521000AD4A1000AD4A0800A54A0800A54A
      0800A54A0800A5420800A5420800A54208009C4208009C4208009C3908009C39
      08009C3908000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000008CBD0008C6F70000CEFF0000C6
      FF0000C6F70000BDF70000BDEF0000B5EF0008B5E70008B5E70008ADDE0008AD
      DE0008ADDE001084B50000000000000000003BB3520033A848002FA34200299A
      390023923100000000003BB3520033A848002FA34200299A3900239231000000
      00003BB3520033A848002FA34200299A39002392310000000000C26F1500CF90
      5300C16D1000C16C0E00C06A0A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B55A
      1000FFF7EF00FFF7EF00FFF7EF00FFF7E700FFEFE700FFEFE700FFEFDE00FFEF
      DE00FFEFD600FFEFD600FFE7D600FFE7CE00FFE7CE00FFE7C600FFE7C600FFDE
      C6009C3908000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000008CBD0021B5E70000CEFF0000CE
      FF0000C6FF0000C6F70000C6F70000BDEF0008BDEF0008B5E70008B5E70008B5
      E70008ADDE000894C60094B5BD00000000007FCC87006BBE740088BB80006DD8
      820069D37C00000000007FCC87006BBE740088BB80006DD8820069D37C000000
      00007FCC87006BBE740088BB80006DD8820069D37C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B55A
      1000FFF7F700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFF7E700FFEFE700FFEF
      DE00FFEFDE00FFEFD600FFE7D600FFE7D600FFE7CE00FFE7CE00FFE7C600FFE7
      C6009C390800000000000000000000000000105AAD0008317B00083173000831
      73000831730008316B0008296B0008296300008CBD0029A5CE0000CEFF0000CE
      FF0000CEFF0000CEFF0000C6F70000C6F70008BDEF0008BDEF0008BDEF0008B5
      E70008B5E70008ADDE004A94B50000000000DFA58D00E5B5B400D79B7400BB8E
      5500C3862A0000000000DFA58D00E5B5B400D79B7400BB8E5500C3862A000000
      0000DFA58D00E5B5B400D79B7400BB8E5500C3862A00000000003BB3520033A8
      48002FA34200299A390023923100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B563
      1000FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFEFE700FFEF
      E700FFEFDE00FFEFDE00FFEFD600FFE7D600FFE7D600FFE7CE00FFE7CE00FFE7
      C6009C420800000000000000000000000000318CCE00186BBD0052A5DE0052A5
      D600529CD6004A9CD6004A9CD6004A9CCE00008CBD00219CC60010CEFF0000CE
      FF0000CEFF0000CEFF0000CEFF0000C6FF0008C6F70008BDF70008BDEF0008BD
      EF0008B5E70008B5E700298CB50000000000C26F1500CF905300C16D1000C16C
      0E00C06A0A0000000000C26F1500CF905300C16D1000C16C0E00C06A0A000000
      0000C26F1500CF905300C16D1000C16C0E00C06A0A00000000007FCC87006BBE
      740088BB80006DD8820069D37C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B563
      1000FFFFF700FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFEF
      E700FFEFE700FFEFDE00FFEFDE00FFEFD600FFE7D600FFE7D600FFE7CE00FFE7
      CE00A5420800000000000000000000000000318CD600104A9400188C2100188C
      2100108C2100108418001084180010841800008CBD000884B50073F7FF0073F7
      FF0073F7FF0073F7FF0073F7FF0073F7FF0008C6F70008C6F70008C6F70008BD
      EF0008BDEF0008BDEF00109CCE00A5BDC6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DFA58D00E5B5
      B400D79B7400BB8E5500C3862A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD63
      1000FFFFF700FFFFF700FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7EF00FFF7
      E700FFF7E700FFEFE700FFEFDE00FFEFDE00FFEFD600FFE7D600FFE7D600FFE7
      CE00A5420800000000000000000000000000318CD600104A9400219C3900219C
      3100219C3100219C31002194310018942900008CBD001094BD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD007BF7FF0008CEFF0008C6F70008C6
      F70008BDF70008BDEF0008B5E700529CB50031A544002D9F3E00299A39002694
      3300228E2D001E8928001B842300177E1D000000000031A544002D9F3E00299A
      390026943300228E2D001E8928001B842300177E1D0000000000C26F1500CF90
      5300C16D1000C16C0E00C06A0A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6B
      1000FFFFF700FFFFF700FFFFF700FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7
      EF00FFF7E700FFF7E700FFEFE700FFEFDE00FFEFDE00FFEFD600FFE7D600FFE7
      D600A5420800000000000000000000000000398CD600104A9C0031B54A0031B5
      4A0031AD4A0029AD420029AD420029AD4200008CBD006BF7FF0052CEE70052CE
      E70052CEE70052CEE70052CEE70052CEE7000894BD007BF7FF007BF7FF007BF7
      FF007BF7FF007BF7FF0008BDEF003194BD0059E180004ACA690044C262004BCC
      6C0050D473004DCE6D0049C9690045C463000000000059E180004ACA690044C2
      62004BCC6C0050D473004DCE6D0049C9690045C4630000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6B
      1000FFFFFF00FFFFF700FFFFF700FFFFF700FFF7F700FFF7F700FFF7EF00FFF7
      EF00FFF7EF00FFF7E700FFEFE700FFEFE700FFEFDE00FFEFDE00FFEFD600FFE7
      D600A54A0800000000000000000000000000398CD600104A9C0042C65A0039C6
      5A0039C65A0039BD520031B54A0029AD4200008CBD00BDFFFF006BF7FF006BF7
      FF006BF7FF006BF7FF006BF7FF0063E7F7005ACEE7000894BD000894BD000894
      BD000894BD000894BD000894BD003994BD009DBE8D007DCA85007EBB7C009AA6
      7C00A0B4870076D5830070DE870077CB7A00000000009DBE8D007DCA85007EBB
      7C009AA67C00A0B4870076D5830070DE870077CB7A00000000003BB3520033A8
      48002FA34200299A390023923100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6B
      1000FFFFFF00FFFFFF00FFFFF700FFFFF700FFFFF700FFF7F700FFF7EF00FFF7
      EF00FFF7EF00FFF7E700FFF7E700FFF7E700FFEFDE00FFEFDE00FFEFDE00FFEF
      D600A54A0800000000000000000000000000398CDE00104A9C004ADE73004AD6
      6B0042CE630039C65A0039BD520031B54A00008CBD00BDFFFF006BEFFF006BF7
      FF0073F7FF0073F7FF007BF7FF006BF7FF006BE7F7005ACEE7005ACEE7005ACE
      E7005ACEE7000894BD00BDC6C600C6C6C600DFA49700E1ABAA00E1ABAA00E1A9
      A500C98F6800B0895A00AF8E5100C3872D0000000000DFA49700E1ABAA00E1AB
      AA00E1A9A500C98F6800B0895A00AF8E5100C3872D00000000007FCC87006BBE
      740088BB80006DD8820069D37C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD73
      1000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7F700FFF7
      EF00FFF7EF00FFF7EF00FFF7E700FFF7E700FFEFE700FFEFDE00FFEFDE00FFEF
      DE00AD4A08000000000000000000000000003994DE00104AA50063E784005AEF
      840052E77B004ADE73004AD66B0042CE6300008CBD00BDFFFF007BF7FF007BF7
      FF0084FFFF0084FFFF008CFFFF008CFFFF0094FFFF009CFFFF00633131006331
      310063313100633131006331310063313100D1862B00E3B69700ECCCC300DA9E
      6200CC7D1400CE8C3800CD801B00CC7A0A0000000000D1862B00E3B69700ECCC
      C300DA9E6200CC7D1400CE8C3800CD801B00CC7A0A0000000000DFA58D00E5B5
      B400D79B7400BB8E5500C3862A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C673
      1000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7F700FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFF7E700FFEFE700FFEFDE00FFEF
      DE00AD5210000000000000000000000000003994DE001052A500D6847B00ADAD
      8C0084D68C0073DE8C0063E784006BD67B00008CBD00BDFFFF008CFFFF008CFF
      FF0094FFFF0094FFFF0094FFFF00008CBD00008CBD00008CBD00008CBD006331
      3100C6733100CE732100B563180063313100BC640A00BE6A1400C4782C00BC64
      0A00BC640A00BC640A00BC640A00BC640A0000000000BC640A00BE6A1400C478
      2C00BC640A00BC640A00BC640A00BC640A00BC640A0000000000C26F1500CF90
      5300C16D1000C16C0E00C06A0A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C673
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7
      F700FFF7F700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFF7E700FFEFE700FFE7
      CE00AD5210000000000000000000000000003994DE001052A500DE949400DE94
      9400DE949400DE949400DE949400DE949400DE949400008CBD00BDFFFF00BDFF
      FF00BDFFFF008463630031637B0094633100848C52007B9C5A007B9452006B42
      310094523900DE843100CE732100633131000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C673
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFF
      F700FFFFF700FFF7F700FFF7EF00FFF7EF00FFEFDE00FFE7D600FFDEBD00EFC6
      A500AD5210000000000000000000000000003994E7001052AD00E7A5A500E7A5
      A500E7A5A500E7A5A500E7A5A500E7A5A500DEA5A500E7A5A500008CBD00008C
      BD00008CBD004252630063313100734239007B4A39007B4239006B3931009452
      3900E79C4A0094523900C6733100633131002695340024913000228E2D001F8B
      2A001D8828001B85240019812000177E1D00157B1A0013781700117414000F72
      11000C6F0E000A6C0B00000000003BB3520033A848002FA34200299A39002392
      3100000000003BB3520033A84800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C67B
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      F700FFFFF700FFEFDE00FFE7CE00FFDEBD00EFC6A500EFC6A500EFC6A500EFC6
      A500AD5210000000000000000000000000003994E7001052AD00E7A56B00E7BD
      BD00E7BDBD00E7BDBD00E7BDBD00E7BDBD00E7BDBD00E7BDBD00E7BDBD00E7B5
      B500DE9C4A00C68C4A008452420063313100633131008C524A00BD7B5A00CE8C
      5A009C5A42007339290063313100633131003FBB5A003DB857003CB4530039B1
      500037AE4E0035AB4A0033A8470030A544002EA241002C9E3D002A9B3B002998
      37002695340024923100000000007FCC87006BBE740088BB80006DD8820069D3
      7C00000000007FCC87006BBE7400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE7B
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFF700BD6B1000BD631000BD631000B5631000B5631000B55A1000B55A
      1000AD5A10000000000000000000000000003994E7001052AD00D67B0000DE94
      3900EFC6B500EFCECE00EFCECE00EFCECE00EFCECE00EFCECE00EFCEC600DE9C
      4200D67B0000D67B0000D68C2900B58463007B4A420073423900733939007339
      310084422900C67300002173AD006331310059E1800057DE7D004CCE6D0045C2
      620049CA69004FD171004DCE6D004BCB6A0048C8670046C4630044C1610041BF
      5E003FBC5B003DB8570000000000DFA58D00E5B5B400D79B7400BB8E5500C386
      2A0000000000DFA58D00E5B5B400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE7B
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00BD6B1000FFFFFF00FFF7F700FFEFE700FFE7D600FFDEC600B55A
      100000000000000000000000000000000000399CE7001052B500CE730000CE73
      0000CE7B1000E7BD9400F7DEDE00F7DEDE00F7DEDE00F7D6CE00D68C3100CE73
      0000CE730000CE730000CE730000CE7B1000D68C2100CE730000CE730000CE73
      0000CE730000CE7300002173AD0000316B0068F8980056DC7B004BCB6A0045C2
      62003EBA590042BF5F0056DE7D0063F2910062EE8D0060EB8A005EE787005CE4
      840059E2810057DE7D0000000000C26F1500CF905300C16D1000C16C0E00C06A
      0A0000000000C26F1500CF905300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00BD6B1000FFF7F700FFEFE700FFE7D600FFDEC600B56310000000
      000000000000000000000000000000000000429CE7001052B500C66B0000C66B
      0000C66B0000C66B0000D69C5200F7EFE700EFD6B500CE7B1800C66B0000C66B
      0000C66B0000C66B0000C66B0000C66B0000C66B0000C66B0000C66B0000C66B
      0000C66B0000C66B00002173AD0008316B00A8AF870077DD8D005EE9880058E1
      800064CB78007DB07300A4957400B59A810084DC93006BFC9B006BFC9B006BFC
      9B006BFC9B0074E08B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00BD731000FFEFE700FFE7D600FFDEC600BD631000000000000000
      000000000000000000000000000000000000429CEF00105ABD00BD630000BD63
      0000BD630000BD630000BD630000C6731800BD630000BD630000BD630000BD63
      0000BD630000BD630000BD630000BD630000BD630000BD630000BD630000BD63
      0000BD630000BD6300002973B50008396B00DA959500DA959500DA959500DA95
      9500DA959500DA959500DA959500DA959500B1755E0092744600869F6200869F
      62008B8D5600A56C3B000000000031A544002D9F3E00299A390026943300228E
      2D001E8928001B842300177E1D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6731800FFE7D600FFDEC600BD6B100000000000000000000000
      000000000000000000000000000000000000429CEF00185ABD00B55A0000B55A
      0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A
      0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A0000B55A
      0000B55A0000B55A00002973B50008396B00E2ACA900E2ADAD00E2ADAD00E2AD
      AD00E2ADAD00E2ADAD00E2ADAD00D39F8900B4845800B4845800B4845800B585
      5700C78A3C00DA911F000000000059E180004ACA690044C262004BCC6C0050D4
      73004DCE6D0049C9690045C46300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6731800FFDEC600BD7310000000000000000000000000000000
      000000000000000000000000000000000000429CEF00186BCE000842AD000842
      A5000842A50008429C0008399C000839940008398C0008398C0008398C000839
      84000831840008317B0008317B00083173000831730008316B0008316B000829
      6B000829630000296300084A8C0008397300D78E2900E6BAA700EAC5C500EAC5
      C500EAC5C500EAC5C500E3B08900D4851000D1943F00D0A67600D1A36C00D28A
      2100D3830A00D3830A00000000009DBE8D007DCA85007EBB7C009AA67C00A0B4
      870076D5830070DE870077CB7A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6731800C6731800000000000000000000000000000000000000
      00000000000000000000000000000000000052ADF70063B5F70063B5EF0063B5
      EF0063B5EF005AB5EF005AADEF005AADE7005AADE7005AADE7005AADE70052A5
      DE0052A5DE0052A5DE0052A5DE0052A5D60052A5D600529CD6004A9CD6004A9C
      CE004A9CCE004A9CCE004A94CE001063AD00CA770A00CC7B1200E2B48700F3DF
      DF00F3DFDF00E4B88F00CB7A0F00CA770A00CA770A00D0862500CD7F1800CA77
      0A00CA770A00CA770A0000000000DFA49700E1ABAA00E1ABAA00E1A9A500C98F
      6800B0895A00AF8E5100C3872D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE84
      1800CE841800CE841800CE841800CE841800CE841800CE841800CE7B1800CE7B
      1800C67B1800C67B180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C16B0A00C16B0A00C16B0A00D398
      5500D7A16600C16B0A00C16B0A00C16B0A00C16B0A00C16B0A00C16B0A00C16B
      0A00C16B0A00C16B0A0000000000D1862B00E3B69700ECCCC300DA9E6200CC7D
      1400CE8C3800CD801B00CC7A0A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B85F0A00B85F0A00B85F0A00B85F
      0A00B85F0A00B85F0A00B85F0A00B85F0A00B85F0A00B85F0A00B85F0A00B85F
      0A00B85F0A00B85F0A0000000000BC640A00BE6A1400C4782C00BC640A00BC64
      0A00BC640A00BC640A00BC640A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF0003FFFFFF000000E00007FF
      0003041041000000E00007FF000104107F000000E00007000001041041000000
      E00007000001041041000000E00007000000FFFFC1000000E000070000000080
      41000000E0000700000000807F000000E00007000000008041000000E0000700
      0000008041000000E00007000000008041000000E00007000000008041000000
      E00007000000FFFFFF000000E00007000000000209000000E000070000000002
      09000000E00007000000000209000000E0000F000000000209000000E0001F00
      00000003FF000000E0003F000000000201000000E0007F000000000201000000
      E000FF000000000201000000E001FF000000000201000000E003FFFFFFFF0002
      01000000FFFFFFFFFFFF00020100000000000000000000000000000000000000
      000000000000}
  end
end
