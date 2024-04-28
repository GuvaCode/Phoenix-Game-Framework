object FrmGeneratorRange: TFrmGeneratorRange
  Left = 0
  Height = 430
  Top = 0
  Width = 676
  Caption = 'Select character range'
  ClientHeight = 430
  ClientWidth = 676
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnResize = FormResize
  Position = poMainFormCenter
  LCLVersion = '3.2.0.0'
  object PaintBox1: TPaintBox
    Left = 8
    Height = 366
    Top = 8
    Width = 443
    Anchors = [akTop, akLeft, akRight, akBottom]
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnPaint = PaintBox1Paint
  end
  object ScrollBar1: TScrollBar
    Left = 461
    Height = 366
    Top = 8
    Width = 13
    Anchors = [akTop, akRight, akBottom]
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = ScrollBar1Change
  end
  object lwCodebase: TListBox
    Left = 480
    Height = 366
    Top = 8
    Width = 188
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 0
    TabOrder = 1
    TopIndex = -1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Height = 18
    Top = 412
    Width = 676
    Panels = <    
      item
        Width = 200
      end    
      item
        Width = 150
      end    
      item
        Width = 150
      end>
  end
  object btnImport: TButton
    Left = 89
    Height = 25
    Top = 380
    Width = 75
    Anchors = [akLeft, akBottom]
    Caption = 'Import'
    TabOrder = 3
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 593
    Height = 25
    Top = 380
    Width = 75
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnOk: TButton
    Left = 512
    Height = 25
    Top = 380
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnClear: TButton
    Left = 8
    Height = 25
    Top = 380
    Width = 75
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 6
    OnClick = btnClearClick
  end
  object OpenTextFileDialog1: TOpenDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 24
    Top = 40
  end
end
