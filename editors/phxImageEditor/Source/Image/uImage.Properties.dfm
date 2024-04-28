object FrmImageProperties: TFrmImageProperties
  Left = 0
  Height = 412
  Top = 0
  Width = 244
  BorderSpacing.Around = 4
  ClientHeight = 412
  ClientWidth = 244
  TabOrder = 0
  DesignLeft = 371
  DesignTop = 180
  object GroupBox1: TGroupBox
    Left = 0
    Height = 180
    Top = 232
    Width = 244
    Align = alClient
    Caption = 'Texture'
    ClientHeight = 163
    ClientWidth = 242
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Height = 16
      Top = 8
      Width = 39
      Caption = '&Width:'
      Font.Color = clWindowText
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Height = 16
      Top = 41
      Width = 43
      Caption = '&Height:'
      Font.Color = clWindowText
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Height = 16
      Top = 72
      Width = 46
      Caption = '&Format:'
      Font.Color = clWindowText
      ParentFont = False
    end
    object edTextureWidth: TEdit
      Left = 72
      Height = 28
      Top = 8
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object edTextureHeight: TEdit
      Left = 72
      Height = 28
      Top = 40
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edTextureFormat: TEdit
      Left = 72
      Height = 28
      Top = 72
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object btnTexture: TButton
      Left = 132
      Height = 25
      Top = 112
      Width = 105
      Anchors = [akTop, akRight]
      Caption = 'Texture settings'
      TabOrder = 3
      Visible = False
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Height = 232
    Top = 0
    Width = 244
    Align = alTop
    Caption = 'Image properties'
    ClientHeight = 215
    ClientWidth = 242
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Height = 16
      Top = 8
      Width = 38
      Caption = '&Name:'
    end
    object Label2: TLabel
      Left = 8
      Height = 16
      Top = 40
      Width = 46
      Caption = '&Author:'
    end
    object Label3: TLabel
      Left = 8
      Height = 16
      Top = 72
      Width = 48
      Caption = '&Version:'
    end
    object Label8: TLabel
      Left = 8
      Height = 16
      Top = 102
      Width = 61
      Caption = '&Comment:'
    end
    object edName: TEdit
      Left = 72
      Height = 28
      Top = 8
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 0
      OnChange = edNameChange
    end
    object edAuthor: TEdit
      Left = 72
      Height = 28
      Top = 40
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 1
      OnChange = edAuthorChange
    end
    object edComment: TMemo
      Left = 72
      Height = 89
      Top = 104
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 2
      OnChange = edCommentChange
    end
    object edVersion: TEdit
      Left = 72
      Height = 28
      Top = 72
      Width = 165
      Anchors = [akTop, akLeft, akRight]
      TabOrder = 3
      OnChange = edVersionChange
    end
  end
end
