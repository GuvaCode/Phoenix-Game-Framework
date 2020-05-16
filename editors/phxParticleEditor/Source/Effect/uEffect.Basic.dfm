object FrmEffectBasic: TFrmEffectBasic
  Left = 0
  Top = 0
  Width = 300
  Height = 690
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object edLink: TCheckBox
    Left = 7
    Top = 398
    Width = 191
    Height = 17
    Caption = 'Link particles to system'
    TabOrder = 0
  end
  object edIdleWhileParentActive: TCheckBox
    Left = 7
    Top = 375
    Width = 201
    Height = 17
    Caption = 'Idle while parent is active'
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 4
    Width = 292
    Height = 165
    Align = alTop
    Caption = 'Effect properties'
    TabOrder = 2
    ExplicitLeft = 7
    ExplicitTop = 7
    DesignSize = (
      292
      165)
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 37
      Height = 13
      Caption = '&Author:'
    end
    object Label3: TLabel
      Left = 8
      Top = 75
      Width = 39
      Height = 13
      Caption = '&Version:'
    end
    object Label8: TLabel
      Left = 8
      Top = 102
      Width = 49
      Height = 13
      Caption = '&Comment:'
    end
    object edName: TEdit
      Left = 75
      Top = 18
      Width = 210
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edNameChange
      ExplicitWidth = 310
    end
    object edAuthor: TEdit
      Left = 75
      Top = 45
      Width = 210
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edAuthorChange
      ExplicitWidth = 310
    end
    object edComment: TMemo
      Left = 75
      Top = 99
      Width = 210
      Height = 60
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
      OnChange = edCommentChange
    end
    object edVersion: TEdit
      Left = 75
      Top = 72
      Width = 210
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edVersionChange
      ExplicitWidth = 310
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 169
    Width = 292
    Height = 128
    Align = alTop
    Caption = 'Basic'
    TabOrder = 3
    ExplicitTop = 201
    ExplicitWidth = 392
    DesignSize = (
      292
      128)
    object Label4: TLabel
      Left = 8
      Top = 21
      Width = 31
      Height = 13
      Caption = '&Delay:'
    end
    object Label5: TLabel
      Left = 8
      Top = 48
      Width = 30
      Height = 13
      Caption = '&Quota'
    end
    object Label6: TLabel
      Left = 8
      Top = 75
      Width = 17
      Height = 13
      Caption = '&Life'
    end
    object Label7: TLabel
      Left = 8
      Top = 102
      Width = 61
      Height = 13
      Caption = '&Life variance'
    end
    object edDelay: TJvSpinEdit
      Left = 75
      Top = 18
      Width = 210
      Height = 21
      Hint = 'Inital delay before the first particle is spawned'
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = edDelayChange
      ExplicitWidth = 310
    end
    object edQuota: TJvSpinEdit
      Left = 75
      Top = 45
      Width = 210
      Height = 21
      Hint = 'Max number of alive particles'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = edQuotaChange
      ExplicitWidth = 310
    end
    object edLifeValue: TJvSpinEdit
      Left = 75
      Top = 72
      Width = 210
      Height = 21
      Hint = 'Life for each particle in seconds'
      Increment = 0.100000000000000000
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = edLifeValueChange
      ExplicitWidth = 310
    end
    object edLifeVariance: TJvSpinEdit
      Left = 75
      Top = 99
      Width = 210
      Height = 21
      Hint = 'Variance to add to the life for each particle'
      Increment = 0.100000000000000000
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnChange = edLifeVarianceChange
      ExplicitWidth = 310
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 297
    Width = 292
    Height = 72
    Align = alTop
    Caption = 'Initial updating'
    TabOrder = 4
    ExplicitTop = 329
    ExplicitWidth = 242
    DesignSize = (
      292
      72)
    object Label9: TLabel
      Left = 8
      Top = 21
      Width = 42
      Height = 13
      Caption = '&Interval:'
    end
    object Label10: TLabel
      Left = 8
      Top = 48
      Width = 33
      Height = 13
      Caption = '&Count:'
    end
    object edInitalUpdateInterval: TJvSpinEdit
      Left = 79
      Top = 18
      Width = 206
      Height = 21
      Hint = 'Interval for the initial updating'
      ValueType = vtFloat
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edInitalUpdateIntervalChange
    end
    object edInitalUpdateCount: TJvSpinEdit
      Left = 79
      Top = 45
      Width = 206
      Height = 21
      Hint = 'Number of repeats to update the particles'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = edInitalUpdateCountChange
    end
  end
end
