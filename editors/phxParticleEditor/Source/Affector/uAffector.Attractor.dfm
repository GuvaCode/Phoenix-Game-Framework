object FrmAffectorAttractor: TFrmAffectorAttractor
  Left = 0
  Top = 0
  Width = 320
  Height = 187
  TabOrder = 0
  DesignSize = (
    320
    187)
  object LabelForce: TLabel
    Left = 8
    Top = 25
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Force:'
  end
  object Label1: TLabel
    Left = 8
    Top = 99
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&X:'
  end
  object Label2: TLabel
    Left = 8
    Top = 126
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Y:'
  end
  object Label3: TLabel
    Left = 8
    Top = 153
    Width = 50
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Z:'
  end
  object Label4: TLabel
    Left = 8
    Top = 72
    Width = 317
    Height = 18
    AutoSize = False
    Caption = 'Position'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 3
    Width = 302
    Height = 13
    AutoSize = False
    Caption = 'Basic'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EditForce: TEdit
    Left = 64
    Top = 22
    Width = 241
    Height = 21
    Hint = 'The force to attract the particles with'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = EditForceChange
  end
  object EditPositionX: TEdit
    Left = 64
    Top = 96
    Width = 241
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = EditPositionXChange
  end
  object EditPositionY: TEdit
    Left = 64
    Top = 123
    Width = 241
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditPositionYChange
  end
  object EditPositionZ: TEdit
    Left = 64
    Top = 150
    Width = 241
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = EditPositionZChange
  end
  object EditRelative: TCheckBox
    Left = 64
    Top = 49
    Width = 241
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Relative to system'
    TabOrder = 4
    OnClick = EditRelativeClick
  end
end
