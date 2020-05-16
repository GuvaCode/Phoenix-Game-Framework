object FrmImageList: TFrmImageList
  Left = 0
  Top = 0
  Width = 294
  Height = 418
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object ListBox1: TListBox
    Left = 4
    Top = 4
    Width = 286
    Height = 379
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    ExplicitLeft = 88
    ExplicitTop = 160
    ExplicitWidth = 121
    ExplicitHeight = 97
  end
  object Panel1: TPanel
    Left = 4
    Top = 383
    Width = 286
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 376
    object Button1: TButton
      Left = 0
      Top = 6
      Width = 286
      Height = 25
      Action = ModActions.actLoadImage
      Align = alBottom
      ImageAlignment = iaCenter
      ImageMargins.Left = -100
      Images = ModActions.ActionImages
      TabOrder = 0
      ExplicitWidth = 289
    end
  end
end
