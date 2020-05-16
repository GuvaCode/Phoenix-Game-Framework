object FrmPalette: TFrmPalette
  Left = 0
  Top = 0
  Width = 218
  Height = 413
  TabOrder = 0
  object CategoryButtons1: TCategoryButtons
    Left = 0
    Top = 0
    Width = 218
    Height = 413
    Align = alClient
    ButtonFlow = cbfVertical
    ButtonWidth = 120
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions, boBoldCaptions, boUsePlusMinus, boCaptionOnlyBorder]
    Categories = <
      item
        Caption = 'Report Elements'
        Color = 16771818
        Collapsed = False
        Items = <
          item
            Caption = 'Label'
            Hint = 'Label element'
            ImageIndex = 0
          end
          item
            Caption = 'Image'
            Hint = 'Image element'
            ImageIndex = 1
          end
          item
            Caption = 'Rectangle'
            Hint = 'Rectangle element'
            ImageIndex = 2
          end
          item
            Caption = 'Round Rectangle'
            Hint = 'Rectangle element with rounded corners'
            ImageIndex = 3
          end
          item
            Caption = 'Elipse'
            Hint = 'Elipse element'
            ImageIndex = 4
          end
          item
            Caption = 'Line'
            Hint = 'Line element'
            ImageIndex = 5
          end>
      end
      item
        Caption = 'Templates'
        Color = 16771839
        Collapsed = False
        Items = <
          item
            Caption = 'Page X of Y'
            Hint = 'Creates a label showing the current page'
            ImageIndex = 6
          end
          item
            Caption = 'Print Date'
            ImageIndex = 6
          end
          item
            Caption = 'Print Time'
            ImageIndex = 6
          end
          item
            Caption = 'PageNumber'
          end
          item
            Caption = 'PageCount'
          end>
      end>
    RegularButtonColor = 15660791
    SelectedButtonColor = 13361893
    ShowHint = True
    TabOrder = 0
    OnMouseMove = CategoryButtons1MouseMove
  end
end
