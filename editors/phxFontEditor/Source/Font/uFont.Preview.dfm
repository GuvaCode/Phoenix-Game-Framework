object FrmFontPreview: TFrmFontPreview
  Left = 0
  Top = 0
  Width = 516
  Height = 483
  Padding.Left = 4
  Padding.Top = 4
  Padding.Right = 4
  Padding.Bottom = 4
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 508
    Height = 145
    Align = alTop
    Caption = 'Preview'
    TabOrder = 0
    object edText: TMemo
      Left = 2
      Top = 15
      Width = 504
      Height = 128
      Align = alClient
      Lines.Strings = (
        'The quick brown fox jumps'
        'over the lazy dog.'
        ''
        
          'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque' +
          ' volutpat placerat luctus. Vestibulum '
        
          'laoreet est eget risus dapibus porttitor. In eleifend aliquet ia' +
          'culis. Vestibulum nec auctor mauris. Cras '
        
          'rhoncus, arcu quis luctus volutpat, enim ligula pulvinar dui, in' +
          ' facilisis est tortor ac odio. Phasellus '
        
          'feugiat porta lacus, non cursus leo semper sed. Nulla quam odio,' +
          ' tempus vitae tristique vel, pulvinar '
        
          'tempor ligula. Praesent interdum posuere venenatis. Donec congue' +
          ' tortor sed felis egestas congue. '
        
          'Morbi pharetra euismod metus, eu tempor felis commodo vitae. Dui' +
          's eu lacus sapien, quis ullamcorper '
        
          'eros. Nunc in tempor magna. Nullam varius nulla sed sem interdum' +
          ' ultricies. Ut condimentum arcu vel '
        
          'massa consectetur at egestas felis cursus. Vestibulum urna purus' +
          ', fringilla imperdiet pellentesque vel, '
        
          'tempor sed dui. Donec convallis, libero ut condimentum tincidunt' +
          ', justo lacus mattis sapien, a fringilla '
        
          'odio leo luctus nunc. Praesent imperdiet bibendum nunc, non aliq' +
          'uam urna fermentum eu. Suspendisse '
        
          'vestibulum felis sit amet nibh porta vel convallis sapien rutrum' +
          '.'
        ''
        
          'Aenean euismod aliquam tortor, sed viverra dolor eleifend sollic' +
          'itudin. Suspendisse enim risus, '
        
          'tincidunt sit amet congue ut, dictum sed diam. Sed molestie adip' +
          'iscing nunc interdum sagittis. Aliquam '
        
          'vel urna vitae tellus mattis dignissim. In lacus libero, ornare ' +
          'eget mollis id, dapibus in metus. Aliquam '
        
          'vel molestie ante. Nunc pellentesque magna sed sem porttitor bib' +
          'endum. Aenean pretium lacinia elit '
        
          'vitae tincidunt. Quisque nec libero lectus, sed commodo tellus. ' +
          'Nulla facilisi. Praesent fermentum est '
        
          'vel mi ultricies consequat. Suspendisse sagittis, tellus quis al' +
          'iquam porttitor, purus diam hendrerit '
        
          'nulla, eget varius nisi tellus vitae arcu. Class aptent taciti s' +
          'ociosqu ad litora torquent per conubia '
        
          'nostra, per inceptos himenaeos. Vestibulum eros sem, feugiat bla' +
          'ndit lacinia vel, pulvinar eu neque. '
        
          'Vivamus sagittis tellus non justo pulvinar eget sagittis elit su' +
          'scipit. Donec nec lacus ac justo fermentum '
        
          'commodo. Etiam dolor dui, pharetra non ullamcorper molestie, ali' +
          'quam aliquam diam.'
        ''
        
          'Cum sociis natoque penatibus et magnis dis parturient montes, na' +
          'scetur ridiculus mus. Quisque eget '
        
          'eros magna, ut blandit est. Ut et mi neque, in ornare eros. Cura' +
          'bitur eget neque ac justo ullamcorper '
        
          'malesuada vel eu felis. Cum sociis natoque penatibus et magnis d' +
          'is parturient montes, nascetur '
        
          'ridiculus mus. Integer bibendum risus vitae est aliquam at inter' +
          'dum purus sodales. Donec dictum felis '
        
          'et mauris vulputate hendrerit. Etiam sed eros id leo luctus dapi' +
          'bus sed et lectus. Vestibulum nisi nibh, '
        
          'feugiat vel dapibus vitae, laoreet tempus lorem. Proin lacinia p' +
          'ellentesque varius. Nulla consequat '
        
          'placerat magna, vitae lacinia erat lobortis mattis. Class aptent' +
          ' taciti sociosqu ad litora torquent per '
        
          'conubia nostra, per inceptos himenaeos. Nulla facilisi. Pellente' +
          'sque rhoncus sapien sed est ultrices sed '
        
          'auctor enim sodales. Nunc posuere ullamcorper diam at consequat.' +
          ' Curabitur fermentum nisl vitae '
        
          'mauris suscipit molestie. Mauris id mi arcu. Donec accumsan posu' +
          'ere ante id accumsan. Curabitur a '
        'diam nisl, id commodo ante. ')
      TabOrder = 0
      OnChange = edTextChange
    end
  end
  object Panel6: TPanel
    Left = 4
    Top = 149
    Width = 508
    Height = 330
    Align = alClient
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    object pbPreview: TPaintBox
      Left = 0
      Top = 0
      Width = 504
      Height = 326
      Align = alClient
      PopupMenu = PopupMenu1
      OnPaint = pbPreviewPaint
      ExplicitLeft = 7
      ExplicitTop = 166
      ExplicitWidth = 385
      ExplicitHeight = 255
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 176
    object BackgroundColor1: TMenuItem
      Caption = '&Background Color'
      OnClick = BackgroundColor1Click
    end
  end
  object ColorDialog1: TColorDialog
    Left = 248
    Top = 224
  end
end
