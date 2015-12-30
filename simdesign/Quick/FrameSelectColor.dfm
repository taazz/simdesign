object frSelectColor: TfrSelectColor
  Left = 0
  Top = 0
  Width = 127
  Height = 79
  TabOrder = 0
  object gbTitle: TGroupBox
    Left = 0
    Top = 0
    Width = 127
    Height = 79
    Caption = 'Selected Color:'
    TabOrder = 0
    object lbOpacityTitle: TLabel
      Left = 5
      Top = 42
      Width = 39
      Height = 13
      Caption = 'Opacity:'
    end
    object lbOpacity: TLabel
      Left = 50
      Top = 42
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Caption = '255'
    end
    object pnlColor: TPanel
      Left = 5
      Top = 16
      Width = 70
      Height = 25
      BevelOuter = bvLowered
      TabOrder = 0
      object pbColor: TPaintBox
        Left = 1
        Top = 1
        Width = 68
        Height = 23
        Align = alClient
        OnPaint = pbColorPaint
      end
    end
    object btnPick: TButton
      Left = 80
      Top = 16
      Width = 41
      Height = 17
      Caption = 'Pick...'
      TabOrder = 1
      OnClick = btnPickClick
    end
    object btnSelect: TButton
      Left = 80
      Top = 34
      Width = 41
      Height = 17
      Caption = 'Select'
      TabOrder = 2
      OnClick = btnSelectClick
    end
    object tbOpacity: TTrackBar
      Left = 2
      Top = 56
      Width = 72
      Height = 17
      Ctl3D = True
      Max = 255
      ParentCtl3D = False
      PageSize = 16
      Frequency = 32
      TabOrder = 3
      ThumbLength = 10
      OnChange = tbOpacityChange
    end
    object udOpacity: TUpDown
      Left = 81
      Top = 56
      Width = 24
      Height = 17
      Max = 255
      Orientation = udHorizontal
      TabOrder = 4
      OnClick = udOpacityClick
    end
  end
end
