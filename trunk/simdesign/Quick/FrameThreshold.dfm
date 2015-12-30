object frThreshold: TfrThreshold
  Left = 0
  Top = 0
  Width = 127
  Height = 52
  TabOrder = 0
  object gbThreshold: TGroupBox
    Left = 0
    Top = 0
    Width = 127
    Height = 52
    Align = alClient
    Caption = 'Threshold'
    TabOrder = 0
    object lbThreshold: TLabel
      Left = 56
      Top = 15
      Width = 12
      Height = 13
      Alignment = taRightJustify
      Caption = '10'
    end
    object lbValue: TLabel
      Left = 5
      Top = 15
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object tbThreshold: TTrackBar
      Left = 2
      Top = 29
      Width = 71
      Height = 17
      Max = 128
      PageSize = 8
      Frequency = 10
      TabOrder = 0
      ThumbLength = 10
      OnChange = tbThresholdChange
    end
    object udThreshold: TUpDown
      Left = 81
      Top = 29
      Width = 24
      Height = 17
      Max = 255
      Orientation = udHorizontal
      TabOrder = 1
      OnClick = udThresholdClick
    end
  end
end
