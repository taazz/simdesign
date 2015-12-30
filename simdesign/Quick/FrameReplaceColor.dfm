inherited frReplaceColor: TfrReplaceColor
  Width = 266
  Height = 157
  inherited pnlTop: TPanel
    Width = 266
    inherited pbTitle: TPaintBox
      Width = 266
    end
    inherited lbTitle: TLabel
      Width = 266
      Caption = 'Replace Color'
    end
  end
  inline frSourceColor: TfrSelectColor
    Left = 4
    Top = 20
    Width = 127
    Height = 79
    TabOrder = 1
    inherited gbTitle: TGroupBox
      Caption = 'Replace Color...'
    end
  end
  inline frTargetColor: TfrSelectColor
    Left = 136
    Top = 20
    Width = 127
    Height = 79
    TabOrder = 2
    inherited gbTitle: TGroupBox
      Caption = '..With Color:'
    end
  end
  object btnPreview: TButton
    Left = 136
    Top = 112
    Width = 57
    Height = 17
    Caption = 'Preview'
    TabOrder = 3
    OnClick = btnPreviewClick
  end
  object btnAccept: TButton
    Left = 200
    Top = 112
    Width = 62
    Height = 17
    Caption = 'Accept'
    Enabled = False
    TabOrder = 4
    OnClick = btnAcceptClick
  end
  object btnCancel: TButton
    Left = 200
    Top = 132
    Width = 62
    Height = 17
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 5
    OnClick = btnCancelClick
  end
  inline frThreshold: TfrThreshold
    Left = 4
    Top = 102
    Width = 127
    Height = 52
    TabOrder = 6
    inherited gbThreshold: TGroupBox
      inherited tbThreshold: TTrackBar
        Max = 50
      end
    end
  end
end
