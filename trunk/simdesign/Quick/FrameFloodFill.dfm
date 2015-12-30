inherited frFloodFill: TfrFloodFill
  Width = 266
  Height = 157
  inherited pnlTop: TPanel
    Width = 266
    inherited pbTitle: TPaintBox
      Width = 266
    end
    inherited lbTitle: TLabel
      Width = 266
      Caption = 'Flood Fill'
    end
  end
  inline frFloodFillColor: TfrSelectColor
    Left = 4
    Top = 20
    Width = 127
    Height = 79
    TabOrder = 1
    inherited gbTitle: TGroupBox
      Caption = 'Floodfill Color'
    end
  end
  object btnPreview: TButton
    Left = 136
    Top = 112
    Width = 57
    Height = 17
    Caption = 'Preview'
    TabOrder = 2
    Visible = False
  end
  object btnAccept: TButton
    Left = 200
    Top = 112
    Width = 62
    Height = 17
    Caption = 'Accept'
    Enabled = False
    TabOrder = 3
    OnClick = btnAcceptClick
  end
  object btnCancel: TButton
    Left = 200
    Top = 132
    Width = 62
    Height = 17
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 4
    OnClick = btnCancelClick
  end
  inline frThreshold: TfrThreshold
    Left = 4
    Top = 102
    Width = 127
    Height = 52
    TabOrder = 5
    inherited gbThreshold: TGroupBox
      inherited tbThreshold: TTrackBar
        Max = 50
      end
    end
  end
  object gbPosition: TGroupBox
    Left = 136
    Top = 20
    Width = 127
    Height = 79
    Caption = 'Mouse Position'
    TabOrder = 6
    object lbXPos: TLabel
      Left = 5
      Top = 18
      Width = 31
      Height = 13
      Caption = 'X-Pos:'
    end
    object lbXVal: TLabel
      Left = 50
      Top = 18
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Caption = '255'
    end
    object lbYPos: TLabel
      Left = 5
      Top = 34
      Width = 31
      Height = 13
      Caption = 'Y-Pos:'
    end
    object lbYVal: TLabel
      Left = 50
      Top = 34
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Caption = '255'
    end
    object btnPick: TButton
      Left = 80
      Top = 16
      Width = 41
      Height = 17
      Caption = 'Pick...'
      TabOrder = 0
      OnClick = btnPickClick
    end
  end
end
