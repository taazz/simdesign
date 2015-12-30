inherited frRenderOptions: TfrRenderOptions
  Width = 231
  Height = 119
  inherited pnlTop: TPanel
    Width = 231
    inherited pbTitle: TPaintBox
      Width = 231
    end
    inherited lbTitle: TLabel
      Width = 231
      Caption = 'Render Options'
    end
  end
  object gbRenderEngine: TGroupBox
    Left = 8
    Top = 24
    Width = 217
    Height = 89
    Caption = 'Choose rendering engine'
    TabOrder = 1
    object rbBasicRenderer: TRadioButton
      Left = 8
      Top = 24
      Width = 201
      Height = 17
      Caption = 'Basic (non-Pyro) using only GDI'
      TabOrder = 0
      OnClick = rbRendererClick
    end
    object rbGDIRenderer: TRadioButton
      Left = 8
      Top = 40
      Width = 201
      Height = 17
      Caption = 'GDI with Pyro enhancements'
      Enabled = False
      TabOrder = 1
      OnClick = rbRendererClick
    end
    object rbPyroRenderer: TRadioButton
      Left = 8
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Full Pyro engine'
      TabOrder = 2
      OnClick = rbRendererClick
    end
  end
end
