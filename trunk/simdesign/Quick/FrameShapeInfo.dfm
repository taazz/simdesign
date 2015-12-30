inherited frShapeInfo: TfrShapeInfo
  Width = 264
  Font.Height = -9
  Font.Name = 'Tahoma'
  ParentFont = False
  inherited pnlTop: TPanel
    Width = 264
    inherited pbTitle: TPaintBox
      Width = 264
    end
    inherited lbTitle: TLabel
      Width = 264
    end
  end
  object pcShape: TPageControl
    Left = 0
    Top = 17
    Width = 264
    Height = 223
    ActivePage = tsPaint
    Align = alClient
    TabOrder = 1
    object tsInfo: TTabSheet
      Caption = 'Info'
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 14
        Height = 11
        Caption = 'ID:'
      end
      object lbShapeId: TLabel
        Left = 40
        Top = 8
        Width = 42
        Height = 11
        Caption = 'lbShapeId'
      end
      object Label2: TLabel
        Left = 8
        Top = 20
        Width = 28
        Height = 11
        Caption = 'Name:'
      end
      object lbShapeName: TLabel
        Left = 40
        Top = 20
        Width = 42
        Height = 11
        Caption = 'lbShapeId'
      end
    end
    object tsPaint: TTabSheet
      Caption = 'Paint'
      ImageIndex = 1
      object Label3: TLabel
        Left = 128
        Top = 136
        Width = 56
        Height = 11
        Caption = 'Stroke Width:'
      end
      inline frFillColor: TfrSelectColor
        Left = 0
        Top = 48
        Width = 129
        Height = 89
        TabOrder = 0
        inherited gbTitle: TGroupBox
          Caption = 'Fill Color:'
          inherited lbOpacityTitle: TLabel
            Width = 36
            Height = 11
          end
          inherited lbOpacity: TLabel
            Left = 53
            Width = 15
            Height = 11
          end
          inherited btnPick: TButton
            Caption = 'Set...'
          end
        end
      end
      inline frStrokeColor: TfrSelectColor
        Left = 128
        Top = 48
        Width = 129
        Height = 89
        TabOrder = 1
        inherited gbTitle: TGroupBox
          Caption = 'Stroke Color:'
          inherited lbOpacityTitle: TLabel
            Width = 36
            Height = 11
          end
          inherited lbOpacity: TLabel
            Left = 53
            Width = 15
            Height = 11
          end
          inherited btnPick: TButton
            Caption = 'Set...'
          end
        end
      end
      object rgFill: TRadioGroup
        Left = 0
        Top = 0
        Width = 127
        Height = 49
        Caption = 'Fill Type:'
        Items.Strings = (
          'No Fill'
          'Color')
        TabOrder = 2
      end
      object rgStroke: TRadioGroup
        Left = 128
        Top = 0
        Width = 127
        Height = 49
        Caption = 'Stroke Type:'
        Items.Strings = (
          'No Stroke'
          'Color')
        TabOrder = 3
      end
      object edStrokeWidth: TEdit
        Left = 192
        Top = 132
        Width = 57
        Height = 19
        TabOrder = 4
      end
    end
  end
end
