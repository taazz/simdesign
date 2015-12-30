object frToolWin: TfrToolWin
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  ParentBackground = False
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object pbTitle: TPaintBox
      Left = 0
      Top = 0
      Width = 320
      Height = 17
      Align = alClient
      OnPaint = pbTitlePaint
    end
    object lbTitle: TLabel
      Left = 0
      Top = 0
      Width = 320
      Height = 17
      Align = alClient
      Alignment = taCenter
      Caption = 'Title'
      Transparent = True
    end
  end
end
