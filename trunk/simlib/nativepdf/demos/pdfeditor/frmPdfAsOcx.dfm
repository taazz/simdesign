object fmAsOCX: TfmAsOCX
  Left = 716
  Top = 360
  Width = 468
  Height = 424
  Caption = 'fmAsOCX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 460
    Height = 29
    ButtonHeight = 21
    ButtonWidth = 66
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = acPrevPage
    end
    object ToolButton2: TToolButton
      Left = 66
      Top = 2
      Action = acNextPage
    end
    object ToolButton3: TToolButton
      Left = 132
      Top = 2
      Caption = 'ToolButton3'
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 198
      Top = 2
      Caption = 'ToolButton4'
      ImageIndex = 3
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 378
    Width = 460
    Height = 19
    Panels = <>
  end
  object ActionList1: TActionList
    Left = 408
    Top = 10
    object acPrevPage: TAction
      Caption = 'acPrevPage'
      OnExecute = acPrevPageExecute
    end
    object acNextPage: TAction
      Caption = 'acNextPage'
      OnExecute = acNextPageExecute
    end
  end
end
