object Form1: TForm1
  Left = 322
  Top = 255
  Width = 621
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 145
    Width = 613
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 613
    Height = 116
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 107
      Height = 13
      Caption = 'PDF debug messages:'
    end
    object mmDebug: TMemo
      Left = 0
      Top = 27
      Width = 613
      Height = 89
      Align = alBottom
      Lines.Strings = (
        'mmDebug')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 148
    Width = 613
    Height = 419
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 613
      Height = 419
      HorzScrollBar.Tracking = True
      VertScrollBar.Tracking = True
      Align = alClient
      TabOrder = 0
      object imPdf: TImage
        Left = 0
        Top = 0
        Width = 105
        Height = 105
        AutoSize = True
      end
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 567
    Width = 613
    Height = 19
    Panels = <>
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 613
    Height = 29
    ButtonHeight = 21
    ButtonWidth = 74
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 3
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = acPagePrev
    end
    object ToolButton2: TToolButton
      Left = 74
      Top = 2
      Action = acPageNext
    end
    object ToolButton3: TToolButton
      Left = 148
      Top = 2
      Action = acZoomMinus
    end
    object ToolButton4: TToolButton
      Left = 222
      Top = 2
      Action = acZoomPlus
    end
    object ToolButton5: TToolButton
      Left = 296
      Top = 2
      Action = acAsOCX
    end
  end
  object MainMenu1: TMainMenu
    Left = 440
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object acPdfLoad1: TMenuItem
        Action = acPdfLoad
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object acPagePrev1: TMenuItem
        Action = acPagePrev
      end
      object acPageNext1: TMenuItem
        Action = acPageNext
      end
      object acZoomPlus1: TMenuItem
        Action = acZoomPlus
      end
      object acZoomMinus1: TMenuItem
        Action = acZoomMinus
      end
    end
  end
  object ActionList1: TActionList
    Left = 472
    Top = 24
    object acPdfLoad: TAction
      Caption = 'Open PDF...'
      OnExecute = acPdfLoadExecute
    end
    object acPagePrev: TAction
      Caption = 'acPagePrev'
      OnExecute = acPagePrevExecute
    end
    object acPageNext: TAction
      Caption = 'acPageNext'
      OnExecute = acPageNextExecute
    end
    object acZoomPlus: TAction
      Caption = 'acZoomPlus'
      OnExecute = acZoomPlusExecute
    end
    object acZoomMinus: TAction
      Caption = 'acZoomMinus'
      OnExecute = acZoomMinusExecute
    end
    object acAsOCX: TAction
      Caption = 'acAsOCX'
      OnExecute = acAsOCXExecute
    end
  end
end
