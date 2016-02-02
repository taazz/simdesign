object frmMain: TfrmMain
  Left = 234
  Top = 137
  Caption = 'pgPyroControl Demo'
  ClientHeight = 582
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 112
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 632
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Button 2'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 449
    Height = 553
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
  end
  object mnuMain: TMainMenu
    Left = 776
    Top = 16
    object File1: TMenuItem
      Caption = 'File'
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object mnuAnimate: TMenuItem
      Caption = 'Animate'
      object mnuRun: TMenuItem
        Caption = 'Run!'
        OnClick = mnuRunClick
      end
    end
    object mnuOptions: TMenuItem
      Caption = 'Options'
      object mnuPyrodrawing: TMenuItem
        AutoCheck = True
        Caption = 'Pyro-drawing'
        Checked = True
        OnClick = mnuPyrodrawingClick
      end
    end
  end
end
