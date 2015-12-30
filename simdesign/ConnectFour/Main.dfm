object frmMain: TfrmMain
  Left = 541
  Top = 208
  Width = 550
  Height = 455
  Caption = 'Connect Four (Vier op '#39'n Rij)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object sbMain: TStatusBar
    Left = 0
    Top = 382
    Width = 542
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object mnuMain: TMainMenu
    Left = 496
    Top = 16
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object mnuGame: TMenuItem
      Caption = 'Game'
      object mnuPlayerPlayer: TMenuItem
        Caption = 'Player - Player'
        Checked = True
        RadioItem = True
      end
      object mnuComputerPlayer: TMenuItem
        Caption = 'Computer - Player'
        RadioItem = True
      end
      object mnuPlayerComputer: TMenuItem
        Caption = 'Player - Computer'
        RadioItem = True
      end
    end
  end
end
