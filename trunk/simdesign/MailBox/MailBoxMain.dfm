object frmMailbox: TfrmMailbox
  Left = 421
  Top = 119
  Caption = 'Mailbox'
  ClientHeight = 682
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbName: TLabel
    Left = 16
    Top = 8
    Width = 58
    Height = 13
    Caption = 'GMail Name'
  end
  object lbPassword: TLabel
    Left = 16
    Top = 27
    Width = 76
    Height = 13
    Caption = 'GMail Password'
  end
  object Memo1: TMemo
    Left = 16
    Top = 88
    Width = 785
    Height = 577
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object edName: TEdit
    Left = 128
    Top = 8
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object btnCheckMail: TButton
    Left = 8
    Top = 46
    Width = 75
    Height = 25
    Caption = 'Check Mail'
    TabOrder = 2
    OnClick = btnCheckMailClick
  end
  object IdSMTP1: TIdSMTP
    OnConnected = IdSMTP1Connected
    SASLMechanisms = <>
    Left = 768
    Top = 16
  end
end
