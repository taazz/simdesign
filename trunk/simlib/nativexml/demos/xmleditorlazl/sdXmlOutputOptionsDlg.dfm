object XmlOutputOptionsDlg: TXmlOutputOptionsDlg
  Left = 1389
  Height = 282
  Top = 727
  Width = 478
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'XML Output Options'
  ClientHeight = 282
  ClientWidth = 478
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  LCLVersion = '1.0.14.0'
  object GroupBox1: TGroupBox
    Left = 8
    Height = 217
    Top = 8
    Width = 233
    Caption = 'XML Encoding'
    ClientHeight = 201
    ClientWidth = 229
    TabOrder = 0
    object rbStoredEncoding: TRadioButton
      Left = 8
      Height = 24
      Top = 24
      Width = 136
      Caption = 'Stored Encoding'
      TabOrder = 0
    end
    object rbDefaultEncodings: TRadioButton
      Left = 8
      Height = 24
      Top = 48
      Width = 151
      Caption = 'Default Encodings:'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbCodepage: TRadioButton
      Left = 8
      Height = 24
      Top = 152
      Width = 274
      Caption = 'Ansi encoding with specific codepage'
      TabOrder = 2
    end
    object lbDefaultEncodings: TListBox
      Left = 24
      Height = 73
      Top = 72
      Width = 121
      Items.Strings = (
        'utf-8'
        'utf-16'
        'windows-1252'
      )
      ItemHeight = 20
      ScrollWidth = 119
      TabOrder = 3
    end
    object edCodepage: TEdit
      Left = 24
      Height = 24
      Top = 176
      Width = 121
      TabOrder = 4
      Text = 'windows-1258'
    end
  end
  object btnOK: TButton
    Left = 144
    Height = 25
    Top = 232
    Width = 75
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 272
    Height = 25
    Top = 232
    Width = 75
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object rgXmlFormat: TRadioGroup
    Left = 247
    Height = 81
    Top = 8
    Width = 218
    AutoFill = True
    Caption = 'Xml Format'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.TopBottomSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 65
    ClientWidth = 214
    ItemIndex = 2
    Items.Strings = (
      'Compact'
      'Readable'
      'Preserve'
    )
    TabOrder = 3
  end
end