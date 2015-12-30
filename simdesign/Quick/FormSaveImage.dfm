object frmSaveImage: TfrmSaveImage
  Left = 440
  Top = 287
  Width = 408
  Height = 292
  Caption = 'Save Image Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline scBackground: TfrSelectColor
    Left = 8
    Top = 104
    Width = 201
    Height = 89
    TabOrder = 0
    inherited gbTitle: TGroupBox
      Width = 193
      Height = 81
      Caption = 'Background Color:'
      inherited btnPick: TButton
        Visible = False
      end
    end
  end
  object gbBitmapType: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 89
    Caption = 'Bitmap Type'
    TabOrder = 1
    object rbARGBPlain: TRadioButton
      Left = 8
      Top = 16
      Width = 177
      Height = 17
      Caption = 'ARGB plain colors 8bits/chan'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbBitmapFormatClick
    end
    object rbARGBPre: TRadioButton
      Left = 8
      Top = 40
      Width = 177
      Height = 17
      Caption = 'ARGB premultiplied 8bits/chan'
      TabOrder = 1
      OnClick = rbBitmapFormatClick
    end
    object rbRGB: TRadioButton
      Left = 8
      Top = 64
      Width = 113
      Height = 17
      Caption = 'RGB'
      TabOrder = 2
      OnClick = rbBitmapFormatClick
    end
  end
  object gbImageFormat: TGroupBox
    Left = 208
    Top = 8
    Width = 185
    Height = 177
    Caption = 'Image Format'
    TabOrder = 2
    object lbQualTitle: TLabel
      Left = 32
      Top = 104
      Width = 35
      Height = 13
      Caption = 'Quality:'
      Visible = False
    end
    object lbJpgQuality: TLabel
      Left = 80
      Top = 104
      Width = 12
      Height = 13
      Caption = '90'
      Visible = False
    end
    object rbBMP: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Windows BMP'
      TabOrder = 0
      OnClick = rbImageFormatClick
    end
    object rbPNG: TRadioButton
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'PNG'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbImageFormatClick
    end
    object rbGIF: TRadioButton
      Left = 8
      Top = 64
      Width = 113
      Height = 17
      Caption = 'GIF'
      TabOrder = 2
      OnClick = rbImageFormatClick
    end
    object rbJPG: TRadioButton
      Left = 8
      Top = 88
      Width = 113
      Height = 17
      Caption = 'JPEG'
      TabOrder = 3
      OnClick = rbImageFormatClick
    end
    object tbJpgQuality: TTrackBar
      Left = 24
      Top = 120
      Width = 150
      Height = 17
      Max = 100
      Frequency = 10
      Position = 90
      TabOrder = 4
      ThumbLength = 10
      Visible = False
      OnChange = tbJpgQualityChange
    end
  end
  object btnSave: TButton
    Left = 318
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 318
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
