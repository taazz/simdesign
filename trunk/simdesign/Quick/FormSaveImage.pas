unit FormSaveImage;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, FrameSelectColor, Pyro, pgColor, pgWinGDI;

type

  TpgImageFormat = (
    ifBMP,
    ifPNG,
    ifGIF,
    ifJPG
  );

  TpgBitmapFormat = (
    bfARGBOrg,
    bfARGBPre,
    bfRGB
  );

  TpgExportImageSettings = record
    ImageFormat: TpgImageFormat;
    BackgroundColor: TpgColor32;
    BitmapFormat: TpgBitmapFormat;
    JpgQuality: integer;
  end;

const

  cDefaultExportImageSettings: TpgExportImageSettings =
    (ImageFormat: ifPNG;
     BackgroundColor: $00000000;
     BitmapFormat: bfARGBOrg;
     JpgQuality: 90;
    );

  cFilterForFormat: array[TpgImageFormat] of string =
    ('Windows Bitmap (*.bmp)|*.bmp',
     'Portable Network Graphics (*.png)|*.png',
     'Graphics Interchange Format (*.gif)|*.gif',
     'Joint Experts Group (*.jpg)|*.jpg');

  cExtensionForFormat: array[TpgImageFormat] of string =
    ('.bmp', '.png', '.gif', '.jpg');

  cMimeTypeForFormat: array[TpgImageFormat] of string =
    ('image/bmp', 'image/png', 'image/gif', 'image/jpg');
type

  TfrmSaveImage = class(TForm)
    scBackground: TfrSelectColor;
    gbBitmapType: TGroupBox;
    rbARGBPlain: TRadioButton;
    rbARGBPre: TRadioButton;
    rbRGB: TRadioButton;
    gbImageFormat: TGroupBox;
    rbBMP: TRadioButton;
    rbPNG: TRadioButton;
    rbGIF: TRadioButton;
    rbJPG: TRadioButton;
    tbJpgQuality: TTrackBar;
    lbQualTitle: TLabel;
    lbJpgQuality: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure rbImageFormatClick(Sender: TObject);
    procedure rbBitmapFormatClick(Sender: TObject);
    procedure tbJpgQualityChange(Sender: TObject);
  private
    { Private declarations }
    FSettings: TpgExportImageSettings;
    FIsUpdating: boolean;
    procedure UpdateControls;
  public
    { Public declarations }
    procedure FormFromSettings(const ASettings: TpgExportImageSettings);
    procedure FormToSettings(var ASettings: TpgExportImageSettings);
  end;

var
  frmSaveImage: TfrmSaveImage;

implementation

{$R *.dfm}

procedure TfrmSaveImage.FormCreate(Sender: TObject);
begin
  scBackground.SelectColor := clBlack;
  scBackground.SelectOpacity := 0;
end;

procedure TfrmSaveImage.rbImageFormatClick(Sender: TObject);
begin
  if FIsUpdating then exit;
  if rbBMP.Checked then FSettings.ImageFormat := ifBMP;
  if rbPNG.Checked then FSettings.ImageFormat := ifPNG;
  if rbGIF.Checked then FSettings.ImageFormat := ifGIF;
  if rbJPG.Checked then FSettings.ImageFormat := ifJPG;
  UpdateControls;
end;

procedure TfrmSaveImage.rbBitmapFormatClick(Sender: TObject);
begin
  if FIsUpdating then exit;
  if rbARGBPlain.Checked then FSettings.BitmapFormat := bfARGBOrg;
  if rbARGBPre.Checked   then FSettings.BitmapFormat := bfARGBPre;
  if rbRGB.Checked       then FSettings.BitmapFormat := bfRGB;
  UpdateControls;
end;

procedure TfrmSaveImage.UpdateControls;
var
  IsJPG: boolean;
  Alpha: byte;
  Color: TColor;
begin
  FIsUpdating := True;
  try
    rbBMP.Checked := FSettings.ImageFormat = ifBMP;
    rbPNG.Checked := FSettings.ImageFormat = ifPNG;
    rbGIF.Checked := FSettings.ImageFormat = ifGIF;
    rbARGBPlain.Checked := FSettings.BitmapFormat = bfARGBOrg;
    rbARGBPre.Checked   := FSettings.BitmapFormat = bfARGBPre;
    rbRGB.Checked       := FSettings.BitmapFormat = bfRGB;
    IsJPG := FSettings.ImageFormat = ifJPG;
    rbJPG.Checked := IsJPG;
    lbQualTitle.Visible := IsJPG;
    lbJpgQuality.Visible := IsJPG;
    tbJpgQuality.Visible := IsJPG;
    if IsJPG then begin
      rbRGB.Checked := True;
      FSettings.BitmapFormat := bfRGB;
      lbJpgQuality.Caption := IntToStr(FSettings.JpgQuality);
      tbJpgQuality.Position := FSettings.JpgQuality;
    end;
    rbARGBPlain.Enabled := not IsJPG;
    rbARGBPre.Enabled := not IsJPG;
    Color32ToGDI(FSettings.BackgroundColor, Alpha, Color);
    scBackground.SelectColor := Color;
    scBackground.SelectOpacity := Alpha;
  finally
    FIsUpdating := False;
  end;
end;

procedure TfrmSaveImage.tbJpgQualityChange(Sender: TObject);
begin
  if FIsUpdating then exit;
  FSettings.JpgQuality := tbJpgQuality.Position;
  UpdateControls;
end;

procedure TfrmSaveImage.FormFromSettings(const ASettings: TpgExportImageSettings);
begin
  FSettings := ASettings;
  UpdateControls;
end;

procedure TfrmSaveImage.FormToSettings(var ASettings: TpgExportImageSettings);
begin
  FSettings.BackgroundColor :=
    GDIToColor32(scBackground.SelectColor, scBackground.SelectOpacity);
  ASettings := FSettings;
end;

end.
