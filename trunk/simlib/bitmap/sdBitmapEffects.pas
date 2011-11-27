unit sdBitmapEffects;
{
  This unit contains high-level bitmap effect generators. It uses the low-level
  SVG filter implementations in sdMapEffects in order to do so.

  The basic effect class is TsdBitmapEffect (does nothing). An effect has an
  Input bitmap and Output bitmap, and can be invoked with the Execute method.

  The TsdDropShadow effect adds a dropshadow to a bitmap, the TsdInnerBevel
  effect adds an "inner-bevel" effect.

  Author: Nils Haeck
  Copyright (c) 2007 - 2011 by SimDesign BV

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Modif: now uses GR32 TBitmap32

  Please visit http://www.simdesign.nl for more information.
}

interface

uses
  Classes, SysUtils,
  sdMapIterator, GR32, sdDebug;

type

  // Generic bitmap effect: serves as ancestor for bitmap effect classes
  TsdBitmapEffect = class(TDebugPersistent)
  private
    FInput: TBitmap32;
    FOutput: TBitmap32;
    FSImg: TsdMapIterator;
    FDImg: TsdMapIterator;
    FSAlpha: TsdMapIterator;
    FDAlpha: TsdMapIterator;
    FUseEqualDimensions: boolean;
  protected
    // This effect produces an output that has equal dimensions to input
    property UseEqualDimensions: boolean read FUseEqualDimensions;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
    property Input: TBitmap32 read FInput write FInput;
    property Output: TBitmap32 read FOutput write FOutput;
  end;

  // Add a drop-shadow effect to the bitmap in Input, and put the result in
  // Output.
  TsdDropShadowEffect = class(TsdBitmapEffect)
  private
    FBlurRadius: double;
    FOffsetY: double;
    FOffsetX: double;
    FShadowColor: TColor32;
  public
    constructor Create; override;
    procedure Execute; override;
    // Radius of the gaussian blur that creates the shadow.
    property BlurRadius: double read FBlurRadius write FBlurRadius;
    // Offset of shadow relative to shape in X
    property OffsetX: double read FOffsetX write FOffsetX;
    // Offset of shadow relative to shape in Y
    property OffsetY: double read FOffsetY write FOffsetY;
    // Color of the shadow, defaults to clBlack
    property ShadowColor: TColor32 read FShadowColor write FShadowColor;
  end;

  TsdInnerBevelEffect = class(TsdBitmapEffect)
  private
    FLightColor: TColor32;
    FLightPositionX: double;
    FLightPositionY: double;
    FLightPositionZ: double;
    FBevelRadius: double;
    FBumpyness: double;
  public
    constructor Create; override;
    procedure Execute; override;
    // Color of lighting on the graphic (default clWhite)
    property LightColor: TColor32 read FLightColor write FLightColor;
    // Position of light relative to graphic in X (default -40)
    property LightPositionX: double read FLightPositionX write FLightPositionX;
    // Position of light relative to graphic in Y (default -40)
    property LightPositionY: double read FLightPositionY write FLightPositionY;
    // Position of light relative to graphic in Z (default 100)
    property LightPositionZ: double read FLightPositionZ write FLightPositionZ;
    // Radius of the bevel; this is the radius of the gaussian blur used to
    // create the bevel (default 4.0)
    property BevelRadius: double read FBevelRadius write FBevelRadius;
    // Bumpyness of the bevel (default 8.0)
    property Bumpyness: double read FBumpyness write FBumpyness;
  end;

implementation

uses
  sdBitmapConversion32, sdMapEffects;

{ TsdBitmapEffect }

constructor TsdBitmapEffect.Create;
begin
  inherited;
  FSImg := TsdMapIterator.Create;
  FDImg := TsdMapIterator.Create;
  FSAlpha := TsdMapIterator.Create;
  FDAlpha := TsdMapIterator.Create;
end;

destructor TsdBitmapEffect.Destroy;
begin
  FreeAndNil(FSImg);
  FreeAndNil(FDImg);
  FreeAndNil(FSAlpha);
  FreeAndNil(FDAlpha);
  inherited;
end;

procedure TsdBitmapEffect.Execute;
begin
  // default does some checks and builds iterators
  if not assigned(FInput) then
  begin
    DoDebugOut(Self, wsFail, 'No input bitmap specified');
    exit;
  end;
  if not assigned(FOutput) then
  begin
    DoDebugOut(Self, wsFail, 'No output bitmap specified');
    exit;
  end;

  // use equal dimensions?
  if FUseEqualDimensions then
  begin
    FOutput.Width := FInput.Width;
    FOutput.Height := FInput.Height;
  end;

  GetBitmapIterator(FInput, FSImg);
  GetBitmapIterator(FOutput, FDImg);

  FSAlpha.Assign(FSImg);
  FSAlpha.IncrementMap(3);
  FDAlpha.Assign(FDImg);
  FDAlpha.IncrementMap(3);
end;

{ TsdDropShadowEffect }

constructor TsdDropShadowEffect.Create;
begin
  inherited;
  FBlurRadius := 10.0;
  FOffsetX := 5.0;
  FOffsetY := 5.0;
  FShadowColor := clBlack32;
end;

procedure TsdDropShadowEffect.Execute;
var
  W, H: integer;
  Blur: array of byte;
  BImg, BAlpha: TsdMapIterator;
begin
  inherited;
  W := FInput.Width;
  H := FInput.Height;

  BImg := TsdMapIterator.Create;
  BAlpha := TsdMapIterator.Create;
  try

    // blurred alpha channel buffer
    SetLength(Blur, W * H);
    BAlpha.Map := @Blur[0];
    BAlpha.Width := W;
    BAlpha.Height := H;
    BAlpha.CellStride := 1;
    BAlpha.ScanStride := W;

    // Iterator for shadow color
    BImg.Assign(BAlpha);
    BImg.CellStride := 0;
    BImg.ScanStride := 0;
    BImg.Map := @FShadowColor;

    // Offset the alphas buffer
    MapOffset8bitInteger(FSAlpha, BAlpha, 1, round(FOffsetX), round(FOffsetY));

    // Blur to the Alphas buffer
    MapFakeGaussianBlur8bit(BAlpha, BAlpha, FBlurRadius, FBlurRadius);

    // Composite S and Alpha layer on D
    MapComposite8bit(FSImg, BImg, FDImg, FSAlpha, BAlpha, FDAlpha, coOver);

  finally
    BImg.Free;
    BAlpha.Free;
  end;
end;

{ TsdInnerBevelEffect }

constructor TsdInnerBevelEffect.Create;
begin
  inherited;
  FLightColor := clWhite32;
  FLightPositionX := -40;
  FLightPositionY := -40;
  FLightPositionZ := 100;
  FBevelRadius := 4.0;
  FBumpyness := 8.0;
end;

procedure TsdInnerBevelEffect.Execute;
var
  W, H: integer;
  Blur: array of byte;
  Grads: array of smallint;
  BImg, BAlpha, Gx, Gy: TsdMapIterator;
  LS: TsdLightSource;
begin
  inherited;
  W := FInput.Width;
  H := FInput.Height;

  BImg := TsdMapIterator.Create;
  BAlpha := TsdMapIterator.Create;
  Gx := TsdMapIterator.Create;
  Gy := TsdMapIterator.Create;
  try

    // blurred alpha channel buffer
    SetLength(Blur, W * H);
    BAlpha.Map := @Blur[0];
    BAlpha.Width := W;
    BAlpha.Height := H;
    BAlpha.CellStride := 1;
    BAlpha.ScanStride := W;

    // Gradients
    SetLength(Grads, W * H * 2);
    Gx.Map := @Grads[0];
    Gx.Width := W;
    Gx.Height := H;
    Gx.CellStride := 4; // two smallints
    Gx.ScanStride := 4 * W;
    Gy.Assign(Gx);
    Gy.IncrementMap(2);

    // Blur to the Blur buffer
    MapFakeGaussianBlur8bit(FSAlpha, BAlpha, FBevelRadius, FBevelRadius);

    // Create Sobel gradients
    MapSobelGradient8bitToSmallint(BAlpha, Gx, Gy);

    // Lightsource structure
    FillChar(LS, SizeOf(LS), 0);
    LS.Lx := FLightPositionX;
    LS.Ly := FLightPositionY;
    LS.Lz := FLightPositionZ;
    LS.LightType := ltInfinite;

    // Calculate the diffuse ligting on the surface. The surface "bumpyness"
    // is determined by the Bumpyness factor (8.0 default)
    MapDiffuseLighting(FDImg, BAlpha, Gx, Gy, FLightColor, LS, FBumpyness, 1.0);

    // Composite the map using the arithmetic compositor. The values used for
    // K1 (0.8), K2 (0.1) and K3 (0.3) are empyrically determined to "look good".
    // K1 means influence of original map*lightmap, K2 means influence of light map, K3
    // means influence of original map. These coefficients should normally lie
    // in interval [0..1], and added up amount to approximately 1 - 1.5
    MapComposite8bit(FDImg, FSImg, FDImg, FDAlpha, FSAlpha, FDAlpha, coArithmetic,
      3, 0.8, 0.1, 0.3, 0);

    // Make sure to keep only the destination bitmap, and nothing outside the
    // source alpha channel
    MapComposite8bit(FDImg, FSImg, FDImg, FSAlpha, FSAlpha, FDAlpha, coOver);

  finally
    BImg.Free;
    BAlpha.Free;
    Gx.Free;
    Gy.Free;
  end;
end;

end.
