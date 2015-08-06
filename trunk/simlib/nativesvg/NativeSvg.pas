{ Copyright (c) 2008 By Nils Haeck M.Sc. - SimDesign
  More information: www.simdesign.nl or n.haeck@simdesign.nl

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.
}
unit NativeSvg;

interface

uses
  {$ifdef MSWINDOWS}Windows,{$endif}
  {$ifdef lcl}IntfGraphics,{$endif}
  // RTL
  Classes, SysUtils, Graphics,

  // NativeJpg component
  NativeJpg, sdColorTransforms,

  // general, svg and pyro
  sdMapIterator, sdBitmapConversionWin, sdSvgFormat, sdBitmapResize, Pyro;

type

  // Implementation of TGraphic that renders SVG onto a bitmap
  TsdSvgGraphic = class(TGraphic)
  private
    FSvgFormat: TsdSvgFormat;
    FBitmap: TBitmap;
    FBackgroundColor: TColor;
    function GetVersion: string;
  protected
    // Assign this TsdSvgGraphic to Dest. The only valid type for Dest is TBitmap.
    // The bitmap in TsdSvgGraphic will be copied and color-converted to the bitmap in Dest.
    procedure AssignTo(Dest: TPersistent); override;
    // Draw the SVG graphic to the canvas, this routine uses the Windows function
    // AlphaBlend to blend the graphic to the canvas background.
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
  public
    // Assign Source to us, if it is a TsdSvgGraphic
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    // Since SVG graphics are often (semi) transparent, the background color
    // can be set for when a non-transparent bitmap is created.
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    // 
    property Bitmap: TBitmap read FBitmap;
    // Version returns the current version of the Pyro library.
    property Version: string read GetVersion;
    // Reference to the internal TsdSvgFormat
    property SvgFormat: TsdSvgFormat read FSvgFormat;
  end;

implementation

{ TsdSvgGraphic }

procedure TsdSvgGraphic.Assign(Source: TPersistent);
var
  Svg: TsdSvgGraphic;
begin
  if Source is TsdSvgGraphic then
  begin
    Svg := TsdSvgGraphic(Source);
    FBitmap.Assign(Svg.FBitmap);
  end else
    inherited;
end;

procedure TsdSvgGraphic.AssignTo(Dest: TPersistent);
var
  y: integer;
  Dst: TBitmap;
  SIter, DIter: TsdMapIterator;
  S, D: PByte;
  CT: TsdTransformBGRAToBGR;
begin
  if Dest is TBitmap then
  begin
    Dst := TBitmap(Dest);
    Dst.PixelFormat := pf24bit;
    DIter := TsdMapIterator.Create;
    SIter := TsdMapIterator.Create;
    try
      GetBitmapIterator(FBitmap, SIter);
      Dst.Width := FBitmap.Width;
      Dst.Height := FBitmap.Height;
      GetBitmapIterator(Dst, DIter);

      // Color transform that blends with a background color
      CT := TsdTransformBGRAToBGR.Create;
      CT.BkColor := FBackgroundColor;
      for y := 0 to DIter.Height - 1 do
      begin
        S := SIter.At(0, y);
        D := DIter.At(0, y);
        CT.Transform(S, D, DIter.Width);
      end;
      CT.Free;



    finally
      SIter.Free;
      DIter.Free;
    end;
  end else
    inherited
end;

constructor TsdSvgGraphic.Create;
begin
  inherited;
  FSvgFormat := TsdSvgFormat.Create(nil);
  FBitmap := TBitmap.Create;
  FBackgroundColor := clWhite;
end;

destructor TsdSvgGraphic.Destroy;
begin
  FreeAndNil(FSvgFormat);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TsdSvgGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  WReq, HReq, W, H: integer;
  Dest: TBitmap;
  BF: TBlendFunction;
begin
  // Determine correct scale
  WReq := Rect.Right - Rect.Left;
  HReq := Rect.Bottom - Rect.Top;
  W := GetWidth;
  H := GetHeight;

  // Blend function
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := $FF;
  BF.AlphaFormat := AC_SRC_ALPHA;

  if ((W = WReq) and (H = HReq)) or
     (W < WReq) or
     (H < HReq) then
  begin

    // Alpha-blended stretchdraw to canvas
    AlphaBlend(
      ACanvas.Handle, Rect.Left, Rect.Top, WReq, HReq,
      FBitmap.Canvas.Handle, 0, 0, W, H, BF);

  end else
  begin

    // Use a fast downsizing algo
    Dest := TBitmap.Create;
    try
      Dest.PixelFormat := pf32Bit;
      Dest.Width := WReq;
      Dest.Height := HReq;
      DownscaleBitmapWin(FBitmap, Dest);

      // Alphablended draw to canvas (since it's the right size now)
      AlphaBlend(
        ACanvas.Handle, Rect.Left, Rect.Top, WReq, HReq,
        Dest.Canvas.Handle, 0, 0, WReq, HReq, BF);
    finally
      Dest.Free;
    end;

  end;
end;

function TsdSvgGraphic.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TsdSvgGraphic.GetVersion: string;
begin
  Result := cPyroVersion;
end;

function TsdSvgGraphic.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TsdSvgGraphic.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
  inherited;
//not implemented
end;

procedure TsdSvgGraphic.LoadFromStream(Stream: TStream);
var
  M: TMemoryStream;
begin
  inherited;

  M := TMemoryStream.Create;
  try

    // Load the svg into the scene
    M.LoadFromStream(Stream);
    M.Position := 0;
    FSvgFormat.LoadFromStream(M);

    // Now get the bitmap
    FSvgFormat.RenderToBitmap(FBitmap);

  finally
    M.Free;
  end;
end;

procedure TsdSvgGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  inherited;
//not implemented
end;

procedure TsdSvgGraphic.SaveToStream(Stream: TStream);
begin
  inherited;
//todo
end;

initialization

  TPicture.RegisterFileFormat('svg', 'Scalable Vector Graphics', TsdSvgGraphic);

finalization

  TPicture.UnregisterGraphicClass(TsdSvgGraphic);

end.
