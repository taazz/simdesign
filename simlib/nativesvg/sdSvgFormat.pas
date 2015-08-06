{ Project: Pyro
  Module: Pyro SVG

  Description:
  SVG document (Scalable Vector Graphics) using the Pyro document object model.
  Pyro's scene definition and object model is already SVG oriented.

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit sdSvgFormat;

interface

uses
  Classes, SysUtils, Windows, Graphics, sdDebug, pgScene, Pyro;

type

  TsdSvgFormat = class(TComponent)
  private
    FScene: TpgScene;
    FHeight: integer;
    FWidth: integer;
    FOnDebugOut: TsdDebugEvent;
    FDPI: double;
    procedure SetDPI(const Value: double);
  protected
    procedure GetSize;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderToBitmap(ABitmap: TBitmap);
    procedure LoadFromStream(S: TStream);
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property DPI: double read FDPI write SetDPI;
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

implementation

uses
  pgSvgImport, pgCoreRender, pgPyroCanvas;

{ TsdSvgFormat }

constructor TsdSvgFormat.Create(AOwner: TComponent);
begin
  inherited;
  FScene := TpgScene.Create(nil);
  FDPI := 120;
end;

destructor TsdSvgFormat.Destroy;
begin
  FreeAndNil(FScene);
  inherited;
end;

procedure TsdSvgFormat.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if assigned(FOnDebugOut) then
    FOnDebugOut(Sender, WarnStyle, AMessage);
end;

procedure TsdSvgFormat.GetSize;
var
  DI: TpgDeviceInfo;
begin
  DI.DPI.X := FDPI;
  DI.DPI.Y := FDPI;

  FWidth  := round(FScene.ViewPort.Width.ToDevice(DI) + 0.5);
  FHeight := round(FScene.ViewPort.Height.ToDevice(DI) + 0.5);
end;

procedure TsdSvgFormat.LoadFromStream(S: TStream);
var
  Import: TpgSvgImport;
begin
  // Create SVG Import class
  Import := TpgSvgImport.Create(Self);
  try

    // Import the scene into FScene, after this, the scene will have
    // a complete representation of the SVG file
    FreeAndNil(FScene);
    FScene := TpgScene.Create(nil);
    Import.OnDebugOut := Self.DoDebugOut;
    Import.ImportScene(FScene, S);

    // Get the size of the scene
    GetSize;

  finally
    Import.Free;
  end;
end;

procedure TsdSvgFormat.RenderToBitmap(ABitmap: TBitmap);
var
  R: TpgRect;
  BC: TpgPyroBitmapCanvas;
  CR: TpgCoreRenderer;
begin
  BC := TpgPyroBitmapCanvas.Create;
  CR := TpgCoreRenderer.Create;
  try

    // Device info - we must set this to assure the SVG renderer can
    // calculate lengths.
    BC.DeviceInfo.DPI.X := FDPI;
    BC.DeviceInfo.DPI.Y := FDPI;

    // Device rectangle in pixels of this size
    R := pgRect(0, 0, FWidth, FHeight);
    BC.DeviceRect := R;

    // fill with black-transparent
    BC.FillDeviceRect(R, $00000000);

    // Render the scene viewport to the canvas
    CR.Render(BC, FScene.ViewPort, nil);

    ABitmap.Assign(BC.Bitmap);

  finally
    CR.Free;
    BC.Free;
  end;
end;

procedure TsdSvgFormat.SetDPI(const Value: double);
begin
  FDPI := Value;
  GetSize;
end;

end.
