{ Project: Pyro

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgControls;

interface

uses
  Windows, Graphics, Messages, Classes, SysUtils, Controls,
  // pyro
  pgWinGDI, pgCanvas, pgPyroCanvas, pgGDICanvas, pgTransform, pgPlatform, Pyro;

type

  // A simple standalone TWinControl override, a la TCustomControl, providing
  // a Pyro canvas TpgCanvas descendant instead of VCL TCanvas.
  TpgCustomControl = class(TWinControl)
  private
    FCanvas: TpgCanvas;
    FCanvasType: TpgCanvasType;
    FFillBackground: boolean;
    FHandle: HDC;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetCanvasType(const Value: TpgCanvasType);
  protected
    procedure RecreateCanvas;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    property FillBackground: boolean read FFillBackground write FFillBackground default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TpgCanvas read FCanvas;
    property Handle: HDC read FHandle;
  published
    property CanvasType: TpgCanvasType read FCanvasType write SetCanvasType;
  end;

implementation

{ TpgCustomControl }

constructor TpgCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  // By default
  if CanvasClassByType(ctPyro) <> nil then
    FCanvasType := ctPyro
  else
    FCanvasType := ctGDI;
  FFillBackground := True;
  RecreateCanvas;
end;

destructor TpgCustomControl.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited;
end;

procedure TpgCustomControl.Paint;
begin
// default does nothing
end;

procedure TpgCustomControl.PaintWindow(DC: HDC);
var
  S: TpgState;
  R: TpgRect;
  Color: TColor;
begin
  // Get clipping rect
  pgGetClipBox(DC, R);
  if pgIsRectEmpty(R) then
    exit;

  try

    // set canvas clipping rectangle
    FCanvas.DeviceRect := R;

    // Fill with background color
    if FFillBackground then
      FCanvas.FillDeviceRect(R, GDIToColor32(Color, $FF));

    // Do a canvas push first so we can do a pop later and remove all the objects
    // the user has created in the overridden paint event
    S := FCanvas.Push;
    try
      // Now call paint
      Paint;
    finally
      // Restore settings
      FCanvas.Pop(S);
    end;

  finally

    // And draw the resulting bitmap to the device
    pgSetWindowOrgEx(FCanvas.DeviceHandle, 0, 0, nil);
    pgBitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
      FCanvas.DeviceHandle, 0, 0, pgSRCCOPY);

  end;
end;

procedure TpgCustomControl.RecreateCanvas;
var
  CanvasClass: TpgCanvasClass;
begin
  // Free old canvas-related objects
  FreeAndNil(FCanvas);

  CanvasClass := CanvasClassByType(FCanvasType);
  if not assigned(CanvasClass) then
    raise Exception.Create('Invalid canvas type');
    
  FCanvas := CanvasClass.Create;
end;

procedure TpgCustomControl.SetCanvasType(const Value: TpgCanvasType);
begin
  if FCanvasType <> Value then
  begin
    FCanvasType := Value;
    RecreateCanvas;
    Invalidate;
  end;
end;

procedure TpgCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // No automatic erase of background
  Message.Result := integer(False);
end;

procedure TpgCustomControl.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

function IsPyroControl(AControl: TControl): boolean;
begin
  Result := False;
end;

function IsPyroWinControl(AControl: TControl): boolean;
begin
  Result := False;
end;

end.
