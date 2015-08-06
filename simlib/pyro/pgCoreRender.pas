{ Project: Pyro
  Module: Pyro Edit

  Description:
  Renderer for scene viewer

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgCoreRender;

{$i simdesign.inc}

interface

uses
  SysUtils, Contnrs, Graphics, Classes, pgTransform,
  pgShape, pgPath, Pyro, pgCanvas, pgDocument, pgImage, pgText,
  pgViewPort, pgScene;

type

  TpgRenderFilter = (
    rfAll,
    rfNonSelectable
  );

  TpgCoreRenderer = class(TPersistent)
  private
    FPath: TpgPath; // ref to path obtained from canvas
    FFill: TpgFill; // ref to fill obtained from canvas
    FStroke: TpgStroke; // ref to stroke obtained from canvas
    FFilter: TpgRenderFilter;
  protected
    procedure RenderElement(ACanvas: TpgCanvas; AElement: TpgElement); virtual;
    procedure RenderGraphic(ACanvas: TpgCanvas; AGraphic: TpgGraphic); virtual;
    procedure RenderShape(ACanvas: TpgCanvas; AShape: TpgShape); virtual;
    procedure RenderImageView(ACanvas: TpgCanvas; AImageView: TpgImageView); virtual;
    procedure RenderText(ACanvas: TpgCanvas; AText: TpgText); virtual;
    procedure PaintDefinitionFromPaintable(ACanvas: TpgCanvas; APaintable: TpgPaintable); virtual;
    function FontDefinitionFromText(ACanvas: TpgCanvas; AText: TpgText): TpgFont; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetScene(AScene: TpgScene); virtual;

    // Render AGraphic and all its descendant elements to the canvas. ATransform
    // describes the transform from document to screen.
    procedure Render(ACanvas: TpgCanvas; AElement: TpgElement; ATransform: TpgTransform); virtual;

    // Filter indicating what to render
    property Filter: TpgRenderFilter read FFilter write FFilter;
  end;

  TpgRendererClass = class of TpgCoreRenderer;

implementation

type

  TImageViewAccess = class(TpgImageView);
  TTransformListAccess = class(TpgTransformList);

{ TpgCoreRenderer }

constructor TpgCoreRenderer.Create;
begin
  inherited Create;
  // Defaults
end;

destructor TpgCoreRenderer.Destroy;
begin
  inherited;
end;

function TpgCoreRenderer.FontDefinitionFromText(ACanvas: TpgCanvas; AText: TpgText): TpgFont;
var
  FontFamily: string;
begin
  FontFamily := AText.FontFamily.Value;
  Result := ACanvas.NewFont(FontFamily);
end;

procedure TpgCoreRenderer.PaintDefinitionFromPaintable(ACanvas: TpgCanvas; APaintable: TpgPaintable);
var
  i: integer;
  DAPaintable: TpgLengthProtectList;
  // local
  procedure PaintPropToPaint(APaintProp: TpgPaintProp; APaint: TpgPaint);
  begin
    APaint.SetColorWithInfo(APaintProp.Color, APaintProp.ColorInfo);
    APaint.PaintStyle := APaintProp.PaintStyle;
  end;
// main
begin
  // Fill
  PaintPropToPaint(APaintable.Fill, FFill);
  FFill.Opacity := APaintable.FillOpacity.Value * APaintable.Opacity.Value;

  // Stroke
  PaintPropToPaint(APaintable.Stroke, FStroke);
  FStroke.Opacity := APaintable.StrokeOpacity.Value * APaintable.Opacity.Value;
  FStroke.Width := APaintable.StrokeWidth.ToDevice(ACanvas.DeviceInfo^);

  // Dash array in stroke
  if APaintable.StrokeDashArray.ExistsLocal then
  begin
    DAPaintable := APaintable.StrokeDashArray.Values;
    for i := 0 to DAPaintable.Count - 1 do
      FStroke.Dashes[i] := DAPaintable[i].ToDevice(ACanvas.DeviceInfo^);
    FStroke.DashOffset := APaintable.StrokeDashOffset.ToDevice(ACanvas.DeviceInfo^);
  end else
    FStroke.ClearDashes;
end;

procedure TpgCoreRenderer.Render(ACanvas: TpgCanvas; AElement: TpgElement; ATransform: TpgTransform);
var
  S: TpgState;
begin
  S := ACanvas.Push;
  try
    // create objects we're going to re-use, valid until canvas.pop below
    FPath := ACanvas.NewPath;
    FFill := ACanvas.NewFill;
    FStroke := ACanvas.NewStroke;

    ACanvas.AddTransform(ATransform, True);
    RenderElement(ACanvas, AElement);
  finally
    ACanvas.Pop(S);
  end;
end;

procedure TpgCoreRenderer.RenderElement(ACanvas: TpgCanvas; AElement: TpgElement);
var
  S: TpgState;
  i: integer;
  MustRender: boolean;
  VP: TpgViewPort;
  T: TpgTransform;
  L: TpgLayer;
begin
  // get updated transform
  S := ACanvas.Push;
  try
    // Concat a transform of the TpgGraphic element - if any
    if AElement is TpgGraphic then
      ACanvas.AddTransform(TpgGraphic(AElement).Transform.Value, True);

    // Element is a viewport?
    if AElement is TpgViewPort then
    begin
      VP := AElement as TpgViewPort;
      // Clip to the new viewport
      if (VP.Width.Value > 0) and (VP.Height.Value > 0) then
        ACanvas.ClipRectangle(
          VP.X.ToDevice(ACanvas.DeviceInfo^), VP.Y.ToDevice(ACanvas.DeviceInfo^),
          VP.Width.ToDevice(ACanvas.DeviceInfo^), VP.Height.ToDevice(ACanvas.DeviceInfo^),
          0, 0);
      // Put the viewbox transform to work
      T := VP.BuildViewBoxTransform(ACanvas.DeviceInfo^);
      ACanvas.AddTransform(T);
    end;

    // Apply filters
    MustRender := True;
    if (FFilter = rfNonSelectable) and (AElement is TpgGraphic) then
      MustRender := eoDenySelect in TpgGraphic(AElement).EditorOptions.Value;

    // Do we render this element?
    if not MustRender then
      exit;

    // do rendering here
    if AElement is TpgGraphic then
      RenderGraphic(ACanvas, TpgGraphic(AElement));

    // render any children
    if AElement is TpgGroup then
    begin
      // Check group opacity
      if TpgGroup(AElement).Opacity.Value < 1 then
      begin
        // Render the group on a layer, so we can apply the opacity
        L := ACanvas.PushLayer(1);
        try
          // Set layer opacity
          L.Opacity := TpgGroup(AElement).Opacity.Value;
          // Layer rendering
          for i := 0 to AElement.ElementCount - 1 do
          begin
            if AElement.Elements[i] is TpgPaintable then // check this!
              RenderElement(L.Canvas, TpgElement(AElement).Elements[i]);
          end;
        finally
          ACanvas.PopLayer(L);
        end;
      end else
      begin
        // Normal rendering, no layer needed
        for i := 0 to AElement.ElementCount - 1 do
        begin
          if AElement.Elements[i] is TpgPaintable then // check this!
            RenderElement(ACanvas, TpgElement(AElement).Elements[i]);
        end;
      end;
    end;

  finally
    // Restore transformlist
    ACanvas.Pop(S);
  end;
end;

procedure TpgCoreRenderer.RenderGraphic(ACanvas: TpgCanvas; AGraphic: TpgGraphic);
begin
  // Split up between element types
  if AGraphic is TpgShape then

    RenderShape(ACanvas, TpgShape(AGraphic))

  else if AGraphic is TpgImageView then

    RenderImageView(ACanvas, TpgImageView(AGraphic))

  else if AGraphic is TpgText then

    RenderText(ACanvas, TpgText(AGraphic));

end;

procedure TpgCoreRenderer.RenderImageView(ACanvas: TpgCanvas; AImageView: TpgImageView);
var
  VBMinX, VBMinY, VBWidth, VBHeight: double;
begin
  TImageViewAccess(AImageView).GetViewBoxProps(VBMinX, VBMinY, VBWidth, VBHeight);
  ACanvas.PaintBitmap(AImageView.Image.Bitmap, -VBMinX, -VBMinY, VBWidth, VBHeight);
end;

procedure TpgCoreRenderer.RenderShape(ACanvas: TpgCanvas; AShape: TpgShape);
begin
  PaintDefinitionFromPaintable(ACanvas, AShape);

  // Setup path
  FPath.Clear;
  FPath.PixelScale := 1 * ACanvas.PixelScale;
  if not ACanvas.IsLinear then
    FPath.BreakupLength := 2 / FPath.PixelScale;

  if FStroke.PaintStyle <> psNone then
    FPath.ExpectedStrokeWidth := FStroke.Width

  else
    FPath.ExpectedStrokeWidth := 0;

  // play the path
  AShape.PlayFillPath(FPath, ACanvas.DeviceInfo^);

  // Shape
  if not FPath.IsEmpty then
    ACanvas.PaintPath(FPath, FFill, FStroke)
end;

procedure TpgCoreRenderer.RenderText(ACanvas: TpgCanvas; AText: TpgText);
var
  Font: TpgFont;
  FontSize: double;
  X, Y: double;
begin
  PaintDefinitionFromPaintable(ACanvas, AText);
  Font := FontDefinitionFromText(ACanvas, AText);
  FontSize := AText.FontSize.ToDevice(ACanvas.DeviceInfo^);

  // this needs *way* more
  X := 0; Y := 0;
  if AText.X.Values.Count > 0 then
    X := AText.X.Values[0].Value;

  if AText.Y.Values.Count > 0 then
    Y := AText.Y.Values[0].Value;

  ACanvas.PaintText(X, Y, AText.Text.AsString, Font, FontSize, FFill, FStroke);
end;

procedure TpgCoreRenderer.SetScene(AScene: TpgScene);
begin
// default does nothing
end;

end.
