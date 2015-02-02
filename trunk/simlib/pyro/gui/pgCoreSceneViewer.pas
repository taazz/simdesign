{ <b>Project</b>: Pyro<p>

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgCoreSceneViewer;

interface

uses
  SysUtils, Classes, Messages, Graphics, pgScene, pgViewer,
  pgSurface, pgViewPort, pgContentProvider, pgBitmap, {pgGDICanvas,}
  pgTransform, pgWinGDI, pgCanvas, Math, Pyro;

type

  // Viewer that renders a single scene to the canvas or device of the
  // associate
  TpgCoreSceneViewer = class(TpgViewer)
  private
    FScene: TpgScene;
  protected
    procedure SetScene(const Value: TpgScene); virtual;
    procedure RenderContent(ACanvas: TpgCanvas); override;
    procedure SceneBeforeChange(Sender: TObject; AElementId, APropId: longword;
      AChange: TpgChangeType); virtual;
    procedure SceneAfterChange(Sender: TObject; AElementId, APropId: longword;
      AChange: TpgChangeType); virtual;
  published
    destructor Destroy; override;
    // Reference to scene to render with the viewer
    property Scene: TpgScene read FScene write SetScene;
  end;

implementation

{ TpgCoreSceneViewer }

destructor TpgCoreSceneViewer.Destroy;
begin
  SetScene(nil);
  inherited;
end;

procedure TpgCoreSceneViewer.RenderContent(ACanvas: TpgCanvas);
begin
  // Render main viewport
  if assigned(FScene) then
    Renderer.Render(ACanvas, FScene.ViewPort, Transform);
end;

procedure TpgCoreSceneViewer.SceneAfterChange(Sender: TObject; AElementId,
  APropId: longword; AChange: TpgChangeType);
begin
  // Since we have no notion of bounding boxes in scene viewer, we invalidate the
  // whole scene on any change
  Invalidate;
end;

procedure TpgCoreSceneViewer.SceneBeforeChange(Sender: TObject; AElementId,
  APropId: longword; AChange: TpgChangeType);
begin
  // Since we have no notion of bounding boxes in scene viewer, we invalidate the
  // whole scene on any change
  Invalidate;
end;

procedure TpgCoreSceneViewer.SetScene(const Value: TpgScene);
var
  VP: TpgViewPort;
  L: TpgSceneListener;
begin
  if FScene <> Value then
  begin
    if assigned(FScene) then
      FScene.Listeners.DeleteRef(Self);
    FScene := Value;
    if assigned(Renderer) then
      Renderer.SetScene(Scene);
    if assigned(FScene) then
    begin
      VP := FScene.ViewPort;
      SetContentSize(
        VP.Width.ToDevice(DeviceInfo^),
        VP.Height.ToDevice(DeviceInfo^));
      L := FScene.Listeners.AddRef(Self);
      L.OnBeforeChange := SceneBeforeChange;
      L.OnAfterChange := SceneAfterChange;
    end;
    Invalidate;
  end;
end;

end.
