{ <b>Project</b>: Pyro<p>

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgSceneViewer;

interface

uses
  pgCoreSceneViewer, pgCoreRender, pgRender;

type

  TpgSceneViewer = class(TpgCoreSceneViewer)
  protected
    class function GetRendererClass: TpgRendererClass; override;
  end;

implementation

{ TpgSceneViewer }

class function TpgSceneViewer.GetRendererClass: TpgRendererClass;
begin
  Result := TpgRenderer;
end;

end.
