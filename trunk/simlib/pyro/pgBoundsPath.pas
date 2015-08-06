{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Render<p>

  <b>Description:</b><p>
  Special path implementation used for boundary detection

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgBoundsPath;

{$i simdesign.inc}

interface

uses
  pgRenderPath, Pyro;

type

  TpgBoundsPath = class(TpgRenderPath)
  protected
    procedure ComputeTolerances; override;
  public
  end;

implementation

{ TpgBoundsPath }

procedure TpgBoundsPath.ComputeTolerances;
var
  Eps: double;
begin
  if PixelScale = 0 then exit;

  // Equivalent to 5 pixel error
  Eps := 5 / PixelScale;

  // distance tolerances for beziers
  FDistTolSqr := sqr(0.5 * Eps);
  FDistTolTaxicab := 4 * Eps;

  // Allow small error on cusps
  FCuspLimit := 0.1;

  // this indicates that no angle tolerance will be used
  FAngleTol := 0;
end;

end.
