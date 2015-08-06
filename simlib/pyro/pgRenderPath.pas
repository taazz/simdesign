{ Project: Pyro
  Module: Pyro Render

  Description:
  Specific path implementation used in rendering. It subdivides beziers and arcs
  into line segments.

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgRenderPath;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, pgGeometry, pgPath, pgPolygon, Pyro;

type

  // Single-precision path implementation which subdivides all arcs and beziers
  // into line segments and which is used for rendering.
  TpgRenderPath = class(TpgPath)
  private
    FPoly: TpgPolyPolygon;
  protected
    // tolerances, used internally when rendering arcs and beziers
    FDistTolSqr: double;
    FDistTolTaxicab: double;
    FCuspLimit: single;
    FAngleTol: single;
    procedure CurveToCubicRecursive(const P1, P2, P3, P4: TpgPoint; Level: integer);
    procedure CurveToQuadraticIncremental(const P1, P2, P3: TpgPoint);
    procedure CurveToQuadraticRecursive(const P1, P2, P3: TpgPoint; Level: integer);
    procedure ComputeTolerances; override;
    function GetPathLength: double; override;
    function GetBreakupLength: double; override;
    procedure SetBreakupLength(const Value: double); override;
    function GetAsPolyPolygon: TpgPolyPolygon; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function BoundingBox: TpgBox; override;
    procedure Clear; override;
    procedure CleanupMemory; override;
    procedure ClosePath; override;
    procedure MoveTo(const X, Y: double); override;
    procedure LineTo(const X, Y: double); override;
    procedure CurveToCubic(const C1x, C1y, C2x, C2y, X, Y: double); override;
    procedure CurveToQuadratic(const Cx, Cy, X, Y: double); override;
    procedure CurveToQuadraticInc(const Cx, Cy, X, Y: double); override;
    procedure ArcTo(const Rx, Ry, Angle: double; LargeArc, Sweep: boolean; const X, Y: double); override;
    procedure ArcToStraight(const Cx, Cy, Rx, Ry, N1, N2: double;
      const X, Y: double); override;
    function IsEmpty: boolean; override;
    procedure CopySegment(Dest: TpgPath; const PosStart, PosEnd: TpgPathPosition); override;
    function PositionAlongPath(var APos: TpgPathPosition; Increment: double = 0): TpgPoint; override;
    function SizeInBytes: integer; override;
    procedure ScaleUniform(const S: double); override;
    function PointInPath(const X, Y: double; FillRule: TpgFillRule): boolean; override;
    function PointOnPath(const X, Y, Tolerance: double): boolean; override;
  end;

procedure PathItemBuildNormals(AItem: TpgPolygonItem; var Normals: array of TpgPoint);

const

  cDefaultCuspLimit = 0;
  cCurveRecursionLimit = 30;
  cCurveCollinearityEps = 1e-30;

implementation

procedure PathItemBuildNormals(AItem: TpgPolygonItem; var Normals: array of TpgPoint);
var
  i: integer;
  PointA, PointB: PpgPoint;
  Dx, Dy: single;
  R, F: double;
begin
  if AItem.PointCount <= 0 then
    exit;
  PointA := AItem.FirstPoint;
  for i := 0 to AItem.PointCount - 1 do
  begin

    // Get point B, auto-wrap around
    PointB := AItem.Points[(i + 1) mod AItem.PointCount];

    // The normals are rotated +90 deg
    Dx := PointA.Y - PointB.Y;
    Dy := PointB.X - PointA.X;

    // Normalize them
    R := Sqrt(Sqr(Dx) + Sqr(Dy));
    if R > 0 then
    begin
      F := 1 / R;
      Normals[i].X := Dx * F;
      Normals[i].Y := Dy * F;
    end else
    begin
      Normals[i].X := 0;
      Normals[i].Y := 0;
    end;
    PointA := PointB;
  end;
end;

{ TpgRenderPath }

procedure TpgRenderPath.ArcTo(const Rx, Ry, Angle: double; LargeArc,
  Sweep: boolean; const X, Y: double);
var
  i, Count: integer;
  P1, Pi: TpgPoint;
  N1, N2, Delta: double;
  E: TpgEllipseR;
  Alpha, NSinNew, NCosNew: double;
  DSin, DCos, NSin, NCos: extended;
begin
  FPoly.CheckHasStart;
  P1 := FPoly.LastPoint^;

  // Checks
  if (PixelScale = 0) or ((ExpectedStrokeWidth = 0) and (pgDistanceSqr(P1, pgPoint(X, Y)) < FDistTolSqr)) then
  begin
    LineTo(X, Y);
    exit;
  end;

  // Calculate ellipse center and generalized angles
  pgEllipseFromTwoPoints(P1, pgPoint(X, Y), Angle, Rx, Ry, LargeArc, Sweep, E, N1, N2);

  // Minimum angle based on Alpha = sqrt(8 * Eps / R)
  // with Eps being 0.125 / Pixelscale
  // R being max(Rx, Ry) + strokewidth/2
  Alpha := sqrt(1 / (PixelScale * (pgMax(Rx, Ry) + 0.5 * ExpectedStrokeWidth)));

  // Segments to use
  Count := pgMax(1, pgCeil(abs(N2 - N1) / Alpha));

  // Just one segment: we can lineto it directly
  if Count = 1 then
  begin
    LineTo(X, Y);
    exit;
  end;

  // Delta angle
  Delta := (N2 - N1) / Count;
  // Delta trigonio
  pgSinCos(Delta, DSin, DCos);
  // N trigonio
  pgSinCos(N1, NSin, NCos);

  for i := 1 to Count - 1 do
  begin
    // Calculate new N trigonios
    NCosNew :=  DCos * NCos - DSin * NSin;
    NSinNew :=  DSin * NCos + DCos * NSin;
    // calculate point to add
    Pi := pgEllipsePointSinCos(E, NSinNew, NCosNew);
    LineTo(Pi.X, Pi.Y);

    NCos := NCosNew;
    NSin := NSinNew;
  end;

  // Finally line to last point
  LineTo(X, Y);
end;

procedure TpgRenderPath.ArcToStraight(const Cx, Cy, Rx, Ry, N1,
  N2: double; const X, Y: double);
var
  i, Count: integer;
  P1: TpgPoint;
  Delta, PiX, PiY: double;
  Alpha, NSinNew, NCosNew: double;
  DSin, DCos, NSin, NCos: extended;
begin
  FPoly.CheckHasStart;
  P1 := FPoly.LastPoint^;

  // Checks
  if (PixelScale = 0) or ((ExpectedStrokeWidth = 0) and (pgDistanceSqr(P1, pgPoint(X, Y)) < FDistTolSqr)) then
  begin
    LineTo(X, Y);
    exit;
  end;

  // Minimum angle based on Alpha = sqrt(8 * Eps / R)
  // with Eps being 0.125 / Pixelscale
  // R being max(Rx, Ry) + strokewidth/2
  Alpha := sqrt(1 / (PixelScale * (pgMax(Rx, Ry) + 0.5 * ExpectedStrokeWidth)));

  // Segments to use
  Count := pgMax(1, pgCeil(abs(N2 - N1) / Alpha));

  // Just one segment: we can lineto it directly
  if Count = 1 then
  begin
    LineTo(X, Y);
    exit;
  end;

  // Delta angle
  Delta := (N2 - N1) / Count;
  // Delta trigonio
  pgSinCos(Delta, DSin, DCos);
  // N trigonio
  pgSinCos(N1, NSin, NCos);

  for i := 1 to Count - 1 do
  begin
    // Calculate new N trigonios
    NCosNew :=  DCos * NCos - DSin * NSin;
    NSinNew :=  DSin * NCos + DCos * NSin;
    // calculate point to add
    PiX := Cx + Rx * NCosNew;
    PiY := Cy + Ry * NSinNew;
    LineTo(PiX, PiY);

    NCos := NCosNew;
    NSin := NSinNew;
  end;

  // Finally line to last point
  LineTo(X, Y);
end;

procedure TpgRenderPath.Assign(Source: TPersistent);
begin
  if Source is TpgRenderPath then
    FPoly.Assign(TpgRenderPath(Source).FPoly)
  else
    inherited;
end;

function TpgRenderPath.BoundingBox: TpgBox;
begin
  Result := FPoly.BoundingBox;
end;

procedure TpgRenderPath.CleanupMemory;
begin
  FPoly.CleanupMemory;
end;

procedure TpgRenderPath.Clear;
begin
  FPoly.Clear;
end;

procedure TpgRenderPath.ClosePath;
begin
  FPoly.ClosePath;
end;

procedure TpgRenderPath.ComputeTolerances;
var
  Eps: double;
begin
  if PixelScale = 0 then
    exit;

  // Equivalent to 1 pixel error
  Eps := 1 / PixelScale;

  // distance tolerances for beziers
  FDistTolSqr := sqr(0.5 * Eps);
  FDistTolTaxicab := 4 * Eps;

  // We want real cusps, best quality
  FCuspLimit := 0;

  // Do an estimation on required angle tolerance for beziers
  if ExpectedStrokeWidth > 0 then
  begin
    // defined by alpha ~ sqrt(8 * e / R), where
    //   e = 0.25 * eps ( we allow a quarter of a pixel error)
    //   R = strokewidth / 2
    FAngleTol := sqrt(4 * Eps / ExpectedStrokeWidth);
  end else
  begin
    // this indicates that no angle tolerance will be used
    FAngleTol := 0;
  end;
end;

procedure TpgRenderPath.CopySegment(Dest: TpgPath; const PosStart, PosEnd: TpgPathPosition);
var
  PosCur: TpgPathPosition;
  P: TpgPoint;
begin
  // Check
  if not assigned(Dest) then
    exit;

  // Start by adding a new path at PosStart
  PosCur := PosStart;
  P := PositionAlongPath(PosCur);
  Dest.MoveTo(P.X, P.Y);

  // Add intermediate segments
  while (PosCur.PathIndex <> PosEnd.PathIndex) or (PosCur.PointIndex <> PosEnd.PointIndex) do
  begin
    // Add current position's endpoint
    PosCur.Fraction := 1;
    P := PositionAlongPath(PosCur);
    Dest.LineTo(P.X, P.Y);
    // Go to next position
    inc(PosCur.PointIndex);
    // Last in path? If so, move to next
    if PosCur.PointIndex > FPoly[PosCur.PathIndex].EdgeCount - 1 then
    begin
      PosCur.PathIndex := (PosCur.PathIndex + 1) mod FPoly.Count;
      PosCur.PointIndex := 0;
      PosCur.Fraction := 0;
      P := PositionAlongPath(PosCur);
      if (FPoly.Count = 1) and FPoly[0].IsClosed then
        Dest.LineTo(P.X, P.Y)
      else
        Dest.MoveTo(P.X, P.Y);
    end;
  end;
  // Finish by adding last point
  if PosEnd.Fraction > 0 then
  begin
    PosCur := PosEnd;
    P := PositionAlongPath(PosCur);
    Dest.LineTo(P.X, P.Y);
  end;
end;

constructor TpgRenderPath.Create;
begin
  inherited Create;
  FPoly := TpgPolyPolygon.Create;
  PixelScale := 1.0;
  ComputeTolerances;
end;

procedure TpgRenderPath.CurveToCubic(const C1x, C1y, C2x, C2y, X, Y: double);
begin
  FPoly.CheckHasStart;
  // recursive subdivision
  CurveToCubicRecursive(FPoly.LastPoint^, pgPoint(C1x, C1y), pgPoint(C2x, C2y), pgPoint(X, Y), 0);
  LineTo(X, Y);
end;

procedure TpgRenderPath.CurveToCubicRecursive(const P1, P2, P3, P4: TpgPoint; Level: integer);
var
  T12, T23, T34, T123, T234, T1234, dP: TpgPoint;
  d2, d3, da1, da2, a23: single;
  C: integer;
begin
  // Check level
  if Level > cCurveRecursionLimit then
    exit;

  // Calculate midpoints
  T12   := pgMidPoint(P1, P2);
  T23   := pgMidPoint(P2, P3);
  T34   := pgMidPoint(P3, P4);
  T123  := pgMidPoint(T12, T23);
  T234  := pgMidPoint(T23, T34);
  T1234 := pgMidPoint(T123, T234);

  // Approximate with single straight line
  dP := pgDelta(P1, P4);
  d2 := abs((P2.X - P4.X) * dP.Y - (P2.Y - P4.Y) * dP.X);
  d3 := abs((P3.X - P4.X) * dP.Y - (P3.Y - P4.Y) * dP.X);

  C := 0;
  if d2 > cCurveCollinearityEps then
    inc(C, 2);
  if d3 > cCurveCollinearityEps then
    inc(C, 1);

  case C of
  0: // all collinear or P1 = P4
    if(abs(P1.X + P3.X - 2 * P2.X) +
       abs(P1.Y + P3.Y - 2 * P2.Y) +
       abs(P2.X + P4.X - 2 * P3.X) +
       abs(P2.Y + P4.Y - 2 * P3.Y)) <= FDistTolTaxicab then
      exit;

  1: // p1,p2,p4 are collinear, p3 is considerable
    if d3*d3 <= FDistTolSqr * (dP.X*dP.X + dP.Y*dP.Y) then
    begin

      // Do we use the angle condition?
      if FAngleTol = 0 then
      begin
        // No.. just add the points
        LineTo(T23.X, T23.Y);
        exit;
      end;

      // Angle Condition
      da1 := abs(pgArctan2(P4.Y - P3.Y, P4.X - P3.X) - pgArctan2(P3.Y - P2.Y, P3.X - P2.X));
      if da1 >= pi then
        da1 := 2*pi - da1;

      if da1 < FAngleTol then
      begin
        LineTo(P2.X, P2.Y);
        LineTo(P3.X, P3.Y);
        exit;
      end;

      // do we use the cusp limit? if so, compare the angle with it
      if (FCuspLimit <> 0) and (da1 > FCuspLimit) then
      begin
        LineTo(P3.X, P3.Y);
        exit;
      end;

    end;
  2: // p1,p3,p4 are collinear, p2 is considerable
    if d2*d2 <= FDistTolSqr * (dP.X*dP.X + dP.Y*dP.Y) then
    begin

      // Do we use the angle condition?
      if FAngleTol = 0 then
      begin
        // No.. just add the points
        LineTo(T23.X, T23.Y);
        exit;
      end;

      // Angle Condition
      da1 := abs(pgArctan2(P3.Y - P2.Y, P3.X - P2.X) - pgArctan2(P2.Y - P1.Y, P2.X - P1.X));
      if da1 >= pi then
        da1 := 2*pi - da1;

      if da1 < FAngleTol then
      begin
        LineTo(P2.X, P2.Y);
        LineTo(P3.X, P3.Y);
        exit;
      end;

      // do we use the cusp limit? if so, compare the angle with it
      if (FCuspLimit <> 0) and (da1 > FCuspLimit) then
      begin
        LineTo(P2.X, P2.Y);
        exit;
      end;

    end;
  3: // Regular care
    if (d2 + d3)*(d2 + d3) <= FDistTolSqr * (dP.X*dP.X + dP.Y*dP.Y) then
    begin

      // Do we use the angle condition?
      if FAngleTol = 0 then
      begin
        // No.. just add the points
        LineTo(T23.X, T23.Y);
        exit;
      end;

      // Angle Condition
      a23 := pgArctan2(P3.Y - P2.Y, P3.X - P2.X);
      da1 := abs(a23 - pgArctan2(P2.Y - P1.Y, P2.X - P1.X));
      da2 := abs(pgArctan2(P4.Y - P3.Y, P4.X - P3.X) - a23);
      if da1 >= pi then
        da1 := 2*pi - da1;
      if da2 >= pi then
        da2 := 2*pi - da2;

      if (da1 + da2) < FAngleTol then
      begin
        // we can stop the recursion
        LineTo(T23.X, T23.Y);
        exit;
      end;

      // do we use the cusp limit? if so, compare the angles with it
      if (FCuspLimit <> 0) then
      begin
        if da1 > FCuspLimit then
        begin
          LineTo(P2.X, P2.Y);
          exit;
        end;
        if da2 > FCuspLimit then
        begin
          LineTo(P3.X, P3.Y);
          exit;
        end;
      end;

    end;
  end;//case

  // If we arrive here, we must subdivide (recursive call)
  CurveToCubicRecursive(P1, T12, T123, T1234, Level + 1);
  CurveToCubicRecursive(T1234, T234, T34, P4, Level + 1);
end;

procedure TpgRenderPath.CurveToQuadratic(const Cx, Cy, X, Y: double);
begin
  FPoly.CheckHasStart;
  // recursive subdivision
  CurveToQuadraticRecursive(FPoly.LastPoint^, pgPoint(Cx, Cy), pgPoint(X, Y), 0);
  LineTo(X, Y);
end;

procedure TpgRenderPath.CurveToQuadraticInc(const Cx, Cy, X, Y: double);
begin
  FPoly.CheckHasStart;
  // incremental
  CurveToQuadraticIncremental(FPoly.LastPoint^, pgPoint(Cx, Cy), pgPoint(X, Y));
  LineTo(X, Y);
end;

procedure TpgRenderPath.CurveToQuadraticIncremental(const P1, P2,
  P3: TpgPoint);
begin
  // to do
  raise Exception.Create('not implemented');
end;

procedure TpgRenderPath.CurveToQuadraticRecursive(const P1, P2, P3: TpgPoint; Level: integer);
var
  T12, T23, T123, dP: TpgPoint;
  d, da: single;
begin
  // Check level
  if Level > cCurveRecursionLimit then
    exit;

  // Calculate midpoints
  T12  := pgMidPoint(P1, P2);
  T23  := pgMidPoint(P2, P3);
  T123 := pgMidPoint(T12, T23);

  // Approximate with single straight line
  dP := pgDelta(P1, P3);
  d := abs((P2.X - P3.X) * dP.Y - (P2.Y - P3.Y) * dP.X);

  if d > cCurveCollinearityEps then
  begin

    // Regular care
    if d * d <= FDistTolSqr * (dP.X*dP.X + dP.Y*dP.Y) then
    begin

      // Do we use the angle condition?
      if FAngleTol = 0 then
      begin
        // No.. just add the points
        LineTo(T123.X, T123.Y);
        exit;
      end;

      // Angle and Cusp Condition
      da := abs(pgArctan2(P3.Y - P2.Y, P3.X - P2.X) - pgArctan2(P2.Y - P1.Y, P2.X - P1.X));
      if da >= pi then
        da := 2*pi - da;

      if da < FAngleTol then
      begin
        // we can stop the recursion
        LineTo(T123.X, T123.Y);
        exit;
      end;

    end;
  end else
  begin

    if(abs(P1.X + P3.X - 2 * P2.X) +
       abs(P1.Y + P3.Y - 2 * P2.Y)) <= FDistTolTaxicab then
    begin
      LineTo(T123.X, T123.Y);
      exit;
    end;

  end;

  // If we arrive here, we must subdivide (recursive call)
  CurveToQuadraticRecursive(P1, T12, T123, Level + 1);
  CurveToQuadraticRecursive(T123, T23, P3, Level + 1);
end;

destructor TpgRenderPath.Destroy;
begin
  FreeAndNil(FPoly);
  inherited;
end;

function TpgRenderPath.GetAsPolyPolygon: TpgPolyPolygon;
begin
  Result := FPoly;
end;

function TpgRenderPath.GetBreakupLength: double;
begin
  Result := FPoly.BreakupLength;
end;

function TpgRenderPath.GetPathLength: double;
begin
  Result := FPoly.GetPathLength;
end;

function TpgRenderPath.IsEmpty: boolean;
begin
  Result := FPoly.IsEmpty;
end;

procedure TpgRenderPath.LineTo(const X, Y: double);
begin
  FPoly.LineTo(pgPoint(X, Y), True);
end;

procedure TpgRenderPath.MoveTo(const X, Y: double);
begin
  FPoly.MoveTo(pgPoint(X, Y));
end;

function TpgRenderPath.PointInPath(const X, Y: double; FillRule: TpgFillRule): boolean;
var
  i, Winding: integer;
begin
  Result := False;
  Winding := 0;
  for i := 0 to FPoly.Count - 1 do
    Winding := Winding + pgPointInPolygon(FPoly[i].FirstPoint, FPoly[i].PointCount, pgPoint(X, Y));
  case FillRule of
  frNonZero: Result := Winding <> 0;
  frEvenOdd: Result := odd(Winding);
  end;
end;

function TpgRenderPath.PointOnPath(const X, Y, Tolerance: double): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FPoly.Count - 1 do
  begin
    Result := pgPointOnPolygon(FPoly[i].FirstPoint, FPoly[i].PointCount,
      FPoly[i].IsClosed, pgPoint(X, Y), Tolerance);
    if Result then exit;
  end;
end;

function TpgRenderPath.PositionAlongPath(var APos: TpgPathPosition; Increment: double): TpgPoint;
var
  CurrentPath: integer;
  CurrentPoint: integer;
  Distance, Surplus: double;

  // local
  function IsValidPosition(const APos: TpgPathPosition): boolean;
  begin
    if (APos.PathIndex >= 0) and (APos.PathIndex < FPoly.Count) then
    begin
      CurrentPath  := APos.PathIndex;
      CurrentPoint := APos.PointIndex;
      // We allow points at the start of the segment, or as first point if there
      // are no segments
      if (CurrentPoint >= 0) and (CurrentPoint < pgMax(1, FPoly[CurrentPath].EdgeCount)) then
      begin
        Result := (APos.Fraction >= 0) and (APos.Fraction <= 1)
      end else
        Result := False;
    end else
      Result := False;
  end;

  // local
  function NextValidPosition: boolean;
  var
    NewPos: TpgPathPosition;
  begin
    inc(CurrentPoint);
    if CurrentPoint < FPoly[CurrentPath].EdgeCount then
    begin
      Result := True;
      APos.PointIndex := CurrentPoint;
      exit;
    end;
    NewPos := APos;
    inc(NewPos.PathIndex);
    // wraparound
    NewPos.PathIndex := NewPos.PathIndex mod FPoly.Count;
    NewPos.PointIndex := 0;
    Result := IsValidPosition(NewPos);
    if Result then
      APos := NewPos;
  end;

  // local
  function DistanceOfSegment: double;
  begin
    with FPoly[CurrentPath] do
    begin
      if PointCount > 1 then
        Result := pgDistance(
          Points[CurrentPoint]^,
          Points[(CurrentPoint + 1) mod PointCount]^)
      else
        Result := 0;
    end;
  end;

  // local
  function GetPathPosition: TpgPoint;
  var
    P1, P2: TpgPoint;
  begin
    // Return the position
    if (APos.PathIndex >= 0) and (APos.PathIndex < FPoly.Count) then
    begin
      CurrentPath  := APos.PathIndex;
      CurrentPoint := APos.PointIndex;
      if (CurrentPoint >= 0) and (CurrentPoint < FPoly[CurrentPath].EdgeCount) then
      begin

        // start and end points
        with FPoly[CurrentPath] do
        begin
          if APos.Fraction = 0 then
            // First point
            Result := Points[CurrentPoint]^
          else
          begin
            if APos.Fraction = 1 then
              // Last point
              Result := Points[(CurrentPoint + 1) mod PointCount]^
            else
            begin
              // interpolate
              P1 := Points[CurrentPoint]^;
              P2 := Points[(CurrentPoint + 1) mod PointCount]^;
              Result := pgInterpolatePoint(P1, P2, APos.Fraction);
            end;
          end;
        end;

      end else
      begin
        // is it the only point in open path?
        if CurrentPoint = 0 then
          Result := FPoly[CurrentPath].Points[0]^
        else
          Result := pgPoint(0, 0);
      end;
    end else
      Result := pgPoint(0, 0);
  end;

// main
begin
  if IsValidPosition(APos) then
  begin

    if Increment > 0 then
    begin

      // Forward travel through path with wraparound
      while Increment > 0 do
      begin
        Distance := DistanceOfSegment;
        Surplus  := Distance * (1 - APos.Fraction);
        if Increment > Surplus then
        begin
          Increment := Increment - Surplus;
          if NextValidPosition then
          begin
            APos.Fraction := 0;
          end else
          begin
            // At the end, just truncate
            APos.Fraction := 1;
            Increment := 0;
          end;
        end else
        begin
          APos.Fraction := APos.Fraction + Increment / Distance;
          Increment := 0;
        end;
      end;

    end else
      if Increment < 0 then

        raise Exception.Create(sIncrementCannotBeNegative);

  end;

  // Final position on the path, or directly if Increment = 0
  Result := GetPathPosition;
end;

procedure TpgRenderPath.ScaleUniform(const S: double);
begin
  FPoly.ScaleUniform(S);
end;

procedure TpgRenderPath.SetBreakupLength(const Value: double);
begin
  FPoly.BreakupLength := Value;
end;

function TpgRenderPath.SizeInBytes: integer;
begin
  Result := FPoly.Capacity * SizeOf(TpgPoint);
end;

end.
