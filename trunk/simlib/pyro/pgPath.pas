{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Core<p>

  <b>Description:</b><p>
  Generic path type, functions as base for descendant path types.

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgPath;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, pgPolygon, Pyro;

type

  TpgPathPosition = record
    PathIndex: integer;  // index into the current path
    PointIndex: integer; // index into the current point
    Fraction: double;    // fraction of the length on current segment (0..1)
  end;

  // Generic path object, which provides a framework that should be implemented
  // in descendant classes. These classes *must* implement all abstract methods,
  // and *may* override some of the non-abstract ones.
  TpgPath = class(TPersistent)
  private
    FPixelScale: double;
    FExpectedStrokeWidth: double;
    procedure SetExpectedStrokeWidth(const Value: double);
    procedure SetPixelScale(const Value: double);
  protected
    function GetPathLength: double; virtual;
    procedure ComputeTolerances; virtual;
    function GetBreakupLength: double; virtual;
    procedure SetBreakupLength(const Value: double); virtual;
    function GetAsPolyPolygon: TpgPolyPolygon; virtual;
  public
    constructor Create; virtual;
    // Clears the collection of paths and current path
    procedure Clear; virtual; abstract;
    // Find enclosing bounding box around all the points
    function BoundingBox: TpgBox; virtual; abstract;
    // Cleans any excess memory used in the paths. Use this procedure after
    // a path is completed, to save memory resources
    procedure CleanupMemory; virtual;
    // Close the current path
    procedure ClosePath; virtual; abstract;
    // Draw a complete ellipse,
    // The ellipse is drawn around center Cx Cy with radii Rx and Ry, and always has
    // angle Theta = 0. If Result = false, parameters were illegal.
    function Ellipse(const Cx, Cy, Rx, Ry: double): boolean; virtual;
    // Draw a rectangle, from top/left X Y and with Width, Height size. If Rx and Ry
    // contain values, the rectangle is rounded. If Result = false, parameters were
    // illegal.
    function Rectangle(const X, Y, Width, Height, Rx, Ry: double): boolean; virtual;
    // Move to new startpoint P
    procedure MoveTo(const X, Y: double); virtual; abstract;
    // Line to new point P
    procedure LineTo(const X, Y: double); virtual; abstract;
    // Curve to new point X,Y using cubic bezier segment, with control points C1x,C1y
    // and C2x,C2y.
    procedure CurveToCubic(const C1x, C1y, C2x, C2y, X, Y: double); virtual; abstract;
    // Curve to new point X,Y using quadratic bezier segment with control point Cx,Cy.
    procedure CurveToQuadratic(const Cx, Cy, X, Y: double); virtual; abstract;
    // Curve to using quadratic bezier, but preferring incremental method (for fonts only!)
    procedure CurveToQuadraticInc(const Cx, Cy, X, Y: double); virtual;
    // Arc to the new point X,Y from the current point, using the arc settings like
    // in SVG.
    procedure ArcTo(const Rx, Ry, Angle: double; LargeArc, Sweep: boolean; const X, Y: double); virtual; abstract;
    // Draws a straight arc (Theta = 0) from generalized angle N1 to N2, around
    // center Cx,Cy to point X,Y.
    procedure ArcToStraight(const Cx, Cy, Rx, Ry, N1, N2, X, Y: double); virtual; abstract;
    // IsEmpty returns true if there are no paths in this pathset, or all paths
    // have zero pointcount.
    function IsEmpty: boolean; virtual; abstract;
    // Get the position along the path (for use with text-on-path or dash arrays),
    // the variable APos is updated, and the point on the path is returned. Increment
    // contains the length which must be travelled. If no valid position can be
    // returned, either begin or endpoint will be returned. If Increment = 0,
    // the point at parametric APos will be returned. If the increment plus current
    // position is larger than the pathlenght, a wraparound will be done. Increment
    // must always be 0 or positive!
    function PositionAlongPath(var APos: TpgPathPosition; Increment: double = 0): TpgPoint; virtual; abstract;
    // Copy a part of the path to another path, starting at PosStart and ending
    // at PosEnd, with wraparound.
    procedure CopySegment(Dest: TpgPath; const PosStart, PosEnd: TpgPathPosition); virtual;
    function SizeInBytes: integer; virtual; abstract;
    procedure ScaleUniform(const S: double); virtual; abstract;
    // PointInPath returns true if the point X,Y is contained in the current path.
    function PointInPath(const X, Y: double; FillRule: TpgFillRule): boolean; virtual; abstract;
    // PointOnPath returns true if the point X,Y is located on the current path. Tolerance
    // specifies the maximum distance of X,Y from the path.
    function PointOnPath(const X, Y, Tolerance: double): boolean; virtual; abstract;
    // PixelScale is the number of pixels that 1 unit will cover.
    property PixelScale: double read FPixelScale write SetPixelScale;
    // If this path will be used later to stroke, set this value to the expected
    // stroke width. This will influence the way that bezier curves and arcs
    // are approximated with line segments, to ensure that the strokes will look
    // acceptably smooth. The stroke width is twice the offset width.
    property ExpectedStrokeWidth: double read FExpectedStrokeWidth write SetExpectedStrokeWidth;
    // Calculation of the length of the total path (not including moveto
    // operations). It is a summation of all line segment lengths, and takes into
    // account whether paths are closed or not.
    property PathLength: double read GetPathLength;
    // Set breakup length > 0 to cause all LineTo's to break up in segments of
    // at most BreakupLength. This is useful when later transforming the points
    // with non-linear transforms.
    property BreakupLength: double read GetBreakupLength write SetBreakupLength;
    property AsPolyPolygon: TpgPolyPolygon read GetAsPolyPolygon;
  end;
  TpgPathClass = class of TpgPath;

const

  cPathStartPosition: TpgPathPosition =
    (PathIndex: 0; PointIndex: 0; Fraction: 0);

resourcestring

  sCannotMoveToInsidePath       = 'MoveTo command not allowed inside path';
  sExpectMoveToFirst            = 'Expect MoveTo command first';
  sCannotClosePath              = 'Cannot close path (not enough points)';
  sIncrementCannotBeNegative    = 'Increment cannot be negative';
  sPathConversionNotImplemented = 'Path conversion not implemented';
  sNotImplemented               = 'Not Implemented';

implementation

{ TpgPath }

procedure TpgPath.CleanupMemory;
begin
// default does nothing
end;

procedure TpgPath.ComputeTolerances;
begin
// default does nothing
end;

procedure TpgPath.CopySegment(Dest: TpgPath; const PosStart,
  PosEnd: TpgPathPosition);
begin
  raise Exception.Create(sNotImplemented);
end;

constructor TpgPath.Create;
begin
  inherited Create;
  FPixelScale := 1.0;
  FExpectedStrokeWidth := 0.0;
  ComputeTolerances;
end;

procedure TpgPath.CurveToQuadraticInc(const Cx, Cy, X, Y: double);
begin
  // By default we just use the standard method
  CurveToQuadratic(Cx, Cy, X, Y);
end;

function TpgPath.Ellipse(const Cx, Cy, Rx, Ry: double): boolean;
const
  cHalfPi    = 0.5 * pi;
  cOneHalfPi = 1.5 * pi;
  cTwoPi     = 2   * pi;
begin
  Result := True;
  if (Rx < 0) or (Ry < 0) then begin
    Result := False;
    exit;
  end;
  MoveTo(Cx + Rx, Cy);
  ArcToStraight(Cx, Cy, Rx, Ry, 0, cHalfPi,          Cx, Cy + Ry);
  ArcToStraight(Cx, Cy, Rx, Ry, cHalfPi, pi,         Cx - Rx, Cy);
  ArcToStraight(Cx, Cy, Rx, Ry, pi, cOneHalfPi,      Cx, Cy - Ry);
  ArcToStraight(Cx, Cy, Rx, Ry, cOneHalfPi, cTwoPi,  Cx + Rx, Cy);
  ClosePath;
end;

function TpgPath.GetAsPolyPolygon: TpgPolyPolygon;
begin
  // By default, this is not implemented
  raise Exception.Create(sPathConversionNotImplemented);
end;

function TpgPath.GetBreakupLength: double;
begin
  // Default doesn't use this
  Result := 0;
end;

function TpgPath.GetPathLength: double;
begin
  // Default doesn't calculate
  Result := 0;
end;

function TpgPath.Rectangle(const X, Y, Width, Height, Rx,
  Ry: double): boolean;
var
  Rounded: boolean;
  CRx, CRy: double;
begin
  Result := True;
  if (Width = 0) or (Height = 0) then exit;
  if (Ry < 0) or (Rx < 0) then begin
    Result := False;
    exit;
  end;

  Rounded := (Rx > 0) or (Ry > 0);
  if Rounded then begin
    CRx := Rx;
    CRy := Ry;
    // Check rounded corner values
    if CRx = 0 then CRx := CRy;
    if CRy = 0 then CRy := CRx;
    CRx := pgMin(CRx, Width / 2);
    CRy := pgMin(CRy, Height / 2);
    // Create path with rounded corners
    MoveTo(X + Width - CRx, Y);
    ArcTo(CRx, CRy, 0, False, True, X + Width, Y + CRy);
    LineTo(X + Width, Y + Height - CRy);
    ArcTo(CRx, CRy, 0, False, True, X + Width - CRx, Y + Height);
    LineTo(X + CRx, Y + Height);
    ArcTo(CRx, CRy, 0, False, True, X, Y + Height - CRy);
    LineTo(X, Y + CRy);
    ArcTo(CRx, CRy, 0, False, True, X + CRx, Y);
    ClosePath;
  end else begin
    // Create square path
    MoveTo(X, Y);
    LineTo(X + Width, Y);
    LineTo(X + Width, Y + Height);
    LineTo(X, Y + Height);
    ClosePath;
  end;
end;

procedure TpgPath.SetBreakupLength(const Value: double);
begin
// default doesn't use this
end;

procedure TpgPath.SetExpectedStrokeWidth(const Value: double);
begin
  if FExpectedStrokeWidth <> Value then begin
    FExpectedStrokeWidth := Value;
    ComputeTolerances;
  end;
end;

procedure TpgPath.SetPixelScale(const Value: double);
begin
  if FPixelScale <> Value then begin
    FPixelScale := Value;
    ComputeTolerances;
  end;
end;

end.
