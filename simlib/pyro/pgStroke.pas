{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Render<p>

  <b>Description:</b><p>
  Stroking of paths is the process where a path is converted into a stroked line
  with a certain thickness.

  Creation Date:
  24Dec2004

  Modifications:
  05Oct2005: Added stroke dash array
  11Oct2005: Added invalid segment detection, linecap

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2004 - 2006 SimDesign BV
}
unit pgStroke;

{$i simdesign.inc}

interface

uses

  Classes, pgPath, pgPolygon, Pyro, pgGeometry, Math,
  pgCanvas;

type

  PpgStrokeSegment = ^TPgStrokeSegment;
  PpgStrokeJoinRec = ^TpgStrokeJoinRec;
  TpgStrokeJoinRec = record
    Vertex: PpgPoint;
    LSegment: PpgStrokeSegment;
    RSegment: PpgStrokeSegment;
    Convex: boolean;
    JoinType: TpgStrokeJoin;
    Miter: TpgPoint;
  end;

  TpgStrokeSegment = record
    Normal: TpgPoint;
    Offset: TpgPoint;
    LJoin: PpgStrokeJoinRec;
    RJoin: PpgStrokeJoinRec;
    IsValid: boolean;
  end;

  // TpgStroker is a geometrical drawing class that creates a stroke path from
  // another path.
  TpgStroker = class(TPersistent)
  private
    FStrokeWidth: double;
    FLineCap: TpgLineCap;
    FLineJoin: TpgLineJoin;
    FMiterLimit: double;
    FOffset: double;
    FNormals: array of TpgPoint;
    FSegments: array of TpgStrokeSegment;
    FJoins: array of TpgStrokeJoinRec;
    FSegmentCount: integer;
    FDashOffset: double;
    FDashArray: TpgFloatList;
    FDashes: array of double;
  protected
    procedure PrepareStroking(AItem: TpgPolygonItem);
    procedure OutlineSegments(ADest: TpgPath; FirstSegmentIdx: integer);
    procedure OutlineClosedPath(AItem: TpgPolygonItem; ADest: TpgPath; const AOutlineWidth: double);
    procedure StrokeClosedPath(AItem: TpgPolygonItem; ADest: TpgPath);
    procedure StrokeOpenPath(AItem: TpgPolygonItem; ADest: TpgPath);
  public
    constructor Create; virtual;
    function CreateDashedPath(ASource: TpgPath): TpgPath;
    procedure Stroke(APath, ADest: TpgPath);
    procedure Outline(APath, ADest: TpgPath; const AOutlineWidth: double);
    property StrokeWidth: double read FStrokeWidth write FStrokeWidth;
    property LineCap: TpgLineCap read FLineCap write FLineCap;
    property LineJoin: TpgLineJoin read FLineJoin write FLineJoin;
    property MiterLimit: double read FMiterLimit write FMiterLimit;
    property DashArray: TpgFloatList read FDashArray write FDashArray;
    property DashOffset: double read FDashOffset write FDashOffset;
  end;

implementation

{ TpgStroke }

constructor TpgStroker.Create;
begin
  inherited Create;
  // Default miter limit
  FMiterLimit := 4;
  FLineJoin := ljMiter;
end;

function TpgStroker.CreateDashedPath(ASource: TpgPath): TpgPath;
var
  i, Count, Index: integer;
  DashLength, PathLength, PathCurPos, PathEndPos, Increment: double;
  PStart, PClose: TpgPathPosition;
  Last: boolean;
begin
  // create resulting path containing dashed segments
  Result := TpgPathClass(ASource.ClassType).Create;

  // Copy dash array locally
  Count := FDashArray.Count;
  SetLength(FDashes, Count);
  for i := 0 to Count - 1 do
    FDashes[i] := FDashArray[i];

  // First off double the length with a copy if odd
  if odd(Count) then begin
    SetLength(FDashes, Count * 2);
    for i := 0 to Count - 1 do begin
      FDashes[i + Count] := FDashes[i];
    end;
  end;

  // Check DashArray
  DashLength := 0;
  for i := 0 to length(FDashes) - 1 do begin
    FDashes[i] := abs(FDashes[i]);
    DashLength := DashLength + FDashes[i];
  end;
  if DashLength = 0 then exit;

  // Path length, and check for zero
  PathLength := ASource.PathLength;
  if PathLength = 0 then exit;

  // First move over DashOffset
  PClose := cPathStartPosition;
  PathCurPos := FDashOffset;
  while PathCurPos < 0 do
    PathCurPos := PathCurPos + PathLength;
  ASource.PositionAlongPath(PClose, FDashOffset);

  // We adjust here slightly to avoid wraparound of last point due to small
  // roundoff errors
  PathLength := PathLength - 1E-20;

  // Last position
  PathEndPos := PathLength + FDashOffset;

  // Breakup each path into dashed segments
  Index := 0;
  Last := False;
  repeat

    // Find correct increment (not too much)
    Increment := FDashes[Index];
    if PathCurPos + Increment >= PathEndPos then begin
      Increment := PathEndPos - PathCurPos;
      Last := True;
    end;

    // New position along the curve
    PStart := PClose;
    ASource.PositionAlongPath(PClose, Increment);

    // If we're in drawing mode (Index = even), then copy the segment
    if not odd(Index) then
      ASource.CopySegment(Result, PStart, PClose);

    // Update dash sequence and current position on the path
    Index := (Index + 1) mod length(FDashes);
    PathCurPos := PathCurPos + Increment;
  until Last;
{  // if Index is odd here, it means the last segment can be connected to the first
  if odd(Index) then
    Result.Poly.ConnectLastToFirst;}
end;

procedure TpgStroker.Outline(APath, ADest: TpgPath; const AOutlineWidth: double);
var
  i: integer;
  PP: TpgPolyPolygon;
begin
  if not assigned(APath) or not assigned(ADest) then exit;

  FOffset := abs(AOutlineWidth);

  PP := APath.AsPolyPolygon;
  for i := 0 to PP.Count - 1 do
    if PP[i].IsClosed then
      OutlineClosedPath(PP[i], ADest, AOutlineWidth)
    else
      StrokeOpenPath(PP[i], ADest);
end;

procedure TpgStroker.OutlineClosedPath(AItem: TpgPolygonItem; ADest: TpgPath;
  const AOutlineWidth: double);
var
  i: integer;
  TempJ: PpgStrokeJoinRec;
  TempS: PpgStrokeSegment;
begin
  if AItem.PointCount < 2 then exit;

  // Preparation
  PrepareStroking(AItem);

  // We have same number of elements as the path's point count
  FSegmentCount := AItem.PointCount;
  SetLength(FSegments, Max(Length(FSegments), FSegmentCount));
  SetLength(FJoins,    Max(Length(FJoins),    FSegmentCount));

  // Build segment and join arrays
  for i := 0 to FSegmentCount - 1 do begin
    with FSegments[i] do begin
      // Set the normals
      Normal := FNormals[i];
      // Set join pointers
      LJoin := @FJoins[i];
      RJoin := @FJoins[(i + 1) mod FSegmentCount];
    end;
    with FJoins[i] do begin
      Vertex := AItem.Points[i];
      LSegment := @FSegments[(FSegmentCount + i - 1) mod FSegmentCount];
      RSegment := @FSegments[i];
      JoinType := jtUndetermined;
    end;
  end;

  if AOutlineWidth >= 0 then begin

    // Outline the segments outward
    OutlineSegments(ADest, 0);

  end else begin

    // Outline the segments inward
    for i := 0 to FSegmentCount - 1 do with FSegments[i] do begin
      // Flip the normals
      Normal := pgFlipVector(Normal);
      with FSegments[i] do begin
        // Swap segment joins
        TempJ := LJoin;
        LJoin := RJoin;
        RJoin := TempJ;
      end;
      with FJoins[i] do begin
        // Swap join segments
        TempS := LSegment;
        LSegment := RSegment;
        RSegment := TempS;
        JoinType := jtUndetermined;
      end;
    end;

    OutlineSegments(ADest, FSegmentCount - 1);

  end;
end;

procedure TpgStroker.OutlineSegments(ADest: TpgPath; FirstSegmentIdx: integer);
var
  i: integer;
  Dx, Dy, R2, F, MiterLength: double;
  P, P1, P2: TpgPoint;
  IsFirstPoint: boolean;
  FirstJoin, Join: PpgStrokeJoinRec;
  // Local
  procedure AddPoint(const P: TpgPoint);
  begin
    if IsFirstPoint then begin
      ADest.MoveTo(P.X, P.Y);
      IsFirstPoint := False;
    end else
      ADest.LineTo(P.X, P.Y);
  end;
begin
  for i := 0 to FSegmentCount - 1 do with FJoins[i] do begin

    // Step 1: determine convex/concave
    Convex := pgCrossProduct(RSegment.Normal, LSegment.Normal) >= 0;

    // Step 2: determine join type
    if JoinType = jtUndetermined then begin
      if Convex then begin
        JoinType := TpgStrokeJoin(integer(FLineJoin) + 1);
      end else
        JoinType := jtMiter;
    end;

    // Step 3: calculate and check miters
    if JoinType = jtMiter then begin
      // Calculate miter direction
      Dx := LSegment.Normal.X + RSegment.Normal.X;
      Dy := LSegment.Normal.Y + RSegment.Normal.Y;
      R2 := Max(1E-30, sqr(Dx) + sqr(Dy));
      F := 2 / R2;
      Miter.X := F * Dx * FOffset;
      Miter.Y := F * Dy * FOffset;

      // Check miter limit
      if Convex then begin
        MiterLength := 2 / sqrt(R2);
        if MiterLength > FMiterLimit then
          JoinType := jtBevel;
      end;
    end;
  end;

  // Step 4: detect degenerate segments
  for i := 0 to FSegmentCount - 1 do with FSegments[i] do begin

    // Calculate offset vector for this segment
    Offset.X := Normal.X * FOffset;
    Offset.Y := Normal.Y * FOffset;

    // Find the start and end point for each segment
    case LJoin.JoinType of
    jtMiter:
      P1 := pgAddPoint(LJoin.Vertex^, LJoin.Miter);
    jtBevel, jtRound:
      P1 := pgAddPoint(LJoin.Vertex^, Offset);
    end;
    case RJoin.JoinType of
    jtMiter:
      P2 := pgAddPoint(RJoin.Vertex^, RJoin.Miter);
    jtBevel, jtRound:
      P2 := pgAddPoint(RJoin.Vertex^, Offset);
    end;

    // if the offsetted segment is in the same direction as the vertex, it is
    // valid. If it is opposite, the length has become negative, which indicates
    // an outlining error.
    IsValid := pgDotProduct(pgDelta(P2, P1), pgDelta(RJoin.Vertex^, LJoin.Vertex^)) >= 0;
  end;

  // Step 5: set joins that border an invalid segments and are concave to jtBevel
  for i := 0 to FSegmentCount - 1 do with FJoins[i] do
    if (not Convex) and ((not LSegment.IsValid) or (not RSegment.IsValid)) then
       JoinType := jtBevel;

  // Step 6: draw the outline, by drawing the joins
  Join := FSegments[FirstSegmentIdx].LJoin;
  FirstJoin := Join;
  IsFirstPoint := True;
  repeat
    with Join^ do
      case JoinType of
      jtBevel:
        begin
          AddPoint(pgAddPoint(Vertex^, LSegment.Offset));
          AddPoint(pgAddPoint(Vertex^, RSegment.Offset));
        end;
      jtRound:
        begin
          AddPoint(pgAddPoint(Vertex^, LSegment.Offset));
          P := pgAddPoint(Vertex^, RSegment.Offset);
          ADest.ArcTo(FOffset, FOffset, 0, False, False, P.X, P.Y);
        end;
      jtMiter:
        begin
          AddPoint(pgAddPoint(Vertex^, Miter));
        end;
      end;
    Join := Join.RSegment.RJoin;
  until Join = FirstJoin;

  if not IsFirstPoint then ADest.ClosePath;
end;

procedure TpgStroker.PrepareStroking(AItem: TpgPolygonItem);
begin
  if AItem.PointCount < 2 then exit;

  // Prepare
  SetLength(FNormals, AItem.PointCount);

  // Create normals
  pgBuildNormals(AItem.FirstPoint, @FNormals[0], AItem.PointCount);
end;

procedure TpgStroker.Stroke(APath, ADest: TpgPath);
var
  i: integer;
  Source: TpgPath;
  PP: TpgPolyPolygon;
begin
  if not assigned(APath) or not assigned(ADest) then exit;

  // prepare
  FOffset := FStrokeWidth * 0.5;
  if FOffset <= 0 then exit;

  Source := nil;
  try

    // dashed or not?
    if assigned(FDashArray) and (FDashArray.Count > 0) then begin

      Source := CreateDashedPath(APath);

    end else begin

      // No dashes: Use the original path directly
      Source := APath;

    end;

    // Simply add all paths as single stroked entities
    PP := Source.AsPolyPolygon;
    for i := 0 to PP.Count - 1 do
      if PP[i].IsClosed then
        StrokeClosedPath(PP[i], ADest)
      else
        StrokeOpenPath(PP[i], ADest);

  finally
    // Free the source if we created it for the dashes
    if Source <> APath then Source.Free;
  end;
end;

procedure TpgStroker.StrokeClosedPath(AItem: TpgPolygonItem; ADest: TpgPath);
var
  i: integer;
  TempJ: PpgStrokeJoinRec;
  TempS: PpgStrokeSegment;
begin
  if AItem.PointCount < 2 then exit;

  // Preparation
  PrepareStroking(AItem);

  // We have same number of elements as the path's point count
  FSegmentCount := AItem.PointCount;
  SetLength(FSegments, Max(Length(FSegments), FSegmentCount));
  SetLength(FJoins,    Max(Length(FJoins),    FSegmentCount));

  // Build segment and join arrays
  for i := 0 to FSegmentCount - 1 do begin
    with FSegments[i] do begin
      // Set the normals
      Normal := FNormals[i];
      // Set join pointers
      LJoin := @FJoins[i];
      RJoin := @FJoins[(i + 1) mod FSegmentCount];
    end;
    with FJoins[i] do begin
      Vertex := AItem.Points[i];
      LSegment := @FSegments[(FSegmentCount + i - 1) mod FSegmentCount];
      RSegment := @FSegments[i];
      JoinType := jtUndetermined;
    end;
  end;

  // Outline the segments, outer curve
  OutlineSegments(ADest, 0);

  // Now flip the direction, and outline the other offset
  for i := 0 to FSegmentCount - 1 do with FSegments[i] do begin
    // Flip the normals
    Normal := pgFlipVector(Normal);
    with FSegments[i] do begin
      // Swap segment joins
      TempJ := LJoin;
      LJoin := RJoin;
      RJoin := TempJ;
    end;
    with FJoins[i] do begin
      // Swap join segments
      TempS := LSegment;
      LSegment := RSegment;
      RSegment := TempS;
      JoinType := jtUndetermined;
    end;
  end;

  // Outline the segments, inner curve
  OutlineSegments(ADest, FSegmentCount - 1);
end;

procedure TpgStroker.StrokeOpenPath(AItem: TpgPolygonItem; ADest: TpgPath);
var
  i, Index: integer;
  P, Q: TpgPoint;
begin
  if AItem.PointCount < 2 then begin
    if AItem.PointCount = 1 then begin
      // Specific drawing of "dots"
      P := AItem.Points[0]^;
      case FLineCap of
      //lcButt: nothing to do for lcButt
      lcRound:
        begin
          // Draw a circle
          ADest.Ellipse(P.X, P.Y, FOffset, FOffset);
        end;
      lcSquare:
        begin
          // Draw a square, but find directionality first
          // to do: directionality from parent pathset
          Q := pgOffsetPoint(P,  FOffset,  FOffset);
          ADest.MoveTo(Q.X, Q.Y);
          Q := pgOffsetPoint(P, -FOffset,  FOffset);
          ADest.LineTo(Q.X, Q.Y);
          Q := pgOffsetPoint(P, -FOffset, -FOffset);
          ADest.LineTo(Q.X, Q.Y);
          Q := pgOffsetPoint(P,  FOffset, -FOffset);
          ADest.LineTo(Q.X, Q.Y);
          ADest.ClosePath;
        end;
      end;
    end;
    exit;
  end;

  // Preparation
  PrepareStroking(AItem);

  // Build segment and join arrays
  FSegmentCount := 2 * (AItem.PointCount - 1);
  if FLineCap = lcSquare then inc(FSegmentCount, 2);
  SetLength(FSegments, Max(Length(FSegments), FSegmentCount));
  SetLength(FJoins,    Max(Length(FJoins),    FSegmentCount));
  for i := 0 to FSegmentCount - 1 do
    FJoins[i].JoinType := jtUndetermined;

  Index := 0;
  // Forward path
  for i := 0 to AItem.PointCount - 2 do begin
    FJoins[Index].Vertex := AItem.Points[i];
    FSegments[Index].Normal := FNormals[i];
    inc(Index);
  end;

  // ending linecap
  case FLineCap of
  lcButt:  FJoins[Index].JoinType := jtBevel;
  lcRound: FJoins[Index].JoinType := jtRound;
  lcSquare:
    begin
      // We add another segment and join
      FJoins[Index].JoinType := jtMiter;
      FJoins[Index].Vertex := AItem.Points[Index];
      FSegments[Index].Normal := pgPoint(FNormals[Index - 1].Y, -FNormals[Index - 1].X);
      inc(Index);
      FJoins[Index].JoinType := jtMiter;
    end;
  end;

  // Backward path
  for i := AItem.PointCount - 2 downto 0 do begin
    FJoins[Index].Vertex := AItem.Points[i + 1];
    FSegments[Index].Normal := pgFlipVector(FNormals[i]);
    inc(Index);
  end;

  // starting linecap
  case FLineCap of
  lcButt:  FJoins[0].JoinType := jtBevel;
  lcRound: FJoins[0].JoinType := jtRound;
  lcSquare:
    begin
      // We add another segment and join
      FJoins[Index].JoinType := jtMiter;
      FJoins[Index].Vertex := AItem.Points[0];
      FSegments[Index].Normal := pgPoint(-FNormals[0].Y, FNormals[0].X);
      FJoins[0].JoinType := jtMiter;
    end;
  end;

  // Hook up segments and joins
  for i := 0 to FSegmentCount - 1 do begin
    with FSegments[i] do begin
      LJoin := @FJoins[i];
      RJoin := @FJoins[(i + 1) mod FSegmentCount];
    end;
    with FJoins[i] do begin
      LSegment := @FSegments[(i + FSegmentCount - 1) mod FSegmentCount];
      RSegment := @FSegments[i];
    end;
  end;

  // Now we can outline the segments
  OutlineSegments(ADest, 0);
end;

end.
