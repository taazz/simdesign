unit sdFeatureResolver;

interface

uses
  Classes, SysUtils, sdSortedLists, sdLaserPointDetection, sdPoints2D, sdPoints3D,
  sdLineFilters, sdLines2D, sdLineSimplifier, sdClusterFinder, sdInspectionData,
  sdGeomFit2D, sdGeomFit3D;

type

  TsdFeatureResolver = class;

  // Data line: contains data per image laser line
  TsdDataLine = class(TPersistent)
  private
    FOwner: TsdFeatureResolver;
    FPolygon: TsdVertex2DList;
    FWidth: integer;
    FIntensity: array of double;
    FCgY: array of double;
    FPoints: array of TsdLaserPoint;
    FLineId: integer;
  protected
    procedure FillBlanks;
    function GetPoints(AIndex: integer): TsdLaserPoint;
    procedure SetWidth(const Value: integer);
  public
    constructor Create(AOwner: TsdFeatureResolver);
    destructor Destroy; override;
    function CgYValueAtX(AX: double): double;
    class procedure GetCgXData(AFirst: PDouble; ACount: integer);
    procedure GetCgYData(AFirst: PDouble; ACount: integer);
    procedure GetIntensityData(AFirst: PDouble; ACount: integer);
    procedure GetPixelCountData(AFirst: PInteger; ACount: integer);
    procedure GetSlopeData(AFirst: PDouble; ACount: integer);
    property Width: integer read FWidth;
    property LineId: integer read FLineId;
    property Polygon: TsdVertex2DList read FPolygon;
  end;

  // List of Data lines
  TsdDataLineList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdDataLine;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    function ByLineId(ALineId: integer): TsdDataLine;
    property Items[Index: integer]: TsdDataLine read GetItems; default;
  end;

  TsdFeatureType = (
    ftUnknown,   // Unknown feature type
    ftLftEdge,   // Feature lies on the left of intensity boost
    ftRgtEdge,   // Feature lies on the right of intensity boost
    ftLftCorner, // Feature lies on the left of slope
    ftRgtCorner, // Feature lies on the right of slope
    ftCircle     // Feature is part of the circle fit
  );

  TsdFeaturePoint = class(TPersistent)
  private
    FOwner: TsdFeatureResolver;
    FFeatureType: TsdFeatureType;
    FXY: TsdPoint2D;
    FXYZ: TsdPoint3D;
    FLineId: integer;
    FWeight: double;
    FXYZIsValid: boolean;
    function GetXY: PsdPoint2D;
    function GetIsLftFeature: boolean;
    function GetXYZ: PsdPoint3D;
  public
    constructor Create(AOwner: TsdFeatureResolver);
    property FeatureType: TsdFeatureType read FFeatureType write FFeatureType;
    property XY: PsdPoint2D read GetXY;
    property XYZ: PsdPoint3D read GetXYZ;
    property LineId: integer read FLineId write FLineId;
    property Weight: double read FWeight write FWeight;
    // Lft oriented feature? (ftLftEdge, ftLftCorner)
    property IsLftFeature: boolean read GetIsLftFeature;
    property XYZIsValid: boolean read FXYZIsValid write FXYZIsValid;
  end;

  TsdFeaturePointList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdFeaturePoint;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    property Items[Index: integer]: TsdFeaturePoint read GetItems; default;
  end;

  // Resolve features in the image (2D with LineId information)
  TsdFeatureResolver = class(TComponent)
  private
    FLines: TsdDataLineList;
    FWidth: integer;
    FHeight: integer;
    FOnMessage: TStringEvent;
    FPoints: TsdFeaturePointList;
    FCurrentLine: TsdDataLine;
    FEdgeFilter: TsdEdgeFilter;
    FSimplifier: TsdLineSimplifier;
    FClusterFinder: TsdClusterFinder;
    FPlaneFit: TsdPlaneFit3D;
    FCircleFit: TsdCircleFit2D;
    FMedianLineSlope: double;
    FMeasurementType: TsdMeasurementType;
    // some data buffers, global to the object (Xvalues, Yvalues, Yvalues filtered)
    FXV, FYVr, FYVf: array of double;
    procedure SetWidth(const Value: integer);
  protected
    procedure AddCornerFeaturesFromPolygon(APolygon: TsdVertex2DList);
    procedure EdgeFeature(Sender: TObject; ADir: TsdEdgeDir; const AXValue, AWeight: double);
    procedure DoMessage(const AMessage: string);
    function GetAverageSlope(APolygon: TsdVertex2DList): double;
    procedure DetectCircleClusters;
    procedure RunEdgeFilters;
    procedure RunLineSimplificationFilters;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Run;
    // Determine circle center, normal and radius from the 3D feature points
    function DetermineCircle3D(var Center, Normal: TsdPoint3D; var Radius: double): boolean;
    procedure AddDataFromPolyLine(PL: TsdPolyLine);
    // Width of the image in pixels
    property Width: integer read FWidth write SetWidth;
    // Height of the image in pixels
    property Height: integer read FHeight write FHeight;
    // List of data lines (owned)
    property Lines: TsdDataLineList read FLines;
    // List of feature points (owned)
    property Points: TsdFeaturePointList read FPoints;
    property ClusterFinder: TsdClusterFinder read FClusterFinder;
    // set this property before running the resolver, it serves to detect the
    // corner points (based on slope difference with the median slope)
    property MedianLineSlope: double read FMedianLineSlope write FMedianLineSlope;
    property MeasurementType: TsdMeasurementType read FMeasurementType write FMeasurementType;
    property OnMessage: TStringEvent read FOnMessage write FOnMessage;
  end;

const

  cFeatureTypeNames: array[TsdFeatureType] of string =
    ('Unknown', 'LftEdge', 'RgtEdge', 'LftCorner', 'RgtCorner', 'Circle');


implementation

uses
  Math;

{ TsdDataLine }

function TsdDataLine.CgYValueAtX(AX: double): double;
var
  Idx: integer;
  Res: TsdPoint2D;
begin
  // Interpolate?
  Idx := trunc(AX - 0.5);
  // Values we cannot interpolate
  if Idx >= FWidth - 1 then
  begin
    Result := FCgY[FWidth - 1];
    exit;
  end;
  if Idx < 0 then
  begin
    Result := FCgY[0];
    exit;
  end;

  Interpolation2D(
    MakePoint2D(Idx + 0.5, FCgY[Idx]),
    MakePoint2D(Idx + 1.5, FCgY[Idx + 1]), AX - 0.5 - Idx, Res);

  Result := Res.Y;
end;

constructor TsdDataLine.Create(AOwner: TsdFeatureResolver);
begin
  inherited Create;
  FOwner := AOwner;
  FPolygon := TsdVertex2DList.Create(True);
end;

destructor TsdDataLine.Destroy;
begin
  FreeAndNil(FPolygon);
  inherited;
end;

procedure TsdDataLine.FillBlanks;
var
  i, Idx1, Idx2, Dx: integer;
  P: TsdLaserPoint;
begin
  // Interpolate the CgY values where they do not exist
  P := nil;
  Idx1 := 0;
  while Idx1 < FWidth do
  begin
    P := FPoints[Idx1];
    if assigned(P) then
      break;
    inc(Idx1);
  end;

  if not assigned(P) then
    exit;

  // Extrapolate left
  for i := 0 to Idx1 do
    FCgY[i] := P.Cg.Y;

  Idx2 := Idx1 + 1;

  while Idx2 < FWidth do
  begin
    P := FPoints[Idx2];
    if assigned(P) then
    begin
      FCgY[Idx2] := P.Cg.Y;

      // Interpolate
      if Idx1 < Idx2 - 1 then
      begin
        Dx := Idx2 - Idx1;
        for i :=  1 to Dx - 1 do
          FCgY[Idx1 + i] := FCgY[Idx1] + (FCgY[Idx2] - FCgY[Idx1]) * (i / Dx);

      end;
      Idx1 := Idx2;
    end;
    inc(Idx2);
  end;

  // extrapolate right
  if Idx1 < FWidth - 1 then
    for i := Idx1 + 1 to FWidth - 1 do
      FCgY[i] := FCgY[Idx1];

end;

class procedure TsdDataLine.GetCgXData(AFirst: PDouble; ACount: integer);
var
  i: integer;
begin
  for i := 0 to ACount - 1 do
  begin
    AFirst^ := i + 0.5;
    inc(AFirst);
  end;
end;

procedure TsdDataLine.GetCgYData(AFirst: PDouble; ACount: integer);
begin
  if (ACount = 0) or (FWidth = 0) then
    exit;
  if ACount > FWidth then
    ACount := FWidth;

  Move(FCgY[0], AFirst^, ACount * SizeOf(double));
end;

procedure TsdDataLine.GetIntensityData(AFirst: PDouble; ACount: integer);
begin
  if (ACount = 0) or (FWidth = 0) then
    exit;
  if ACount > FWidth then
    ACount := FWidth;
  Move(FIntensity[0], AFirst^, ACount * SizeOf(double));
end;

procedure TsdDataLine.GetPixelCountData(AFirst: PInteger; ACount: integer);
var
  i: integer;
begin
  if (ACount = 0) or (FWidth = 0) then
    exit;
  if ACount > FWidth then
    ACount := FWidth;
  for i := 0 to ACount - 1 do
  begin
    if assigned(FPoints[i]) then
      AFirst^ := FPoints[i].Count
    else
      AFirst^ := 0;
    inc(AFirst);
  end;
end;

function TsdDataLine.GetPoints(AIndex: integer): TsdLaserPoint;
begin
  if (AIndex >= 0) and (AIndex < FWidth) then
    Result := FPoints[AIndex]
  else
    Result := nil;
end;

procedure TsdDataLine.GetSlopeData(AFirst: PDouble; ACount: integer);
var
  i, Delta: integer;
  P1, P2: TsdLaserPoint;
const
  cWidth = 5;
begin
  if (ACount = 0) or (FWidth = 0) then
    exit;
  if ACount > FWidth then
    ACount := FWidth;

  for i := 0 to ACount - 1 do
  begin
    Delta := 2 * cWidth;
    P1 := GetPoints(i - cWidth);
    if P1 = nil then
    begin
      P1 := GetPoints(i);
      dec(Delta, cWidth);
      if P1 = nil then
        dec(Delta, cWidth)
    end;
    P2 := GetPoints(i + cWidth);
    if P2 = nil then
    begin
      P2 := GetPoints(i);
      dec(Delta, cWidth);
      if P2 = nil then
        dec(Delta, cWidth)
    end;
    if Delta <= 0 then
      AFirst^ := 0
    else
      AFirst^ := (P2.Cg.Y - P1.Cg.Y) / Delta;

    inc(AFirst);
  end;
end;

procedure TsdDataLine.SetWidth(const Value: integer);
begin
  FWidth := Value;
  // Set dynamic arrays
  SetLength(FPoints, FWidth);
  SetLength(FIntensity, FWidth);
  SetLength(FCgY, FWidth);
end;

{ TsdDataLineList }

function TsdDataLineList.ByLineId(ALineId: integer): TsdDataLine;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].LineId = ALineId then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TsdDataLineList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TsdDataLine(Item1).LineId, TsdDataLine(Item2).LineId);
end;

function TsdDataLineList.GetItems(Index: integer): TsdDataLine;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TsdFeaturePoint }

constructor TsdFeaturePoint.Create(AOwner: TsdFeatureResolver);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TsdFeaturePoint.GetXY: PsdPoint2D;
begin
  Result := @FXY;
end;

function TsdFeaturePoint.GetXYZ: PsdPoint3D;
begin
  Result := @FXYZ;
end;

function TsdFeaturePoint.GetIsLftFeature: boolean;
begin
  Result := FFeatureType in [ftLftEdge, ftLftCorner];
end;

{ TsdFeaturePointList }

function TsdFeaturePointList.DoCompare(Item1, Item2: TObject): integer;
var
  FP1, FP2: TsdFeaturePoint;
begin
  FP1 := TsdFeaturePoint(Item1);
  FP2 := TsdFeaturePoint(Item2);
  Result := -CompareDouble(FP1.FWeight, FP2.FWeight);
end;

function TsdFeaturePointList.GetItems(Index: integer): TsdFeaturePoint;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TsdFeatureResolver }

procedure TsdFeatureResolver.AddCornerFeaturesFromPolygon(APolygon: TsdVertex2DList);
// Segments are classified as
// - Horizontal: close to median line slope
// - Downwards: going from left to right, slope positive (Y grows downward in image)
// - Upwards: going from left to right, slope negative
// Vertex weight based on
// - Length of slope dn segments after, slope up segments before
// - closeness to optimal slope
type
  TsdSegmentType = (
    stHorizontal,
    stSlopeDn,
    stSlopeUp
  );
  
{var
  i: integer;
  Slope: double;
  SegmentsTypes: array of TsdSegmentType;}
begin

{  Slope := GetAverageSlope(APolygon);
  // from left to right
  for i := 0 to APolygon.Count - 1 do
  begin

  end;}
end;

procedure TsdFeatureResolver.AddDataFromPolyLine(PL: TsdPolyLine);
var
  i, j, x: integer;
  LL: TsdLaserLine;
  DL: TsdDataLine;
  LP: TsdLaserPoint;
begin
  // Create and add a new dataline
  DL := TsdDataLine.Create(Self);
  DL.FLineId := PL.LineId;
  DL.SetWidth(FWidth);
  FLines.Add(DL);

  for i := 0 to PL.Lines.Count - 1 do
  begin
    LL := PL.Lines[i];
    for j := 0 to LL.Points.Count - 1 do
    begin
      LP := LL.Points[j];
      x := LP.X;
      if (X < 0) or (X >= Width) then
        continue;

      // Copy the data of this laser point
      DL.FPoints[x] := LP;
      DL.FIntensity[x] := LP.Intensity;

    end;
  end;

  // Create helper lines for parts where there are no lines
  DL.FillBlanks;
end;

procedure TsdFeatureResolver.Clear;
begin
  FLines.Clear;
  FPoints.Clear;
  FWidth := 0;
end;

constructor TsdFeatureResolver.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TsdDataLineList.Create(True);
  FPoints := TsdFeaturePointList.Create(True);
  FEdgeFilter := TsdEdgeFilter.Create(Self);
  FSimplifier := TsdLineSimplifier.Create(Self);
  FClusterFinder := TsdClusterFinder.Create(Self);
  FPlaneFit := TsdPlaneFit3D.Create(Self);
  FCircleFit := TsdCircleFit2D.Create(Self);
end;

destructor TsdFeatureResolver.Destroy;
begin
  FreeAndNil(FLines);
  FreeAndNil(FPoints);
  inherited;
end;

procedure TsdFeatureResolver.DetectCircleClusters;
var
  i, j, k: integer;
  procedure AddCircleTriplet(A, B, C: TsdFeaturePoint);
  var
    i, j: integer;
    Tmp, R2, Den, W: double;
    D2: array[0..2] of double;
    V: array[0..2] of double;
    Ctr: TsdPoint2D;
    Res: boolean;
    Cluster: TsdCluster;
  begin
    // Distances A->B, A->C and B->C
    D2[0] := SquaredDist2D(A.XY^, B.XY^);
    D2[1] := SquaredDist2D(A.XY^, C.XY^);
    D2[2] := SquaredDist2D(B.XY^, C.XY^);

    // find smallest two distances
    for i := 0 to 1 do
      for j := i + 1 to 2 do
        if D2[j] < D2[i] then
        begin
          Tmp := D2[i];
          D2[i] := D2[j];
          D2[j] := Tmp;
        end;

    // Find the circle through these points
    Res := CircleFrom3PointsR2(A.XY^, B.XY^, C.XY^, Ctr, R2, Den);
    if Res then
    begin
      // Determine if points A,B,C are on the correct side
      Res := (A.XY.X > Ctr.X) xor A.IsLftFeature;
      Res := Res and ((B.XY.X > Ctr.X) xor B.IsLftFeature);
      Res := Res and ((C.XY.X > Ctr.X) xor C.IsLftFeature);

      // All points are on correct side?
      if Res then
      begin

        // Values of the cluster point (Center XY and radius)
        V[0] := Ctr.X;
        V[1] := Ctr.Y;
        V[2] := Sqrt(R2);

        // Weight for the cluster point:
        // - the two shortest distances divided by R^2 (L0 * L1) / (R * R)
        // - multiplied by the sum of the weights of the feature points
        W := (sqrt(D2[0]*D2[1]) / R2) * (A.Weight + B.Weight + C.Weight);

        // Add the cluster point
        Cluster := FClusterFinder.AddPoint(V, W);
        if assigned(Cluster) then
        begin

          // Add A, B, C references to the cluster
          Cluster.References.AddUnique(A);
          Cluster.References.AddUnique(B);
          Cluster.References.AddUnique(C);
        end;

      end;
    end;
  end;
begin
  // Add triplets of feature points to the cluster finder

  // Setup cluster finder
  FClusterFinder.Clear;
  FClusterFinder.Dimensions := 3;

  // Original values for tolerance were 30, 30, 30 (which worked), though
  // 15, 15, 15 seems a bit better for separation from #2. Have to check the optimum here!
  // Lower numbers give more candidates obviously, so slower. But will also
  // yield a better initial estimate.

  // Limits for XY position are 0..Width and 0..Height
  FClusterFinder.SetLimits(0, 0, FWidth, 15);
  FClusterFinder.SetLimits(1, 0, FHeight, 15);

  // Radius search from 50 to 500 px, so diameters of 100-1000 pixels
  FClusterFinder.SetLimits(2, 50, 500, 15);

  // Run through feature points and add triplet points i, j, k
  for i := 0 to FPoints.Count - 3 do
    for j := i + 1 to FPoints.Count - 2 do
      for k := j + 1 to FPoints.Count - 1 do
      begin
        AddCircleTriplet(FPoints[i], FPoints[j], FPoints[k]);
      end;
end;

procedure TsdFeatureResolver.DoMessage(const AMessage: string);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, AMessage);
end;

procedure TsdFeatureResolver.EdgeFeature(Sender: TObject; ADir: TsdEdgeDir; const AXValue, AWeight: double);
var
  FP: TsdFeaturePoint;
begin
  FP := TsdFeaturePoint.Create(Self);
  FP.XY.X := AXValue;
  FP.XY.Y := FCurrentLine.CgYValueAtX(AXValue);
  FP.FWeight := AWeight;

  // When using the intensity points also for countersink, their edge direction
  // must be inverted
  if FMeasurementType = mtCountersink then
  begin
    case ADir of
    edUp:   FP.FeatureType := ftRgtEdge;
    edDown: FP.FeatureType := ftLftEdge;
    end;
  end else
  begin
    // Other algorithms, i.e. pin
    case ADir of
    edUp:   FP.FeatureType := ftLftEdge;
    edDown: FP.FeatureType := ftRgtEdge;
    end;
  end;
  
  FP.FLineId := FCurrentLine.FLineId;
  FPoints.Add(FP);
end;

function TsdFeatureResolver.GetAverageSlope(APolygon: TsdVertex2DList): double;
var
  Dx: double;
  V1, V2: TsdVertex2D;
begin
  Result := 0;
  if APolygon.Count < 2 then
    exit;
  V1 := APolygon.Items[0];
  V2 := APolygon.Items[APolygon.Count - 1];
  Dx := V2.X - V1.X;
  if abs(Dx) < 1E-12 then
    exit;
  Result := (V2.Y - V1.Y) / Dx;
end;

procedure TsdFeatureResolver.RunEdgeFilters;
var
  i: integer;
begin
  // Run edge filters
  for i := 0 to FLines.Count - 1 do
  begin
    FCurrentLine := FLines[i];

    // Get the intensity data
    FCurrentLine.GetIntensityData(@FYVr[0], FWidth);

    // Average filter (with window of 31 wide)
    sdWalkingAverageArrayDouble(@FYVr[0], @FYVf[0], FWidth, 15, 31);

    // Set filter's data and run it
    FEdgeFilter.SetXYData(@FXV[0], @FYVf[0], FWidth);
    FEdgeFilter.BaseLimit := 300;
    FEdgeFilter.DeltaLimit := 300;
    FEdgeFilter.Run;
  end;

end;

procedure TsdFeatureResolver.RunLineSimplificationFilters;
var
  i: integer;
begin
  for i := 0 to FLines.Count - 1 do
  begin
    FCurrentLine := FLines[i];

    // Line simplifier
    FCurrentLine.GetCgYData(@FYVr[0], FWidth);
    sdWalkingMedianArrayDouble(@FYVr[0], @FYVf[0], FWidth, 25, 51);

    // X and Y data for simplifier
    FSimplifier.Count := FWidth;
    FSimplifier.SetXValues(@FXV[0], FWidth);
    FSimplifier.SetYValues(@FYVf[0], FWidth);

    // Weights for simplifier

    {
    // Get the intensity data
    FCurrentLine.GetIntensityData(@YVr[0], FWidth);
    for j := 0 to FWidth - 1 do
      YVf[j] := Max(1, YVr[j]);

    FSimplifier.SetWeights(@YVf[0], FWidth);
    }

    FSimplifier.LineTolerance := 15;
    FSimplifier.YScaleUp := 20;
    FSimplifier.UseWeights := True;

    FSimplifier.Solve;

    // Copy polygon to dataline
    FCurrentLine.Polygon.CopyFrom(FSimplifier.Polygon);

    AddCornerFeaturesFromPolygon(FCurrentLine.Polygon);

  end;
end;

procedure TsdFeatureResolver.Run;
var
  i, Idx: integer;
  Cluster, CNext: TsdCluster;
  FP: TsdFeaturePoint;

begin
  if FWidth = 0 then
    exit;

  // Reserve mem for line data arrays
  SetLength(FXV, FWidth);
  SetLength(FYVr, FWidth);
  SetLength(FYVf, FWidth);

  // Make sure we get the feature points
  FEdgeFilter.OnFeature := EdgeFeature;

  // Get the Xcg values
  TsdDataLine.GetCgXData(@FXV[0], FWidth);

  // Run edge filters
  DoMessage('Running edge filters');
  RunEdgeFilters;

  // Run line simplification filters (not used yet, but are going to be used
  // for the countersink algo)
  DoMessage('Running line simplification filters');

  RunLineSimplificationFilters;

  DoMessage(Format('# Feature points detected: %d', [FPoints.Count]));

  DoMessage('Detecting circle clusters');

  DetectCircleClusters;

  DoMessage(Format('Circle clusters found: %d', [FClusterFinder.Clusters.Count]));

  Cluster := FClusterFinder.Clusters[0];

  if not assigned(Cluster) then
    exit;

  if FClusterFinder.Clusters.Count >= 2 then
  begin
    CNext := FClusterFinder.Clusters[1];
    DoMessage(
      Format('First cluster weight: %5.1f (%d triplets, %d points), second cluster weight%%: %3.1f%% (%d triplets)',
        [Cluster.Weight, Cluster.Count, Cluster.References.Count,
         (CNext.Weight/Cluster.Weight) * 100, CNext.Count]));
  end;

  // Use the features that construct the first circle cluster
  for i := FPoints.Count - 1 downto 0 do
  begin
    FP := FPoints[i];
    if not Cluster.References.Find(FP, Idx) then
      FPoints.Delete(i)
    else
      FP.FeatureType := ftCircle;
  end;

end;

procedure TsdFeatureResolver.SetWidth(const Value: integer);
var
  i: integer;
begin
  if Value <> FWidth then
  begin
    FWidth := Value;
    for i := 0 to FLines.Count - 1 do
      FLines[i].SetWidth(Value);
  end;
end;

function TsdFeatureResolver.DetermineCircle3D(var Center, Normal: TsdPoint3D; var Radius: double): boolean;
var
  i, Idx, Count: integer;
  Ps, Pf: array of TsdPoint3D;
  X, Y: array of double;
  W: array of double;
  MaxErr, RMS, Zabs: double;
  FP: TsdFeaturePoint;
  S2F, F2S: TsdMatrix3x4;
begin
  // Clear results
  Result := False;
  Center := cZero3D;
  Normal := cZero3D;
  Radius := 0;

  // Initial length
  SetLength(Ps, FPoints.Count);
  SetLength(W, FPoints.Count);

  // Copy over valid 3D points
  Idx := 0;
  for i := 0 to FPoints.Count - 1 do
  begin
    FP := FPoints[i];
    if (FP.FeatureType = ftCircle) and (FP.XYZIsValid) then
    begin
      Ps[Idx] := FP.XYZ^;
      W[Idx] := FP.Weight;
      inc(Idx);
    end;
  end;

  // Set the true length now
  Count := Idx;
  SetLength(Ps, Count);
  SetLength(Pf, Count);
  SetLength(W, Count);
  SetLength(X, Count);
  SetLength(Y, Count);

  // No business here when Count <= 2 (we cannot do a 3D plane fit in that case)
  if Count <= 2 then
    exit;

  // Do a plane fit
  FPlaneFit.Count := Count;
  // Add points and weights
  FPlaneFit.SetPoints(@Ps[0], Count);
  FPlaneFit.SetWeights(@W[0], Count);

  // Solve the plane fit
  FPlaneFit.Solve;

  // Our normal
  Normal.X := FPlaneFit.A;
  Normal.Y := FPlaneFit.B;
  Normal.Z := FPlaneFit.C;

  // Now construct a transform Sensor->"Flat"
  S2F := TransformMatrixFromTwoVectors(Normal, cXAxis3D, caZAxis, caXAxis);
  TranslateMatrix3x4(S2F, S2F, Point3D(0, 0, FPlaneFit.D));
  TransformPoints(@Ps[0], @Pf[0], Count, S2F);

  // Find Max error and RMS and copy data to XY
  MaxErr := 0;
  RMS := 0;
  for i := 0 to Count - 1 do
  begin
    X[i] := Pf[i].X;
    Y[i] := Pf[i].Y;
    Zabs := abs(Pf[i].Z);
    if Zabs > MaxErr then
      MaxErr := Zabs;
    RMS := RMS + Sqr(Zabs);
  end;
  RMS := sqrt(RMS / Count);

  DoMessage(Format('Planefit %d pts, max err=%5.3fmm, RMS=%5.3fmm, ijk=%4.3f,%4.3f,%4.3f (algo space)',
      [Count, MaxErr, RMS, Normal.X, Normal.Y, Normal.Z]));

  // Do the 2D circle fit
  FCircleFit.Count := Count;
  FCircleFit.SetXValues(@X[0], Count);
  FCircleFit.SetYValues(@Y[0], Count);
  FCircleFit.SetWeights(@W[0], Count);

  FCircleFit.Solve;

  FCircleFit.CalculateError(MaxErr, RMS);

  // Transform back the center
  //OrthoMatrix3x4Inverse(S2F, F2S);
  Matrix3x4Inverse(S2F, F2S);
  TransformPoint(Point3D(FCircleFit.XCenter, FCircleFit.YCenter, 0), Center, F2S);
  Radius := FCircleFit.Radius;
  Result := True;

  DoMessage(Format('Circlefit %d pts, max err=%5.3fmm, RMS=%5.3fmm, Ctr=%5.2f,%5.2f,%5.2fmm (algo space), Radius= %5.3fmm',
    [Count, MaxErr, RMS, Center.X, Center.Y, Center.Z, FCircleFit.Radius]));

end;

end.
