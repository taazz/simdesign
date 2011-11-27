unit sdLineFilters;

interface

uses
  Classes, sdPoints2D;

type

  TsdEdgeDir = (
    edHor,
    edUp,
    edDown
  );

  TsdFeatureEvent = procedure(Sender: TObject; EdgeDir: TsdEdgeDir; const XValue, Weight: double) of object;

  TsdLineFilter = class(TComponent)
  private
    FXValues: array of double;
    FYValues: array of double;
    FCount: integer;
    FOnFeature: TsdFeatureEvent;
    procedure SetCount(const Value: integer);
  protected
    procedure DoFeature(ADir: TsdEdgeDir; AXValue, AWeight: double);
  public
    procedure SetXYData(XValues, YValues: PDouble; ACount: integer);
    procedure Run; virtual; abstract;
    property Count: integer read FCount write SetCount;
    property OnFeature: TsdFeatureEvent read FOnFeature write FOnFeature;
  end;

  TsdEdgeFilter = class(TsdLineFilter)
  private
    FBaseLimit: double;
    FDeltaLimit: double;
  protected
    procedure ProcessInterval(Idx1, Idx2: integer; ADir: TsdEdgeDir);
  public
    procedure Run; override;
    property BaseLimit: double read FBaseLimit write FBaseLimit;
    property DeltaLimit: double read FDeltaLimit write FDeltaLimit;
  end;

implementation

{ TsdLineFilter }

procedure TsdLineFilter.DoFeature(ADir: TsdEdgeDir; AXValue, AWeight: double);
begin
  if assigned(FOnFeature) then
    FOnFeature(Self, ADir, AXValue, AWeight);
end;

procedure TsdLineFilter.SetCount(const Value: integer);
begin
  FCount := Value;
  SetLength(FXValues, FCount);
  SetLength(FYValues, FCount);
end;

procedure TsdLineFilter.SetXYData(XValues, YValues: PDouble; ACount: integer);
begin
  SetCount(ACount);
  if ACount > 0 then
  begin
    Move(XValues^, FXValues[0], FCount * SizeOf(double));
    Move(YValues^, FYValues[0], FCount * SizeOf(double));
  end;
end;

{ TsdEdgeFilter }

procedure TsdEdgeFilter.ProcessInterval(Idx1, Idx2: integer; ADir: TsdEdgeDir);
var
  Yl, Yh, YCheck, YFrac, Delta: double;
  i, Idx: integer;
  Res: TsdPoint2D;
begin
  // Vertical spacing
  case ADir of
  edUp:
    begin
      Yl := FYValues[Idx1];
      Yh := FYValues[Idx2 - 1];
    end;
  edDown:
    begin
      Yl := FYValues[Idx2 - 1];
      Yh := FYValues[Idx1];
    end
  else
    // No need to detect horizontal edges..
    exit;
  end;
  // Check against thresholds
  Delta := Yh - Yl;
  if (Yl > FBaseLimit) or (Delta < FDeltaLimit) then
    exit;

  // We seem to have a valid interval, find X at VCheck which is the Y coordinate
  YCheck := Yl + Delta * 0.5;
  Idx := -1;
  for i := Idx1 to Idx2 - 1 do
  begin
    if ((FYValues[i] > YCheck) xor (FYValues[i + 1] < YCheck)) = False then
    begin
      Idx := i;
      break;
    end;
  end;
  if Idx = -1 then
    exit;

  // Find X location at intersection
  YFrac := (YCheck - FYValues[Idx]) / (FYValues[Idx + 1] - FYValues[Idx]);

  Interpolation2D(
    MakePoint2D(FXValues[Idx], FYValues[Idx]),
    MakePoint2D(FXValues[Idx + 1], FYValues[Idx + 1]), YFrac, Res);

  // Fire event
  DoFeature(ADir, Res.X, Delta);
end;

procedure TsdEdgeFilter.Run;
var
  D: array of TsdEdgeDir;
  DCur: TsdEdgeDir;
  i, Idx1, Idx2: integer;
  V1, V2: double;
begin
  if FCount < 2 then
    exit;

  SetLength(D, FCount);
  for i := 0 to FCount - 2 do
  begin
    V1 := FYValues[i];
    V2 := FYValues[i + 1];
    if V2 > V1 then
      D[i] := edUp
    else if V2 < V1 then
      D[i] := edDown
    else
      D[i] := edHor;
  end;

  Idx1 := 0;
  repeat
    DCur := D[Idx1];
    Idx2 := Idx1 + 1;
    while (Idx2 < Count) and (D[Idx2] = DCur) do
      inc(Idx2);
    // We're now at an interval [Idx1, Idx2>
    ProcessInterval(Idx1, Idx2, DCur);
    Idx1 := Idx2;
  until Idx2 = Count;

end;

end.
