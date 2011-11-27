{ unit sdSegmentate

  Segmentation of bytemaps into a number of blob segments ready to be processed
  further.

  (c) Copyright 2005 by Nils Haeck (SimDesign B.V.)
  for more info please visit www.simdesign.nl

}
unit sdSegmentate;

interface

uses
  Windows, Classes, SysUtils, Contnrs, sdByteMap, Graphics, Math;

type

  // Test result
  TsdTestResult = (
    trTestLeft,
    trTestTop,
    trTestRight,
    trTestBottom,
    trTestLT,  // Left/Top if diagonal touch (this and below only for custom methods)
    trTestRT,  // Right/Top if diagonal touch
    trTestLB,  // Left/Bottom if diagonal touch
    trTestRB,  // Right/Bottom if diagonal touch
    trTestNone // For 1x1 segments, if required
  );
  // Set of test results
  TsdTestResults = set of TsdTestResult;

const
  trTestAll = [trTestLeft, trTestTop, trTestRight, trTestBottom];

type

  TsdFloatPoint = packed record
    X: double;
    Y: double;
  end;

  TsdSegmentPixelEvent = procedure(Sender: TObject; X, Y: integer; var Result: TsdTestResults) of object;

  TsdSegmentationMethod = (
    smThreshold,
    smBandwidth,
    smCustom
  );

  TsdSegmentator = class;

  TsdSegment = class(TPersistent)
  private
    FOwner: TsdSegmentator;
    FTop: integer;
    FLeft: integer;
    FRight: integer;
    FBottom: integer;
    FYCg: double;
    FXCg: double;
    FAverage: double;
    FArea: integer;
    FSegIndex: integer;
    FSelected: Boolean;
    FScore: double;
    FSigma: double;
    FTag: integer;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetAsFloatPoint: TsdFloatPoint;
    function GetSurface: integer;
    function GetCover: double;
  protected
    constructor Create(AOwner: TsdSegmentator); virtual;
  public
    function AsRect: TRect;
    procedure CalculateAreaAndCGFast;
    procedure CalculateAverage(AMap: TsdByteMap);
    procedure CalculateAreaAndCG(AMap: TsdByteMap);
    procedure DrawToBitmap(Color: TColor; Bitmap: TBitmap);
    function FindMinNthValueWordmap(SegMap, ValueMap: TsdWordmap; Start, Close: integer): double;
    // If at x,y relative to Left/Top the segment is present, it returns True
    function Present(x, y: integer): boolean; virtual;
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Right: integer read FRight write FRight;
    property Bottom: integer read FBottom write FBottom;
    // Area is the number of pixels within the segment that are part of
    // the segment. It must be calculated first with methods CalculateAreaAndCG
    // or CalculateAreaAndCGFast
    property Area: integer read FArea write FArea;
    property Average: double read FAverage write FAverage;
    property Sigma: double read FSigma write FSigma;
    property XCg: double read FXCg write FXCg;
    property YCg: double read FYCg write FYCg;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    // Surface is the Width * Height area of the segment
    property Surface: integer read GetSurface;
    // Cover is the fraction (0..1) of pixels within the surface that
    // are part of the segment, and thus is Area/Surface
    property Cover: double read GetCover;
    property SegIndex: integer read FSegIndex write FSegIndex;
    property Selected: Boolean read FSelected write FSelected;
    // Score can be used by the application to determine a score of the
    // segment for use in detection algorithms
    property Score: double read FScore write FScore;
    // User-specifyable value
    property Tag: integer read FTag write FTag;
    // AsFloatPoint returns a float point structure with Xcg and Ycg
    property AsFloatPoint: TsdFloatPoint read GetAsFloatPoint;
    property Owner: TsdSegmentator read FOwner;
  end;

  TsdSegmentClass = class of TsdSegment;

  TsdSegmentList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdSegment;
  public
    // return separation in X between segment at Index1 and Index2, this is a negative
    // number if they overlap in X.
    class function SeparationX(Segment1, Segment2: TsdSegment): integer;
    class function SeparationY(Segment1, Segment2: TsdSegment): integer;
    procedure GetBounds(out ALeft, ATop, ARight, ABottom: integer);
    // Join segments at Index1 and Index2 (this results in a segment at Index1,
    // and the one at Index2 deleted)
    procedure Join(Index1, Index2: integer);
    property Items[Index: integer]: TsdSegment read GetItems; default;
  end;

  // Fast bitmap segmentator: limited to 65535 segments
  TsdSegmentator = class(TPersistent)
  private
    FSegments: TsdSegmentList;
    FHasAreaCGFast: boolean;
    FHasAreaCG: boolean;
    FHasAverage: boolean;
    FSegMap: TsdWordMap;
    FThreshold: integer;
    FSegmentationMethod: TsdSegmentationMethod;
    FWidth: integer;
    FHeight: integer;
    FOnTestPixel: TsdSegmentPixelEvent;
    FSegmentClass: TsdSegmentClass;
  protected
    // override in descendants to create descendant TsdSegment instances
    function NewSegment: TsdSegment; virtual;
    function SegmentateMap(Src: TsdByteMap; Dst: TsdWordMap; ClearMap: boolean): integer;
    procedure BuildSegments(Count: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    // Segmentate AMap, using AThreshold. All values in AMap above or equal to
    // AThreshold that form connected areas are put in the segment list.
    procedure SegmentateThreshold(AMap: TsdByteMap; AThreshold: integer);
    // Segmentate AMap, using a bandwidth. The neighbours of a value in the map
    // are compared to the value of the pixel, and if they're within
    // [Value - ABandwidth, Value + ABandwidth] then they form a segment.
    procedure SegmentateBandwidth(AMap: TsdByteMap; ABandWidth: integer);
    // Segmentate with a custom test routine, which should be provided in
    // OnTestPixel.
    procedure SegmentateCustom(AWidth, AHeight: integer);
    procedure CalculateAreaAndCG(AMap: TsdByteMap);
    procedure CalculateAreaAndCGFast;
    procedure CalculateAverage(AMap: TsdByteMap);
    function DistanceBetweenSegments(Index1, Index2: integer): double;
    procedure DrawToCanvas(ACanvas: TCanvas);
    procedure DrawSegmentToBitmap(Index: integer; Color: TColor; Bitmap: TBitmap);
    procedure SegmentJoin(Index1, Index2: integer; AMap: TsdByteMap = nil);
    procedure SortSegmentsByScore;
    procedure SegmentSeparation(Index1, Index2: integer; out DistX, DistY: integer);
    // Join segments that are close
    procedure JoinCloseSegments(MaxWidth, MaxHeight: integer);
    // Keep best segments based on segment score, with separation MaxWidth/MaxHeight
    procedure KeepBestSegments(MaxWidth, MaxHeight: integer);
    property HasAreaCgFast: boolean read FHasAreaCgFast write FHasAreaCgFast;
    // List of TsdSegment descendants, found through segmentation
    property Segments: TsdSegmentList read FSegments;
    property SegmentationMethod: TsdSegmentationMethod read FSegmentationMethod write FSegmentationMethod;
    // An owned TsdWordMap holding segment indices per pixel
    property SegmentMap: TsdWordmap read FSegMap;
    // Provide OnTestPixel in combination with a call to SegmentateCustom, to
    // provide a custom test for each pixel. The test should return a combination
    // of the TsdTestResult flags, indicating which neighbours connect to this pixel
    property OnTestPixel: TsdSegmentPixelEvent read FOnTestPixel write FOnTestPixel;
    // Assign another segment class (based on TsdSegment) if you want the segmentator
    // to create custom segments
    property SegmentClass: TsdSegmentClass read FSegmentClass write FSegmentClass;
  end;

procedure SeparationBetweenRects(const R1, R2: TRect; out DistX, DistY: integer);

function SegmentByScoreCompare(Item1, Item2: pointer): integer;
function SegmentByAreaCompare(Item1, Item2: pointer): integer;
function SegmentByXcgCompare(Item1, Item2: pointer): integer;
function SegmentByLeftCompare(Item1, Item2: pointer): integer;

resourcestring

  sbmTooManySegments       = 'Too many segments (>255) in segmentation';
  sbmOnTestPixelNotDefined = 'Event OnTestPixel must be defined';


implementation

type

  PWord = ^word;

{ Procedures }

procedure CopyValuesInMap16b(Map: TsdWordMap; OldVal, NewVal: word);
var
  i: integer;
  P: PWord;
begin
  P := PWord(Map.MapPointer);
  for i := 0 to Map.ElementCount - 1 do
  begin
    if P^ = OldVal then
      P^ := NewVal;
    inc(P);
  end;
end;

procedure SeparationBetweenRects(const R1, R2: TRect; out DistX, DistY: integer);
begin
  // separation in X
  if R1.Right < R2.Left then
    // no overlap, seg1 left of seg2
    DistX := R2.Left - R1.Right
  else
    if R1.Left > R2.Right then
      // no overlap, seg1 right of seg2
      DistX := R1.Left - R2.Right
    else
      // overlap
      DistX := Max(R1.Left, R2.Left) - Min(R1.Right, R2.Right);

  // separation in Y
  if R1.Bottom < R2.Top then
    // no overlap, seg1 above seg2
    DistY := R2.Top - R1.Bottom
  else
    if R1.Top > R2.Bottom then
      // no overlap, seg1 below of seg2
      DistY := R1.Top - R2.Bottom
    else
      // overlap
      DistY := Max(R1.Top, R2.Top) - Min(R1.Bottom, R2.Bottom);
end;

{ TsdSegment }

function TsdSegment.AsRect: TRect;
begin
  Result := Rect(FLeft, FTop, FRight, FBottom);
end;

procedure TsdSegment.CalculateAreaAndCG(AMap: TsdByteMap);
var
  x, y, Value, Cover: integer;
  Area, XCg, YCg: int64;
  SegMap: TsdWordMap;
begin
  SegMap := FOwner.FSegMap;
  Area := 0;
  Cover := 0;
  XCg := 0;
  YCg := 0;
  for y := FTop to FBottom - 1 do
    for x := FLeft to FRight - 1 do
    begin
      if SegMap[x, y] = FSegIndex then
      begin
        Value := AMap[x, y];
        inc(Cover);
        inc(Area, Value);
        inc(XCg, x * Value);
        inc(YCg, y * Value);
      end;
    end;
  // we add 0.5 because the center of the pixel is at 0.5, 0.5
  FArea := Cover;
  FXCg := XCg / Area + 0.5;
  FYCg := YCg / Area + 0.5;
end;

procedure TsdSegment.CalculateAreaAndCGFast;
var
  x, y: integer;
  Area, XCg, YCg: integer;
  SegMap: TsdWordMap;
begin
  SegMap := FOwner.FSegMap;
  Area := 0;
  XCg := 0;
  YCg := 0;
  for y := FTop to FBottom - 1 do
    for x := FLeft to FRight - 1 do
    begin
      if SegMap[x, y] = FSegIndex then
      begin
        inc(Area);
        inc(XCg, x);
        inc(YCg, y);
      end;
    end;
  FArea := Area;
  if FArea > 0 then
  begin
    FXCg := XCg / Area + 0.5;
    FYCg := YCg / Area + 0.5;
  end;
end;

procedure TsdSegment.CalculateAverage(AMap: TsdByteMap);
var
  x, y, Value: integer;
  SegMap: TsdWordMap;
  N: integer;
  SumX, SumX2: int64;
begin
  SegMap := FOwner.FSegMap;
  SumX := 0;
  SumX2 := 0;
  N := 0;
  for y := FTop to FBottom - 1 do
    for x := FLeft to FRight - 1 do
    begin
      if SegMap[x, y] = FSegIndex then
      begin
        Value := AMap[x, y];
        inc(SumX, Value);
        inc(SumX2, sqr(Value));
        inc(N);
      end;
    end;
  if N > 0 then
  begin
    FAverage := SumX / N;
    FSigma := sqrt((SumX2 - (SumX * SumX)/N)/N);
  end else
  begin
    FAverage := 0;
    FSigma := 0;
  end;
end;

constructor TsdSegment.Create(AOwner: TsdSegmentator);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TsdSegment.DrawToBitmap(Color: TColor; Bitmap: TBitmap);
var
  x, y: integer;
  SegMap: TsdWordMap;
begin
  SegMap := FOwner.FSegMap;
  for y := FTop to FBottom - 1 do
    for x := FLeft to FRight - 1 do
      if SegMap[x, y] = FSegIndex then
        Bitmap.Canvas.Pixels[x, y] := Color;
end;

function TsdSegment.FindMinNthValueWordmap(SegMap, ValueMap: TsdWordmap; Start, Close: integer): double;
var
  i, x, y, ACount: integer;
  AValue: word;
  Values: array of word;
// local
procedure InsertValue(AValue: word; Pos: integer);
var
  i: integer;
begin
  for i := ACount - 2 downto Pos do
    Values[i + 1] := Values[i];
  Values[Pos] := AValue;
end;
// main
begin
  ACount := Close;
  SetLength(Values, ACount);
  for i := 0 to ACount - 1 do
    Values[i] := $FFFF;
  for y := Top to Bottom - 1 do
    for x := Left to Right - 1 do
      if SegMap[x, y] = SegIndex then
      begin
        AValue := ValueMap[x, y];
        if AValue >= Values[ACount - 1] then
          break;
        for i := 0 to ACount - 1 do
          if AValue < Values[i] then
          begin
            InsertValue(AValue, i);
            break;
          end;
      end;
  Result := 0;
  for i := Start to Close - 1 do
    Result := Result + Values[i];
  if Close - Start > 0 then
    Result := Result / (Close - Start);
end;

function TsdSegment.GetAsFloatPoint: TsdFloatPoint;
begin
  Result.X := Xcg;
  Result.Y := Ycg;
end;

function TsdSegment.GetCover: double;
begin
  Result := FArea / Surface;
end;

function TsdSegment.GetHeight: integer;
begin
  Result := Bottom - Top;
end;

function TsdSegment.GetSurface: integer;
begin
  Result := Width * Height;
end;

function TsdSegment.GetWidth: integer;
begin
  Result := Right - Left;
end;

function TsdSegment.Present(x, y: integer): boolean;
begin
  Result := FOwner.FSegMap[x, y] = FSegIndex;
end;

{ TsdSegmentList }

procedure TsdSegmentList.GetBounds(out ALeft, ATop, ARight, ABottom: integer);
var
  i: integer;
  S: TsdSegment;
begin
  ALeft := 0;
  ATop := 0;
  ARight := 0;
  ABottom := 0;
  for i := 0 to Count - 1 do
  begin
    S := Items[i];
    if i = 0 then
    begin
      ALeft := S.Left;
      ATop := S.Top;
      ARight := S.Right;
      ABottom := S.Bottom;
    end else
    begin
      ALeft := Min(S.Left, ALeft);
      ATop := Min(S.Top, ATop);
      ARight := Max(S.Right, ARight);
      ABottom := Max(S.Bottom, ABottom);
    end;
  end;
end;

function TsdSegmentList.GetItems(Index: integer): TsdSegment;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

procedure TsdSegmentList.Join(Index1, Index2: integer);
var
  x, y, S1Index, S2Index: integer;
  S1, S2: TsdSegment;
  Map: TsdWordMap;
begin
  S1 := Items[Index1];
  S2 := Items[Index2];
  if not assigned(S1) or not assigned(S2) then
    exit;

  S1Index := S1.SegIndex;
  S2Index := S2.SegIndex;

  // Copy pixels
  Map := S1.Owner.FSegMap;
  for y := S2.Top to S2.Bottom - 1 do
    for x := S2.Left to S2.Right - 1 do
      if Map[x, y] = S2Index then
        Map[x, y] := S1Index;

  // Combine limits
  S1.Left   := min(S1.Left,   S2.Left);
  S1.Top    := min(S1.Top,    S2.Top);
  S1.Right  := max(S1.Right,  S2.Right);
  S1.Bottom := max(S1.Bottom, S2.Bottom);

  // Delete segment 2
  Delete(Index2);
end;

class function TsdSegmentList.SeparationX(Segment1, Segment2: TsdSegment): integer;
begin
  // separation in X
  if Segment1.Right < Segment1.Left then
    // no overlap, seg1 left of seg2
    Result := Segment2.Left - Segment1.Right
  else
    if Segment1.Left > Segment2.Right then
      // no overlap, seg1 right of seg2
      Result := Segment1.Left - Segment2.Right
    else
      // overlap
      Result := Max(Segment1.Left, Segment2.Left) - Min(Segment1.Right, Segment2.Right);
end;

class function TsdSegmentList.SeparationY(Segment1, Segment2: TsdSegment): integer;
begin
  // separation in Y
  if Segment1.Bottom < Segment1.Top then
    // no overlap, seg1 left of seg2
    Result := Segment2.Top - Segment1.Bottom
  else
    if Segment1.Top > Segment2.Bottom then
      // no overlap, seg1 right of seg2
      Result := Segment1.Top - Segment2.Bottom
    else
      // overlap
      Result := Max(Segment1.Top, Segment2.Top) - Min(Segment1.Bottom, Segment2.Bottom);
end;

{ TsdSegmentator }

procedure TsdSegmentator.BuildSegments(Count: integer);
var
  i, x, y: integer;
  ASegment: TsdSegment;
begin
  Clear;
  // Create segments
  for i := 0 to Count - 1 do
  begin
    ASegment := NewSegment;
    ASegment.Left := FWidth;
    ASegment.Right := 0;
    ASegment.Top := FHeight;
    ASegment.Bottom := 0;
    ASegment.SegIndex := i + 1;
    FSegments.Add(ASegment);
  end;

  // Find sizes of each segment
  for y := 0 to FSegMap.Height - 1 do
    for x := 0 to FSegMap.Width - 1 do
    begin
      if FSegMap[x, y] > 0 then
      begin
        ASegment := Segments[FSegMap[x, y] - 1];
        if not assigned(ASegment) then
          continue;
        ASegment.Left   := Min(ASegment.Left, x);
        ASegment.Right  := Max(ASegment.Right, x + 1);
        ASegment.Top    := Min(ASegment.Top, y);
        ASegment.Bottom := Max(ASegment.Bottom, y + 1);
      end;
    end;
end;

procedure TsdSegmentator.CalculateAreaAndCG(AMap: TsdByteMap);
var
  i: integer;
begin
  // Area and CG
  for i := 0 to Segments.Count - 1 do
    Segments[i].CalculateAreaAndCG(AMap);
  FHasAreaCG := True;
end;

procedure TsdSegmentator.CalculateAreaAndCGFast;
var
  i: integer;
begin
  // Area and CG Fast
  for i := 0 to Segments.Count - 1 do
    Segments[i].CalculateAreaAndCGFast;
  FHasAreaCGFast := True;
end;

procedure TsdSegmentator.CalculateAverage(AMap: TsdByteMap);
var
  i: integer;
begin
  for i := 0 to Segments.Count - 1 do
    Segments[i].CalculateAverage(AMap);
  FHasAverage := True;
end;

procedure TsdSegmentator.Clear;
begin
  FSegments.Clear;
  FHasAreaCG := False;
  FHasAreaCGFast := False;
  FHasAverage := False;
end;

constructor TsdSegmentator.Create;
begin
  inherited Create;
  FSegments := TsdSegmentList.Create(True);
  FSegmentClass := TsdSegment;
  FSegMap := TsdWordMap.Create;
end;

destructor TsdSegmentator.Destroy;
begin
  FreeAndNil(FSegMap);
  FreeAndNil(FSegments);
  inherited;
end;

function TsdSegmentator.DistanceBetweenSegments(Index1, Index2: integer): double;
var
  Seg1, Seg2: TsdSegment;
begin
  Result := 0;
  Seg1 := Segments[Index1];
  Seg2 := Segments[Index2];
  if not assigned(Seg1) or not assigned(Seg2) then
    exit;
  Result := Sqrt(Sqr(Seg1.Xcg - Seg2.Xcg) + Sqr(Seg1.Ycg - Seg2.Ycg));
end;

procedure TsdSegmentator.DrawSegmentToBitmap(Index: integer; Color: TColor;
  Bitmap: TBitmap);
var
  ASegment: TsdSegment;
begin
  ASegment := Segments[Index];
  if not assigned(ASegment) then
    exit;
  ASegment.DrawToBitmap(Color, Bitmap);
end;

procedure TsdSegmentator.DrawToCanvas(ACanvas: TCanvas);
var
  i: integer;
  ASegment: TsdSegment;
begin
  for i := 0 to Segments.Count - 1 do
  begin
    ASegment := Segments[i];
    ACanvas.Rectangle(ASegment.Left, ASegment.Top, ASegment.Right, ASegment.Bottom);
  end;
end;

procedure TsdSegmentator.JoinCloseSegments(MaxWidth, MaxHeight: integer);
var
  i, j, x, y, AWidth, AHeight: integer;
  HasJoined: boolean;
begin
  // Calculate areas
  CalculateAreaAndCGFast;

  // Sort segments by area
  FSegments.Sort(SegmentByAreaCompare);

  // Join segments that are close enough
  repeat
    HasJoined := False;
    for i := 0 to Segments.Count - 2 do
    begin
      if HasJoined then
        break;
      for j := i + 1 to Segments.Count - 1 do
      begin
        AWidth := max(Segments[i].Right, Segments[j].Right) - min(Segments[i].Left, Segments[j].Left);
        if AWidth <= MaxWidth then
        begin
          AHeight := max(Segments[i].Bottom, Segments[j].Bottom) - min(Segments[i].Top, Segments[j].Top);
          if AHeight <= MaxHeight then
          begin
            // Join'em
            for y := Segments[j].Top to Segments[j].Bottom - 1 do
              for x := Segments[j].Left to Segments[j].Right - 1 do
                if FSegMap[x, y] = Segments[j].SegIndex then
                  FSegMap[x, y] := Segments[i].SegIndex;
            Segments[i].Left := Min(Segments[i].Left, Segments[j].Left);
            Segments[i].Right := Max(Segments[i].Right, Segments[j].Right);
            Segments[i].Top := Min(Segments[i].Top, Segments[j].Top);
            Segments[i].Bottom := Max(Segments[i].Bottom, Segments[j].Bottom);
            Segments.Delete(j);
            HasJoined := True;
            break;
          end;
        end;
      end;
    end;
  until HasJoined = False;
end;

procedure TsdSegmentator.KeepBestSegments(MaxWidth, MaxHeight: integer);
var
  i, j, AWidth, AHeight: integer;
  HasDeleted: boolean;
begin
  // Sort segments by score
  FSegments.Sort(SegmentByScoreCompare);

  // Keep segments that are distant enough
  repeat
    HasDeleted := False;
    for i := 0 to Segments.Count - 2 do
    begin
      if HasDeleted then
        break;
      for j := Segments.Count - 1 downto i + 1 do
      begin
        AWidth := max(Segments[i].Right, Segments[j].Right) - min(Segments[i].Left, Segments[j].Left);
        if AWidth <= MaxWidth then
        begin
          AHeight := max(Segments[i].Bottom, Segments[j].Bottom) - min(Segments[i].Top, Segments[j].Top);
          if AHeight <= MaxHeight then
          begin
            // Delete j
            Segments.Delete(j);
            HasDeleted := True;
          end;
        end;
      end;
    end;
  until HasDeleted = False;
end;

function TsdSegmentator.NewSegment: TsdSegment;
begin
  Result := FSegmentClass.Create(Self);
end;

procedure TsdSegmentator.SegmentateBandwidth(AMap: TsdByteMap; ABandWidth: integer);
var
  ACount: integer;
begin
  FThreshold := ABandWidth;
  FSegmentationMethod := smBandwidth;

  // Build segmetation indices into FSegMap
  ACount := SegmentateMap(AMap, FSegMap, True);
  BuildSegments(ACount);
end;

procedure TsdSegmentator.SegmentateCustom(AWidth, AHeight: integer);
var
  ACount: integer;
begin
  FSegmentationMethod := smCustom;
  FWidth := AWidth;
  FHeight := AHeight;
  if not assigned(FOnTestPixel) then
    raise Exception.Create(sbmOnTestPixelNotDefined);

  // Build segmetation indices into FSegMap
  ACount := SegmentateMap(nil, FSegMap, True);
  BuildSegments(ACount);
end;

function TsdSegmentator.SegmentateMap(Src: TsdByteMap; Dst: TsdWordMap; ClearMap: boolean): integer;
// Diagonal touching is not a touch!
type
  TsdTest = record
    X: integer;
    Y: integer;
    Test: TsdTestResult;
  end;
const
  cNeighbourCount = 4;
  cNeighbours: array[0..cNeighbourCount - 1] of TsdTest =
    ((X: -1; Y:  0; Test: trTestLeft),
     (X:  0; Y: -1; Test: trTestTop),
     (X:  1; Y:  0; Test: trTestRight),
     (X:  0; Y:  1; Test: trTestBottom));
var
  Maps, Segs: array of word;
  SegCount: integer;
// local
function CondenseMap(Count: integer): integer;
var
  i: integer;
  P: PWord;
begin
  // First pass: find maps in use
  SetLength(Maps, Max(length(Maps), Count + 1));
  FillChar(Maps[0], (Count + 1) * SizeOf(word), 0);
  P := PWord(Dst.MapPointer);
  for i := 0 to Dst.ElementCount - 1 do
  begin
    if P^ > 0 then Maps[P^] := P^;
    inc(P);
  end;
  // Find empty maps
  Result := 0;
  for i := 1 to Count do
    if Maps[i] > 0 then
    begin
      inc(Result);
      Maps[i] := Result;
    end;
  // Second pass: change map numbers
  P := PWord(Dst.MapPointer);
  for i := 0 to Dst.ElementCount - 1 do
  begin
    if P^ > 0 then P^ := Maps[P^];
    inc(P);
  end;
end;
// local
function TestCondition(x, y: integer): TsdTestResults;
var
  i, Value, LoLim, HiLim: integer;
begin
  case FSegmentationMethod of
  smThreshold:
    if Src[x, y] >= FThreshold then
      Result := trTestAll
    else
      Result := [];
  smBandwidth:
    begin
      // Band limits
      LoLim := Src[x, y]; HiLim := LoLim;
      dec(LoLim, FThreshold);
      inc(HiLim, FThreshold);
      // Four sides
      Result := [];
      for i := 0 to cNeighbourCount - 1 do
      begin
        Value := Src[x + cNeighBours[i].X, y + cNeighbours[i].y];
        if (Value >= LoLim) and (Value <= HiLim) then
          include(Result, cNeighbours[i].Test);
        if (i >= 1) and (Result <> []) then break;
      end;
    end;
  smCustom:
    FOnTestPixel(Self, x, y, Result);
  end;//case
end;
// local
function SegResolve(M: integer): integer;
begin
  Result := M;
  if Result > 0 then
  begin
    while Segs[Result] < Result do
      Result := Segs[Result];
    Segs[M] := Result;
  end;
end;
//
var
  x, y, i: integer;
  Low, High, M: word;
  Next: integer;
  P: PWord;
  TestResult: TsdTestResults;
// main
begin
  if assigned(Src) then
  begin
    FWidth := Src.Width;
    FHeight := Src.Height;
  end;
  Result := 0;
  if not assigned(Dst) then
    exit;

  SetLength(Segs, 0);
  SegCount := 0;
  if not ((Dst.Width = FWidth) and (Dst.Height = FHeight)) then
  begin
    Dst.SetSize(FWidth, FHeight);
    Dst.Clear(0);
  end else
    if ClearMap then
      Dst.Clear(0);

  Next := 0;
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      // Condition is OK?
      TestResult := TestCondition(x, y);
      if TestResult <> [] then
      begin
        Low := 65535;
        High := 0;
        if trTestLeft in TestResult then
        begin
          M := SegResolve(Dst[x - 1, y]);
          if M > 0 then
          begin
            Low := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if trTestTop in TestResult then
        begin
          M := SegResolve(Dst[x, y - 1]);
          if M > 0 then
          begin
            Low := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if trTestLT in TestResult then
        begin
          M := SegResolve(Dst[x - 1, y - 1]);
          if M > 0 then
          begin
            Low := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if trTestRT in TestResult then
        begin
          M := SegResolve(Dst[x + 1, y - 1]);
          if M > 0 then
          begin
            Low := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if High > 0 then
        begin
          // There's a neighbour with a map number
          Dst[x, y] := Low;
          if High > Low then
            Segs[High] := Low;
        end else
        begin
          // No neighbour, add new map number
          inc(Next);

          // Check maximum number of segments
          if Next = 65536 then
            raise Exception.Create(sbmTooManySegments);

          // Segment list
          if SegCount < Next + 1 then
          begin
            inc(SegCount, 100);
            SetLength(Segs, SegCount);
          end;
          Segs[Next] := Next;

          // Destination word
          Dst[x, y] := Next;
        end;
      end;
    end;

  // Before we condense, make sure to apply segment list
  for i := 1  to Next do
    SegResolve(i);
  P := PWord(Dst.MapPointer);
  for i := 0 to Dst.ElementCount - 1 do
  begin
    if P^ > 0 then P^ := Segs[P^];
    inc(P);
  end;

  // Condense the map
  Result := CondenseMap(Next);
end;

procedure TsdSegmentator.SegmentateThreshold(AMap: TsdByteMap; AThreshold: integer);
var
  ACount: integer;
begin
  FThreshold := AThreshold;
  FSegmentationMethod := smThreshold;

  // Build segmetation indices into FSegMap
  ACount := SegmentateMap(AMap, FSegMap, True);

  BuildSegments(ACount);
end;

procedure TsdSegmentator.SegmentJoin(Index1, Index2: integer; AMap: TsdByteMap);
var
  Seg1: TsdSegment;
begin
  Segments.Join(Index1, Index2);
  Seg1 := Segments[Index1];
  if not assigned(Seg1) then
    exit;

  // Recalcuate statistics
  if FHasAreaCGFast then
    Seg1.CalculateAreaAndCGFast;
  if FHasAreaCG and assigned(AMap) then
    Seg1.CalculateAreaAndCG(AMap);
  if FHasAverage and assigned(AMap) then
    Seg1.CalculateAverage(AMap);
end;

procedure TsdSegmentator.SegmentSeparation(Index1, Index2: integer;
  out DistX, DistY: integer);
var
  Seg1, Seg2: TsdSegment;
begin
  DistX := 0;
  DistY := 0;
  Seg1 := Segments[Index1];
  Seg2 := Segments[Index2];
  if not assigned(Seg1) or not assigned(Seg2) then
    exit;

  DistX := Segments.SeparationX(Seg1, Seg2);
  DistY := Segments.SeparationY(Seg1, Seg2);
end;

procedure TsdSegmentator.SortSegmentsByScore;
begin
  FSegments.Sort(SegmentByScoreCompare);
end;

function SegmentByScoreCompare(Item1, Item2: pointer): integer;
var
  S1, S2: TsdSegment;
begin
  S1 := TsdSegment(Item1);
  S2 := TsdSegment(Item2);
  if S1.Score < S2.Score then
    Result := -1
  else
    if S1.Score > S2.Score then
      Result := 1
    else
      Result := 0;
end;

function SegmentByAreaCompare(Item1, Item2: pointer): integer;
var
  S1, S2: TsdSegment;
begin
  S1 := TsdSegment(Item1);
  S2 := TsdSegment(Item2);
  if S1.Area < S2.Area then
    Result := -1
  else
    if S1.Area > S2.Area then
      Result := 1
    else
      Result := 0;
end;

function SegmentByXcgCompare(Item1, Item2: pointer): integer;
var
  S1, S2: TsdSegment;
begin
  S1 := TsdSegment(Item1);
  S2 := TsdSegment(Item2);
  if S1.Xcg < S2.Xcg then
    Result := -1
  else
    if S1.Xcg > S2.Xcg then
      Result := 1
    else
      Result := 0;
end;

function SegmentByLeftCompare(Item1, Item2: pointer): integer;
var
  S1, S2: TsdSegment;
begin
  S1 := TsdSegment(Item1);
  S2 := TsdSegment(Item2);
  if S1.Left < S2.Left then
    Result := -1
  else
    if S1.Left > S2.Left then
      Result := 1
    else
      Result := 0;
end;

end.
