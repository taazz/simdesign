{
  Laser point and laser line detection on a 2D grayscale image.

  Purpose of the detection is to find laser lines in an image with sub-pixel
  accuracy, and find how they are connected.

  Creation Date: 15Sep2008 (NH)

  Modifications:

  Copyright (c) 2008 By Nils Haeck M.Sc. - SimDesign
  More information: www.simdesign.nl or n.haeck@simdesign.nl

}
unit sdLaserPointDetection;

interface

uses
  Classes, SysUtils, Contnrs, sdPoints2D, sdMapIterator, sdSortedLists;

type

  TStringEvent = procedure(Sender: TObject; const AMessage: string) of object;

  TsdLaserPointDetector = class;

  TsdSearchDirection = (
    sdLeft,
    sdRight
  );

  // Single laser point distilled from the grayscale image. It has top pixel
  // coordinates X and Y, and vertically covers Count pixels. The CG contains a
  // floating point precision calculation of the center, based on the intensity
  // range over the pixels.
  TsdLaserPoint = class(TPersistent)
  private
    FX: integer;
    FY: integer;
    FCount: integer;
    FIntensity: integer;
    FCG: TsdPoint2D;
    FScore: double;
    FLftNbr: TsdLaserPoint;
    FRgtNbr: TsdLaserPoint;
    FLineIdx: integer;
    function GetCG: PsdPoint2D;
  public
    procedure Assign(Source: TPersistent); override;
    function IsIsolated: boolean;
    function IsLftEndpt: boolean;
    function IsRgtEndpt: boolean;
    function IsEndpt: boolean;
    function IsFullyConnected: boolean;
    function IsHelper: boolean;
    // Start pixel in X
    property X: integer read FX write FX;
    // Start pixel in Y
    property Y: integer read FY write FY;
    // Number of pixels included
    property Count: integer read FCount write FCount;
    // Intensity integral
    property Intensity: integer read FIntensity write FIntensity;
    // The center point (CG) of the laser point in pixel space (floating point precision)
    property CG: PsdPoint2D read GetCG;
    // Score can be used to store scoring in aux algorithms
    property Score: double read FScore write FScore;
    // left neighbour
    property LftNbr: TsdLaserPoint read FLftNbr write FLftNbr;
    // right neghbour
    property RgtNbr: TsdLaserPoint read FRgtNbr write FRgtNbr;
    // LineIdx: line index of the line this point belongs to (only valid after
    // calling SetLineIndices). If LineIdx = -1, the point belongs to no line
    property LineIdx: integer read FLineIdx write FLineIdx;
  end;

  // Laser point sort methods
  TsdLPSortMethod = (
    psByXY,          // first by X, then by Y, return 0 if overlap
    psByIntensity,   // by intensity, increasing
    psXLeftToRight,  // X increasing
    psXRightToLeft   // X decreasing
  );

  // List of laser points
  TsdLaserPointList = class(TCustomSortedList)
  private
    FSortMethod: TsdLPSortMethod;
    function GetItems(Index: integer): TsdLaserPoint;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    procedure DetectNeighbours;
    // Merge the point at IdxAdd into the one at IdxMain
    procedure MergePoints(IdxMain, IdxAdd: integer);
    property Items[Index: integer]: TsdLaserPoint read GetItems; default;
    property SortMethod: TsdLPSortMethod read FSortMethod write FSortMethod;
  end;

  // Sort methods for laser lines and polylines
  TsdLineSortMethod = (
    lsByPointCount,    // Sort by point count (increasing)
    lsXLeftToRight,    // left endpoint, left-to-right (X increasing)
    lsByLineId         // Sort polylines by plane id
  );

  TsdPolyLine = class;

  // Line made of laser points that are connected left->right. A list of these
  // lines is owned by TsdLaserPointDetector, and can be built with DetectLines
  TsdLaserLine = class(TPersistent)
  private
    FOwner: TsdLaserPointDetector;
    FPoints: TsdLaserPointList;
    FPolyLine: TsdPolyLine;
    function GetLft: TsdLaserPoint;
    function GetRgt: TsdLaserPoint;
  public
    constructor Create(AOwner: TsdLaserPointDetector); virtual;
    destructor Destroy; override;
    // Delete a segment of points from the list, and also remove the points
    // from the owner's list. Start at AIndex, and remove ACount points
    procedure DeleteSegment(AIndex, ACount: integer);
    // Build a helper line (class method) - builds a line of helper points between
    // P1 and P2. P1 and P2 must already exist. Helper points are added to
    // APointList, and wired up (lft/rgt neighbours). P1 must be to the left of P2.
    class procedure BuildHelperLine(P1, P2: TsdLaserPoint; APointList: TsdLaserPointList);
    // Line length (euclidian) from start to end point
    function GetLineLength: double;
    function SlopeAt(Idx, AWidth: integer): double;
    // List of laserpoints (unowned) belonging to this line, sorted by X coordinate
    property Points: TsdLaserPointList read FPoints;
    // Reference to left point of this line
    property Lft: TsdLaserPoint read GetLft;
    // Reference to right point of this line
    property Rgt: TsdLaserPoint read GetRgt;
    // Reference to polyLine we belong to, or nil
    property PolyLine: TsdPolyLine read FPolyLine write FPolyLine;
  end;

  TsdLaserLineList = class(TCustomSortedList)
  private
    FSortMethod: TsdLineSortMethod;
    function GetItems(Index: integer): TsdLaserLine;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    // Join laser lines LL1 and LL2. LL1 will contain both, LL2 will cease to
    // exist. If there's overlap, segment with lowest intensity will be removed,
    // if there's spacing, helper points will be added. LL1 must be to the left
    // of LL2.
    procedure JoinLines(LL1, LL2: TsdLaserLine);
    // Returns the index of the longest line (longest is measured in points)
    function LongestLineIdx: integer;
    property Items[Index: integer]: TsdLaserLine read GetItems; default;
    property SortMethod: TsdLineSortMethod read FSortMethod write FSortMethod;
  end;

  // Laser point detector algorithm for structured light. It finds pixel columns
  // above a certain threshold (laser points) and determines their CG, returning
  // them in a list of laser points.
  TsdLaserPointDetector = class(TComponent)
  private
    FPoints: TsdLaserPointList;
    FLines: TsdLaserLineList;
  protected
    function PointAbove(APoint: TsdLaserPoint): TsdLaserPoint;
    function PointBelow(APoint: TsdLaserPoint): TsdLaserPoint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Clear laser point and line lists
    procedure Clear;
    // Detect points on the 8bit AInputMap (bytes), using a threshold from AThresholdMap.
    // if AThresholdMap is nil, only the fixed threshold AThreshold will be used.
    // if AThresholdMap is assigned, the threshold for the current pixel will be
    // the value of the map, plus the fixed AThreshold value. All
    // pixels equal or above the threshold are considered part of laser points.
    // Current laser points and lines lists will be cleared.
    procedure DetectPoints8bit(AInputMap, AThresholdMap: TsdMapIterator; AThreshold: integer);
    // Detect lines in the image, by finding left endpoints, then travelling
    // through them until a right endpoint is found.
    procedure DetectLines;
    // If Lines contains a set of lines, the index in the array is transferred
    // to the Point.LineIdx property. Points without lines will have LineIdx -1.
    procedure SetLineIndices;
    // Owned list of points
    property Points: TsdLaserPointList read FPoints;
    // Owned list of lines
    property Lines: TsdLaserLineList read FLines;
  end;

{
  Laserpoint connector using tunnels

  *T**** <- top line
   |
  *S--C*
   |
  *B**** <- btm line

  We start the tunnels with the S (seed) point, which is either an isolated
  point or left/right connected. The tunnel has the horizontal direction *away*
  from the endpoint, so in above example, to the right. If S is isoltated, we
  simply create two tunnels, in both directions.

  T is the top point, above S, and B is the bottom point, below S. When we Step
  the tunnel, we go to T's right neighbour, and B's right neighbour (always moving
  only one column).

  We search between the new T and B points, and see if we find a candidate C. In
  above example, we find C at 3rd step (skipping 2 empty spaces). The position of
  C vertically is restricted to a certain interval in order for it to be acceptable.

  We connect the tunnel points S and C, adding intermediate helper points H. So
  the new situation after connection is:

  *T**** <- top line
   |
  *SHHC*
   |
  *B**** <- btm line

  Helper points have an interpolated CG, and are just one pixel high, and have
  intensity = 0.

  By this time, the tunnel can be discarded. If Top or Btm lines terminate before
  a Connection point is found, the tunnel is discared too.

  Tricky situation:

  *T**** <- top line
   |
   |  C*
  *S-?
   |  C*
   |
  *B**** <- btm line

  We find TWO or more candidates. We must choose the C that matches best.

  Tunnel score is based on the vertical distance Dist of Hp and C, Hp is the
  predicted intermediate point (T-Hp-B has same ratio as T-S-B). The # steps
  required to get to C is also a factor. Score uses the following formula:

  Score = sqr(Dist) + sqr(Steps)   // Steps >= 1

  Tunnels search for candidates within a certain horizontal search bracket, and
  keep a list of them. Final tunnel candidate is the one with lowest score.

  Tunnels that have at least one candidate are then used (lowest score first) to
  generate connections. After the connection is made, the points S and C are removed
  from the Pool, and tunnels based on them are removed from the tunnel list.

  New tunnels are built for candidates Cfail that didn't make it. First, tunnels with
  Seed = Cfail are removed, then rebuilt.

  Tunnels that are strung up with Top/Btm points being S or C are also re-created.

  Termination: We continue the process until we cannot create any more connections.

}
  // Tunnel class (also used for slopes)
  TsdTunnel = class(TPersistent)
  private
    FSearch: TsdSearchDirection;
    FPoints: TsdLaserPointList;
    FSeed: TsdLaserPoint;
    FTarget: TsdLaserPoint;
    FTop: TsdLaserPoint;
    FBtm: TsdLaserPoint;
    FIsValid: boolean;
    FRatio: double;
    FScore: double;
    FCPool: TsdLaserPointList;
  public
    constructor Create;
    destructor Destroy; override;
    function SearchSlopeStep(AStep: integer; const ASearchHeight: double): boolean;
    function SearchTunnelStep(AStep: integer; const ASearchHeight: double): boolean;
    procedure BuildSlope(ASeedIdx: integer; ASlope: double; ASearch: TsdSearchDirection);
    procedure BuildTunnel(ASeedIdx: integer; ASearch: TsdSearchDirection);
    procedure SearchSlopeTarget(const ASearchHeight: double; ASearchBracket: integer);
    procedure SearchTunnelTarget(const ANominalTunnelHeight, ASearchHeight: double; ASearchBracket: integer);
    // Connect the tunnel's Seed to Target
    function Connect: boolean;
    // Seed (startpoint) of the tunnel/slope
    property Seed: TsdLaserPoint read FSeed;
    // Target (endpoint) of the tunnel/slope
    property Target: TsdLaserPoint read FTarget;
    // Top laser point of the tunnel (not used for slope search)
    property Top: TsdLaserPoint read FTop;
    // Bottom laser point of the tunnel (not used for slope search)
    property Btm: TsdLaserPoint read FBtm;
    // Is the tunnel (still) valid?
    property IsValid: boolean read FIsValid;
    // Direction in which we search (left or right)
    property Search: TsdSearchDirection read FSearch;
    // Score of this tunnel: ranks it in the tunnel list, and tunnels with
    // highest score are first connected
    property Score: double read FScore;
    // Pool of candidates (owned list)
    property CPool: TsdLaserPointList read FCPool;
    // Reference to master point list
    property Points: TsdLaserPointList read FPoints;
  end;

  TsdTunnelList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdTunnel;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    function BySeed(ASeed: TsdLaserPoint): integer;
    function BySeedAndSearch(ASeed: TsdLaserPoint; ASearch: TsdSearchDirection): integer;
    function ByTopBtm(APoint: TsdLaserPoint): integer;
    procedure RemoveBySeed(ASeed: TsdLaserPoint);
    property Items[Index: integer]: TsdTunnel read GetItems; default;
  end;

  // This class implements the tunnel search to connect laser points
  TsdTunnelSearch = class(TComponent)
  private
    FTunnels: TsdTunnelList;
    FPool: TsdLaserPointList;
    FPoints: TsdLaserPointList;
    FNominalTunnelHeight: double;
    FSearchBracket: integer;
    FSearchHeight: double;
    FOnMessage: TStringEvent;
    FSlope: double;
  protected
    procedure DoMessage(const AMessage: string);
    procedure AddSlopesForSeed(ASeed: TsdLaserPoint; var T: TsdTunnel);
    procedure AddTunnelsForSeed(ASeed: TsdLaserPoint; var T: TsdTunnel);
    procedure BuildPool;
    procedure BuildSlopeList;
    procedure BuildTunnelList;
    function NewTunnel: TsdTunnel;
    // List of seed points (owned list)
    property Pool: TsdLaserPointList read FPool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Make connections based on tunneling principle
    procedure MakeTunnelConnections;
    // Make connections based on slope search
    procedure MakeSlopeConnections;
    property Tunnels: TsdTunnelList read FTunnels;
    property SearchBracket: integer read FSearchBracket write FSearchBracket;
    property SearchHeight: double read FSearchHeight write FSearchHeight;
    property NominalTunnelHeight: double read FNominalTunnelHeight write FNominalTunnelHeight;
    // Used for slope search, must be set prior - should indicate average line slope
    property Slope: double read FSlope write FSlope;
    // List of all points (referenced list)
    property Points: TsdLaserPointList read FPoints write FPoints;
    property OnMessage: TStringEvent read FOnMessage write FOnMessage;
  end;

  // Link information between PolyLines, this object is used to generated the
  // Plane Id's
  TsdLinkInfo = class
  private
    FTop: TsdPolyLine;
    FBtm: TsdPolyLine;
    FTopScore: double; // # connections towards top, weighted with spacing score
    FBtmScore: double; // # connections towards btm, weighted with spacing score
  public
    function Score: double;
    function IdSetCount: integer;
    property Top: TsdPolyLine read FTop;
    property Btm: TsdPolyLine read FBtm;
  end;

  // LIst of TsdLinkInfo objects
  TsdLinkInfoList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdLinkInfo;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    function ByTopBtm(ATop, ABtm: TsdPolyLine): TsdLinkInfo;
    property Items[Index: integer]: TsdLinkInfo read GetItems; default;
  end;

  // Collection of LaserLines, sorted by X-start value, increasing
  TsdPolyLine = class(TPersistent)
  private
    FLines: TsdLaserLineList;
    FLinks: TsdLinkInfoList;
    FId: integer;
    FLineId: integer;
    FLineIdIsSet: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function TotalPointCount: integer;
    procedure AddLinkToIsAbove(APoly: TsdPolyLine; const AScore: double);
    procedure AddLinkToIsBelow(APoly: TsdPolyLine; const AScore: double);
    // Chain up  the laser lines, from left to right
    procedure ChainLaserLines(const ASearchWidth, ASearchHeight: double);
    // Remove laser lines shorter than ALineLengthLimit from the polyline, and
    // return the number of lines removed.
    function RemoveShortLines(ALineLengthLimit: double): integer;
    // Get the X position of the CG for all points
    procedure GetXCgValues(AFirst: PDouble; ACount: integer);
    // Get intensity values of all points (ACount must equal TotalPointCount!)
    procedure GetIntensityValues(AFirst: PInteger; ACount: integer);
    // Get the perceived brightness of the polyline, this is an average intensity
    // value, excluding zero-intensity sections
    function GetBrightness: double;
    // List of laser lines (owned)
    property Lines: TsdLaserLineList read FLines;
    // List of links (shared)
    property Links: TsdLinkInfoList read FLinks write FLinks;
    // Internal ID, should not be changed once PL is created, and is unique
    // in the list of polylines
    property Id: integer read FId;
    // Laserplane's line ID in case of multi-line laser images, belonging to the
    // corresponding laser line. Multiple polylines can have the same LineID.
    property LineId: integer read FLineId write FLineId;
  end;

  // List of polylines
  TsdPolyLineList = class(TCustomSortedList)
  private
    FSortMethod: TsdLineSortMethod;
    function GetItems(Index: integer): TsdPolyLine;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    function ById(AId: integer): TsdPolyLine;
    function ByLineId(ALineId: integer): TsdPolyline;
    // Join polylines PL1 and PL2, PL1 will contain both, and PL2 ceases to exist
    procedure JoinPolyLines(PL1, PL2: TsdPolyLine);
    property Items[Index: integer]: TsdPolyLine read GetItems; default;
    property SortMethod: TsdLineSortMethod read FSortMethod write FSortMethod;
  end;

  // Connect multiple lines into polylines
  TsdLineConnector = class(TComponent)
  private
    FPolyLines: TsdPolyLineList;
    FLinks: TsdLinkInfoList;
    FDetector: TsdLaserPointDetector;
    FOnMessage: TStringEvent;
    FSearchHeight: double;
    FLineLengthLimit: double;
    FSearchWidth: double;
    FMedianLineSpacing: double;
  protected
    procedure DoMessage(const AMessage: string);
    procedure DetectAboveBelow(APoly: TsdPolyLine);
    procedure ClearPolyLine(APoly: TsdPolyLine);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    // Find connected lines and form a list of polylines from it
    procedure FindConnections;
    // Search width used to match End->Start point
    property SearchWidth: double read FSearchWidth write FSearchWidth;
    // Search height used to match End->Start point
    property SearchHeight: double read FSearchHeight write FSearchHeight;
    // Median line spacing, used to weigh the link influence (above and below
    // points must match this as good as possible)
    property MedianLineSpacing: double read FMedianLineSpacing write FMedianLineSpacing;
    // Single laser lines that are left over, and shorter than LineLengthLimit
    // will be removed.
    property LineLengthLimit: double read FLineLengthLimit write FLineLengthLimit;
    // Reference to the laser point detector
    property Detector: TsdLaserPointDetector read FDetector write FDetector;
    // List of polylines
    property PolyLines: TsdPolyLineList read FPolyLines;
    property OnMessage: TStringEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
  Math;

{ TsdLaserPoint }

procedure TsdLaserPoint.Assign(Source: TPersistent);
var
  S: TsdLaserPoint;
begin
  if Source is TsdLaserPoint then
  begin
    S := TsdLaserPoint(Source);
    FX := S.FX;
    FY := S.FY;
    FCount := S.FCount;
    FIntensity := S.FIntensity;
    FCG := S.FCG;
    FLftNbr := S.FLftNbr;
    FRgtNbr := S.FRgtNbr;
  end else
    inherited;
end;

function TsdLaserPoint.GetCG: PsdPoint2D;
begin
  Result := @FCG;
end;

function TsdLaserPoint.IsEndpt: boolean;
begin
  Result := IsLftEndpt or IsRgtEndpt;
end;

function TsdLaserPoint.IsFullyConnected: boolean;
begin
  Result := assigned(FLftNbr) and assigned(FRgtNbr);
end;

function TsdLaserPoint.IsHelper: boolean;
begin
  Result := (FIntensity = 0);
end;

function TsdLaserPoint.IsIsolated: boolean;
begin
  Result := (FLftNbr = nil) and (FRgtNbr = nil);
end;

function TsdLaserPoint.IsLftEndpt: boolean;
begin
  Result := (FLftNbr = nil) and (FRgtNbr <> nil);
end;

function TsdLaserPoint.IsRgtEndpt: boolean;
begin
  Result := (FLftNbr <> nil) and (FRgtNbr = nil);
end;

{ TsdLaserPointList }

procedure TsdLaserPointList.DetectNeighbours;
var
  i, j, Idx, NbCount, MaxI: integer;
  LP, Dummy: TsdLaserPoint;
begin
  // Assure we're sorted
  FSortMethod := psByXY;
  Sorted := True;
  Dummy := TsdLaserPoint.Create;
  try

    // Loop through the list
    for i := 0 to Count - 1 do
    begin
      LP := Items[i];
      Dummy.Assign(LP);

      // Find left neighbour with largest intensity
      Dummy.FX := LP.FX - 1;
      FindMultiple(Dummy, Idx, NbCount);
      MaxI := 0;
      for j := Idx to Idx + NbCount - 1 do
      begin
        if Items[j].FIntensity > MaxI then
        begin
          LP.FLftNbr := Items[j];
          MaxI := Items[j].FIntensity;
        end;
      end;

      // Find right neighbour with largest intensity
      Dummy.FX := LP.FX + 1;
      FindMultiple(Dummy, Idx, NbCount);
      MaxI := 0;
      for j := Idx to Idx + NbCount - 1 do
      begin
        if Items[j].FIntensity > MaxI then
        begin
          LP.FRgtNbr := Items[j];
          MaxI := Items[j].FIntensity;
        end;
      end;

    end;

    // Now nil all neighbours that have neighbours that don't point back
    for i := 0 to Count - 1 do
    begin
      LP := Items[i];
      if assigned(LP.FLftNbr) then
      begin
        if LP.FLftNbr.FRgtNbr <> LP then
          LP.FLftNbr := nil;
      end;
      if assigned(LP.FRgtNbr) then
      begin
        if LP.FRgtNbr.FLftNbr <> LP then
          LP.FRgtNbr := nil;
      end;
    end;

  finally
    Dummy.Free;
  end;
end;

function TsdLaserPointList.DoCompare(Item1, Item2: TObject): integer;
var
  LP1, LP2: TsdLaserPoint;
  // local: compare (vertical) intervals, if they overlap then return 0, if
  // [Y1L,Y1H] totally above (smaller) than [Y2L,Y2H], return -1 and if totally
  // below (larger), return 1.
  function CompareIntervals(Y1L, Y1H, Y2L, Y2H: integer): integer;
  begin
    if Y1L < Y2L then
    begin
      if Y1H < Y2L then
      begin
        Result := -1;
        exit;
      end;
    end;
    if Y1H > Y2H then
    begin
      if Y1L > Y2H then
      begin
        Result := 1;
        exit;
      end;
    end;
    Result := 0;
  end;
// main
begin
  LP1 := TsdLaserPoint(Item1);
  LP2 := TsdLaserPoint(Item2);
  case FSortMethod of
  psByXY:
    begin
      Result := CompareInteger(LP1.X, LP2.X);
      if Result = 0 then
      begin
        Result := CompareIntervals(LP1.Y, LP1.Y + LP1.Count, LP2.Y, LP2.Y + LP2.Count);
      end;
    end;
  psByIntensity:
    begin
      Result := CompareInteger(LP1.Intensity, LP2.Intensity);
    end;
  psXLeftToRight:
    begin
      Result := CompareInteger(LP1.X, LP2.X);
    end;
  psXRightToLeft:
    begin
      Result := -CompareInteger(LP1.X, LP2.X);
    end;

  else
    Result := 0;
  end;
end;

function TsdLaserPointList.GetItems(Index: integer): TsdLaserPoint;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

procedure TsdLaserPointList.MergePoints(IdxMain, IdxAdd: integer);
var
  LPM, LPA: TsdLaserPoint;
  IntensitySum: integer;
begin
  // Melt point at IdxAdd into point at IdxMain
  LPM := Items[IdxMain];
  LPA := Items[IdxAdd];
  IntensitySum := LPM.Intensity + LPA.Intensity;
  LPM.CG.Y := (LPM.CG.Y * LPM.Intensity + LPA.CG.Y * LPA.Intensity) / IntensitySum;
  LPM.FIntensity := IntensitySum;

  // Now delete the point at IdxAdd
  Delete(IdxAdd);
end;

{ TsdLaserLine }

constructor TsdLaserLine.Create(AOwner: TsdLaserPointDetector);
begin
  inherited Create;
  FOwner := AOwner;
  FPoints := TsdLaserPointList.Create(False);
end;

destructor TsdLaserLine.Destroy;
begin
  FreeAndNil(FPoints);
  inherited;
end;

function TsdLaserLine.GetLft: TsdLaserPoint;
begin
  Result := FPoints[0];
end;

function TsdLaserLine.GetRgt: TsdLaserPoint;
begin
  Result := FPoints[FPoints.Count - 1];
end;

procedure TsdLaserLine.DeleteSegment(AIndex, ACount: integer);
var
  Idx: integer;
  P, Q: TsdLaserPoint;
begin
  // Left neighbour
  P := FPoints[AIndex];
  if assigned(P) then
  begin
    Q := P.FLftNbr;
    if assigned(Q) then
      Q.FRgtNbr := nil;
  end;

  // Delete the points..
  while ACount > 0 do
  begin
    P := FPoints[AIndex];
    if assigned(P) then
    begin
      // Right neighbour
      Q := P.FRgtNbr;
      if assigned(Q) then
        Q.FLftNbr := nil;

      // Delete the point
      FPoints.Delete(AIndex);
      if FOwner.FPoints.Find(P, Idx) then
        FOwner.FPoints.Delete(Idx);
    end;
    dec(ACount);
  end;

  // Verify RgtNbr/LftNbr of Idx point
  P := FPoints[AIndex];
  if assigned(P) then
  begin
    Q := FPoints[AIndex + 1];
    P.FRgtNbr := Q;
    if assigned(Q) then
      Q.FLftNbr := P;
    Q := FPoints[AIndex - 1];
    P.FLftNbr := Q;
    if assigned(Q) then
      Q.FRgtNbr := P;
  end;
end;

class procedure TsdLaserLine.BuildHelperLine(P1, P2: TsdLaserPoint; APointList: TsdLaserPointList);
var
  i, Len: integer;
  Pts: array of TsdLaserPoint;
  DyDx: double;
  H: TsdLaserPoint;
begin
  if (P1 = nil) or (P2 = nil) or (APointList = nil) then
    exit;

  Len := P2.X - P1.X + 1;
  if Len <= 1 then
    exit;

  SetLength(Pts, Len);
  Pts[0]       := P1;
  Pts[Len - 1] := P2;

  // Dy/Dx
  DyDx := (P2.CG.Y - P1.CG.Y) / (Len - 1);

  // Create intermediate points
  for i := 1 to Len - 2 do
  begin
    H := TsdLaserPoint.Create;
    H.FX := Pts[0].FX + i;
    H.CG.X := H.FX + 0.5;
    H.CG.Y := Pts[0].CG.Y + DyDx * i;
    H.FY := round(H.CG.Y - 0.5);
    H.FCount := 1;
    H.FIntensity := 0;
    Pts[i] := H;
    // add H to our master list
    APointList.Add(H);
  end;

  // Connect the chain
  for i := 1 to Len - 2 do
  begin
    Pts[i].FLftNbr := Pts[i - 1];
    Pts[i].FRgtNbr := Pts[i + 1];
  end;
  P1.FRgtNbr := Pts[1];
  P2.FLftNbr := Pts[Len - 2];

end;

function TsdLaserLine.GetLineLength: double;
var
  P1, P2: TsdLaserPoint;
begin
  P1 := GetLft;
  P2 := GetRgt;
  if (P1 = nil) or (P2 = nil) then
  begin
    Result := 0;
    exit;
  end else
  begin
    Result := sqrt(sqr(P2.Cg.X - P1.Cg.X) + sqr(P2.Cg.Y - P1.Cg.Y));
  end;
end;

function TsdLaserLine.SlopeAt(Idx, AWidth: integer): double;
var
  P1, P2: TsdLaserPoint;
  D: integer;
begin
  Result := 0;
  if AWidth <= 0 then
    exit;

  P1 := Points[Idx - AWidth];
  P2 := Points[Idx + AWidth];

  D := 2 * AWidth;

  // Endpoints?
  if P1 = nil then
  begin
    D := AWidth;
    P1 := Points[Idx];
  end;
  if P2 = nil then
  begin
    D := AWidth;
    P2 := Points[Idx];
  end;

  // Still no luck? (should not happen)
  if (P1 = nil) or (P2 = nil) then
    exit;

  Result := (P2.Cg.Y - P1.Cg.Y) / D;
end;

{ TsdLaserLineList }

function TsdLaserLineList.DoCompare(Item1, Item2: TObject): integer;
var
  LL1, LL2: TsdLaserLine;
begin
  LL1 := TsdLaserLine(Item1);
  LL2 := TsdLaserLine(Item2);

  case FSortMethod of

  lsByPointCount:
    // sort the lines by pointcount, longest to shortest
    Result := -CompareInteger(LL1.Points.Count, LL2.Points.Count);

  lsXLeftToRight:
    Result := CompareInteger(LL1.Lft.X, LL2.Lft.X);

  else
    Result := 0;
  end;
end;

function TsdLaserLineList.GetItems(Index: integer): TsdLaserLine;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

procedure TsdLaserLineList.JoinLines(LL1, LL2: TsdLaserLine);
var
  i, Idx1, Idx2, Dx, Intensity1, Intensity2: integer;
  P, Q: TsdLaserPoint;
begin
  // Check for overlap
  Dx := LL1.Rgt.X - LL2.Lft.X + 1;

  if Dx > 0 then
  begin
    // Calculate sum of intensities for both segments
    Intensity1 := 0;
    Intensity2 := 0;
    Idx1 := LL1.Points.Count - Dx;
    Idx2 := 0;
    for i := 0 to Dx - 1 do
    begin
      P := LL1.Points[Idx1 + i];
      if assigned(P) then
        inc(Intensity1, P.Intensity);
      P := LL2.Points[Idx2 + i];
      if assigned(P) then
        inc(Intensity2, P.Intensity);
    end;

    // Delete the segment from the line with smallest total intensity
    if Intensity1 < Intensity2 then
      LL1.DeleteSegment(Idx1, Dx)
    else
      LL2.DeleteSegment(Idx2, Dx);

  end;

  // Check for spacing
  if Dx < 0 then
  begin
    TsdLaserLine.BuildHelperLine(LL1.Rgt, LL2.Lft, LL1.FOwner.FPoints);
  end;

  // Now add the points of L2 to L1
  P := LL1.Rgt;
  for i := 0 to LL2.Points.Count - 1 do
  begin
    Q := LL2.Points[i];
    if i = 0 then
    begin
      Q.FLftNbr := P;
      if assigned(P) then
        P.FRgtNbr := Q;
    end;
    LL1.Points.Add(Q);
  end;

  // And delete L2
  Remove(LL2);
end;

function TsdLaserLineList.LongestLineIdx: integer;
var
  i, Len, MaxLen: integer;
begin
  MaxLen := -1;
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    Len := Items[i].Points.Count;
    if Len > MaxLen then
    begin
      Result := i;
      MaxLen := Len;
    end;
  end;
end;

{ TsdLaserPointDetector }

procedure TsdLaserPointDetector.Clear;
begin
  FPoints.Clear;
  FLines.Clear;
end;

constructor TsdLaserPointDetector.Create(AOwner: TComponent);
begin
  inherited;
  FPoints := TsdLaserPointList.Create(True);
  FLines := TsdLaserLineList.Create(True);
end;

destructor TsdLaserPointDetector.Destroy;
begin
  FreeAndNil(FPoints);
  FreeAndNil(FLines);
  inherited;
end;

procedure TsdLaserPointDetector.DetectLines;
var
  i: integer;
  LP: TsdLaserPoint;
  LL: TsdLaserLine;
begin
  FLines.Clear;

  // Create chains of connected points; start with points that are only right-connected,
  // then follow the chain, until the last point that is only left-connected.
  for i := 0 to FPoints.Count - 1 do
  begin
    LP := FPoints[i];
    if LP.IsLftEndpt then
    begin

      // Found a start point
      LL := TsdLaserLine.Create(Self);
      while assigned(LP.RgtNbr) do
      begin
        LL.Points.Add(LP);
        LP := LP.RgtNbr;
      end;

      // Add last one
      LL.Points.Add(LP);

      // And add the line. When it gets added it will be sorted on pointcount (largest first)
      FLines.Add(LL);

    end;
  end;
end;

procedure TsdLaserPointDetector.DetectPoints8bit(AInputMap, AThresholdMap: TsdMapIterator; AThreshold: integer);
// After this algorithm, FPoints contains a list of feature points, that are sorted first in X, then in Y.
var
  X, Y, W, H, T, Dot: integer;
  IP, TP: PByte;
  UTM: boolean;
  LP: TsdLaserPoint;
  // local
  procedure FinalizeFP;
  begin
    // The CG is located at 0.5, 0.5 from calculated CG (center of the pixel)
    LP.FCG.X := LP.X + 0.5;
    LP.FCG.Y := LP.Y + 0.5 + Dot / LP.Intensity;
    LP := nil;
  end;
// main
begin
  // Clear all possible data
  Clear;
  LP := nil;
  TP := nil;
  FPoints.Sorted := False;
  FPoints.SortMethod := psByXY;

  W := AInputMap.Width;
  H := AInputMap.Height;

  // Use Threshold Map?
  UTM := assigned(AThresholdMap);
  if UTM then
    if (W <> AThresholdMap.Width) or (H <> AThresholdMap.Height) then
      raise exception.Create('Input and threshold maps not the same size');

  for X := 0 to W - 1 do
  begin
    IP := AInputMap.At(X, 0);
    if UTM then
      TP := AThresholdMap.At(X, 0);

    for Y := 0 to H - 1 do
    begin

      // threshold: the fixed threshold plus the threshold of the map
      T := AThreshold;
      if UTM then
        inc(T, TP^);

      // pixel present (equal or above threshold)?
      if IP^ >= T then
      begin

        // Pixel is present, do we already have an FP?
        if assigned(LP) then
        begin

          // add to current FP
          inc(LP.FCount);
          inc(LP.FIntensity, IP^);
          inc(Dot, (Y - LP.Y) * IP^);

        end else
        begin

          // Create a new FP
          LP := TsdLaserPoint.Create;
          LP.FX := X;
          LP.FY := Y;
          LP.FCount := 1;
          LP.FIntensity := IP^;
          FPoints.Add(LP);
          Dot := 0;

        end;

      end else
      begin

        // pixel is not present, finalize a possible FP
        if assigned(LP) then
          FinalizeFP;

      end;

      // increment pointers
      inc(IP, AInputMap.ScanStride);
      if UTM then
        inc(TP, AThresholdMap.ScanStride);

    end;

    // finalize a possible FP after finishing a column
    if assigned(LP) then
      FinalizeFP;

  end;

  // Sort the points by X then Y
  FPoints.Sorted := True;

end;

function TsdLaserPointDetector.PointAbove(APoint: TsdLaserPoint): TsdLaserPoint;
var
  Idx: integer;
begin
  Result := nil;
  if FPoints.Find(APoint, Idx) then
  begin
    Result := FPoints[Idx - 1];
    if assigned(Result) and (Result.X <> APoint.X) then
      Result := nil;
  end;
end;

function TsdLaserPointDetector.PointBelow(APoint: TsdLaserPoint): TsdLaserPoint;
var
  Idx: integer;
begin
  Result := nil;
  if FPoints.Find(APoint, Idx) then
  begin
    Result := FPoints[Idx + 1];
    if assigned(Result) and (Result.X <> APoint.X) then
      Result := nil;
  end;
end;

procedure TsdLaserPointDetector.SetLineIndices;
var
  i, j: integer;
  L: TsdLaserLine;
begin
  // first reset all to -1
  for i := 0 to FPoints.Count - 1 do
    FPoints[i].FLineIdx := -1;

  // now set the line indices
  for i := 0 to FLines.Count - 1 do
  begin
    L := FLines[i];
    for j := 0 to L.Points.Count - 1 do
      L.Points[j].FLineIdx := i;
  end;
end;

{ TsdTunnel }

procedure TsdTunnel.BuildSlope(ASeedIdx: integer; ASlope: double; ASearch: TsdSearchDirection);
begin
  FIsValid := True;
  FSearch := ASearch;

  // Seed point
  FSeed := FPoints[ASeedIdx];

  // Slope
  FRatio := ASlope;
end;

procedure TsdTunnel.BuildTunnel(ASeedIdx: integer; ASearch: TsdSearchDirection);
begin
  FIsValid := True;
  FSearch := ASearch;

  // Seed, Top and Btm points
  FSeed := FPoints[ASeedIdx];
  FTop := FPoints[ASeedIdx - 1];
  FBtm := FPoints[ASeedIdx + 1];

  // Check if Top and Btm points actually exist
  if not assigned(FTop) or (FTop.FX <> FSeed.FX) then
  begin
    FIsValid := False;
    exit;
  end;
  if not assigned(FBtm) or (FBtm.FX <> FSeed.FX) then
  begin
    FIsValid := False;
    exit;
  end;

  // Ratio
  FRatio := (FSeed.CG.Y - FTop.CG.Y) / (FBtm.CG.Y - FTop.CG.Y);
end;

function TsdTunnel.Connect: boolean;
var
  P1, P2: TsdLaserPoint;
begin
  Result := True;

  // Make sure we have a target
  if FTarget = nil then
    raise Exception.Create('Target = nil');

  // Set start/end point S and C, also verify if we can connect (things might've
  // changed due to connections elsewhere)
  if FSearch = sdLeft then
  begin
    // target to the left
    if assigned(FSeed.LftNbr) or assigned(FTarget.RgtNbr) then
    begin
      Result := False;
      exit;
    end;
    P1 := FTarget;
    P2 := FSeed;
  end else
  begin
    // target to the right
    if assigned(FSeed.RgtNbr) or assigned(FTarget.LftNbr) then
    begin
      Result := False;
      exit;
    end;
    P1 := FSeed;
    P2 := FTarget;
  end;

  TsdLaserLine.BuildHelperLine(P1, P2, FPoints);

end;

constructor TsdTunnel.Create;
begin
  inherited;
  FCPool := TsdLaserPointList.Create(False);
end;

destructor TsdTunnel.Destroy;
begin
  FreeAndNil(FCPool);
  inherited;
end;

function TsdTunnel.SearchSlopeStep(AStep: integer; const ASearchHeight: double): boolean;
var
  i, Idx, X, Yl, Yh, Count: integer;
  CP: TsdLaserPoint;
  Dummy: TsdLaserPoint;
  Dist, Yc: double;
begin
  Result := True;

  // Find new top/btm points
  case FSearch of
  sdLeft:
    begin
      Yc := FSeed.Cg.Y - AStep * FRatio;
      X := FSeed.X - AStep;
    end;
  sdRight:
    begin
      Yc := FSeed.Cg.Y + AStep * FRatio;
      X := FSeed.X + AStep;
    end;
  else
    Yc := 0; // avoid warning
    X := 0;
  end;

  Yl := trunc(Yc - ASearchHeight);
  Yh := trunc(Yc + ASearchHeight + 1);

  Dummy := TsdLaserPoint.Create;
  try
    Dummy.FX := X;
    Dummy.FY := Yl;
    Dummy.FCount := Yh - Yl;

    FPoints.FindMultiple(Dummy, Idx, Count);

    for i := Idx to Idx + Count - 1 do
    begin
      // Possible candidate point
      CP := FPoints[i];
      if CP = nil then // should not happen
      begin
        raise Exception.Create('CP = nil');
      end;

      // Vertical fictive distance
      Dist := abs(CP.CG.Y - Yc);

      // We do not allow it to be already connected towards our direction
      case FSearch of
      sdLeft:
        if assigned(CP.RgtNbr) then
        begin
          Result := False;
          exit;
        end;
      sdRight:
        if assigned(CP.LftNbr) then
        begin
          Result := False;
          exit;
        end;
      end;

      // Score formula
      CP.Score := sqr(Dist) + Sqr(AStep);

      // Add the potential candidate
      FCPool.Add(CP);
    end;
  finally
    Dummy.Free;
  end;
end;

function TsdTunnel.SearchTunnelStep(AStep: integer; const ASearchHeight: double): boolean;
var
  i, Idx1, Idx2: integer;
  Res: boolean;
  CP: TsdLaserPoint;
  NewTop, NewBtm: TsdLaserPoint;
  Dist: double;
begin
  Result := True;
  NewTop := nil;
  NewBtm := nil;

  // Find new top/btm points
  case FSearch of
  sdLeft:
    begin
      NewTop := FTop.FLftNbr;
      NewBtm := FBtm.FLftNbr;
    end;
  sdRight:
    begin
      NewTop := FTop.FRgtNbr;
      NewBtm := FBtm.FRgtNbr;
    end;
  end;

  if (NewTop = nil) or (NewBtm = nil) then
  begin
    // We're strung up, cannot move on
    Result := False;
    exit;
  end;

  if (abs(NewTop.X - FTop.X) <> 1) or (abs(NewBtm.X - FBtm.X) <> 1) then
  begin
    // should not happen
    raise Exception.Create('X interval <> 1');
  end;

  // For next step, only when we had no problems searching
  FTop := NewTop;
  FBtm := NewBtm;

  // Find candidate(s)
  Res := FPoints.Find(FTop, Idx1);
  if not Res then
  begin // should not happen
    raise Exception.Create('Top not found');
  end;

  Res := FPoints.Find(FBtm, Idx2);
  if not Res then
  begin // should not happen
    raise Exception.Create('Btm not found');
  end;

  for i := Idx1 + 1 to Idx2 - 1 do
  begin
    // Possible candidate point
    CP := FPoints[i];
    if CP = nil then // should not happen
    begin
      raise Exception.Create('CP = nil');
    end;

    // Vertical fictive distance
    Dist := FTop.CG.Y + FRatio * (FBtm.CG.Y - FTop.CG.Y) - CP.CG.Y;

    // Outside search height?
    if abs(Dist) > 0.5 * ASearchHeight then
      continue;

    // But we do not allow it to be already connected towards our direction
    case FSearch of
    sdLeft:
      if assigned(CP.RgtNbr) then
      begin
        Result := False;
        exit;
      end;
    sdRight:
      if assigned(CP.LftNbr) then
      begin
        Result := False;
        exit;
      end;
    end;

    // Score formula
    CP.Score := sqr(Dist) + Sqr(AStep);

    // Add the potential candidate
    FCPool.Add(CP);
  end;

end;

procedure TsdTunnel.SearchSlopeTarget(const ASearchHeight: double; ASearchBracket: integer);
var
  i, Step: integer;
  Res: boolean;
begin
  // Clear
  FTarget := nil;
  FCPool.Clear;

  Res := True;
  for Step := 1 to ASearchBracket do
  begin
    Res := SearchSlopeStep(Step, ASearchHeight);
    if not Res then
      break;
  end;

  // Any candidates?
  if FCPool.Count = 0 then
  begin
    // If searched, only valid if Res = True
    FIsValid := Res;
    exit;
  end;

  // Find best candidate
  FTarget := FCPool[0];
  FScore := FTarget.Score;
  for i := 1 to FCPool.Count - 1 do
  begin
    if FCPool[i].Score < FScore then
    begin
      FTarget := FCPool[i];
      FScore := FTarget.Score;
    end;
  end;
end;

procedure TsdTunnel.SearchTunnelTarget(const ANominalTunnelHeight, ASearchHeight: double; ASearchBracket: integer);
var
  i, Step: integer;
  Res: boolean;
  Adjust: double;
begin
  // Clear
  FTarget := nil;
  FCPool.Clear;

  // Score adjustment for tunnel height
  Adjust := sqr(ANominalTunnelHeight - (FBtm.CG.Y - FTop.CG.Y));

  Res := True;
  for Step := 1 to ASearchBracket do
  begin
    Res := SearchTunnelStep(Step, ASearchHeight);
    if not Res then
      break;
  end;

  // searched till end of tunnel?
  if Res then
  begin
    // Clear FTop and FBottom
    FTop := nil;
    FBtm := nil;
  end;

  // Any candidates?
  if FCPool.Count = 0 then
  begin
    // If searched till end then not valid
    FIsValid := assigned(FTop) or assigned(FBtm);
    exit;
  end;

  // Find best candidate
  FTarget := FCPool[0];
  FScore := FTarget.Score;
  for i := 1 to FCPool.Count - 1 do
  begin
    if FCPool[i].Score < FScore then
    begin
      FTarget := FCPool[i];
      FScore := FTarget.Score;
    end;
  end;

  // Adjust the score for deviation from nominal tunnel height
  FScore := FScore + 0.1 * Adjust;

end;

{ TsdTunnelList }

function TsdTunnelList.BySeed(ASeed: TsdLaserPoint): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Seed = ASeed then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TsdTunnelList.BySeedAndSearch(ASeed: TsdLaserPoint; ASearch: TsdSearchDirection): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].Seed = ASeed) and (Items[i].Search = ASearch) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TsdTunnelList.ByTopBtm(APoint: TsdLaserPoint): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].Top = APoint) or (Items[i].Btm = APoint) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TsdTunnelList.DoCompare(Item1, Item2: TObject): integer;
var
  Idx: integer;
  T1, T2: TsdTunnel;
begin
  // Sort by score (ascending)
  T1 := TsdTunnel(Item1);
  T2 := TsdTunnel(Item2);

  Idx := 0;
  if assigned(T1.Target) then
    inc(Idx);
  if assigned(T2.Target) then
    inc(Idx, 2);

  case Idx of
  0: // both nil, not important case.. just use ratio
    Result := CompareDouble(T1.FRatio, T2.FRatio);
  1: // T1's target is assigned, so wins
    Result := -1;
  2: // T2's target is assigned so wins
    Result := 1;
  3: // both targets assigned, so compare score
    Result := CompareDouble(T1.Score, T2.Score);
  else
    Result := 0; // should not happen
  end;
end;

function TsdTunnelList.GetItems(Index: integer): TsdTunnel;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

procedure TsdTunnelList.RemoveBySeed(ASeed: TsdLaserPoint);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if Items[i].Seed = ASeed then
      Delete(i);
end;

{ TsdTunnelSearch }

procedure TsdTunnelSearch.AddSlopesForSeed(ASeed: TsdLaserPoint; var T: TsdTunnel);
var
  SeedIdx: integer;
  Res: boolean;
  // local
  procedure SlopeSearchAndAdd;
  begin
    T.SearchSlopeTarget(FSearchHeight, FSearchBracket);
    if T.IsValid then
    begin
      // Add, and sort the slope by score.
      FTunnels.Add(T);
      T := NewTunnel;
    end;
  end;
// main
begin
  Res := FPoints.Find(ASeed, SeedIdx);
  if not Res then
    raise exception.Create('Seed not found');

  // Remove any existing ones first
  FTunnels.RemoveBySeed(ASeed);

  // Is it already fully connected?
  if ASeed.IsFullyConnected then
  begin
    // Remove it from the pool
    FPool.Remove(ASeed);
    exit;
  end;

  // Check left side
  if ASeed.FLftNbr = nil then
  begin
    // Search left
    T.BuildSlope(SeedIdx, FSlope, sdLeft);
    SlopeSearchAndAdd;
  end;

  // Check right side
  if ASeed.FRgtNbr = nil then
  begin
    // Search right
    T.BuildSlope(SeedIdx, FSlope, sdRight);
    SlopeSearchAndAdd;
  end;
end;

procedure TsdTunnelSearch.AddTunnelsForSeed(ASeed: TsdLaserPoint; var T: TsdTunnel);
var
  SeedIdx: integer;
  Res: boolean;
  // local
  procedure TunnelSearchAndAdd;
  begin
    T.SearchTunnelTarget(FNominalTunnelHeight, FSearchHeight, FSearchBracket);
    if T.IsValid then
    begin
      // Add, and sort the tunnel by score. Tunnels without targets are still in
      // here, but at the end of the list. They might come active once their Top/Btm
      // points are getting connected.
      FTunnels.Add(T);
      T := NewTunnel;
    end;
  end;
// main
begin
  Res := FPoints.Find(ASeed, SeedIdx);
  if not Res then
    raise exception.Create('Seed not found');

  // Remove any existing ones first
  FTunnels.RemoveBySeed(ASeed);

  // Is it already fully connected?
  if ASeed.IsFullyConnected then
  begin
    // Remove it from the pool
    FPool.Remove(ASeed);
    exit;
  end;

  // Check left side
  if ASeed.FLftNbr = nil then
  begin
    // Search left
    T.BuildTunnel(SeedIdx, sdLeft);
    if T.IsValid then
      TunnelSearchAndAdd;
  end;

  // Check right side
  if ASeed.FRgtNbr = nil then
  begin
    // Search right
    T.BuildTunnel(SeedIdx, sdRight);
    if T.IsValid then
      TunnelSearchAndAdd;
  end;
end;

procedure TsdTunnelSearch.BuildPool;
var
  i: integer;
  LP: TsdLaserPoint;
begin
  FPool.Clear;

  // Add points to the pool
  for i := 0 to FPoints.Count - 1 do
  begin
    LP := FPoints[i];

    // Add isolated points and endpoints to the pool
    if not LP.IsFullyConnected then
      FPool.Add(LP);

  end;
end;

procedure TsdTunnelSearch.BuildSlopeList;
var
  i: integer;
  Seed: TsdLaserPoint;
  T: TsdTunnel;
begin
  FTunnels.Clear;

  T := NewTunnel;

  // Loop through the pool, build slopes, add to our list
  for i := 0 to FPool.Count - 1 do
  begin
    Seed := FPool[i];

    // Add one or two slopes, depending on if it is an isolated seed, or lft/rgt connected
    AddSlopesForSeed(Seed, T);
  end;

  // Delete the obsolete slope candidate we created
  FreeAndNil(T);

end;

procedure TsdTunnelSearch.BuildTunnelList;
var
  i: integer;
  Seed: TsdLaserPoint;
  T: TsdTunnel;
begin
  FTunnels.Clear;

  T := NewTunnel;

  // Loop through the pool, build tunnels, add to our list
  for i := 0 to FPool.Count - 1 do
  begin
    Seed := FPool[i];

    // Add one or two tunnels, depending on if it is an isolated seed, or lft/rgt connected
    AddTunnelsForSeed(Seed, T);
  end;

  // Delete the obsolete tunnel candidate we created
  FreeAndNil(T);

end;

constructor TsdTunnelSearch.Create(AOwner: TComponent);
begin
  inherited;
  FPool := TsdLaserPointList.Create(False);
  FTunnels := TsdTunnelList.Create(True);
  // defaults
  FNominalTunnelHeight := 20.0;
  FSearchBracket := 20;
  FSearchHeight := 5.0;
end;

destructor TsdTunnelSearch.Destroy;
begin
  FreeAndNil(FPool);
  FreeAndNil(FTunnels);
  inherited;
end;

procedure TsdTunnelSearch.DoMessage(const AMessage: string);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, AMessage)
end;

procedure TsdTunnelSearch.MakeSlopeConnections;
var
  i: integer;
  T, Tunnel: TsdTunnel;
  S, C, CFail: TsdLaserPoint;
  Res: boolean;
  ConnectionsMade: integer;
begin
  DoMessage('Make Slope Connections');

  // Build pool
  BuildPool;
  DoMessage(Format('Poolcount (initial): %d', [FPool.Count]));

  // Build slope list
  BuildSlopeList;
  DoMessage(Format('Slope count (initial): %d', [FTunnels.Count]));

  // We use the tunnel class for the slope connections too 
  T := NewTunnel;
  try
    ConnectionsMade := 0;

    while Tunnels.Count > 0 do
    begin
      Tunnel := Tunnels[0];
      S := Tunnel.Seed;
      C := Tunnel.Target;

      // No target?
      if not assigned(C) then
        // No longer a valid tunnel, we're finished
        break;

      // Connect S -> C
      Res := Tunnel.Connect;
      if Res then
        inc(ConnectionsMade);

      // Remove S and C from pool if they're now fully connected
      if S.IsFullyConnected then
        FPool.Remove(S);
      if C.IsFullyConnected then
        FPool.Remove(C);

      // Re-add any failed candidates
      for i := 0 to Tunnel.CPool.Count - 1 do
      begin
        CFail := Tunnel.CPool[i];
        if CFail = C then
          continue;
        AddSlopesForSeed(CFail, T);
      end;

      // Remove any tunnel connected to S (that's us, for one)
      FTunnels.RemoveBySeed(S);

      // Remove any tunnel connected to C
      FTunnels.RemoveBySeed(C);

      // Re-add if not fully connected
      if not S.IsFullyConnected then
        AddSlopesForSeed(S, T);
      if not C.IsFullyConnected then
        AddSlopesForSeed(C, T);

    end;

    BuildPool;

    DoMessage(Format('Poolcount (final): %d', [FPool.Count]));
    DoMessage(Format('Connections made: %d', [ConnectionsMade]));

  finally
    FreeAndNil(T);
  end;
end;

procedure TsdTunnelSearch.MakeTunnelConnections;
var
  i, Idx: integer;
  T, Tunnel: TsdTunnel;
  S, C, CFail: TsdLaserPoint;
  Seeds: TsdLaserPointList;
  Res: boolean;
  ConnectionsMade: integer;
begin
  DoMessage('Make Tunnel Connections');

  // Build pool
  BuildPool;
  DoMessage(Format('Poolcount (initial): %d', [FPool.Count]));

  // Build tunnel list
  BuildTunnelList;
  DoMessage(Format('Tunnelcount (initial): %d', [FTunnels.Count]));

  T := NewTunnel;
  Seeds := TsdLaserPointList.Create(False);
  try
    ConnectionsMade := 0;

    while Tunnels.Count > 0 do
    begin
      Tunnel := Tunnels[0];
      S := Tunnel.Seed;
      C := Tunnel.Target;

      // No target?
      if not assigned(C) then
        // No longer a valid tunnel, we're finished
        break;

      // Connect S -> C
      Res := Tunnel.Connect;
      if Res then
        inc(ConnectionsMade);

      // Remove S and C from pool if they're now fully connected
      if S.IsFullyConnected then
        FPool.Remove(S);
      if C.IsFullyConnected then
        FPool.Remove(C);

      // Re-add any failed candidates
      for i := 0 to Tunnel.CPool.Count - 1 do
      begin
        CFail := Tunnel.CPool[i];
        if CFail = C then
          continue;
        AddTunnelsForSeed(CFail, T);
      end;

      // Remove any tunnel connected to S (that's us, for one)
      FTunnels.RemoveBySeed(S);

      // Remove any tunnel connected to C
      FTunnels.RemoveBySeed(C);

      // Re-add if not fully connected
      if not S.IsFullyConnected then
        AddTunnelsForSeed(S, T);
      if not C.IsFullyConnected then
        AddTunnelsForSeed(C, T);

      Seeds.Clear;

      // Re-add stuck-up tunnels that have Top or Btm as S
      repeat
        Idx := Tunnels.ByTopBtm(S);
        if Idx >= 0 then
        begin
          Seeds.AddUnique(Tunnels[Idx].Seed);
          Tunnels.Delete(Idx);
        end;
      until Idx < 0;

      // Re-add stuck-up tunnels that have Top or Btm as C
      repeat
        Idx := Tunnels.ByTopBtm(C);
        if Idx >= 0 then
        begin
          Seeds.AddUnique(Tunnels[Idx].Seed);
          Tunnels.Delete(Idx);
        end;
      until Idx < 0;

      for i := 0 to Seeds.Count - 1 do
        AddTunnelsForSeed(Seeds[i], T);

    end;

    BuildPool;

    DoMessage(Format('Poolcount (final): %d', [FPool.Count]));
    DoMessage(Format('Connections made: %d', [ConnectionsMade]));

  finally
    FreeAndNil(Seeds);
    FreeAndNil(T);
  end;
end;

function TsdTunnelSearch.NewTunnel: TsdTunnel;
begin
  // new tunnel candidate
  Result := TsdTunnel.Create;
  // Add reference to master point list
  Result.FPoints := FPoints;
end;

{ TsdLinkInfo }

function TsdLinkInfo.Score: double;
begin
  Result := FTopScore + FBtmScore;
end;

function TsdLinkInfo.IdSetCount: integer;
begin
  Result := 0;
  if FTop.FLineIdIsSet then
    inc(Result);
  if FBtm.FLineIdIsSet then
    inc(Result);
end;

{ TsdLinkInfoList }

function TsdLinkInfoList.ByTopBtm(ATop, ABtm: TsdPolyLine): TsdLinkInfo;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].Top = ATop) and (items[i].Btm = ABtm) then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TsdLinkInfoList.DoCompare(Item1, Item2: TObject): integer;
var
  L1, L2: TsdLinkInfo;
const
  cIsSetCountToOrder: array[0..2] of integer = (1, 0, 2);
begin
  L1 := TsdLinkInfo(Item1);
  L2 := TsdLinkInfo(Item2);

  // Get the IdSetCounts of 1 to the front..
  Result := CompareInteger(cIsSetCountToOrder[L1.IdSetCount], cIsSetCountToOrder[L2.IdSetCount]);

  // If equal, sort by score
  if Result = 0 then
    Result := -CompareDouble(L1.Score, L2.Score);

end;

function TsdLinkInfoList.GetItems(Index: integer): TsdLinkInfo;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TsdPolyLine }

procedure TsdPolyLine.AddLinkToIsAbove(APoly: TsdPolyLine; const AScore: double);
var
  L: TsdLinkInfo;
begin
  if AScore <= 0 then
    exit;

  L := FLinks.ByTopBtm(Self, APoly);
  if not assigned(L) then
  begin
    L := TsdLinkInfo.Create;
    L.FTop := Self;
    L.FBtm := APoly;
    FLinks.Add(L);
  end;
  L.FTopScore := L.FTopScore + AScore;
end;

procedure TsdPolyLine.AddLinkToIsBelow(APoly: TsdPolyLine; const AScore: double);
var
  L: TsdLinkInfo;
begin
  if AScore <= 0 then
    exit;

  L := FLinks.ByTopBtm(APoly, Self);
  if not assigned(L) then
  begin
    L := TsdLinkInfo.Create;
    L.FTop := APoly;
    L.FBtm := Self;
    FLinks.Add(L);
  end;
  L.FBtmScore := L.FBtmScore + AScore;
end;

procedure TsdPolyLine.ChainLaserLines(const ASearchWidth, ASearchHeight: double);
var
  Idx: integer;
  Ex, Ey: double;
  LL1, LL2: TsdLaserLine;
  LP1, LP2: TsdLaserPoint;
begin
  if FLines.Count < 2 then
    exit;

  // We already have a set of feature lines, sorted by X coordinate

  // Loop through the lines, connect lines that have an endpoint match
  Idx := 0;
  while (Idx <= Lines.Count - 2) do
  begin
    LL1 := Lines[Idx];
    LL2 := Lines[Idx + 1];
    LP1 := LL1.Rgt;
    LP2 := LL2.Lft;

    // Determine closeness of endpoints, within ellipse with axis lengths
    // ASearchWidth (hor), ASearchHeight (vert)
    Ex := (LP2.Cg.X - LP1.Cg.X) / ASearchWidth;
    Ey := (LP2.Cg.Y - LP1.Cg.Y) / ASearchHeight;

    if sqr(Ex) + sqr(Ey) < 1 then
    begin

      // Close enough; so join these. We don't increase the index, since the
      // updated LL1 contains LL2.
      FLines.JoinLines(LL1, LL2);

      // Also remove the line from its owner
      LL2.FOwner.FLines.Remove(LL2);

    end else
      inc(Idx);
  end;

end;

constructor TsdPolyLine.Create;
begin
  inherited;
  FLines := TsdLaserLineList.Create(False);
  FLines.SortMethod := lsXLeftToRight;
  FLineId := 0;
end;

destructor TsdPolyLine.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

procedure TsdPolyLine.GetXCgValues(AFirst: PDouble; ACount: integer);
var
  i, j: integer;
  LL: TsdLaserLine;
begin
  if ACount <> TotalPointCount then
    exit;

  // Copy XCg values to array
  for i := 0 to Lines.Count - 1 do
  begin
    LL := Lines[i];
    for j := 0 to LL.Points.Count - 1 do
    begin
      AFirst^ := LL.Points[j].Cg.X;
      inc(AFirst);
    end;
  end;
end;

procedure TsdPolyLine.GetIntensityValues(AFirst: PInteger; ACount: integer);
var
  i, j: integer;
  LL: TsdLaserLine;
begin
  if ACount <> TotalPointCount then
    exit;

  // Copy intensity values to array
  for i := 0 to Lines.Count - 1 do
  begin
    LL := Lines[i];
    for j := 0 to LL.Points.Count - 1 do
    begin
      AFirst^ := LL.Points[j].Intensity;
      inc(AFirst);
    end;
  end;
end;

function TsdPolyLine.GetBrightness: double;
var
  Idx, Len, BMin: integer;
  B: array of integer;
begin
  Result := 0;

  // List of intensity values for each point in the polyline
  Len := TotalPointCount;
  if Len = 0 then
    exit;

  SetLength(B, Len);

  // Copy intensity values to B
  GetIntensityValues(@B[0], Len);

  // sort this list
  sdSortArrayInteger(@B[0], Len);

  // Skip all the 0 values
  BMin := 1;
  Idx := 0;
  while (B[Idx] < BMin) and (Idx < Len - 1) do
    inc(Idx);

  // Take the average of the rest
  Result := sdAverageOfArrayInteger(@B[Idx], Len - Idx);
end;

function TsdPolyLine.RemoveShortLines(ALineLengthLimit: double): integer;
var
  i, j: integer;
  LL: TsdLaserLine;
  PD: TsdLaserPointDetector;
begin
  Result := 0;
  for i := FLines.Count - 1 downto 0 do
  begin
    LL := FLines[i];
    if LL.GetLineLength < ALineLengthLimit then
    begin
      // remove from owner (including points)
      PD := LL.FOwner;
      for j := 0 to LL.Points.Count - 1 do
      begin
        PD.Points.Remove(LL.Points[j]);
      end;
      PD.Lines.Remove(LL);

      // Remove from polyline
      FLines.Delete(i);

      inc(Result);
    end;
  end;
end;

function TsdPolyLine.TotalPointCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Lines.Count - 1 do
    inc(Result, Lines[i].Points.Count);
end;

{ TsdPolyLineList }

function TsdPolyLineList.ById(AId: integer): TsdPolyLine;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Id = AId then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TsdPolyLineList.ByLineId(ALineId: integer): TsdPolyline;
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

procedure TsdPolyLineList.JoinPolyLines(PL1, PL2: TsdPolyLine);
var
  i: integer;
  LL: TsdLaserLine;
begin
  for i := 0 to PL2.Lines.Count - 1 do
  begin
    LL := PL2.Lines[i];
    PL1.Lines.Add(LL);
    LL.PolyLine := PL1;
  end;
  PL2.Lines.Clear;
  Remove(PL2);
end;

function TsdPolyLineList.DoCompare(Item1, Item2: TObject): integer;
var
  PL1, PL2: TsdPolyLine;
begin
  PL1 := TsdPolyLine(Item1);
  PL2 := TsdPolyLine(Item2);

  case FSortMethod of
  lsByPointCount:
    begin
      Result := -CompareInteger(PL1.TotalPointCount, PL2.TotalPointCount);
      if Result = 0 then
        Result := CompareInteger(PL1.FId, PL2.FId);
    end;
  lsByLineId:
    begin
      Result := CompareInteger(PL1.LineId, PL2.LineId);
    end;
  else
    Result := 0;
  end;
end;

function TsdPolyLineList.GetItems(Index: integer): TsdPolyLine;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TsdLineConnector }

procedure TsdLineConnector.Clear;
begin
  FPolyLines.Clear;
  FLinks.Clear;
end;

procedure TsdLineConnector.ClearPolyLine(APoly: TsdPolyLine);
var
  i: integer;
  LL: TsdLaserLine;
begin
  for i := 0 to APoly.Lines.Count - 1 do
  begin
    LL := APoly.Lines[i];
    FDetector.Lines.Remove(LL);
  end;
  APoly.Lines.Clear;
end;

constructor TsdLineConnector.Create(AOwner: TComponent);
begin
  inherited;
  FPolyLines := TsdPolyLineList.Create(True);
  FLinks := TsdLinkInfoList.Create(True);
  FLinks.Sorted := False;
  FSearchWidth := 10.0;
  FSearchHeight := 5.0;
  FLineLengthLimit := 15.0;
end;

destructor TsdLineConnector.Destroy;
begin
  FreeAndNil(FPolyLines);
  FreeAndNil(FLinks);
  inherited;
end;

procedure TsdLineConnector.DetectAboveBelow(APoly: TsdPolyLine);
var
  i, j, Len, Idx: integer;
  PolyAbove: array of double;
  PolyBelow: array of double;
  LL: TsdLaserLine;
  LP, PA, PB: TsdLaserPoint;
  PLA, PLB: TsdPolyLine;

  // Determine the score for these two points - how good are they at being
  // each other's upper/lower neighbour?
  function SpacingScore(P1, P2: TsdLaserPoint): double;
  var
    Dy, Dml: double;
  begin
    Dy := abs(P1.Cg.Y - P2.Cg.Y);
    Dml := abs(Dy - FMedianLineSpacing);
    Result := FMedianLineSpacing -  Dml;
    if Result < 0 then
      Result := 0;
  end;

// main
begin
  Len := FPolyLines.Count;
  SetLength(PolyAbove, Len);
  SetLength(PolyBelow, Len);

  // Run through the points in APoly (we always have a Lines count of 1 here)
  for i := 0 to APoly.Lines.Count - 1 do
  begin
    LL := APoly.Lines[i];
    for j := 0 to LL.Points.Count - 1 do
    begin
      LP := LL.Points[j];
      PA := FDetector.PointAbove(LP);
      if assigned(PA) and (PA.LineIdx >= 0) then
      begin
        Idx := FDetector.Lines[PA.LineIdx].FPolyLine.FId;
        PolyAbove[Idx] := PolyAbove[Idx] + SpacingScore(LP, PA);
      end;
      PB := FDetector.PointBelow(LP);
      if assigned(PB) and (PB.LineIdx >= 0) then
      begin
        Idx := FDetector.Lines[PB.LineIdx].FPolyLine.FId;
        PolyBelow[Idx] := PolyBelow[Idx] + SpacingScore(LP, PB);
      end;
    end;
  end;

  // Is Above/Below
  for i := 0 to Len - 1 do
  begin
    if PolyAbove[i] > 0 then
    begin
      PLA := FPolyLines.ById(i);
      if assigned(PLA) then
        PLA.AddLinkToIsAbove(APoly, PolyAbove[i])
    end;
    if PolyBelow[i] > 0 then
    begin
      PLB := FPolyLines.ById(i);
      if assigned(PLB) then
        PLB.AddLinkToIsBelow(APoly, PolyBelow[i])
    end;
  end;

end;

procedure TsdLineConnector.DoMessage(const AMessage: string);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, AMessage);
end;

procedure TsdLineConnector.FindConnections;
var
  i, Cnt: integer;
  LL: TsdLaserLine;
  PL: TsdPolyLine;
  L: TsdLinkInfo;
begin
  // We must have our objects
  if not assigned(FDetector) then
    exit;

  // clear polylines and links
  Clear;

  // set line indices of all points
  FDetector.SetLineIndices;

  // No lines? Get out
  if FDetector.Lines.Count = 0 then
    exit;

  // Create list of polylines, all containing one line
  for i := 0 to FDetector.Lines.Count - 1 do
  begin
    LL := FDetector.Lines[i];

    // Create the polyline, and add one laser line
    PL := TsdPolyLine.Create;
    PL.Lines.Add(LL);
    PL.Links := FLinks;
    LL.FPolyLine := PL;
    PL.FId := i;

    // Add the polyline
    FPolyLines.Add(PL);
  end;

  // Detect polylines above and below the current polyline PL
  for i := 0 to FPolyLines.Count - 1 do
  begin
    PL := FPolyLines[i];
    DetectAboveBelow(PL);
  end;

  DoMessage(Format('# Polyline links: %d', [FLinks.Count]));

  // Link everything up
  repeat

    // We re-sort the list each time
    FLinks.Sort;

    // Look at first
    L := FLinks[0];

    if L.IdSetCount in [0, 1] then
    begin

      if not L.Top.FLineIdIsSet then
      begin
        L.Top.FLineId := L.Btm.FLineId - 1;
        L.Top.FLineIdIsSet := True;
      end;
      if not L.Btm.FLineIdIsSet then
      begin
        L.Btm.FLineId := L.Top.FLineId + 1;
        L.Btm.FLineIdIsSet := True;
      end;

    end else

      // All line Id's are set, we're done
      break;

  until False;

  // Did its work..
  FLinks.Clear;

  // Rearrange all lines into a minimal set of polylines

  // Sort them by Line Id, which is now going from small to large from top to bottom
  // of the image. We have no centerline yet, so the Id's are relative.
  FPolyLines.SortMethod := lsByLineId;
  FPolyLines.Sort;

  i := 0;
  while i < FPolyLines.Count - 2 do
  begin
    while (i < FPolyLines.Count - 1) and (FPolyLines[i].LineId = PolyLines[i + 1].LineId) do
      FPolyLines.JoinPolyLines(FPolyLines[i], FPolyLines[i + 1]);
    inc(i);
  end;

  DoMessage(Format('# Polylines (condensed): %d', [FPolyLines.Count]));

  DoMessage('Chaininig up Polylines');
  DoMessage(Format('# Laser lines (before chaining up): %d', [FDetector.Lines.Count]));

  // Chain up the laser lines
  for i := 0 to FPolyLines.Count - 1 do
  begin
    PL := FPolyLines[i];
    repeat
      PL.ChainLaserLines(FSearchWidth, FSearchHeight);
      Cnt := PL.RemoveShortLines(FLineLengthLimit);
    until Cnt = 0;
  end; 

  DoMessage(Format('# Laser lines (after chaining up): %d', [FDetector.Lines.Count]));

end;

end.
