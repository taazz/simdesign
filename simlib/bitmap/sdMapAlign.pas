{ unit sdMapAlign

  The class TsdMapAlign allows to align two maps: MasterMap and SampleMap. By
  calling procedure Align, the optimal shift in X, Y, rotation and scaling will
  be sought with which SampleMap aligns with MasterMap.

  Created:
  16Dec2003

  Author: Nils Haeck M.Sc. - email: n.haeck@simdesign.nl web: www.simdesign.nl

  copyright (c) 2003 - 2007 SimDesign B.V.

  All rights reserved - It is illegal to make copies of this software or to publish
  it in any form without prior written permission

}
unit sdMapAlign;

interface

uses
  Classes, Contnrs, SysUtils, sdByteMap, sdMatrices, sdOptimizeBFGS, Math,
  sdTransforms, Dialogs, Forms;

type

  TsdResultMethod = (
    rmGrayscale,
    rmRedGreen,
    rmMissingAdded
  );

  TsdErrorMethod = (
    emAbsDifferences, // Absolute value of difference between master and sample
    emMasterOnly      // Error counts only when master is black, sample is white
                      // this option is good for alignment of scanned filled in forms
  );


  TsdMatchRegion = class
  private
    FXMin: double;
    FYMin: double;
    FXMax: double;
    FYMax: double;
  public
    constructor Create(AXMin, AXMax, AYMin, AYMax: double);
    procedure ToPixels(AWidth, AHeight: integer; out Xpmin, Xpmax, Ypmin, Ypmax: integer);
    property XMin: double read FXMin write FXMin;
    property XMax: double read FXMax write FXMax;
    property YMin: double read FYMin write FYMin;
    property YMax: double read FYMax write FYMax;
  end;

  TsdMatchRegionList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdMatchRegion;
  public
    property Items[Index: integer]: TsdMatchRegion read GetItems; default;
  end;

  // The class TsdMapAlign allows to align two maps: MasterMap and SampleMap. By
  // calling procedure Align, the optimal shift in X, Y, rotation and scaling will
  // be sought with which SampleMap aligns with MasterMap.
  TsdMapAlign = class(TComponent)
  private
    FMasterMap: TsdByteMap;   // The owned master map (to compare to)
    FSampleMap: TsdTransformByteMap; // The owned sample map (to compare with)
    FSampleFreq: integer;     // Frequency of used pixels in final result
    FWorkMas: TsdByteMap;     // Pointer to working copy of master
    FWorkSmp: TsdTransformByteMap;  // Pointer to working copy of sample
    FDesign: TsdVector;       // Design vector X in optimisation
    FDifferMap: TsdByteMap;   // Map that shows differences
    FMissingMap: TsdByteMap;
    FAddedMap: TsdByteMap;
    FSkeletonize: boolean;    // Use special filter to emphasise differences only
    FFinalMaster: TsdByteMap;
    FFinalSample: TsdByteMap;
    FDebug: TStrings;
    FMasMaps, FSmpMaps: array of TsdTransformByteMap;
    FResultMethod: TsdResultMethod;
    FErrorMethod: TsdErrorMethod;
    FRegions: TsdMatchRegionList;
    FFinalError: double;
    procedure AddDebugLine(ALine: string);
    procedure SetMasterMap(const Value: TsdByteMap);
    procedure SetSampleMap(const Value: TsdTransformByteMap);
    procedure SetDifferMap(const Value: TsdByteMap);
    procedure SetTransform(Trans: TAffineTransformation; X: TsdVector);
  protected
    procedure BuildDifferMap;
    procedure WorkEvaluate(Sender: TObject; XVars: TsdVector; var F: double);
    procedure WorkProgress(Sender: TObject; EvalCount: integer; Step: double; X: TsdVector; F: double; G: TsdVector);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Call Align to align SampleMap to MasterMap. After aligment finishes, the
    // resulting aligned map can be found in FinalSmp. The Scale parameter indicates
    // at which scale the alignment should be done.. Scale = 1 means full scale,
    // Scale = 2 means 50%, Scale = 3 means 25%, and so on.
    procedure Align(Scale: integer);
    // String list with low-level debug information
    property Debug: TStrings read FDebug write FDebug;
    // A map that contains differences after the alignment algo finishes.
    property DifferMap: TsdByteMap read FDifferMap write SetDifferMap;
    // FinalMaster can be given a reference to a TsdByteMap in which the final
    // master image must be stored after alignment. When Scale = 1, this is the
    // same image as MasterMap, but with Scale > 1, it is a downscaled version.
    property FinalMaster: TsdByteMap read FFinalMaster write FFinalMaster;
    // FinalSample can be given a reference to a TsdByteMap in which the final
    // aligned sample image is stored after alignment. It is thus different from
    // the SampleMap image, because this one will contain the transformation.
    property FinalSample: TsdByteMap read FFinalSample write FFinalSample;
    // Final average error per pixel (in pixel values) after alignment.
    property FinalError: double read FFinalError;
    // ResultMethod indicates how the resulting maps are displayed.
    property ResultMethod: TsdResultMethod read FResultMethod write FResultMethod;
    // MasterMap is the input master image to which the input sample image will
    // be aligned.
    property MasterMap: TsdByteMap read FMasterMap write SetMasterMap;
    // SampleMap is the input sample image that will be aligned to the master
    // iamge.
    property SampleMap: TsdTransformByteMap read FSampleMap write SetSampleMap;
    // In case ResultMethod = rmMissingAdded, the data missing in sample, but
    // present in master will be put in this map.
    property MissingMap: TsdByteMap read FMissingMap;
    // In case ResultMethod = rmMissingAdded, the data added in sample, but
    // not present in master will be put in this map.
    property AddedMap: TsdByteMap read FAddedMap;
    // If True, and ResultMethod = rmGrayscale, the resulting difference map
    // will be skeletonized (emphasizing larger differences).
    property Skeletonize: boolean read FSkeletonize write FSkeletonize;
    // If ErrorMethod = emAbsDifferences, the error calculated will be based
    // on data present in master, but not in sample, *and* vice versa. When
    // ErrorMethod = emMasterOnly, then only the data that is in master, but
    // not in sample, will be seen as an error.
    property ErrorMethod: TsdErrorMethod read FErrorMethod write FErrorMethod;
    // A list of regions of interest (ROI). The parameters of these regions
    // are given in fractions of the master image to be analysed. By default,
    // one region is added with parameters XMin = 0.1, XMax = 0.9, YMin = 0.1,
    // YMax = 0.9. This corresponds to a window that covers 80%x80% of the
    // image, leaves 10% along the borders free (this is often a cluttered area).
    property Regions: TsdMatchRegionList read FRegions;
  end;

// Change the map so it is scaled in such a way that white paper = 0 and the
// Percent darkest pixels are scaled to AMax. This is a good method to preprocess
// scans so that they become comparable
procedure MapWhitePaper(Map: TsdByteMap; Percent: double; AMax: byte);

implementation

{ TsdMatchRegion }

constructor TsdMatchRegion.Create(AXMin, AXMax, AYMin, AYMax: double);
begin
  inherited Create;
  FXMin := AXMin;
  FXMax := AXMax;
  FYMin := AYMin;
  FYMax := AYMax;
end;

procedure TsdMatchRegion.ToPixels(AWidth, AHeight: integer; out Xpmin,
  Xpmax, Ypmin, Ypmax: integer);
begin
  Xpmin := round(AWidth * XMin);
  Xpmax := Max(Xpmin + 1, round(AWidth * XMax));
  Ypmin := round(AHeight * YMin);
  Ypmax := Max(Ypmin + 1, round(AHeight * YMax));
end;

{ TsdMatchRegionList }

function TsdMatchRegionList.GetItems(Index: integer): TsdMatchRegion;
begin
  Result := Get(Index);
end;

{ TsdMapAlign }

procedure TsdMapAlign.AddDebugLine(ALine: string);
begin
  if assigned(Debug) then
    Debug.Add(ALine);
  Application.ProcessMessages;
end;

procedure TsdMapAlign.BuildDifferMap;
// Build the difference map by comparing the standard with the adjusted sample,
// using X-shift, Y-shift, rotation and scale
var
  Trans: TAffineTransformation;
  Dst: TsdTransformByteMap;
  FilterSize: integer;
begin
  // Create
  Trans := TAffineTransformation.Create;
  Dst := TsdTransformByteMap.Create;
  try
    // Setup transformation
    SetTransform(Trans, FDesign);
    Trans.SrcRect := FloatRect(0, 0, SampleMap.Width, SampleMap.Height);

    // Set dest map size
    Dst.SetSize(FWorkMas.Width, FWorkMas.Height);

    // Do transformation
    Transform(FWorkSmp, Dst, Trans);

    // Add to finals (if used)
    if assigned(FFinalMaster) then
      FFinalMaster.Assign(FWorkMas);
    if assigned(FFinalSample) then
      FFinalSample.Assign(Dst);

    // Create absolute difference map
    if Skeletonize and (ResultMethod = rmGrayscale) then begin
      // Filtersize must be an odd number, at least 3
      FilterSize := max(3, Dst.Height div 700);
      if not odd(FilterSize) then inc(FilterSize);
      // Find the absolute difference of pixels relative to the maxima of an area
      // of Filtersize x Filtersize in the other map
      AbsDiffWithSurround(FWorkMas, Dst, DifferMap, FilterSize, FilterSize);
    end else begin
      case ResultMethod of
      // The absolute difference between map FWorkStd and Dst
      rmGrayscale: AbsDifferentialMap(FWorkMas, Dst, DifferMap);
      // The relative difference between map FWorkStd and Dst
      rmRedGreen:  RelDifferentialMap(FWorkMas, Dst, DifferMap);
      // Two maps, one with missing one with added
      rmMissingAdded:
        begin
          if not assigned(FMissingMap) then
            FMissingMap := TsdBytemap.Create;
          if not assigned(FAddedMap) then
            FAddedMap := TsdBytemap.Create;
          SplitDifferenceMap(FWorkMas, Dst, FMissingMap, FAddedMap);
        end;
      end;//case
    end;

    // Normalize the difference map
    case ResultMethod of
    rmGrayscale: DifferMap.Normalize;
    rmRedGreen:; // DifferMap.SignedNormalize
    rmMissingAdded:
      begin
        FMissingMap.Normalize;
        FAddedMap.Normalize;
      end;
    end;//case

  finally
    Trans.Free;
    Dst.Free;
  end;
end;

constructor TsdMapAlign.Create(AOwner: TComponent);
begin
  inherited;
  FMasterMap := TsdByteMap.Create;
  FSampleMap := TsdTransformByteMap.Create;
  FDifferMap := TsdByteMap.Create;
  FDesign := TsdVector.Create;
  FRegions := TsdMatchRegionList.Create(True);
  // Create window within 80% x 80% of the FOV
  FRegions.Add(TsdMatchRegion.Create(0.1, 0.9, 0.1, 0.9));
  // Defaults
  FSampleFreq := 1;
  FResultMethod := rmRedGreen;
end;

destructor TsdMapAlign.Destroy;
begin
  FreeAndNil(FRegions);
  FreeAndNil(FMasterMap);
  FreeAndNil(FSampleMap);
  FreeAndNil(FDifferMap);
  FreeAndNil(FDesign);
  FreeAndNil(FMissingMap);
  FreeAndNil(FAddedMap);
  inherited;
end;

procedure TsdMapAlign.Align(Scale: integer);
// Call Align to do an alignment of the maps in StandardMap and SampleMap. It uses
// BFGS optimisation to achieve the best alignment. StepSize and Tolerance are
// somewhat tunable parameters, and can influence the time taken considerably.

// Set Scale to > 1 if you want to just optimize to a downscaled version (e.g.
// Scale = 2 will optimize the 2x downsized version)
var
  Level, N, MapCount: integer;
  Factor: double;
  FOptim: TsdOptimizer;
begin
  FDebug.Add('Preparation of maps.');
  Application.ProcessMessages;

  // Create optimizer
  FOptim := TsdOptimizer.Create;
  FOptim.GradientStep := 1.0;

  // Our evaluation function
  FOptim.OnEvaluate := WorkEvaluate;
  FOptim.OnProgress := WorkProgress;

  // Number of design vars
  FOptim.XVars.SetSize(4, 1);

  // About design variables:
  // X0 is defined as hor translation over a length of X0/100 * Width pixels
  // X1 is defined as ver translation over a length of X1/100 * Width pixels
  // X2 is defined as rotation over X2/100 rad in center
  // X3 is defined as scaling by 2^(X3/100)

  // Design var initial values are zero
  FOptim.XVars[0] := 0.0;
  FOptim.XVars[1] := 0.0;
  FOptim.XVars[2] := 0.0;
  FOptim.XVars[3] := 0.0;

  // Initial stepsize
  FOptim.StepSize  := 0.3;//0.5;

  // Ending tolerance
  FOptim.Tolerance := 0.005;

  // Determine neccesary levels - smallest map will have less than 500 pixels in total
  Factor := sqrt(SampleMap.Elementcount / 500);
  N := 1;
  MapCount := 0;
  while N < Factor do begin
    N := N * 2;
    inc(MapCount);
  end;

  // Create downsized maps
  SetLength(FMasMaps, MapCount);
  SetLength(FSmpMaps, MapCount);
  for Level := 0 to MapCount - 1 do begin
    FMasMaps[Level] := TsdTransformByteMap.Create;
    FSmpMaps[Level] := TsdTransformByteMap.Create;
    if Level = 0 then begin
      ScaleDown(MasterMap, FMasMaps[Level], 2);
      ScaleDown(SampleMap, FSmpMaps[Level], 2);
    end else begin
      ScaleDown(FMasMaps[Level - 1], FMasMaps[Level], 2);
      ScaleDown(FSmpMaps[Level - 1], FSmpMaps[Level], 2);
    end;
  end;

  Level := MapCount - 1;
  while N >= Scale do begin
    FSampleFreq := Max(1, 16 div N);

    if Level >= 0 then begin
      // Scaled down copy
      FWorkMas := FMasMaps[Level];
      FWorkSmp := FSmpMaps[Level];
    end else begin
      // Our original
      FWorkMas := MasterMap;
      FWorkSmp := SampleMap;
    end;
    AddDebugLine(Format('Optimize level "%d" - #Eval, Step, F, X[], G[] Mapsize %d x %d',
      [N, FWorkMas.Width, FWorkMas.Height]));

    N := N div 2;
    dec(Level);

    case FOptim.Execute of
    orOK:
      AddDebugLine('Optimization successful'); // All ok
    else
      AddDebugLine('Optimization failed');
    end;
  end;

  // Store results
  FDesign.Assign(FOptim.XVars);

  // Show results
  AddDebugLine('Results:');
  AddDebugLine(Format('X-shift (pixels): %10.3f', [FDesign[0] / 100 * FWorkMas.Width]));
  AddDebugLine(Format('Y-shift (pixels): %10.3f', [FDesign[1] / 100 * FWorkMas.Width]));
  AddDebugLine(Format('Rotation of center (degrees): %10.3f', [FDesign[2] / 100 * (180/pi)]));
  AddDebugLine(Format('Scale: %10.3f', [Power(2, FDesign[3] / 100)]));
  AddDebugLine(Format('Final error per pixel: %8.2f', [FFinalError]));
  AddDebugLine('Building differential map...');

  FOptim.Free;

  // Create differential map
  BuildDifferMap;

  // Free maps
  for Level := 0 to MapCount - 1 do begin
    FMasMaps[Level].Free;
    FSmpMaps[Level].Free;
  end;
  SetLength(FMasMaps, 0);
  SetLength(FSmpMaps, 0);

  AddDebugLine('Finished.');
end;

procedure TsdMapAlign.SetDifferMap(const Value: TsdByteMap);
begin
  FDifferMap.Assign(Value);
end;

procedure TsdMapAlign.SetSampleMap(const Value: TsdTransformByteMap);
begin
  FSampleMap.Assign(Value);
end;

procedure TsdMapAlign.SetMasterMap(const Value: TsdByteMap);
begin
  FMasterMap.Assign(Value);
end;

procedure TsdMapAlign.SetTransform(Trans: TAffineTransformation; X: TsdVector);
// Setup a transformation
var
  Tx, Ty, R, S: double;
begin
  // Tx and Ty given as 0.01 * shift necessary to shift over Width pixels
  Tx := X[0] * 0.01 * FWorkMas.Width;
  Ty := X[1] * 0.01 * FWorkMas.Width;
  // R is given as 0.01 * one rad of rotation
  R  := X[2] * 0.01 * (180 / pi);
  // S is given as scalefactor 2^ (scale * 0.01)
  S  := Power(2, X[3] * 0.01);

  // The transformation is set up with translations first, then rotation then scaling
  // Translate with first two design variables
  Trans.Translate(Tx, Ty);
  // Rotate around center of map, amount of 3rd variable in 10ths of degrees
  Trans.Rotate(FWorkMas.Width div 2, FWorkMas.Height div 2, R);
  // Scale map
  Trans.Scale(S, S);
end;

procedure TsdMapAlign.WorkEvaluate(Sender: TObject; XVars: TsdVector; var F: double);
// calculate the difference of standard and transformed sample map, which is put in
// F, the object function of the minimization
var
  i, y, x, E: integer;
  Err: int64;
  XMin, XMax, YMin, YMax: integer;
  Trans: TAffineTransformation;
  Dst: TsdTransformByteMap;
begin
  // Create
  Trans := TAffineTransformation.Create;
  Dst := TsdTransformByteMap.Create;
  try
    // Setup transformation
    SetTransform(Trans, XVars);

    // Set dest size to master size
    Dst.SetSize(FWorkMas.Width, FWorkMas.Height);

    // Calculate object function
    F := 0;
    for i := 0 to Regions.Count - 1 do
    begin
      Err := 0;
      Dst.Clear($00);

      // Our window which we check (rectangle in XMin/XMax/YMin/YMax)
      Regions[i].ToPixels(FWorkMas.Width, FWorkMas.Height, XMin, XMax, YMin, YMax);

      // Do the transform
      Trans.SrcRect := FloatRect(XMin, YMin, XMax, YMax);
      TransformSample(FWorkSmp, Dst, Trans, FSampleFreq);

      // Loop through the rows
      for y := YMin to YMax - 1 do
        // When r on the sample frequency then process this row
        if y mod FSampleFreq = 0 then
          // loop through the columns
          for x := XMin to XMax - 1 do
            // When c is on the sample frequency then process the cell
            if x mod FSampleFreq = 0 then
            begin
              case FErrorMethod of
              emAbsDifferences:
                // The error is increased with the absolute difference of the two maps
                inc(Err, abs(Dst[x, y] - FWorkMas[x, y]));
              emMasterOnly:
                begin
                  E := FWorkMas[x, y] - Dst[x, y];
                  if E < 0 then E := 0;
                  inc(Err, E);
                end;
              end;
            end;
      // Our cost function is the error divided by the approx. number of pixels analysed
      F := F + Err / ((XMax - XMin) * (YMax - YMin)) * sqr(FSampleFreq);
    end;
    FFinalError := F;

  finally
    Dst.Free;
    Trans.Free;
  end;
end;

procedure TsdMapAlign.WorkProgress(Sender: TObject; EvalCount: integer;
  Step: double; X: TsdVector; F: double; G: TsdVector);
// Plot a line with object function result, design variable values and gradient values
var
  i: integer;
  Line: string;
begin
  Line := Format('%.3d %10.3f %10.3f  [', [EvalCount, F, Step]);
  for i := 0 to X.Count - 1 do
    Line := Line + Format('%10.3f ', [X[i]]);
  Line := Line + ']  [';
  for i := 0 to G.Count - 1 do
    Line := Line + Format('%10.3f ', [G[i]]);
  Line := Line + ']';
  AddDebugLine(Line);
end;

{ procedures }

procedure MapWhitePaper(Map: TsdByteMap; Percent: double; AMax: byte);
// Change the map so it is scaled in such a way that white paper = 0 and the
// Percent darkest pixels are scaled to AMax. This is a good method to preprocess
// scans so that they become comparable
var
  i, Count, Acc, Limit: integer;
  Hist: THisto;
  LUT: TLut;
  ALow, AHigh: byte;
begin
  with Map do begin
    if IsEmpty then exit;
    Histogram(Hist);
    Count := 0;
    for i := 0 to 255 do
      Count := Count + Hist[i];

    // Low limit is the first occurrance of "white"
    ALow := 255;
    while Hist[ALow] = 0 do dec(ALow);

    // High limit is the first % occurrance of "black"
    Limit := round(Count * Percent);
    AHigh := 0;
    Acc := Hist[AHigh];
    while Acc < Limit do begin
      inc(AHigh);
      inc(Acc, Hist[AHigh]);
    end;

    if AHigh >= ALow then begin
      // This is an extreme case, with just one color or very few pixels. We will
      // search for another AHigh: the last one with color
      for AHigh := 0 to ALow - 1 do
        if Hist[AHigh] > 0 then
          break;
    end;

    // Scale in a LUT
    for i := 0 to 255 do
      LUT[i] := Max(0, Min(255, round((i - ALow) / (AHigh - ALow) * AMax)));

    // Apply
    ApplyLut(LUT);
  end;
end;

end.
