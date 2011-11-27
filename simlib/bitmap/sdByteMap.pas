{ unit sdBytemap

  This unit implements an object that holds a map (or matrix) of bytes,
  sized Width x Height. It can be assigned to and from a TBitmap. Use
  MyMap[x, y] to access individual elements (bytes). It allows access
  to pixels outside the map, it returns 0 in that case.

  Working with bytemaps is much faster than working with TBitmap objects.

  There are some additional color and palette handling routines and a fast
  segmentation routine.

  Modificatiions:
  - 18sep2011: unified all versions of sdByteMap.pas

  (c) Copyright 2003 - 2011 by Nils Haeck (SimDesign B.V.)
  for more info please visit www.simdesign.nl

}
unit sdBytemap;

interface

uses
  Classes, SysUtils, Contnrs, Windows, Graphics, Math;

type

  PCardinal = ^cardinal;

  THisto = array[0..255] of integer;
  TDiffHisto = array[-255..255] of integer;
  TLut = array[0..255] of byte;

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt - 1] of byte;

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..MaxInt div SizeOf(SmallInt) - 1] of SmallInt;

  PWordArray = ^TWordArray;
  TWordArray = array[0.. MaxInt div SizeOf(word) - 1] of word;

  TBitmapChannelType  = (
    chUniform,   // Convert bits to grayscale as (R+G+B) / 3
    chWeighted,  // Use weighted formula fitting with eye sensitivity
    chRed,       // Use the red color of the bits
    chGreen,     // Use the green color of the bits
    chBlue       // Use the blue color of the bits
  );

  // TsdMap is a generic map type that functions as ancestor for maps that contain
  // one type of data
  TsdMap = class(TPersistent)
  private
    function GetElementCount: integer; virtual;
  protected
    FHeight: integer;
    FWidth: integer;
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
  public
    // Check whether the map is empty (Width x Height = 0)
    function IsEmpty: boolean;
    // Set the size of the map. Note: the array is not cleared, only space
    // is allocated
    procedure SetSize(AWidth, AHeight: integer); virtual; abstract;
    // Set the size of the map to the same size as AMap
    procedure SetSizeToMap(AMap: TsdMap);
    // Check if the map equals the size of AMap
    function SizeEquals(AMap: TsdMap): boolean;
    // Number of elements in the array (Width x Height)
    property ElementCount: integer read GetElementCount;
    // Height of the map array
    property Height: integer read FHeight write SetHeight;
    // Width of the map array
    property Width: integer read FWidth write SetWidth;
  end;

  // TsdByteMap holds a two-dimensional array of bytes. TsdByteMap can be assigned
  // to and from TBitmap. TsdByteMap contains many methods for working with
  // grayscale images or mathematically created images with samples in range [0..255].
  TsdByteMap = class(TsdMap)
  private
    FMap: PByteArray;
    FBorrowed: boolean; // If true, the map is not owned by the object, and will not be freed
    function GetElements(x, y: integer): byte;
    procedure SetElements(x, y: integer; const Value: byte);
    function GetScanline(y: integer): PByteArray;
    procedure SetBorrowed(const Value: boolean);
  protected
    // This method should not be called directly, but is called by any other TPersistent
    // object to which the TsdBytemap is assigned. This method implements assignment
    // to a TBitmap. The TBitmap will be made pf8bit and the data from the bytemap
    // will be copied one-on-one
    procedure AssignTo(Dest: TPersistent); override;
    property Map: PByteArray read FMap;
  public
    destructor Destroy; override;
    // Add a constant to all elements of the map. Values will be kept between
    // 0 and 255.
    procedure AddConstant(Value: integer);
    // Assign a TPersistent descendant to the bytemap. Usually this is another
    // bytemap or it is a TBitmap, in this case, the pixel values in the TBitmap
    // are converted using (R + G + B) div 3
    procedure Assign(Source: TPersistent); override;
    // Assign a bitmap to the map, and copy Channel (either gray, red, green, blue)
    procedure AssignBitmap(Bitmap: TBitmap; Channel: TBitmapChannelType); virtual;
    // Show the map as HEX numbers in a text
    function AsString: string;
    // Apply a lookup table in LUT to each element of the bytemap. Each element
    // will become Element[x, y] := LUT[Element[x, y]]
    procedure ApplyLUT(LUT: TLut);
    // Apply a threshold to a Bytemap. Elements below threshold become value 0
    // and above the threshold the value gets 255
    procedure ApplyThreshold(AThreshold: Integer);
    // Borrow the map AMap. If an owned map was already assigned, it will first be
    // freed. Calls to SetSize or Destroying the object will not reallocate the map
    // once borrowed. Calling BorrowMap with nil will ensure that reference to the
    // borrowed map is removed, and a new owned map is allocated. BorrowMap can be
    // used to work on any memory location in another bytemap.
    procedure BorrowMap(AMap: pointer);
    // Blur the map with a gaussian blur radius Radius. The blurring algorithm
    // used is not true gaussian blur but a very fast approximation.
    procedure Blur(Radius: single);
    // Blur the map with RegionX and RegionY and do the recursion Steps * 2 times.
    procedure BlurExt(RegionX, RegionY, Steps: integer);
    procedure CenterOfGravity(ARect: TRect; var XPos, YPos: single);
    // Clear the complete bytemap with Value (default = 0).
    procedure Clear(Value: byte = 0);
    // Copy another map but first apply a lookup table ALut before setting the
    // map values.
    procedure CopyFromMapWithLUT(AMap: TsdByteMap; const ALut: TLut);
    // Automatically cropping of a map, using a threshold. Resizing map, resulting
    // in a map without borders below threshold.
    procedure AutoCrop(AThresholdValue: integer);
    //
    procedure DownScale(NewWidth, NewHeight: integer);
    // Copy (draw) map AMap onto us at position X, Y
    procedure DrawMap(AMap: TsdByteMap; X, Y: integer);
    // Bilinear interpolation on the map using SAR_8 coordinates (256 fractional
    // values). Example: a value of 3.5 would translate to $0300 + $80 = $0380
    function Get_S256(X256, Y256: integer): byte;
    // Create a histogram of the bytemap and put the result in Histo.
    procedure Histogram(var Histo: THisto);
    //
    procedure HistogramStretch;
    // Increase the map by a factor AFactor in both X and Y direction. AFactor
    // must be 2 or bigger.
    procedure IncreaseSize(AFactor: integer); virtual;
    // Invert the map's values: 0 becomes 255 and 255 becomes 0.
    procedure Invert; virtual;
    // Create a thresholded map with 0 where value < Threshold and 255 where
    // value >= Threshold. The function returns the number of pixels equal or above
    // the threshold.
    function Threshold(Threshold: byte): integer;
    // Calculate the mean value of the map
    function MeanValue: double;
    // Normalize the map between ALow and AHigh (default 0 and 255). Map values
    // are determined by simple rounding, no dithering is performed. Note that
    // information may get lost using this procedure.
    procedure Normalize(ALow: byte = 0; AHigh: byte = 255);
    // Normalize the map between ALow and AHigh (default -127 and 127), assuming
    // that the map consists of shortint values.
    procedure SignedNormalize(ALow: shortint = -127; AHigh: shortint = 127);
    // Shift all bytes in the map using shr or shl, by a number of bits of Shift.
    // If Shift is positive, bits will be shifted right, when negative, bits will
    // be shifted left.
    procedure ShiftBits(Shift: integer); virtual;
    //
    procedure SetSize(AWidth, AHeight: integer); override;
    // Create a histogram of the bytemap and sort it by occurance, and put the
    // crossreference index for the sorting in Sort.
    procedure SortedHistogram(var Histo: THisto; var Sort: TLut);
    // Reduce the map by a factor AFactor in both X and Y direction. The values
    // stored are the averages of each square of size AFactor x AFactor.
    procedure ReduceSize(AFactor: integer); virtual;
    // XOR all values in the map with the values in map AMap. The mapsizes must
    // match.
    procedure XorMap(AMap: TsdByteMap); virtual;
    // If Borrowed is True (non-default), the actual map data is not owned by
    // the bytemap and will not be freed or reallocated in any way by the bytemap.
    // See also BorrowMap.
    property Borrowed: boolean read FBorrowed write SetBorrowed;
    // Elements[x, y] returns the byte element at [x, y]. When values outside of
    // the map are requested, Elements returns 0. Set elements in the map using
    // Elements[x, y] := Value. Setting elements outside the map does nothing. Elements
    // on the map are found by FMap[x + y * FWidth] (after testing).
    property Elements[x, y: integer]: byte read GetElements write SetElements; default;
    // The ScanLine array property returns a PByteArray pointer to the y-th scanline.
    property Scanline[y: integer]: PByteArray read GetScanline;
    // MapPointer is a PByteArray pointer to the map data.
    property MapPointer: PByteArray read FMap;
  end;

  // TsdSmallIntMap holds a two-dimensional array of SmallInt values (16bits signed integer).
  TsdSmallIntMap = class(TsdMap)
  private
    FMap: PSmallIntArray;
    function GetElements(x, y: integer): SmallInt;
    procedure SetElements(x, y: integer; const Value: SmallInt);
    function GetElementsMirrored(x, y: integer): SmallInt;
    function GetScanLine(y: integer): PSmallIntArray;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear(Value: smallint = 0);
    procedure DifferenceHistogram(var Histo: TDiffHisto); virtual;
    procedure SetSize(AWidth, AHeight: integer); override;
    procedure SubstractMaps(MapA, MapB: TsdMap); virtual;
    procedure HorizontalGradientOf(AMap: TsdMap; KernelSize: integer);
    property Elements[x, y: integer]: SmallInt read GetElements write SetElements; default;
    // Return value just like Elements, but mirrored on border when requesting
    // values out of range.
    property ElementsMirrored[x, y: integer]: SmallInt read GetElementsMirrored;
    // MapPointer is a PSmallIntArray pointer to the map data.
    property MapPointer: PSmallIntArray read FMap;
    property ScanLine[y: integer]: PSmallIntArray read GetScanLine;
  end;

  TsdWordMap = class(TSdMap)
  private
    FMap: PWordArray;
    function GetElements(x, y: integer): word;
    procedure SetElements(x, y: integer; const Value: word);
    function GetElementPtr(x, y: integer): pointer;
  public
    destructor Destroy; override;
    procedure Clear(Value: word = 0);
    procedure CopyRectangle(Source: TsdWordMap; ARect: TRect);
    procedure SetSize(AWidth, AHeight: integer); override;
    procedure LoadFromStream(S: TStream);
    procedure SaveToStream(S: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure InvertX;
    property Elements[x, y: integer]: word read GetElements write SetElements; default;
    property ElementPtr[x, y: integer]: pointer read GetElementPtr;
    property MapPointer: PWordArray read FMap;
  end;

  T32bitPaletteEntry = packed record
    case integer of
    0: (R, G, B, A: byte);
    1: (Color: longint);
  end;

  T8bitPaletteArray = array[0..255] of T32bitPaletteEntry;

const

  cMaxGradKernelSize = 8;

resourcestring

  sbmImcompatibleMapSizes = 'Incompatible map sizes';
  sbmImcompatibleMapTypes = 'Incompatible map types';
  sbmInvalidKernelSize    = 'Invalid kernel size';

// Set the palette of an pf8bit TBitmap to the values that are given in
// Palette.
procedure SetBitmap8bitPalette(const Palette: T8bitPaletteArray; Bitmap: TBitmap);

// Set the palette of a pf8bit bitmap to a grayscale palette, starting at [0, 0, 0] for
// index 0 and ending at [255, 255, 255] for index 255.
procedure SetBitmap8bitGrayscale(Bitmap: TBitmap);

// Set the palette of a pf8bit bitmap to an inverse grayscale palette, starting at
// [255, 255, 255] for index 0 and ending at [0, 0, 0] for index 255.
procedure SetBitmap8bitInverseGrayscale(Bitmap: TBitmap);

// Set the palette of a pf8bit bitmap to a "rainbow" palette.
procedure SetBitmap8bitRainbow(Bitmap: TBitmap);

// Interpolate between two colors Col1 and Col2. Frac = 0 will return Col1,
// Frac = 1 will return Col2, and any fraction inbetween will return a
// combination. Colors are in Windows RGB format. Use ColorToRGB() with
// any Delphi TColor first, e.g.:
// MyColor := InterpolateColor(ColorToRGB(clBlue), ColorToRGB(clRed), 0.5);
function InterpolateColor(Col1, Col2: longint; Frac: single): longint;

// Find the absolute differential of Map1 and Map2 and store in Result. The
// absolute differential is defined as Result[i, j] := abs(Map2[i, j] - Map1[i, j])
procedure AbsDifferentialMap(Map1, Map2, Dest: TsdByteMap);

// Differential map, will put differences (Map2 - Map1) in ADest
procedure RelDifferentialMap(Map1, Map2: TsdByteMap; Dest: TsdSmallIntMap); overload;

// Differential map, but now as bytemap
procedure RelDifferentialMap(Map1, Map2, Dest: TsdByteMap); overload;

// Store the minimum of each Rows x Cols block
procedure LocalMinimumMap(Source, Dest: TsdByteMap; Cols, Rows: integer);

procedure SplitDifferenceMap(SrcBefore, SrcAfter, DstBefore, DstAfter: TsdByteMap);

// Store the maximum of each Rows x Cols block
procedure LocalMaximumMap(Source, Dest: TsdByteMap; Rows, Cols: integer);

// Store the maximum and minimum of each Rows x Cols block
procedure LocalMinAndMaxMap(Source, MinMap, MaxMap: TsdByteMap; Cols, Rows: integer);

// Equalize the map: take source value and scale it between min and max, store in Dest
procedure Equalizemap(Source, MinMap, MaxMap, Dest: TsdByteMap; MinimumDelta: integer);

// difference with maximum of surrounding pixels in Rows x Cols section
procedure AbsDiffWithSurround(Map1, Map2, Dest: TsdByteMap; Rows, Cols: integer);

// fast ridge
procedure FastRidge(Source, Dest: TsdByteMap; SpanX, SpanY: integer; Scale: double);

// detect ridge
procedure DetectRidge(Source, Dest: TsdByteMap; Span, Flat, Threshold: integer);

// canny edge detection
procedure CannyEdge(Source, Dest: TsdByteMap);

// Scale down the Source map by Factor and put result in Dest.
procedure ScaleDown(Source, Dest: TsdByteMap; Factor: integer);

// Determine mean and standard deviation of the histogram Histo.
procedure HistoMeanAndSigma(Histo: THisto; var Mean, Sigma: double);

// Combine MapA and MapB and put result in Dest. Use weighting WeightA and WeightB to do
// so, in other words:
// Dest[i, j] := (MapA[i, j] * WeightA + MapB[i, j] * WeightB) / (WeightA + WeightB)
procedure CombineMapsWithWeight(MapA, MapB, Dest: TsdByteMap; WeightA, WeightB: integer);

// Do a recursive Rectangular Filter over a width of Length pixels, with the
// result value taken at Half. This filter assumes that the edge areas are empty
// so that all data can be processed in one long row.
procedure RecursiveRectFilter(Src, Dst: TsdByteMap; Half, Length: integer);

// Create a floating average map of Src in Dst, with windows WindowX and WindowY
// in X resp Y direction, and using a Circular index for X and Y when CircularX
// or CircularY is set to true
procedure FloatAverageMap(Src, Dst: TsdSmallIntMap; WindowX, WindowY: integer;
  CircularX, CircularY: boolean);

// Do nothing else than put the transposed bits from Src into Dst. So we do NOT
// set the bitmap sizes, nor is any error checking done.
procedure TransposeByteMap(Src, Dst: TsdByteMap; Width, Height: integer);

// Split the bitmap to 3 individual maps for R, G and B
procedure Bitmap24ToRGBMaps(Bitmap: TBitmap; MapR, MapG, MapB: TsdByteMap);

// Join the bitmap from 3 individual maps for R, G and B
procedure RGBMapsToBitmap24(MapR, MapG, MapB: TsdByteMap; Bitmap: TBitmap);

// CopyValuesInMap scans through all elements, and if it finds OldVal, it will
// replace it by NewVal.
procedure CopyValuesInMap(Map: TsdByteMap; OldVal, NewVal: byte);

// Segmentate will check Src values that are equal or above threshold, and find
// regions that are connected. Each region gets its own number. The function returns
// the number of regions found. The regions are stored in Dst. Since Dst is a
// bytemap, this function can only work with a maximum total of regions of 255.
// Dst values that are zero indicate that Src is below threshold.
// Connections are not checked diagonally!
function SegmentateMap256(Src, Dst: TsdByteMap; Threshold: byte): byte;

// CopyValuesInMap scans through all elements, and if it finds OldVal, it will
// replace it by NewVal.
procedure CopyValuesInMap16b(Map: TsdWordMap; OldVal, NewVal: word);

// Segmentate will check Src values that are equal or above threshold, and find
// regions that are connected. Each region gets its own number. The function returns
// the number of regions found. The regions are stored in Dst.
// Connections are not checked diagonally!
function SegmentateMap16b(Src: TsdByteMap; Dst: TsdWordMap; Threshold: byte): integer;

function SegmentRectangle16b(Segments: TsdWordMap; Index: integer): TRect;

resourcestring

  sbmTooManySegments = 'Too many segments (>255) in segmentation';
  sbmInvalidNewSize  = 'Invalid new size';

implementation

const bias = $00000080;

function SAR_8(Value: Integer): Integer;
asm
        SAR EAX,8
end;

function CombineByte(X, Y, W: integer): integer;
// Hallelujah, my first ASM! (NH)
asm
  // combine values X and Y with the weight given in W (0..255)
  // Result Z = W * X + (1 - W) * Y
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
        JCXZ    @1              // CX = 0 ?    => Result := EDX
        CMP     ECX,$FF         // CX = $FF ?  => Result := EAX
        JE      @2

  // P = W * X
        IMUL    EAX,ECX         // EAX  <-  00 00 Pb **
        ADD     EAX,bias
        AND     EAX,$FFFFFF00   // EAX  <-  00 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 00 00 Pb

  // W = 1 - W; Q = W * Y
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        IMUL    EDX,ECX         // EDX  <-  00 00 Qb **
        ADD     EDX,bias
        AND     EDX,$FFFFFF00   // EDX  <-  00 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 00 00 Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EDX         // EAX  <-  00 00 00 Zb

        RET

@1:     MOV     EAX,EDX
@2:     RET
end;

{ TsdMap }

function TsdMap.GetElementCount: integer;
begin
  Result := Width * Height;
end;

function TsdMap.IsEmpty: boolean;
begin
  Result := ElementCount = 0;
end;

procedure TsdMap.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
    SetSize(FWidth, Value);
end;

procedure TsdMap.SetSizeToMap(AMap: TsdMap);
begin
  SetSize(AMap.Width, AMap.Height);
end;

procedure TsdMap.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
    SetSize(Value, FHeight);
end;

function TsdMap.SizeEquals(AMap: TsdMap): boolean;
begin
  if not assigned(AMap) then
    Result := False
  else
    Result := (Width = AMap.Width) and (Height = AMap.Height);
end;

{ TsdBytemap }

procedure TsdByteMap.AddConstant(Value: integer);
var
  i: integer;
  P: PByte;
begin
  P := PByte(FMap);
  for i := 0 to ElementCount - 1 do
  begin
    P^ := Max(0, Min(255, P^ + Value));
    inc(P);
  end;
end;

procedure TsdBytemap.ApplyLUT(LUT: TLut);
// Apply a lookup table in LUT
var
  i: integer;
begin
  for i := 0 to ElementCount - 1 do
    FMap[i] := LUT[FMap[i]];
end;

procedure TsdBytemap.ApplyThreshold(AThreshold: Integer);
// Apply threshold to the map
var
  x, y: integer;
begin
  for y := 0 to Height-1 do
  begin
    for x := 0 to Width-1 do
    begin
      if Elements[X,Y]<= AThreshold then
        Elements[X,Y] := 0      //Below the threshold, the pixel gets black
      else
        Elements[X,Y] := 255;   //Above the threshold, the pixel gets white
    end;
  end;
end;

procedure TsdBytemap.Assign(Source: TPersistent);
// Assign Source to the map, Source being either another TBytemap or a TBitmap
var
  x, y: integer;
  AScan: PByte;
  APos, AVal: integer;

  // local
  function ColorToUniformGray(AColor: TColor): byte;
  var
    C: longint;
  begin
    C := ColorToRGB(AColor);
    Result := ((C shr 16 and $FF)+ (C shr 8 and $FF) + (C and $FF)) div 3;
  end;

// main
begin
  if Source is TsdByteMap then
  begin
    // Copy the bytemap
    SetSize(TsdByteMap(Source).Width, TsdByteMap(Source).Height);
    Move(TsdByteMap(Source).FMap^, FMap^, Width * Height);
  end else
  begin
    if Source is TBitmap then
    begin
      // Assign the TBitmap data to our map
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      case TBitmap(Source).PixelFormat of
      pf8bit:
        begin
          // pf8bit allows us to just copy the scanlines. So here we are just using
          // the palette indices as element values for our bytemap. No attempt is
          // made here to convert to grayscale or something similar
          for y := 0 to Height - 1 do
            Move(TBitmap(Source).Scanline[y]^, FMap[y * Width], Width);
        end;
      pf24bit:
        begin
          // pf24bit is easy; just copy the byte triplets to our map
          for y := 0 to Height - 1 do
          begin
            AScan := TBitmap(Source).Scanline[y];
            APos := y * Width;
            for x := 0 to Width - 1 do
            begin
              // Get average (uniform gray) from 3 conseq pixel values RGB
              AVal := AScan^;
              inc(AScan);
              AVal := AVal + AScan^;
              inc(AScan);
              AVal := (AVal + AScan^) div 3;
              inc(AScan);
              FMap[APos] := AVal; inc(APos);
            end;
          end;
        end;
      else
        // We'll use a very simple but slow pixels[] property approach for all other
        // pixelformats.
        for y := 0 to Height - 1 do
          for x := 0 to Width - 1 do
            // Change to grayscale, use uniform formula (R, G, B contribute equally)
            Elements[x, y] := ColorToUniformGray(TBitmap(Source).Canvas.Pixels[x, y]);
      end;//case
    end else
      inherited;
  end;
end;

procedure TsdByteMap.AssignBitmap(Bitmap: TBitmap; Channel: TBitmapChannelType);
// Assign the TBitmap data to our map
var
  x, y: integer;
  AScan: PByte;
  APos, AVal: integer;
begin
  if Bitmap.PixelFormat <> pf24bit then
  begin
    Assign(Bitmap);
    exit;
  end;

  SetSize(Bitmap.Width, Bitmap.Height);

  // pf24bit is easy; just copy the byte triplets to our map
  for y := 0 to Height - 1 do
  begin
    AScan := Bitmap.Scanline[y];
    APos := y * Width;
    case Channel of
    chUniform:
      begin
        for x := 0 to Width - 1 do
        begin
          // Get average (uniform gray) from 3 conseq pixel values RGB
          AVal := AScan^;
          inc(AScan);
          AVal := AVal + AScan^;
          inc(AScan);
          AVal := (AVal + AScan^) div 3;
          inc(AScan);
          FMap[APos] := AVal; inc(APos);
        end;
      end;
    chWeighted:
      // (R * 61 + G * 174 + B * 21) / 256
      begin
        for x := 0 to Width - 1 do
        begin
          // Get average (uniform gray) from 3 conseq pixel values RGB
          AVal := AScan^ * 21;
          inc(AScan);
          AVal := AVal + AScan^ * 174;
          inc(AScan);
          AVal := (AVal + AScan^ * 61) div 256;
          inc(AScan);
          FMap[APos] := AVal; inc(APos);
        end;
      end;
    chRed:
      begin
        inc(AScan, 2);
        for x := 0 to Width - 1 do
        begin
          FMap[APos] := AScan^;
          inc(APos);
          inc(AScan, 3);
        end;
      end;
    chGreen:
      begin
        inc(AScan, 1);
        for x := 0 to Width - 1 do
        begin
          FMap[APos] := AScan^;
          inc(APos);
          inc(AScan, 3);
        end;
      end;
    chBlue:
      begin
        for x := 0 to Width - 1 do
        begin
          FMap[APos] := AScan^;
          inc(APos);
          inc(AScan, 3);
        end;
      end;
    end;//case
  end;
end;

procedure TsdBytemap.AssignTo(Dest: TPersistent);
//
var
  x, y: integer;
  AScan: PByte;
  B: byte;
begin
  if Dest is TBitmap then
  begin
    if TBitmap(Dest).PixelFormat = pf24bit then
    begin
      // We will draw grayscale onto this pf24bit bitmap
      TBitmap(Dest).Width  := Width;
      TBitmap(Dest).Height := Height;
      // Process scanlines
      for y := 0 to Height - 1 do
      begin
        AScan := TBitmap(Dest).Scanline[y];
        for x := 0 to Width - 1 do
        begin
          B := FMap[y * Width + x];
          AScan^ := B;
          inc(AScan);
          AScan^ := B;
          inc(AScan);
          AScan^ := B;
          inc(AScan);
        end;
      end;
    end else
    begin
      // Set the bitmap to pf8bit if not yet. The bytemap has no info about
      // palettes, so we do not even try to set one in the bitmap
      TBitmap(Dest).Pixelformat := pf8bit;
      TBitmap(Dest).Width  := Width;
      TBitmap(Dest).Height := Height;
      // Copy per scanline
      for y := 0 to Height - 1 do
        Move(FMap[y * Width], TBitmap(Dest).Scanline[y]^, Width);
    end;
  end else
    inherited;
end;

function TsdByteMap.AsString: string;
var
  x, y: integer;
  ALine: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(Format('Width x Height = %d x %d', [Width, Height]));
    Lines.Add('Hex Map:');
    for y := 0 to Height - 1 do
    begin
      ALine := '';
      for x := 0 to Width - 1 do
        ALine := ALine + IntToHex(Elements[x, y], 2) + ' ';
      Lines.Add(ALine);
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TsdBytemap.AutoCrop(AThresholdValue: integer);
var
  AMap: TsdBytemap;
  Left, Right, Bottom, Top: integer;
  x, y: integer;
  NewWidth, NewHeight: integer;
begin
  Left   := Width;
  Right  := 0;
  Bottom := Height;
  Top    := 0;
  for y := 0 to Height-1 do
    for x := 0 to Width-1 do
      if Elements[x, y] >= AthresholdValue then
      begin
        Left   := min(x, Left);
        Right  := max(x, Right);
        Bottom := min(y, Bottom);
        Top    := max(y, Top);
      end;
  NewWidth  := Max(0, Right - Left + 1);
  NewHeight := Max(0, Top - Bottom + 1);

  AMap :=  TsdBytemap.Create;
  try
    AMap.Assign(self);
    SetSize(NewWidth, NewHeight);
    if NewWidth * NewHeight = 0 then
      exit;
    DrawMap(AMap, -Left, -Bottom);
  finally
    AMap.Free;
  end;
end;

procedure TsdByteMap.Blur(Radius: single);
// This is a novice (not seen before) blurring algorithm that uses recursive
// filters to accomplish a closely matching gaussian blur.
// Copyright (c) by Nils Haeck
var
  i: integer;
  ATmp1, ATmp2: TsdByteMap;
  AHalf1, AHalf2, ALength: integer;
begin
  // Determine recursive filter length
  ALength := round(Radius);
  if ALength < 2 then exit;
  if ALength = 2 then ALength := 3;
  AHalf1 := ALength div 2;
  AHalf2 := AHalf1;
  if not odd(ALength) then
    AHalf2 := Max(1, AHalf2 - 1);

  // Create intermediate bitmap to hold blur temp results
  ATmp1 := TsdByteMap.Create;
  ATmp2 := TsdByteMap.Create;
  try
    ATmp1.SetSize(Width + ALength * 2, Height + ALength * 2);
    ATmp2.SetSize(Width + ALength * 2, Height + ALength * 2);
    ATmp1.Clear(0);
    ATmp1.DrawMap(Self, ALength, ALength);

    // Repeat N times. After some internal debate and extensive testing, it
    // turns out that a value of N=2 is quite adequate. N=3 would also work
    // but since it is slower, it makes no sense. Going to higer values of N
    // will yield too much blur for small blur lengths. Theory states that
    // repeating this infinite times yields the gaussian.. (Rectangle ->
    // Triangle -> Hat-shape). Actually, N=2 means 4x horiz, 4x vertical
    for i := 1 to 2 do
    begin
      RecursiveRectFilter(ATmp1, ATmp2, AHalf1, ALength);
      RecursiveRectFilter(ATmp2, ATmp1, AHalf2, ALength);
    end;

    // Transpose the bits, so that the horizontal recursive filter works on
    // vertical columns
    TransposeByteMap(ATmp1, ATmp2, ATmp1.Width, ATmp1.Height);
    for i := 1 to 2 do
    begin
      RecursiveRectFilter(ATmp2, ATmp1, AHalf2, ALength);
      RecursiveRectFilter(ATmp1, ATmp2, AHalf1, ALength);
    end;

    // Transpose back
    TransposeByteMap(ATmp2, ATmp1, ATmp2.Height, ATmp2.Width);

    // Now we have the result back in DIB
    DrawMap(ATmp1, -ALength, -ALength);
  finally
    ATmp1.Free;
    ATmp2.Free;
  end;

end;

procedure TsdByteMap.BlurExt(RegionX, RegionY, Steps: integer);
var
  i: integer;
  ATmp1, ATmp2: TsdByteMap;
  AHalfX, AHalfY: integer;
begin
  // Determine recursive filter length
  RegionX := max(3, RegionX);
  RegionY := max(3, RegionY);
  if not odd(RegionX) then inc(RegionX);
  if not odd(RegionY) then inc(RegionY);

  AHalfX := RegionX div 2;
  AHalfY := RegionY div 2;

  // Create intermediate bitmap to hold blur temp results
  ATmp1 := TsdByteMap.Create;
  ATmp2 := TsdByteMap.Create;
  try
    ATmp1.SetSize(Width + RegionX * 2, Height + RegionY * 2);
    ATmp2.SetSize(Width + RegionX * 2, Height + RegionY * 2);
    ATmp1.Clear(0);
    ATmp1.DrawMap(Self, RegionX, RegionY);

    // Repeat N times. After some internal debate and extensive testing, it
    // turns out that a value of N=2 is quite adequate. N=3 would also work
    // but since it is slower, it makes no sense. Going to higer values of N
    // will yield too much blur for small blur lengths. Theory states that
    // repeating this infinite times yields the gaussian.. (Rectangle ->
    // Triangle -> Hat-shape). Actually, N=2 means 4x horiz, 4x vertical
    for i := 1 to Steps do
    begin
      RecursiveRectFilter(ATmp1, ATmp2, AHalfX, RegionX);
      RecursiveRectFilter(ATmp2, ATmp1, AHalfX, RegionX);
    end;

    // Transpose the bits, so that the horizontal recursive filter works on
    // vertical columns
    TransposeByteMap(ATmp1, ATmp2, ATmp1.Width, ATmp1.Height);
    for i := 1 to 2 do
    begin
      RecursiveRectFilter(ATmp2, ATmp1, AHalfY, RegionY);
      RecursiveRectFilter(ATmp1, ATmp2, AHalfY, RegionY);
    end;

    // Transpose back
    TransposeByteMap(ATmp2, ATmp1, ATmp2.Height, ATmp2.Width);

    // Now we have the result back in DIB
    DrawMap(ATmp1, -RegionX, -RegionY);
  finally
    ATmp1.Free;
    ATmp2.Free;
  end;
end;

procedure TsdByteMap.BorrowMap(AMap: pointer);
begin
  if assigned(AMap) then
  begin
    if assigned(FMap) and not FBorrowed then
      ReallocMem(FMap, 0);
    FMap := AMap;
    FBorrowed := True;
  end else
  begin
    if FBorrowed then
    begin
      FBorrowed := False;
      ReallocMem(FMap, FWidth * FHeight);
    end;
  end;
end;

procedure TsdByteMap.CenterOfGravity(ARect: TRect; var XPos, YPos: single);
var
  x, y: integer;
  Val: byte;
  Tot, XTot, YTot: int64;
begin
  XTot := 0;
  YTot := 0;
  Tot := 0;
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    for x := ARect.Left to ARect.Right - 1 do
    begin
      Val := Elements[x, y];
      inc(Tot, Val);
      inc(XTot, Val * (x - ARect.Left));
      inc(YTot, Val * (y - ARect.Top));
    end;
  end;
  XPos := XTot / Tot + 0.5 + ARect.Left;
  YPos := YTot / Tot + 0.5 + ARect.Top;
end;

procedure TsdBytemap.Clear(Value: byte);
// Clear the map with Value
begin
  if ElementCount > 0 then
    FillChar(FMap^, ElementCount, Value);
end;

procedure TsdByteMap.CopyFromMapWithLUT(AMap: TsdByteMap; const ALut: TLut);
var
  i: integer;
begin
  for i := 0 to ElementCount - 1 do
    FMap^[i] := ALut[AMap.FMap^[i]];
end;

destructor TsdBytemap.Destroy;
begin
  if not FBorrowed then
    ReallocMem(FMap, 0);
  inherited;
end;

procedure TsdByteMap.DownScale(NewWidth, NewHeight: integer);
  // local
  procedure CreateDivision(Num, Den: integer; var Start, Size: array of integer);
  // Create Den parts from Num, with startpoints in Start (zerobased) and Sizes in Size
  var
    i, Run, Left, Width: integer;
  begin
    Run := 0;
    Left := 0;
    Width := Num div Den;
    for i := 0 to Den - 1 do
    begin
      Start[i] := Left;
      Size[i] := Width;
      Run := Run + Num - Width * Den;
      if Run >= Den then
      begin
        inc(Size[i]);
        Run := Run - Den;
      end;
      inc(Left, Size[i]);
    end;
  end;
var
  r, br, c, bc: integer;
  XStart, XSize: array of integer;
  YStart, YSize: array of integer;
  Target: array of integer;
  Scan: PByte;
  Area: integer;
// main
begin
  if (NewWidth * NewHeight  = 0) or (FWidth * FHeight = 0) or (NewWidth > FWidth) or (NewHeight > FHeight) then
    raise Exception.Create(sbmInvalidNewSize);

  SetLength(XStart, NewWidth);
  SetLength(XSize , NewWidth);
  SetLength(YStart, NewHeight);
  SetLength(YSize , NewHeight);
  SetLength(Target, NewWidth);

  // Create divisions
  CreateDivision(FWidth, NewWidth , XStart, XSize);
  CreateDivision(FHeight, NewHeight, YStart, YSize);

  // Rows
  for r := 0 to NewHeight - 1 do
  begin
    FillChar(Target[0], Length(Target) * SizeOf(integer), 0);
    for br := YStart[r] to YStart[r] + YSize[r] - 1 do
    begin
      // Scan is a pointer to the first byte of the row in the source rect
      Scan := PByte(ScanLine[br]);
      for c := 0 to NewWidth - 1 do
      begin
        for bc := 1 to XSize[c] do
        begin
          inc(Target[c], Scan^);
          inc(Scan);
        end;
      end;
    end;

    // averages
    for c := 0 to NewWidth - 1 do
    begin
      Area := YSize[r] * XSize[c];
      FMap^[r * NewWidth + c] := Target[c] div Area;
    end;
  end;

  // Set new map size
  SetSize(NewWidth, NewHeight);
end;

procedure TsdByteMap.DrawMap(AMap: TsdByteMap; X, Y: integer);
// this procedure can be optimized considerably using Move
var
  xi, yi: integer;
begin
  for yi := 0 to AMap.Height - 1 do
  begin
    for xi := 0 to AMap.Width - 1 do
    begin
      Elements[X + xi, Y + yi] := AMap.Elements[xi, yi];
    end;
  end;
end;

function TsdBytemap.GetElements(x, y: integer): byte;
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    Result := FMap[y * Width + x]
  else
    Result := 0;
end;

function TsdByteMap.GetScanline(y: integer): PByteArray;
begin
  Result := @FMap^[y * FWidth]
end;

function TsdBytemap.Get_S256(X256, Y256: integer): byte;
// Bilinear interpolation using SAR_8 coordinates (256 fractional values)
var
  flrx, flry, celx, cely: integer;
  C1, C2, C3, C4: byte;
  Idx: integer;
  X, Y: integer;
// Main
begin
  flrx := X256 and $FF;
  flry := Y256 and $FF;

  X := SAR_8(X256);
  Y := SAR_8(Y256);

  celx := flrx xor 255;
  cely := flry xor 255;

  if (X >= 0) and (X < Width  - 1) and
     (Y >= 0) and (Y < Height - 1) then
  begin
    // We are inside the map, interpolate between four neighbors
    Idx := X + Y * Width;
    C1 := FMap[Idx];
    inc(Idx);
    C2 := FMap[Idx];
    inc(Idx, Width);
    C4 := FMap[Idx];
    dec(Idx);
    C3 := FMap[Idx];
    Result := CombineByte(CombineByte(C1, C2, celx), CombineByte(C3, C4, celx), cely);
  end else
  begin
    if (X < -1) or (Y < -1) or (X >= Width) or (Y >= Height) then
    begin
      // (X,Y) coordinate is out of the SrcRect, do not interpolate
      Result := 0;
    end else
    begin
      // Handle edge points, use Elements[] to avoid polling outside the map.
      C1 := Elements[X, Y];
      C2 := Elements[X + 1, Y];
      C3 := Elements[X, Y + 1];
      C4 := Elements[X + 1, Y + 1];
      Result := CombineByte(CombineByte(C1, C2, celx), CombineByte(C3, C4, celx), cely);
    end;
  end;
end;

procedure TsdBytemap.Histogram(var Histo: THisto);
// Create a histogram of the data in the map
var
  i: integer;
  S: PByte;
begin
  // Initialize
  FillChar(Histo, SizeOf(Histo), 0);
  // Histogram
  S := PByte(FMap);
  for i := 0 to ElementCount - 1 do
  begin
    inc(Histo[S^]);
    inc(S);
  end;
end;

procedure TsdByteMap.HistogramStretch;
var
  i: integer;
  Histo: THisto;
  Map: array[0..255] of byte;
  Delta, Limit: double;
  Lim, Count, R, Err1, Err2: integer;
  P: PByte;
begin
  // Original histogram
  Histogram(Histo);

  // Calculate stretch map
  Delta := ElementCount / 255;
  Limit := 0;
  Count := 0;
  R := 0;
  for i := 0 to 255 do
  begin
    Limit := Limit + Delta;
    Lim := round(Limit);
    repeat
      Err1 := abs(Count            - Lim);
      Err2 := abs(Count + Histo[R] - Lim);
      if Err2 <= Err1 then
      begin
        Count := Count + Histo[R];
        Map[R] := i;
        inc(R);
      end;
      if R > 255 then
        break;
    until Err2 > Err1;
    if R > 255 then
      break;
  end;
  while R < 255 do
  begin
    Map[R] := 255;
    inc(R);
  end;

  // Change values based on new histogram
  P := PByte(FMap);
  for i := 0 to ElementCount - 1 do
  begin
    P^ := Map[P^];
    inc(P);
  end;
  Histogram(Histo);
  for i := 0 to 255 do
    Histo[i] := 0;
end;

procedure TsdByteMap.IncreaseSize(AFactor: integer);
var
  x, y, Ydiv: integer;
  NewWidth, NewHeight: integer;
  Temp: TsdByteMap;
begin
  // We cannot resize a borrowed map
  if FBorrowed then
    exit;

  // Check if we have anything to do
  if AFactor <= 1 then
    exit;

  NewWidth  := Width * AFactor;
  NewHeight := Height * AFactor;

  // Temporary copy of ourself
  Temp := TsdByteMap.Create;
  try
    Temp.Assign(Self);
    // Set new size
    SetSize(NewWidth, NewHeight);
    // Loop through all pixels and calculate averages
    for y := 0 to NewHeight - 1 do
    begin
      Ydiv := y div AFactor;
      for x := 0 to NewWidth - 1 do
        Elements[x, y] := Temp[x div AFactor, Ydiv];
    end;
  finally
    Temp.Free;
  end;
end;

procedure TsdByteMap.Invert;
var
  i: integer;
begin
  for i := 0 to ElementCount - 1 do
    FMap[i] := FMap[i] xor $FF;
end;

function TsdBytemap.MeanValue: double;
// Calculate the mean value of the map
var
  i, ATotal: integer;
begin
  Result := 0;
  if ElementCount = 0 then
    exit;

  ATotal := 0;
  for i := 0 to ElementCount - 1 do
    ATotal := ATotal + FMap[i];
  Result := ATotal / ElementCount;
end;

procedure TsdBytemap.Normalize(ALow, AHigh: byte);
// Normalize the map between ALow and AHigh. Since this is a byte map (integers),
// information may get lost!
var
  i: integer;
  AMin, AMax, ARange, ADelta: byte;
begin
  if ElementCount <= 0 then
    exit;

  // Find minimum/maximum
  AMin := FMap[0];
  AMax := FMap[0];
  for i := 1 to ElementCount - 1 do
  begin
    AMin := Min(FMap[i], AMin);
    AMax := Max(FMap[i], AMax);
  end;

  // Determine range
  ARange := AMax - AMin;
  if ARange = 0 then
  begin
    // If all values are the same then set to ALow
    for i := 0 to ElementCount - 1 do
      FMap[i] := ALow;
  end else
  begin
    ADelta := AHigh - ALow;
    // Normalize
    for i := 0 to ElementCount - 1 do
      FMap[i] := MulDiv(FMap[i] - AMin, ADelta, ARange) + ALow;
  end;
end;

procedure TsdByteMap.ReduceSize(AFactor: integer);
var
  xi, yi, xc, yc, x, y: integer;
  Sum, Area, NewWidth, NewHeight: integer;
begin
  // We cannot resize a borrowed map
  if FBorrowed then
    exit;

  // Check if we have anything to do
  if AFactor <= 1 then
    exit;

  Area := sqr(AFactor);
  NewWidth  := Width div AFactor;
  NewHeight := Height div AFactor;

  // Loop through all pixels and calculate averages
  for yi := 0 to NewHeight - 1 do
  begin
    y := yi * AFactor;
    for xi := 0 to NewWidth - 1 do
    begin
      x := xi * AFactor;
      // Calculate sum for destination pixel
      Sum := 0;
      for yc := 0 to AFactor - 1 do
        for xc := 0 to AFactor - 1 do
          inc(Sum, Elements[x + xc, y + yc]);
      // Calculate value of destination pixel and set it
      Elements[xi, yi] := Sum div Area;
    end;
  end;

  // Move parts of the map
  for yi := 1 to NewHeight - 1 do
    Move(FMap[yi * Width], FMap[yi * NewWidth], NewWidth);

  // Resize the map
  SetSize(NewWidth, NewHeight);
end;

procedure TsdByteMap.SetBorrowed(const Value: boolean);
begin
  if FBorrowed <> Value then
  begin
    if not FBorrowed then
      ReallocMem(FMap, 0);
    FBorrowed := Value;
    if not FBorrowed then
      ReallocMem(FMap, FWidth * FHeight);
  end;
end;

procedure TsdBytemap.SetElements(x, y: integer; const Value: byte);
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    FMap[y * Width + x] := Value;
end;

procedure TsdBytemap.SetSize(AWidth, AHeight: integer);
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then
    exit;

  FWidth := AWidth;
  FHeight := AHeight;

  if not FBorrowed then
    ReallocMem(FMap, FWidth * FHeight);
end;

procedure TsdByteMap.ShiftBits(Shift: integer);
var
  i, AShift: integer;
begin
  if Shift = 0 then
    exit;

  AShift := abs(Shift);
  if Shift < 0 then
    for i := 0 to ElementCount - 1 do
      FMap[i] := FMap[i] shl AShift
  else
    for i := 0 to ElementCount - 1 do
      FMap[i] := FMap[i] shr AShift
end;

procedure TsdByteMap.SignedNormalize(ALow, AHigh: shortint);
// Assume that the bytevalues are shortint's and normalize them
var
  i: integer;
  AMin, AMax: shortint;
  ARange: integer;
  Scale: single;
begin
  if ElementCount <= 0 then
    exit;

  // Find minimum/maximum
  AMin := FMap[0];
  AMax := FMap[0];
  for i := 1 to ElementCount - 1 do
  begin
    AMin := Min(shortInt(FMap[i]), AMin);
    AMax := Max(shortint(FMap[i]), AMax);
  end;

  // Determine range
  ARange := AMax - AMin;
  if ARange = 0 then
  begin

    // If all values are the same then set to 0
    for i := 0 to ElementCount - 1 do
      FMap[i] := 0;

  end else
  begin

    if (AMax > 0) and (AHigh > 0) then
    begin
      Scale := AHigh / AMax;
      if Scale <> 1.0 then
      begin
        // Normalize
        for i := 0 to ElementCount - 1 do
        begin
          if FMap[i] > 0 then
            FMap[i] := round(shortint(FMap[i]) * Scale);
        end;
      end;
    end;

    if (AMin < 0) and (ALow < 0) then
    begin
      Scale := ALow / AMin;
      if Scale <> 1.0 then
      begin
        // Normalize
        for i := 0 to ElementCount - 1 do
        begin
          if FMap[i] > 0 then
            FMap[i] := round(shortint(FMap[i]) * Scale);
        end;
      end;
    end;
  end;
end;

procedure TsdByteMap.SortedHistogram(var Histo: THisto; var Sort: TLut);
var
  i, j: integer;
  Temp: byte;
begin
  Histogram(Histo);

  // Sort histogram by occurance
  for i := 0 to 255 do
    Sort[i] := i;

  for i := 0 to 254 do
  begin
    for j := 255 downto i + 1 do
    begin
      if Histo[Sort[i]] < Histo[Sort[j]] then
      begin
        Temp := Sort[i];
        Sort[i] := Sort[j];
        Sort[j] := Temp;
      end;
    end;
  end;
end;

function TsdByteMap.Threshold(Threshold: byte): integer;
var
  i: integer;
  P: Pbyte;
begin
  P := PByte(FMap);
  Result := 0;
  for i := 0 to ElementCount - 1 do
  begin
    if P^ >= Threshold then
    begin
      P^ := 255;
      inc(Result);
    end else
      P^ := 0;
    inc(P);
  end;
end;

procedure TsdByteMap.XorMap(AMap: TsdByteMap);
var
  i, ALength: integer;
  Sc, Rc: PCardinal;
  Sb, Rb: PByte;
begin
  if not assigned(AMap) or (Width <> AMap.Width) or (Height <> AMap.Height) then
    exit;

  ALength := (Width * Height) div 4;
  Sc := PCardinal(FMap);
  Rc := PCardinal(AMap.MapPointer);

  // Use full cardinals for bulk
  for i := 0 to ALength - 1 do
  begin
    Sc^ := Sc^ xor Rc^;
    inc(Sc);
    inc(Rc);
  end;
  // Rest
  Sb := PByte(Sc);
  Rb := PByte(Rc);
  for i := ALength * 4 to ElementCount - 1 do
  begin
    Sb^ := Sb^ xor Rb^;
    inc(Sb);
    inc(Rb);
  end;
end;

{ TsdSmallIntMap }

procedure TsdSmallIntMap.Assign(Source: TPersistent);
var
  i: integer;
  S: PByte;
  D: PSmallInt;
begin
  if Source is TsdByteMap then
  begin
    SetSizeToMap(TsdByteMap(Source));
    S := PByte(TsdByteMap(Source).MapPointer);
    D := PSmallInt(MapPointer);
    for i := 0 to ElementCount - 1 do
    begin
      D^ := S^;
      inc(D);
      inc(S);
    end;
  end else
    inherited;
end;

procedure TsdSmallIntMap.Clear(Value: smallint);
// Clear the map with Value
var
  i: integer;
begin
  if ElementCount = 0 then
    exit;

  for i := 0 to ElementCount - 1 do
    FMap[i] := Value;
end;

destructor TsdSmallIntMap.Destroy;
begin
  ReallocMem(FMap, 0);
  inherited;
end;

procedure TsdSmallIntMap.DifferenceHistogram(var Histo: TDiffHisto);
// Create a histogram of the data in the map
var
  i: integer;
  S: PSmallInt;
begin
  // Initialize
  FillChar(Histo, SizeOf(Histo), 0);

  // Histogram
  S := PSmallInt(FMap);
  for i := 0 to ElementCount - 1 do
  begin
    // Clamp value between -255 and 255, and add to histogram
    inc(Histo[Min(255, Max(-255, S^))]);
    inc(S);
  end;
end;

function TsdSmallIntMap.GetElements(x, y: integer): SmallInt;
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    Result := FMap[y * Width + x]
  else
    Result := 0;
end;

function TsdSmallIntMap.GetElementsMirrored(x, y: integer): SmallInt;
begin
  if x < 0 then
  begin
    Result := 2 * ElementsMirrored[0, y] - ElementsMirrored[-x, y];
    exit;
  end;

  if x >= Width then
  begin
    Result := 2 * ElementsMirrored[Width - 1, y] - ElementsMirrored[2 * Width - x - 2, y];
    exit;
  end;

  if y < 0 then
  begin
    Result := 2 * ElementsMirrored[x, 0] - ElementsMirrored[x, -y];
    exit;
  end;

  if y >= Height then
  begin
    Result := 2 * ElementsMirrored[x, Height - 1] - ElementsMirrored[x, 2 * Height - y - 2];
    exit;
  end;

  Result := FMap[y * Width + x]
end;

function TsdSmallIntMap.GetScanLine(y: integer): PSmallIntArray;
begin
  Result := @FMap^[y * FWidth]
end;

procedure TsdSmallIntMap.HorizontalGradientOf(AMap: TsdMap;
  KernelSize: integer);
var
  r, c, KernelHalf: integer;
  TotalV: smallint;
  GradientLine: array of SmallInt;
  PS: PSmallInt;

  // local
  procedure BuildGradientLine(Row: integer);
  var
    c: integer;
    PB: PByte;
    OldV, NewV: SmallInt;
  begin
    if AMap is TsdByteMap then
    begin
      PB := PByte(TsdByteMap(AMap).ScanLine[Row]);
      OldV := PB^;
      inc(PB);
      for c := KernelHalf to Width - KernelHalf do
      begin
        NewV := PB^;
        GradientLine[c] := NewV - OldV;
        OldV := NewV;
        inc(PB);
      end;
    end;
    for c := 0 to KernelHalf - 1 do
      GradientLine[c] := GradientLine[KernelHalf];
    for c := Width - KernelHalf + 1 to Width + KernelSize - 2 do
      GradientLine[c] := GradientLine[Width - KernelHalf - 1];
  end;

// main
begin
  if not (AMap is TsdByteMap) and not (AMap is TsdSmallIntMap) then
    exit;
  if (KernelSize < 2) or (KernelSize > cMaxGradKernelSize) or odd(KernelSize) then
    raise Exception.Create(sbmInvalidKernelSize);

  KernelHalf := KernelSize div 2;
  SetSizeToMap(AMap);

  // Set length of gradient line
  SetLength(GradientLine, Width + KernelSize);

  // Loop through rows
  for r := 0 to Height - 1 do
  begin
    // Build gradient line
    BuildGradientLine(r);
    // Prepare Kernel
    TotalV := 0;
    PS := PSmallInt(ScanLine[r]);
    for c := 0 to KernelSize - 1 do
      inc(TotalV, GradientLine[c]);
    for c := 0 to Width - 1 do
    begin
      PS^ := TotalV div KernelSize;
      inc(TotalV, GradientLine[c + KernelSize]);
      dec(TotalV, GradientLine[c]);
      inc(PS);
    end;
  end;
end;

procedure TsdSmallIntMap.SetElements(x, y: integer; const Value: SmallInt);
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    FMap[y * Width + x] := Value;
end;

procedure TsdSmallIntMap.SetSize(AWidth, AHeight: integer);
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then
    exit;

  FWidth := AWidth;
  FHeight := AHeight;
  ReallocMem(FMap, FWidth * FHeight * SizeOf(SmallInt));
end;

procedure TsdSmallIntMap.SubstractMaps(MapA, MapB: TsdMap);
// Take the difference of MapA and MapB (or: MapA - MapB)
var
  i: integer;
  Ds: PSmallInt;
  Ab, Bb: PByte;
begin
  if not (SizeEquals(MapA) and SizeEquals(MapB)) then
    raise Exception.Create(sbmImcompatibleMapSizes);

  if (MapA is TsdByteMap) and (MapB is TsdByteMap) then
  begin
    Ds := PSmallInt(FMap);
    Ab := PByte(TsdByteMap(MapA).MapPointer);
    Bb := PByte(TsdByteMap(MapB).MapPointer);
    for i := 0 to ElementCount - 1 do
    begin
      Ds^ := Ab^ - Bb^;
      inc(Ds);
      inc(Ab);
      inc(Bb);
    end;
  end else
    raise Exception.Create(sbmImcompatibleMapTypes);
end;

{ TsdWordMap }

procedure TsdWordMap.Clear(Value: word);
// Clear the map with Value
var
  i: integer;
begin
  if ElementCount = 0 then
    exit;

  for i := 0 to ElementCount - 1 do
    FMap[i] := Value;
end;

procedure TsdWordMap.CopyRectangle(Source: TsdWordMap; ARect: TRect);
var
  y: integer;
begin
  SetSize(0, 0);
  if not assigned(Source) then
    exit;

  ARect.Left := Max(0, ARect.Left);
  ARect.Top := Max(0, ARect.Top);
  ARect.Right := Min(Source.Width, ARect.Right);
  ARect.Bottom := Min(Source.Height, ARect.Bottom);
  if (ARect.Left >= ARect.Right) or (ARect.Top >= ARect.Bottom) then
    exit;

  // Set the size
  SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  // Do the copy
  for y := 0 to Height - 1 do
    Move(
      Source.ElementPtr[ARect.Left, ARect.Top + y]^,
      ElementPtr[0, y]^, Width * SizeOf(Word));
end;

destructor TsdWordMap.Destroy;
begin
  ReallocMem(FMap, 0);
  inherited;
end;

function TsdWordMap.GetElementPtr(x, y: integer): pointer;
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    Result := @FMap[y * Width + x]
  else
    Result := nil;
end;

function TsdWordMap.GetElements(x, y: integer): word;
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    Result := FMap[y * Width + x]
  else
    Result := 0;
end;

procedure TsdWordMap.InvertX;
var
  x, y: integer;
  Buf: array of word;
  W1, W2: PWord;
begin
  if IsEmpty then
    exit;

  SetLength(Buf, FWidth);
  for y := 0 to FHeight - 1 do
  begin
    // Invert scanline
    W1 := PWord(@FMap[y * Width]);
    W2 := PWord(@Buf[FWidth - 1]);
    for x := 0 to FWidth - 1 do
    begin
      W2^ := W1^;
      inc(W1);
      dec(W2);
    end;
    // Write back result
    Move(Buf[0], FMap[y * Width], FWidth * SizeOf(word));
  end;
end;

procedure TsdWordMap.LoadFromFile(const AFileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TsdWordMap.LoadFromStream(S: TStream);
var
  AWidth, AHeight: integer;
begin
  S.Read(AWidth, SizeOf(AWidth));
  S.Read(AHeight, SizeOf(AHeight));
  SetSize(AWidth, AHeight);
  S.Read(FMap^, SizeOf(word) * ElementCount);
end;

procedure TsdWordMap.SaveToFile(const AFileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TsdWordMap.SaveToStream(S: TStream);
begin
  S.Write(FWidth, SizeOf(FWidth));
  S.Write(FHeight, SizeOf(FHeight));
  S.Write(FMap^, SizeOf(word) * ElementCount);
end;

procedure TsdWordMap.SetElements(x, y: integer; const Value: word);
begin
  if (x >= 0) and (x < Width) and
     (y >= 0) and (y < Height) then
    FMap[y * Width + x] := Value;
end;

procedure TsdWordMap.SetSize(AWidth, AHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  ReallocMem(FMap, FWidth * FHeight * SizeOf(word));
end;

{ additional utility procedures }

procedure SetBitmap8bitPalette(const Palette: T8bitPaletteArray; Bitmap: TBitmap);
// Add a 256-color palette to a bitmap
var
  i, y: integer;
  pal: PLogPalette;
  hpal: HPALETTE;
begin
  {$R-}
  if not assigned(Bitmap) then
    exit;

  // 8 bits per pixel
  Bitmap.PixelFormat := pf8bit;

  // Create a gradient palette between foreground and background color
  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 256);
  try
    pal.palVersion := $300;
    pal.palNumEntries := 256;
    for i := 0 to 255 do begin
      pal.palPalEntry[i].peRed   := Palette[i].R;
      pal.palPalEntry[i].peGreen := Palette[i].G;
      pal.palPalEntry[i].peBlue  := Palette[i].B;
    end;
    hpal := CreatePalette(pal^);
    if hpal <> 0 then
      Bitmap.Palette := hpal;
  finally
    FreeMem(pal);
  end;

  // Fill bitmap with background color
  for y := 0 to Bitmap.Height - 1 do
    FillChar(Bitmap.Scanline[y]^, Bitmap.Width, 0);
  {$R+}  
end;

procedure SetBitmap8bitGrayscale(Bitmap: TBitmap);
var
  i: integer;
  APal: T8bitPaletteArray;
begin
  // Create grayscale palette
  for i := 0 to 255 do
  begin
    APal[i].R := i;
    APal[i].G := i;
    APal[i].B := i;
  end;

  // Set it for the bitmap
  SetBitmap8bitPalette(APal, Bitmap);
end;

procedure SetBitmap8bitInverseGrayscale(Bitmap: TBitmap);
var
  i: integer;
  APal: T8bitPaletteArray;
begin
  // Create grayscale palette
  for i := 0 to 255 do
  begin
    APal[i].R := 255 - i;
    APal[i].G := 255 - i;
    APal[i].B := 255 - i;
  end;

  // Set it for the bitmap
  SetBitmap8bitPalette(APal, Bitmap);
end;

procedure SetBitmap8bitRainbow(Bitmap: TBitmap);
// Use a "rainbow" palette for Bitmap
var
  i: integer;
  Col1, Col2, Col3, Col4, Col5: integer;
  APal: T8bitPaletteArray;
begin
  Col1 := $00FF00FF; // Magenta
  Col2 := $00FF0000; // Blue
  Col3 := $0000FF00; // Green
  Col4 := $0000FFFF; // Yellow
  Col5 := $000000FF; // Red
  //
  for i := 0 to 50 do
  begin
    APal[i      ].Color := InterpolateColor(Col1, Col2, i / 51);
    APal[i +  51].Color := InterpolateColor(Col2, Col3, i / 51);
    APal[i + 102].Color := InterpolateColor(Col3, Col4, i / 51);
    APal[i + 153].Color := InterpolateColor(Col4, Col5, i / 51);
    APal[i + 204].Color := InterpolateColor(Col5, Col1, i / 51);
  end;
  APal[255] := APal[0];

  // Set it for the bitmap
  SetBitmap8bitPalette(APal, Bitmap);
end;

function InterPolateColor(Col1, Col2: longint; Frac: single): longint;
begin
  Result :=
    Round((Col1 shr 16 and $FF) * (1 - Frac) + (Col2 shr 16 and $FF) * Frac) shl 16 +
    Round((Col1 shr  8 and $FF) * (1 - Frac) + (Col2 shr  8 and $FF) * Frac) shl  8 +
    Round((Col1        and $FF) * (1 - Frac) + (Col2        and $FF) * Frac);
end;

procedure AbsDifferentialMap(Map1, Map2, Dest: TsdByteMap);
// Find the absolute differential of Map1 and Map2 and store in Result. The
// absolute differential is defined as Result[i, j] := abs(Map2[i, j] - Map1[i, j])
var
  x, y: integer;
begin
  // Checks
  if not assigned(Map1) or not assigned(Map2) or not assigned(Dest) then
    exit;

  // Ensure size is adequate
  Dest.SetSize(Map1.Width, Map1.Height);
  // And calculate absolute differential
  for y := 0 to Dest.Height  - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      Dest[x, y] := abs(Map2[x, y] - Map1[x, y]);
    end;
  end;
end;

procedure RelDifferentialMap(Map1, Map2: TsdByteMap; Dest: TsdSmallIntMap);
// Find the relative differential of Map1 and Map2 and store in Result. The
// relative differential is defined as Result[i, j] := Map2[i, j] - Map1[i, j]
var
  x, y: integer;
begin
  // Checks
  if not assigned(Map1) or not assigned(Map2) or not assigned(Dest) then
    exit;

  // Ensure size is adequate
  Dest.SetSize(Map1.Width, Map1.Height);

  // And calculate absolute differential
  for y := 0 to Dest.Height  - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      Dest[x, y] := Map2[x, y] - Map1[x, y];
    end;
  end;
end;

procedure RelDifferentialMap(Map1, Map2, Dest: TsdByteMap);
// Differential map, but now as bytemap. We must store each difference's div2
// otherwise we can get ugly overflows
var
  x, y, ADiff: integer;
begin
  // Checks
  if not assigned(Map1) or not assigned(Map2) or not assigned(Dest) then
    exit;

  // Ensure size is adequate
  Dest.SetSize(Map1.Width, Map1.Height);

  // And calculate absolute differential
  for y := 0 to Dest.Height  - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      ADiff := Map2[x, y] - Map1[x, y];
      if ADiff > 0 then
        ADiff := Min(127, (ADiff + 1) div 2)
      else
        ADiff := Max(-127, (ADiff - 1) div 2);
      Dest[x, y] := byte(ADiff); // This will wrap around for neg numbers
    end;
  end;
end;

procedure LocalMinimumMap(Source, Dest: TsdByteMap; Cols, Rows: integer);
// Store the minimum of each Rows x Cols block
var
  x, y, x1, Rdiv2, Cdiv2: integer;
  AMin: byte;
  AMinPos, APos: integer;
  ATmp: TsdByteMap;
begin
  Rdiv2 := Rows div 2;
  Cdiv2 := Cols div 2;

  // Size of destination map
  Dest.SetSize(Source.Width, Source.Height);
  ATmp := TsdByteMap.Create;
  try
    // An intermediate map that is transposed
    ATmp.SetSize(Source.Height, Source.Width);

    // Work first on rows
    for y := 0 to Source.Height - 1 do
    begin
      AMin := 255;
      AMinPos := - 2 * Cdiv2 - 1;
      for x := -Cdiv2 to Source.Width - 1 do
      begin
        APos := x + Cdiv2;
        if Source[APos, y] <= AMin then
        begin
          // The new minimum
          AMinPos := APos;
          AMin := Source[APos, y];
        end else
        begin
          if AMinPos < x - Cdiv2 then
          begin
            // Find new minimum
            AMinPos := APos;
            AMin := Source[AMinPos, y];
            for x1 := APos - 1 downto x - Cdiv2 do
              if Source[x1, y] < AMin then
              begin
                AMin := Source[x1, y];
                AMinPos := x1;
              end;
          end;
        end;
        ATmp[y, x] := AMin;
      end;
    end;

    // And now work on the columns
    for y := 0 to ATmp.Height - 1 do
    begin
      AMin := 255;
      AMinPos := - 2 * Rdiv2 - 1;
      for x := -Rdiv2 to ATmp.Width - 1 do
      begin
        APos := x + Rdiv2;
        if ATmp[APos, y] <= AMin then
        begin
          // The new maximum
          AMinPos := APos;
          AMin := ATmp[APos, y];
        end else
        begin
          if AMinPos < x - Rdiv2 then
          begin
            // Find new maximum
            AMinPos := APos;
            AMin := ATmp[AMinPos, y];
            for x1 := APos - 1 downto x - Rdiv2 do
              if ATmp[x1, y] < AMin then
              begin
                AMin := ATmp[x1, y];
                AMinPos := x1;
              end;
          end;
        end;
        Dest[y, x] := AMin;
      end;
    end;

  finally
    ATmp.Free;
  end;
end;

procedure SplitDifferenceMap(SrcBefore, SrcAfter, DstBefore, DstAfter: TsdByteMap);
var
  x, y, V: integer;
begin
  // Checks
  if not assigned(SrcBefore) or not assigned(SrcAfter) or
     not assigned(DstBefore) or not assigned(DstAfter) then
    exit;

  // Ensure size is adequate
  DstBefore.SetSize(SrcBefore.Width, SrcBefore.Height);
  DstAfter.SetSize(SrcBefore.Width, SrcBefore.Height);
  DstBefore.Clear;
  DstAfter.Clear;
  for y := 0 to SrcBefore.Height  - 1 do
    for x := 0 to SrcBefore.Width - 1 do
    begin
      V := SrcAfter[x, y] - SrcBefore[x, y];
      if V > 0 then
        DstAfter[x, y] := V
      else
        DstBefore[x, y] := -V;
    end;
end;

procedure LocalMaximumMap(Source, Dest: TsdByteMap; Rows, Cols: integer);
// Store the maximum of each Rows x Cols block
var
  x, y, x1, Rdiv2, Cdiv2: integer;
  AMax: byte;
  AMaxPos, APos: integer;
  ATmp: TsdByteMap;
begin
  Rdiv2 := Rows div 2;
  Cdiv2 := Cols div 2;

  // Size of destination map
  Dest.SetSize(Source.Width, Source.Height);
  ATmp := TsdByteMap.Create;
  try
    // An intermediate map that is transposed
    ATmp.SetSize(Source.Height, Source.Width);

    // Work first on rows
    for y := 0 to Source.Height - 1 do
    begin
      AMax := 0;
      AMaxPos := - 2 * Cdiv2 - 1;
      for x := -Cdiv2 to Source.Width - 1 do
      begin
        APos := x + Cdiv2;
        if Source[APos, y] >= AMax then
        begin
          // The new maximum
          AMaxPos := APos;
          AMax := Source[APos, y];
        end else
        begin
          if AMaxPos < x - Cdiv2 then
          begin
            // Find new maximum
            AMaxPos := APos;
            AMax := Source[AMaxPos, y];
            for x1 := APos - 1 downto x - Cdiv2 do
            begin
              if Source[x1, y] > AMax then
              begin
                AMax := Source[x1, y];
                AMaxPos := x1;
              end;
            end;
          end;
        end;
        ATmp[y, x] := AMax;
      end;
    end;

    // And now work on the columns
    for y := 0 to ATmp.Height - 1 do
    begin
      AMax := 0;
      AMaxPos := - 2 * Rdiv2 - 1;
      for x := -Rdiv2 to ATmp.Width - 1 do
      begin
        APos := x + Rdiv2;
        if ATmp[APos, y] >= AMax then
        begin
          // The new maximum
          AMaxPos := APos;
          AMax := ATmp[APos, y];
        end else
        begin
          if AMaxPos < x - Rdiv2 then
          begin
            // Find new maximum
            AMaxPos := APos;
            AMax := ATmp[AMaxPos, y];
            for x1 := APos - 1 downto x - Rdiv2 do
            begin
              if ATmp[x1, y] > AMax then
              begin
                AMax := ATmp[x1, y];
                AMaxPos := x1;
              end;
            end;
          end;
        end;
        Dest[y, x] := AMax;
      end;
    end;

  finally
    ATmp.Free;
  end;
end;

procedure LocalMinAndMaxMap(Source, MinMap, MaxMap: TsdByteMap; Cols, Rows: integer);
// Store the maximum and minimum of each Rows x Cols block
var
  x, y, x1, Rdiv2, Cdiv2: integer;
  AMax, AMin: byte;
  AMaxPos, AMinPos, APos: integer;
  AMaxTmp, AMinTmp: TsdByteMap;
begin
  Rdiv2 := Rows div 2;
  Cdiv2 := Cols div 2;

  // Size of destination maps
  MaxMap.SetSize(Source.Width, Source.Height);
  MinMap.SetSize(Source.Width, Source.Height);
  AMaxTmp := TsdByteMap.Create;
  AMinTmp := TsdByteMap.Create;
  try
    // Intermediate maps that are transposed
    AMaxTmp.SetSize(Source.Height, Source.Width);
    AMinTmp.SetSize(Source.Height, Source.Width);

    // Work first on rows
    for y := 0 to Source.Height - 1 do
    begin
      AMax := 0;
      AMin := 255;
      AMaxPos := - 2 * Cdiv2 - 1;
      AMinPos := AMaxPos;
      for x := -Cdiv2 to Source.Width - 1 do
      begin
        APos := x + Cdiv2;
        // Maximum calculation
        if Source[APos, y] >= AMax then
        begin
          // The new maximum
          AMaxPos := APos;
          AMax := Source[APos, y];
        end else
        begin
          if AMaxPos < x - Cdiv2 then
          begin
            // Find new maximum
            AMaxPos := APos;
            AMax := Source[AMaxPos, y];
            for x1 := APos - 1 downto x - Cdiv2 do
            begin
              if Source[x1, y] > AMax then
              begin
                AMax := Source[x1, y];
                AMaxPos := x1;
              end;
            end;
          end;
        end;
        AMaxTmp[y, x] := AMax;

        // Minimum calculation
        if Source[APos, y] <= AMin then
        begin
          // The new maximum
          AMinPos := APos;
          AMin := Source[APos, y];
        end else
        begin
          if AMinPos < x - Cdiv2 then
          begin
            // Find new maximum
            AMinPos := APos;
            AMin := Source[AMinPos, y];
            for x1 := APos - 1 downto x - Cdiv2 do
            begin
              if Source[x1, y] < AMin then
              begin
                AMin := Source[x1, y];
                AMinPos := x1;
              end;
            end;
          end;
        end;
        AMinTmp[y, x] := AMin;
      end;
    end;

    // And now work on the columns
    for y := 0 to AMaxTmp.Height - 1 do
    begin
      // Maximum calculation
      AMax := 0;
      AMaxPos := - 2 * Rdiv2 - 1;
      for x := -Rdiv2 to AMaxTmp.Width - 1 do
      begin
        APos := x + Rdiv2;
        if AMaxTmp[APos, y] >= AMax then
        begin
          // The new maximum
          AMaxPos := APos;
          AMax := AMaxTmp[APos, y];
        end else
        begin
          if AMaxPos < x - Rdiv2 then
          begin
            // Find new maximum
            AMaxPos := APos;
            AMax := AMaxTmp[AMaxPos, y];
            for x1 := APos - 1 downto x - Rdiv2 do
            begin
              if AMaxTmp[x1, y] > AMax then
              begin
                AMax := AMaxTmp[x1, y];
                AMaxPos := x1;
              end;
            end;
          end;
        end;
        MaxMap[y, x] := AMax;
      end;

      // Minimum calculation
      AMin := 255;
      AMinPos := - 2 * Rdiv2 - 1;
      for x := -Rdiv2 to AMinTmp.Width - 1 do
      begin
        APos := x + Rdiv2;
        if AMinTmp[APos, y] <= AMin then
        begin
          // The new maximum
          AMinPos := APos;
          AMin := AMinTmp[APos, y];
        end else
        begin
          if AMinPos < x - Rdiv2 then
          begin
            // Find new maximum
            AMinPos := APos;
            AMin := AMinTmp[AMinPos, y];
            for x1 := APos - 1 downto x - Rdiv2 do
            begin
              if AMinTmp[x1, y] < AMin then
              begin
                AMin := AMinTmp[x1, y];
                AMinPos := x1;
              end;
            end;
          end;
        end;
        MinMap[y, x] := AMin;
      end;
    end;

  finally
    AMaxTmp.Free;
    AMinTmp.Free;
  end;
end;

procedure Equalizemap(Source, MinMap, MaxMap, Dest: TsdByteMap; MinimumDelta: integer);
var
  x, y: integer;
  AMin, Delta: integer;
begin
  for y := 0 to Source.Height - 1 do
  begin
    for x := 0 to Source.Width - 1 do
    begin
      AMin := MinMap[x, y];
      Delta := Max(MinimumDelta, MaxMap[x, y] - AMin);
      Dest[x, y] := Max(0, Min(255, round((Source[x, y] - AMin) / Delta * 255)));
    end;
  end;
end;

procedure AbsDiffWithSurround(Map1, Map2, Dest: TsdByteMap; Rows, Cols: integer);
var
  x, y: integer;
  Max1, Max2: TsdByteMap;
begin
  Max1 := TsdByteMap.Create;
  Max2 := TsdByteMap.Create;
  try
    LocalMaximumMap(Map1, Max1, Rows, Cols);
    LocalMaximumMap(Map2, Max2, Rows, Cols);

    // Ensure size is adequate
    Dest.SetSize(Map1.Width, Map1.Height);

    // Find difference to maxima
    for y := 0 to Dest.Height  - 1 do
    begin
      for x := 0 to Dest.Width - 1 do
      begin
        // Compare Map1 pixel to Map2's maximum
        Dest[x, y] := Max(0,          Map1[x, y] - Max2[x, y]);

        // Compare Map2 pixel with Map1's maximum
        Dest[x, y] := Max(Dest[x, y], Map2[x, y] - Max1[x, y]);
      end;
    end;
  finally
    Max1.Free;
    Max2.Free;
  end;
end;

procedure CannyEdge(Source, Dest: TsdByteMap);
var
  x, y: integer;
  L, R, T, B: PByte;
begin
  Dest.SetSizeToMap(Source);

  // Bulk of points
  for y := 1 to Source.Height - 2 do
  begin
    T := PByte(Source.ScanLine[y - 1]); inc(T);
    L := PByte(Source.ScanLine[y]);
    R := L; inc(R, 2);
    B := PByte(Source.Scanline[y + 1]); inc(B);
    for x := 1 to Source.Width - 2 do
    begin
      Dest[x, y] := Max(abs(T^ - B^), abs(L^ - R^));
      inc(T); inc(L); inc(R); inc(B);
    end;
  end;

  // Top/Bottom edge
  y := Source.Height - 1;
  for x := 0 to Source.Width - 1 do
  begin
    Dest[x, 0] := abs(Source[x, 0] - Source[x, 1]);
    Dest[x, y] := abs(Source[x, y] - Source[x, y - 1]);
  end;

  // Left/Right edge
  x := Source.Width - 1;
  for y := 1 to Source.Height - 2 do
  begin
    Dest[0, y] := abs(Source[0, y] - Source[1, y]);
    Dest[x, y] := abs(Source[x, y] - Source[x - 1, y]);
  end;
end;

procedure FastRidge(Source, Dest: TsdByteMap; SpanX, SpanY: integer; Scale: double);
var
  x, y, i, SpanYd2: integer;
  S1, S2, S, D: Pbyte;
  Tmp: TsdByteMap;
begin
  Dest.SetSize(Source.Width, Source.Height);
  Dest.Clear(0);

  Tmp := TsdByteMap.Create;
  Tmp.SetSize(Source.Height, Source.Width);
  try
    // Horizontal
    for y := 0 to Source.Height - 1 do
    begin
      S1 := PByte(Source.ScanLine[y]);
      S2 := S1; inc(S2, SpanX);
      S := S1; inc(S, SpanX div 2);
      D := PByte(Dest.ScanLine[y]); inc(D, SpanX div 2);
      for x := 0 to Source.Width - 1 - SpanX do
      begin
        D^ := Max(0, S^ - max(S1^, S2^));
        inc(S1); inc(S2); inc(S); inc(D);
      end;
    end;

    // Transpose Source to Tmp map
    TransposeBytemap(Source, Tmp, Source.Width, Source.Height);

    // Vertical
    SpanYd2 := SpanY div 2;
    for y := 0 to Tmp.Height - 1 do
    begin
      S1 := PByte(Tmp.ScanLine[y]);
      S2 := S1; inc(S2, SpanY);
      S := S1; inc(S, SpanYd2);
      for x := SpanYd2 to Source.Width - 1 - SpanYd2 do
      begin
        Dest[y, x] := Max(Dest[y, x], S^ - max(S1^, S2^));
        inc(S1); inc(S2); inc(S);
      end;
    end;

    // Scale
    if Scale <> 1 then
    begin
      D := PByte(Dest.MapPointer);
      for i := 0 to Dest.ElementCount - 1 do
      begin
        D^ := Max(0, Min(255, round(D^ * Scale)));
        inc(D);
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

procedure DetectRidge(Source, Dest: TsdByteMap; Span, Flat, Threshold: integer);
var
  x, y, i, SpanYd2: integer;
  S11, S12, S13, S21, S22, S23, S31, S32, S33, D: Pbyte;
  Tmp: TsdByteMap;
  B, Bt: byte;
  Mt, Dt, Mean, Diff: integer;
begin
  Dest.SetSize(Source.Width, Source.Height);
  Dest.Clear(0);

  for y := Span to Source.Height - 1 - Span do
  begin
    Mean := 0;
    S11 := PByte(Source.ScanLine[y - Span]);
    S12 := S11;
    inc(S12, Span);
    S13 := S12;
    inc(S13, Span);
    S21 := PByte(Source.ScanLine[y]);
    S22 := S21;
    inc(S22, Span);
    S23 := S22;
    inc(S23, Span);
    S31 := PByte(Source.ScanLine[y + Span]);
    S32 := S31;
    inc(S32, Span);
    S33 := S32;
    inc(S33, Span);
    D := PByte(Dest.ScanLine[y]);
    inc(D, Span);

    for x := Span to Source.Width - 1 - Span do
    begin
      Diff := 0;
      if abs(S11^ - S33^) < Flat then
      begin
        Mean := (S11^ + S33^) div 2;
        Diff := abs(S22^ - Mean);
      end;
      if abs(S12^ - S32^) < Flat then
      begin
        Mt := (S12^ + S32^) div 2;
        Dt := abs(S22^ - Mt);
        if Dt > Diff then
        begin
          Mean := Mt;
          Diff := Dt;
        end;
      end;
      if abs(S13^ - S31^) < Flat then
      begin
        Mt := (S13^ + S31^) div 2;
        Dt := abs(S22^ - Mt);
        if Dt > Diff then
        begin
          Mean := Mt;
          Diff := Dt;
        end;
      end;
      if abs(S21^ - S23^) < Flat then
      begin
        Mt := (S21^ + S23^) div 2;
        Dt := abs(S22^ - Mt);
        if Dt > Diff then
        begin
          Mean := Mt;
          Diff := Dt;
        end;
      end;
      if Diff > Threshold then
      begin
        // Make sure to add at least a 1 to distinguish from the 0 where no
        // ridges are detected
        D^ := Max(1, Mean);
      end;

      // increment all pointers
      inc(S11);
      inc(S12);
      inc(S13);
      inc(S21);
      inc(S22);
      inc(S23);
      inc(S31);
      inc(S32);
      inc(S33);
      inc(D);
    end;
  end;

end;

procedure ScaleDown(Source, Dest: TsdByteMap; Factor: integer);
// Scale down the Source map by Factor and put result in Dest.
var
  x, y, r, c, Row, Den: integer;
  Totals: array of integer;
begin
  // Checks
  if not assigned(Source) or not assigned(Dest) then
    exit;
  if Factor <= 1 then
    exit;

  // Set size of Dest
  Dest.Width  := (Source.Width  div Factor);
  Dest.Height := (Source.Height div Factor);
  SetLength(Totals, Dest.Width);
  Den := sqr(Factor);

  // Loop through horz scanlines
  for y := 0 to Dest.Height - 1 do
  begin
    // Reset totals
    for x := 0 to Dest.Width - 1 do
    begin
      Totals[x] := 0;
    end;
    for r := 0 to Factor - 1 do
    begin
      Row := y * Factor + r;
      // Loop through values
      for x := 0 to Dest.Width - 1 do
      begin
        for c := 0 to Factor - 1 do
          inc(Totals[x], Source[x * Factor + c, Row]);
      end;
    end;
    // Find Dest values
    for x := 0 to Dest.Width - 1 do
    begin
      Dest[x, y] := round(Totals[x] / Den);
    end;
  end;
end;

procedure HistoMeanAndSigma(Histo: THisto; var Mean, Sigma: double);
// Determine mean and standard deviation of the histogram Histo
var
  i, Count: integer;
  SigX, SigX2: double;
begin
  Count := 0;
  SigX  := 0;
  SigX2 := 0;
  for i := 0 to 255 do
  begin
    // Number of values
    inc(Count, Histo[i]);
    // Sum of all values, in each bucket there are Histo[i] values
    SigX  := SigX  + Histo[i] * i;
    // Sum of squares
    SigX2 := SigX2 + int64(Histo[i]) * int64(i) * int64(i);
  end;
  Mean  := SigX / Count;
  Sigma := sqrt((SigX2 - sqr(SigX) / Count) / Count);
end;


procedure CombineMapsWithWeight(MapA, MapB, Dest: TsdByteMap; WeightA, WeightB: integer);
// Combine MapA and MapB and put result in Dest. Use weighting WeightA and WeightB to do
// so, in other words:
// Dest[i, j] := (MapA[i, j] * WeightA + MapB[i, j] * WeightB) / (WeightA + WeightB)
var
  i, Sum: integer;
begin
  // Checks
  if not assigned(MapA) or not assigned(MapB) or not assigned(Dest) or
    (MapA.Width <> MapB.Width) or (MapA.Height <> MapB.Height) or
    (WeightA < 1) or (WeightB < 1) then
  begin
    exit;
  end;

  // Set the size of Dest
  Dest.SetSize(MapA.Width, MapA.Height);

  Sum := WeightA + WeightB;
  for i := 0 to MapA.ElementCount - 1 do
    Dest.FMap[i] := round((MapA.FMap[i] * WeightA + MapB.FMap[i] * WeightB) / Sum);
end;

procedure RecursiveRectFilter(Src, Dst: TsdByteMap; Half, Length: integer);
// Do a recursive Rectangular Filter over a width of Length pixels, with the
// result value taken at Half. This filter assumes that the edge areas are empty
// so that all data can be processed in one long row.
var
  i: integer;
  P, Q, QEnd, R: PByte;
  Total: integer;
  ASize: integer;
begin
  ASize := Src.Width * Src.Height;
  if ASize <> Dst.Width * Dst.Height then
    exit;
  if ASize < Length then
    exit;

  // Initialization of Total over Half pixels
  Total := 0;
  Q := @Src.FMap[0];
  for i := 0 to Half - 1 do
  begin
    inc(Total, Q^);
    inc(Q);
  end;

  // Start setting values
  R := @Dst.FMap[0];
  for i := Half to Length - 1 do
  begin
    inc(Total, Q^);
    R^ := Total div Length;
    inc(Q);
    inc(R);
  end;

  // Now run through all the bits
  P := @Src.FMap[0];
  QEnd := @Src.FMap[ASize];
  while integer(Q) < integer(QEnd) do
  begin
    inc(Total, Q^ - P^);
    R^ := Total div Length;
    inc(P);
    inc(Q);
    inc(R);
  end;

  // And do the last itty bitty
  while integer(R) < integer(@Dst.FMap[ASize]) do
  begin
    inc(Total, -P^);
    R^ := Total div Length;
    inc(P);
    inc(R);
  end;
end;

procedure FloatAverageMap(Src, Dst: TsdSmallIntMap; WindowX, WindowY: integer;
  CircularX, CircularY: boolean);
var
  Tmp: TsdSmallIntMap;
  x, y, xi, yi, StartX, StartY, CloseX, CloseY, AverX, AverY: integer;

  // local
  function IndexX(x: integer): integer;
  begin
    if CircularX then
      Result := (x + Src.Width) mod Src.Width
    else
    begin
      Result := x;
      if x < 0 then
        Result := 0;
      if x >= Src.Width then
        Result := Src.Width - 1;
    end;
  end;

  // local
  function IndexY(y: integer): integer;
  begin
    if CircularY then
      Result := (y + Src.Height) mod Src.Height
    else
    begin
      Result := y;
      if y < 0 then
        Result := 0;
      if y >= Src.Height then
        Result := Src.Height - 1;
    end;
  end;
  
// main
begin
  if not assigned(Src) or not assigned(Dst) then
    exit;

  Tmp := TsdSmallIntMap.Create;
  try
    Tmp.SetSize(Src.Width, Src.Height);
    Dst.SetSize(Src.Width, Src.Height);
    StartX := -(WindowX div 2);
    CloseX := StartX + WindowX - 1;
    StartY := -(WindowY div 2);
    CloseY := StartY + WindowY - 1;

    // Do window X: from Src to Tmp
    for y := 0 to Src.Height - 1 do
    begin
      // Build initial average
      AverX := 0;
      for xi := StartX to CloseX do
        AverX := AverX + Src.Elements[IndexX(xi), y];
      for x := 0 to Src.Width - 1 do
      begin
        Tmp.Elements[x, y] := AverX div WindowX;
        // update average
        dec(AverX, Src.Elements[IndexX(x + StartX), y]);
        inc(AverX, Src.Elements[IndexX(x + CloseX + 1), y]);
      end;
    end;

    // Do window Y: from Tmp to Dst
    for x := 0 to Src.Width - 1 do
    begin
      // Build initial average
      AverY := 0;
      for yi := StartY to CloseY do
        AverY := AverY + Tmp.Elements[x, IndexY(yi)];
      for y := 0 to Src.Height - 1 do
      begin
        Dst.Elements[x, y] := AverY div WindowY;
        // update average
        dec(AverY, Tmp.Elements[x, IndexY(y + StartY)]);
        inc(AverY, Tmp.Elements[x, IndexY(y + CloseY + 1)]);
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

procedure TransposeByteMap(Src, Dst: TsdByteMap; Width, Height: integer);
// Do nothing else than put the transposed bits from Src into Dst. So we do NOT
// set the bitmap sizes, nor is any error checking done.
var
  r, c: integer;
  P, Q: PByte;
begin
  P := @Src.FMap[0];
  for r := 0 to Height - 1 do
  begin
    Q := @Dst.FMap[r];
    for c := 0 to Width - 1 do
    begin
      Q^ := P^;
      inc(Q, Height);
      inc(P);
    end;
  end;
end;

procedure Bitmap24ToRGBMaps(Bitmap: TBitmap; MapR, MapG, MapB: TsdByteMap);
var
  x, y: integer;
  AWidth, AHeight, APos: integer;
  AScan: PByte;
begin
  // Checks
  if not (assigned(Bitmap) and assigned(MapR) and assigned(MapG) and assigned(MapB)) then
    exit;

  AWidth  := Bitmap.Width;
  AHeight := Bitmap.Height;
  if AWidth * AHeight = 0 then
    exit;

  // Set map sizes
  MapR.SetSize(AWidth, AHeight);
  MapG.SetSize(AWidth, AHeight);
  MapB.SetSize(AWidth, AHeight);

  // Run through scanlines
  for y := 0 to AHeight - 1 do
  begin
    AScan := Bitmap.Scanline[y];
    APos := y * AWidth;
    for x := 0 to AWidth - 1 do
    begin
      MapB.MapPointer[APos] := AScan^; inc(AScan);
      MapG.MapPointer[APos] := AScan^; inc(AScan);
      MapR.MapPointer[APos] := AScan^; inc(AScan);
      inc(APos);
    end;
  end;

end;

procedure RGBMapsToBitmap24(MapR, MapG, MapB: TsdByteMap; Bitmap: TBitmap);
var
  x, y: integer;
  AWidth, AHeight, APos: integer;
  AScan: PByte;
begin
  // Checks
  if not (assigned(Bitmap) and assigned(MapR) and assigned(MapG) and assigned(MapB)) then
    exit;
  if not (MapG.SizeEquals(MapR) or MapB.SizeEquals(MapR)) then
    exit;

  AWidth  := MapR.Width;
  AHeight := MapR.Height;
  Bitmap.Width  := AWidth;
  Bitmap.Height := AHeight;

  // Run through scanlines
  for y := 0 to AHeight - 1 do
  begin
    AScan := Bitmap.Scanline[y];
    APos := y * AWidth;
    for x := 0 to AWidth - 1 do
    begin
      AScan^ := MapB.MapPointer[APos]; inc(AScan);
      AScan^ := MapG.MapPointer[APos]; inc(AScan);
      AScan^ := MapR.MapPointer[APos]; inc(AScan);
      inc(APos);
    end;
  end;

end;

procedure CopyValuesInMap(Map: TsdByteMap; OldVal, NewVal: byte);
var
  i: integer;
  P: PByte;
begin
  P := PByte(Map.MapPointer);
  for i := 0 to Map.ElementCount - 1 do
  begin
    if P^ = OldVal then
      P^ := NewVal;
    inc(P);
  end;
end;

function SegmentateMap256(Src, Dst: TsdByteMap; Threshold: byte): byte;
// Diagonal touching is not a touch!
const
  cNeighbourCount = 4;
  cNeighbours: array[0..cNeighbourCount - 1] of TPoint =
    ((X: -1; Y: 0), (X: 1; Y: 0), (X: 0; Y: -1), (X: 0; Y: 1));

  // local
  function CondenseMap: byte;
  var
    i: integer;
    Maps: array[0..255] of byte;
    P: PByte;
  begin
    // First pass: find maps in use
    FillChar(Maps, SizeOf(Maps), 0);
    P := PByte(Dst.MapPointer);
    for i := 0 to Dst.ElementCount - 1 do
    begin
      if P^ > 0 then Maps[P^] := P^;
      inc(P);
    end;

    // Find empty maps
    Result := 0;
    for i := 0 to 255 do
      if Maps[i] > 0 then
      begin
        inc(Result);
        Maps[i] := Result;
      end;

    // Second pass: change map numbers
    P := PByte(Dst.MapPointer);
    for i := 0 to Dst.ElementCount - 1 do
    begin
      if P^ > 0 then P^ := Maps[P^];
      inc(P);
    end;
  end;
//
var
  x, y, i: integer;
  Low, High, M: byte;
  Next: integer;
// main
begin
  Result := 0;
  if not assigned(Src) or not assigned(Dst) then
    exit;
  Dst.SetSize(Src.Width, Src.Height);
  Dst.Clear(0);
  Next := 0;

  for y := 0 to Src.Height - 1 do
  begin
    for x := 0 to Src.Width - 1 do
    begin
      // Above threshold?
      if Src[x, y] >= Threshold then
      begin
        Low := 255;
        High := 0;

        // Check neighbours in Dst, do they have a map number?
        for i := 0 to cNeighbourCount - 1 do
        begin
          M := Dst[x + cNeighbours[i].X, y + cNeighbours[i].Y];
          if M > 0 then
          begin
            Low  := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if Low < 255 then
        begin
          // There's a neighbour with a map number
          Dst[x, y] := Low;
          if High > Low then
          begin
            // Other neighbours with other numbers
            for i := 0 to cNeighbourCount - 1 do
            begin
              M := Dst[x + cNeighbours[i].X, y + cNeighbours[i].Y];
              if (M > 0) and (M <> Low) then
                CopyValuesInMap(Dst, M, Low);
            end;
          end;
        end else
        begin
          // No neighbour, add new map number
          inc(Next);
          if Next = 256 then
            Next := CondenseMap + 1;
          if Next = 256 then
            raise Exception.Create(sbmTooManySegments);
          Dst[x, y] := Next;
        end;
      end;
    end;
  end;
  Result := CondenseMap;
end;

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

function SegmentateMap16b(Src: TsdByteMap; Dst: TsdWordMap; Threshold: byte): integer;
// Diagonal touching is not a touch!
const
  cNeighbourCount = 4;
  cNeighbours: array[0..cNeighbourCount - 1] of TPoint =
    ((X: -1; Y: 0), (X: 1; Y: 0), (X: 0; Y: -1), (X: 0; Y: 1));
var
  Maps: array of word;
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
      if P^ > 0 then
        Maps[P^] := P^;
      inc(P);
    end;

    // Find empty maps
    Result := 0;
    for i := 1 to Count do
    begin
      if Maps[i] > 0 then
      begin
        inc(Result);
        Maps[i] := Result;
      end;
    end;

    // Second pass: change map numbers
    P := PWord(Dst.MapPointer);
    for i := 0 to Dst.ElementCount - 1 do
    begin
      if P^ > 0 then
        P^ := Maps[P^];
      inc(P);
    end;
  end;
//
var
  x, y, i: integer;
  Low, High, M: word;
  Next: integer;
// main
begin
  Result := 0;
  if not assigned(Src) or not assigned(Dst) then
    exit;
  Dst.SetSize(Src.Width, Src.Height);
  Dst.Clear(0);
  Next := 0;

  for y := 0 to Src.Height - 1 do
  begin
    for x := 0 to Src.Width - 1 do
    begin

      // Above threshold?
      if Src[x, y] >= Threshold then
      begin
        Low := 65535;
        High := 0;
        // Check neighbours in Dst, do they have a map number?
        for i := 0 to cNeighbourCount - 1 do
        begin
          M := Dst[x + cNeighbours[i].X, y + cNeighbours[i].Y];
          if M > 0 then
          begin
            Low  := Min(M, Low);
            High := Max(M, High);
          end;
        end;
        if Low < 65535 then
        begin
          // There's a neighbour with a map number
          Dst[x, y] := Low;
          if High > Low then
          begin
            // Other neighbours with other numbers
            for i := 0 to cNeighbourCount - 1 do
            begin
              M := Dst[x + cNeighbours[i].X, y + cNeighbours[i].Y];
              if (M > 0) and (M <> Low) then
                CopyValuesInMap16b(Dst, M, Low);
            end;
          end;
        end else
        begin
          // No neighbour, add new map number
          inc(Next);
          if Next = 65535 then
            Next := CondenseMap(Next) + 1;
          if Next = 65536 then
            raise Exception.Create(sbmTooManySegments);
          Dst[x, y] := Next;
        end;
      end;
    end;
  end;
  Result := CondenseMap(Next);
end;

function SegmentRectangle16b(Segments: TsdWordMap; Index: integer): TRect;
var
  x, y: integer;
begin
  Result.Left   := Segments.Width;
  Result.Top    := Segments.Height;
  Result.Right  := 0;
  Result.Bottom := 0;
  for y := 0 to Segments.Height - 1 do
    for x := 0 to Segments.Width - 1 do
      if Segments[x, y] = Index then
      begin
        Result.Left   := min(Result.Left,   x);
        Result.Top    := min(Result.Top,    y);
        Result.Right  := max(Result.Right,  x + 1);
        Result.Bottom := max(Result.Bottom, y + 1);
      end;
end;

end.
