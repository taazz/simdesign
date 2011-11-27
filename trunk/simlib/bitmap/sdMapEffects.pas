unit sdMapEffects;
{
  This unit contains the implementation of a number of filter effects that
  are specified in the SVG specification under Filters.

  This unit works with non-premultiplied colour channels, and uses a general
  map iterator approach to describe bitmaps. There is no dependence on
  the Graphics unit.

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.

}

interface

uses
  sdMapIterator;

type

  // Operators for the Composite filter effect
  TsdCompositeOperator = (
    coOver,
    coIn,
    coOut,
    coAtop,
    coXor,
    coArithmetic
  );

  // Light type
  TsdLightType = (
    ltInfinite,
    ltPointLight,
    ltSpotLight
  );

  // Information record for lightsources
  TsdLightSource = record
    Lx, Ly, Lz: double; // lightsource position
    LightType: TsdLightType;
    SpecularExponent: double; // spotlights only
    PointsAtX, PointAtX, PointsAtZ: double; // spotlights only
  end;

// Blur the Src map and put the result in Dst. This blur algorithm approximates
// Gaussian blur with a radius of RadiusX in X direction and RadiusY in Y direction.
// It uses a box-blur (a recursive algorithm) that is much faster than a true
// kernel-based gaussian blur. Dst may be the same map as Src.
procedure MapFakeGaussianBlur8bit(Src, Dst: TsdMapIterator; const RadiusX, RadiusY: double);

// Offset pixels in Src by DeltaX, DeltaY and put result in Dst. Src and Dst can
// be the same map. ChannelCount is the number of channels to move, and should
// be equal or less to each maps cellstride.
procedure MapOffset8bitInteger(Src, Dst: TsdMapIterator; ChannelCount, DeltaX, DeltaY: integer);

// Composites images ImgA and ImgB into image ImgD, using AOperator.
// If AOperator = coArithmetic, the values K1..K4 are used with formula
// result = k1*i1*i2 + k2*i1 + k3*i2 + k4.
// The alpha channels are defined by AlphaA, AlphaB, AlphaD. If an alpha channel
// input is nil, the alpha for that channel is assumed to be $FF.
// AChannelCount represents number of colour channels in the colour maps (usually 3).
// ImgD can be the same as ImgA or ImgB
procedure MapComposite8bit(ImgA, ImgB, ImgD, AlphaA, AlphaB, AlphaD: TsdMapIterator;
  AOperator: TsdCompositeOperator; AChannelCount: integer = 3;
  AK1: double = 0; AK2: double = 0; AK3: double = 0; AK4: double = 0);

// Creates gradient images GradX, GradY from Src. The gradient results are in
// fixed with a scale of 12 * 255 (So 1.0 = 12 * 255).
// Src should be 8bit values (one channel), GradX, GradY should
// be 16bit smallint values (one channel). The gradient method used is a 3x3
// Sobel gradient kernel, with specific kernels for edges and corners.
procedure MapSobelGradient8bitToSmallInt(Src, GradX, GradY: TsdMapIterator);

// Produces a resulting image in Dst according to the specification in SVG for
// diffuse lighting.
// Dst should contain a 24bit or 32bit bitmap. Alpha should contain
// the surface heights (8bit), GradX and GradY are the gradients produced
// by MapSobelGradient8bitToSmallInt.
procedure MapDiffuseLighting(Dst, Alpha, GradX, GradY: TsdMapIterator;
  ALightingColor: integer; const ALightSource: TsdLightSource;
  const ASurfaceScale: double = 1.0;
  const ADiffuseConstant: double = 1.0);

// Substract pixels in B from A (Dst = B - A), if resulting value is smaller than
// 0, it will be set to 0. Maps should have the same dimensions
procedure MapSubstract8Bit(SrcA, SrcB, Dst: TsdMapIterator);

implementation

function IntMin(const X1, X2: integer): integer;
begin
  if X1 < X2 then
    Result := X1
  else
    Result := X2;
end;

procedure MapFakeGaussianBlur8bit(Src, Dst: TsdMapIterator; const RadiusX, RadiusY: double);
type
  TBuffer = array of cardinal;
var
  x, y, W, H, Idx, Lx, Ly, Curb, SStride, DStride, BufLen: integer;
  L1, L2, L3, C1, C2, C3: integer;
  Divisor: cardinal;
  BufA, BufB: TBuffer;
  P: Pbyte;

  // local
  procedure SetupPasses(ALength: integer);
  begin
    if odd(ALength) then
    begin
      // Odd length: 3x this blur, with centerpixel of length
      L1 := ALength;
      C1 := ALength div 2;
      L2 := L1;
      C2 := C1;
      L3 := L1;
      C3 := C1;
    end else
    begin
      // Even length: 2x this blur center pixel left/right, then 1x blur on
      // length + 1
      L1 := ALength;
      C1 := ALength div 2 - 1;
      L2 := L1;
      C2 := C1 + 1;
      L3 := L1 + 1;
      C3 := C1 + 1;
    end;
    Divisor := L1 * L2 * L3;
  end;

  // local
  procedure BlurBuffer(ACenter, ALength: integer; const BSrc, BDst: TBuffer);
  var
    i, Idx, IdxL, IdxR: integer;
    Cum: cardinal; // cumulative
  begin
    IdxL := 0;
    IdxR := 0;
    Idx := ACenter;
    Cum := 0;

    // Start area
    for i := 0 to ALength - 1 do
    begin
      inc(Cum, BSrc[IdxR]);
      inc(IdxR);
    end;
    for i := 0 to Idx - 1 do
      BDst[i] := 0;

    // Bulk
    while IdxR < BufLen do
    begin
      BDst[Idx] := Cum;
      inc(Cum, BSrc[IdxR]);
      dec(Cum, BSrc[IdxL]);
      inc(Idx);
      inc(IdxL);
      inc(IdxR);
    end;

    // Close area
    while Idx < BufLen do
    begin
      BDst[Idx] := Cum;
      dec(Cum, BSrc[IdxL]);
      inc(Idx);
      inc(IdxL);
    end;
  end;
// main
begin
  // Minimum sizes
  W := IntMin(Src.Width, Dst.Width);
  H := IntMin(Src.Height, Dst.Height);

  // Approximating box width
  Lx := trunc(RadiusX * 3* sqrt(2 * pi) / 4 + 0.5);
  Ly := trunc(RadiusY * 3* sqrt(2 * pi) / 4 + 0.5);

  // Horizontal

  Curb := 3 * ((Lx + 1) div 2);
  BufLen := W + 2 * Curb;
  SetLength(BufA, BufLen);
  SetLength(BufB, BufLen);
  SStride := Src.CellStride;
  DStride := Dst.CellStride;
  SetupPasses(Lx);
  for y := 0 to H - 1 do
  begin
    FillChar(BufA[0], BufLen * SizeOf(cardinal), 0);
    // Put data in buffer
    P := Src.At(0, y);
    Idx := Curb;
    for x := 0 to W - 1 do
    begin
      BufA[Idx] := P^;
      inc(Idx);
      inc(P, SStride);
    end;

    // Now 3x the blur
    BlurBuffer(C1, L1, BufA, BufB);
    BlurBuffer(C2, L2, BufB, BufA);
    BlurBuffer(C3, L3, BufA, BufB);

    // Put buffer in data
    Idx := Curb;
    P := Dst.At(0, y);
    for x := 0 to W - 1 do
    begin
      P^ := BufB[Idx] div Divisor;
      inc(Idx);
      inc(P, DStride);
    end;
  end;

  // Vertical

  Curb := 3 * ((Ly + 1) div 2);
  BufLen := H + 2 * Curb;
  SetLength(BufA, BufLen);
  SetLength(BufB, BufLen);
  DStride := Dst.ScanStride;
  SetupPasses(Ly);
  for x := 0 to W - 1 do
  begin
    FillChar(BufA[0], BufLen * SizeOf(cardinal), 0);
    // Put data in buffer
    P := Dst.At(x, 0);
    Idx := Curb;
    for y := 0 to H - 1 do
    begin
      BufA[Idx] := P^;
      inc(Idx);
      inc(P, DStride);
    end;

    // Now 3x the blur
    BlurBuffer(C1, L1, BufA, BufB);
    BlurBuffer(C2, L2, BufB, BufA);
    BlurBuffer(C3, L3, BufA, BufB);

    // Put buffer in data
    Idx := Curb;
    P := Dst.At(x, 0);
    for y := 0 to H - 1 do
    begin
      P^ := BufB[Idx] div Divisor;
      inc(Idx);
      inc(P, DStride);
    end;
  end;
end;

procedure MapOffset8bitInteger(Src, Dst: TsdMapIterator; ChannelCount, DeltaX, DeltaY: integer);
var
  y, ys, W, H, SStride, DStride: integer;
  Ps, Pd: Pbyte;
  Direct: boolean;

  // local
  procedure ZeroY(XStart, XClose: integer);
  var
    x: integer;
  begin
    Pd := Dst.At(XStart, y);
    if Direct then
      FillChar(Pd^, (XClose - XStart) * ChannelCount, 0)
    else
      for x := 0 to XClose - XStart - 1 do
      begin
        FillChar(Pd^, ChannelCount, 0);
        inc(Pd, DStride);
      end;
  end;

  // local
  procedure DoInnerLoop;
  var
    x, WBrack: integer;
  begin
    ys := y - DeltaY;
    if (ys < 0) or (ys >= H) then
    begin
      ZeroY(0, W);
      exit;
    end;
    if DeltaX > 0 then
    begin
      Ps := Src.At(0, ys);
      Pd := Dst.At(DeltaX, y);
    end else
    begin
      Ps := Src.At(-DeltaX, ys);
      Pd := Dst.At(0, y);
    end;
    WBrack := W - abs(DeltaX);
    if Direct then
      Move(Ps^, Pd^, ChannelCount * WBrack)
    else
    begin
      if DeltaX < 0 then
      begin
        for x := 0 to WBrack - 1 do
        begin
          Move(Ps^, Pd^, ChannelCount);
          inc(Ps, SStride);
          inc(Pd, DStride);
        end;
      end else
      begin
        inc(Ps, WBrack * SStride);
        inc(Pd, WBrack * DStride);
        for x := WBrack - 1 downto 0 do
        begin
          dec(Ps, SStride);
          dec(Pd, DStride);
          Move(Ps^, Pd^, ChannelCount);
        end;
      end;
    end;
    if DeltaX > 0 then
    begin
      ZeroY(0, DeltaX);
    end else
    begin
      ZeroY(WBrack, W);
    end;
  end;
begin
  W := IntMin(Src.Width, Dst.Width);
  H := IntMin(Src.Height, Dst.Height);

  // Source and dest strides
  SStride := Src.CellStride;
  DStride := Dst.CellStride;

  // Can we use a direct move on the rows?
  Direct := (ChannelCount = SStride) and (ChannelCount = Dstride);
  if DeltaY < 0 then
  begin
    for y := 0 to H - 1 do
      DoInnerLoop;
  end else
  begin
    for y := H - 1 downto 0 do
      DoInnerLoop;
  end;
end;

procedure MapComposite8bit(ImgA, ImgB, ImgD, AlphaA, AlphaB, AlphaD: TsdMapIterator;
  AOperator: TsdCompositeOperator; AChannelCount: integer = 3;
  AK1: double = 0; AK2: double = 0; AK3: double = 0; AK4: double = 0);
// arithmetic result = k1*i1*i2 + k2*i1 + k3*i2 + k4.
// The compositing works with non-premultiplied inputs and outputs
const
  cFull: byte = $FF;
var
  x, y, i, W, H: integer;
  SImgA, SImgB, SImgD, SAlphaA, SAlphaB, SAlphaD: integer;
  DImgA, DImgB, DImgD: integer;
  PImgA, PImgB, PImgD, PAlphaA, PAlphaB, PAlphaD: Pbyte;
  V, A, B, {Fa, Fb,} AFa, BFb, Ra, Cp, A1, A2, A1A2, C1, C2, C1C2: integer;
  K1, K2, K3, K4_255: integer;

  // Local: return the map location at Y, or a reference to a fixed value (in that case,
  // Stride = 0)
  procedure MapYOrFixed(AMap: TsdMapIterator; Y: integer; out PValue: PByte; out Stride: integer);
  begin
    if not assigned(AMap) then
    begin
      PValue := @cFull;
      Stride := 0;
    end else
    begin
      PValue := AMap.At(0, Y);
      Stride := AMap.CellStride;
    end;
  end;

  // Local: divide V by 255 in fast but correct way using shifts, with shortcuts
  // for 0 and 255*255
  function Div255(V: integer): integer;
  var
    t: integer;
  begin
    if V = 0 then
      Result := 0
    else
      if V = 255 * 255 then
        Result := 255
      else
      begin
        t := V + $80;
        Result := (t shr 8 + t) shr 8;
      end;
  end;

  // Local: Clamp value between 0 and 255*255
  function Clamp(V: integer): integer;
  begin
    if V < 0 then
      Result := 0
    else
      if V > 255 * 255 then
        Result := 255 * 255
      else
        Result := V;
  end;
// main
begin
  W := IntMin(IntMin(ImgA.Width, ImgB.Width), ImgD.Width);
  H := IntMin(IntMin(ImgA.Height, ImgB.Height), ImgD.Height);
  K1 := round(AK1 * 255);
  K2 := round(AK2 * 255);
  K3 := round(AK3 * 255);
  K4_255 := round(AK4 * 255 * 255);
  for y := 0 to H - 1 do
  begin
    MapYOrFixed(ImgA, y, PImgA, SImgA);
    MapYOrFixed(ImgB, y, PImgB, SImgB);
    MapYOrFixed(ImgD, y, PImgD, SImgD);
    DImgA := SImgA - AChannelCount;
    DImgB := SImgB - AChannelCount;
    DImgD := SImgD - AChannelCount;
    MapYOrFixed(AlphaA, y, PAlphaA, SAlphaA);
    MapYOrFixed(AlphaB, y, PAlphaB, SAlphaB);
    MapYOrFixed(AlphaD, y, PAlphaD, SAlphaD);
    if AOperator = coArithmetic then
    begin
      // Arithmetic: component-wise
      for x := 0 to W - 1 do
      begin
        A1 := PAlphaA^;
        A2 := PAlphaB^;
        A1A2 := Div255(A1 * A2);
        PAlphaD^ := Div255(Clamp(K1 * A1A2 + K2 * A1 + K3 * A2 + K4_255));
        for i := 0 to AChannelCount - 1 do
        begin
          C1 := PImgA^;
          C2 := PImgB^;
          C1C2 := Div255(C1 * C2);
          PImgD^ := Div255(Clamp(K1 * C1C2 + K2 * C1 + K3 * C2 + K4_255));
          inc(PImgA);
          inc(PImgB);
          inc(PImgD);
        end;
        inc(PImgA, DImgA);
        inc(PImgB, DImgB);
        inc(PImgD, DImgD);
        inc(PAlphaA, SAlphaA);
        inc(PAlphaB, SAlphaB);
        inc(PAlphaD, SAlphaD);
      end;
    end else
    begin
      // non-arithmetic
      for x := 0 to W - 1 do
      begin
        // Alpha calculation: See Porter/Duff. These are alphas of A and B map
        A := PAlphaA^;
        B := PAlphaB^;
        // For each operator, we will directly determine the A*Fa and B*Fb products.
        case AOperator of
        coOver:
          begin
            AFa := A;                     //Fa := 255;
            BFb := Div255(B * (255 - A)); //Fb := 255 - A;
          end;
        coIn:
          begin
            AFa := Div255(A * B);         //Fa := B;
            BFb := 0;                     //Fb := 0;
          end;
        coOut:
          begin
            AFa := Div255(A * (255 - B)); //Fa := 255 - B;
            BFb := 0;                     //Fb := 0;
          end;
        coAtop:
          begin
            AFa := Div255(A * B);         //Fa := B;
            BFb := Div255(B * (255 - A)); //Fb := 255 - A;
          end;
        coXor:
          begin
            AFa := Div255(A * (255 - B)); //Fa := 255 - B;
            BFb := Div255(B * (255 - A)); //Fb := 255 - A;
          end;
        else
          // avoid warnings
          AFa := 0;
          BFb := 0;
        end;//case

        // Resulting alpha
        Ra := AFa + BFb;
        PAlphaD^ := Ra;

        // Process color channels
        for i := 0 to AChannelCount - 1 do
        begin
          if Ra = 0 then
          begin
            PImgD^ := 0;
          end else
          begin
            // Premultiplied channel value (* 255)
            Cp := AFa * PImgA^ + BFb * PImgB^;
            // Original channel value
            if Ra = 255 then
              PImgD^ := Div255(Cp)
            else
              PImgD^ := Cp div Ra;
          end;
          inc(PImgA);
          inc(PImgB);
          inc(PImgD);
        end;

        // Increment pointers
        inc(PImgA, DImgA);
        inc(PImgB, DImgB);
        inc(PImgD, DImgD);
        inc(PAlphaA, SAlphaA);
        inc(PAlphaB, SAlphaB);
        inc(PAlphaD, SAlphaD);
      end;
    end;
  end;
end;

procedure MapSobelGradient8bitToSmallInt(Src, GradX, GradY: TsdMapIterator);
var
  x, y, W, H, SStride, GStride: integer;
  Ps: Pbyte;
  Pg: Psmallint;
  G0, G1, G2: smallint;

  // local: gradient function, turning -1/+1 duo's into gradient value
  procedure GradientFunc(APos, ACount: integer);
  begin
    if APos = 0 then
    begin
      G1 := Ps^;
      inc(Ps, SStride);
      G2 := Ps^;
      Pg^ := 2 * (G2 - G1);
    end else
      if APos = ACount - 1 then
      begin
        Pg^ := 2 * (G2 - G1);
      end else
      begin
        inc(Ps, SStride);
        G0 := G1;
        G1 := G2;
        G2 := Ps^;
        Pg^ := G2 - G0;
      end;
    inc(Pg, GStride);
  end;

  // local: average multiple gradient function lines (weighted 1-2-1)
  procedure AverageFunc(APos, ACount: integer);
  var
    PgOld: Psmallint;
  begin
    if APos = 0 then
    begin
      G1 := Pg^;
      PgOld := Pg;
      inc(Pg, GStride);
      G2 := Pg^;
      PgOld^ := -4 * (G2 + 2 * G1);
    end else
      if APos = ACount - 1 then
      begin
        Pg^ := -4 * (2 * G2 + G1);
      end else
      begin
        PgOld := Pg;
        G0 := G1;
        G1 := G2;
        inc(Pg, GStride);
        G2 := Pg^;
        PgOld^ := -3 * (G2 + 2 * G1 + G0);
      end;
  end;
// main
begin
  W := IntMin(Src.Width, IntMin(GradX.Width, GradY.Width));
  H := IntMin(Src.Height, IntMin(GradX.Height, GradY.Height));
  if (W < 2) or (H < 2) then
    exit;

  // Gradient X
  SStride := Src.CellStride;
  GStride := GradX.CellStride div 2;
  for y := 0 to H - 1 do
  begin
    Ps := Src.At(0, y);
    Pg := Psmallint(GradX.At(0, y));
    for x := 0 to W - 1 do
      GradientFunc(x, W);
  end;
  GStride := GradX.ScanStride div 2;
  for x := 0 to W - 1 do
  begin
    Pg := Psmallint(GradX.At(x, 0));
    for y := 0 to H - 1 do
      AverageFunc(y, H);
  end;

  // Gradient Y
  SStride := Src.ScanStride;
  GStride := GradY.ScanStride div 2;
  for x := 0 to W - 1 do
  begin
    Ps := Src.At(x, 0);
    Pg := Psmallint(GradY.At(x, 0));
    for y := 0 to H - 1 do
      GradientFunc(y, H);
  end;
  GStride := GradY.CellStride div 2;
  for y := 0 to H - 1 do
  begin
    Pg := Psmallint(GradY.At(0, y));
    for x := 0 to W - 1 do
      AverageFunc(x, W);
  end;
end;

procedure MapDiffuseLighting(Dst, Alpha, GradX, GradY: TsdMapIterator;
  ALightingColor: integer; const ALightSource: TsdLightSource;
  const ASurfaceScale: double = 1.0;
  const ADiffuseConstant: double = 1.0);
var
  i, x, y, W, H, ChannelCount: integer;
  Lc: array[0..2] of byte;
  S, D, Pa: Pbyte;
  Px: Psmallint;
  Py: Psmallint;
  SmGrad, SmAlpha, Nx, Ny, Nz, Lx, Ly, Lz, Lxp, Lyp, Lzp, LMul, NL, Kd, z: single;
  Infinite, HasAlpha: boolean;

  // local: clamp between 0 and 255
  function Clamp(V: integer): integer;
  begin
    if V < 0 then
      Result := 0
    else
      if V > 255 then
        Result := 255
      else
        Result := V;
  end;
// main
begin
  W := Dst.Width;
  H := Dst.Height;
  SmGrad := ASurfaceScale / (12 * 255);
  SmAlpha := ASurfaceScale / 255;
  Kd := ADiffuseConstant;
  ChannelCount := IntMin(3, Dst.CellStride);
  Hasalpha := Dst.CellStride >= 4;

  // Light colour components
  Lc[0] := (ALightingColor shr 16) and $FF;
  Lc[1] := (ALightingColor shr 8) and $FF;
  Lc[2] := ALightingColor and $FF;

  Lxp := ALightSource.Lx;
  Lyp := ALightSource.Ly;
  Lzp := ALightSource.Lz;
  Infinite := ALightSource.LightType = ltInfinite;
  if Infinite then
  begin
    // calculate lightsource direction
    LMul := 1 / sqrt(sqr(Lxp) + sqr(Lyp) + sqr(Lzp)); // 1/length
    Lx := Lxp * LMul;
    Ly := Lyp * LMul;
    Lz := Lzp * LMul;
  end else
  begin
    // avoid warning
    Lx := 0;
    Ly := 0;
    Lz := 0;
  end;

  D := Dst.First;
  Pa := Alpha.First;
  Px := Psmallint(GradX.First);
  Py := Psmallint(GradY.First);
  for y := 0 to H - 1 do
  begin
    for x := 0 to W - 1 do
    begin
      // Surface normal
      if (Px^ = 0) and (Py^ = 0) then
      begin
        Nx := 0;
        Ny := 0;
        Nz := 1;
      end else
      begin
        Nx := Px^ * SmGrad;
        Ny := Py^ * SmGrad;
        Nz := 1 / sqrt(sqr(Nx) + sqr(Ny) + 1); // 1/length
        Nx := Nx * Nz;
        Ny := Ny * Nz;
      end;
      if not Infinite then
      begin
        // calculate lightsource direction in case point or spot light
        z := Pa^ * SmAlpha;
        Lx := Lxp - x;
        Ly := Lyp - y;
        Lz := Lzp - z;
        LMul := 1 / sqrt(sqr(Lx) + sqr(Ly) + sqr(Lz));
        Lx := Lx * LMul;
        Ly := Ly * LMul;
        Lz := Lz * LMul;
      end;

      // dotproduct of N * L
      NL := (Nx * Lx + Ny * Ly + Nz * Lz);

      // for each channel..
      for i := 0 to ChannelCount - 1 do
      begin
        D^ := Clamp(round(Kd * NL * Lc[i]));
        inc(D);
      end;

      // Add opaque alpha channel
      if HasAlpha then
        D^ := 255;

      // Next
      D := Dst.Next;
      Pa := Alpha.Next;
      Px := PsmallInt(GradX.Next);
      Py := PsmallInt(GradY.Next);
    end;
  end;
end;

procedure MapSubstract8Bit(SrcA, SrcB, Dst: TsdMapIterator);
var
  A, B, D: PByte;
  Res: integer;
begin
  if SrcA = SrcB then
    exit;

  A := SrcA.First;
  B := SrcB.First;
  D := Dst.First;

  while assigned(A) and assigned(B) and assigned(D) do
  begin
    Res := B^ - A^;
    if Res < 0 then
      Res := 0;
    D^ := Res;
    A := SrcA.Next;
    B := SrcB.Next;
    if Dst = SrcA then
      D := A
    else if Dst = SrcB then
      D := B
    else
      D := Dst.Next;
  end;
end;

end.
