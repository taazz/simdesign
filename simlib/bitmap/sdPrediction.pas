{ unit sdPrediction

  Methods for map prediction

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.
}
unit sdPrediction;

interface

uses
  sdMapIterator, Math;

// Predict map Src and put results in Dst, using the Paeth predictor. Note that
// Src and Dst may be the same map. If not, they should have the same width and
// height.
procedure PredictMapPaethSmallInt(Src, Dst: TsdMapIterator);
procedure PredictMapPaethShortInt(Src, Dst: TsdMapIterator);

// Determines the theoretical entropy of elements in AMap
function EntropyShortInt(AMap: TsdMapIterator): double;

// Determine Xmin/Ymin/Xmax/Ymax (not inclusive last 2) of AMap,
// where AMap's value > Limit
function BoundingBox8bit(AMap: TsdMapIterator; ALimit: byte;
  out Xmin, Xmax, Ymin, Ymax: integer): boolean;

implementation

function PaethPredictor(a, b, c: integer): integer;
var
  pa, pb, pc: Integer;
begin
  // a = left, b = above, c = upper left
  pa := abs(b - c);      // distances to a, b, c
  pb := abs(a - c);
  pc := abs(a + b - c * 2);

  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

procedure PredictMapPaethSmallInt(Src, Dst: TsdMapIterator);
var
  x, y: integer;
  SScanS, DScanS, SCellS, DCellS, Stride: integer;
  PA, PB, PC, PD: Psmallint;
begin
  // Scan and cell strides (we divide by 2, since we will be working with smallint pointers)
  SScanS := Src.ScanStride div 2;
  DScanS := Dst.ScanStride div 2;
  SCellS := Src.CellStride div 2;
  DCellS := Dst.CellStride div 2;
  // Position on last element
  PA := Psmallint(Src.Map);
  inc(PA, SScanS * (Src.Height - 1) + SCellS * (Src.Width - 2));
  PC := PA;
  dec(PC, SScanS);
  PB := PC;
  inc(PB, SCellS);
  PD := Psmallint(Dst.Map);
  inc(PD, DScanS * (Src.Height - 1) + DCellS * (Src.Width - 1));
  for y := Src.Height - 1 downto 0 do
  begin
    if y = 0 then
    begin
      // No prediction for 0th element of map, otherwise: horizontal predict
      for x := Src.Width - 1 downto 1 do
      begin
        PD^ := PD^ - PA^;
        dec(PA, SCellS);
        dec(PD, DCellS);
      end;
    end else
    begin
      for x := Src.Width - 1 downto 0 do
      begin
        if x = 0 then
        begin
          // Use just the top one as predictor
          PD^ := PD^ - PB^;
        end else
        begin
          PD^ := PD^ - PaethPredictor(PA^, PB^, PC^);
        end;
        dec(PA, SCellS);
        dec(PB, SCellS);
        dec(PC, SCellS);
        dec(PD, DCellS);
      end;
    end;
    // Move back to end of line, and then one line down
    Stride := SCellS * Src.Width - SScanS;
    inc(PA, Stride);
    inc(PB, Stride);
    inc(PC, Stride);
    inc(PD, DCellS * Src.Width - DScanS);
  end;
end;

procedure PredictMapPaethShortInt(Src, Dst: TsdMapIterator);
var
  x, y: integer;
  SScanS, DScanS, SCellS, DCellS, Stride: integer;
  PA, PB, PC, PD: Pshortint;
begin
  // Scan and cell strides
  SScanS := Src.ScanStride;
  DScanS := Dst.ScanStride;
  SCellS := Src.CellStride;
  DCellS := Dst.CellStride;
  // Position on last element
  PA := Pshortint(Src.Map);
  inc(PA, SScanS * (Src.Height - 1) + SCellS * (Src.Width - 2));
  PC := PA;
  dec(PC, SScanS);
  PB := PC;
  inc(PB, SCellS);
  PD := Pshortint(Dst.Map);
  inc(PD, DScanS * (Src.Height - 1) + DCellS * (Src.Width - 1));
  for y := Src.Height - 1 downto 0 do
  begin
    if y = 0 then
    begin
      // No prediction for 0th element of map, otherwise: horizontal predict
      for x := Src.Width - 1 downto 1 do
      begin
        PD^ := PD^ - PA^;
        dec(PA, SCellS);
        dec(PD, DCellS);
      end;
    end else
    begin
      for x := Src.Width - 1 downto 0 do
      begin
        if x = 0 then
        begin
          // Use just the top one as predictor
          PD^ := PD^ - PB^;
        end else
        begin
          PD^ := PD^ - PaethPredictor(PA^, PB^, PC^);
        end;
        dec(PA, SCellS);
        dec(PB, SCellS);
        dec(PC, SCellS);
        dec(PD, DCellS);
      end;
    end;
    // Move back to end of line, and then one line down
    Stride := SCellS * Src.Width - SScanS;
    inc(PA, Stride);
    inc(PB, Stride);
    inc(PC, Stride);
    inc(PD, DCellS * Src.Width - DScanS);
  end;
end;

function EntropyShortInt(AMap: TsdMapIterator): double;
// Determines the theoretical entropy of elements in AMap
var
  i, Count: integer;
  P: Pshortint;
  Histo: array[shortint] of integer;
begin
  P := Pshortint(AMap.First);
  Count := 0;
  // Histogram the data
  FillChar(Histo[-128], SizeOf(Histo), 0);
  while assigned(P) do
  begin
    inc(Count);
    inc(Histo[P^]);
    P := Pshortint(AMap.Next);
  end;
  // Theoretical entropy
  Result := 0;
  for i := -128 to 127 do
    if Histo[i] > 0 then
      if Histo[i] = Count then
      begin
        Result := 0;
        exit;
      end else
        Result := Result + Histo[i] * log2(Count / Histo[i]);
end;

function BoundingBox8bit(AMap: TsdMapIterator; ALimit: byte;
  out Xmin, Xmax, Ymin, Ymax: integer): boolean;
// Can be made a bit faster by skipping over the inner loop once
// a result has been achieved.
var
  x, y: integer;
  P: Pbyte;
begin
  Xmin := 0;
  Ymin := 0;
  Xmax := 0;
  Ymax := 0;
  // Detect rectangle
  P := AMap.First;
  Result := False;
  while assigned(P) do
  begin
    for y := 0 to AMap.Height - 1 do
      for x := 0 to AMap.Width - 1 do
      begin
        if P^ > ALimit then
        begin
          if not Result then
          begin
            XMin := x;
            XMax := x + 1;
            YMin := y;
            YMax := y + 1;
            Result := True;
          end else
          begin
            Xmin := Min(x, Xmin);
            Xmax := Max(x + 1, Xmax);
            Ymin := Min(y, Ymin);
            Ymax := Max(y + 1, Ymax);
          end;
        end;
        P := AMap.Next;
      end;
  end;
end;

end.
