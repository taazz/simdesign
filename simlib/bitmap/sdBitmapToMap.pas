unit sdBitmapToMap;

interface

uses
  Graphics, sdByteMap, sdMapIterator;

type

  TsdGrayscaleConversion  = (
    gcUniform,   // Convert bits to grayscale as (R+G+B) / 3
    gcWeighted,  // Use weighted formula fitting with eye sensitivity
    gcRed,       // Use the red color of the bits
    gcGreen,     // Use the green color of the bits
    gcBlue       // Use the blue color of the bits
  );

procedure GetByteMapIterator(AMap: TsdByteMap; AIterator: TsdMapIterator);

procedure Bitmap24ToRGBMaps(Bitmap: TBitmap; MapR, MapG, MapB: TsdByteMap);
procedure RGBMapsToBitmap24(MapR, MapG, MapB: TsdByteMap; Bitmap: TBitmap);

implementation

procedure GetByteMapIterator(AMap: TsdByteMap; AIterator: TsdMapIterator);
begin
  AIterator.Width := AMap.Width;
  AIterator.Height := AMap.Height;
  AIterator.Map := pbyte(AMap.MapPointer);
  AIterator.ScanStride := AMap.Width;
  AIterator.CellStride := 1;
end;

procedure Bitmap24ToRGBMaps(Bitmap: TBitmap; MapR, MapG, MapB: TsdByteMap);
var
  x, y: integer;
  AWidth, AHeight, APos: integer;
  AScan: PByte;
begin
  // Checks
  if not (assigned(Bitmap) and assigned(MapR) and assigned(MapG) and assigned(MapB)) then exit;
  AWidth  := Bitmap.Width;
  AHeight := Bitmap.Height;
  if AWidth * AHeight = 0 then exit;
  // Set map sizes
  MapR.SetSize(AWidth, AHeight);
  MapG.SetSize(AWidth, AHeight);
  MapB.SetSize(AWidth, AHeight);
  // Run through scanlines
  for y := 0 to AHeight - 1 do begin
    AScan := Bitmap.Scanline[y];
    APos := y * AWidth;
    for x := 0 to AWidth - 1 do begin
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
  if not (assigned(Bitmap) and assigned(MapR) and assigned(MapG) and assigned(MapB)) then exit;
  if not (MapG.SizeEquals(MapR) or MapB.SizeEquals(MapR)) then exit;
  AWidth  := MapR.Width;
  AHeight := MapR.Height;
  Bitmap.Width  := AWidth;
  Bitmap.Height := AHeight;
  // Run through scanlines
  for y := 0 to AHeight - 1 do begin
    AScan := Bitmap.Scanline[y];
    APos := y * AWidth;
    for x := 0 to AWidth - 1 do begin
      AScan^ := MapB.MapPointer[APos]; inc(AScan);
      AScan^ := MapG.MapPointer[APos]; inc(AScan);
      AScan^ := MapR.MapPointer[APos]; inc(AScan);
      inc(APos);
    end;
  end;
end;

end.
