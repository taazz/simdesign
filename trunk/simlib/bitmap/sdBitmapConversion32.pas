{  unit sdBitmapConversion32

  Conversion routines for GR32 bitmaps (TBitmap32 class).

  Author: Nils Haeck M.Sc.
  Copyright (c) 2011 Simdesign B.V.

  This software may ONLY be used or replicated in accordance with
  the LICENSE found in this source distribution.

  Please visit http://www.simdesign.nl for more information.
}
unit sdBitmapConversion32;

{$ifdef fpc}{$mode delphi}{$endif fpc}

interface

uses
  SysUtils, GR32, sdMapIterator;

type

  TsdGrayscaleConversion  = (
    gcUniform,   // Convert bits to grayscale as (R+G+B) / 3
    gcWeighted,  // Use weighted formula fitting with eye sensitivity
    gcRed,       // Use the red color of the bits
    gcGreen,     // Use the green color of the bits
    gcBlue       // Use the blue color of the bits
  );

  T32bitPaletteEntry = packed record
    case integer of
    0: (R, G, B, A: byte);
    1: (Color: longint);
  end;
  T8bitPaletteArray = array[0..255] of T32bitPaletteEntry;

// construct a TsdMapIterator object with the info from ABitmap32.
procedure GetBitmapIterator(ABitmap32: TBitmap32; AIterator: TsdMapIterator);

// create a TBitmap based on the information in AIterator
function SetBitmapFromIterator(const AIterator: TsdMapIterator): TBitmap32;

// Perform the operation in AOperation on Src, with the result in Dst. Both
// Src and Dst must be assigned!
procedure BitmapOperation(Src, Dst: TBitmap32; AOperation: TsdMapOperation);

// Interpolate colors Col1 and Col2 using a fraction in range [0..1]
function InterpolateColor(Col1, Col2: TColor32; Frac: single): TColor32;

// cross-platform scanline getter for TBitmap32
function GetBitmapScanline(ABitmap32: TBitmap32; Y: integer): pointer;

resourcestring

  sInvalidPixelFormat = 'Invalid pixelformat';
  sIncompatiblePixelFormats = 'incompatible pixel formats';

implementation

function GetBitmapScanline(ABitmap32: TBitmap32; Y: integer): pointer;
begin
  Result := ABitmap32.ScanLine[Y];
end;

procedure GetBitmapIterator(ABitmap32: TBitmap32; AIterator: TsdMapIterator);
begin
  AIterator.Width := ABitmap32.Width;
  AIterator.Height := ABitmap32.Height;
  if ABitmap32.Width * ABitmap32.Height = 0 then
    exit;

  AIterator.Map := GetBitmapScanline(ABitmap32, 0);
  if AIterator.Height > 1 then
    AIterator.ScanStride := integer(GetBitmapScanline(ABitmap32, 1)) - integer(GetBitmapScanline(ABitmap32, 0))
  else
    AIterator.ScanStride := 0;

  AIterator.CellStride := 4;
  AIterator.BitCount := 32;
end;

function SetBitmapFromIterator(const AIterator: TsdMapIterator): TBitmap32;
begin
  Result := TBitmap32.Create;
  Result.Width := AIterator.Width;
  Result.Height := AIterator.Height;
  if AIterator.CellStride <> 4 then
    raise Exception.CreateFmt('incorrect cellstride %d', [AIterator.CellStride]);
end;

procedure ToGrayscale3ChTo1Ch(SIter, DIter: TsdMapIterator; AMethod: TsdGrayscaleConversion);
var
  S, D: pbyte;
  Val: integer;
begin
  if (SIter.CellStride < 3) or (DIter.CellStride < 1) then
    raise Exception.Create('Invalid cellstride');

  // RGB are layed out in memory as BGR
  case AMethod of
  gcRed:   SIter.IncrementMap(2);
  gcGreen: SIter.IncrementMap(1);
  gcBlue:  SIter.IncrementMap(0);
  end;//case
  S := SIter.First;
  D := DIter.First;
  case AMethod of

  gcUniform:
    while assigned(S) do
    begin
      Val := S^;
      inc(S); inc(Val, S^);
      inc(S); inc(Val, S^);
      D^ := Val div 3;
      S := SIter.Next;
      D := DIter.Next;
    end;

  gcWeighted:
    // (R * 61 + G * 174 + B * 21) / 256
    while assigned(S) do
    begin
      Val := S^ * 21;
      inc(S); inc(Val, S^ * 174);
      inc(S); inc(Val, S^ * 61);
      D^ := Val shr 8;
      S := SIter.Next;
      D := DIter.Next;
    end;

  gcRed, gcGreen, gcBlue:
    while assigned(S) do
    begin
      D^ := S^;
      S := SIter.Next;
      D := DIter.Next;
    end;
  end;
end;

procedure BitmapOperation(Src, Dst: TBitmap32; AOperation: TsdMapOperation);
var
  SI: TsdMapIterator;
  DI: TsdMapIterator;
begin
  case AOperation of
  moRotate90, moRotate270, moTranspose:
    begin
      Dst.Width := Src.Height;
      Dst.Height := Src.Width;
    end;
  moRotate180, moMirror, moFlip:
    begin
      Dst.Width := Src.Width;
      Dst.Height := Src.Height;
    end;
  end;

  // Create bitmap iterators
  SI := TsdMapIterator.Create;
  DI := TsdMapIterator.Create;
  try
    GetBitmapIterator(Src, SI);
    GetBitmapIterator(Dst, DI);
    // Check if cellstrides are equal
    if SI.CellStride <> DI.CellStride then
      raise Exception.Create(sIncompatiblePixelFormats);
    // Do the operation
    PerformMapOperation(SI, DI, AOperation);
  finally
    SI.Free;
    DI.Free;
  end;
end;

function InterpolateColor(Col1, Col2: TColor32; Frac: single): TColor32;
// untested!
begin
  Result :=
    Round((Col1 shr 24 and $FF) * (1 - Frac) + (Col2 shr 24 and $FF) * Frac) shl 24 +
    Round((Col1 shr 16 and $FF) * (1 - Frac) + (Col2 shr 16 and $FF) * Frac) shl 16 +
    Round((Col1 shr  8 and $FF) * (1 - Frac) + (Col2 shr  8 and $FF) * Frac) shl  8 +
    Round((Col1        and $FF) * (1 - Frac) + (Col2        and $FF) * Frac);
end;

end.
