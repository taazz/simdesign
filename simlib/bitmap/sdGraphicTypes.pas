{ unit sdGraphicTypes.pas

  Description:
  platform-independent Graphic types used by SimDesign

  Author: Nils Haeck M.Sc.
  Copyright (c) 2011 SimDesign B.V.
  Creation Date: 04Apr2011
  More information: www.simdesign.nl or n.haeck"at"simdesign.nl

  This software may ONLY be used or replicated in accordance with
  the LICENSE found in this source distribution.

}
unit sdGraphicTypes;

interface

type

  // device independent color
  PsdColor = ^TsdColor;
  TsdColor = cardinal;

  // device independent pixelformat
  TsdPixelFormat =
    (spf1bit, spf2bit, spf4bit, spf8bit, spf10bit, spf12bit, spf15bit, spf16bit,
     spf24bit, spf30bit, spf32bit, spf36bit, spf48bit, spf64bit);

  // device independent PaletteEntry record (8bit x 4ch)
  PsdPaletteEntryRec = ^TsdPaletteEntryRec;
  TsdPaletteEntryRec = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;

  // device independent Palette record
  PsdPaletteRec = ^TsdPaletteRec;
  TsdPaletteRec = packed record
    NumEntries: word;
    Entry: array[0..0] of TsdPaletteEntryRec;
  end;

  // TsdRect
  PsdRect = ^TsdRect;
  TsdRect = packed record
    Left, Top, Right, Bottom: Longint;
  end;

// reallocate APalette with ANumEntries entries, result is palette size in bytes
function sdReallocPalette(var APalette: PsdPaletteRec; const ANumEntries: word): integer;

// size of bits in storage for a bitmap's cell with pixelformat AFormat
function sdPixelFormatToBitCount(const AFormat: TsdPixelFormat): integer;
function sdPixelFormatToByteCount(const AFormat: TsdPixelFormat): integer;
function sdBitCountToPixelFormat(const ABitCount: integer): TsdPixelFormat;

implementation

function sdPixelFormatToBitCount(const AFormat: TsdPixelFormat): integer;
begin
  case AFormat of
  spf1bit: Result := 1;
  spf2bit: Result := 2;
  spf4bit: Result := 4;
  spf8bit: Result := 8;
  spf10bit: Result := 10;
  spf12bit: Result := 12;
  spf15bit: Result := 15;
  spf16bit: Result := 16;
  spf24bit: Result := 24;
  spf30bit: Result := 32;
  spf32bit: Result := 32;
  spf36bit: Result := 36;
  spf48bit: Result := 48;
  spf64bit: Result := 64;
  else
    Result := 0;
  end;
end;

function sdPixelFormatToByteCount(const AFormat: TsdPixelFormat): integer;
begin
  case AFormat of
  spf1bit: Result := 1;
  spf2bit: Result := 1;
  spf4bit: Result := 1;
  spf8bit: Result := 1;
  spf10bit: Result := 2;
  spf12bit: Result := 2;
  spf15bit: Result := 2;
  spf16bit: Result := 2;
  spf24bit: Result := 3;
  spf30bit: Result := 4;
  spf32bit: Result := 4;
  spf36bit: Result := 5;
  spf48bit: Result := 6;
  spf64bit: Result := 8;
  else
    Result := 0;
  end;
end;

function sdBitCountToPixelFormat(const ABitCount: integer): TsdPixelFormat;
begin
  case ABitCount of
  1: Result := spf1bit;
  2: Result := spf2bit;
  4: Result := spf4bit;
  8: Result := spf8bit;
  10: Result := spf10bit;
  12: Result := spf12bit;
  15: Result := spf15bit;
  16: Result := spf16bit;
  24: Result := spf24bit;
  32: Result := spf32bit;
  36: Result := spf36bit;
  48: Result := spf48bit;
  64: Result := spf64bit;
  else
    Result := spf8bit;
  end;
end;

function sdReallocPalette(var APalette: PsdPaletteRec; const ANumEntries: word): integer;
begin
  Result := SizeOf(TsdPaletteRec) + ANumEntries * SizeOf(TsdPaletteEntryRec);
  ReallocMem(APalette, Result);
end;

end.