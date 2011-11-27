{ unit sdMonochromeDither

  Dither a 24-bit colour bitmap into a monochrome bi-colour bitmap (just black and
  white pixels), using Floyd-Steinberg dithering.

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.
}
unit sdMonochromeDither;

interface

uses
  Windows, // for gettickcount
  Classes, SysUtils, Graphics,
  sdBitmapConversionWin, sdMapIterator;


type

  TsdDynIntArray = array of integer;

  TsdMonochromeDither = class(TComponent)
  private
    FLines: array of TsdDynIntArray;
    FErrors: array of pinteger;
    FLineWidth: integer;
    FDest: TBitmap;
    FSource: TBitmap;
    FDurationMS: integer;
  protected
    procedure PrepareErrorLines(ALineCount, AOvershoot: integer);
    procedure RollErrorLines(ALineCount: integer);
  public
    // Dither the image in Source using Floyd-Steinberg dithering, producing
    // exactly 2 colors (white and black pixels). The F-S algorithm uses error
    // dispersion: averaging dithering errors over surrounding pixels. F-S uses
    // 4 pixels to average over (1 to the right, 3 below the current pixel), and
    // changes direction left to right, right to left, with each scanline.
    procedure DitherFloydSteinberg;
    // The incoming Source bitmap is expected to be a pf24bit RGB bitmap.
    property Source: TBitmap read FSource write FSource;
    // The destination bitmap Dest will contain the dithered image, and will
    // be set to pf8bit, with 2 colours defined in the palette (black-0, and white-1).
    property Dest: TBitmap read FDest write FDest;
    // Duration of last dithering operation in millisec
    property DurationMS: integer read FDurationMS;
  end;

implementation

{ TsdMonochromeDither }

procedure TsdMonochromeDither.DitherFloydSteinberg;
var
  FStartTick: cardinal;
  Error0: pinteger;
  Error1: pinteger;
  Line, Direction: integer;
  Cmp, Err, Err16: integer;
  ZigZag: TsdMapIterator;
  S: pbyte;
  // local
  procedure SetStartErrorPosition;
  begin
    Error0 := FErrors[0];
    Error1 := FErrors[1];
    if ZigZag.Direction > 0 then
    begin
      // set to start of line + 1
      inc(Error0, 1);
      inc(Error1, 1);
    end else
    begin
      // set to end of line - 1
      inc(Error0, FLineWidth - 2);
      inc(Error1, FLineWidth - 2);
    end;
  end;
  // Propagate Floyd-Steinberg error terms.
  // Errors are accumulated into the error arrays, at a resolution of
  // 1/16th of a pixel count. The error at a given pixel is propagated
  // to its not-yet-processed neighbors using the standard F-S fractions,
  //    ....	  *   7/16
  //    3/16  5/16  1/16
  // We work left-to-right on even rows, right-to-left on odd rows.
  procedure ErrorDisperseToRight(Err: integer);
  var
    Err2, Err3, Err5, Err7: integer;
  begin
    Err2 := 2 * Err;
    Err3 := Err2 + Err;
    Err5 := Err3 + Err2;
    Err7 := Err5 + Err2;
    inc(Error0);
    inc(Error0^, Err7);
    inc(Error1^, Err5);
    dec(Error1);
    inc(Error1^, Err3);
    inc(Error1, 2);
    inc(Error1^, Err);
  end;
  procedure ErrorDisperseToLeft(Err: integer);
  var
    Err2, Err3, Err5, Err7: integer;
  begin
    Err2 := 2 * Err;
    Err3 := Err2 + Err;
    Err5 := Err3 + Err2;
    Err7 := Err5 + Err2;
    dec(Error0);
    inc(Error0^, Err7);
    inc(Error1^, Err5);
    inc(Error1);
    inc(Error1^, Err3);
    dec(Error1, 2);
    inc(Error1^, Err);
  end;
begin
  // Set up destination bitmap
  FDest.PixelFormat := pf8bit;
  FDest.Width := FSource.Width;
  FDest.Height := FSource.Height;
  SetBitmap8bitBlackWhite(FDest);

  // Dithering process: first convert to grayscale, then dither to mono

  // Start tick (for duration)
  FStartTick := GetTickCount;

  // Prepare the error lines
  PrepareErrorLines(2, 1);

  // Convert to grayscale
  BitmapToGrayScale24To8(FSource, FDest, gcWeighted);

  // Zig-zag through lines, and do error-dispersion
  ZigZag := TsdMapIterator.Create;
  try
    // Setup iterator
    GetBitmapIterator(FDest, ZigZag);
    ZigZag.Method := imZigZag;

    // start cell
    S := ZigZag.First;
    Line := 0;
    Direction := 1;
    SetStartErrorPosition;

    while assigned(S) do
    begin

      // Check for line jump
      if Line <> ZigZag.Line then
      begin
        // we're on a new line
        RollErrorLines(2);
        SetStartErrorPosition;
        Line := ZigZag.Line;
        Direction := ZigZag.Direction;
      end;

      // Compare value
      Err16 := Error0^;
      if abs(Err16) < 8 then
        Cmp := S^
      else
        if Err16 > 0 then
          Cmp := S^ + (Err16 + 8) shr 4
        else
          Cmp := S^ - (-Err16 + 8) shr 4;

      if Cmp >= 128 then
      begin
        // We set this pixel to white
        S^ := 1;
        Err := Cmp - 255;
      end else
      begin
        // we set this pixel to black
        S^ := 0;
        Err := Cmp;
      end;

      // Depending on direction we disperse
      if Err = 0 then
      begin
        // no dispersion if Error = 0, just go to next cell
        inc(Error0, Direction);
        inc(Error1, Direction);
      end else
        if Direction > 0 then
          ErrorDisperseToRight(Err)
        else
          ErrorDisperseToLeft(Err);

      // Next cell
      S := ZigZag.Next;
    end;
  finally
    ZigZag.Free;
  end;

  // Duration of dithering operation
  FDurationMS := cardinal(GetTickCount - FStartTick);
end;

procedure TsdMonochromeDither.PrepareErrorLines(ALineCount, AOvershoot: integer);
var
  i: integer;
begin
  FLineWidth := FSource.Width + 2 * AOvershoot;
  SetLength(FLines, ALineCount);
  SetLength(FErrors, ALineCount);
  for i := 0 to ALineCount - 1 do
  begin
    SetLength(FLines[i], FLineWidth);
    FillChar(FLines[i, 0], FLineWidth * SizeOf(integer), 0);
    FErrors[i] := @FLines[i, 0];
  end;
end;

procedure TsdMonochromeDither.RollErrorLines(ALineCount: integer);
var
  i: integer;
  TmpError: pinteger;
begin
  // Roll 'em
  TmpError := FErrors[0];
  for i := 0 to ALineCount - 2 do
    FErrors[i] := FErrors[i + 1];
  FErrors[ALineCount - 1] := TmpError;
  // Clear last line
  FillChar(TmpError^, FLineWidth * SizeOf(integer), 0);
end;

end.
