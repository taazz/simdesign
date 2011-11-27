{ unit sdMapResample

  Resample a map such that the resulting map's pixels contain the exact average
  of the area covered in the source map. In order to allow sub-pixel accuracy,
  the source rectangle that is copied to the destination map must be given
  in fixed point format (24.8).

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.
}
unit sdMapResample;

interface

uses
  Windows, sdMapIterator;

// Resamples the SrcRect rectangle to the Dst map. Src and Dst should be
// 8bit/pixel map iterators, SrcRect should be given in 24.8 fixed format.
procedure ResampleExactCoverage8bit(Src, Dst: TsdMapIterator; SrcRect: TRect);

implementation

procedure ResampleExactCoverage8bit(Src, Dst: TsdMapIterator; SrcRect: TRect);
var
  Xf0, Xf1, Yf0, Yf1, XB0, XB1, x, y, BufLen: integer;
  Dxf, Dyf: double;
  PS, PD: Pbyte;
  Buffer: array of integer;
  // local
  procedure CopyYStripToBuffer(Yf0, Yf1: integer);
  var
    i, y, Y0, Y1, Dy, Dy1: integer;
  begin
    Y0 := Yf0 shr 8;
    Y1 := Yf1 shr 8;
    if Y0 = Y1 then
    begin
      // Just one strip
      PS := Src.At(XB0, Y0);
      for i := 0 to BufLen - 1 do
      begin
        Buffer[i] := PS^;
        inc(PS, Src.CellStride);
      end;
    end else
    begin
      // Multiple strips - first strip
      Dy := 256 - (Yf0 - Y0 * 256);
      PS := Src.At(XB0, Y0);
      for i := 0 to BufLen - 1 do
      begin
        Buffer[i] := PS^ * Dy;
        inc(PS, Src.CellStride);
      end;
      // intermediate strips
      for y := Y0 + 1 to Y1 - 1 do
      begin
        PS := Src.At(XB0, Y);
        inc(Dy, 256);
        for i := 0 to BufLen - 1 do
        begin
          inc(Buffer[i], PS^ * 256);
          inc(PS, Src.CellStride);
        end;
      end;
      // last strip
      Dy1 := Yf1 - Y1 * 256;
      if Dy1 > 0 then
      begin
        PS := Src.At(XB0, Y1);
        for i := 0 to BufLen - 1 do
        begin
          inc(Buffer[i], PS^ * Dy1);
          inc(PS, Src.CellStride);
        end;
        inc(Dy, Dy1);
      end;
      // Now scale values
      for i := 0 to BufLen - 1 do
      begin
        Buffer[i] := Buffer[i] div Dy;
      end;
    end;
  end;
  // local
  procedure CopyXStripToDest(Xf0, Xf1: integer);
  var
    x, X0, X1, XB, Dx, Dx1, V: integer;
  begin
    X0 := Xf0 shr 8;
    X1 := Xf1 shr 8;
    XB := X0 - XB0;
    if X0 = X1 then
    begin
      // Only one col
      PD^ := Buffer[XB];
    end else
    begin
      // Multiple cols - first col
      Dx := 256 - (Xf0 - X0 * 256);
      V := Dx * Buffer[XB];
      // intermediate strips
      for x := 1 to X1 - X0 - 1 do
      begin
        inc(V, Buffer[x + XB] * 256);
        inc(Dx, 256);
      end;
      // last col
      Dx1 := Xf1 - X1 * 256;
      if Dx1 > 0 then
      begin
        inc(V, Buffer[X1 - X0 + XB] * Dx1);
        inc(Dx, Dx1);
      end;
      // Now scale values
      PD^ := V div Dx;
    end;
  end;
// main  
begin
  Xf0 := SrcRect.Left;
  Xf1 := SrcRect.Right;
  XB0 := Xf0 shr 8;
  XB1 := (Xf1 - 1) shr 8;
  BufLen := XB1 - XB0 + 1;
  SetLength(Buffer, BufLen);
  Yf0 := SrcRect.Top;
  Yf1 := SrcRect.Bottom;
  Dyf := (Yf1 - Yf0) / Dst.Height;
  Dxf := (Xf1 - Xf0) / Dst.Width;
  PD := Dst.First;
  for y := 0 to Dst.Height - 1 do
  begin
    Yf1 := round(SrcRect.Top + (y + 1) * Dyf);
    // Copy this strip
    CopyYStripToBuffer(Yf0, Yf1);
    Xf0 := SrcRect.Left;
    for x := 0 to Dst.Width - 1 do
    begin
      Xf1 := round(SrcRect.Left + (x + 1) * Dxf);
      CopyXStripToDest(Xf0, Xf1);
      Xf0 := Xf1;
      PD := Dst.Next;
    end;
    // Rolldown
    Yf0 := Yf1;
  end;
end;

end.
