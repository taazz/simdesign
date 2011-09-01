{ unit MandelbrotMain

  The purpose of this demo is to show how to use the virtual paradigm:
  TsdVirtualScrollbox only asks this code to compute the mandelbrot set
  in the visible area of the window.
  Whenever the user scrolls the scrollbox, the visible area is updateda and
  recomputed through the event OnPaint (implemented in BoxPaint).

  Author: Nils Haeck M.Sc.
  copyright (c) 2003 SimDesign B.V.

}
unit MandelbrotMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sdVirtualScrollbox, sdBitmapConversionWin, sdMapIterator;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FBox: TsdVirtualScrollBox;
    FBmp: TBitmap;
    procedure BoxPaint(Sender: TObject; ACanvas: TCanvas);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

procedure Mandelbrot(AIter: TsdMapIterator; ALeft, ATop: integer;
  const XScl, XOfs, YScl, YOfs, Lim: double);
var
  xi, yi, lp: integer;
  PB: Pbyte;
  xv, yv, a1, a2, b1, b2: double;
  X: array of double;
begin
  SetLength(X, AIter.Width);
  for xi := 0 to AIter.Width - 1 do
    X[xi] := (ALeft + xi) * XScl + XOfs;
  PB := AIter.First;
  for yi := 0 to AIter.Height - 1 do
  begin
    yv := (ATop + yi) * YScl + YOfs;
    for xi := 0 to AIter.Width - 1 do
    begin
      xv := X[xi];
      // Mandelbrot formula
      a1 := xv;
      b1 := yv;
      lp := 0;
      repeat
        // Do one iteration.
        inc(lp);
        a2 := a1 * a1 - b1 * b1 + xv;
        b2 := 2 * a1 * b1 + yv;
        // This is indeed the square of a+bi, done component-wise.
        a1 := a2;
        b1 := b2;
      until (lp > 255) or ((a1 * a1) + (b1 * b1) > Lim);
      // The first condition is satisfied if we have convergence.
      // The second is satisfied if we have divergence.
      if lp > 255 then lp := 0;
      PB^ := lp;
      PB := AIter.Next;
    end;
  end;
end;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FBox := TsdVirtualScrollbox.Create(Self);
  FBox.Parent := Self;
  FBox.Align := alClient;
  FBox.OnPaint := BoxPaint;
  FBox.SetScrollBounds(0, 0, 2000, 2000);
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf8bit;
  SetBitmap8bitRainbow(FBmp);
end;

procedure TfrmMain.BoxPaint(Sender: TObject; ACanvas: TCanvas);
var
  B, C: TRect;
  Iter: TsdMapIterator;
begin
  // Canvas clipping rectangle
  C := ACanvas.ClipRect;
  // Virtual rectangle
  B := Rect(0, 0, FBox.ScrollWidth, FBox.ScrollHeight);
  OffsetRect(B, -FBox.ScrollLeft, -FBox.ScrollTop);
  IntersectRect(C, B, C);
  if IsRectEmpty(C) then
    exit;
  // Create bitmap size of rectangle to paint
  FBmp.Width := C.Right - C.Left;
  FBmp.Height := C.Bottom - C.Top;
  Iter := TsdMapIterator.Create;
  try
    GetBitmapIterator(FBmp, Iter);
    Mandelbrot(Iter, C.Left + FBox.ScrollLeft, C.Top + FBox.ScrollTop, 0.001, -1, 0.001, -1, 4);
    ACanvas.Draw(C.Left, C.Top, FBmp);
  finally
    Iter.Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
end;

end.
