{ unit sdPdfGDIRenderer

  PDF demonstration unit that shows how to render to a GDI device context.
  PDF 5th edition, version 1.5

  Author: Nils Haeck M.Sc.

  Changes:
    14Jan2004 - Created
    21Sep2005 - Extended RenderToRGBbitmap

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfGDIRenderer;

interface

uses
  Classes, SysUtils, Windows, Graphics, sdPdfDocument, sdPdfContentStream, sdPdfObjects,
  sdPdfCoordinateSpace, sdPdfImages, sdPdfColors;

type

  // Render a PDF page to any GDI device context, using GDI commands. This is a
  // very crude way of rendering since many sophisticated PDF features are not
  // available as GDI command. Also, there's dependence on Windows version:
  // Some things render correctly on NT/2K/XP but not on 95/98. Another big
  // point of concern are fonts; the windows font rasterizing looks awful when
  // working with (integer) spacing between characters, and only TrueType fonts
  // are somewhat reliably substituted, if present on the clients machine.
  TPdfGDIViewer = class(TPersistent)
  private
    FPage: integer;
    FDocument: TPdfDocument;
    FHeight: integer;
    FParser: TPdfParser;
  protected
    // Convert pdf coordinates ot canvas coordinates
    procedure PdfToCanvas(const Xpdf, Ypdf: double; var Xcanvas, Ycanvas: integer);
    procedure RenderTextShape(AShape: TPdfTextShape; Canvas: TCanvas); virtual;
    procedure RenderImageShape(AShape: TPdfImageShape; Canvas: TCanvas); virtual;
    procedure RenderShapes(Parser: TPdfParser; Canvas: TCanvas);
    procedure RenderToRGBBitmap(AShape: TPdfImageShape; Bitmap: TBitmap); virtual;
  public
    procedure RenderToCanvas(Canvas: TCanvas; Dest: TRect);
    property Document: TPdfDocument read FDocument write FDocument;
    property Page: integer read FPage write FPage;
  end;

implementation

{ TPdfGDIViewer }

procedure TPdfGDIViewer.PdfToCanvas(const Xpdf, Ypdf: double; var Xcanvas,
  Ycanvas: integer);
begin
  // We use a scaling factor of 1000 and have to invert the Y coordinate
  Xcanvas := round(Xpdf * 1000);
  Ycanvas := FHeight - round(Ypdf * 1000);
end;

procedure TPdfGDIViewer.RenderImageShape(AShape: TPdfImageShape; Canvas: TCanvas);
// Create a bitmap and render it to the page
var
  ABmp: TBitmap;
  R: TRect;
begin
  // Setup bitmap
  ABmp := TBitmap.Create;
  try
    // Make our life easy: always use pf24Bit - note: this is not the fastest solution
    RenderToRGBBitmap(AShape, ABmp);
    // Draw to the device context. We don't handle rotation in the GDI renderer!
    with AShape.Position do begin
      PdfToCanvas(E, D + F, R.Left, R.Top);
      PdfToCanvas(A + E, F, R.Right, R.Bottom);
      Canvas.StretchDraw(R, ABmp);
    end;
  finally
    ABmp.Free;
  end;
end;

procedure TPdfGDIViewer.RenderShapes(Parser: TPdfParser; Canvas: TCanvas);
var
  i: integer;
begin
  if not assigned(Parser) then exit;
  FParser := Parser;
  with Parser do begin
    for i := 0 to ShapeCount - 1 do begin
      if Shapes[i] is TPdfTextShape then begin
        RenderTextShape(TPdfTextShape(Shapes[i]), Canvas);
        continue;
      end;
      if Shapes[i] is TPdfImageShape then begin
        RenderImageShape(TPdfImageShape(Shapes[i]), Canvas);
        continue;
      end;
    end;
  end;
end;

procedure TPdfGDIViewer.RenderTextShape(AShape: TPdfTextShape; Canvas: TCanvas);
var
  i, Xc, Yc: integer;
  Start, Close: integer;
  Widths: array of integer;
  AText: string;
// local
function EqualHeightAndSize(i, j: integer): boolean;
begin
  with AShape do begin
    Result :=
      (Glyphs[i].Position.D = Glyphs[j].Position.D) and
      (Glyphs[i].Position.F = Glyphs[j].Position.F);
  end;
end;
// main
begin
  Canvas.Font.Color := TColor(AShape.PaintColor.AsWindowsRGB);
  Canvas.Font.Assign(AShape.Font.GDIFont);

  with AShape do begin
    // Remove spaces, no sense to draw them
    for i := GlyphCount - 1 downto 0 do
      if Glyphs[i].Char = ' ' then
        GlyphDelete(i);

    // Find strings that are equal in height and size
    Start := 0;
    repeat
      if Start >= GlyphCount then break;

      // Process characters, find biggest string that is equal width
      Close := Start + 1;
      while (Close < GlyphCount) and EqualHeightAndSize(Start, Close) do
        inc(Close);
      dec(Close);

      // Now find the partial string and its widths
      AText := '';
      SetLength(Widths, Close - Start + 1);
      for i := Start to Close do begin
        AText := AText + Glyphs[i].Text;
        if i < Close then
          Widths[i - Start] := round((Glyphs[i + 1].Position.E - Glyphs[i].Position.E) * 1000)
        else
          Widths[i - Start] := 0;
      end;

      // And set the font height, then draw TextOut
      Canvas.Font.Height := round(Glyphs[Start].Position.D * 1000);

      // Convert to canvas coords
      PdfToCanvas(Glyphs[Start].Position.E, Glyphs[Start].Position.F, Xc, Yc);

      // output text to device
      ExtTextOut(Canvas.Handle, Xc, Yc, 0, nil, pchar(AText), length(AText), @Widths[0]);

      // Next partial string
      Start := Close + 1;
    until False;
  end;
end;

procedure TPdfGDIViewer.RenderToCanvas(Canvas: TCanvas; Dest: TRect);
var
  AParser: TPdfParser;
  AStream: TPdfContentStream;
  AWindow: TRect;
  ABox: TPdfRectangle;
begin
  if not assigned(Document) then exit;
  AStream := TPdfContentStream.Create;
  AParser := TPdfParser.Create(Document);
  try
    // Load content stream
    Document.DoDebugMessage(dlInfo, Format('Rendering Page %d', [Page]));
    AStream.LoadData(Document.PageTree[Page].ValueByKey('Contents'));
    Document.DoDebugMessage(dlInfo, 'Page contents:'#13#10 + AStream.AsString);

    // Parse to intermediate shape list format
    AParser.ParseContent(AStream, Document.PageTree[Page]);

    // Set DC mode and size
    SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
    SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    SetTextAlign(Canvas.Handle, TA_BASELINE);

    // Set window and viewport coordinates
    SetViewportOrgEx(Canvas.Handle, Dest.Left, Dest.Top, nil);
    SetViewportExtEx(Canvas.Handle, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top, nil);

    ABox := Document.Pages[Page].Cropbox;
    AWindow := Rect(
      round(ABox.Left),
      round(ABox.Top),
      round(ABox.Right),
      round(ABox.Bottom));
    with AWindow do begin
      SetWindowOrgEx(Canvas.Handle, Left, Top, nil);
      FHeight := (Bottom - Top) * 1000;
      SetWindowExtEx(Canvas.Handle, (Right - Left) * 1000, FHeight, nil);
    end;

    // Render to the canvas
    RenderShapes(AParser, Canvas);

  finally
    AParser.Free;
    AStream.Free;
  end;
end;

procedure TPdfGDIViewer.RenderToRGBBitmap(AShape: TPdfImageShape;
  Bitmap: TBitmap);
var
  r, c8, c, i, j, k: integer;
  B: byte;
  ChannelCount: integer;
  ScanWidth: integer;
  Scan: PByte;
begin
  if not assigned(AShape) or not assigned(Bitmap) then exit;
  // Checks
  if not assigned(AShape.Source) then begin
    FParser.DoDebugMessage(dlWarning, 'No image source found');
    exit;
  end;
  if AShape.Source.Size = 0 then begin
    FParser.DoDebugMessage(dlWarning, 'Image source contains nothing');
    exit;
  end;
  AShape.Source.Seek(0, soFromBeginning);
  // 24bit image
  Bitmap.PixelFormat := pf24bit;
  // Set image size
  Bitmap.Width  := AShape.Width;
  Bitmap.Height := AShape.Height;
  // Channels and bitspercomponent
  ChannelCount := AShape.ChannelCount;
  case AShape.BitsPerComponent of
  1: // 1-bit (monochrome)
    begin
      if ChannelCount <> 1 then begin
        FParser.DoDebugMessage(dlWarning, Format('Unable to display 1-bpc %d channel image', [ChannelCount]));
        exit;
      end;
      ScanWidth := (AShape.Width + 7) div 8;
      for r := 0 to AShape.Height - 1 do begin
        Scan := Bitmap.Scanline[r];
        c := 0;
        for c8 := 0 to ScanWidth - 1 do begin
          AShape.Source.Read(B, 1);
          for i := 0 to 7 do begin
            if C < AShape.Width then
              if (B and $80) = 0 then begin
                // White
                Scan^ := $FF; inc(Scan);
                Scan^ := $FF; inc(Scan);
                Scan^ := $FF; inc(Scan);
              end else begin
                // Black
                Scan^ := $00; inc(Scan);
                Scan^ := $00; inc(Scan);
                Scan^ := $00; inc(Scan);
              end;
            inc(C);
            B := B shl 1;
          end;
        end;
      end;
    end;
  8: // 8-bit grayscale or 24-bit color
    begin
      case ChannelCount of
      1:
        begin
          for i := 0 to AShape.Height - 1 do begin
            Scan := Bitmap.ScanLine[i];
            for j := 0 to AShape.Width - 1 do begin
              AShape.Source.Read(B, 1);
              for k := 0 to 2 do begin
                Scan^ := B; inc(Scan);
              end;
            end;
          end;
        end;
      3:
        begin
          for i := 0 to Bitmap.Height - 1 do
            AShape.Source.Read(Bitmap.ScanLine[i]^, AShape.Width * 3);
        end;
      else
        FParser.DoDebugMessage(dlWarning, Format('Unable to display 8-bpc %d channel image', [ChannelCount]));
        exit;
      end;//case
    end;
  else
    FParser.DoDebugMessage(dlWarning, Format('Unable to display image with %d bitspercomponent', [AShape.BitsPerComponent]));
  end;
end;

end.
