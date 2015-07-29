unit dtpRsRuler;

//----------------------------------------------------------------------------
//  Delphi 2-6, C++Builder 5 Ruler component, version 3.0, 7 nov 2001
//----------------------------------------------------------------------------
//  (c) 2000, 2001 Hans Roos, Roos Software, The Netherlands
//  Website: www.RoosSoftware.nl
//  Email: mail@roossoftware.nl
//----------------------------------------------------------------------------
//  Features:
//  4 layouts rdTop, rdLeft, rdRight and rdBottom with
//    automatic scale adjustments for each layout
//  Units: Inches, Centimetres, Millimetres, Pixels
//  Automatic calculation of scalenumbers (no overlapping)
//  Sideways text for vertical layouts
//  Flat or 3D appearance
//  TRsRulerCorner: extra component for joining up to 4
//    rulers, can show the unit ('cm', 'mm', 'in' or 'px')
//  Font can be changed; sideways fonts only possible if True Type font!
//----------------------------------------------------------------------------
//  See demo project for usage
//  Licence: Freeware! Use in non-commercial or commercial apps
//  Feel free to modify the source for your own needs, but don't remove
//  my name from this file, please.
//  If you find this component useful, please let me know.
//  Don't send money, just be grateful ;)
//----------------------------------------------------------------------------
//  Known issues: None
//  Not yet implemented:
//  Better scale divisions when Inches are used
//  (is it customary to divide inches in 4ths, 8ths, 16ths etc?)
//  Anything YOU can think of; please let me know!! (mail@roossoftware.nl)
//----------------------------------------------------------------------------
//  Revision History
//  v.3.0, 07/11/2001
//    Added properties:
//    property Font, ParentFont: user can select any font for scale-drawing.
//    (vertical fonts can only be drawn if True Type font is chosen)
//    property Color, TickColor, Font.Color, ScaleColor
//    property Offset: if you want RsRuler to begin with another number than 0
//    Offset is recalculated when you choose another measuring unit.
//    property ShowMinus: if negative offset, toggle minus sign visibility
//  v.2.0, 31/10/2001
//    Added property value: ruPixel, for measuring pixel units.
//    Added public function Pos2Unit: to calculate unit from mouse position.
//    (see LogoImageMouseMove procedure in demo project for usage)
//  v.1.1, 30/06/2001
//    Added properties :
//    property HairLine, HairLinePos: line on scale, moving with CursorPos.
//    property HairLineStyle: hlsLine (just a hairline)
//      or hlsRect (inverted rectangle).
//  v.1.0, 22/11/2000
//    First release.
//----------------------------------------------------------------------------

// Adapted for dtpDocuments by Nils Haeck
// - Added ScreenDpm property that should be set to TdtpDocument.ScreenDpm
// - Prepended with dtp* to avoid confusion with RsRuler of Crystal Reports on the palette
// contributor: JF


interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, ExtCtrls, Math;

type

  TRulerDir =
   (rdTop,
    rdLeft,
    rdRight,
    rdBottom);

  TRulerUnit =
   (ruCenti,
    ruMilli,
    ruInch,
    ruPixel,
    ruNone);

  TCornerPos =
   (cpLeftTop,
    cpRightTop,
    cpLeftBottom,
    cpRightBottom);

  THairLineStyle =
   (hlsLine,
    hlsRect);

const

  // Optimal number of pixels per label
  cOptimalLabelDist = 40;

  // Ruler unit names
  cRulerUnitNames: array[TRulerUnit] of string =
    ('cm',
     'mm',
     'in',
     'px',
     '');

  // Ruler unit hints
  cRulerUnitHints: array[TRulerUnit] of string =
    ('centimeter',
     'millimeter',
     'inch',
     'pixel',
     '');

type

  // base class, defines common properties and behaviour of its
  // descendants TRsRuler and TRsRulerCorner
  TdtpRsBaseRuler = class(TGraphicControl)
  private
    FFlat: Boolean;
    FScaleColor: TColor;
    FTickColor: TColor;
    FUnits: TRulerUnit;
    procedure SetFlat(const Value: Boolean);
    procedure SetScaleColor(const Value: TColor);
    procedure SetTickColor(const Value: TColor);
  protected
    LeftSideLF, RightSideLF, NormLF: TLogFont;
    OldFont, NormFont, LeftSideFont, RightSideFont: HFont;
    FirstTime: Boolean;
    procedure Paint; override;
    procedure SetUnit(const Value: TRulerUnit); virtual;
    procedure FontChange(Sender: TObject);
    procedure ChangeFonts;
    procedure DeleteFonts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Units: TRulerUnit read FUnits write SetUnit;
    property Flat: Boolean read FFlat write SetFlat;
    property ScaleColor: TColor read FScaleColor write SetScaleColor;
    property TickColor: TColor read FTickColor write SetTickColor;
  end;

  // added by J.F. Feb 2011
  TdtpRsRulerEvent = procedure (Sender: TObject; var AMessage: TMessage) of object;

  TdtpRsRuler = class(TdtpRsBaseRuler)
  private
    FDirection: TRulerDir;
    FHairLine: Boolean;
    FHairLinePos: Integer;
    FHairLineStyle: THairLineStyle;
    FOffset: Double;
    FShowMinus: Boolean;
    FScreenDpm: double;
    FScale: double;
    // added by J.F. Feb 2011
    FOnRulerEvent: TdtpRsRulerEvent;

    procedure SetDirection(const Value: TRulerDir);
    procedure SetHairLine(const Value: Boolean);
    procedure SetHairLinePos(const Value: Integer);
    procedure SetHairLineStyle(const Value: THairLineStyle);
    procedure SetOffset(const Value: Double);
    procedure SetShowMinus(const Value: Boolean);
    procedure SetScreenDpm(const Value: double);
    procedure SetScale(const Value: double);
  protected
    procedure SetUnit(const Value: TRulerUnit); override;
    procedure DrawHairLine;
    procedure PaintScaleLabels;
    procedure Paint; override;
    function GetPixPerUnit: double;
    // added by J.F. Feb 2011
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    property PixPerUnit: double read GetPixPerUnit;
    // added by J.F. Feb 2011
    property OnRulerEvent: TdtpRsRulerEvent read FOnRulerEvent write FOnRulerEvent;
  published
    property Direction: TRulerDir read FDirection write SetDirection;
    property Units;
    property HairLine: Boolean read FHairLine write SetHairLine;
    property HairLinePos: Integer read FHairLinePos write SetHairLinePos;
    property HairLineStyle: THairLineStyle read FHairLineStyle write SetHairLineStyle;
    property ScaleColor;
    property TickColor;
    // Offset in pixels
    property Offset: Double read FOffset write SetOffset;
    property ShowMinus: Boolean read FShowMinus write SetShowMinus;
    property ScreenDpm: double read FScreenDpm write SetScreenDpm;
    property Scale: double read FScale write SetScale;
    property Align;
    property Font;
    property Color;
    property Height;
    property Width;
    property Visible;
    property Hint;
    property ShowHint;
    property Tag;
    property ParentFont;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

  TdtpRsRulerCorner = class(TdtpRsBaseRuler)
  private
    FPosition: TCornerPos;
    procedure SetPosition(const Value: TCornerPos);
  protected
    FUStr: String;
    procedure Paint; override;
    procedure SetUnit(const Value: TRulerUnit); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Position: TCornerPos read FPosition write SetPosition;
    property Flat;
    property ScaleColor;
    property TickColor;
    property Font;
    property Color;
    property Units;
    property Visible;
    property Hint;
    property ShowHint;
    property Tag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SimDesign', [TdtpRsRuler, TdtpRsRulerCorner]);
end;

{ TdtpRsBaseRuler }

constructor TdtpRsBaseRuler.Create(AOwner: TComponent);
begin
  inherited;
  // Initialize vars:
  FFlat := False;
  FUnits := ruCenti;
  FScaleColor := clWindow;
  FTickColor := clWindowText;
  FirstTime := True;
  OldFont := 0;
  NormFont := 0;
  LeftSideFont := 0;
  RightSideFont := 0;
  Font.OnChange := FontChange;
end;

procedure TdtpRsBaseRuler.ChangeFonts;
begin
  DeleteFonts;
  // Fill LogFont structures:
  with LeftSideLF do
  begin
    FillChar(LeftSideLF, SizeOf(LeftSideLF), 0);
    lfEscapement := 900;
    lfOrientation := 900;
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;
  with RightSideLF do
  begin
    FillChar(RightSideLF, SizeOf(RightSideLF), 0);
    lfEscapement := 2700;
    lfOrientation := 2700;
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;
  with NormLF do
  begin
    FillChar(NormLF, SizeOf(NormLF), 0);
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;

  Canvas.Font.Color := Font.Color;
  LeftSideFont := CreateFontIndirect(LeftSideLF);
  RightSideFont := CreateFontIndirect(RightSideLF);
  NormFont := CreateFontIndirect(NormLF);
end;

procedure TdtpRsBaseRuler.DeleteFonts;
begin
  if NormFont <> 0 then
    DeleteObject(NormFont);
  if LeftSideFont <> 0 then
    DeleteObject(LeftSideFont);
  if RightSideFont <> 0 then
    DeleteObject(RightSideFont);
end;

destructor TdtpRsBaseRuler.Destroy;
begin
  DeleteFonts;
  inherited;
end;

procedure TdtpRsBaseRuler.FontChange(Sender: TObject);
begin
  ChangeFonts;
  Invalidate;
end;

procedure TdtpRsBaseRuler.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  if FirstTime then
  begin
    // setup fonts, cannot be done in Create method,
    // so do it when Ruler gets painted...
    FirstTime := False;
    ChangeFonts;
    OldFont := Canvas.Font.Handle;
  end;
end;

procedure TdtpRsBaseRuler.SetFlat(const Value: Boolean);
begin
  if Value <> fFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TdtpRsBaseRuler.SetScaleColor(const Value: TColor);
begin
  if Value <> fScaleColor then
  begin
    FScaleColor := Value;
    Invalidate;
  end;
end;

procedure TdtpRsBaseRuler.SetTickColor(const Value: TColor);
begin
  if Value <> fTickColor then
  begin
    FTickColor := Value;
    Invalidate;
  end;
end;

procedure TdtpRsBaseRuler.SetUnit(const Value: TRulerUnit);
begin
// method is empty, see descendants
end;

{ TdtpRsRuler }

constructor TdtpRsRuler.Create(AOwner: TComponent);
begin
  inherited;
  FDirection := rdTop;
  Height := 33;
  Width := 200;
  FOffset := 0.0;
  FHairLinePos := -1;
  FHairLine := False;
  FHairLineStyle := hlsLine;
  FShowMinus := True;
  FScreenDpm := 90 / 25.4;
  FScale := 1.0;
  FOnRulerEvent := nil;
end;

procedure TdtpRsRuler.WndProc(var Message: TMessage);
// added by J.F. Feb 2011
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if ((Message.Msg = WM_LBUTTONUP) or ((TWMMouse(Message).Keys and MK_LBUTTON <> 0))) and Assigned(FOnRulerEvent) then
      FOnRulerEvent(self,Message);
end;


function TdtpRsRuler.GetPixPerUnit: double;
begin
  // Pixels per unit
  Result := 1.0;
  case FUnits of
  ruMilli: Result := FScreenDpm;
  ruCenti: Result := FScreenDpm * 10;
  ruInch:  Result := FScreenDpm * 25.4;
  ruPixel: Result := 1.0;
  end;//case
  Result := Result * FScale;
end;

function RoundCorrect(Value: extended): integer;
// added by J.F. Feb 2011 fixes Round and RoundTo in some compiler versioins
begin
  // extract the integer part
  Result := Trunc(Value);
  // if fractional part >= 0.5 then..
  if Frac(Value) >= 0.5 then
    // ...add 1
    inc(Result);
end;

procedure TdtpRsRuler.PaintScaleLabels;
var
  i, ScaleN, Start, Close, IUnitsPerLabel, ITicksPerLabel, TickMod, TickLen: integer;
  Last, Wi, He: Integer;
  LabelPos: integer; //changed by J.F. Feb 2011
  LabelText: Utf8String;
  PixPerUnit, PixPerLabel, PixPerTick, UnitsPerLabel, R: double;
  Base10: int64;
begin
  if (fDirection = rdTop) or (fDirection = rdBottom) then
    Last := Width
  else
    Last := Height;

  // Pixels per unit
  PixPerUnit := GetPixPerUnit;

  // Float value
  UnitsPerLabel := cOptimalLabelDist / PixPerUnit;

  // And int version
  Base10 := 1;
  R := UnitsPerLabel;
  while R > 10 do
  begin
    Base10 := Base10 * 10;
    R := R / 10;
  end;

  // changed by J.F. Feb 2011
  if FUnits = ruInch then
  begin
    TickMod := 4;
    IUnitsPerLabel := 1;
    if R < 0.12 then
    begin
      ITicksPerLabel := 64
    end else
    begin
      if R < 0.21 then
      begin
        ITicksPerLabel := 32
      end else
      begin
        if R < 0.49 then
        begin
          ITicksPerLabel := 16
        end else
        begin
          if R < 0.7 then
          begin
            ITicksPerLabel := 8;
            TickMod:= 2;
          end else
          begin
            if R < 1.0 then
            begin
              ITicksPerLabel := 4;
              TickMod:= 2;
            end else
            begin
              ITicksPerLabel := 2;
              TickMod:= 1;
            end;
          end;
        end;
      end;
    end;

  end else
  begin
    //changed by J.F. Feb 2011
    // FUnits <> ruInch
    ITicksPerLabel := 10;
    if R < 1.2 then
    begin
      IUnitsPerLabel := Base10;
      TickMod := 5;
    end else
    begin
      if R < 2.5 then
      begin
        IUnitsPerLabel := 2 * Base10;
        TickMod := 5;
      end else
      begin
        if R < 6 then
        begin
          IUnitsPerLabel := 5 * Base10;
          TickMod := 2;
        end else
        begin
          IUnitsPerLabel := 10 * Base10;
          TickMod := 5;
        end;
      end;
    end;
  end;

  PixPerLabel := IUnitsPerLabel * PixPerUnit;
  PixPerTick := PixPerLabel / ITicksPerLabel;

  // Start and close label
  Start := Floor(FOffset / PixPerLabel);
  Close := Ceil((FOffset + Last) / PixPerLabel);

  // Draw labels
  Canvas.Pen.Color := Font.Color;
  for i := Start to Close do
  begin
    LabelPos := RoundCorrect(-FOffset + (i * PixPerLabel)); // changed by J.F. Feb 2011
    ScaleN := i * IUnitsPerLabel;
    if fShowMinus then
      LabelText := IntToStr(ScaleN)
    else
      LabelText := IntToStr(Abs(ScaleN));
    Wi := Canvas.TextWidth(LabelText);
    He := Canvas.TextHeight(LabelText);
    if (fDirection = rdTop) or (fDirection = rdBottom) then
    begin
      Canvas.MoveTo(LabelPos, 1);  // only Pos is important
      if fDirection = rdTop then
      begin
        // draw number..
        Canvas.TextOut(Canvas.PenPos.X - Wi div 2, Height - He - 8, LabelText)
      end;
      if fDirection = rdBottom then
      begin
        // draw number..
        Canvas.TextOut(Canvas.PenPos.X - Wi div 2, 8, LabelText)
      end;
    end else
    begin
      Canvas.MoveTo(1, LabelPos);
      if fDirection = rdLeft then
      begin
        // draw number..
        Canvas.TextOut(Width - He - 7, Canvas.PenPos.Y + Wi div 2, LabelText)
      end;
      if fDirection = rdRight then
      begin
        Canvas.TextOut(He + 7, Canvas.PenPos.Y - Wi div 2, LabelText)
      end;
    end;
  end;

  // Start and close tick
  Start := Floor(FOffset / PixPerTick);
  Close := Ceil((FOffset + Last) / PixPerTick);

  // Draw ticks
  Canvas.Pen.Color := fTickColor;
  for i := Start to Close do
  begin
    LabelPos := RoundCorrect(-FOffset + (i * PixPerTick)); // changed by J.F. FEb 2011
    TickLen := 2 * (3 + Integer(i mod TickMod = 0));
    if (fDirection = rdTop) or (fDirection = rdBottom) then
    begin
      if fDirection = rdTop then
      begin
        Canvas.MoveTo(LabelPos, Height - 1);
        Canvas.LineTo(LabelPos, Height - TickLen);
      end;
      if fDirection = rdBottom then
      begin
        Canvas.MoveTo(LabelPos, 0);
        Canvas.LineTo(LabelPos, TickLen - 1);
      end;
    end else
    begin
      if fDirection = rdLeft then
      begin
        Canvas.MoveTo(Width - 1, LabelPos);
        Canvas.LineTo(Width - TickLen, LabelPos);
      end;
      if fDirection = rdRight then
      begin
        Canvas.MoveTo(0, LabelPos);
        Canvas.LineTo(TickLen - 1, LabelPos);
      end;
    end;
  end;
end;

procedure TdtpRsRuler.Paint;
var
  Rect: TRect;
  He, d: Integer;
begin
  inherited;
  FHairLinePos := -1;
  Rect := ClientRect;
  if not Flat then
    DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT);
  d := 2 - Integer(Flat);
  SelectObject(Canvas.Handle, NormFont);
  He := Canvas.TextHeight('0') + 6;
  if (FDirection = rdTop) or (FDirection = rdBottom) then
  begin
    if FDirection = rdTop then
      SetRect(Rect, d, Height - He - 1, Width - d, Height - 8);
    if (FDirection = rdBottom) then
      SetRect(Rect, d, 8, Width - d, He + 1);
    SelectObject(Canvas.Handle, NormFont);
  end else
  begin
    if FDirection = rdLeft then
    begin
      SetRect(Rect, Width - He, d, Width - 8, Height - d);
      SelectObject(Canvas.Handle, LeftSideFont);
    end;
    if FDirection = rdRight then
    begin
      SetRect(Rect, He, d, 8, Height - d);
      SelectObject(Canvas.Handle, RightSideFont);
    end;
  end;
  Canvas.Brush.Color := FScaleColor;
  Canvas.FillRect(Rect);
  SetBKMode(Canvas.Handle, TRANSPARENT);
  PaintScaleLabels;
  SetBKMode(Canvas.Handle, OPAQUE);
  SelectObject(Canvas.Handle, OldFont);
end;

procedure TdtpRsRuler.SetDirection(const Value: TRulerDir);
var
  Dim: TPoint;
  OldDir: TRulerDir;
begin
  OldDir := fDirection;
  if Value <> FDirection then
  begin
    if ((OldDir = rdTop) or (OldDir = rdBottom)) and ((Value = rdLeft) or (Value = rdRight))
    or ((OldDir = rdLeft) or (OldDir = rdRight)) and ((Value = rdTop) or (Value = rdBottom)) then
    begin
      Dim := Point(Width, Height);
      Width := Dim.Y;
      Height := Dim.X;
    end;
    FDirection := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetUnit(const Value: TRulerUnit);
begin
  if Value <> FUnits then
  begin
    FUnits := Value;
    Invalidate;
  end;
end;


procedure TdtpRsRuler.SetHairLine(const Value: Boolean);
begin
  if Value <> fHairLine then
  begin
    FHairLine := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetHairLinePos(const Value: Integer);
begin
  if Value <> fHairLinePos then
  begin
    DrawHairLine; // erase old position
    FHairLinePos := Value;
    DrawHairLine; // draw new position
  end;
end;

procedure TdtpRsRuler.DrawHairLine;
var
  He: Integer;
begin
  if not FHairLine then
    exit;

  if FHairLinePos <> -1 then
  begin
    Canvas.Pen.Mode := pmNotXOr;
    SelectObject(Canvas.Handle, NormFont);
    He := Canvas.TextHeight('0') + 6;
    SelectObject(Canvas.Handle, OldFont);
    if FDirection = rdTop then
    begin
      if FHairLineStyle = hlsLine then
        InvertRect(Canvas.Handle, Rect(FHairLinePos - 1, Height - He - 1, FHairLinePos, Height - 8))
      else
        InvertRect(Canvas.Handle, Rect(1, Height - He - 1, FHairLinePos, Height - 8));
    end;
    if FDirection = rdBottom then
    begin
      if FHairLineStyle = hlsLine then
        InvertRect(Canvas.Handle, Rect(FHairLinePos - 1, 8, FHairLinePos, He))
      else
        InvertRect(Canvas.Handle, Rect(1, 8, FHairLinePos, He + 1));
    end;
    if FDirection = rdLeft then
    begin
      if FHairLineStyle = hlsLine then
        InvertRect(Canvas.Handle, Rect(Width - He, FHairLinePos - 1, Width - 8, FHairLinePos))
      else
        InvertRect(Canvas.Handle, Rect(Width - He, 1, Width - 8, FHairLinePos));
    end;
    if FDirection = rdRight then
    begin
      if FHairLineStyle = hlsLine then
        InvertRect(Canvas.Handle, Rect(8, FHairLinePos - 1, He, FHairLinePos))
      else
        InvertRect(Canvas.Handle, Rect(8, 1, He, FHairLinePos));
    end;
    Canvas.Pen.Mode := pmCopy;
  end;
end;

procedure TdtpRsRuler.SetHairLineStyle(const Value: THairLineStyle);
begin
  if Value <> FHairLineStyle then
  begin
    FHairLineStyle := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetOffset(const Value: Double);
begin
  if Value <> FOffset then
  begin
    FOffset := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetShowMinus(const Value: Boolean);
begin
  if Value <> FShowMinus then
  begin
    FShowMinus := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetScreenDpm(const Value: double);
begin
  if FScreenDpm <> Value then
  begin
    FScreenDpm := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRuler.SetScale(const Value: double);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    Invalidate;
  end;
end;

{ TdtpRsRulerCorner }

constructor TdtpRsRulerCorner.Create(AOwner: TComponent);
begin
  inherited;
  FPosition := cpLeftTop;
  FUStr := cRulerUnitNames[ruCenti];
  Width := 24;
  Height := 24;
  Hint := '';
end;

procedure TdtpRsRulerCorner.Paint;
var
  Wi, He, d: Integer;
  R: TRect;
begin
  inherited;
  R := ClientRect;
  SelectObject(Canvas.Handle, NormFont);

  if not Flat then
    DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT);

  Canvas.Brush.Color := FScaleColor;
  He := Canvas.TextHeight('0') + 6;
  SetBKMode(Canvas.Handle, TRANSPARENT);
  Canvas.Font.Color := Font.Color;

  Wi := Canvas.TextWidth(FUStr);
  d := 2 - Integer(Flat);

  if FPosition = cpLeftTop then
  begin
    Canvas.FillRect(Rect(Width - He, Height - He - 1, Width - d, Height - 8));
    Canvas.FillRect(Rect(Width - He, Height - He, Width - 8, Height - d));
    Canvas.TextOut(Width - He + 1 + (He - 2 - Wi) div 2, Height - He - 1, fUStr);
  end;

  if FPosition = cpRightTop then
  begin
    Canvas.FillRect(Rect(d, Height - He - 1, He, Height - 8));
    Canvas.FillRect(Rect(8, Height - He, He, Height - d));
    Canvas.TextOut(2 + (He - Wi) div 2, Height - He, FUStr);
  end;

  if FPosition = cpLeftBottom then
  begin
    Canvas.FillRect(Rect(Width - He, 8, Width - d, He + 1));
    Canvas.FillRect(Rect(Width - He, d, Width - 8, He));
    Canvas.TextOut(Width - He + 1 + (He - 2 - Wi) div 2, 8, FUStr);
  end;

  if FPosition = cpRightBottom then
  begin
    Canvas.FillRect(Rect(d, 8, He, He + 1));
    Canvas.FillRect(Rect(8, d, He, He));
    Canvas.TextOut(2 + (He - Wi) div 2, 8, FUStr);
  end;

  SetBKMode(Canvas.Handle, OPAQUE);
  SelectObject(Canvas.Handle, OldFont);
end;

procedure TdtpRsRulerCorner.SetPosition(const Value: TCornerPos);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TdtpRsRulerCorner.SetUnit(const Value: TRulerUnit);
begin
  if Value <> FUnits then
  begin
    FUnits := Value;
    FUStr := cRulerUnitNames[FUnits];
    Hint := cRulerUnitHints[FUnits];
    Invalidate;
  end;
end;

end.
