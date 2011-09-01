{
  Unit sdVirtualScrollbox

  TsdVirtualScrollbox is a TCustomControl descendant just like TScrollBox but it will
  not create a bigger canvas than the normal client size. Scrolling is based on
  the parameters ScrollWidth, ScrollHeight (the "virtual area") and ScrollLeft,
  ScrollTop, which form the upperleft corner of the area currently displayed.

  With SetScrollBounds() you can set all bounds at once to avoid flicker.

  With ScrollBy() you can scroll the window by an amount DeltaX, DeltaY. Scrollbar
  positions as well as internal variables are updated, and minima/maxima are checked.

  Projects:
   - DtpDocuments
   - NativeJpg

  Author: Nils Haeck M.Sc.
  Creation Date: 11aug2003
  Version: 1.02

  Modifications:


  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.

  Copyright (c) 2003 SimDesign B.V.
}
unit sdVirtualScrollbox;

interface

uses
  Graphics, Controls, Forms, Classes, Windows, Messages, SysUtils, Math,
  Dialogs, sdDebug;

type

  TsdBoxPlacement = (
    bpCenter,
    bpLeftTop
  );

  TsdVirtualPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  // The TsdVirtualScrollBox is a windowed control that can virtually scroll over its
  // scrollable area, indicated by ScrollWidth and ScrollHeight. The left and top
  // position of the visible part is indicated by ScrollLeft and ScrollTop. Set
  // them all together using SetScrollBounds() in order to avoid flicker
  TsdVirtualScrollBox = class(TCustomControl)
  private
    FAutoScroll: Boolean;    // If set, the control will automatically scroll
    FBorderStyle: TBorderStyle; // Border style for this scrollbox (bsNone or bsSingle)
    FBoxPlacement: TsdBoxPlacement; // Default placement when scrollbox is smaller than client
    FScrollLeft: integer;    // Left position of scroll window on virtual window
    FScrollTop: integer;     // Top position of scroll window on virtual window
    FScrollWidth: integer;   // Total width of scrollable area
    FScrollHeight: integer;  // Total height of scrollable area
    FScrollScale: single;    // Scale on scrolling in case Width or Height > 32767 (handled automatically)
    FTracking: boolean;      // If set (default), the window updates immediately when scrollbars are moved
    FIncrement: integer;     // Increment (in pixels) when arrows on scrollbar are clicked
    FOnUpdateScrollPosition: TNotifyEvent;
    FOnPaint: TsdVirtualPaintEvent;
    FOnDebugOut: TsdDebugEvent;
    procedure SetAutoScroll(const Value: Boolean);
    procedure ScrollMessage(const AMessage: TWMHScroll; ACode: word; var APos: integer; ASize, AClient: integer);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetBorderStyle(const Value: TBorderStyle);
    function CalculateThumbPosition(const Requested, Size, Client: integer): integer;
    procedure SetBoxPlacement(const Value: TsdBoxPlacement);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure RemoveScrollbars;
    procedure UpdateScrollbars;
    procedure UpdateScrollPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollBy(DeltaX, DeltaY: integer); virtual;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
    // Use ClientToBox to determine the position of mouse coordinates X and Y in
    // box coordinates. If the mouse is outside the box, the function returns False.
    function ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
    procedure SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BoxPlacement: TsdBoxPlacement read FBoxPlacement write SetBoxPlacement default bpCenter;
    property Increment: integer read FIncrement write FIncrement default 8;
    property ScrollLeft: integer read FScrollLeft;
    property ScrollTop: integer read FScrollTop;
    property ScrollWidth: integer read FScrollWidth;
    property ScrollHeight: integer read FScrollHeight;
    property Tracking: boolean read FTracking write FTracking default True;
    // Event OnUpdateScrollPosition is fired whenever the user has scrolled.
    property OnUpdateScrollPosition: TNotifyEvent read FOnUpdateScrollPosition write FOnUpdateScrollPosition;
    property OnPaint: TsdVirtualPaintEvent read FOnPaint write FOnPaint;
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

implementation

{ TsdVirtualScrollBox }

function TsdVirtualScrollBox.CalculateThumbPosition(const Requested, Size, Client: integer): integer;
var
  OverShoot: integer;
begin
  if FBoxPlacement = bpCenter then
  begin
    if Size = 0 then
      OverShoot := 0
    else
      OverShoot := Max(0, Client - Size);
  end else
    OverShoot := 0;
  Result := Max(-OverShoot div 2, Min(Requested, Size - Client));
end;

function TsdVirtualScrollBox.ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
begin
  BoxX := 0;
  BoxY := 0;
  Result := False;
  if (FScrollWidth <= 0) or (FScrollHeight <= 0) then
    exit;
  BoxX := X + FScrollLeft;
  BoxY := Y + FScrollTop;
  if (BoxX >= 0) and (BoxX < FScrollWidth) and (BoxY >= 0) and (BoxY < FScrollHeight) then
    Result := True;
end;

constructor TsdVirtualScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoScroll := True;
  FIncrement := 9;
  FScrollScale  := 1.0;
  FTracking := True;
  Color := clAppWorkspace;
  Width := 150;
  Height := 250;
  FBorderStyle := bsSingle;
end;

procedure TsdVirtualScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    with WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TsdVirtualScrollBox.DoDebugOut(Sender: TObject;
  WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if assigned(FOnDebugOut) then
    FOnDebugOut(Sender, WarnStyle, AMessage);
end;

procedure TsdVirtualScrollBox.Paint;
// Override this method in descendants. Call "inherited" if you want to automatically
// clear the area that is outside of the scrollbox
var
  R, B, C, Dest: TRect;
  // local
  procedure PaintRect;
  begin
    if IsRectEmpty(R) then
      exit;
    IntersectRect(Dest, R, C);
    if IsRectEmpty(Dest) then
      exit;
    Canvas.FillRect(Dest);
  end;
// main
begin
  // Onpaint event
  if assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
  // Paint area around scrolled area (if any is visible)
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  C := Canvas.ClipRect;
  B := Rect(0, 0, FScrollWidth, FScrollHeight);
  OffsetRect(B, -FScrollLeft, -FScrollTop);
  R := Rect(0, 0, ClientWidth, B.Top);
  PaintRect;
  R := Rect(0, B.Top, B.Left, B.Bottom);
  PaintRect;
  R := Rect(B.Right, B.Top, ClientWidth, B.Bottom);
  PaintRect;
  R := Rect(0, B.Bottom, ClientWidth, ClientHeight);
  PaintRect;
end;

procedure TsdVirtualScrollBox.RemoveScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then
    exit;
  // Horizontal scrollbar
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  // Vertical scrollbar
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TsdVirtualScrollBox.ScrollBy(DeltaX, DeltaY: integer);
// Call this procedure to scroll the window and update the scrollbars, all in
// one command
var
  NewX, NewY: integer;
  ThumbPosX, ThumbPosY: integer;
begin
  // Calculate new position in X and Y
  NewX := CalculateThumbPosition(FScrollLeft + DeltaX, FScrollWidth, ClientWidth);
  DeltaX := NewX - FScrollLeft;
  NewY := CalculateThumbPosition(FScrollTop  + DeltaY, FScrollHeight, ClientHeight);
  DeltaY := NewY - FScrollTop;
  if (DeltaX = 0) and (DeltaY = 0) then
    exit; // no changes

  FScrollLeft := NewX;
  FScrollTop  := newY;
  UpdateScrollPosition;

  // Scroll the window
  ScrollWindow(Handle, -DeltaX, -DeltaY, nil, nil);

  // Set scrollbar positions
  ThumbPosX := round(NewX * FScrollScale);
  ThumbPosY := round(NewY * FScrollScale);
  if GetScrollPos(Handle, SB_HORZ) <> ThumbPosX then
    SetScrollPos(Handle, SB_HORZ, ThumbPosX, True);
  if GetScrollPos(Handle, SB_VERT) <> ThumbPosY then
    SetScrollPos(Handle, SB_VERT, ThumbPosY, True);
end;

procedure TsdVirtualScrollBox.ScrollMessage(const AMessage: TWMHScroll; ACode: word;
  var APos: integer; ASize, AClient: integer);
  // local
  procedure SetPosition(NewPos: single);
  var
    ANewPos: single;
    ADelta: integer;
    AIntPos: integer;
  begin
//    DoDebugOut(Self, wsInfo, 'ScrollMessage.SetPosition called');
    // Calculate new position
    ANewPos := Min(Max(0, NewPos), Max(0, ASize - AClient));
    ADelta := round(ANewPos - APos);
    if ADelta = 0 then
      exit; // no changes

    APos := round(ANewPos);
    UpdateScrollPosition;

    // Scroll the window
    case ACode of
    SB_HORZ: ScrollWindow(Handle, -ADelta, 0, nil, nil);
    SB_VERT: ScrollWindow(Handle, 0, -ADelta, nil, nil);
    end;//case

    // Set scrollbar position
    AIntPos := round(NewPos * FScrollScale);
    if GetScrollPos(Handle, ACode) <> AIntPos then
      SetScrollPos(Handle, ACode, AIntPos, True);
  end;
// main
begin
  if not AutoScroll then
    exit;
  with AMessage do
  begin
    case ScrollCode of
    SB_LINEUP:        SetPosition(APos - Increment);
    SB_LINEDOWN:      SetPosition(APos + Increment);
    SB_PAGEUP:        SetPosition(APos - AClient);
    SB_PAGEDOWN:      SetPosition(APos + AClient);
    SB_THUMBPOSITION: SetPosition(Pos / FScrollScale);
    SB_THUMBTRACK:    if Tracking then
                        SetPosition(Pos / FScrollScale);
    SB_TOP:           SetPosition(0);
    SB_BOTTOM:        SetPosition(ASize - AClient);
    SB_ENDSCROLL:     ;
    end;//case
  end;
end;

procedure TsdVirtualScrollBox.SetAutoScroll(const Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then
      UpdateScrollBars
    else
    begin
      RemoveScrollbars;
      FScrollLeft := 0;
      FScrollTop  := 0;
    end;
  end;
end;

procedure TsdVirtualScrollBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TsdVirtualScrollBox.SetBoxPlacement(
  const Value: TsdBoxPlacement);
begin
  if FBoxPlacement <> Value then
  begin
    FBoxPlacement := Value;
    UpdateScrollBars;
  end;
end;

procedure TsdVirtualScrollBox.SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) or
     (FScrollWidth <> AWidth) or (FScrollHeight <> AHeight) then
  begin
    if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) then
    begin
      FScrollLeft   := ALeft;
      FScrollTop    := ATop;
      UpdateScrollPosition;
    end;
    FScrollWidth  := AWidth;
    FScrollHeight := AHeight;
    UpdateScrollbars;
  end;
end;

procedure TsdVirtualScrollBox.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
  AMax: integer;
  AScrollLeft, AScrollTop: integer;
begin
  if not HandleAllocated then
    exit;
  // Adjust scale
  AMax := Max(FScrollWidth, FScrollHeight);
  if AMax > 30000 then
    FScrollScale := 30000 / AMax
  else
    FScrollScale := 1.0;

  // Check limits on Pos
  AScrollLeft := CalculateThumbPosition(FScrollLeft, FScrollWidth, ClientWidth);
  AScrollTop := CalculateThumbPosition(FScrollTop, FScrollHeight, ClientHeight);
  if (AScrollLeft <> FScrollLeft) or (AScrollTop <> FScrollTop) then
  begin
    FScrollLeft := AScrollLeft;
    FScrollTop  := AScrollTop;
    UpdateScrollPosition;
    // We need an extra invalidate here, the standard WinControl seems to
    // forget this case
    Invalidate;
  end;
  if not AutoScroll then
    exit;

  // Horizontal scrollbar
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollWidth * FScrollScale);
  ScrollInfo.nPage := round(ClientWidth  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollLeft  * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  // Vertical scrollbar
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollHeight * FScrollScale);
  ScrollInfo.nPage := round(ClientHeight  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollTop    * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TsdVirtualScrollBox.UpdateScrollPosition;
// Override in descendants to update a label that displays scroll position etc
begin
  // Default fires event
  if assigned(FOnUpdateScrollPosition) then
    FOnUpdateScrollPosition(Self);
end;

procedure TsdVirtualScrollBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// This message handler is called when windows is about to work on the background
// of the window, and this procedure signals not to "erase" (or fill) it, to
// avoid flicker
begin
  // No automatic erase of background
  Message.Result := LRESULT(False);
end;

procedure TsdVirtualScrollBox.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(Message, SB_HORZ, FScrollLeft, FScrollWidth, ClientWidth);
end;

procedure TsdVirtualScrollBox.WMSize(var Message: TWMSize);
// React to a resize
begin
  // use the info to update the scrollbars
  UpdateScrollbars;
  // and call inherited method
  inherited;
end;

procedure TsdVirtualScrollBox.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message, SB_VERT, FScrollTop, FScrollHeight, ClientHeight);
end;

end.

