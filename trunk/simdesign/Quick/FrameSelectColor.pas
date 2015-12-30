unit FrameSelectColor;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Math, Pyro, pgWinGDI;

type

  TfrSelectColor = class(TFrame)
    gbTitle: TGroupBox;
    pnlColor: TPanel;
    btnPick: TButton;
    btnSelect: TButton;
    lbOpacityTitle: TLabel;
    tbOpacity: TTrackBar;
    lbOpacity: TLabel;
    udOpacity: TUpDown;
    pbColor: TPaintBox;
    procedure udOpacityClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOpacityChange(Sender: TObject);
    procedure btnPickClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure pbColorPaint(Sender: TObject);
  private
    FSelectColor: TColor;
    FSelectOpacity: integer;
    FIsUpdating: boolean;
    FOnPickColor: TNotifyEvent;
    FColorDlg: TColorDialog;
    FOpacityEnabled: boolean;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure SetSelectColor(const Value: TColor);
    procedure UpdateControls;
    procedure SetSelectOpacity(const Value: integer);
    procedure SetOpacityEnabled(const Value: boolean);
    function GetSelectColor32: TpgColor32;
    procedure SetSelectColor32(const Value: TpgColor32);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property OnPickColor: TNotifyEvent read FOnPickColor write FOnPickColor;
  published
    property Title: string read GetTitle write SetTitle;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clWhite;
    property SelectOpacity: integer read FSelectOpacity write SetSelectOpacity default 255;
    property SelectColor32: TpgColor32 read GetSelectColor32 write SetSelectColor32;
    property OpacityEnabled: boolean read FOpacityEnabled write SetOpacityEnabled default true;
  end;

implementation

{$R *.dfm}

{ TfrSelectColor }

constructor TfrSelectColor.Create(AOwner: TComponent);
begin
  inherited;
  FSelectColor := clWhite;
  FSelectOpacity := 255;
  FOpacityEnabled := True;
  UpdateControls;
end;

function TfrSelectColor.GetTitle: string;
begin
  Result := gbTitle.Caption;
end;

procedure TfrSelectColor.SetSelectColor(const Value: TColor);
begin
  if FSelectColor <> Value then begin
    FSelectColor := Value;
    UpdateControls;
  end;
end;

procedure TfrSelectColor.SetSelectOpacity(const Value: integer);
begin
  if FSelectOpacity <> Value then begin
    FSelectOpacity := Max(0, Min(255, Value));
    UpdateControls;
  end;
end;

procedure TfrSelectColor.SetTitle(const Value: string);
begin
  gbTitle.Caption := Value;
end;

procedure TfrSelectColor.SetOpacityEnabled(const Value: boolean);
begin
  if FOpacityEnabled <> Value then begin
    FOpacityEnabled := Value;
    UpdateControls;
  end;
end;

procedure TfrSelectColor.UpdateControls;
begin
  FIsUpdating := True;
  try
    pnlColor.Color := FSelectColor;
    lbOpacityTitle.Visible := FOpacityEnabled;
    lbOpacity.Visible := FOpacityEnabled;
    tbOpacity.Visible := FOpacityEnabled;
    udOpacity.Visible := FOpacityEnabled;
    tbOpacity.Position := FSelectOpacity;
    udOpacity.Min := 0;
    udOpacity.Max := 255;
    udOpacity.Position := FSelectOpacity;
    lbOpacity.Caption := IntToStr(FSelectOpacity);
    pbColor.Invalidate;
  finally
    FIsUpdating := False;
  end;
end;

procedure TfrSelectColor.udOpacityClick(Sender: TObject;
  Button: TUDBtnType);
begin
  if FIsUpdating then exit;
  case Button of
  btPrev: SelectOpacity := SelectOpacity - 1;
  btNext: SelectOpacity := SelectOpacity + 1;
  end;
end;

procedure TfrSelectColor.tbOpacityChange(Sender: TObject);
begin
  if FIsUpdating then exit;
  SelectOpacity := tbOpacity.Position;
end;

procedure TfrSelectColor.btnPickClick(Sender: TObject);
begin
  if assigned(FOnPickColor) then
    FOnPickColor(Self);
end;

procedure TfrSelectColor.btnSelectClick(Sender: TObject);
begin
  // Show color selection dialog
  if not assigned(FColorDlg) then
    FColorDlg := TColorDialog.Create(Self);
  FColorDlg.Color := SelectColor;  
  if FColorDlg.Execute then
    SelectColor := FColorDlg.Color;
end;

procedure TfrSelectColor.pbColorPaint(Sender: TObject);
  function MixColors(Col1, Col2, Opacity: integer): TColor;
  var
    P1, P2, R: pbyte;
    i: integer;
  begin
    P1 := @Col1;
    P2 := @Col2;
    R := @Result;
    for i := 0 to 2 do begin
      R^ := (P1^ * Opacity + P2^ * (255 - Opacity)) div 255;
      inc(R);
      inc(P1);
      inc(P2);
    end;
    R^ := 0;
  end;
const
  cSquareWidth = 8;
var
  Cols: array[0..1] of TColor;
  x, y: integer;
begin
  Cols[0] := MixColors(FSelectColor, ColorToRgb(clWhite), FSelectOpacity);
  Cols[1] := MixColors(FSelectColor, ColorToRgb(clLtGray), FSelectOpacity);
  pbColor.Canvas.Brush.Style := bsSolid;
  for y := 0 to (pbColor.Height + cSquareWidth - 1) div cSquareWidth - 1 do
    for x := 0 to (pbColor.Width + cSquareWidth - 1) div cSquareWidth - 1 do
    begin
      pbColor.Canvas.Brush.Color := Cols[(x + y) mod 2];
      pbColor.Canvas.FillRect(Rect(
       x * cSquareWidth, y * cSquareWidth,
       x * cSquareWidth + cSquareWidth, y * cSquareWidth + cSquareWidth));
    end;
end;

function TfrSelectColor.GetSelectColor32: TpgColor32;
begin
  Result := GDIToColor32(SelectColor, SelectOpacity);
end;

procedure TfrSelectColor.SetSelectColor32(const Value: TpgColor32);
var
  Alpha: byte;
  Color: TColor;
begin
  Color32ToGDI(Value, Alpha, Color);
  SelectColor := Color;
  SelectOpacity := Alpha;
end;

end.
