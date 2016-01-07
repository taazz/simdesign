unit DemoMain;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls,
  Forms, Dialogs, Menus, ExtCtrls,

  // pyro
   pgDocument, pgCanvas, pgPyroCanvas, pgGDICanvas, pgColor, pgBitmap, pgRaster,
  pgRasterJpg, pgTransform, {pgPolygonRasterizer,} pgCover, Pyro;

type

  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    File1: TMenuItem;
    mnuAnimate: TMenuItem;
    mnuRun: TMenuItem;
    Label1: TLabel;
    mnuExit: TMenuItem;
    mnuOptions: TMenuItem;
    mnuPyrodrawing: TMenuItem;
    Button1: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuRunClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuPyrodrawingClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ControlPaint(Sender: TObject);
  private
    FMyControl: TpgPyroControl;
    FButton: TButton;
    procedure ButtonClick(Sender: TObject);
  public
    { Public declarations }
    FPyroBitmap: TpgBitmap;
    XPos, YPos: integer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.ButtonClick(Sender: TObject);
var
  Msg: string;
begin
  if Sender is TButton then
    Msg := TButton(Sender).Caption;
  ShowMessage(Format('You clicked the button "%s"!', [Msg]));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Add a pyro control descendant
  FMyControl := TpgPyroControl.Create(Self);
  FMyControl.Align := alClient;
  FMyControl.Parent := Panel1;
  FMyControl.Canvas.InterpolationMethod := imLinear;
  FMyControl.OnPaint := ControlPaint;

  // Add a button on top of the pyro control
  FButton := TButton.Create(Self);
  FButton.Left := 60;
  FButton.Top := 150;
  FButton.Caption := 'Hi there';
  FButton.OnClick := ButtonClick;
  FButton.Parent := FMyControl;

  // For test
  XPos := 10;
  YPos := 10;
  FPyroBitmap := TpgBitmap.Create;

  //LoadImageFromFile('c:\temp\test.jpg', FMyControl.FBmp);
  LoadImageFromFile('c:\tortoise\source\simlib\formats2d\testfiles\jpg\sas.jpg', FPyroBitmap);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FPyroBitmap.Free;
end;

procedure TfrmMain.mnuRunClick(Sender: TObject);
var
  i: integer;
  R: TpgRect;
begin
  FMyControl.Invalidate;
  for i := 0 to 197 do
  begin
    XPos := 10 + i;
    YPos := 10 + i;
    R := pgRect(7 + i, 7 + i, 73 + i, 63 + i);
    //pgInvalidateRect(FMyControl.Handle, @R, true);
    FMyControl.Invalidate;
    Application.ProcessMessages;
  end;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuPyrodrawingClick(Sender: TObject);
begin
  if mnuPyroDrawing.Checked then
    FMyControl.CanvasType := ctPyro
  else
    FMyControl.CanvasType := ctGDI;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  R: TpgRasterizer;
  P: array[0..3] of TpgPoint;
  C: TpgCover;
begin
  R := TpgRasterizer.Create;
  R.SetSize(10, 10);
  P[0] := pgPoint(0,  0);
  P[1] := pgPoint(10, 0);
  P[2] := pgPoint(0, 10);
  P[3] := pgPoint(10, 10);
  C := TpgCover.Create;
  C.SetSize(10, 10);
  R.Cover := C;
  C.Free;
  //R.RasterizePolygon(@P[0], 4);
  R.Free;
end;

procedure TfrmMain.ControlPaint(Sender: TObject);
var
  L: TpgLayer;
  State: TpgState;
  Stroke: TpgStroke;
  Fill: TpgFill;
  Font: TpgFont;
begin
  // overloaded function without guid
  L := FMyControl.Canvas.PushLayer;
  try
    if not L.IsCached then
    begin
      L.Canvas.Translate(-150, -200);
      //L.Canvas.ClipEllipse(300,400,250,120);
      L.Canvas.Rotate(XPos, 300, 400);
      if assigned(FPyroBitmap) then
      begin
        Fill := L.Canvas.NewFill;
        Fill.Reference := FPyroBitmap;
        Fill.Opacity := 0.8;
        L.Canvas.PaintRectangle(150, 250, 300, 300, 100, 100, Fill, nil);

        //L.Canvas.PaintBitmap(FPyroBitmap, 0, 0, FPyroBitmap.Width, FPyroBitmap.Height);
      end;
    end;
  finally
    // At this moment, the layer gets drawn
    FMyControl.Canvas.PopLayer(L);
  end;
  Stroke := FMyControl.Canvas.NewStroke;
  Fill := FMyControl.Canvas.NewFill;
  Font := FMyControl.Canvas.NewFont('verdana', fsNormal, fwBold);
  Stroke.Color := clBlack32;
  Stroke.Width := 3;
  Stroke.Dashes[0] := 20;
  FMyControl.Canvas.PaintLine(5, 5, 200, 200, Stroke);
  Stroke.ClearDashes;
  FMyControl.Canvas.PaintLine(-5, 10, 15, 400, Stroke);
  Stroke.Color := clBlue32;
  Fill.Color := clRed32;// and $7FFFFFFF;
  Fill.Opacity := 0.7;
  FMyControl.Canvas.Translate(XPos, YPos);
  State := FMyControl.Canvas.Push;
  try
    //FMyControl.Canvas.ClipEllipse(30, 25, 20, 40);
    FMyControl.Canvas.PaintRectangle(0, 0, 150, 60, 10, 0, Fill, nil);
    Fill.Color := clBlue32;
    Fill.Opacity := 0.7;
    FMyControl.Canvas.PaintText(5, 10, 'Hi there!', Font, 10, Fill, nil);
    FMyControl.Canvas.PaintText(5, 30, 'You all..', Font, 24, Fill, nil);
    Fill.Opacity := 1.0;
    FMyControl.Canvas.PaintText(5, 50, 'See ya!', Font, 24, Fill, nil);
  finally
    FMyControl.Canvas.Pop(State);
  end;
end;

end.
