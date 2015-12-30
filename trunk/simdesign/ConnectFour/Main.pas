unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, Pyro, pgScene, pgSceneViewer, pgCustomView, {pgTypes,}
  {pgAntiAliasing,} pgCanvasUsingPyro, {pgGraphic,} {pgText,} sdConnectFour, pgColor,
  {pgViewport,} {pgShape,} Math;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    sbMain: TStatusBar;
    mnuGame: TMenuItem;
    mnuPlayerPlayer: TMenuItem;
    mnuComputerPlayer: TMenuItem;
    mnuPlayerComputer: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FScene: TpgScene;
    FViewer: TpgSceneViewer;
    FPanel: TpgCustomView;
    FGame: TsdGame;
    FEvaluating: boolean;
    FFieldShapes: array[0..cColCount * cRowCount - 1] of TpgCircle;
    FPlayer1Shape: TpgCircle;
    FPlayer2Shape: TpgCircle;
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForceRedraw;
  public
    { Public declarations }
    procedure BuildScene;
    procedure UpdateScene;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FGame := TsdGame.Create;

  FScene := TpgScene.Create(Self);

  // Create a scene viewer, this is also a non-visual component
  // The engine package provides a more mature scene viewer, that does support interaction
  // like clicking in the document window.
  FViewer := TpgSceneViewer.Create(Self);

  // Add a custom view to the form, this is a TpgCustomControl descendant
  FPanel := TpgCustomView.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alClient;
  FPanel.OnMouseDown := PanelMouseDown;

  // Use our Pyro canvas
  FPanel.CanvasType := ctPyro;

  // Connect the scene to the viewer
  FViewer.Scene := FScene;

  // The provider that will draw the panel is the viewer
  FPanel.Provider := FViewer;

  // Set AA quality to 4 (256 levels)
  pgSetAntiAliasing(4);

  BuildScene;
  ForceRedraw;

end;

procedure TfrmMain.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Column: integer;
  CW: double;
begin
  if FEvaluating then exit;
  // Determine which column was clicked
  CW := (FPanel.Width - 30) / 7;
  Column := trunc((X - 15) / CW);

  sbMain.SimpleText := Format('%d', [Column]);

  FGame.CurrentPose[3] := ftPlayer1;
  FGame.CurrentPose[4] := ftPlayer2;

  UpdateScene;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.BuildScene;
var
  W, H, i, j: integer;
  Parent: TpgGraphic;
  T: TpgText;
  R: TpgRectangle;
  C: TpgCircle;
  V: TpgViewport;
  Xw, Yw, Rd: double;
begin
  FScene.Clear;
  W := FPanel.Width;
  H := FPanel.Height;
  V := FScene.Viewport;
  V.Width.FloatValue := W;
  V.Height.FloatValue := H;
  Parent := FScene.Viewport;
  // left player 1
  T := TpgText.CreateParent(FScene, Parent);
  T.Text.StringValue := FGame.Player1.Name;
  T.FontFamily.Value := 'Verdana';
  T.FontSize.FloatValue := 20;
  T.Fill.AsColor32 := clBlack32;
  T.X.Add(30, luNone);
  T.Y.Add(20, luNone);
  FPlayer1Shape := TpgCircle.CreateParent(FScene, Parent);
  FPlayer1Shape.Cx.FloatValue := 18;
  FPlayer1Shape.Cy.FloatValue := 13;
  FPlayer1Shape.R.FloatValue := 10;
  FPlayer1Shape.Fill.AsColor32 := clLightGray32;


  // right player 2
  T := TpgText.CreateParent(FScene, Parent);
  T.Text.StringValue := FGame.Player2.Name;
  T.FontFamily.Value := 'Verdana';
  T.FontSize.FloatValue := 20;
  T.Fill.AsColor32 := clBlack32;
  T.X.Add(W - 90, luPerc);
  T.Y.Add(20, luPerc);
  FPlayer2Shape := TpgCircle.CreateParent(FScene, Parent);
  FPlayer2Shape.Cx.FloatValue := W - 102;
  FPlayer2Shape.Cy.FloatValue := 13;
  FPlayer2Shape.R.FloatValue := 10;
  FPlayer2Shape.Fill.AsColor32 := clLightGray32;

  // Board square
  R := TpgRectangle.CreateParent(FScene, Parent);
  R.X.FloatValue := 10;
  R.Y.FloatValue := 30;
  R.Width.FloatValue := W - 20;
  R.Height.FloatValue := H - 40;
  R.Stroke.AsColor32 := $FF0000D0;
  R.StrokeWidth.FloatValue := 3;
  R.Fill.AsColor32 := clBlue32;

  // Board pieces
  Xw := (R.Width.FloatValue - 10) / cColCount;
  Yw := (R.Height.FloatValue - 10) / cRowCount;
  Rd := Min(Xw, Yw) * 0.5 - 5;
  for i := 0 to cColCount - 1 do
    for j := 0 to cRowCount - 1 do
    begin
      C := TpgCircle.CreateParent(FScene, Parent);
      FFieldShapes[j * cColCount + i] := C;
      C.Cx.FloatValue := R.X.FloatValue + 5 + (i + 0.5) * Xw;
      C.Cy.FloatValue := R.Y.FloatValue + 5 + (j + 0.5) * Yw;
      C.R.FloatValue := Rd;
      C.Fill.AsColor32 := clWhite32;
    end;

end;

procedure TfrmMain.ForceRedraw;
begin
  FPanel.Invalidate;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGame);
end;

procedure TfrmMain.UpdateScene;
var
  i: integer;
begin
  for i := 0 to cRowCount * cColCount - 1 do
  begin
    case FGame.CurrentPose[i] of
    ftNone: FFieldShapes[i].Fill.AsColor32 := clWhite32;
    ftPlayer1: FFieldShapes[i].Fill.AsColor32 := clRed32;
    ftPlayer2: FFieldShapes[i].Fill.AsColor32 := clYellow32;
    end;
  end;
  if FGame.CurrentPlayer = FGame.Player1 then
  begin
    FPlayer1Shape.Fill.AsColor32 := clRed32;
    FPlayer2Shape.Fill.AsColor32 := clLightGray32;
  end else
  begin
    FPlayer1Shape.Fill.AsColor32 := clLightGray32;
    FPlayer2Shape.Fill.AsColor32 := clYellow32;
  end;
  FPanel.Invalidate;
end;

end.
