unit FrameShapeInfo;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameToolWin, StdCtrls, ExtCtrls, pgScene, pgDocument,
  ComCtrls, FrameSelectColor, Pyro;

type

  TfrShapeInfo = class(TfrToolWin)
    pcShape: TPageControl;
    tsInfo: TTabSheet;
    tsPaint: TTabSheet;
    frFillColor: TfrSelectColor;
    frStrokeColor: TfrSelectColor;
    Label1: TLabel;
    lbShapeId: TLabel;
    Label2: TLabel;
    lbShapeName: TLabel;
    rgFill: TRadioGroup;
    rgStroke: TRadioGroup;
    Label3: TLabel;
    edStrokeWidth: TEdit;
  private
    FScene: TpgScene;
    FShape: TpgPaintable;
    procedure SetScene(const Value: TpgScene);
    procedure SetShape(Value: TpgPaintable);
    procedure UpdateShape;
    procedure UpdatePaint(APaint: TpgPaintProp; AFrame: TfrSelectColor);
    procedure SetColor(Sender: TObject);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
    property Scene: TpgScene read FScene write SetScene;
    property Shape: TpgPaintable read FShape write SetShape;
  end;

var
  frShapeInfo: TfrShapeInfo;

implementation

{$R *.dfm}

{ TfrShapeInfo }

constructor TfrShapeInfo.Create(AOwner: TComponent);
begin
  inherited;
  frFillColor.OnPickColor := SetColor;
  frStrokeColor.OnPickColor := SetColor;
end;

procedure TfrShapeInfo.SetColor(Sender: TObject);
begin
  if not assigned(FShape) then exit;
  if Sender = frFillColor then
    FShape.Fill.AsColor32 := frFillColor.SelectColor32;
  if Sender = frStrokeColor then
    FShape.Stroke.AsColor32 := frStrokeColor.SelectColor32;
end;

procedure TfrShapeInfo.SetScene(const Value: TpgScene);
begin
  if FScene <> Value then
  begin
    SetShape(nil);
    FScene := Value;
  end;
end;

procedure TfrShapeInfo.SetShape(Value: TpgPaintable);
var
  Element: TpgElement;
begin
  if FShape <> Value then
  begin
    if assigned(FScene) then
    begin
      Element := Value;
      if Element is TpgPaintable then
        FShape := TpgPaintable(Element);
    end;
    UpdateShape;
  end;
end;

procedure TfrShapeInfo.UpdatePaint(APaint: TpgPaintProp; AFrame: TfrSelectColor);
begin
  if APaint.PaintStyle = psColor then begin
    AFrame.Visible := True;
    AFrame.SelectColor32 := APaint.AsColor32;
  end else
    AFrame.Visible := False;
end;

procedure TfrShapeInfo.UpdateShape;
begin
  if assigned(FShape) then
  begin
    Title := copy(FShape.ClassName, 4, length(FShape.ClassName));
    lbShapeId.Caption := FShape.ID;;
    lbShapeName.Caption := FShape.Name;
    UpdatePaint(FShape.Fill, frFillColor);
    UpdatePaint(FShape.Stroke, frStrokeColor);
  end else begin
    Title := '';
  end;
end;

end.
