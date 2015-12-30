unit FrameThreshold;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Math;

type
  TfrThreshold = class(TFrame)
    gbThreshold: TGroupBox;
    lbThreshold: TLabel;
    tbThreshold: TTrackBar;
    lbValue: TLabel;
    udThreshold: TUpDown;
    procedure tbThresholdChange(Sender: TObject);
    procedure udThresholdClick(Sender: TObject; Button: TUDBtnType);
  private
    FThreshold: integer;
    FIsUpdating: boolean;
    FThresholdMax: integer;
    procedure SetThreshold(const Value: integer);
    procedure UpdateControls;
    procedure SetThresholdMax(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Threshold: integer read FThreshold write SetThreshold default 5;
    property ThresholdMax: integer read FThresholdMax write SetThresholdMax default 50;
  end;

implementation

{$R *.dfm}

{ TfrThreshold }

constructor TfrThreshold.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreshold := 5;
  FThresholdMax := 100;
  UpdateControls;
end;

procedure TfrThreshold.SetThreshold(const Value: integer);
begin
  if FThreshold <> Value then begin
    FThreshold := Max(0, Min(FThresholdMax, Value));
    UpdateControls;
  end;
end;

procedure TfrThreshold.SetThresholdMax(const Value: integer);
begin
  if FThresholdMax <> Value then begin
    FThresholdMax := Value;
    UpdateControls;
  end;
end;

procedure TfrThreshold.UpdateControls;
begin
  FIsUpdating := True;
  try
    lbThreshold.Caption := IntToStr(FThreshold);
    tbThreshold.Max := FThresholdMax;
    tbThreshold.Position := FThreshold;
    udThreshold.Max := FThresholdMax;
    udThreshold.Position := FThreshold;
  finally
    FIsUpdating := False;
  end;
end;

procedure TfrThreshold.tbThresholdChange(Sender: TObject);
begin
  if FIsUpdating then exit;
  Threshold := tbThreshold.Position;
end;

procedure TfrThreshold.udThresholdClick(Sender: TObject;
  Button: TUDBtnType);
begin
  if FIsUpdating then exit;
  case Button of
  btPrev: Threshold := Threshold - 1;
  btNext: Threshold := Threshold + 1;
  end;
end;

end.
