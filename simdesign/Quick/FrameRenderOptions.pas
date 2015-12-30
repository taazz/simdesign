unit FrameRenderOptions;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameToolWin, StdCtrls, ExtCtrls, pgControlUsingPyro, pgCanvasUsingGDI,
  pgCanvasUsingPyro, Pyro;

type
  TfrRenderOptions = class(TfrToolWin)
    gbRenderEngine: TGroupBox;
    rbBasicRenderer: TRadioButton;
    rbGDIRenderer: TRadioButton;
    rbPyroRenderer: TRadioButton;
    procedure rbRendererClick(Sender: TObject);
  private
    FControl: TpgPyroControl;
    FIsUpdating: boolean;
    procedure SetControl(const Value: TpgPyroControl);
  public
    property Control: TpgPyroControl read FControl write SetControl;
  end;

var
  frRenderOptions: TfrRenderOptions;

implementation

{$R *.dfm}

{ TfrRenderOptions }

procedure TfrRenderOptions.SetControl(const Value: TpgPyroControl);
begin
  FControl := Value;
  FIsUpdating := True;
  try
    rbBasicRenderer.Checked := FControl.CanvasType = ctGDI;
    rbPyroRenderer.Checked := FControl.CanvasType = ctPyro;
  finally
    FIsUpdating := False;
  end;
end;

procedure TfrRenderOptions.rbRendererClick(Sender: TObject);
begin
  if FIsUpdating then exit;
  if rbBasicRenderer.Checked then FControl.CanvasType := ctGDI;
  if rbPyroRenderer.Checked then FControl.CanvasType := ctPyro;
end;

end.
