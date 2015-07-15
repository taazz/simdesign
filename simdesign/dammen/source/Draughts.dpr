program Dammen;

uses
  Forms,
  guiMain in 'guiMain.pas' {MainForm},
  DraughtsConfig in 'DraughtsConfig.pas',
  DraughtsEval in 'DraughtsEval.pas',
  GameEval in 'GameEval.pas',
  GameConst in 'GameConst.pas',
  GameMinimax in 'GameMinimax.pas',
  GameCheck in 'GameCheck.pas',
  guiGamePaint in 'guiGamePaint.pas',
  sdPaintHelper in 'sdPaintHelper.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
