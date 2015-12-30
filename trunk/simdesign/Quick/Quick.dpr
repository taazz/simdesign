program Quick;

uses
  //FastMM,
  Forms,
  FormMain in 'FormMain.pas' {frmMain},
  FrameSelectColor in 'FrameSelectColor.pas' {frSelectColor: TFrame},
  FrameToolWin in 'FrameToolWin.pas' {frToolWin: TFrame},
  FrameReplaceColor in 'FrameReplaceColor.pas' {frReplaceColor: TFrame},
  FrameThreshold in 'FrameThreshold.pas' {frThreshold: TFrame},
  FrameFloodFill in 'FrameFloodFill.pas' {frFloodFill: TFrame},
  FrameRenderOptions in 'FrameRenderOptions.pas' {frRenderOptions: TFrame},
  pgFrameRulerWindow in '..\..\Source\GUI\pgFrameRulerWindow.pas' {frRulerWindow: TFrame},
  FrameShapeInfo in 'FrameShapeInfo.pas' {frShapeInfo: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
