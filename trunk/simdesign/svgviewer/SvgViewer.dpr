program SvgViewer;

uses
  Forms,
  SvgViewerMain in 'SvgViewerMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

