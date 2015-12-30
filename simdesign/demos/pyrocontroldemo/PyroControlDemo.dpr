program PyroControlDemo;

uses
  //FastMM,
  Forms,
  DemoMain in 'DemoMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
