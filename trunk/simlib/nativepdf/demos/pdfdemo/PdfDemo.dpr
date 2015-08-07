program PdfDemo;

uses
  Forms,
  PdfDemoMain in 'PdfDemoMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
