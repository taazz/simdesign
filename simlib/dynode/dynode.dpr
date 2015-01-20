program dynode;

uses
  Forms,
  dnMain in 'dnMain.pas' {frmMain},
  frmProjectOptions in 'frmProjectOptions.pas' {dlgProjectOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdlgProjectOptions, dlgProjectOptions);
  Application.Run;
end.
