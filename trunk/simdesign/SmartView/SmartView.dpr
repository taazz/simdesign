program SmartView;

uses
  Forms,
  SmartViewMain in 'SmartViewMain.pas' {frmSmartview};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSmartview, frmSmartview);
  Application.Run;
end.
