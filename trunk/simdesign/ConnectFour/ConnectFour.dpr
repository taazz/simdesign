program ConnectFour;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  sdConnectFour in 'sdConnectFour.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
