{ program xmltest - a simple NativeXml tester }
program xmltest;
uses
  Forms,
  xmltestmain in 'xmltestmain.pas' {frmMain},
  NativeXml in '..\..\NativeXml.pas',
  ElAES in '..\..\..\..\extlib\filters\eldos_aes\ElAES.pas';
  
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
