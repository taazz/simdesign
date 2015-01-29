program projProcessRunner;

uses
  Forms,
  testCL in 'testCL.pas' {Form1},
  dnProcessRunner in 'dnProcessRunner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
