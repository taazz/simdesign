program MailBox;

uses
  Forms,
  MailBoxMain in 'MailBoxMain.pas' {frmMailbox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMailbox, frmMailbox);
  Application.Run;
end.
