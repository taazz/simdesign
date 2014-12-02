program XmlEditor;

uses
  //FastMM,
  Forms,
  XmlEditorMain in 'XmlEditorMain.pas' {frmMain};
{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //Application.CreateForm(TXmlOutputOptionsDlg, XmlOutputOptionsDlg);
  Application.Run;
end.
