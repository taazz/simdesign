program XmlEditor;

uses
  FastMM,
  Forms,
  XmlEditorMain in 'XmlEditorMain.pas' {frmMain},
  sdXmlOutputOptionsDlg in 'sdXmlOutputOptionsDlg.pas' {XmlOutputOptionsDlg},
  SimpleXmlObjectStorage in '..\..\SimpleXmlObjectStorage.pas',
  SimpleXml in '..\..\SimpleXml.pas',
  SimpleXmlCodepages in '..\..\SimpleXmlCodepages.pas',
  SimpleXmlNodes in '..\..\SimpleXmlNodes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TXmlOutputOptionsDlg, XmlOutputOptionsDlg);
  Application.Run;
end.
