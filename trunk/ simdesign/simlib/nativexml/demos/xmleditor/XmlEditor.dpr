program XmlEditor;

uses
  FastMM,
  Forms,
  XmlEditorMain in 'XmlEditorMain.pas' {frmMain},
  sdXmlOutputOptionsDlg in 'sdXmlOutputOptionsDlg.pas' {XmlOutputOptionsDlg},
  NativeXml in '..\..\NativeXml.pas',
  NativeXmlObjectStorage in '..\..\NativeXmlObjectStorage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TXmlOutputOptionsDlg, XmlOutputOptionsDlg);
  Application.Run;
end.
