program XmlEditor;

uses
  FastMM,
  Forms,
  XmlEditorMain in 'XmlEditorMain.pas' {frmMain},
  sdXmlOutputOptionsDlg in 'sdXmlOutputOptionsDlg.pas' {XmlOutputOptionsDlg},
  NativeXmlObjectStorage in '..\..\NativeXmlObjectStorage.pas',
  NativeXml in '..\..\NativeXml.pas',
  NativeXmlCodepages in '..\..\NativeXmlCodepages.pas',
  NativeXmlNodes in '..\..\NativeXmlNodes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TXmlOutputOptionsDlg, XmlOutputOptionsDlg);
  Application.Run;
end.
