program SvgViewer;

uses
  Forms,
  SvgViewerMain in 'SvgViewerMain.pas' {frmMain},
  sdDebug in '..\..\..\general\sdDebug.pas',
  sdStringTable in '..\..\..\general\sdStringTable.pas',
  NativeXml in '..\..\..\nativexml\NativeXml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

