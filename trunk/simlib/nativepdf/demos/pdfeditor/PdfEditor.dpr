program PdfEditor;



uses
  Forms,
  frmPdfEditor in 'frmPdfEditor.pas' {Form1},
  frmPdfAsOcx in 'frmPdfAsOcx.pas' {fmAsOCX};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
