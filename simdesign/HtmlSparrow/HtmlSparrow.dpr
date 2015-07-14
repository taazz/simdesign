program HtmlSparrow;

uses
  Forms,
  HtmlSparrowMain in 'HtmlSparrowMain.pas' {frmHtmlSparrow};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmHtmlSparrow, frmHtmlSparrow);
  Application.Run;
end.
