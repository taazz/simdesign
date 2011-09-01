program Mandelbrot;

uses
  Forms,
  MandelbrotMain in 'MandelbrotMain.pas' {frmMain},
  sdVirtualScrollbox in '..\sdVirtualScrollbox.pas',
  sdMapIterator in '..\..\bitmap\sdMapIterator.pas',
  sdBitmapConversionWin in '..\..\bitmap\sdBitmapConversionWin.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
