unit frmPdfAsOcx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, ComCtrls, ToolWin, ActnList;

type
  TfmAsOCX = class(TForm)
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ActionList1: TActionList;
    acPrevPage: TAction;
    acNextPage: TAction;
    procedure acPrevPageExecute(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAsOCX: TfmAsOCX;

implementation

{$R *.DFM}

procedure TfmAsOCX.acPrevPageExecute(Sender: TObject);
begin
//  PdfOCX.GotoPreviousPage;
end;

procedure TfmAsOCX.acNextPageExecute(Sender: TObject);
begin
//  PdfOCX.GotoNextPage;
end;

end.
