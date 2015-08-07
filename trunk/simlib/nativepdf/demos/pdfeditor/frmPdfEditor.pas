unit frmPdfEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, sdPdfDocument, StdCtrls, ComCtrls, sdPdfObjects,
  ExtCtrls, sdPdfGDIRenderer, Math, ToolWin, frmPdfAsOcx;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ActionList1: TActionList;
    acPdfLoad: TAction;
    acPdfLoad1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    imPdf: TImage;
    acPageNext: TAction;
    acPagePrev: TAction;
    acPagePrev1: TMenuItem;
    acPageNext1: TMenuItem;
    View1: TMenuItem;
    sbMain: TStatusBar;
    acZoomPlus: TAction;
    acZoomMinus: TAction;
    acZoomPlus1: TMenuItem;
    acZoomMinus1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Splitter1: TSplitter;
    acAsOCX: TAction;
    ToolButton5: TToolButton;
    mmDebug: TMemo;
    procedure acPdfLoadExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acPagePrevExecute(Sender: TObject);
    procedure acPageNextExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomMinusExecute(Sender: TObject);
    procedure acAsOCXExecute(Sender: TObject);
  private
    FPdf: TPdfDocument;
    FPdfFileName: string;
    FPage: integer;
    FScale: integer;
    procedure PdfDebugMessage(Sender: TObject; Level: TPdfDebugLevel; const AMessage: string);
    { Private declarations }
  public
    procedure DisplayPage(Page: integer);
    property Pdf: TPdfDocument read FPdf write FPdf;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.acPdfLoadExecute(Sender: TObject);
begin
  mmDebug.Lines.Clear;
  with TOpenDialog.Create(Application) do
    try
      Title := 'Open PDF file';
      Filter := 'PDF files (*.pdf)|*.pdf';
      if Execute then begin
        if Pdf.Modified then begin
          case MessageDlg('The current PDF document has changed. Do you want to save '+#13+#10+'it first?',
            mtWarning, [mbYes,mbNo,mbCancel], 0) of
          mrYes: Pdf.SaveToFile(FPdfFileName);
          mrNo:;
          mrCancel: exit;
          end;
        end;
        Pdf.DelayPageLoad := True;
        Pdf.LoadFromFile(FileName);
        FPdfFileName := FileName;
        DisplayPage(0);
      end;
    finally
      Free;
    end;
end;

procedure TForm1.DisplayPage(Page: integer);
var
  ABmp: TBitmap;
//  ABmp: TMetaFile;
  AViewer: TPdfGDIViewer;
//  AMeta: TMetafileCanvas;
begin
  FPage := Page;
  // A4 size @ FScale * 25.4 DPI
  ABmp := TBitmap.Create;
//  ABmp := TMetaFile.Create;
  try
    ABmp.PixelFormat := pf24bit;
    ABmp.Width := 210 * FScale;
    ABmp.Height := 297 * FScale;
    AViewer := TPdfGDIViewer.Create;
    try
      // Render to bitmap
      AViewer.Document := Pdf;
      AViewer.Page := Page;
//      AMeta := TMetafileCanvas.Create(ABmp, 0);
//      AMeta.Brush.Color := clWhite;
//      AMeta.FillRect(Rect(0, 0, 210 * FScale, 297 * FScale));
      ABmp.Canvas.Lock;
//      AMeta.Lock;
      AViewer.RenderToCanvas(ABmp.Canvas,
        Rect(0, 0, ABmp.Width, ABmp.Height));
//        AMeta.Unlock;
      ABmp.Canvas.Unlock;
//      AMeta.Free;
      // Show this bitmap
      imPdf.Picture.Bitmap.Assign(ABmp);
//      imPdf.Picture.Metafile.Assign(ABmp);
      imPdf.Invalidate;
      sbMain.SimpleText := Format('Page %d of %d, zoom = %d', [Page + 1, Pdf.PageTree.PageCount, FScale]);
    finally
      AViewer.Free;
    end;
  finally
    ABmp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPdf := TPdfDocument.Create(Application);
  FPdf.OnDebugMessage := PdfDebugMessage;
  FScale := 4;
end;

procedure TForm1.PdfDebugMessage(Sender: TObject; Level: TPdfDebugLevel;
  const AMessage: string);
const
  cLevelNames: array[TPdfDebugLevel] of string =
    ('Info', 'Warning', 'Error');
begin
  mmDebug.Lines.Add(Format('[%s] %s', [cLevelNames[Level], AMessage]));
  Application.ProcessMessages;
end;

procedure TForm1.acPagePrevExecute(Sender: TObject);
begin
  dec(FPage);
  FPage := Max(0, Min(Pdf.PageTree.PageCount, FPage));
  DisplayPage(FPage);
end;

procedure TForm1.acPageNextExecute(Sender: TObject);
begin
  inc(FPage);
  FPage := Max(0, Min(Pdf.PageTree.PageCount - 1, FPage));
  DisplayPage(FPage);
end;

procedure TForm1.acZoomPlusExecute(Sender: TObject);
begin
  inc(FScale);
  DisplayPage(FPage);
end;

procedure TForm1.acZoomMinusExecute(Sender: TObject);
begin
  dec(FScale);
  FScale := Max(1, FScale);
  DisplayPage(FPage);
end;

procedure TForm1.acAsOCXExecute(Sender: TObject);
begin
  with TfmAsOCX.Create(Application) do begin
    Show;
//    PdfOCX.SetShowScrollbars(True);
//    PdfOCX.Src := FPdfFilename;
//    PdfOCX.GotoFirstPage;
  end;
end;

end.
