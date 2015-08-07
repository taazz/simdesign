unit PdfDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, pgViewerUsingScene, pgCanvasUsingPyro, pgCanvas, pgTransform,
  pgFrameRulerWindow, ActnList, ImgList, ToolWin, pgPdfImport, dtpRsRuler,
  ExtCtrls, pgScene, pgCustomView;

type
  TfrmMain = class(TForm)
    sbMain: TStatusBar;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    tbMain: TToolBar;
    alMain: TActionList;
    ilMain: TImageList;
    acFileOpen: TAction;
    acPagePrev: TAction;
    acPageNext: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    pnlRuler: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acPagePrevExecute(Sender: TObject);
    procedure acPageNextExecute(Sender: TObject);
  private
    FScene: TpgScene;
    FViewer: TpgSceneViewer;
    FImport: TpgPdfImport;
    //FRulerWindow: TfrRulerWindow;
    FPanel: TpgCustomView;
    FPageIndex: integer;
    procedure ViewerRender(Sender: TObject; ACanvas: TpgCanvas; ATransform: TpgTransform);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FScene := TpgScene.Create(Self);
  FImport := TpgPdfImport.Create(Self);
  FViewer := TpgSceneViewer.Create(Self);
  FViewer.Scene := FScene;
  FViewer.OnRender := ViewerRender;
  // runtime version for rsRuler
  //FRulerWindow := TfrRulerWindow.Create(Self);
  //FrulerWindow.Parent := pnlRuler;
  //FRulerWindow.Align := alClient;

  // Add custom view to the form
  FPanel := TpgCustomView.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alClient;
  FPanel.Provider := FViewer;
  //FPanel.OnMouseDown := PanelMouseDown;
end;

procedure TfrmMain.ViewerRender(Sender: TObject; ACanvas: TpgCanvas;
  ATransform: TpgTransform);
begin
  FImport.RenderPage(ACanvas, ATransform, FPageIndex);
end;

procedure TfrmMain.acFileOpenExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    try
      Filter := 'PDF files (*.pdf)|*.pdf';
      InitialDir := ExtractFileDir(Application.ExeName);
      if Execute then
      begin
        FPageIndex := 0;
        FImport.Document.LoadFromFile(FileName);
        FViewer.Invalidate;
      end;
    finally
      Free;
    end;
end;

procedure TfrmMain.acPagePrevExecute(Sender: TObject);
begin
//
end;

procedure TfrmMain.acPageNextExecute(Sender: TObject);
begin
//
end;

end.
