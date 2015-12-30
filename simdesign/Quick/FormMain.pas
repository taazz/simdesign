unit FormMain;

interface

uses
  // delphi
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, XPMan, ExtCtrls, ComCtrls, ToolWin, ImgList,
  StdCtrls, ExtDlgs, VirtualTrees,

  // frames/forms
  FrameSelectColor, FrameToolWin, FrameReplaceColor, FrameFloodFill,
  FrameRenderOptions, FrameShapeInfo, FormSaveImage,

  // pyro
  pgFrameRulerWindow, pgScene, pgDocument, pgStorage,
  pgEditorUsingScene, pgCanvasUsingPyro,
  pgColor, pgViewer, pgBackgroundFill, pgTransform, pgScalableVectorGraphics,
  pgBitmap, pgRaster, pgContentProvider, 
  pgWinGDI, pgFloodFill, pgSyncTree,
  pgSelector, pgRender, pgPath,
  pgBlockEraser, pgUndoStack, pgInPainting, pgRenderUsingCore,
  pgCanvas, pgPlatform, Pyro,

  // Raster formats:
  pgRasterJpg,
  {pgRasterGif,}
  {pgRasterTga,}
  pgRasterPng,
  // simdesign
  dtpRsRuler, sdDebug;

{$R appcursors.res}

type

  TPolygonStyle = (
    psNormal,
    psEraser,
    psInPainting
  );

  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    acMain: TActionList;
    xmMain: TXPManifest;
    sbMain: TStatusBar;
    mnuFile: TMenuItem;
    pnlCenter: TPanel;
    splMain: TSplitter;
    pnlLeft: TPanel;
    tbMain: TToolBar;
    pnlTools: TPanel;
    splTools: TSplitter;
    pnlShapes: TPanel;
    acLoadImage: TAction;
    acSaveImage: TAction;
    acLoadProject: TAction;
    acSaveProject: TAction;
    acExit: TAction;
    mnuLoadImage: TMenuItem;
    mnuSaveImage: TMenuItem;
    N1: TMenuItem;
    mnuLoadProject: TMenuItem;
    mnuSaveProject: TMenuItem;
    N2: TMenuItem;
    mnuExit: TMenuItem;
    mnuShape: TMenuItem;
    mnuTools: TMenuItem;
    mnuView: TMenuItem;
    mnuWindow: TMenuItem;
    ilMain: TImageList;
    acReplaceColor: TAction;
    acFloodFill: TAction;
    mnuFloodFill: TMenuItem;
    mnuReplaceColor: TMenuItem;
    acAddBitmap: TAction;
    acAddText: TAction;
    acAddEllipse: TAction;
    acAddRectangle: TAction;
    acAddPolygon: TAction;
    mnuAddBitmap: TMenuItem;
    mnuAddText: TMenuItem;
    mnuAddRectangle: TMenuItem;
    mnuAddEllipse: TMenuItem;
    mnuAddPolygon: TMenuItem;
    frRulerWindow: TfrRulerWindow;
    acZoomIn: TAction;
    acZoomOut: TAction;
    acZoom100: TAction;
    acZoomWindow: TAction;
    mnuZoomIn: TMenuItem;
    mnuZoomOut: TMenuItem;
    mnuZoom100: TMenuItem;
    mnuZoomWindow: TMenuItem;
    N3: TMenuItem;
    acZoomExtent: TAction;
    mnuZoomExtent: TMenuItem;
    acShowRulers: TAction;
    mnuShowRulers: TMenuItem;
    acFlattenShapes: TAction;
    mnuFlatten: TMenuItem;
    acRenderOptions: TAction;
    acNewProject: TAction;
    mnuNewProject: TMenuItem;
    acBackgroundFill: TAction;
    acBackgroundGrid: TAction;
    acBackgroundChecker: TAction;
    N4: TMenuItem;
    mnuBackground: TMenuItem;
    mnuSingleColor: TMenuItem;
    mnuGrid: TMenuItem;
    mnuCheckerPattern: TMenuItem;
    acAALevel0: TAction;
    acAALevel1: TAction;
    acAALevel2: TAction;
    acAALevel4: TAction;
    mnuAntiAliasing: TMenuItem;
    mnuAANone: TMenuItem;
    mnuAALow: TMenuItem;
    mnuAAMedium: TMenuItem;
    mnuAAHighest: TMenuItem;
    acAALevel3: TAction;
    mnuAAHigh: TMenuItem;
    Panel1: TPanel;
    Memo1: TMemo;
    acZoomPrev: TAction;
    ZoomPrevious1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    acAddAffine: TAction;
    acAddProjective: TAction;
    acAddCurved: TAction;
    acRemoveTransform: TAction;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    acPolygonEraser: TAction;
    EraserPolygon1: TMenuItem;
    acFlattenSelection: TAction;
    FlattenSelection1: TMenuItem;
    acHoverHighlights: TAction;
    HoverHighlights1: TMenuItem;
    acAddLine: TAction;
    ToolButton25: TToolButton;
    acBlockEraser: TAction;
    BlockEraser1: TMenuItem;
    acUndo: TAction;
    acRedo: TAction;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    acPolygonInPainter: TAction;
    PolygonInpainter1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure acReplaceColorExecute(Sender: TObject);
    procedure acFloodFillExecute(Sender: TObject);
    procedure acLoadImageExecute(Sender: TObject);
    procedure acSaveImageExecute(Sender: TObject);
    procedure acLoadProjectExecute(Sender: TObject);
    procedure acSaveProjectExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acAddBitmapExecute(Sender: TObject);
    procedure acAddTextExecute(Sender: TObject);
    procedure acAddEllipseExecute(Sender: TObject);
    procedure acAddRectangleExecute(Sender: TObject);
    procedure acAddPolygonExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acZoom100Execute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);
    procedure acZoomExtentExecute(Sender: TObject);
    procedure acShowRulersExecute(Sender: TObject);
    procedure acFlattenShapesExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acBackgroundFillExecute(Sender: TObject);
    procedure acBackgroundGridExecute(Sender: TObject);
    procedure acBackgroundCheckerExecute(Sender: TObject);
    procedure acAALevel0Execute(Sender: TObject);
    procedure acAALevel1Execute(Sender: TObject);
    procedure acAALevel2Execute(Sender: TObject);
    procedure acAALevel4Execute(Sender: TObject);
    procedure acAALevel3Execute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acZoomPrevExecute(Sender: TObject);
    procedure acPolygonEraserExecute(Sender: TObject);
    procedure acFlattenSelectionExecute(Sender: TObject);
    procedure acHoverHighlightsExecute(Sender: TObject);
    procedure acAddAffineExecute(Sender: TObject);
    procedure acRemoveTransformExecute(Sender: TObject);
    procedure acAddProjectiveExecute(Sender: TObject);
    procedure acAddCurvedExecute(Sender: TObject);
    procedure acAddLineExecute(Sender: TObject);
    procedure acBlockEraserExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acPolygonInPainterExecute(Sender: TObject);
  private
    FToolFrame: TFrame;
    FReplaceColorFrame: TfrReplaceColor;
    FFloodFillFrame: TfrFloodFill;
    FShapeListFrame: TfrToolWin;
    FRenderOptionsFrame: TfrRenderOptions;
    FShapeInfoFrame: TfrShapeInfo;
    FSaveImageForm: TfrmSaveImage;
    FScene: TpgScene;
    FUndoStack: TpgUndoStack;
    FEditor: TpgSceneEditor;
    FExportImageSettings: TpgExportImageSettings;
    FColorSelector: TfrSelectColor;
    FBackupBitmap: TpgBitmap;
    FHasBackup: boolean;
    FShapeList: TpgSyncTree;
    FPolygonStyle: TPolygonStyle;
    FBlockEraser: TpgBlockEraser;
    { Private declarations }
    procedure SetToolFrame(AToolFrame: TFrame);
    procedure BuildScene;
    procedure RulerMousePosition(Sender: TObject; const X, Y: double; Units: TRulerUnit;
      const Scale: double; IsInside: boolean);
    procedure NewDocumentWithImage(const AFileName: string);
    procedure ExportImage(const Settings: TpgExportImageSettings; const AFileName: string);
    procedure FlattenShapes(AFilter: TpgRenderFilter);
    procedure FlattenSelection;
    procedure SetAntiAliasing(ALevel: integer);
    procedure LoadProject(const AFilename: string);
    procedure SaveProject(const AFilename: string);
    procedure DoPickColor(Sender: TObject);
    procedure EditorPickColor(Sender: TObject; const Mouse: TpgMouseInfo);
    procedure EditorPickLocation(Sender: TObject; const Mouse: TpgMouseInfo);
    procedure EditorDisplayMessage(Sender: TObject; const AMessage: Utf8String);
    procedure EditorDebugMessage(Sender: TObject; Warnstyle: TsdWarnStyle; const AMessage: Utf8String);
    procedure EditorCommandComplete(Sender: TObject; ACommand: TpgEditorCommand;
      AInfo: TpgCommandInfo);
    procedure EditorModeChange(Sender: TObject);
    procedure EditorSelectItem(Sender: TObject; AElement: TpgElement);
    function GetImageBitmap: TpgBitmap;
    procedure ReplacePreview(Sender: TObject);
    procedure ReplaceAccept(Sender: TObject);
    procedure ReplaceCancel(Sender: TObject);
    procedure FloodFillPick(Sender: TObject);
    procedure FloodFillAccept(Sender: TObject);
    procedure FloodFillCancel(Sender: TObject);
    procedure ReplaceTopLeftByTransparent(ABitmap: TpgBitmap);
    function ColorFromPoints(AInfo: TpgCommandInfo): TpgColor32;
    procedure BlockEraserStop(Sender: TObject);
    procedure AddProjectiveTransform(AClass: TpgTransformClass);
    procedure SceneAfterChange(Sender: TObject; AElement: TpgElement; APropId: longword;
      AChange: TpgChangeType);
    function GetCanvas: TpgCanvas;
  public
    { Public declarations }
  end;

const
  cImageFilter = 'Image files|*.bmp;*.jpg;*.png;*.gif|All files|*.*';
  cProjectFilter =
    'Project files (*.pyro)|*.pyro|' +
    'Textual project files (*.txt)|*.txt|' +
    'SVG|*.svg|All files|*.*';

  // cursors
  crColorPick        = -100;
  crFloodFill        = -101;

var
  frmMain: TfrmMain;

implementation

type

  TFloodInfo = record
    SourceColor: TpgColor32;
    TargetColor: TpgColor32;
    Tolerance: integer;
  end;

function FloodFillCallback(AColor: pointer; Info: pointer): boolean;
var
  Dist, Tolerance: integer;
  SourceColor: TpgColor32;
  TargetColor: TpgColor32;
begin
  Result := False;
  SourceColor := TFloodInfo(Info^).SourceColor;
  Tolerance := TFloodInfo(Info^).Tolerance;
  Dist := pgColorDistTaxi32(AColor, @SourceColor);
  if Dist <= Tolerance then
  begin
    Result := True;
    TargetColor := TFloodInfo(Info^).TargetColor;
    if (Tolerance > 0) and (Dist > 0) then
      PpgColor32(AColor)^ := pgColorBlend32(@TargetColor, AColor, round(Dist/Tolerance * 255))
    else
      PpgColor32(AColor)^ := TargetColor;
  end;
end;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  SL: TpgSceneListener;
begin
  // Defaults
  FExportImageSettings := cDefaultExportImageSettings;
  // Scene and editor
  FScene := TpgScene.Create(Self);
  SL := FScene.Listeners.AddRef(Self);
  SL.OnAfterChange := SceneAfterChange;
  // Create undo stack, and connect it to the scene
  FUndoStack := TpgUndoStack.Create(Self);
  FUndoStack.OnMessage := EditorDisplayMessage;
  FUndoStack.Scene := FScene;

  // Create the frames we need (will be freed automatically)
  FReplaceColorFrame := TfrReplaceColor.Create(Self);
  FReplaceColorFrame.Images := ilMain;
  FReplaceColorFrame.ImageIndex := 3;
  FReplaceColorFrame.frSourceColor.OnPickColor := DoPickColor;
  FReplaceColorFrame.frTargetColor.OnPickColor := DoPickColor;
  FReplaceColorFrame.OnPreview := ReplacePreview;
  FReplaceColorFrame.OnAccept := ReplaceAccept;
  FReplaceColorFrame.OnCancel := ReplaceCancel;

  FFloodFillFrame := TfrFloodFill.Create(Self);
  FFloodFillFrame.Images := ilMain;
  FFloodFillFrame.ImageIndex := 4;
  FFloodFillFrame.frFloodFillColor.OnPickColor := DoPickColor;
  FFloodFillFrame.OnPick := FloodFillPick;
  FFloodFillFrame.OnAccept := FloodFillAccept;
  FFloodFillFrame.OnCancel := FloodFillCancel;

  FShapeInfoFrame := TfrShapeInfo.Create(Self);
  FShapeInfoFrame.Align := alClient;
  FShapeInfoFrame.Name := 'ShapeInfo';
  FShapeInfoFrame.Scene := FScene;

  FShapeListFrame := TfrToolWin.Create(Self);
  FShapeListFrame.Title := 'Shape List';
  FShapeListFrame.Align := alClient;
  FShapeListFrame.Parent := pnlShapes;
  FShapeListFrame.Images := ilMain;
  FShapeListFrame.ImageIndex := 14;
  FShapeListFrame.Name := 'ShapeList';

  FShapeList := TpgSyncTree.Create(Self);
  FShapeList.Align := alClient;
  FShapeList.Parent := FShapeListFrame;

  FEditor := TpgSceneEditor.Create(Self);
  // so we can see pixels in grid mode..
  FEditor.Background.Grid.Size.X := 1;
  FEditor.Background.Grid.Size.Y := 1;
  FEditor.OnMessage := EditorDisplayMessage;
  FEditor.OnCommandComplete := EditorCommandComplete;
  FEditor.OnModeChange := EditorModeChange;
  FEditor.OnSelectItem := EditorSelectItem;

  FRenderOptionsFrame := TfrRenderOptions.Create(Self);
  FRenderOptionsFrame.Control := frRulerWindow.Scrollbox;

  SetToolFrame(nil);
  frRulerWindow.ScrollBox.Canvas.ColorInfo^ := cARGB_8b_Org;
  frRulerWindow.ScrollBox.Canvas.OverSampling := 2;
  frRulerWindow.ScrollBox.Canvas.InterpolationMethod := imNearest;
  frRulerWindow.ScrollBox.Provider := FEditor;
  frRulerWindow.OnMousePosition := RulerMousePosition;
  // Paint this form double-buffered, to avoid flicker during resize
  DoubleBuffered := True;

  pgSetAntialiasing(4);

  BuildScene;

  FEditor.Scene := FScene;

  FShapeList.Scene := FScene;

  FBackupBitmap := TpgBitmap.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FScene.Listeners.DeleteRef(Self);
  FreeAndNil(FBackupBitmap);
  FreeAndNil(FBlockEraser);
  // Disconnect the editor.. otherwise it may cause trouble in the sequence of
  // freeing the components
  FEditor.Scene := nil;
end;

procedure TfrmMain.SetToolFrame(AToolFrame: TFrame);
begin
  if assigned(FToolFrame) then
    FToolFrame.Parent := nil;
  FToolFrame := AToolFrame;
  pnlTools.Visible := assigned(FToolFrame);
  splTools.Visible := assigned(FToolFrame);
  if not assigned(FToolFrame) then
    exit;
  pnlLeft.Width := FToolFrame.Width + 2;
  pnlTools.Height := FToolFrame.Height + 2;
  FToolFrame.Align := alClient;
  FToolFrame.Parent := pnlTools;
end;

procedure TfrmMain.BuildScene;
//var
  //Rect: TpgRectangle;
  //T: TpgAffineTransform;
  //Text: TpgText;
  //PT: TpgProjectiveTransform;
begin
  FScene.ViewPort.Width.FloatValue := 400;
  FScene.ViewPort.Height.FloatValue := 300;

{  // testing
  Rect := TpgRectangle.Create(FScene.ViewPort);
  Rect.X.Value := 10;
  Rect.Y.Value := 10;
  Rect.Width.Value := 30;
  Rect.Height.Value := 20;
  Rect.Fill.AsColor32 := clBlue32;
  Rect.Stroke.AsColor32 := clRed32;
  Rect.StrokeWidth.Value := 1;
  T := TpgAffineTransform.Create;
  //T.Translate(20, 20);
  //T.Rotate(30, 0, 0);
  //T.Scale(2, 2);
  Rect.Transform.Value := T;
  Rect.Name.Value := 'Test';}

{  Text := TpgText.Create(FScene.ViewPort);
  Text.Text.Value := 'Hello world';
  Text.X.Add(0, luNone);
  Text.Y.Add(12, luNone);
  Text.Fill.AsColor32 := clBlack32;
  Text.FontFamily.Value := 'arial';
  Text.FontSize.Value := 12;

  PT := TpgProjectiveTransform.Create;
  PT.Points[0]^ := pgPointS(200, 100);
  PT.Points[1]^ := pgPointS(300, 100);
  PT.Points[2]^ := pgPointS(200, 300);
  PT.Points[3]^ := pgPointS(100, 200);
  PT.Width := 60;
  PT.Height := 20;
  Text.Transform.Value := PT;}
end;

type
  TSceneAccess = class(TpgScene);

procedure TfrmMain.LoadProject(const AFilename: string);
var
  //M: TMemoryStream;
  FS: TFileStream;
  SVG: TpgSVGImport;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    if LowerCase(ExtractFileExt(AFileName)) = '.svg' then
    begin

      // Load SVG
      SVG := TpgSVGImport.Create(Self);
      try
        SVG.OnDebugOut := EditorDebugMessage;
        SVG.ImportScene(FScene, FS);
      finally
        SVG.Free;
      end;

    end else
    if LowerCase(ExtractFileExt(AFileName)) = '.xml' then
    begin

      // Load textual Pyro file
      FScene.LoadFromStream(FS);

    end else
    begin
      // Assume extension .pyro (binary file, most compact)

      // Load binary Pyro file
      FScene.LoadFromBinaryStream(FS);

    end;

    // Reset editor
    FEditor.Scene := nil;
    FEditor.Scene := FScene;
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.SaveProject(const AFilename: string);
var
  FS: TFileStream;
//  S: TpgStorage;
begin
  FS := TFileStream.Create(AFileName, fmCreate);
  if LowerCase(ExtractFileExt(AFileName)) = '.xml' then
  begin
    try
      FScene.SaveToStream(FS);
    finally
      FS.Free;
    end;
  end;
  if LowerCase(ExtractFileExt(AFileName)) = '.bxm' then
  begin
    try
      FScene.SaveToBinaryStream(FS);
    finally
      FS.Free;
    end;
  end;
end;

procedure TfrmMain.NewDocumentWithImage(const AFileName: string);
var
  ImageView: TpgImageView;
  Bitmap: TpgBitmap;
begin
  FScene.Clear;
  ImageView := TpgImageView.CreateParent(FScene, FScene.ViewPort);
  ImageView.Image.LoadFromFile(AFileName, True);
  Bitmap := ImageView.Image.Bitmap;
  ImageView.Width.FloatValue := Bitmap.Width;
  ImageView.Height.FloatValue := Bitmap.Height;
  ImageView.EditorOptions.IntValue := [eoDenySelect];
  FScene.ViewPort.Width.FloatValue := Bitmap.Width;
  FScene.ViewPort.Height.FloatValue := Bitmap.Height;
  // Update content size
  FEditor.Scene := nil;
  FEditor.Scene := FScene;
end;

procedure TfrmMain.ExportImage(const Settings: TpgExportImageSettings; const AFileName: string);
var
  Bitmap: TpgBitmap;
begin
  Bitmap := TpgBitmap.Create;
  case Settings.BitmapFormat of
  bfARGBOrg: Bitmap.SetColorInfo(cARGB_8b_Org);
  bfARGBPre: Bitmap.SetColorInfo(cARGB_8b_Pre);
  bfRGB:     Bitmap.SetColorInfo(cRGB_8b);
  end;
  try
    FEditor.RenderToBitmap(Bitmap, Settings.BackgroundColor);
    SaveImageToFile(AFileName, Bitmap, cMimeTypeForFormat[Settings.ImageFormat],
      Settings.JpgQuality);
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmMain.FlattenShapes(AFilter: TpgRenderFilter);
var
  i: integer;
  ImageView: TpgImageView;
  Bitmap: TpgBitmap;
  Element: TObject;
begin
  Bitmap := TpgBitmap.Create;
  Bitmap.SetColorInfo(cARGB_8b_Org);
  try
    FEditor.Renderer.Filter := AFilter;
    FEditor.RenderToBitmap(Bitmap, $00000000);
    FEditor.Renderer.Filter := rfAll;

    // Unwire scene from editor
    FEditor.Scene := nil;

    // Remove the shapes that were flattened
    case AFilter of
    rfAll: FScene.Clear;
    rfNonSelectable:
      begin
        for i := TSceneAccess(FScene).RootNodeCount - 1 downto 0 do
        begin
          Element := TSceneAccess(FScene).RootNodes[i];
          if (Element is TpgGraphic) and
             (eoDenySelect in TpgGraphic(Element).EditorOptions.IntValue) and
             (Element <> FScene.ViewPort) then
            Element.Free;
        end;
      end;
    end;

    // Create a new imageview shape
    ImageView := TpgImageView.CreateParent(FScene, FScene.ViewPort);
    ImageView.Image.Bitmap.Assign(Bitmap);
    ImageView.Width.FloatValue := Bitmap.Width;
    ImageView.Height.FloatValue := Bitmap.Height;
    ImageView.EditorOptions.IntValue := [eoDenySelect];
    // Force imageview to be first in render chain
    //todo ImageView.ElementIndex := 0;

    // Adjust viewport width of scene to imageview
    FScene.ViewPort.Width.FloatValue := Bitmap.Width;
    FScene.ViewPort.Height.FloatValue := Bitmap.Height;

    // Rewire scene to editor
    FEditor.Scene := FScene;
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmMain.FlattenSelection;
var
  i: integer;
  Selector: TpgSelector;
begin
  // Make selected shapes non-selectable so they're rendered with the filter
  // we are going to choose
  for i := 0 to FEditor.Selectors.Count - 1 do
  begin
    Selector := FEditor.Selectors[i];
    Selector.Graphic.EditorOptions.IntValue := Selector.Graphic.EditorOptions.IntValue + [eoDenySelect];
  end;

  FlattenShapes(rfNonSelectable);
end;

procedure TfrmMain.SetAntiAliasing(ALevel: integer);
begin
  pgSetAntiAliasing(ALevel);
  FEditor.Invalidate;
end;

procedure TfrmMain.DoPickColor(Sender: TObject);
begin
  FColorSelector := TfrSelectColor(Sender);
  FEditor.OnUserClick := EditorPickColor;
  FEditor.ToolMode := tmUser;
  FEditor.ToolCursor := crColorPick;
end;

function TfrmMain.GetImageBitmap: TpgBitmap;
var
  i: integer;
  VP: TpgViewPort;
  IV: TpgImageView;
begin
  // Get the image's bitmap (if any)
  VP := FScene.ViewPort;
  IV := nil;
  Result := nil;
  for i := 0 to VP.ElementCount - 1 do
    if VP.Elements[i] is TpgImageView then
    begin
      IV := TpgImageView(VP.Elements[i]);
      break;
    end;
  if assigned(IV) then
    Result := IV.Image.Bitmap;
end;

procedure TfrmMain.EditorPickColor(Sender: TObject; const Mouse: TpgMouseInfo);
var
  Pc: TpgPoint;
  Bitmap: TpgBitmap;
begin
  // We now have the mouse info and the color selector. We need to find the document
  // coordinates for this click
  Pc := FEditor.ToContent(Mouse.X, Mouse.Y);

  // Get the image's bitmap (if any)
  Bitmap := GetImageBitmap;

  // Switch off toolmode
  FEditor.ToolMode := tmNone;
  if not assigned(Bitmap) then
    exit;

  // Find the bitmap color
  FColorSelector.SelectColor32 := Bitmap.Pixels[trunc(Pc.X), trunc(Pc.Y)];
end;

function TfrmMain.ColorFromPoints(AInfo: TpgCommandInfo): TpgColor32;
var
  i: integer;
  Count: cardinal;
  Bitmap: TpgBitmap;
  Col: TpgColor32;
  R, G, B, A: longword;
begin
  Result := clBlack32;
  // Get the image's bitmap (if any)
  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then
    exit;

  R := 0; G := 0; B := 0; A := 0;
  Count := length(AInfo.Points);
  for i := 0 to Count - 1 do
  begin
    Col := Bitmap.Pixels[trunc(AInfo.Points[i].X), trunc(AInfo.Points[i].Y)];
    // Add individual components
    R := R + Col and $FF;
    G := G + (Col shr 8) and $FF;
    B := B + (Col shr 16) and $FF;
    A := A + (Col shr 24) and $FF;
  end;
  // Average color components, and create color
  R := R div Count;
  G := G div Count;
  B := B div Count;
  A := A div Count;
  Result := A shl 24 + B shl 16 + G shl 8 + R;
end;

procedure TfrmMain.ReplacePreview(Sender: TObject);
var
  Bitmap: TpgBitmap;
  SCol, TCol: TpgColor32;
  Tol: integer;
begin
  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then exit;
  // Make sure we have a 4ch bitmap
  pgConvertBitmapToColorInfo(Bitmap, cARGB_8b_Org);

  if not FHasBackup then
  begin
    FBackupBitmap.Assign(Bitmap);
    FHasBackup := True;
  end else
    Bitmap.Assign(FBackupBitmap);

  with FReplaceColorFrame do
  begin
    SCol := frSourceColor.SelectColor32;
    TCol := frTargetColor.SelectColor32;
    Tol := frThreshold.Threshold * 3;// for each color channel
  end;
  pgReplaceColor(Bitmap, SCol, TCol, Tol);
  Bitmap.Changed := True;
  FEditor.Invalidate;
end;

procedure TfrmMain.ReplaceTopLeftByTransparent(ABitmap: TpgBitmap);
var
  SCol, TCol: TpgColor32;
begin
  SCol := ABitmap.Pixels[0, 0];
  TCol := SCol and $00FFFFFF;

  // Make sure we have a 4ch bitmap
  pgConvertBitmapToColorInfo(ABitmap, cARGB_8b_Org);

  pgReplaceColor(ABitmap, SCol, TCol, 5);
  ABitmap.Changed := True;
  FEditor.Invalidate;
end;

procedure TfrmMain.ReplaceAccept(Sender: TObject);
begin
  FHasBackup := False;
end;

procedure TfrmMain.ReplaceCancel(Sender: TObject);
var
  Bitmap: TpgBitmap;
begin
  FHasBackup := False;
  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then
    exit;
  Bitmap.Assign(FBackupBitmap);
  FEditor.Invalidate;
end;

procedure TfrmMain.EditorPickLocation(Sender: TObject; const Mouse: TpgMouseInfo);
var
  Pc: TpgPoint;
  Sx, Sy: integer;
  Bitmap: TpgBitmap;
  FloodInfo: TFloodInfo;
begin
  // We now have the mouse info where clicked
  Pc := FEditor.ToContent(Mouse.X, Mouse.Y);
  Sx := round(Pc.X);
  Sy := round(Pc.Y);

  FFloodFillFrame.btnAccept.Enabled := True;
  FFloodFillFrame.btnCancel.Enabled := True;

  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then
    exit;
  // Make sure we have a 4ch bitmap
  pgConvertBitmapToColorInfo(Bitmap, cARGB_8b_Org);

  if not FHasBackup then
  begin
    FBackupBitmap.Assign(Bitmap);
    FHasBackup := True;
  end else
    Bitmap.Assign(FBackupBitmap);

  // Setup flood info, source color is the clicked color, target from the
  // frame, tolerance from the frame
  FloodInfo.SourceColor := PpgColor32(Bitmap.Elements[Sx, Sy])^;
  with FFloodFillFrame do
  begin
    FloodInfo.TargetColor := frFloodFillColor.SelectColor32;
    FloodInfo.Tolerance := frThreshold.Threshold * 3;// for each color channel
  end;

  // Call the floodfill routine, it uses FloodFillCallback to determine if a
  // pixel is part of the flood
  FloodFill(Bitmap, Sx, Sy, FloodFillCallback, @FloodInfo);
  Bitmap.Changed := True;
  FEditor.Invalidate;
end;

procedure TfrmMain.EditorSelectItem(Sender: TObject; AElement: TpgElement);
begin
  FShapeInfoFrame.Shape := TpgPaintable(AElement);
  SetToolFrame(FShapeInfoFrame);
end;

procedure TfrmMain.FloodFillPick(Sender: TObject);
begin
  FEditor.OnUserClick := EditorPickLocation;
  FEditor.ToolMode := tmUser;
  FEditor.ToolCursor := crFloodFill;
end;

procedure TfrmMain.FloodFillAccept(Sender: TObject);
begin
  FHasBackup := False;
  FEditor.ToolMode := tmNone;
end;

procedure TfrmMain.FloodFillCancel(Sender: TObject);
var
  Bitmap: TpgBitmap;
begin
  FHasBackup := False;
  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then
    exit;
  Bitmap.Assign(FBackupBitmap);
  FEditor.Invalidate;
  FEditor.ToolMode := tmNone;
end;

procedure TfrmMain.acReplaceColorExecute(Sender: TObject);
begin
  SetToolFrame(FReplaceColorFrame);
end;

procedure TfrmMain.acFloodFillExecute(Sender: TObject);
begin
  SetToolFrame(FFloodFillFrame);
end;

procedure TfrmMain.acPolygonEraserExecute(Sender: TObject);
begin
  FPolygonStyle := psEraser;
  FEditor.AddPolygon;
end;

procedure TfrmMain.acPolygonInPainterExecute(Sender: TObject);
begin
  FPolygonStyle := psInPainting;
  FEditor.AddPolygon;
end;

procedure TfrmMain.acBlockEraserExecute(Sender: TObject);
var
  x, y: integer;
  Mask: TpgBitmap;
  Bitmap: TpgBitmap;
  P: PByte;
const
  cMaskInfo: array[0..4, 0..4] of byte =
    (($00, $80, $80, $80, $00),
     ($80, $C0, $FF, $C0, $80),
     ($80, $FF, $FF, $FF, $80),
     ($80, $C0, $FF, $C0, $80),
     ($00, $80, $80, $80, $00));
begin
  // Setup bitmap
  Bitmap := GetImageBitmap;
  if not assigned(Bitmap) then
    exit;
  // Make sure we have a 4ch bitmap
  pgConvertBitmapToColorInfo(Bitmap, cARGB_8b_Org);
  Bitmap.Changed := True;

  // Setup block eraser
  if not assigned(FBlockEraser) then
    FBlockEraser := TpgBlockEraser.Create;
  // Set mask from cMaskInfo
  Mask := TpgBitmap.Create;
  Mask.SetColorInfo(cY_8b);
  Mask.SetSize(5, 5);
  for y := 0 to 4 do
    for x := 0 to 4 do
    begin
      P := Mask.Elements[x, y];
      P^ := AnsiChar(cMaskInfo[y, x]);
    end;
  FBlockEraser.Mask := Mask;
  FBlockEraser.OnStopEraser := BlockEraserStop;
  FBlockEraser.OnInvalidate := FEditor.DoInvalidate;
  FBlockEraser.Bitmap := Bitmap;
  FBlockEraser.Color := clWhite32;
  FBlockEraser.Transform := FEditor.Transform;

  // Setup editor
  FEditor.ToolMode := tmUser;
  FEditor.ToolCursor := crCross;
  FEditor.OnUserClick := FBlockEraser.MouseClick;
  FEditor.OnUserStartDrag := FBlockEraser.MouseStartDrag;
  FEditor.OnUserCloseDrag := FBlockEraser.MouseCloseDrag;
  FEditor.OnUserDrag := FBlockEraser.MouseDrag;

  // Info
  EditorDisplayMessage(nil, 'Block eraser: click right mouse button to stop');
end;

procedure TfrmMain.BlockEraserStop(Sender: TObject);
begin
  FEditor.ToolMode := tmNone;
  FEditor.OnUserClick := nil;
  FEditor.OnUserStartDrag := nil;
  FEditor.OnUserCloseDrag := nil;
  FEditor.OnUserDrag := nil;
  EditorDisplayMessage(nil, 'Block eraser stopped');
end;

procedure TfrmMain.acLoadImageExecute(Sender: TObject);
var
  OD: TOpenPictureDialog;
begin
  OD := TOpenPictureDialog.Create(nil);
  try
    OD.Title := 'Load Image';
    OD.Filter := cImageFilter;
    if OD.Execute then begin
      NewDocumentWithImage(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

procedure TfrmMain.acSaveImageExecute(Sender: TObject);
begin
  if not assigned(FSaveImageForm) then
    FSaveImageForm := TfrmSaveImage.Create(Self);
  FSaveImageForm.FormFromSettings(FExportImageSettings);
  if FSaveImageForm.ShowModal = mrOK then
  begin
    FSaveImageForm.FormToSettings(FExportImageSettings);
    with TSavePictureDialog.Create(nil) do
      try
        Title := 'Save Image';
        Filter := cFilterForFormat[FExportImageSettings.ImageFormat];
        DefaultExt := cExtensionForFormat[FExportImageSettings.ImageFormat];
        if Execute then
        begin
          FileName := ChangeFileExt(FileName, DefaultExt);
          ExportImage(FExportImageSettings, FileName);
        end;
      finally
        Free;
      end;
  end;
end;

procedure TfrmMain.acNewProjectExecute(Sender: TObject);
begin
  FScene.Clear;
  BuildScene;
  // Update content size
  FEditor.Scene := nil;
  FEditor.Scene := FScene;
end;

procedure TfrmMain.acLoadProjectExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := cProjectFilter;
      Title := 'Load Project';
      if Execute then
        LoadProject(FileName);
    finally
      Free;
    end;
end;

procedure TfrmMain.acSaveProjectExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
    try
      Filter := cProjectFilter;
      Title := 'Save Project';
      if Execute then
        SaveProject(FileName);
    finally
      Free;
    end;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acAddBitmapExecute(Sender: TObject);
begin
  FEditor.AddBitmap;
end;

procedure TfrmMain.acAddTextExecute(Sender: TObject);
begin
  FEditor.AddText;
end;

procedure TfrmMain.acAddEllipseExecute(Sender: TObject);
begin
  FEditor.AddEllipse;
end;

procedure TfrmMain.acAddRectangleExecute(Sender: TObject);
begin
  FEditor.AddRectangle;
end;

procedure TfrmMain.acAddPolygonExecute(Sender: TObject);
begin
  FPolygonStyle := psNormal;
  FEditor.AddPolygon;
end;

procedure TfrmMain.acAddLineExecute(Sender: TObject);
begin
  FEditor.AddLine;
end;

procedure TfrmMain.acZoomInExecute(Sender: TObject);
begin
  FEditor.ZoomRelative(sqrt(2),
    frRulerWindow.ScrollBox.Width * 0.5,
    frRulerWindow.ScrollBox.Height * 0.5);
end;

procedure TfrmMain.acZoomOutExecute(Sender: TObject);
begin
  FEditor.ZoomRelative(sqrt(0.5),
    frRulerWindow.ScrollBox.Width * 0.5,
    frRulerWindow.ScrollBox.Height * 0.5);
end;

procedure TfrmMain.acZoom100Execute(Sender: TObject);
begin
  FEditor.Zoom100;
end;

procedure TfrmMain.acZoomWindowExecute(Sender: TObject);
begin
  if acZoomWindow.Checked then
    FEditor.ZoomMode := zmZoomWindow
  else
    FEditor.ZoomMode := zmNone;
end;

procedure TfrmMain.acZoomExtentExecute(Sender: TObject);
begin
  FEditor.ZoomExtent;
end;

procedure TfrmMain.acZoomPrevExecute(Sender: TObject);
begin
  FEditor.ZoomPrevious;
end;

procedure TfrmMain.acShowRulersExecute(Sender: TObject);
begin
  frRulerWindow.RulersVisible := acShowRulers.Checked;
end;

procedure TfrmMain.RulerMousePosition(Sender: TObject; const X, Y: double;
  Units: TRulerUnit; const Scale: double; IsInside: boolean);
  function UnitFormat(const Value: double): string;
  begin
    case Units of
    ruPixel: Result := Format('%4dpx', [round(Value)]);
    ruMilli: Result := Format('%4.1fmm', [Value]);
    ruCenti: Result := Format('%4.2fcm', [Value]);
    ruInch: Result := Format('%4.3fin', [Value]);
    else
      Result := FloatToStr(Value);
    end;
  end;
begin
  // Show on status bar
  if IsInside then
    sbMain.Panels[1].Text := Format('X=%s, Y=%s Zoom=%d%%' + '      ',
      [UnitFormat(X), UnitFormat(Y), round(Scale * 100)])
  else
    sbMain.Panels[1].Text := '';
end;

procedure TfrmMain.acFlattenShapesExecute(Sender: TObject);
begin
  FlattenShapes(rfAll);
end;

procedure TfrmMain.acFlattenSelectionExecute(Sender: TObject);
begin
  FlattenSelection;
end;

procedure TfrmMain.acBackgroundFillExecute(Sender: TObject);
begin
  FEditor.Background.FillType := ftSingleColor;
end;

procedure TfrmMain.acBackgroundGridExecute(Sender: TObject);
begin
  FEditor.Background.FillType := ftGrid;
end;

procedure TfrmMain.acBackgroundCheckerExecute(Sender: TObject);
begin
  FEditor.Background.FillType := ftCheckerPattern;
end;

procedure TfrmMain.acAALevel0Execute(Sender: TObject);
begin
  SetAntiAliasing(0);
end;

procedure TfrmMain.acAALevel1Execute(Sender: TObject);
begin
  SetAntiAliasing(1);
end;

procedure TfrmMain.acAALevel2Execute(Sender: TObject);
begin
  SetAntiAliasing(2);
end;

procedure TfrmMain.acAALevel3Execute(Sender: TObject);
begin
  SetAntiAliasing(3);
end;

procedure TfrmMain.acAALevel4Execute(Sender: TObject);
begin
  SetAntiAliasing(4);
end;

procedure TfrmMain.EditorDisplayMessage(Sender: TObject; const AMessage: Utf8String);
begin
  Memo1.Lines.Add(AMessage);
  sbMain.Panels[0].Text := AMessage;
end;

procedure TfrmMain.EditorDebugMessage(Sender: TObject;
  Warnstyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  Memo1.Lines.Add(AMessage);
  sbMain.Panels[0].Text := AMessage;
end;

procedure TfrmMain.EditorCommandComplete(Sender: TObject;
  ACommand: TpgEditorCommand; AInfo: TpgCommandInfo);
var
  i: integer;
  Text: TpgText;
  ImageView: TpgImageView;
  Bitmap: TpgBitmap;
  Rect: TpgRectangle;
  Ellipse: TpgEllipse;
  Polygon: TpgPolygonShape;
  Line: TpgLine;
  Path: TpgRenderPath;
  InPainter: TpgInPainter;
  OPD: TOpenPictureDialog;
begin
  case ACommand of
  ecTextEnd:
    begin
      Text := TpgText.CreateParent(FScene, FScene.ViewPort);
      Text.X.Add(AInfo.Points[0].X, luNone);
      Text.Y.Add(AInfo.Points[0].Y, luNone);
      Text.Fill.AsColor32 := clBlack32;
      Text.FontFamily.Value := 'arial';
      Text.FontSize.FloatValue := 12;
      Text.Text.StringValue := 'Text here';
    end;
  ecBitmapEnd:
    begin
      // Ask user for bitmap
      OPD := TOpenPictureDialog.Create(nil);
      try
        OPD.Filter := OPD.Filter + '|Any file|*.*';
        if OPD.Execute then
        begin
          // New image
          ImageView := TpgImageView.CreateParent(FScene, FScene.ViewPort);
          ImageView.Image.LoadFromFile(OPD.FileName, True);
          Bitmap := ImageView.Image.Bitmap;
          // Ask if transparency should be created from topleft pixel
          if (MessageDlg('Transparency from topleft pixel?', mtConfirmation,
            [mbYes, mbNo], 0) = mrYes) then
          begin
            // If so, do it
            ReplaceTopLeftByTransparent(Bitmap);
          end;
          // Insert image
          ImageView.Width.FloatValue := AInfo.Points[1].X - AInfo.Points[0].X;;
          ImageView.Height.FloatValue := AInfo.Points[1].Y - AInfo.Points[0].Y;
          ImageView.X.FloatValue := AInfo.Points[0].X;
          ImageView.Y.FloatValue := AInfo.Points[0].Y;
        end;
      finally
        OPD.Free;
      end;
    end;
  ecRectEnd:
    begin
      Rect := TpgRectangle.CreateParent(FScene, FScene.ViewPort);
      Rect.X.FloatValue := AInfo.Points[0].X;
      Rect.Y.FloatValue := AInfo.Points[0].Y;
      Rect.Width.FloatValue  := AInfo.Points[1].X - AInfo.Points[0].X;
      Rect.Height.FloatValue := AInfo.Points[1].Y - AInfo.Points[0].Y;
      Rect.Fill.AsColor32 := clWhite32;
      Rect.Stroke.AsColor32 := clBlack32;
      Rect.StrokeWidth.FloatValue := 1;
    end;
  ecEllipseEnd:
    begin
      Ellipse := TpgEllipse.CreateParent(FScene, FScene.ViewPort);
      Ellipse.Cx.FloatValue := (AInfo.Points[1].X + AInfo.Points[0].X) * 0.5;
      Ellipse.Cy.FloatValue := (AInfo.Points[1].Y + AInfo.Points[0].Y) * 0.5;
      Ellipse.Rx.FloatValue := (AInfo.Points[1].X - AInfo.Points[0].X) * 0.5;
      Ellipse.Ry.FloatValue := (AInfo.Points[1].Y - AInfo.Points[0].Y) * 0.5;
      Ellipse.Fill.AsColor32 := clWhite32;
      Ellipse.Stroke.AsColor32 := clBlack32;
      Ellipse.StrokeWidth.FloatValue := 1;
    end;
  ecPolygonEnd:
    begin
      Polygon := TpgPolygonShape.CreateParent(FScene, FScene.ViewPort);
      for i := 0 to length(AInfo.Points) - 1 do
      begin
        Polygon.Points.Add(AInfo.Points[i].X);
        Polygon.Points.Add(AInfo.Points[i].Y);
      end;
      case FPolygonStyle of
      psNormal:
        begin
          Polygon.Fill.AsColor32 := clWhite32;
          Polygon.Stroke.AsColor32 := clBlack32;
          Polygon.StrokeWidth.FloatValue := 1;
        end;
      psEraser:
        begin
          // We detect the fill color from colors under the points
          Polygon.Fill.AsColor32 := ColorFromPoints(AInfo);
        end;
      psInPainting:
        begin
          // Setup bitmap
          Bitmap := GetImageBitmap;
          if not assigned(Bitmap) then
            exit;
          // Make sure we have a 4ch bitmap
          pgConvertBitmapToColorInfo(Bitmap, cARGB_8b_Org);
          Bitmap.Changed := True;
          // We do not actually add the polygon.. we use it to define
          // an area for the inpainting algorithm
          Path := TpgRenderPath.Create;
          InPainter := TpgInPainter.Create;
          try
            Path.BreakupLength := cDefaultKnotWidth * 0.7;
            Polygon.PlayFillPath(Path, GetCanvas.DeviceInfo^);
            InPainter.FillBitmap(Bitmap, Path);
          finally
            Path.Free;
            InPainter.Free;
          end;
          // Remove the polygon
          Polygon.Free;
          FEditor.Invalidate;
        end;
      end;//case
    end;
  ecLineEnd:
    begin
      Line := TpgLine.CreateParent(FScene, FScene.ViewPort);
      Line.X1.FloatValue := AInfo.Points[0].X;
      Line.Y1.FloatValue := AInfo.Points[0].Y;
      Line.X2.FloatValue := AInfo.Points[1].X;
      Line.Y2.FloatValue := AInfo.Points[1].Y;
      Line.Stroke.AsColor32 := clBlack32;
      Line.StrokeWidth.FloatValue := 1;
    end;
  end;
  FEditor.Invalidate;
end;

procedure TfrmMain.EditorModeChange(Sender: TObject);
begin
  acZoomWindow.Checked := FEditor.ZoomMode = zmZoomWindow;
end;

procedure TfrmMain.acHoverHighlightsExecute(Sender: TObject);
begin
  FEditor.HoverHighlighting := acHoverHighlights.Checked;
end;

procedure TfrmMain.acAddAffineExecute(Sender: TObject);
var
  Graphic: TpgGraphic;
begin
  if FEditor.HasSelection then
  begin
    Graphic := FEditor.Selectors[0].Graphic;
    Graphic.Transform.TransformValue := TpgAffineTransform.Create;
    FEditor.Selectors[0].Update(GetCanvas);
  end;
end;

procedure TfrmMain.AddProjectiveTransform(AClass: TpgTransformClass);
var
  Graphic: TpgGraphic;
  PT: TpgProjectiveTransform;
  BB: TpgBox;
begin
  if FEditor.HasSelection then
  begin
    Graphic := FEditor.Selectors[0].Graphic;
    BB := FEditor.Selectors[0].BoundingBox;
    PT := TpgProjectiveTransform(AClass.Create);
    PT.MinX := BB.Lft;
    PT.MinY := BB.Top;
    PT.Width := BB.Rgt - BB.Lft;
    PT.Height := BB.Btm - BB.Top;
    PT.SetPoint(0, pgPoint(BB.Lft, BB.Top));
    PT.SetPoint(1, pgPoint(BB.Rgt, BB.Top));
    PT.SetPoint(2, pgPoint(BB.Rgt, BB.Btm));
    PT.SetPoint(3, pgPoint(BB.Lft, BB.Btm));
{    if PT is TpgCurvedTransform then
      TpgCurvedTransform(PT).DeltaY := 10;}
    Graphic.Transform.TransformValue := PT;
    FEditor.Selectors[0].Update(GetCanvas);
  end;
end;

procedure TfrmMain.acAddProjectiveExecute(Sender: TObject);
begin
  AddProjectiveTransform(TpgProjectiveTransform);
end;

procedure TfrmMain.acAddCurvedExecute(Sender: TObject);
begin
  AddProjectiveTransform(TpgCurvedTransform);
end;

procedure TfrmMain.acRemoveTransformExecute(Sender: TObject);
var
  Graphic: TpgGraphic;
begin
  if FEditor.HasSelection then
  begin
    Graphic := FEditor.Selectors[0].Graphic;
    Graphic.Transform.TransformValue := nil;
    FEditor.Selectors[0].Update(GetCanvas);
  end;
end;

procedure TfrmMain.SceneAfterChange(Sender: TObject; AElement: TpgElement; APropId: longword; AChange: TpgChangeType);
// Changes in the scene are propagated through here
var
  Graphic: TpgGraphic;
begin
  // Demo: how to get information from "live" edit of the handles of a selector
  if not FEditor.HasSelection then
    exit;
  Graphic := FEditor.Selectors[0].GraphicCopy;
  if not assigned(Graphic) then
    exit;
  // We look for a property update of the fontsize, for the GraphicCopy in the editor's selector
  if (AChange = ctPropUpdate) and (APropId = piFontSize) and (AElement = Graphic) then
  begin
    EditorDisplayMessage(Self, Format('New font size of selection: %4.1f', [Graphic.FontSize.Value]));
  end;
end;

procedure TfrmMain.acUndoExecute(Sender: TObject);
begin
  FUndoStack.Undo;
end;

procedure TfrmMain.acRedoExecute(Sender: TObject);
begin
  FUndoStack.Redo;
end;

function TfrmMain.GetCanvas: TpgCanvas;
begin
  Result := frRulerWindow.ScrollBox.Canvas;
end;

initialization

  Screen.Cursors[crColorPick] := pgLoadCursor(HInstance, 'CRCOLORPICK');
  Screen.Cursors[crFloodFill] := pgLoadCursor(HInstance, 'CRFLOODFILL');

end.
