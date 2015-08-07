unit pgPdfImport;

interface

uses
  Classes, pgCanvas, sdPdfDocument, sdPdfContentStream, pgTransform;

type

  TpgPdfImport = class(TComponent)
  private
    FDocument: TPdfDocument;
  public
    constructor Create(AOwner: TComponent); override;
    // Render the page with APageIndex from the PDF document onto the
    // canvas ACanvas. ACanvas can be any descendant of the generic canvas
    // TpgCanvas.
    procedure RenderPage(ACanvas: TpgCanvas; ATransform: TpgTransform; APageIndex: integer);
    property Document: TPdfDocument read FDocument;
  end;

implementation

{ TpgPdfImport }

constructor TpgPdfImport.Create;
begin
  inherited;
  FDocument := TPdfDocument.Create(Self);
end;

procedure TpgPdfImport.RenderPage(ACanvas: TpgCanvas; ATransform: TpgTransform; APageIndex: integer);
var
  Content: TPdfContentStream;
  Parser: TPdfParser;
begin
  if not assigned(FDocument) then exit;
  if APageIndex >= FDocument.PageTree.PageCount then exit;
  Content := TPdfContentStream.Create;
  Parser := TPdfParser.Create(FDocument);
  try
    // Load the content stream
    Content.LoadData(FDocument.PageTree[APageIndex].ValueByKey('Contents'));
    // Parse to intermediate shape list format
    Parser.ParseContent(Content, FDocument.PageTree[APageIndex]);
  finally
    Parser.Free;
    Content.Free;
  end;
end;

end.
