{ unit sdPdfContentStream

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the objects as described in the PDF reference
  chapter 3.7.1: Content streams
          4.3.3: Graphic state operators
          4.4  : Path construction and painting
          4.5.7: Color operators
          5    : Text operators

  Note: In this unit the PDF shapes are just created, their individual rendering
  is done in other units.

  Author: Nils Haeck M.Sc.

  Changes:
    04Jan2004 - Created
    27Sep2005 - Added Form XObjects
    28Sep2005 - Added inline image

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfContentStream;

interface

uses
  Classes, SysUtils, Contnrs, sdPdfObjects, sdPdfUtil, sdPdfCoordinateSpace,
  sdPdfColors, sdPdfFonts;

type

  TPdfGraphicMode = (
    gmPage,
    gmText,
    gmPath,
    gmClipping,
    gmShading,
    gmInlineImage,
    gmExternal);

  TPdfParser = class;

  // PDF content stream, construct from page by calling
  // CS := TContentStream.Create;
  // CS.LoadData(Page.ValueByKey('Content'));
  TPdfContentStream = class(TMemoryStream)
  public
    function AsString: string;
    procedure LoadData(Data: TPdfObject);
  end;

  // The Graphics state as maintained internally by the PDF content stream renderer.
  // A set of PDF content stream operations changes the graphics state
  TPdfGraphicsState = class(TPersistent)
  private
    // Graphics
    FCTM: TPdfMatrixStruct;
    FStrokeColor: TPdfColor;
    FPaintColor: TPdfColor;
    FLineWidth: PdfFloat;
    // Text
    FTextFont: TPdfFont;
    FTextFontSize: PdfFloat;
    FTextMatrix: TPdfMatrixStruct;
    FTextLineMatrix: TPdfMatrixStruct;
    FTextLeading: PdfFloat;
    FTextHorScale: PdfFloat;
    FTextRise: PdfFloat;
    FTextCharSpacing: PdfFloat;
    FTextWordSpacing: PdfFloat;
    FTextRenderMode: integer;
    FOwner: TPdfParser;
  public
    constructor Create(AOwner: TPdfParser); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function SetStateParameter(AName: string; AValue: TPdfObject): integer;
    property Owner: TPdfParser read FOwner write FOwner;
    property CTM: TPdfMatrixStruct read FCTM write FCTM;
    property PaintColor: TPdfColor read FPaintColor write FPaintColor;
    property StrokeColor: TPdfColor read FStrokeColor write FStrokeColor;
    property LineWidth: PdfFloat read FLineWidth write FLineWidth;
    property TextFont: TPdfFont read FTextFont write FTextFont;
    property TextFontSize: PdfFloat read FTextFontSize write FTextFontSize;
    property TextLeading: PdfFloat read FTextLeading write FTextLeading;
    property TextMatrix: TPdfMAtrixStruct read FTextMatrix write FTextMatrix;
    property TextLineMatrix: TPdfMAtrixStruct read FTextLineMatrix write FTextLineMatrix;
    property TextHorScale: PdfFloat read FTextHorScale write FTextHorScale;
    property TextRise: PdfFloat read FTextRise write FTextRise;
    property TextCharSpacing: PdfFloat read FTextCharSpacing write FTextCharSpacing;
    property TextWordSpacing: PdfFloat read FTextWordSpacing write FTextWordSpacing;
    property TextRenderMode: integer read FTextRenderMode write FTextRenderMode;
  end;

  // Ancestor shape type used for PDF rendering. A shape is a basic building block
  // of our rendering model. This is quite different from the rendering model in
  // PDF, which assumes there's a graphic state and there is a stream of operations.
  // We must use this model (although less efficient) in order to allow for user
  // editing later.
  TPdfShape = class(TPersistent)
  private
    FParser: TPdfParser;
  protected
    property Parser: TPdfParser read FParser write FParser;
  public
    constructor Create(AParser: TPdfParser); virtual;
    function IsEmpty: boolean; virtual;
  end;

  // TPdfGraphicShape contains a stroke and paint color, and serves as ancestor for
  // PdfPathShape and PdfTextShape
  TPdfGraphicShape = class(TPdfShape)
  private
    FStrokeColor: TPdfColor;
    FPaintColor: TPdfColor;
  public
    constructor Create(AParser: TPdfParser); override;
    destructor Destroy; override;
    property StrokeColor: TPdfColor read FStrokeColor;
    property PaintColor: TPdfColor read FPaintColor;
  end;

  TPdfPathItem = class(TPersistent)
  end;

  TPdfPathShape = class(TPdfGraphicShape)
  private
    FItems: TObjectList;
    FLineWidth: PdfFloat;
    function GetItems(Index: integer): TPdfPathItem;
    function GetItemCount: integer;
  public
    constructor Create(AOwner: TPdfParser); override;
    destructor Destroy; override;
    property ItemCount: integer read GetItemCount;
    property Items[Index: integer]: TPdfPathItem read GetItems;
    property LineWidth: PdfFloat read FLineWidth write FLineWidth;
  end;

  // A single glyph to be rendered to the page. It is held in a list of glyphs
  // in the TPdfTextShape class.
  TPdfGlyph = class(TPersistent)
  private
    FText: string;
    FPosition: TPdfMatrixStruct;
    function GetChar: char;
    function GetCharCode: integer;
  public
    property Char: char read GetChar;
    property CharCode: integer read GetCharCode;
    property Text: string read FText write FText;
    property Position: TPdfMatrixStruct read FPosition write FPosition;
  end;

  // The PDF Text shape consists of a number of glyphs (individual characters)
  // that share font, fontsize and color data, but can be individually spaced
  TPdfTextShape = class(TPdfGraphicShape)
  private
    // Currently we use a font as object here but in future this might need to change to
    // only become a pointer to a resource managed by the document (more efficient)
    FFont: TPdfFont;
    FFontSize: PdfFloat;
    FGlyphs: TObjectList;
    FRenderMode: integer;
    function GetGlyphs(Index: integer): TPdfGlyph;
    function GetGlyphCount: integer;
  protected
    procedure AddGlyphChar(Value: string); virtual;
  public
    constructor Create(AOwner: TPdfParser); override;
    destructor Destroy; override;
    function IsEmpty: boolean; override;
    procedure GlyphAdd(AGlyph: TPdfGlyph);
    procedure GlyphDelete(Index: integer);
    procedure AddGlyphString(Value: string); virtual;
    procedure AdjustSpacing(Value: PdfFloat);
    property Font: TPdfFont read FFont write FFont;
    property FontSize: PdfFloat read FFontSize write FFontSize;
    property GlyphCount: integer read GetGlyphCount;
    property Glyphs[Index: integer]: TPdfGlyph read GetGlyphs;
    property RenderMode: integer read FRenderMode write FRenderMode;
  end;

  // PDF parser with graphics state push/pop cache. This object is used to parse PDF
  // content from a content stream into a list of shapes
  TPdfParser = class(TPersistent)
  private
    FDocument: TObject;
    FStates: TObjectList;
    FCompatCount: integer;  // Compatibility BX/EX pair count
    FMode: TPdfGraphicMode;
    FResources: TPdfDictionary; // Pointer to the resources dictionary
    FShapes: TObjectList;
    FText: TPdfTextShape; // current text object
    FPath: TPdfPathShape; // current path object
    FStream: TPdfContentStream; // temp pointer to the stream
    function GetState: TPdfGraphicsState;
    function GetShapeCount: integer;
    function GetShapes(Index: integer): TPdfShape;
    procedure ShiftLineMatrix(ShiftX, ShiftY: PdfFloat);
    function GetStates(Index: integer): TPdfGraphicsState;
  protected
    function DrawXObject(AName: string): integer;
    function DrawInlineImage(S: TStream): integer;
    function GetResources(ResourceDict: TPdfDictionary): TPdfDictionary; virtual;
    function Process_gs(Operator: TPdfOperator): integer;
    procedure RefreshTextShape;
    procedure RefreshPathShape;
    procedure RefreshBitmapShape;
    procedure RefreshShapes;
    function RetrieveFontFromDict(Dict: TPdfDictionary): TPdfFont;
    property Document: TObject read FDocument;
    property Resources: TPdfDictionary read FResources;
    property Text: TPdfTextShape read FText write FText;
    property Path: TPdfPathShape read FPath write FPath;
    property States[Index: integer]: TPdfGraphicsState read GetStates;
  public
    constructor Create(ADocument: TObject);
    destructor Destroy; override;
    procedure DoDebugMessage(Level: TPdfDebugLevel; const AMessage: string);
    function IsStrict: boolean;
    procedure PushState;
    procedure PopState;
    function ProcessOperator(Operator: TPdfOperator): integer;
    procedure ParseContent(S: TPdfContentStream; ResourceDict: TPdfDictionary);
    procedure ShapeAdd(AShape: TPdfShape);
    property Mode: TPdfGraphicMode read FMode write FMode;
    property ShapeCount: integer read GetShapeCount;
    property Shapes[Index: integer]: TPdfShape read GetShapes;
    property State: TPdfGraphicsState read GetState;
  end;

  // Form XObject (see 4.9). It inherits from TPdfParser because the Stream
  // property contains a contentstream that must be parsed to render this
  // object. The TPdfDocument contains a cache of parsed FormXObjects that can
  // be selected with FormXObjectByStream()
  TPdfFormXObject = class(TPdfParser)
  private
    FStream: TPdfStream;
  public
    property Stream: TPdfStream read FStream write FStream;
  end;

  // Reference in a content stream to a Form XObject
  TPdfFormXObjectRef = class(TPdfShape)
  private
    FPosition: TPdfMatrixStruct;
    FBBox: TPdfRectangle;
    FForm: TPdfFormXObject;
  public
    property Position: TPdfMatrixStruct read FPosition write FPosition;
    property BBox: TPdfRectangle read FBBox write FBBox;
    property Form: TPdfFormXObject read FForm write FForm;
    function IsEmpty: boolean; override;
  end;

// Load a new operator (including operands) from a content stream
function LoadOperator(Pdf: TObject; S: TPdfContentStream): TPdfOperator;

implementation

uses
  sdPdfDocument, sdPdfImages;

const

  cProcessOK      = 0;  // Operator was successfully processed

  cProcessWarn    = -1; // Warning in processing of operator
  cProcessErr     = -2; // Error in processing of operator
  cProcessUnknown = -3; // Unknown operator (not in PDF 1.5)
  cProcessNotImpl = -4; // Operator not implemented

function LoadOperator(Pdf: TObject; S: TPdfContentStream): TPdfOperator;
// Load a new operator (including operands) from a content stream
var
  AObject: TPdfObject;
begin
  // Create a result operator to hold arguments
  Result := TPdfOperator.Create;
  repeat
    AObject := LoadObjectFromPdf(Pdf, S, False);
    if assigned(AObject) then begin
      if AObject is TPdfOperator then begin
        // We found the operator, copy its value to result
        Result.Value := TPdfOperator(AObject).Value;
        FreeAndNil(AObject);
      end else begin
        // We found an argument
        Result.ArgAdd(AObject);
      end;
    end;
  until not assigned(AObject);
  // Only return operator if it has content
  if length(Result.Value) = 0 then
    FreeAndNil(Result);
end;

{ TPdfContentStream }

function TPdfContentStream.AsString: string;
// Return the stream as one long string
begin
  if Size = 0 then exit;
  SetLength(Result, Size);
  Seek(0, soFromBeginning);
  Read(Result[1], Size);
  Seek(0, soFromBeginning);
end;

procedure TPdfContentStream.LoadData(Data: TPdfObject);
var
  i: integer;
  ANode: TPdfObject;
begin
  // Reload stream
  Clear;

  // Dereference data
  Data := DerefPdf(Data);
  try
    if Data is TPdfStream then begin
      // Just one stream
      TPdfStream(Data).DecompressToStream(Self);
      exit;
    end;
    if (Data is TPdfArray) then
      // Array of streams.. load and concanate all
      for i := 0 to TPdfArray(Data).ElementCount - 1 do begin
        ANode := DerefPdf(TPdfArray(Data).Elements[i]);
        if not (ANode is TPdfStream) then
          raise EPdfError.Create(sPdfExpectStream);
        TPdfStream(ANode).DecompressToStream(Self);
      end;
  finally
    Seek(0, soFromBeginning);
  end;
end;

{ TPdfParser }

constructor TPdfParser.Create(ADocument: TObject);
begin
  inherited Create;
  FDocument := ADocument;
  FStates := TObjectList.Create;
  FShapes := TObjectList.Create;
  // Add one base state
  FStates.Add(TPdfGraphicsState.Create(Self));
end;

destructor TPdfParser.Destroy;
begin
  FreeAndNil(FStates);
  FreeAndNil(FShapes);
  inherited;
end;

procedure TPdfParser.DoDebugMessage(Level: TPdfDebugLevel;
  const AMessage: string);
begin
  if assigned(Document) then
    TPdfDocument(Document).DoDebugMessage(Level, AMessage);
end;

function TPdfParser.DrawInlineImage(S: TStream): integer;
var
  AStream: TPdfInlineImageStream;
  AImage: TPdfInlineImage;
begin
  Result := cProcessOK;
  AStream := TPdfInlineImageStream.Create;
  try
    try
      AStream.LoadFromPdf(FDocument, S);
      AImage := TPdfInlineImage.Create(Self);
      AImage.Color    := State.PaintColor;
      AImage.Position := State.CTM;
      AImage.Stream   := AStream;
      ShapeAdd(AImage);
    except
      Result := cProcessErr;
    end;
  finally
    AStream.Free;
  end;
end;

function TPdfParser.DrawXObject(AName: string): integer;
// Draw an external object in the content stream. First determine which type then
// delegate
var
  XObjects: TPdfDictionary;
  XObject: TPdfStream;
  Subtype: string;
  AImage: TPdfImageXObject;
  AForm: TpdfFormXObject;
  AFormRef: TpdfFormXObjectRef;
  CS: TPdfContentStream;
  ADict: TPdfDictionary;
begin
  Result := cProcessWarn;
  // Find dictionary
  XObjects := Resources.DictionaryByKey('XObject');
  if not assigned(XObjects) then begin
    DoDebugMessage(dlWarning, Format(sPdfCantFindXObject, [AName]));
    exit;
  end;

  XObject := XObjects.StreamByKey(AName);
  if not assigned(XObject) then begin
    DoDebugMessage(dlWarning, Format(sPdfCantFindXObject, [AName]));
    exit;
  end;

  Result := cProcessOK;

  Subtype := XObject.Dict.StringByKey('Subtype');

  if Subtype = 'Image' then begin
    AImage := TPdfImageXObject.Create(Self);
    try
      AImage.Color    := State.PaintColor;
      AImage.Position := State.CTM;
      AImage.Stream   := XObject;
      ShapeAdd(AImage);
    except
      Result := cProcessErr;
    end;
    exit;
  end;

  if Subtype = 'Form' then begin

    // A form is a definition with its own content stream that can be used in
    // multiple locations. It is thus of TPdfParser class, and is owned by
    // the document. When a form is encountered, on first time it is parsed
    // and on second time a reference is made to it.
    AForm := TPdfDocument(Document).FormXObjectByStream(XObject);
    try
      if not assigned(AForm) then begin

        // We didnt find it so add it now and let it parse
        AForm := TPdfFormXObject.Create(Document);
        AForm.Stream := XObject;
        CS := TPdfContentStream.Create;
        try
          CS.LoadData(XObject);
          // The Form's resources dictionary is either specified or it's the page's one
          // (see 3.7.2)
          ADict := XObject.Dict.DictionaryByKey('Resources');
          if not assigned(ADict) then
            ADict := FResources;
          AForm.FResources := ADict;
          AForm.ParseContent(CS, nil);
        finally
          CS.Free;
        end;

        // Now add it to the document
        TPdfDocument(Document).FormXObjectAdd(AForm);
      end;

      // Now we can create the reference
      AFormRef := TPdfFormXObjectRef.Create(Self);
      // The Form's position
      AFormRef.Position := PdfMatrixMultiply(
        State.CTM,
        PdfMatrixFromArray(XObject.Dict.ArrayByKey('Matrix')));
      // The Form's bounding box
      AFormRef.BBox := PdfRectFromArray(XObject.Dict.ArrayByKey('BBox'));
      DoDebugMessage(dlInfo, Format('Form XObject Position = %s BBox = %S',
        [PdfMatrixToString(AFormRef.Position), PdfRectangleToString(AFormRef.BBox)]));
      AFormRef.Form := AForm;
      ShapeAdd(AFormRef);
    except
      Result := cProcessErr;
    end;

    exit;
  end;

  if Subtype = 'PS' then begin
    DoDebugMessage(dlWarning, sPdfNoSupportPSXObj);
    Result := cProcessWarn;
    exit;
  end;

  // Arriving here means we did not find any XObject known to us
  Result := cProcessWarn;
  DoDebugMessage(dlWarning, sPdfUnknownXObj);
end;

function TPdfParser.GetResources(ResourceDict: TPdfDictionary): TPdfDictionary;
begin
  Result := PdfPageResources(ResourceDict);
end;

function TPdfParser.GetShapeCount: integer;
begin
  Result := 0;
  if assigned(FShapes) then Result := FShapes.Count;
end;

function TPdfParser.GetShapes(Index: integer): TPdfShape;
begin
  Result := nil;
  if (Index >= 0) and (Index < ShapeCount) then
    Result := TPdfShape(FShapes[Index]);
end;

function TPdfParser.GetState: TPdfGraphicsState;
begin
  Result := TPdfGraphicsState(FStates[0]);
end;

function TPdfParser.GetStates(Index: integer): TPdfGraphicsState;
begin
  Result := TPdfGraphicsState(FStates[Index]);
end;

function TPdfParser.IsStrict: boolean;
begin
  Result := FCompatCount = 0;
end;

procedure TPdfParser.ParseContent(S: TPdfContentStream; ResourceDict: TPdfDictionary);
// Step through the stream and convert all commands to shape descendants. If we
// encounter a non-recognised command, the behaviour depends on the compatibility
// state (BX EX operators)
var
  AOperator: TPdfOperator;
  Info: string;
  ProcessResult: integer;
begin
  // Check
  if assigned(ResourceDict) then
    FResources := GetResources(ResourceDict);
  if not assigned(FResources) then
    raise EPdfError.Create(sPdfResourcesMissing);

  FStream := S;
  // Loop through the content stream and process all operators
  repeat
    SkipWhitespace(S);
    if S.Position >= S.Size then break;

    // Load a new operator from the content stream
    try
      AOperator := LoadOperator(Document, S);
    except
      DoDebugMessage(dlWarning, sPdfExpectOperator);
      if S.Position > 0 then begin
        SetLength(Info, S.Position);
        S.Position := 0;
        S.Read(Info[1], length(Info));
      end;
      DoDebugMessage(dlWarning, 'content stream up till problem:');
      if length(Info) > 100 then
        Info := '...' + Copy(Info, length(Info) - 100, length(Info));
      DoDebugMessage(dlWarning, Info);
    end;

    // Check
    if not assigned(AOperator) then
      break;

    // DoDebugMessage(dlInfo, Format('Processing operator %s', [AOperator.Value]));
    // Process the operator
    try
      ProcessResult := ProcessOperator(AOperator);

      // post processing
      case ProcessResult of
{      cProcessNotImpl:
        if IsStrict then
          DoDebugMessage(dlInfo, Format(sPdfOperatorNotImpl, [AOperator.Value]));}
      cProcessUnknown:
        DoDebugMessage(dlInfo, Format(sPdfUnknownOperator, [AOperator.Value]));
      cProcessWarn:
        DoDebugMessage(dlWarning, Format(sPdfOperatorWarning, [AOperator.Value]));
      cProcessErr:
        DoDebugMessage(dlWarning, Format(sPdfOperatorError, [AOperator.Value]));
      end;

    finally
      // Free the processed operator
      FreeAndNil(AOperator);
    end;
  until False;
end;

procedure TPdfParser.PopState;
begin
  if FStates.Count <= 1 then begin
    DoDebugMessage(dlWarning, sPdfStateNestingErr);
    exit;
  end;
  FStates.Delete(0);
end;

function TPdfParser.Process_gs(Operator: TPdfOperator): integer;
var
  i: integer;
  DictName: string;
  ADict: TPdfDictionary;
begin
  // Assume we process it
  Result := cProcessOK;
  with Operator do begin

    // Find the dictionary with graphics state
    DictName := Args[0].AsString;
    ADict := Resources.DictionaryByKey('ExtGState');
    if not assigned(ADict) then begin
      DoDebugMessage(dlWarning, sPdfExtGStNotExist);
      exit;
    end;
    ADict := ADict.DictionaryByKey(DictName);
    if not assigned(ADict) then begin
      DoDebugMessage(dlWarning, sPdfExtGStNotExist);
      exit;
    end;
    // We now have the dictionary, we must apply each element to the graphics state
    for i := 0 to ADict.EntryCount - 1 do begin
      case State.SetStateParameter(ADict.Entries[i].Key, ADict.Entries[i].Value) of
{      cProcessNotImpl:
        DoDebugMessage(dlWarning, Format(sPdfStateParamNotImpl, [ADict.Entries[i].Key]));}
      cProcessUnknown:
        DoDebugMessage(dlWarning, Format(sPdfStateParamUnknown, [ADict.Entries[i].Key]));
      end;//case
    end;
    RefreshShapes;
    exit;

  end;
  // Arriving here means we did not process it
  Result := cProcessWarn;
end;

function TPdfParser.ProcessOperator(Operator: TPdfOperator): integer;
// This huge method does an efficient breakdown of the operator string and chooses
// the correct processing for it.

// the result of this function is any of the cProcessXXXX constants

var
  i: integer;
  C1, C2, C3: char;
  AOper: string;
begin
  Result := cProcessOK;

  // Operator breakdown
  AOper := Operator.Value;

  // Deal with q and Q separately since they're often concanated
  while (length(AOper) > 0) and (AOper[1] in ['q', 'Q']) do begin
    case AOper[1] of
    'q': PushState;
    'Q': PopState;
    end;//case
    AOper := copy(AOper, 2, length(AOper));
    if Length(AOper) = 0 then exit;
  end;

  // Other operators
  case length(AOper) of
  0: raise EPdfError.Create(sPdfExpectOperator);
  1:
    begin
      C1 := AOper[1];
      C2 := ' ';
      C3 := ' ';
    end;
  2:
    begin
      C1 := AOper[1];
      C2 := AOper[2];
      C3 := ' ';
    end;
  3:
    begin
      C1 := AOper[1];
      C2 := AOper[2];
      C3 := AOper[3];
    end;
  else
    // operator is more than 3 characters
    raise EPdfError.Create(sPdfIllegalOperator);
  end;

  // Select on 1st, 2nd and 3rd character using a CASE
  case C1 of
  'b':
    case C2 of
    ' ':// closepath, fill, stroke nonzero
      Result := cProcessNotImpl;
    '*':// closepath, fill, stroke even-odd
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'B':
    case C2 of
    ' ':// fill, stroke nonzero
      Result := cProcessNotImpl;
    '*':// close, fill, stroke even-odd
      Result := cProcessNotImpl;
    'D':// begin marked content w. property list
      Result := cProcessNotImpl;
    'I':// begin inline image object
      Result := DrawInlineImage(FStream);
    'M':// begin marked content
      Result := cProcessNotImpl;
    'T':// begin text object
      begin
        FreeAndNil(FText);
        State.TextMatrix := cPdfIdentityMatrix;
        State.TextLineMatrix := cPdfIdentityMatrix;
        FText := TPdfTextShape.Create(Self);
        Mode := gmText;
      end;
    'X':// begin compatibility section
      inc(FCompatCount);
    else
      Result := cProcessUnknown;
    end;
  'c':
    case C2 of
    ' ':// curveto 3 control pts
      Result := cProcessNotImpl;
    'm':// concat matrix to ctm
      begin
        // Adapt CTM
        State.CTM := PdfMatrixMultiply(PdfMatrixFromOperator(Operator), State.CTM);
      end;
    's':// setcolorspace nonstroking
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'C':
    case C2 of
    'S':// setcolorspace stroking
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'd':
    case C2 of
    ' ':// set linedash pattern
      Result := cProcessNotImpl;
    '0':// set glyphwidth in type3 font
      Result := cProcessNotImpl;
    '1':// set glyphwidth and bb in type3 font
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'D':
    case C2 of
    'o':// Invoke named XObject
      Result := DrawXObject(Operator.Args[0].AsString);
    'P':;// defined marked-content point with property list
    else
      Result := cProcessUnknown;
    end;
  'E':
    case C2 of
    'I':// end inline image object
      Result := cProcessNotImpl;
    'M':// end marked-content sequence
      Result := cProcessNotImpl;
    'T':// end text object
      begin
        ShapeAdd(FText);
        FText := nil;
        Mode := gmPage;
      end;
    'X':// end compatibility section
      begin
        dec(FCompatCount);
        if FCompatCount < 0 then
          raise EPdfError.Create(sPdfCompatNestingErr);
      end;
    else
      Result := cProcessUnknown;
    end;
  'f':
    case C2 of
    ' ':// fill path nonzero
      Result := cProcessNotImpl;
    '*':// fill path even-odd
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'F': // fill path nonzero (obsolete)
    Result := cProcessNotImpl;
  'G': // set gray level stroking
    begin
      State.StrokeColor.Space := csDeviceGray;
      State.StrokeColor.Channels[0] := Operator.Args[0].AsNumber;
      RefreshShapes;
    end;
  'g':
    case C2 of
    ' ':// set gray level nonstroking
      begin
        State.PaintColor.Space := csDeviceGray;
        State.PaintColor.Channels[0] := Operator.Args[0].AsNumber;
        RefreshShapes;
      end;
    's':// set parameters for graphics state dict
      Result := Process_gs(Operator);
    else
      Result := cProcessUnknown;
    end;
  'h': // close path
    Result := cProcessNotImpl;
  'i': // set flatness tol
    Result := cProcessNotImpl;
  'I': // (ID) begin inline data
    Result := cProcessNotImpl;
  'j': // set line join style
    Result := cProcessNotImpl;
  'J': // set line cap style
    Result := cProcessNotImpl;
  'K': // set cmyk color stroking
    begin
      State.StrokeColor.Space := csDeviceCMYK;
      for i := 0 to 3 do
        State.StrokeColor.Channels[i] := Operator.Args[i].AsNumber;
      RefreshShapes;
    end;
  'k': // set cmyk color nonstroking
    begin
      State.PaintColor.Space := csDeviceCMYK;
      for i := 0 to 3 do
        State.PaintColor.Channels[i] := Operator.Args[i].AsNumber;
      RefreshShapes;
    end;
  'l': // lineto
    Result := cProcessNotImpl;
  'm': // moveto
    Result := cProcessNotImpl;
  'M':
    case C2 of
    ' ':// set miter limit
      Result := cProcessNotImpl;
    'P':// define marked-content point
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'n': // end path without filling or stroking
    Result := cProcessNotImpl;
  'r':
    case C2 of
    'e':// append rectangle to path
      Result := cProcessNotImpl;
    'g':// set rgb color nonstroking
      begin
        State.PaintColor.Space := csDeviceRGB;
        for i := 0 to 2 do
          State.PaintColor.Channels[i] := Operator.Args[i].AsNumber;
        RefreshShapes;
      end;
    'i':// set color rendering intent
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'R': //(RG) set rgb color stroking
    begin
      State.StrokeColor.Space := csDeviceRGB;
      for i := 0 to 2 do
        State.StrokeColor.Channels[i] := Operator.Args[i].AsNumber;
      RefreshShapes;
    end;
  's':
    case C2 of
    ' ':// close and stroke path
      Result := cProcessNotImpl;
    'c':
      case C3 of
      ' ':// set color nonstroking
        Result := cProcessNotImpl;
      'n':// set color nonstroking (icc and special)
        Result := cProcessNotImpl;
      else
        Result := cProcessNotImpl;
      end;
    'h':// paint area defined by shading pattern
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'S':
    case C2 of
    ' ':// stroke path
      Result := cProcessNotImpl;
    'C':
      case C3 of
      ' ':// set color stroking
        Result := cProcessNotImpl;
      'N':// set color stroking (icc and special)
        Result := cProcessNotImpl;
      else
        Result := cProcessUnknown;
      end;
    else
      Result := cProcessNotImpl;
    end;
  'T':
    case C2 of
    '*':// move to start of next line
      ShiftLineMatrix(0, -State.TextLeading);

    'c':// set character spacing
      State.TextCharSpacing := Operator.Args[0].AsNumber;

    'd':// move text position
      ShiftLineMatrix(Operator.Args[0].AsNumber, Operator.Args[1].AsNumber);

    'D':// move text position and set leading
      begin
        ShiftLineMatrix(Operator.Args[0].AsNumber, Operator.Args[1].AsNumber);
        State.TextLeading := -Operator.Args[1].AsNumber;
      end;

    'f':// set text font and size
      begin
        // Retrieve the font and set the pointer
        State.TextFont := RetrieveFontFromDict(
          PdfFontDictFromResourceDict(Resources, Operator.Args[0].AsString));
        State.TextFontSize := Operator.Args[1].AsNumber;
        RefreshTextShape;
      end;

    'j':// show text
      Text.AddGlyphString(Operator.Args[0].AsString);

    'J':// show text, allow indiv glyph pos
      with Operator.Args[0] as TPdfArray do begin
        // Loop through all array elements and process them
        for i := 0 to ElementCount - 1 do begin
          if Elements[i] is TPdfString then begin
            // Add the string
            Text.AddGlyphString(Elements[i].AsString);
          end else  if Elements[i] is TPdfNumber then begin
            // Move the caret
            Text.AdjustSpacing(-Elements[i].AsNumber);
          end;
        end;
      end;

    'L':// set text leading
      State.TextLeading := Operator.Args[0].AsNumber;

    'm':// set text matrix and text line matrix
      begin
        State.TextLineMatrix := PdfMatrixFromOperator(Operator);
        State.TextMatrix     := State.TextLineMatrix;
      end;

    'r':// set text rendering mode
      begin
        State.TextRenderMode := Operator.Args[0].AsInteger;
        RefreshTextShape;
      end;
    's':// set text rise
      Result := cProcessNotImpl;
    'w':// set word spacing
      Result := cProcessNotImpl;
    'z':// set horizontal text scaling
      begin
        State.TextHorScale := Operator.Args[0].AsNumber / 100;
        RefreshTextShape;
      end;
    else
      Result := cProcessUnknown;
    end;
  'v':// curveto, replicate 1st point
    Result := cProcessNotImpl;

  'w':// set line width
    State.TextWordSpacing := Operator.Args[0].AsNumber;

  'W':
    case C2 of
    ' ':// set clipping path nonzero
      Result := cProcessNotImpl;
    '*':// set clipping path even-odd
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'y':// curveto, replicate 4th point
    Result := cProcessNotImpl;

  '''': // move to next line and show text
    begin
      // Go to next line
      ShiftLineMatrix(0, -State.TextLeading);
      // Add the string
      Text.AddGlyphString(Operator.Args[0].AsString);
    end;

  '"': // set word and char spacing, move to next line and show text
    begin
      State.TextWordSpacing := Operator.Args[0].AsNumber;
      State.TextCharSpacing := Operator.Args[1].AsNumber;
      // Go to next line
      ShiftLineMatrix(0, -State.TextLeading);
      // Add the string
      Text.AddGlyphString(Operator.Args[2].AsString);
    end;

  else
    Result := cProcessUnknown;
  end;//case

end;

procedure TPdfParser.PushState;
// Create new state, assign the current state to it, and insert at position 0
var
  NewState: TPdfGraphicsState;
begin
  NewState := TPdfGraphicsState.Create(Self);
  NewState.Assign(State);
  FStates.Insert(0, NewState);
end;

procedure TPdfParser.RefreshBitmapShape;
begin
// To do
end;

procedure TPdfParser.RefreshPathShape;
begin
  // Some changes require a new shape, since our path shapes can only
  // deal with a limited set of object changes
  if assigned(Path) and
     ((State.LineWidth <> Path.LineWidth) or
     (not State.StrokeColor.Equals(Path.StrokeColor)) or
     (not State.PaintColor.Equals(Path.PaintColor))) then begin
    ShapeAdd(Path);
    Path := TPdfPathShape.Create(Self);
  end;
end;

procedure TPdfParser.RefreshShapes;
begin
  RefreshTextShape;
  RefreshPathShape;
  RefreshBitmapShape;
end;

procedure TPdfParser.RefreshTextShape;
begin
  // Some changes require a new shape, since our text shapes can only
  // deal with a limited set of object changes
  if assigned(Text) and
    ((State.TextFont       <> Text.Font) or
     (State.TextFontSize   <> Text.FontSize) or
     (State.TextRenderMode <> Text.RenderMode) or
     (not State.StrokeColor.Equals(Text.StrokeColor)) or
     (not State.PaintColor.Equals(Text.PaintColor))) then begin
    ShapeAdd(Text);
    Text := TPdfTextShape.Create(Self);
  end;
end;

function TPdfParser.RetrieveFontFromDict(Dict: TPdfDictionary): TPdfFont;
// Find the font dictionary, create and add it to the Fonts table in the document
// if not yet present.
begin
  Result := TPdfDocument(Document).FontByDict(Dict);
  if not assigned(Result) then begin
    Result := PdfCreateFontFromDict(Dict);
    TPdfDocument(Document).FontAdd(Result);
  end;
end;

procedure TPdfParser.ShapeAdd(AShape: TPdfShape);
begin
  if assigned(FShapes) and assigned(AShape) then
    // We only add shapes that have content
    if AShape.IsEmpty then
      AShape.Free
    else
      FShapes.Add(AShape);
end;

procedure TPdfParser.ShiftLineMatrix(ShiftX, ShiftY: PdfFloat);
var
  Shift: TPdfMatrixStruct;
begin
  Shift := PdfMatrix(1, 0, 0, 1, ShiftX, ShiftY);
  State.TextLineMatrix := PdfMatrixMultiply(Shift, State.TextLineMatrix);
  State.TextMatrix     := State.TextLineMatrix;
end;

{ TPdfShape }

constructor TPdfShape.Create(AParser: TPdfParser);
begin
  inherited Create;
  FParser := AParser;
end;

function TPdfShape.IsEmpty: boolean;
begin
  Result := True;
end;

{ TPdfGraphicShape }

constructor TPdfGraphicShape.Create(AParser: TPdfParser);
begin
  inherited Create(AParser);
  FStrokeColor := TPdfColor.Create;
  FPaintColor := TPdfColor.Create;
  // set defaults
  FStrokeColor.Assign(AParser.State.StrokeColor);
  FPaintColor.Assign(AParser.State.PaintColor);
end;

destructor TPdfGraphicShape.Destroy;
begin
  FreeAndNil(FStrokeColor);
  FreeAndNil(FPaintColor);
  inherited;
end;

{ TPdfPathShape }

constructor TPdfPathShape.Create(AOwner: TPdfParser);
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create;
  FLineWidth := AOwner.State.LineWidth;
end;

destructor TPdfPathShape.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TPdfPathShape.GetItemCount: integer;
begin
  Result := 0;
  if assigned(FItems) then Result := FItems.Count;
end;

function TPdfPathShape.GetItems(Index: integer): TPdfPathItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < ItemCount) then
    Result := TPdfPathItem(FItems[Index]);
end;

{ TPdfGraphicsState }

procedure TPdfGraphicsState.Assign(Source: TPersistent);
var
  AState: TPdfGraphicsState;
begin
  if Source is TPdfGraphicsState then begin
    AState           := TPdfGraphicsState(Source);
    // Copy from source
    CTM              := AState.CTM;
    PaintColor.Assign(AState.PaintColor);
    StrokeColor.Assign(AState.StrokeColor);
    LineWidth        := AState.LineWidth;
    TextFont         := AState.TextFont;
    TextFontSize     := AState.TextFontSize;
    TextMatrix       := AState.TextMatrix;
    TextLineMatrix   := AState.TextLineMatrix;
    TextHorScale     := AState.TextHorScale;
    TextCharSpacing  := AState.TextCharSpacing;
    TextWordSpacing  := AState.TextWordSpacing;
    TextLeading      := AState.TextLeading;
    TextRise         := AState.TextRise;
  end else
    inherited;
end;

constructor TPdfGraphicsState.Create(AOwner: TPdfParser);
begin
  inherited Create;
  FOwner := AOwner;
  FPaintColor := TPdfColor.Create;
  FStrokeColor := TPdfColor.Create;
  // Defaults (where different from 0 or false)
  FCTM := cPdfIdentityMatrix;
  FLineWidth := 1.0;
  FTextMatrix := cPdfIdentityMatrix;
  FTextLineMatrix := cPdfIdentityMatrix;
  FTextHorScale := 1.0;
end;

destructor TPdfGraphicsState.Destroy;
begin
  FreeAndNil(FPaintColor);
  FreeAndNil(FStrokeColor);
  inherited;
end;

function TPdfGraphicsState.SetStateParameter(AName: string; AValue: TPdfObject): integer;
var
  C1, C2, C3: char;
begin
  Result := cProcessOK;
  case length(AName) of
  0:
    begin
      Result := cProcessUnknown;
      exit;
    end;
  1:
    begin
      C1 := AName[1];
      C2 := ' ';
      C3 := ' ';
    end;
  2:
    begin
      C1 := AName[1];
      C2 := AName[2];
      C3 := ' ';
    end;
  else
    begin
      C1 := AName[1];
      C2 := AName[2];
      C3 := AName[3];
    end;
  end;

  case C1 of
  'A':
    case C2 of
    'I': //(AIS) Alpha is shape
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'B':
    case C2 of
    'G':
      case C3 of
      ' ': // black generation
        Result := cProcessNotImpl;
      '2': // black generation default
        Result := cProcessNotImpl;
      else
        Result := cProcessUnknown;
      end;
    'M': // blend mode
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'c':
    case C2 of
    'a': // alpha constant, non-stroking
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'C':
    case C2 of
    'A': // alpha constant, stroking
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'D':
    case C2 of
    ' ': // dash pattern
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'F':
    case C2 of
    'L': // flatness tolerance
      Result := cProcessNotImpl;
    'o': //(Font) // Font and size
      begin
        if not (AValue is TPdfArray) then
          raise EPdfError.Create(sPdfExpectArray);
        TextFont := Owner.RetrieveFontFromDict(TPdfDictionary(DerefPdf(TPdfArray(AValue).Elements[0])));
        TextFontSize := TPdfArray(AValue).Elements[1].AsNumber;
      end;
    else
      Result := cProcessUnknown;
    end;
  'H':
    case C2 of
    'T': // Halftone
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'L':
    case C2 of
    'C': // line cap
      Result := cProcessNotImpl;
    'J': // line join
      Result := cProcessNotImpl;
    'W': // linewidth
      LineWidth := AValue.AsNumber;
    else
      Result := cProcessUnknown;
    end;
  'M':
    case C2 of
    'L': // miter limit
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'o':
    case C2 of
    'p': // overprint control, non-stroking
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'O':
    case C2 of
    'P':
      case C3 of
      ' ': // overprint control, stroking or both
        Result := cProcessNotImpl;
      'M': // overprint mode
        Result := cProcessNotImpl;
      else
        Result := cProcessUnknown;
      end;
    else
      Result := cProcessUnknown;
    end;
  'R':
    case C2 of
    'I': // rendering intent
      Result := cProcessNotImpl;
    else
      Result := cProcessUnknown;
    end;
  'S':
    case C2 of
    'A': // automatic stroke adjustment
      Result := cProcessNotImpl;
    'M':
      case C3 of
      ' ':// smoothness tolerance
        Result := cProcessNotImpl;
      'a': //(SMask) soft mask
        Result := cProcessNotImpl;
      else
        Result := cProcessUnknown;
      end;
    else
      Result := cProcessUnknown;
    end;
  'T':
    case C2 of
    'y': // (Type) - type of dictionary
      begin
        if AValue.AsString <> 'ExtGState' then
          Result := cProcessErr;
      end;
    'K': // text knockout
      Result := cProcessNotImpl;
    'R':
      case C3 of
      ' ': // Transfer function
        Result := cProcessNotImpl;
      '2': // Transfer function default
        Result := cProcessNotImpl;
      else
        Result := cProcessUnknown;
      end;
    else
      Result := cProcessUnknown;
    end;
  'U':
    case C2 of
    'C':
      case C3 of
      'R':
        if AName = 'UCR' then
          // Undercolor removal
          Result := cProcessNotImpl
        else if AName = 'UCR2' then
          // Undercolor removal default
          Result := cProcessNotImpl
        else
          Result := cProcessUnknown;
      else
        Result := cProcessUnknown;
      end;
    else
      Result := cProcessUnknown;
    end;
  else
    Result := cProcessUnknown;
  end;
end;

{ TPdfTextShape }

procedure TPdfTextShape.AddGlyphChar(Value: string);
var
  AGlyph: TPdfGlyph;
  RM: TPdfMatrixStruct;
  Spacing: PdfFloat;
  ShiftX, ShiftY: PdfFloat;
begin
  AGlyph := TPdfGlyph.Create;
  AGlyph.Text := Value;
  with Parser.State do begin
    // Calculate temporary rendering matrix
    RM := PdfMatrix(TextFontSize * TextHorScale, 0, 0, TextFontSize, 0, TextRise);
    // Assign this to the glyphs matrix
    AGlyph.Position := PdfMatrixMultiply(RM, TextMatrix, CTM);
    // Update the state text matrix and text line matrix
    // Tx := ((w0 - Tj / 1000) * Tfs + Tc + Tw) * Th;
    Spacing := TextCharSpacing;
    if AGlyph.Char = #32 then
      Spacing := Spacing + TextWordSpacing;
    ShiftX := 0; ShiftY := 0;
    case Font.WritingMode of
    0:// Horizontal
      ShiftX := (Font.Widths[ord(AGlyph.Char)] * 0.001 * TextFontSize + Spacing) * TextHorScale;
    1:// Vertical
      ShiftY := Font.Widths[ord(AGlyph.Char)] * 0.001 * TextFontSize + Spacing;
    end;
    // Multiply
    TextMatrix := PdfMatrixTranslate(TextMatrix, ShiftX, ShiftY);
  end;
  GlyphAdd(AGlyph);
end;

procedure TPdfTextShape.AddGlyphString(Value: string);
// Add all the glyphs in the string to the text shape
// to do: this does not work for composite fonts yet. These can be multibyte glyphs,
// and should be treated differently (chapter 5.6)
var
  i: integer;
begin
  for i := 1 to length(Value) do
    AddGlyphChar(Value[i]);
end;

procedure TPdfTextShape.AdjustSpacing(Value: PdfFloat);
// Adjust the spacing.. note that Value is in 1000th of text units (glyph units)
var
  ShiftX, ShiftY: PdfFloat;
begin
  with Parser.State do begin
    ShiftX := 0; ShiftY := 0;
    case Font.WritingMode of
    0:// Horizontal
      ShiftX := Value * 0.001 * TextFontSize * TextHorScale;
    1:// Vertical
      ShiftY := Value * 0.001 * TextFontSize;
    end;
    // Multiply
    TextMatrix := PdfMatrixTranslate(TextMatrix, ShiftX, ShiftY);
  end;
end;

constructor TPdfTextShape.Create(AOwner: TPdfParser);
begin
  inherited Create(AOwner);
  FGlyphs := TObjectList.Create;
  // Copy from owner
  with AOwner.State do begin
    Font       := TextFont; // this is a pointer to the font in the font table in the document
    FontSize   := TextFontSize;
    RenderMode := TextRenderMode;
  end;
end;

destructor TPdfTextShape.Destroy;
begin
  FreeAndNil(FGlyphs);
  inherited;
end;

function TPdfTextShape.GetGlyphCount: integer;
begin
  Result := 0;
  if assigned(FGlyphs) then Result := FGlyphs.Count;
end;

function TPdfTextShape.GetGlyphs(Index: integer): TPdfGlyph;
begin
  Result := nil;
  if (Index >= 0) and (Index < GlyphCount) then
    Result := TPdfGlyph(FGlyphs[Index]);
end;

procedure TPdfTextShape.GlyphAdd(AGlyph: TPdfGlyph);
begin
  if assigned(FGlyphs) and assigned(AGlyph) then
    FGlyphs.Add(AGlyph);
end;

procedure TPdfTextShape.GlyphDelete(Index: integer);
begin
  if assigned(FGlyphs) and (Index >= 0) and (Index < GlyphCount) then
    FGlyphs.Delete(Index)
end;

function TPdfTextShape.IsEmpty: boolean;
begin
  Result := GlyphCount = 0;
end;

{ TPdfGlyph }

function TPdfGlyph.GetChar: char;
begin
  Result := #0;
  if length(FText) > 0 then
    Result := FText[1];
end;

function TPdfGlyph.GetCharCode: integer;
begin
  Result := ord(Char);
end;

{ TPdfFormXObjectRef }

function TPdfFormXObjectRef.IsEmpty: boolean;
begin
  Result := False;
end;

end.
