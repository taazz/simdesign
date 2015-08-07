{ unit sdPdfImages

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the objects as described in the PDF reference
  chapter 4.8: Images

  Author: Nils Haeck M.Sc.

  Changes:
    15Jan2004 - Created
    26Sep2005 - Added colorspace unknown check
    28Sep2005 - Added inline image
    28Sep2005 - Added ImageMask and Decode

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfImages;

interface

uses
  Classes, SysUtils, sdPdfObjects, sdPdfContentStream, sdPdfColors,
  sdPdfCoordinateSpace, sdPdfUtil, sdPdfFilters;

type

  TPdfDecodeArray = array of PdfFloat;

  TPdfImageShape = class(TPdfShape)
  private
    FPosition: TPdfMatrixStruct; // Position relative to base
    FSource: TMemoryStream;      // Owned memory stream that will hold uncompressed image bits
    FHeight: integer;            // Image height in pixels
    FWidth: integer;             // Image width in pixels
    FColor: TPdfColor;           // Color used for image masks
    FBitsPerComponent: integer;  // Number of bits per component
    FImageMask: boolean;
    FDecode: TPdfDecodeArray;
    function GetChannelCount: integer;
    procedure SetColor(const Value: TPdfColor); // pointer to a color space dictionary
  protected
    procedure ReadImageProperties(ADict: TPdfDictionary; IsInline: boolean = False);
    procedure CheckSourceConsistency;
  public
    constructor Create(AParser: TPdfParser); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    function IsEmpty: boolean; override;
    property Decode: TPdfDecodeArray read FDecode;
    property Position: TPdfMatrixStruct read FPosition write FPosition;
    property Source: TMemoryStream read FSource write FSource;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property ChannelCount: integer read GetChannelCount;
    property Color: TPdfColor read FColor write SetColor;
    property BitsPerComponent: integer read FBitsPerComponent write FBitsPerComponent;
    property ImageMask: boolean read FImageMask;
  end;

  TPdfImageXObject = class(TPdfImageShape)
  private
    FStream: TPdfStream;
    procedure SetStream(const Value: TPdfStream);
  public
    property Stream: TPdfStream read FStream write SetStream;
  end;

  TPdfInlineImageDictionary = class(TPdfDictionary)
  public
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
  end;

  TPdfInlineImageStream = class(TPdfStream)
  protected
    procedure LoadStreamFromPdf(Pdf: TObject; S: TStream); override;
    procedure GetFilterParams(var AChain, AParms: TPdfObject); override;
    function FirstFilterName: string;
  public
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
  end;

  TPdfInlineImage = class(TPdfImageShape)
  private
    FStream: TPdfInlineImageStream;
    procedure SetStream(const Value: TPdfInlineImageStream);
  public
    property Stream: TPdfInlineImageStream read FStream write SetStream;
  end;

resourcestring

  sImageSourceIsInconsistent = 'Image source is inconsistent';
  sImageMaskWithInvalidBpc   = 'Image mask with bits per component unequal 1';
  sEmptyImageNotAllowed      = 'Empty image not allowed';
  sColorSpaceInfoMissing     = 'Color space info missing';

implementation

uses
  sdPdfDocument;

{ TPdfImageShape }

procedure TPdfImageShape.CheckSourceConsistency;
var
  AScanWidth: integer;
begin
  // In case of unknown, this is technically an error, but we might try to
  // estimate from the size of the stream.. (Some poor PDF's do end up here)
  if FColor.Space = csUnknown then begin
    case BitsPerComponent of
    1:
      begin
        AScanWidth := (FWidth + 7) div 8;
        if FSource.Size div (AScanWidth * FHeight) = 1 then
          FColor.Space := csDeviceGray;
      end;
    8:
      begin
        if (FWidth > 0) and (FSource.Size > 0) and
          (FSource.Size mod (FWidth * FHeight) = 0) then
          case FSource.Size div (FWidth * FHeight) of
          1: FColor.Space := csDeviceGray;
          3: FColor.Space := csDeviceRGB;
          end;//case
      end;
    end;//case
  end;

  // Check if the size of the source is OK
  if not FSource.Size = (FWidth * FHeight * ChannelCount * FBitsPerComponent) div 8 then
    raise Exception.Create(sImageSourceIsInconsistent);
end;

constructor TPdfImageShape.Create(AParser: TPdfParser);
begin
  inherited Create(AParser);
  FColor := TPdfColor.Create;
  FSource := TMemoryStream.Create;
end;

destructor TPdfImageShape.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FColor);
  inherited;
end;

procedure TPdfImageShape.ReadImageProperties(ADict: TPdfDictionary; IsInline: boolean);
var
  i: integer;
  DecodeArray: TPdfArray;
  ASpace: TPdfColorSpace;
  ASpaceDict: TPdfObject;
  ColorSpaceInfo: TPdfObject;
begin
  DecodeArray := nil;
  ColorSpaceInfo := nil;
  with ADict do
  begin

    // Width and height
    if IsInline then
      FWidth  := IntegerByKeyDefault('W', 0);
    if not IsInline or (FWidth = 0) then
      FWidth  := IntegerByKeyDefault('Width', 0);
    if IsInline then
      FHeight := IntegerByKeyDefault('H', 0);
    if not IsInline or (FHeight = 0) then
      FHeight := IntegerByKeyDefault('Height', 0);

    // Check them, and give error if they are empty
    if IsEmpty then
      raise Exception.Create(sEmptyImageNotAllowed);

    if IsInline then
      FBitsPerComponent := IntegerByKeyDefault('BPC', 0);
    if not IsInline or (FBitsPerComponent = 0) then
      FBitsPerComponent := IntegerByKeyDefault('BitsPerComponent', 0);

    // Image mask
    if IsInline then
      FImageMask := BooleanByKeyDefault('IM', False);
    if not IsInline or (FImageMask = False) then
      FImageMask := BooleanByKeyDefault('ImageMask', False);

    // If we have an image mask, bits per component should be 1 and we will
    // use the color and colorspace of the current state.
    if FImageMask then
    begin

      if FBitsPerComponent = 0 then
        FBitsPerComponent := 1;
      if FBitsPerComponent <> 1 then
        raise Exception.Create(sImageMaskWithInvalidBpc);

    end else
    begin

      // Get color space object
      if IsInline then
        ColorSpaceInfo := ValueByKey('CS');
      if not IsInline or not assigned(ColorSpaceInfo) then
        ColorSpaceInfo := DerefPdf(ValueByKey('ColorSpace'));

      // Construct color space
      PdfColorSpaceFromValue(ColorSpaceInfo, ASpace, ASpaceDict, IsInline);
      FColor.Space := ASpace;
      FColor.Dict := ASpaceDict;

      // We *must* have color space info now, if not it is an error
      if FColor.Space = csUnknown then
        raise Exception.Create(sColorSpaceInfoMissing);

    end;

    // Decode arrays
    if IsInline then
      DecodeArray := ArrayByKey('D');
    if not IsInline or not assigned(DecodeArray) then
      DecodeArray := ArrayByKey('Decode');
    if assigned(DecodeArray) then
    begin

      // Build decode tables
      SetLength(FDecode, ChannelCount * 2);
      for i := 0 to ChannelCount * 2 - 1 do
        FDecode[i] := DecodeArray.Elements[i].AsNumber;

    end;

  end;
end;

procedure TPdfImageShape.Clear;
begin
  FWidth := 0;
  FHeight := 0;
  FImageMask := False;
  SetLength(FDecode, 0);
  // other things later
end;

procedure TPdfImageShape.SetColor(const Value: TPdfColor);
begin
  if FColor <> Value then
    FColor.Assign(Value);
end;

{ TPdfImageXObject }

procedure TPdfImageXObject.SetStream(const Value: TPdfStream);
// Decompress the stream into the Source TMemoryStream and store image props
begin
  FStream := Value;
  Clear;
  Source.Clear;
  if not assigned(FStream) then exit;

  ReadImageProperties(FStream.Dict, False);

  FStream.DecompressToStream(FSource);
end;

function TPdfImageShape.GetChannelCount: integer;
begin
  if FImageMask then
    Result := 1
  else
    Result := ChannelCountFromColorSpace(FColor.Space, FColor.Dict);
end;

function TPdfImageShape.IsEmpty: boolean;
begin
  Result := Width * Height = 0;
end;

{ TPdfInlineImageDictionary }

procedure TPdfInlineImageDictionary.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Ch: char;
  AEntry: TPdfKeyValue;
begin
  // At this moment the "BI" has already been read

  // Clear the item list
  Clear;
  // Read name / value pairs
  repeat
    // Read name
    Ch := ReadNextNonWS(S);
    // We expect a key/value pair, with /name
    if Ch = '/' then begin
      S.Seek(-1, soFromCurrent);
      AEntry := TPdfKeyValue.Create;
      AEntry.LoadFromPdf(Pdf, S);
      EntriesAdd(AEntry);
    end else
      break;
  until False;
  // Read "ID"
  if Ch <> 'I' then
    raise EPdfError.Create(sPdfExpectDictClose);
  S.Read(Ch, 1);
  if Ch <> 'D' then
    raise EPdfError.Create(sPdfExpectDictClose);
end;

{ TPdfInlineImageStream }

function TPdfInlineImageStream.FirstFilterName: string;
var
  Filter, AParms: TPdfObject;
begin
  Result := '';
  GetFilterParams(Filter, AParms);
  // Single filter?
  if (Filter is TPdfName) then
    Result := Filter.AsString
  else
    // Array of filters?
    if (Filter is TPdfArray) then
      if TPdfArray(Filter).ElementCount > 0 then
        Result := TPdfArray(Filter).StringByIndex(0);
end;

procedure TPdfInlineImageStream.GetFilterParams(var AChain, AParms: TPdfObject);
begin
  // Filter chain and parameters, handle abbrevs
  AChain := Dict.ValueByKey('F');
  if not assigned(AChain) then
    AChain := Dict.ValueByKey('Filter');
  AParms := Dict.ValueByKey('DP');
  if not assigned(AParms) then
    AParms := Dict.ValueByKey('DecodeParms');
end;

procedure TPdfInlineImageStream.LoadFromPdf(Pdf: TObject; S: TStream);
{var
  Start, Close: integer;
  Line, Info: string;}
begin
  // Recreate and load dictionary
//Start := S.Position - 2;
  Dict := TPdfInlineImageDictionary.Create;
  Dict.LoadFromPdf(Pdf, S);
  // Load stream
  LoadStreamFromPdf(Pdf, S);
{  // debug: show this
  Close := S.Position;
  S.Position := Start;
  SetLength(Info, Close - Start);
  S.Read(Info[1], Close - Start);
  S.Position := Close;
  Info := IntToStr(S.Position) + ' ' + IntToStr(S.Size) + ' ' + Info;
  Info := StringReplace(Info, #13, ' ', [rfReplaceAll]);
  Info := StringReplace(Info, #10, ' ', [rfReplaceAll]);
  if assigned(Pdf) then
    while length(Info) > 0 do begin
      Line := copy(Info, 1, 50);
      Info := copy(Info, 51, length(Info));
      TPdfDocument(Pdf).DoDebugMessage(dlInfo, Line);
    end;}
end;

procedure TPdfInlineImageStream.LoadStreamFromPdf(Pdf: TObject; S: TStream);
// In inline images there's no "Length" field, so the stream length is determined
// by searching for the end sequence "EI", unless the filter type has specific
// termination.
var
  Ch: char;
  Start, Close: integer;
  EndPhrase: string;
  AName: string;
begin
  FreeAndNil(FStream);
  FOwner := Pdf;

  // Search end sequence "EI" by default
  EndPhrase := 'EI';
  AName := FirstFilterName;
  if (AName = 'A85') or (AName = 'AHx') then begin
    // Start of the stream
    Ch := ReadNextNonWS(S);
    Start := S.Position - 1;
    S.Position := Start;
    if AName = 'A85' then
      // We use this endphrase
      EndPhrase := '~>'
  end else begin
    // Skip one blank
    S.Read(Ch, 1);
    Start := S.Position;
  end;

  // to do: other end phrases for specific filters, like ~> for Ascii85
  // Note: there's a risk seeking for the endphrase, since in some of the filter
  // data the "EI" may appear! Don't know how to handle that now. Perhaps it's
  // not allowed by the spec, but it's not mentioned anywhere.

  Close := PosInStream(S, EndPhrase);
  if EndPhrase <> 'EI' then
    Close := PosInStream(S, 'EI');

  // We now have a segment that defines our stream
  FStream := TMemoryStream.Create;
  S.Position := Start;
  FStream.CopyFrom(S, Close - Start);

  // Leave the stream after "EI"
  S.Position := Close + 2;
end;

{ TPdfInlineImage }

procedure TPdfInlineImage.SetStream(const Value: TPdfInlineImageStream);
// Decompress the stream into the Source TMemoryStream and store image props
begin
  FStream := Value;
  Clear;
  Source.Clear;
  if not assigned(FStream) then exit;

  ReadImageProperties(FStream.Dict, True);

  FStream.DecompressToStream(FSource);
end;

end.
