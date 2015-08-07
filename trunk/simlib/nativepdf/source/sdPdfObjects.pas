{ unit sdPdfObjects

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the objects as described in the PDF reference
  chapter 3.2: Syntax - Objects

  Author: Nils Haeck M.Sc.

  Changes:
    04Jan2004 - Created

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfObjects;

interface

uses
  Dialogs, Classes, Contnrs, SysUtils, sdPdfUtil;

type

  // Generic floating point type used in PDF
  PdfFloat = double;

  // Generic PDF rectangle format. Convert from a rectangle array using
  // function PdfRectFromArray(). See 3.8.4
  TPdfRectangle = record
    Left, Top, Right, Bottom: PdfFloat;
  end;

  TPdfDebugLevel = (
    dlInfo,    // Informational debug message
    dlWarning, // Non-fatal warning (operation continues)
    dlError    // Fatal error message (operation was aborted)
  );

  // Forward declarations
  TPdfDictionary = class;
  TPdfStream = class;

  // Generic ancestor type for all PDF object classes
  TPdfObject = class(TPersistent)
  protected
    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
  public
    constructor Create; virtual;
    function AsBoolean: boolean; virtual;
    function AsInteger: integer; virtual;
    function AsNumber: PdfFloat; virtual;
    function DebugInfo: string;
    property AsString: string read GetAsString write SetAsString;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); virtual; abstract;
  end;

  // A PDF number object (integer or real), containing optional sign +/- and
  // dot. No scientific notation allowed.
  TPdfNumber = class(TPdfObject)
  private
    FValue: PdfFloat;
  public
    function AsInteger: integer; override;
    function AsNumber: PdfFloat; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    function IsInteger: boolean;
    function IsNonNegInteger: boolean;
    function IsPositiveInteger: boolean;
    property Value: PdfFloat read FValue write FValue;
  end;

  // PDF boolean object, must be keyword 'true' or 'false'
  TPdfBoolean = class(TPdfObject)
  private
    FValue: boolean;
  public
    function AsBoolean: boolean; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    property Value: boolean read FValue write FValue;
  end;

  // PDF null object, must be keyword 'null'
  TPdfNull = class(TPdfObject)
  public
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
  end;

  // PDF operator as found inside content streams. Args[] contains the arguments
  // to the function
  TPdfOperator = class(TPdfObject)
  private
    FArgs: TObjectList;
    FValue: string;
    function GetArgCount: integer;
    function GetArgs(Index: integer): TPdfObject;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ArgAdd(AArgument: TPdfObject);
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    property Value: string read FValue write FValue;
    property ArgCount: integer read GetArgCount;
    property Args[Index: integer]: TPdfObject read GetArgs;
  end;

  // A PDF string object, aka (~~~~) or <HEX>
  TPdfString = class(TPdfObject)
  private
    FValue: string;
  protected
    function GetAsString: string; override;
  public
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    property Value: string read FValue write FValue;
  end;

  // A PDF name object, aka /Name. The Value property contains
  // the name without the "/"
  TPdfName = class(TPdfObject)
  private
    FValue: string;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
  public
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    property Value: string read FValue write FValue;
  end;

  // PDF array, aka [pdfobject1 pdfobject2 .. pdfobjectN]
  TPdfArray = class(TPdfObject)
  private
    FElements: TObjectList;
    function GetElements(Index: integer): TPdfObject;
    function GetElementCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DictionaryByIndex(Index: integer): TPdfDictionary;
    procedure ElementsAdd(AElement: TPdfObject);
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    function NumberByIndex(Index: integer): PdfFloat;
    function StringByIndex(Index: integer): string;
    property ElementCount: integer read GetElementCount;
    property Elements[Index: integer]: TPdfObject read GetElements; default;
  end;

  // PDF Key/Value pairs as used in PDF dictionary objects
  TPdfKeyValue = class(TPersistent)
  private
    FKey: string;
    FValue: TPdfObject;
    procedure SetValue(const Value: TPdfObject);
  public
    constructor CreateWith(AKey: string; const AValue: TPdfObject); 
    destructor Destroy; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); virtual;
    property Key: string read FKey write FKey;
    property Value: TPdfObject read FValue write SetValue;
  end;

  // PDF dictionary, aka << /Key1 pdfobject1 /Key2 pdfobject2 >>
  TPdfDictionary = class(TPdfObject)
  private
    FEntries: TObjectList;
    function GetEntries(Index: integer): TPdfKeyValue;
    function GetEntryCount: integer;
    function GetDictionaryType: string;
    procedure SetDictionaryType(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function ArrayByKey(AKey: string): TPdfArray;
    function BooleanByKeyDefault(AKey: string; ADefault: boolean): boolean;
    function DictionaryByKey(AKey: string; VerifyType: string = ''): TPdfDictionary;
    procedure EntriesAdd(AEntry: TPdfKeyValue);
    function IntegerByKey(AKey: string): integer;
    function IntegerByKeyDefault(AKey: string; ADefault: integer): integer;
    function NumberByKeyDefault(AKey: string; ADefault: PdfFloat): PdfFloat;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    function StringByKey(AKey: string): string;
    function StringByKeyDefault(AKey: string; ADefault: string): string;
    function StreamByKey(AKey: string): TPdfStream;
    function ValueByKey(AKey: string): TPdfObject;
    property DictionaryType: string read GetDictionaryType write SetDictionaryType;
    property EntryCount: integer read GetEntryCount;
    property Entries[Index: integer]: TPdfKeyValue read GetEntries;
  end;

  TPdfIndirectRef = class;

  // PDF stream, which is a dictionary followed by stream ... endstream
  // Streams must always be wrapped indirect objects, and the TPdfIndirectObject
  // loading code will detect if the embedded object is a dictionary or a stream
  TPdfStream = class(TPdfObject)
  private
    FDict: TPdfDictionary;
    procedure SetDict(const Value: TPdfDictionary);
    function GetStreamLength: integer;
  protected
    // these are protected instead of private to give access in descendants
    FOwner: TObject;        // Pointer to TPdfDocument
    FStream: TMemoryStream;       // Owned TStream
    procedure LoadStreamFromPdf(Pdf: TObject; S: TStream); virtual;
    procedure PrepareStream; virtual;
    procedure GetFilterParams(var AChain, AParms: TPdfObject); virtual;
  public
    destructor Destroy; override;
    function StreamToString: string;
    procedure DecompressToStream(S: TMemoryStream); virtual;
    function IsExternal: boolean;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    property Dict: TPdfDictionary read FDict write SetDict;
    property StreamLength: integer read GetStreamLength;
  end;

  // An indirect reference, aka 12 0 R, which references object 12 generation 0
  // in the global object list
  TPdfIndirectRef = class(TPdfObject)
  private
    FGenerationNum: integer;
    FObjectNum: integer;
    FOwner: TObject;    // Pointer to TPdfDocument
    FValue: TPdfObject;
    FResolved: boolean;
    function GetValue: TPdfObject;
  protected
    procedure LoadIdentifierFromPdf(S: TStream); virtual;
    function GetAsString: string; override;
  public
    function AsInteger: integer; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
    procedure Resolve;
    property ObjectNum: integer read FObjectNum write FObjectNum;
    property Owner: TObject read FOwner write FOwner;
    property GenerationNum: integer read FGenerationNum write FGenerationNum;
    property Value: TPdfObject read GetValue write FValue;
  end;

  // An indirect object specification, aka 12 0 obj [/some /array 0 1] endobj. It
  // should be listed in the crossref table, so that other objects can refer to
  // it.
  TPdfIndirectObject = class(TPdfIndirectRef)
  private
  public
    destructor Destroy; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream); override;
  end;

  // Generic PDF object class
  TPdfObjectClass = class of TPdfObject;

const

  cPdfEmptyRect: TPdfRectangle = (Left: 0; Top: 0; Right: 0; Bottom: 0);

// De-reference the pdf object (returns Value if it is a TPdfIndirectRef or
// TPdfIndirectObject)
function DerefPdf(AObject: TPdfObject): TPdfObject;

// Load the next PDF object frm the stream S
function LoadObjectFromPdf(Pdf: TObject; S: TStream; AllowIndirect: boolean = True): TPdfObject;

// Determine the probable PDF object class from the stream. Reset the stream
// to where it was before calling. We can assume that at least an object is
// expected, or an object is terminated. In that case, return nil.
function DetermineNextPdfObjectType(S: TStream; AllowIndirect: boolean = True): TPdfObjectClass;

// Get the rectangle coordinates from the object in AValue, or return Emptyrect in
// case of error
function PdfRectFromArray(AValue: TPdfObject): TPdfRectangle;

// Write the rectangle coords to a string for debugging
function PdfRectangleToString(R: TPdfRectangle): string;

resourcestring

  sPdfExpectDictStart   = 'Expected "<<" start of dictionary not found';
  sPdfExpectDictClose   = 'Expected ">>" close of dictionary not found';
  sPdfExpectArrayStart  = 'Expected "[" start of array not found';
  sPdfExpectArrayClose  = 'Expected "]" close of array not found';
  sPdfExpectNameObj     = 'Expected "/" of name object';
  sPdfInvNameSyntax     = 'Invalid name syntax';
  sPdfExpectValueObj    = 'Expected a value object after key';
  sPdfIllegalChar       = 'Illegal charcter(s) found: "%s"';
  sPdfInvalidNumber     = 'Invalid number syntax';
  sPdfInvalidToken      = 'Invalid token found';
  sPdfInvalidHexChar    = 'Invalid HEX char in hexadecimal string';
  sPdfStreamSyntaxErr   = 'Syntax error in stream';
  sPdfSLengthMismatch   = 'Stream length mismatch';
  sPdfIndRefSyntaxErr   = 'Indirect reference syntax error';
  sPdfIndObjSyntaxErr   = 'Syntax error reading object #%d (expected "%s", found "%s")';//'Indirect object syntax error';
  sPdfExpectObjIndf     = 'Expected object identifier';
  sPdfNumIsNotInteger   = 'Number is not an integer';
  sPdfCantConvertToInt  = 'Cannot convert to integer';
  sPdfCantConvertToStr  = 'Cannot convert to string';
  sPdfCantConvertToNbr  = 'Cannot convert to number';
  sPdfCantConvertToBool = 'Cannot convert to boolean';
  sPdfEOFMissing        = 'EOF marker missing';
  sPdfXRefSyntaxErr     = 'Syntax error in X-ref table';
  sPdfInvalidXRefEntry  = 'Invalid X-ref entry';
  sPdfUnexpectedEOS     = 'Unexpected end of stream';
  sPdfTrailerNotFound   = 'Keyword "trailer" not found';
  sPdfUnknownRevision   = 'Unknown revision number';
  sPdfObjNumMismatch    = 'Object number mismatch';
  sPdfCannotResolve     = 'Unable to resolve indirect object';
  sPdfTrailerNoRoot     = 'Trailer misses "Root" entry';
  sPdfValueNotFound     = 'Value "%s" not found';
  sPdfExpectDict        = 'Expected dictionary object';
  sPdfExpectArray       = 'Expected array object';
  sPdfExpectStream      = 'Expected stream object';
  sPdfVerifyFailed      = 'Verification failed';
  sPdfInvalidIndex      = 'Invalid index in array';
  sPdfFlateDecodeErr    = 'FlateDecode error #%d';
  sPdfIndNotAllowed     = 'Indirect objects not allowed in content stream';
  sPdfExpectOperator    = 'Expected operator';
  sPdfUnknownOperator   = 'Unknown operator "%s"';
  sPdfOperatorNotImpl   = 'Operator "%s" not implemented';
  sPdfOperatorWarning   = 'Process warning in operator "%s"';
  sPdfOperatorError     = 'Process error in operator "%s"';
  sPdfIllegalOperator   = 'Illegal operator (too long)';
  sPdfCompatNestingErr  = 'Compatibility operator nesting error';
  sPdfResourcesMissing  = 'Resources for page are missing';
  sPdfStateNestingErr   = 'Graphics state nesting error';
  sPdfExtGStNotExist    = 'Dictionary with graphics state does not exist';
  sPdfStateParamNotImpl = 'State parameter "%s" not implemented';
  sPdfStateParamUnknown = 'Unknown state parameter "%s"';
  sPdfCantConvertColor  = 'Unable to convert color space';
  sPdfUnableToDecode    = 'Unable to decode encryption method "%s"';
  sCannotDecryptContent = 'Cannot decrypt content';
  sPdfIncorrectPass     = 'Incorrect password';
  sPdfCantFindXObject   = 'Cannot find XObject "%s"';
  sPdfNoSupportPSXObj   = 'Postscript XObjects not supported. Skipped';
  sPdfUnknownXObj       = 'Unknown XObject encountered in content stream';
  sPdfInvFilterParam    = 'Invalid filter parameters';
  sPdfUnknRowsInFilter  = 'Unknown number of rows in CCITT Fax filter, cannot decode';
  sPdfInvalidColorSpace = 'Invalid color space';
  sPdfInvalidEncEntry   = 'Invalid encryption entry';
  sInvalidRevision      = 'Invalid encryption revision';

implementation

uses
  sdPdfDocument, sdPdfFilters, sdPdfFileStruct;

type
  THackPdfDocument = class(TPdfDocument);

{ procedures }

function DerefPdf(AObject: TPdfObject): TPdfObject;
// De-reference the pdf object (returns Value if it is a TPdfIndirectRef or descendant
// TPdfIndirectObject)
begin
  if (AObject is TPdfIndirectRef) then
    Result := TPdfIndirectRef(AObject).Value
  else
    Result := AObject;
end;

function DetermineNextPdfObjectType(S: TStream; AllowIndirect: boolean = True): TPdfObjectClass;
// Determine the probable PDF object class from the stream. Reset the stream
// to where it was before calling. We can assume that at least an object is
// expected, or an object is terminated. In that case, return nil.
var
  Ch: char;
  Ch2: array[0..1] of char;
  Start: integer;
  First, Second: TPdfNumber;
  Token: string;
begin
  Result := nil;
  Start := S.Position;
  Ch := ReadNextNonWS(S);
  S.Seek(-1, soFromCurrent);
  case Ch of
  '1'..'9': // either number or indirect ref/obj
    begin
      // Assume..
      Result := TPdfNumber;
      if AllowIndirect then begin
        // In order to determine if it is an indirect ref/obj, we must actually
        // read two numbers and a token
        try
          First := TPdfNumber.Create;
          try
            First.LoadFromPdf(nil, S);
            if First.IsPositiveInteger then begin
              Ch := ReadNextNonWS(S);
              S.Seek(-1, soFromCurrent);
              if Ch in ['0'..'9'] then begin
                Second := TPdfNumber.Create;
                try
                  Second.LoadFromPdf(nil, S);
                  // Rules for indirect objects
                  if Second.IsNonNegInteger then begin
                    Token := ReadTokenFromPdfStream(S);
                    // Either found 'R' or 'obj' or no known token, so the lad is a simple number
                    if Token = 'R' then
                      Result := TPdfIndirectRef
                    else
                      if Token = 'obj' then
                        Result := TPdfIndirectObject;
                  end;
                finally
                  Second.Free;
                end;
              end;
            end;
          finally
            First.Free;
          end;
        except
          // Something went wrong, so not a indirect ref/obj, so assume number
        end;
        // Set back the stream
        S.Seek(Start, soFromBeginning);
      end;
    end;
  '+', '-', '0', '.': // A number
    Result := TPdfNumber;
  '(': // string
    Result := TPdfString;
  '<': // dictionary, stream or string
    begin
      S.Read(Ch2[0], 2);
      if Ch2 = '<<' then
        // Note that it is quite costly to find out if this is a dictionary
        // or a stream; so we do not do that here. We assume it is a dictionary
        // and code in loading indirect objects will convert to stream if it is
        Result := TPdfDictionary
      else
        Result := TPdfString;
      // Set back the stream
      S.Seek(-2, soFromCurrent);
    end;
  '/': // name
    Result := TPdfName;
  '[': // array
    Result := TPdfArray;
  '''', '"', 'a'..'z','A'..'Z':
    begin
      // This must be an operator (as found inside content streams)
      Result := TPdfOperator;
      // unless..
      if Ch in ['n', 't', 'f'] then begin
        S.Read(Ch2[0], 2);
        if Ch2 = 'nu' then
          // Null
          Result := TPdfNull
        else if (Ch2 = 'tr') or (Ch2 = 'fa') then
          // Boolean 'true' or 'false'
          Result := TPdfBoolean;
        // Set back the stream
        S.Seek(-2, soFromCurrent);
      end;
    end;
  ']', '>':; // terminations - return nil
  else
    raise EPdfError.CreateFmt(sPdfIllegalChar, [Ch + ' (#$' + IntToHex(ord(Ch), 2) + ')']);
  end;
end;

function LoadObjectFromPdf(Pdf: TObject; S: TStream; AllowIndirect: boolean = True): TPdfObject;
// Load the next PDF object frm the stream S
var
  AClass: TPdfObjectClass;
begin
  Result := nil;
  AClass := DetermineNextPdfObjectType(S, AllowIndirect);
  if assigned(AClass) then begin
    Result := AClass.Create;
    Result.LoadFromPdf(Pdf, S);
  end;
end;

function PdfRectFromArray(AValue: TPdfObject): TPdfRectangle;
// Get the rectangle coordinates from the object in AValue, or return Emptyrect in
// case of error
begin
  Result := cPdfEmptyRect;
  if (AValue is TPdfArray) then with TPdfArray(AValue) do begin
    if ElementCount <> 4 then exit;
    Result.Left   := NumberByIndex(0);
    Result.Bottom := NumberByIndex(1);
    Result.Right  := NumberByIndex(2);
    Result.Top    := NumberByIndex(3);
  end;
end;

function PdfRectangleToString(R: TPdfRectangle): string;
begin
  Result := Format('[%5.3f,%5.3f,%5.3f,%5.3f]', [R.Left, R.Top, R.Right, R.Bottom]);
end;

{ TPdfObject }

function TPdfObject.AsBoolean: boolean;
begin
  raise EPdfError.Create(sPdfCantConvertToBool);
end;

function TPdfObject.AsInteger: integer;
begin
  raise EPdfError.Create(sPdfCantConvertToInt);
end;

function TPdfObject.AsNumber: PdfFloat;
begin
  raise EPdfError.Create(sPdfCantConvertToNbr);
end;

constructor TPdfObject.Create;
begin
  inherited Create;
end;

function TPdfObject.DebugInfo: string;
begin
  Result := GetPdfObjectInfo(Self);
end;

function TPdfObject.GetAsString: string;
begin
  raise EPdfError.Create(sPdfCantConvertToStr);
end;

procedure TPdfObject.SetAsString(const Value: string);
begin
  raise EPdfError.Create(sPdfCantConvertToStr);
end;

{ TPdfKeyValue }

constructor TPdfKeyValue.CreateWith(AKey: string; const AValue: TPdfObject);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

destructor TPdfKeyValue.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TPdfKeyValue.LoadFromPdf(Pdf: TObject; S: TStream);
var
  AKeyName: TPdfName;
begin
  // Load name
  AKeyName := TPdfName.Create;
  try
    AKeyName.LoadFromPdf(Pdf, S);
    FKey := AKeyName.Value;
  finally
    AKeyName.Free;
  end;

  // Load value
  FreeAndNil(FValue);
  FValue := LoadObjectFromPdf(Pdf, S);
  if not assigned(FValue) then
    raise EPdfError.Create(sPdfExpectValueObj);
end;

procedure TPdfKeyValue.SetValue(const Value: TPdfObject);
begin
  FreeAndNil(FValue);
  FValue := Value;
end;

{ TPdfName }

function TPdfName.GetAsString: string;
begin
  Result := FValue;
end;

procedure TPdfName.LoadFromPdf(Pdf: TObject; S: TStream);
var
  i: integer;
  Ch: Char;
  CharCode: integer;
  Raw: string;
begin
  // We expect a / first
  Ch := ReadNextNonWS(S);
  if Ch <> '/' then
    raise EPdfError.Create(sPdfExpectNameObj);

  // Read until first delimiter or whitespace
  Raw := '';
  FValue := '';
  repeat
    if S.Read(Ch, 1) = 0 then break;
    if Ch in cPdfDelimiter then begin
      // We need the delimiter again so rewind stream by one
      S.Seek(-1, soFromCurrent);
      break;
    end;
    if Ch in cPdfWhitespace then break;
    Raw := Raw + Ch;
  until False;

  // Convert raw to name, #HH hex notation to characters
  i := 1;
  while i <= length(Raw) do begin
    if Raw[i] = '#' then begin
      // Next two chars are hex
      CharCode := StrToIntDef('$' + copy(Raw, i + 1, 2), -1);
      if (CharCode >= 0) and (CharCode <= 255) then
        FValue := FValue + Chr(CharCode)
      else
        raise EPdfError.Create(sPdfInvNameSyntax);
      inc(i, 2);
    end else begin
      FValue := FValue + Raw[i];
      inc(i);
    end;
  end;
end;

procedure TPdfName.SetAsString(const Value: string);
begin
  FValue := Value;
end;

{ TPdfNumber }

function TPdfNumber.AsInteger: integer;
begin
  Result := round(FValue);
  if Result <> FValue then
    raise EPdfError(sPdfNumIsNotInteger);
end;

function TPdfNumber.AsNumber: PdfFloat;
begin
  Result := FValue;
end;

function TPdfNumber.IsInteger: boolean;
begin
  Result := Round(FValue) = FValue;
end;

function TPdfNumber.IsNonNegInteger: boolean;
begin
  Result := IsInteger and (FValue >= 0);
end;

function TPdfNumber.IsPositiveInteger: boolean;
begin
  Result := IsInteger and (FValue > 0);
end;

const cDivPow10: array[1..10] of double =
  (0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001, 0.00000001,
   0.000000001, 0.0000000001);

procedure TPdfNumber.LoadFromPdf(Pdf: TObject; S: TStream);
// Read a PDF format number [+,-][num][.[num]]
var
  Raw: string;
  i, Sign: integer;
  Numb, Frac: string;
  HasDot: boolean;
function Pow10(N: integer): integer;
begin
  if N <= 0 then
    Result := 1
  else
    Result := Pow10(N - 1) * 10;
end;
begin
  // Raw string
  Raw := ReadTokenFromPdfStream(S);
  Sign := 1;
  Numb := '';
  Frac := '';
  HasDot := False;
  // Loop through characters
  for i := 1 to length(Raw) do begin
    case Raw[i] of
    '+', '-':
      begin
        if i <> 1 then raise EPdfError.Create(sPdfInvalidNumber);
        if Raw[i] = '-' then Sign := -1;
      end;
    '0'..'9':
      begin
        if HasDot then
          // Fractional part
          Frac := Frac + Raw[i]
        else
          // Whole number part
          Numb := Numb + Raw[i];
      end;
    '.':
      begin
        if HasDot then
          // Only one dot allowed
          raise EPdfError.Create(sPdfInvalidNumber);
        HasDot := True;
      end;
    else
      // Accept only legal characters
      raise EPdfError.Create(sPdfInvalidNumber);
    end;
  end;
  if length(Numb) = 0 then begin
    // Just a plus or minus is no good
    if not HasDot then
      raise EPdfError.Create(sPdfInvalidNumber);
    Numb := '0';
  end;
  FValue := Sign * StrToInt(Numb);
  if HasDot then
    if Length(Frac) > 0 then begin
      // Some seem to have endless digits, limit that to 10 to avoid integer overflow
      if Length(Frac) > 10 then
        Frac := Copy(Frac, 1, 10);
      FValue := FValue + Sign * StrToInt(Frac) * cDivPow10[Length(Frac)];
    end;
end;

{ TPdfBoolean }

function TPdfBoolean.AsBoolean: boolean;
begin
  Result := FValue;
end;

procedure TPdfBoolean.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Raw: string;
begin
  Raw := ReadTokenFromPdfStream(S);
  if Raw = 'true' then
    FValue := True
  else
    if Raw = 'false' then
      FValue := False
    else
      raise EPdfError(sPdfInvalidToken);
end;

{ TPdfNull }

procedure TPdfNull.LoadFromPdf(Pdf: TObject; S: TStream);
begin
  if ReadTokenFromPdfStream(S) <> 'null' then
    raise EPdfError(sPdfInvalidToken);
end;

{ TPdfArray }

constructor TPdfArray.Create;
begin
  inherited;
  FElements := TObjectList.Create;
end;

destructor TPdfArray.Destroy;
begin
  FreeAndNil(FElements);
  inherited;
end;

function TPdfArray.DictionaryByIndex(Index: integer): TPdfDictionary;
// Find the direct or indirect dictionary at element Index
var
  AValue: TPdfObject;
begin
  AValue := DerefPdf(Elements[Index]);
  if not assigned(AValue) then
    raise EPdfError.Create(sPdfInvalidIndex);
  // Check if it is a dictionary
  if AValue.ClassType <> TPdfDictionary then
    raise EPdfError.Create(sPdfExpectDict);
  // This is the result
  Result := TPdfDictionary(AValue);
end;

procedure TPdfArray.ElementsAdd(AElement: TPdfObject);
begin
  if assigned(FElements) and assigned(AElement) then
    FElements.Add(AElement);
end;

function TPdfArray.GetElementCount: integer;
begin
  Result := 0;
  if assigned(FElements) then Result := FElements.Count;
end;

function TPdfArray.GetElements(Index: integer): TPdfObject;
begin
  Result := nil;
  if (Index >= 0) and (Index < ElementCount) then
    Result := TPdfObject(FElements[Index]);
end;

procedure TPdfArray.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Ch: char;
  AElement: TPdfObject;

begin
  // Clear the element list
  FElements.Clear;
  // Read '['
  Ch := ReadNextNonWS(S);
  if Ch <> '[' then
    raise EPdfError.Create(sPdfExpectArrayStart);
  // Read elements
  repeat
    // Read name
    AElement := LoadObjectFromPdf(Pdf, S);
    if assigned(AElement) then
      ElementsAdd(AElement);
  until not assigned(AElement);
  // Read last ']'
  Ch := ReadNextNonWS(S);
  if Ch <> ']' then
    raise EPdfError.Create(sPdfExpectArrayClose);
end;

function TPdfArray.NumberByIndex(Index: integer): PdfFloat;
var
  AValue: TPdfObject;
begin
  AValue := DerefPdf(Elements[Index]);
  if not assigned(AValue) then
    raise EPdfError.Create(sPdfInvalidIndex);
  Result := AValue.AsNumber;
end;

function TPdfArray.StringByIndex(Index: integer): string;
var
  AValue: TPdfObject;
begin
  Result := '';
  AValue := DerefPdf(Elements[Index]);
  if not assigned(AValue) then
    raise EPdfError.Create(sPdfInvalidIndex);
  Result := AValue.AsString;
end;

{ TPdfDictionary }

function TPdfDictionary.ArrayByKey(AKey: string): TPdfArray;
// Find a direct or indirect array by the AKey name
var
  AValue: TPdfObject;
begin
  Result := nil;
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then exit;
  AValue := DerefPdf(AValue);
  // Check if it is an array
  if not (AValue is TPdfArray) then
    raise EPdfError.Create(sPdfExpectArray);
  // This is the result
  Result := TPdfArray(AValue);
end;

function TPdfDictionary.BooleanByKeyDefault(AKey: string;
  ADefault: boolean): boolean;
var
  AValue: TPdfObject;
begin
  Result := ADefault;
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then exit;
  Result := AValue.AsBoolean;
end;

procedure TPdfDictionary.Clear;
begin
  inherited;
  FEntries.Clear;
end;

constructor TPdfDictionary.Create;
begin
  FEntries := TObjectList.Create;
end;

destructor TPdfDictionary.Destroy;
begin
  FreeAndNil(FEntries);
  inherited;
end;

function TPdfDictionary.DictionaryByKey(AKey,
  VerifyType: string): TPdfDictionary;
// Find a direct or indirect dictionary by the AKey name, and verify if the
// field /Type contains VerifyType (use '' for no verification, in that case
// the function will return NIL and not raise any exceptions)
var
  AValue: TPdfObject;
begin
  Result := nil;
  // Find it
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then begin
    if length(VerifyType) > 0 then
      raise EPdfError.CreateFmt(sPdfValueNotFound, [AKey])
    else
      exit;
  end;

  // Dereference
  AValue := DerefPdf(AValue);

  // Check if it is a dictionary
  if not (AValue is TPdfDictionary) then begin
    // In strange cases some authors put null here!
    if AValue is TPdfNull then begin
      Result := nil;
      exit;
    end else
      raise EPdfError.Create(sPdfExpectDict);
  end;

  // This is the result
  Result := TPdfDictionary(AValue);

  // Verify
  if length(VerifyType) > 0 then
    if Result.DictionaryType <> VerifyType then
      raise EPdfError.Create(sPdfVerifyFailed);
end;

procedure TPdfDictionary.EntriesAdd(AEntry: TPdfKeyValue);
begin
  if assigned(AEntry) and assigned(FEntries) then
    FEntries.Add(AEntry);
end;

function TPdfDictionary.GetDictionaryType: string;
begin
  Result := StringByKey('Type');
end;

function TPdfDictionary.GetEntries(Index: integer): TPdfKeyValue;
begin
  Result := nil;
  if (Index >= 0) and (Index < EntryCount) then
    Result := TPdfKeyValue(FEntries[Index]);
end;

function TPdfDictionary.GetEntryCount: integer;
begin
  Result := 0;
  if assigned(FEntries) then Result := FEntries.Count;
end;

function TPdfDictionary.IntegerByKey(AKey: string): integer;
var
  AValue: TPdfObject;
begin
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then
    raise EPdfError.CreateFmt(sPdfValueNotFound, [AKey]);
  Result := AValue.AsInteger;
end;

function TPdfDictionary.IntegerByKeyDefault(AKey: string;
  ADefault: integer): integer;
var
  AValue: TPdfObject;
begin
  Result := ADefault;
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then exit;
  Result := AValue.AsInteger;
end;

procedure TPdfDictionary.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Ch: char;
  AEntry: TPdfKeyValue;
begin
  // Clear the item list
  Clear;
  // Read double angle brackets <<
  Ch := ReadNextNonWS(S);
  if Ch <> '<' then
    raise EPdfError.Create(sPdfExpectDictStart);
  S.Read(Ch, 1);
  if Ch <> '<' then
    raise EPdfError.Create(sPdfExpectDictStart);
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
  // Read last '>>'
  if Ch <> '>' then
    raise EPdfError.Create(sPdfExpectDictClose);
  S.Read(Ch, 1);
  if Ch <> '>' then
    raise EPdfError.Create(sPdfExpectDictClose);
end;

function TPdfDictionary.NumberByKeyDefault(AKey: string;
  ADefault: PdfFloat): PdfFloat;
var
  AValue: TPdfObject;
begin
  Result := ADefault;
  AValue := ValueByKey(AKey);
  if assigned(AValue) then
    Result := AValue.AsNumber;
end;

procedure TPdfDictionary.SetDictionaryType(const Value: string);
var
  AValue: TPdfObject;
  //AKeyValue: TPdfKeyValue;
begin
  AValue := ValueByKey('Type');
  if not assigned(AValue) then begin
    AValue := TPdfName.Create;
    EntriesAdd(TPdfKeyValue.CreateWith('Type', AValue));
  end;
  AValue.AsString := Value;
end;

function TPdfDictionary.StreamByKey(AKey: string): TPdfStream;
var
  AValue: TPdfObject;
begin
  Result := nil;
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then exit;

  AValue := DerefPdf(AValue);

  // Check if it is a stream
  if not (AValue is TPdfStream) then
    raise EPdfError.Create(sPdfExpectStream);

  // This is the result
  Result := TPdfStream(AValue);
end;

function TPdfDictionary.StringByKey(AKey: string): string;
var
  AValue: TPdfObject;
begin
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then
    raise EPdfError.CreateFmt(sPdfValueNotFound, [AKey]);
  Result := AValue.AsString;
end;

function TPdfDictionary.StringByKeyDefault(AKey, ADefault: string): string;
var
  AValue: TPdfObject;
begin
  AValue := ValueByKey(AKey);
  if not assigned(AValue) then
    Result := ADefault
  else
    Result := AValue.AsString;
end;

function TPdfDictionary.ValueByKey(AKey: string): TPdfObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to EntryCount - 1 do
    if Entries[i].Key = AKey then begin
      Result := Entries[i].Value;
      exit;
    end;
end;

{ TPdfString }

function TPdfString.GetAsString: string;
begin
  Result := Value;
end;

procedure TPdfString.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Ch, H0, H1: char;
  Raw, Octal: string;
  HexVal: byte;
  Count, Level: integer;
function HexToDec(Ch: char): integer;
begin
  case Ch of
  '0'..'9': Result := ord(Ch) - ord('0');
  'a'..'f': Result := ord(Ch) - ord('a') + 10;
  'A'..'F': Result := ord(Ch) - ord('A') + 10;
  '>': Result := 0;
  else
    raise EPdfError(sPdfInvalidHexChar);
  end;
end;
function OctalToString(Value: string): string;
var
  Oct: string;
begin
  Result := '';
  while length(Value) > 0 do begin
    Oct := copy(Value, 1, 3);
    Value := Copy(Value, 4, length(Value));
    while length(Oct) < 3 do Oct := '0' + Oct;
    Result := Result + Chr(
      HexToDec(Oct[1]) * 64 + HexToDec(Oct[2]) * 8 + HexToDec(Oct[3]));
  end;
end;
begin
  // Read first char
  Ch := ReadNextNonWS(S);
  Raw := '';
  if Ch = '<' then begin

    // Hexadecimal string
    repeat
      H0 := ReadNextNonWS(S);
      if H0 = '>' then break;
      H1 := ReadNextNonWS(S);
      HexVal := HexToDec(H0) * 16 + HexToDec(H1);
      Raw := Raw + Chr(HexVal);
      if H1 = '>' then break;
    until Ch = #0;

  end else begin

    // Literal string
    Level := 1; // parenthesis level count
    repeat
      Count := S.Read(Ch, 1);

      // Standardize EOFs: convert to #10
      if Ch = #13 then begin
        // Check for possible #10 and skip that
        Count := S.Read(Ch, 1);
        if Ch <> #10 then
          S.Seek(-1, soFromCurrent);
        Ch := #10;
      end;

      // Handle escape chars
      if Ch = '\' then begin
        // Escape char - we must convert it
        Count := S.Read(Ch, 1);
        case Ch of
        #13: // skip
          begin
            // Check for possible #10 and skip that too
            Count := S.Read(Ch, 1);
            if Ch <> #10 then
              S.Seek(-1, soFromCurrent);
            continue;
          end;
        #10: continue; // skip
        'n': Ch := #10; // Line Feed
        'r': Ch := #13; // Carriage Return
        't': Ch := #09; // Horizontal Tab
        'b': Ch := #08; // Backspace BS
        'f': Ch := #12; // Form Feed
        '(': dec(Level);// counteract the level handling
        ')': inc(Level);// counteract the level handling
        '\': ; // just a literal slash
        '0'..'7':
          begin
            // Octal sequence
            Octal := Ch;
            repeat
              Count := S.Read(Ch, 1);
              if not (Ch in ['0'..'7']) then begin
                S.Seek(-1, soFromCurrent);
                break;
              end;
              Octal := Octal + Ch;
            until Count = 0;
            Raw := Raw + OctalToString(Octal);
            continue;
          end;
        end;
      end;

      // Handle matching parenthesis
      if Ch = '(' then inc(Level);
      if Ch = ')' then dec(Level);
      if Level = 0 then break;

      // Add to string
      Raw := Raw + Ch;
    until Count = 0;
    if Count = 0 then
      raise EPdfError(sPdfUnexpectedEOS);
  end;
  FValue := Raw;
end;

{ TPdfStream }

procedure TPdfStream.DecompressToStream(S: TMemoryStream);
var
  AFilter: TPdfFilter;
  AChain, AParms: TPdfObject;
  MustDecrypt: boolean;
  D: TMemoryStream;
  ARef: TPdfObjectRef;
begin
  PrepareStream;

  // Check with parent if we need to decrypt
  MustDecrypt := TPdfDocument(FOwner).IsEncrypted;
  D := nil;
  if MustDecrypt then begin
    // find reference
    ARef := TPdfDocument(FOwner).ObjectByValue(Self);
    D := TMemoryStream.Create;
    TPdfDocument(FOwner).Encryptor.DecryptStream(ARef.Generation, ARef.ObjectIdx, FStream, D);
  end;
  try

    // Filter chain and parameters
    GetFilterParams(AChain, AParms);

    // Do we have a filter
    if assigned(AChain) then begin
      AFilter := TPdfFilter.Create(FOwner);
      try
        AFilter.SetParams(AChain, AParms);
        if assigned(D) then
          AFilter.Decompress(D, S)
        else
          AFilter.Decompress(FStream, S);
      finally
        AFilter.Free;
      end;
    end else begin
      // No filter, so we can just copy
      if assigned(D) then
        S.CopyFrom(D, FStream.Size)
      else
        S.CopyFrom(FStream, FStream.Size);
    end;
  finally
    D.Free;
  end;
end;

destructor TPdfStream.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FDict);
  inherited;
end;

procedure TPdfStream.GetFilterParams(var AChain, AParms: TPdfObject);
begin
  if IsExternal then begin
    AChain := Dict.ValueByKey('FFilter');
    AParms := Dict.ValueByKey('FDecodeParms');
  end else begin
    AChain := Dict.ValueByKey('Filter');
    AParms := Dict.ValueByKey('DecodeParms');
  end;
end;

function TPdfStream.GetStreamLength: integer;
begin
  if assigned(FStream) then
    Result := FStream.Size
  else
    Result := 0;
end;

function TPdfStream.IsExternal: boolean;
begin
  Result := False;
  if assigned(FDict) and assigned(FDict.ValueByKey('F')) then
    Result := True;
end;

procedure TPdfStream.LoadFromPdf(Pdf: TObject; S: TStream);
begin
  // Recreate and load dictionary
  FreeAndNil(FDict);
  FDict := TPdfDictionary.Create;
  FDict.LoadFromPdf(Pdf, S);
  // Load stream
  LoadStreamFromPdf(Pdf, S);
end;

procedure TPdfStream.LoadStreamFromPdf(Pdf: TObject; S: TStream);
var
  Ch: char;
  Count: integer;
  AToken: string;
  StreamLength: integer;
begin
  FreeAndNil(FStream);
  FOwner := Pdf;
  // Read keyword 'stream'
  if ReadTokenFromPdfStream(S) <> 'stream' then
    raise EPdfError.Create(sPdfStreamSyntaxErr);

  // 'stream' keyword must be terminated with #13#10 or #10 alone
  repeat
    Count := S.Read(Ch, 1);
  until (Ch = #10) or (Count = 0);

  // Stream length
  StreamLength := Dict.IntegerByKey('Length');

  // Read of the stream, however: external streams will be loaded in "PrepareStream"
  if not IsExternal then begin
    try
      // Load the stream into a memorystream
      FStream := TMemoryStream.Create;;
      if StreamLength > 0 then
        FStream.CopyFrom(S, StreamLength);
      FStream.Position := 0;
    except
      raise EPdfError.Create(sPdfSLengthMismatch);
    end;
  end;

  // Check 'endstream' keyword
  AToken := ReadTokenFromPdfStream(S);
  if (AToken <> 'endstream') then
    raise EPdfError.Create(sPdfStreamSyntaxErr);
end;

procedure TPdfStream.PrepareStream;
begin
  // TODO: Handle external stream opening here!
  if assigned(FStream) then
    FStream.Seek(0, soFromBeginning);
end;

procedure TPdfStream.SetDict(const Value: TPdfDictionary);
begin
  FreeAndNil(FDict);
  FDict := Value;
end;

function TPdfStream.StreamToString: string;
var
  M: TMemoryStream;
begin
  Result := '';
  M := TMemoryStream.Create;
  try
    DecompressToStream(M);
    if M.Size > 0 then begin
      SetLength(Result, M.Size);
      Move(M.Memory^, Result[1], M.Size);
    end;
  finally
    M.Free;
  end;
end;

{ TPdfIndirectRef }

function TPdfIndirectRef.AsInteger: integer;
begin
  Result := Value.AsInteger;
end;

function TPdfIndirectRef.GetAsString: string;
begin
  Result := Value.AsString;
end;

function TPdfIndirectRef.GetValue: TPdfObject;
begin
  if not FResolved then
    Resolve;
  Result := FValue;
end;

procedure TPdfIndirectRef.LoadFromPdf(Pdf: TObject; S: TStream);
begin
  FValue := nil;
  FResolved := False;
  // Store this for when we must resolve
  FOwner := Pdf;
  LoadIdentifierFromPdf(S);
  // Check for 'R'
  if ReadTokenFromPdfStream(S) <> 'R' then
    raise EPdfError.Create(sPdfIndRefSyntaxErr);
end;

procedure TPdfIndirectRef.LoadIdentifierFromPdf(S: TStream);
// Load the object identifier (object number/generation number)
var
  First, Second: TPdfNumber;
begin
  First := TPdfNumber.Create;
  Second := TPdfNumber.Create;
  try
    First.LoadFromPdf(nil, S);
    if not First.IsPositiveInteger then
      raise EPdfError.Create(sPdfExpectObjIndf);
    Second.LoadFromPdf(nil, S);
    if not Second.IsNonNegInteger then
      raise EPdfError.Create(sPdfExpectObjIndf);
    FObjectNum     := First.AsInteger;
    FGenerationNum := Second.AsInteger;
  finally
    First.Free;
    Second.Free;
  end;
end;

procedure TPdfIndirectRef.Resolve;
// Resolve a pointer to the referenced object
var
  APdf: THackPdfDocument;
begin
  if FResolved then exit;
  FResolved := True;
  APdf := THackPdfDocument(Owner);
  FValue := APdf.ObjectValue[ObjectNum];
  if not assigned(FValue) then
    raise EPdfError.Create(sPdfCannotResolve);
end;

{ TPdfIndirectObject }

destructor TPdfIndirectObject.Destroy;
begin
  // As opposed to TPdfIndirectRef, we own the FValue object and thus must
  // free it.
  FreeAndNil(FValue);
  inherited;
end;

procedure TPdfIndirectObject.LoadFromPdf(Pdf: TObject; S: TStream);
var
  Ch: char;
  ADict: TPdfDictionary;
  Token: string;
begin
  FreeAndNil(FValue);
  FResolved := False;
  LoadIdentifierFromPdf(S);
  // Check for 'obj'
  Token := ReadTokenFromPdfStream(S);
  if Token <> 'obj' then
    raise EPdfError.CreateFmt(sPdfIndObjSyntaxErr, [FObjectNum, 'obj', Token]);
  // Recreate and Load object
  FValue := LoadObjectFromPdf(Pdf, S);
  if not assigned(FValue) then exit;

  // Here we deal with the special stream case, since a stream looks like a
  // dictionary
  if FValue.ClassType = TPdfDictionary then begin
    Ch := ReadNextNonWS(S);
    S.Seek(-1, soFromCurrent);
    if Ch = 's' then begin
      // Obviously, this is a stream..
      ADict := TPdfDictionary(FValue);
      FValue := TPdfStream.Create;
      // The stream becomes the *owner* of ADict! So it must free it too
      TPdfStream(FValue).Dict := ADict;
      TPdfStream(FValue).LoadStreamFromPdf(Pdf, S);
    end;
  end;

  // Check for 'endobj'
  Token := ReadTokenFromPdfStream(S);
  if Token <> 'endobj' then
    TPdfDocument(Pdf).DoDebugMessage(dlWarning, Format(sPdfIndObjSyntaxErr,
      [FObjectNum, 'endobj', Token]));
  FResolved := True;
end;

{ TPdfOperator }

procedure TPdfOperator.ArgAdd(AArgument: TPdfObject);
begin
  // Add
  if assigned(AArgument) and assigned(FArgs) then
    FArgs.Add(AArgument);
end;

constructor TPdfOperator.Create;
begin
  inherited;
  FArgs := TObjectList.Create;
end;

destructor TPdfOperator.Destroy;
begin
  FreeAndNil(FArgs);
  inherited;
end;

function TPdfOperator.GetArgCount: integer;
begin
  Result := 0;
  if assigned(FArgs) then Result := FArgs.Count;
end;

function TPdfOperator.GetArgs(Index: integer): TPdfObject;
begin
  Result := nil;
  if (Index >= 0) and (Index < ArgCount) then
    Result := TPdfObject(FArgs[Index]);
end;

procedure TPdfOperator.LoadFromPdf(Pdf: TObject; S: TStream);
begin
  Value := ReadTokenFromPdfStream(S);
end;

end.
