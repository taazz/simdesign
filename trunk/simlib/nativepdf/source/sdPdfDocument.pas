{
  unit sdPdfDocument

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  In some of the comments, the Implementation Notes in the specification
  (appendix H) are referenced.

  Created:
    06Jan2004

  Changes:
    25Nov2005: Added ObjectAdd

  Copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfDocument;

interface

uses
  Classes, Contnrs, SysUtils, sdPdfUtil, sdPdfObjects, sdPdfFileStruct,
  Dialogs, sdPdfObjectTrees, sdPdfContentStream, sdPdfEncryption, sdPdfFonts;

type

  TPdfErrorMethod = (
    emEvent,    // Errors will be shown through debug event
    emDialog,   // Errors will cause dialog to show
    emException // Errors will cause a highlevel exception
  );

  TPdfDebugMessageEvent = procedure (Sender: TObject; Level: TPdfDebugLevel; const AMessage: string) of object;
  TPdfPasswordEvent = procedure (Sender: TObject; Action: TUserAction; var Password: string) of object;

  TPdfPage = class(TPdfDictionary)
  private
    function GetRotate: integer;
  public
    function GetAttribute(const AName: string): TPdfObject;
    function Contents: TPdfObject;
    function CropBox: TPdfRectangle;
    function MediaBox: TPdfRectangle;
    function Resources: TPdfDictionary;
    property Rotate: integer read GetRotate;
  end;

  TPdfDocument = class(TComponent)
  private
    FCatalog: TPdfDictionary; // Pointer to the PDF catalog dict (see 3.6.1)
    FDelayPageLoad: boolean;  // If true, only load the page from the PDF stream if required
    FEncryptor: TPdfEncryptor;// Owned encryption object
    FErrorMethod: TPdfErrorMethod; // Determine how errors are reported
    FFonts: TObjectList;      // Owned list of fonts
    FFormXObjects: TObjectList;
    FFileID: TPdfArray;       // pointer to File Identifier
    FModified: boolean;
    FObjectRefs: TObjectList; // Owned list of object references
    FPageTree: TPdfPageTree;  // Owned TPdfPageTree object with pointer to the page tree dict (see 3.6.2)
    FPdfStream: TStream;      // Pointer to the PDF stream or nil
    FRevision: integer;       // Revision number to read (0 = most recent)
    FVersion: string;         // Version of the PDF document, 1.0 through 1.5
    FOnDebugMessage: TPdfDebugMessageEvent;
    FOnRequirePassword: TPdfPasswordEvent;
    procedure SetModified(const Value: boolean);
    procedure ReadHeader;
    procedure ReadTrailer;
    function GetObjects(Index: integer): TPdfObjectRef;
    function GetObjectCount: integer;
    function GetFontCount: integer;
    function GetFonts(Index: integer): TPdfFont;
    function GetPages(Index: integer): TPdfPage;
    function GetObjectValue(Index: integer): TPdfObject;
    function GetPageTree: TPdfPageTree;
  protected
    property FontCount: integer read GetFontCount;
    property Fonts[Index: integer]: TPdfFont read GetFonts;
    property ObjectValue[Index: integer]: TPdfObject read GetObjectValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DoDebugMessage(Level: TPdfDebugLevel; const AMessage: string);
    function FontByDict(ADict: TPdfDictionary): TPdfFont;
    procedure FontAdd(AFont: TPdfFont);
    function FormXObjectByStream(AStream: TPdfStream): TPdfFormXObject;
    procedure FormXObjectAdd(AForm: TPdfFormXObject);
    function IsEncrypted: boolean;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(S: TStream);
    function ObjectByValue(Value: TPdfObject): TPdfObjectRef;
    function ObjectAdd(AClass: TpdfObjectClass): TPdfObject;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    property DelayPageLoad: boolean read FDelayPageLoad write FDelayPageLoad;
    property Encryptor: TPdfEncryptor read FEncryptor write FEncryptor;
    property ErrorMethod: TPdfErrorMethod read FErrorMethod write FErrorMethod;
    property FileID: TPdfArray read FFileID;
    property Modified: boolean read FModified write SetModified;
    property ObjectCount: integer read GetObjectCount;
    property Objects[Index: integer]: TPdfObjectRef read GetObjects;
    property PageTree: TPdfPageTree read GetPageTree;
    property Pages[Index: integer]: TPdfPage read GetPages;
    // Revision = 0 is most recent, and 1, 2, 3 .. are previous versions. Set
    // Revision before loading to load an old version of the PDF
    property Revision: integer read FRevision write FRevision;
    property Version: string read FVersion write FVersion;
    property OnDebugMessage: TPdfDebugMessageEvent read FOnDebugMessage write FOnDebugMessage;
    property OnRequirePassword: TPdfPasswordEvent read FOnRequirePassword write FOnRequirePassword;
  end;

// Get informative line of text about a PDF object
function GetPdfObjectInfo(AObject: TPdfObject): string;

// Page functions

// Returns a pointer to the page's resources dictionary. If it returns nil,
// something is wrong with the PDF structure
function PdfPageResources(APage: TPdfDictionary): TPdfDictionary;

// Returns a pointer to the page's attribute if found in the page dictionary, or
// through inheritance in any of the higher page tree objects. Returns NIL if
// not found
function PdfPageAttribute(APage: TPdfDictionary; const AName: string): TPdfObject;

implementation

function GetPdfObjectInfo(AObject: TPdfObject): string;
// Get an informative line of text about a PDF object
begin
  Result := 'Not specified';

  // Object = nil
  if not assigned(AObject) then
  begin
    Result := 'nil';
    exit;
  end;

  // TPdfObject
  if AObject.ClassType = TPdfObject then
  begin
    Result := 'Ancestor TPdfObject';
    exit;
  end;

  // TPdfName
  if AObject.ClassType = TPdfName then
  begin
    Result := 'Name';
    exit;
  end;

  // TPdfNumber
  if AObject.ClassType = TPdfNumber then
  begin
    Result := 'Number';
    exit;
  end;

  // TPdfArray
  if AObject.ClassType = TPdfArray then
  begin
    Result := Format('Array with %d elements',
      [TPdfArray(AObject).ElementCount]);
    exit;
  end;

  // TPdfStream
  if AObject.ClassType = TPdfStream then
  begin
    Result := Format('Stream with compressed length %d',
      [TPdfStream(AObject).StreamLength]);
    exit;
  end;

  // TPdfString
  if AObject.ClassType = TPdfString then
  begin
    Result := Format('String with length %d',
      [length(TPdfString(AObject).Value)]);
    exit;
  end;

  // TPdfDictionary
  if AObject.ClassType = TPdfDictionary then
  begin
    Result := Format('Dictionary with %d entries',
      [TPdfDictionary(AObject).EntryCount]);
    exit;
  end;

  // TPdfBoolean
  if AObject.ClassType = TPdfBoolean then
  begin
    Result := 'Boolean';
    exit;
  end;

  // TPdfNull
  if AObject.ClassType = TPdfNull then
  begin
    Result := 'Null';
    exit;
  end;

  // TPdfIndirectObject
  if AObject.ClassType = TPdfIndirectObject then
  begin
    Result := Format('Indirect object number %d with %s',
      [TPdfIndirectObject(AObject).ObjectNum,
       GetPdfObjectInfo(TPdfIndirectObject(AObject).Value)]);
    exit;
  end;

  // TPdfIndirectRef
  if AObject.ClassType = TPdfIndirectRef then
  begin
    Result := Format('Indirect reference number %d',
      [TPdfIndirectObject(AObject).ObjectNum]);
    exit;
  end;

  // Unknown
  Result := 'Unknown PDF classtype';
end;

function PdfPageResources(APage: TPdfDictionary): TPdfDictionary;
// Returns a pointer to the page's resources dictionary. If it returns nil,
// something is wrong with the PDF structure
begin
  Result := nil;
  if not assigned(APage) then
    exit;
  Result := APage.DictionaryByKey('Resources');
  if not assigned(Result) then
    // if there's no resource, try the parent's resource
    Result := PdfPageResources(APage.DictionaryByKey('Parent'));
end;

function PdfPageAttribute(APage: TPdfDictionary; const AName: string): TPdfObject;
begin
  Result := nil;
  if not assigned(APage) then
    exit;
  Result := APage.ValueByKey(AName);
  if not assigned(Result) then
    // if there's no attribute, try the parent's attribute
    Result := PdfPageAttribute(APage.DictionaryByKey('Parent'), AName);
end;

{ TPdfPage }

function TPdfPage.Contents: TPdfObject;
begin
  Result := ValueByKey('Contents');
end;

function TPdfPage.CropBox: TpdfRectangle;
var
  ABox: TPdfObject;
begin
  // Get the cropbox of the PDF
  ABox := PdfPageAttribute(Self, 'CropBox');
  if not assigned(ABox) then
    // If not found, default to the mediabox
    ABox := PdfPageAttribute(Self, 'MediaBox');
  Result := PdfRectFromArray(ABox);
end;

function TPdfPage.GetAttribute(const AName: string): TPdfObject;
begin
  Result := ValueByKey(AName);
  if not assigned(Result) then
    // if there's no attribute, try our parent's attribute
    Result := PdfPageAttribute(DictionaryByKey('Parent'), AName);
end;

function TPdfPage.GetRotate: integer;
var
  AObj: TPdfObject;
begin
  AObj := GetAttribute('Rotate');
  if assigned(AObj) then
    Result := AObj.AsInteger
  else
    Result := 0;
end;

function TPdfPage.MediaBox: TPdfRectangle;
var
  ABox: TPdfObject;
begin
  // If not found, default to the mediabox
  ABox := PdfPageAttribute(Self, 'MediaBox');
  Result := PdfRectFromArray(ABox)
end;

function TPdfPage.Resources: TPdfDictionary;
// Returns a pointer to the page's resources dictionary. If it returns nil,
// something is wrong with the PDF structure
begin
  Result := DictionaryByKey('Resources');
  if not assigned(Result) then
    // if there's no resource, try the parent's resource
    Result := PdfPageResources(DictionaryByKey('Parent'));
end;

{ TPdfDocument }

procedure TPdfDocument.Clear;
begin
  FModified := False;
  FVersion := '1.5';
  FreeAndNil(FPdfStream);
  FreeAndNil(FPageTree);
  FreeAndNil(FEncryptor);
  FObjectRefs.Clear;
  FFonts.Clear;
end;

constructor TPdfDocument.Create(AOwner: TComponent);
begin
  inherited;
  // Defaults
  FModified := False;
  FVersion := '1.5';
  FObjectRefs := TObjectList.Create;
  FFonts := TObjectList.Create;
end;

destructor TPdfDocument.Destroy;
begin
  FreeAndNil(FPdfStream);
  FreeAndNil(FObjectRefs);
  FreeAndNil(FPageTree);
  FreeAndNil(FEncryptor);
  FreeAndNil(FFonts);
  FreeAndNil(FFormXObjects);
  inherited;
end;

procedure TPdfDocument.DoDebugMessage(Level: TPdfDebugLevel; const AMessage: string);
begin
  if Level = dlError then
  begin
    // Fatal errors
    case ErrorMethod of
    emEvent:
      if assigned(OnDebugMessage) then
        OnDebugMessage(Self, Level, AMessage);
    emDialog:
      ShowMessage(AMessage);
    emException:
      raise EPdfError.Create(AMessage);
    end;
  end else
    // Info and warnings
    if assigned(OnDebugMessage) then
      OnDebugMessage(Self, Level, AMessage);
end;

procedure TPdfDocument.FontAdd(AFont: TPdfFont);
begin
  if assigned(FFonts) and assigned(AFont) then
    FFonts.Add(AFont);
end;

function TPdfDocument.FontByDict(ADict: TPdfDictionary): TPdfFont;
// Search our font list, if the font is not found, we will create it and add it
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FontCount - 1 do
    if Fonts[i].Dict = ADict then
    begin
      Result := Fonts[i];
      exit;
    end;
end;

procedure TPdfDocument.FormXObjectAdd(AForm: TPdfFormXObject);
begin
  if not assigned(FFormXobjects) then
    FFormXObjects := TObjectList.Create;
  FFormXObjects.Add(AForm);
end;

function TPdfDocument.FormXObjectByStream(AStream: TPdfStream): TPdfFormXObject;
var
  i: integer;
begin
  Result := nil;
  if not assigned(FFormXObjects) then
    exit;
  for i := 0 to FFormXObjects.Count - 1 do
    if TPdfFormXObject(FFormXObjects[i]).Stream = AStream then
    begin
      Result := TPdfFormXObject(FFormXObjects[i]);
      exit;
    end;
end;

function TPdfDocument.GetFontCount: integer;
begin
  Result := 0;
  if assigned(FFonts) then Result := FFonts.Count;
end;

function TPdfDocument.GetFonts(Index: integer): TPdfFont;
begin
  Result := nil;
  if (Index >= 0) and (Index < FontCount) then
    Result := TPdfFont(FFonts[Index]);
end;

function TPdfDocument.GetObjectCount: integer;
begin
  Result := 0;
  if assigned(FObjectRefs) then
    Result := FObjectRefs.Count;
end;

function TPdfDocument.GetObjects(Index: integer): TPdfObjectRef;
begin
  Result := nil;
  if (Index >= 0) and (Index < ObjectCount) then
    Result := TPdfObjectRef(FObjectRefs[Index]);
end;

function TPdfDocument.GetObjectValue(Index: integer): TPdfObject;
var
  APos: integer;
  AObject: TPdfObjectRef;
begin
  Result := nil;
  AObject := Objects[Index];
  if not assigned(AObject) then
     exit;

  // Must we load?
  if not assigned(AObject.Value) and assigned(FPdfStream) then
  begin
    // Since we're loading we must store the original stream position and restore later
    APos := FPdfStream.Position;
    // We must load the indirect object..
    AObject.LoadFromPdf(Self, FPdfStream);
    // ..and get a pointer to it
    Result := AObject.Value;
    // Restore stream pos
    FPdfStream.Position := APos;
  end else
    Result := AObject.Value;
end;

function TPdfDocument.GetPages(Index: integer): TPdfPage;
begin
  Result := TPdfPage(FPageTree[Index]);
end;

function TPdfDocument.GetPageTree: TPdfPageTree;
var
  ATree: TPdfDictionary;
begin
  if not assigned(FPageTree) then
  begin
    // We do not have a pagetree yet.. create it
    ATree := TPdfDictionary(ObjectAdd(TPdfDictionary));
    // type = 'Pages'
    ATree.DictionaryType := 'Pages';
    // add 'Kids' array
    ATree.EntriesAdd(TPdfKeyValue.CreateWith('Kids', TPdfArray.Create));
    FPageTree := TpdfPageTree.Create(ATree);
  end;
  Result := FPageTree;
end;

function TPdfDocument.IsEncrypted: boolean;
begin
  Result := assigned(FEncryptor);
end;

procedure TPdfDocument.LoadFromFile(Filename: string);
var
  S: TStream;
  M: TMemoryStream;
begin
  // Checks
  if length(FileName) = 0 then
    raise Exception.Create('Filename is empty');
  if not FileExists(FileName) then
    raise EFOpenError.CreateFmt('Unable to open %s', [FileName]);

  // Create new stream - open as read only
  DoDebugMessage(dlInfo, Format('Opening file "%s"', [FileName]));
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if DelayPageLoad then
    begin
      // We use the file directly for (possibly huge) documents
      LoadFromStream(S);
    end else
    begin
      // We load the document at once, it makes sense to convert to memory stream
      M := TMemoryStream.Create;
      try
        M.CopyFrom(S, S.Size);
        M.Seek(0, soFromBeginning);
        LoadFromStream(M);
      finally
        M.Free;
      end;
    end;
  finally
    // If we loaded the entire document, we must free the stream
    if not DelayPageLoad then
      S.Free;
  end;
end;

procedure TPdfDocument.LoadFromStream(S: TStream);
var
  i: integer;
begin
  try
    // Clear the document, this frees FPdfStream and FEncryption
    Clear;

    // Make a reference
    FPdfStream := S;

    // Read the header
    DoDebugMessage(dlInfo, 'Reading PDF header');
    ReadHeader;

    // Read the trailer, this generates the ObjectRefs list
    DoDebugMessage(dlInfo, 'Reading PDF trailer');
    ReadTrailer;

    // Do we load all objects now?
    if not DelayPageLoad then
    begin
      for i := 0 to ObjectCount - 1 do
      begin
        if i < 100 then
          DoDebugMessage(dlInfo, Format('Loading indirect object @ pos %d', [Objects[i].Offset]))
        else
          if i = 100 then
            DoDebugMessage(dlInfo, Format('Loading %d more objects...', [ObjectCount - 100]));
        Objects[i].LoadFromPdf(Self, FPdfStream);
        if i < 100 then
          DoDebugMessage(dlInfo, Format('Loaded object info: %s', [GetPdfObjectInfo(Objects[i].Value)]));
      end;

      // Remove reference if we direct-load, otherwise this signals that the caller
      // will trust TPdfDocument to free the stream when done
      FPdfStream := nil;
    end;
    DoDebugMessage(dlInfo, Format('PDF read OK, contains %d pages', [PageTree.PageCount]));
  except
    on E: EPdfError do
      DoDebugMessage(dlError, Format('pos %d: %s', [S.Position, E.Message]));
  end;
end;

function TPdfDocument.ObjectAdd(AClass: TpdfObjectClass): TPdfObject;
var
  AObject: TPdfObjectRef;
begin
  AObject := TPdfObjectRef.Create;
  AObject.Value := AClass.Create;
  // to do: add ObjectIdx/generation
  FObjectRefs.Add(AObject);
  Result := AObject.Value; 
end;

function TPdfDocument.ObjectByValue(Value: TPdfObject): TPdfObjectRef;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ObjectCount - 1 do
    if Objects[i].Value = Value then
    begin
      Result := Objects[i];
      exit;
    end;
end;

procedure TPdfDocument.ReadHeader;
// Scan the first 1024 bytes for a header %PDF-X.XX (IN H3.4.1-12)
var
  P: integer;
begin
  P := PosInStream(FPdfStream, cPdfHeader, 1024);
  if P < 0 then
    raise EPdfError.Create('No header found');
  FPdfStream.Position := P + length(cPdfHeader);
  Version := ReadTokenFromPdfStream(FPdfStream);
  if length(Version) < 3 then
    raise EPdfError.CreateFmt('Illegal version number %s', [Version]);
end;

procedure TPdfDocument.ReadTrailer;
// Scan the file from the end for %%EOF and "trailer" (IN H3.4.4-14)
var
  ATrailer, ARevision: TPdfTrailer;
  ARoot: TPdfIndirectRef;
  AEncrypt: TPdfDictionary;
  APass: string;
begin
  // load the trailer
  ATrailer := TPdfTrailer.Create(Self);
  try
    ATrailer.LoadFromPdf(FPdfStream);
    DoDebugMessage(dlInfo, Format('Getting revision %d', [Revision]));
    ARevision := ATrailer.GetRevision(Revision);
    DoDebugMessage(dlInfo, 'Generating object list');
    ARevision.GenerateObjectList(FObjectRefs);
    DoDebugMessage(dlInfo, Format('Total number of objects: %d', [ObjectCount]));
    // Find the root
    ARoot := TPdfIndirectRef(ARevision.Dictionary.ValueByKey('Root'));
    if not assigned(ARoot) then
      ARevision := ATrailer;

    // Find the catalog
    FCatalog := ARevision.Dictionary.DictionaryByKey('Root', 'Catalog');

    // Find the pagetree
    FreeAndNil(FPageTree);
    FPageTree := TPdfPageTree.Create(FCatalog.DictionaryByKey('Pages'));

    // Find the File ID
    FFileID := ARevision.Dictionary.ArrayByKey('ID');

    // Encryption dictionary
    AEncrypt := ARevision.Dictionary.DictionaryByKey('Encrypt');
    if assigned(AEncrypt) then
    begin
      FEncryptor := TPdfEncryptor.Create(Self);
      FEncryptor.Dict := AEncrypt;
      with FEncryptor do
      begin
        if CanDecode then
        begin
          if RequirePassword(uaRead) then
          begin
            if assigned(OnRequirePassword) then
              OnRequirePassword(Self, uaRead, APass)
            else
              APass := InputBox('PDF reading requires a password', 'Password', '');
            if not Encryptor.CheckPassword(APass, uaRead) then
              raise EPdfError.Create(sPdfIncorrectPass);
          end;
        end else
        begin
          //DoDebugMessage(dlWarning, Format(sPdfUnableToDecode, [Encryptor.AlgorithmName]));
          raise EPdfError.CreateFmt(sPdfUnableToDecode, [Encryptor.AlgorithmName]);
        end;
      end;
      DoDebugMessage(dlInfo, Format('Document is encrypted with "%s"', [Encryptor.AlgorithmName]));
    end;
  finally
    ATrailer.Free;
  end;
end;

procedure TPdfDocument.SaveToFile(Filename: string);
begin
// to do
end;

procedure TPdfDocument.SaveToStream(S: TStream);
begin
// to do
end;

procedure TPdfDocument.SetModified(const Value: boolean);
begin
  FModified := Value;
end;

end.
