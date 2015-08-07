{ unit sdPdfFileStruct

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the objects as described in the PDF reference
  chapter 3.4: Syntax - File structure

  Author: Nils Haeck M.Sc.

  Created:
    06Jan2004

  TODO:
    - Version 1.5 Object streams (3.4.6)
    - Version 1.5 Cross-reference streams (3.4.7)

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfFileStruct;

interface

uses
  Classes, Contnrs, sdPdfObjects, sdPdfUtil, SysUtils;

type

  // PDF object reference in TPdfDocument's object reference list Objects[]. This
  // list contains a collection of all crossref index tables in the PDF file
  TPdfObjectRef = class(TPersistent)
  private
    FOffset: integer;     // If IsUsed = true, points to position in stream otherwise to next free object
    FGeneration: integer; // Generation number
    FValue: TPdfObject;   // Holds PdfObject or nil if not loaded or unused
    FIsUsed: boolean;     // If true, the XRef table states this object is in use
    FObjectIdx: integer;  // Index of this object ref
  public
    destructor Destroy; override;
    procedure LoadFromPdf(Pdf: TObject; S: TStream);
    property Generation: integer read FGeneration write FGeneration;
    property ObjectIdx: integer read FObjectIdx write FObjectIdx;
    property IsUsed: boolean read FIsUsed write FIsUsed;
    property Offset: integer read FOffset write FOffset;
    property Value: TPdfObject read FValue write FValue;
  end;

  // An entry item in the trailer's cross-reference table
  TPdfXRefItem = class(TPersistent)
  private
    FOffset: integer;
    FGeneration: integer;
    FObjectIdx: integer;
    FObjectTyp: char;
  public
    property Generation: integer read FGeneration write FGeneration;
    property ObjectIdx: integer read FObjectIdx write FObjectIdx;
    property ObjectTyp: char read FObjectTyp write FObjectTyp;
    property Offset: integer read FOffset write FOffset;
  end;

  // The cross-reference table object in the PDF file's trailer
  TPdfXRefTable = class(TPersistent)
  private
    FItems: TObjectList;
    function GetItems(Index: integer): TPdfXRefItem;
    function GetItemCount: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ItemsAdd(AItem: TPdfXRefItem);
    procedure LoadFromPdf(S: TStream); virtual;
    property ItemCount: integer read GetItemCount;
    property Items[Index: integer]: TPdfXRefItem read GetItems;
  end;

  // PDF document trailer parser, which will find the trailer and the cross-
  // ref table for the revision selected (Revision 0 is most recent, 1 is before-
  // last, ectetera)
  TPdfTrailer = class(TPersistent)
  private
    FXrefOffset: integer;
    FXRefTable: TPdfXRefTable;
    FDictionary: TPdfDictionary;
    FPrevious: TPdfTrailer;
    FOwner: TObject;
  protected
    procedure LoadXRefTableAndDict(S: TStream; Warn: boolean); virtual;
    property Owner: TObject read FOwner write FOwner;
    property XRefOffset: integer read FXRefOffset write FXRefOffset;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure DoDebugMessage(Level: TPdfDebugLevel; const AMessage: string);
    procedure LoadFromPdf(S: TStream); virtual;
    function GetRevision(RevisionNumber: integer): TPdfTrailer;
    procedure GenerateObjectList(ObjectRefs: TObjectList);
    property Previous: TPdfTrailer read FPrevious write FPrevious;
    property XRefTable: TPdfXRefTable read FXRefTable write FXRefTable;
    property Dictionary: TPdfDictionary read FDictionary write FDictionary;
  end;

const

  // Some PDF file format string constants

  cPdfHeader  = '%PDF-';
  cPdfEOF     = '%%EOF';
  cPdfTrailer = 'trailer';

implementation

uses
  sdPdfDocument;

{ TPdfTrailer }

constructor TPdfTrailer.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TPdfTrailer.Destroy;
begin
  FreeAndNil(FPrevious);
  FreeAndNil(FXRefTable);
  FreeAndNil(FDictionary);
  inherited;
end;

procedure TPdfTrailer.DoDebugMessage(Level: TPdfDebugLevel; const AMessage: string);
begin
  if assigned(Owner) then
    TPdfDocument(Owner).DoDebugMessage(Level, AMessage);
end;

procedure TPdfTrailer.GenerateObjectList(ObjectRefs: TObjectList);
// Calling this with RevisionNumber > 0 will cause one of the previous revisions
// to be loaded
var
  i, j, ASize: integer;
  RevisionList: TList;
  AObjectRef: TPdfObjectRef;
// local
procedure RecursiveAddRevisions(ATrailer: TPdfTrailer);
begin
  RevisionList.Add(ATrailer);
  if assigned(ATrailer.Previous) then
    RecursiveAddRevisions(ATrailer.Previous);
end;
// main
begin
  RevisionList := TList.Create;
  try
    // Add all revisions
    RecursiveAddRevisions(Self);
    ASize := Dictionary.IntegerByKey('Size');

    // Initialize
    ObjectRefs.Clear;
    ObjectRefs.Capacity := ASize;
    for i := 0 to ASize - 1 do begin
      AObjectRef := TPdfObjectRef.Create;
      ObjectRefs.Add(AObjectRef);
    end;

    // Start from the bottom and fill objects
    for i := RevisionList.Count - 1 downto 0 do
      with TPdfTrailer(RevisionList[i]).XRefTable do
        for j := 0 to ItemCount - 1 do begin
          // copy items
          AObjectRef := TPdfObjectRef(ObjectRefs[Items[j].ObjectIdx]);
          AObjectRef.IsUsed := Items[j].ObjectTyp = 'n';
          AObjectRef.Offset := Items[j].Offset;
          AObjectRef.Generation := Items[j].Generation;
          AObjectRef.ObjectIdx := Items[j].ObjectIdx;
        end;
  finally
    RevisionList.Free;
  end;
end;

function TPdfTrailer.GetRevision(RevisionNumber: integer): TPdfTrailer;
// The revision number is based from 0 (most recent version) and goes up to
// 1 (older), 2 (yet older), etc. until at some point there are no older
// revisions (and an exception will be raised)
begin
  // Return older revisions
  if RevisionNumber > 0 then begin
    if not assigned(FPrevious) then
      raise EPdfError.Create(sPdfUnknownRevision);
    Result := FPrevious.GetRevision(RevisionNumber - 1);
  end else
    // Return this revision
    Result := Self;
end;

procedure TPdfTrailer.LoadFromPdf(S: TStream);
// Load the trailer from the PDF file. We do this by approach from the end of
// the file, read the trailer and XRef table, and scan for previous trailers
// IN H3.4.4-14
var
  Index: integer;
begin
  // Move to the end of the file
  DoDebugMessage(dlInfo, 'Seeking last trailer position');
  S.Seek(0, soFromEnd);

  // Find position of EOF
  Index := PosInStreamBackwards(S, cPdfEOF, S.Position - 1024);
  if Index <= 0 then
    raise EPdfError.Create(sPdfEOFMissing);

  // Read offset position backwards
  FXrefOffset := StrToIntDef(ReadTokenFromPdfStreamBackwards(S), -1);

  // Load XRefTable and trailer dictionary
  LoadXRefTableAndDict(S, True);
end;

procedure TPdfTrailer.LoadXRefTableAndDict(S: TStream; Warn: boolean);
// Load the cross-reference table and trailer dictionary from the stream. if
// Warn is True, a warning will be issued if the "Root" property is missing.
// This should only be used for top level trailer dictionaries.
var
  APrev: TPdfObject;
begin
  S.Seek(FXRefOffset, soFromBeginning);
  DoDebugMessage(dlInfo, Format('Loading XREF table @ pos %d', [S.Position]));

  // Recreate and load XRef table
  FreeAndNil(FXRefTable);
  FXRefTable := TPdfXRefTable.Create;
  FXRefTable.LoadFromPdf(S);

  // Read 'trailer'
  DoDebugMessage(dlInfo, Format('Loading trailer dict @ pos %d', [S.Position]));
  if not (ReadTokenFromPdfStream(S) = 'trailer') then
    raise EPdfError.Create(sPdfTrailerNotFound);

  // Recreate and load dictionary
  FreeAndNil(FDictionary);
  FDictionary := TPdfDictionary.Create;
  FDictionary.LoadFromPdf(Owner, S);

  // checks
  if not assigned(FDictionary.ValueByKey('Size')) then
    DoDebugMessage(dlWarning, 'Trailer dict misses "Size" entry');
  if not assigned(FDictionary.ValueByKey('Root')) and Warn then
    DoDebugMessage(dlWarning, 'Trailer dict misses "Root" entry');

  // Load previous (iterative process)
  APrev := Dictionary.ValueByKey('Prev');
  if assigned(APrev) then begin
    FreeAndNil(FPrevious);
    FPrevious := TPdfTrailer.Create(Owner);
    FPrevious.XRefOffset := APrev.AsInteger;
    FPrevious.LoadXRefTableAndDict(S, False);
  end;
end;

{ TPdfXRefTable }

constructor TPdfXRefTable.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TPdfXRefTable.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TPdfXRefTable.GetItemCount: integer;
begin
  Result := 0;
  if assigned(FItems) then Result := FItems.Count;
end;

function TPdfXRefTable.GetItems(Index: integer): TPdfXRefItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < ItemCount) then
    Result := TPdfXRefItem(FItems[Index]);
end;

procedure TPdfXRefTable.ItemsAdd(AItem: TPdfXRefItem);
begin
  if assigned(FItems) and assigned(AItem) then
    FItems.Add(AItem);
end;

procedure TPdfXRefTable.LoadFromPdf(S: TStream);
type
  TXRefRecord = packed record
    Offset: array[0..9] of char;
    Skip1: char;
    Gener:  array[0..4] of char;
    Skip2: char;
    ObjectTyp: char;
  end;
var
  i: integer;
  Ch: char;
  Start, Count: integer;
  Rec: TXRefRecord;
  AItem: TPdfXRefItem;
begin
  // Read keyword 'xref'
  if ReadTokenFromPdfStream(S) <> 'xref' then
    raise EPdfError.Create(sPdfXRefSyntaxErr);

  // Read subsections
  repeat
    Ch := ReadNextNonWS(S);
    S.Seek(-1, soFromCurrent);
    if not(Ch in ['0'..'9']) then break;
    // Load two numbers
    Start := ReadIntegerFromPdf(S);
    Count := ReadIntegerFromPdf(S);
    SkipWhitespace(S);
    // Load entries
    for i := 0 to Count - 1 do begin
      if S.Read(Rec, SizeOf(Rec)) <> SizeOf(Rec) then
        raise EPdfError.Create(sPdfUnexpectedEOS);
      try
        AItem := TPdfXRefItem.Create;
        AItem.ObjectIdx  := Start + i;
        AItem.Offset     := StrToInt(Rec.Offset);
        AItem.Generation := StrToInt(Rec.Gener);
        AItem.ObjectTyp  := Rec.ObjectTyp;
        ItemsAdd(AItem);
        SkipWhitespace(S);
      except
        raise EPdfError.Create(sPdfInvalidXRefEntry);
      end;
    end;
  until False;
end;

{ TPdfObjectRef }

destructor TPdfObjectRef.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TPdfObjectRef.LoadFromPdf(Pdf: TObject; S: TStream);
var
  AObject: TPdfIndirectObject;
begin
  if not IsUsed then exit;

  // Only load if not yet done
  if assigned(FValue) then exit;

  // Find position
  S.Seek(Offset, soFromBeginning);

  // Load indirect object
  AObject := TPdfIndirectObject.Create;
  try
    AObject.LoadFromPdf(Pdf, S);
    // Check
    if AObject.ObjectNum <> ObjectIdx then
      raise EPdfError.Create(sPdfObjNumMismatch);
    FValue := AObject.Value;
    AObject.Value := nil;
  finally
    AObject.Free;
  end;
end;

end.
