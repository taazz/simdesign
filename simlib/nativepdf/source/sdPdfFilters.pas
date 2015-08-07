{ unit sdPdfFilters

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the stream filters as described in the PDF reference
  chapter 3.3: Syntax - Filters

  TODO:
  - Add AsciiHexDecode
  - Add RunlengthDecode
  - Add JBIG2 Decode (PDF1.4)
  - Add JPX Decode (PDF1.5)
  - Add Crypt Decode (PDF1.5)

  Author: Nils Haeck M.Sc.

  Changes:
    27Sep2005 - Replaced DCTDecode Jpeg implementation with native one that
                allows reading of CMYK etc
    27Sep2005 - Added PNG unprediction
    26Sep2005 - Added Ascii85Decode
    21Sep2005 - fixed CCITTFax G3/G4 with Joris' code
    21Sep2005 - Added DCTDecode
    17Aug2004 - Added LZWDecode
    15Jan2004 - Added CCITTFaxDecode
    07Jan2004 - Added Flate decode
    06Jan2004 - Created

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfFilters;

interface

uses
  Windows, Classes, SysUtils, Contnrs, sdPdfObjects, sdPdfUtil, sdBitstreams,
  sdPdfFilterTables, Graphics, NativeJpg, Math, sdColorTransforms;

type

  TPdfFilterItem = class(TPersistent)
  private
    FParams: TPdfObject;
  public
    constructor Create; virtual;
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); virtual; abstract;
    property Params: TPdfObject read FParams write FParams;
  end;

  TPdfFilterItemClass = class of TPdfFilterItem;

  TPdfFilter = class(TPersistent)
  private
    FFilters: TObjectList;
    FOwner: TObject;
    function GetFilters(Index: integer): TPdfFilterItem;
    function GetFilterCount: integer; // pointer to TPdfDocument
  public
    constructor Create(Owner: TObject); virtual;
    destructor Destroy; override;
    procedure SetParams(Filter: TPdfObject; Params: TPdfObject);
    function Decompress(Source, Dest: TMemoryStream): integer;
    procedure FiltersAdd(AFilter: TPdfFilterItem);
    property FilterCount: integer read GetFilterCount;
    property Filters[Index: integer]: TPdfFilterItem read GetFilters; default;
  end;

  TASCIIHexDecodeItem = class(TPdfFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  TASCII85DecodeItem = class(TPdfFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  TColumnBasedFilterItem = class(TPdfFilterItem)
  protected
    procedure UndoPNGPrediction(Dest: TStream);
  end;

  // This filter performs Flate decode/encode
  // We use the open source MPL implementation of Jean-loup Gailly and Mark Adler
  // as found in Filters\MZLIB.PAS (converted to Delphi by Mike Lischke)
  TFlateDecodeItem = class(TColumnBasedFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  // This filter performs LZW decode/encode.
  // Loosely based on the open source MPL implementation of Mike Lischke
  TLZWDecodeItem = class(TColumnBasedFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  // This filter performs Runlength decode/encode.
  TRunlengthDecodeItem = class(TPdfFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  // This filter performs CITTFaxDecode G3 (1D/2D) and G4
  TCCITTFaxDecodeItem = class(TPdfFilterItem)
  private
    PSrc: pointer;
    FPackedSize: integer;
  protected
    procedure CodecBufFill(var SucInBuf: Pointer; var SucInBufLen: Cardinal);
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

  // This filter performs DCT decode/encode.
  TDctDecodeItem = class(TPdfFilterItem)
  public
    procedure Decompress(Source, Dest: TMemoryStream; SourceSize: integer); override;
  end;

procedure RegisterFilterClass(AName, AAbrev: string; AClass: TPdfFilterItemClass);

resourcestring

  sPdfUnknownFilter          = 'Unknown filter "%s"';
  sPdfIllegalCharInAscii85   = 'Illegal character in ASCII 85 codec';
  sPdfOverflowInAscii85      = 'Overfow in ASCII 85 codec';
  sDestMustBeMemStream       = 'Destination stream must be memory stream';
  sIllegalPNGPredictor       = 'Illegal PNG predictor';
  sUnsupportedColorspaceConv = 'Unsupported colorspace conversion';

implementation

uses
  MZLib, AsCcittFax;

type

  TPdfFilterListEntry = class
  public
    FName: string;
    FAbbrev: string;
    FClass: TPdfFilterItemClass;
  end;

const // LZW encoding and decoding support
  NoLZWCode = 4096;

var
  // Global filter list
  GPdfFilterList: TObjectList = nil;

procedure RegisterFilterClass(AName, AAbrev: string; AClass: TPdfFilterItemClass);
var
  AEntry: TPdfFilterListEntry;
begin
  AEntry := TPdfFilterListEntry.Create;
  AEntry.FName := AName;
  AEntry.FAbbrev := AAbrev;
  AEntry.FClass := AClass;
  if assigned(GPdfFilterList) then
    GPdfFilterList.Add(AEntry);
end;

function CreateFilterItemWithName(AName: string): TPdfFilterItem;
// Loop through the list of available filters and create the one we find (if any)
var
  i: integer;
  AEntry: TPdfFilterListEntry;
begin
  Result := nil;
  if not assigned(GPdfFilterList) then exit;
  for i := 0 to GPdfFilterList.Count - 1 do begin
    AEntry := TPdfFilterListEntry(GPdfFilterList[i]);
    if (AEntry.FName = AName) or (AEntry.FAbbrev = AName) then begin
      Result := AEntry.FClass.Create;
      exit;
    end;
  end;
end;

// Calculation of the paeth predictor
function PaethPredictor(a, b, c: byte): byte;
var
  pa, pb, pc: integer;
begin
  // a = left, b = above, c = upper left
  pa := abs(b - c);      // distances to a, b, c
  pb := abs(a - c);
  pc := abs(a + b - c * 2);

  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

{ TPdfFilterItem }

constructor TPdfFilterItem.Create;
begin
  inherited Create;
end;

{ TPdfFilter }

constructor TPdfFilter.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
  FFilters := TObjectList.Create;
end;

function TPdfFilter.Decompress(Source, Dest: TMemoryStream): integer;
// Decompress the source to dest using the chain of filters
var
  i: integer;
  M1, M2: TMemoryStream;
  S, D: TMemoryStream;
begin
  Result := Source.Size;
  case Filtercount of
  0:
    begin
      Dest.CopyFrom(Source, Source.Size);
      Result := Dest.Size;
    end;
  1:
    begin
      Filters[0].Decompress(Source, Dest, Source.Size);
      Result := Dest.Size;
    end;
  else
    // 2 or more
    M1 := TMemoryStream.Create;
    M2 := TMemoryStream.Create;
    try
      D := nil;
      // All steps but last
      for i := 0 to FilterCount - 2 do begin
        // Setup stream pointers
        if i = 0 then begin
          S := Source;
          D := M1;
        end else begin
          if Odd(i) then begin
            S := M1;
            D := M2;
          end else begin
            S := M2;
            D := M1;
          end;
          S.Position := 0;
          D.Position := 0;
          D.Size := 0;
        end;
        // Do the decompress
        Filters[i].Decompress(S, D, Result);
        Result := D.Size;
      end;
      // Last step
      D.Position := 0;
      Filters[FilterCount - 1].Decompress(D, Dest, Result);
      Result := Dest.Size;
    finally
      M1.Free;
      M2.Free;
    end;
  end;
end;

destructor TPdfFilter.Destroy;
begin
  FreeAndNil(FFilters);
  inherited;
end;

procedure TPdfFilter.FiltersAdd(AFilter: TPdfFilterItem);
begin
  if assigned(FFilters) and assigned(AFilter) then
    FFilters.Add(AFilter);
end;

function TPdfFilter.GetFilterCount: integer;
begin
  Result := 0;
  if assigned(FFilters) then Result := FFilters.Count;
end;

function TPdfFilter.GetFilters(Index: integer): TPdfFilterItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < Filtercount) then
    Result := TPdfFilterItem(FFilters[Index]);
end;

procedure TPdfFilter.SetParams(Filter, Params: TPdfObject);
// Split out the Filter array over the Filters[]
var
  i: integer;
  AItem: TPdfFilterItem;
// local
procedure AddFilter(AName: string; AParms: TPdfObject);
begin
  AItem := CreateFilterItemWithName(AName);
  if not assigned(AItem) then
    raise EPdfError.CreateFmt(sPdfUnknownFilter, [AName]);
  if assigned(AParms) then
    AItem.Params := DerefPdf(AParms);
  FiltersAdd(AItem);
end;
// main
begin
  FFilters.Clear;

  // Single filter?
  if (Filter is TPdfName) then begin
    AddFilter(Filter.AsString, Params);
  end else

    // Array of filters?
    if (Filter is TPdfArray) then begin
      if not (Params is TPdfArray) then Params := nil;
      for i := 0 to TPdfArray(Filter).ElementCount - 1 do begin
        if assigned(Params) then
          AddFilter(TPdfArray(Filter).StringByIndex(i), TPdfArray(Params)[i])
        else
          AddFilter(TPdfArray(Filter).StringByIndex(i), nil)
      end;
    end;
end;

{ TASCIIHexDecodeItem }

procedure TASCIIHexDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
begin
  // to do
  raise EPdfError.CreateFmt(sPdfUnknownFilter, ['ASCIIHexDecode']);
end;

{ TASCII85DecodeItem }

procedure TASCII85DecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
const
  cZeroes: array[0..3] of byte = (0, 0, 0, 0);
var
  Count: integer;
  C: byte;
  EOF: boolean;
  B: array[0..3] of byte;
  W: int64; // must be int64 to avoid overflow during calc
begin
  Source.Position := 0;
  EOF := False;
  repeat
    Count := 0;
    W := 0;
    repeat
      Source.Read(C, 1);
      case C of
      33..117: // standard characters
        begin
          W := W * 85 + C - 33;
          inc(Count);
        end;
      ord('z'): // 4x zeroes
        begin
          if Count > 0 then
            raise EPdfError.Create(sPdfIllegalCharInAscii85);
          // Add 4 zeroes to the dest stream
          Dest.Write(cZeroes, 4);
          break;
        end;
      ord('~'): // EOF
        begin
          EOF := True;
        end;
      9, 10, 13:;// skip whitespaces
      else
        raise EPdfError.Create(sPdfIllegalCharInAscii85);
      end;

      // End condition
      if (Count = 5) or EOF then begin
        if Count = 1 then
          raise EPdfError.Create(sPdfIllegalCharInAscii85);
        if W > $FFFFFFFF then
          raise EPdfError.Create(sPdfOverflowInAscii85);
        if Count > 1 then begin
          dec(Count);
          B[3] := W and $FF; W := W shr 8;
          B[2] := W and $FF; W := W shr 8;
          B[1] := W and $FF; W := W shr 8;
          B[0] := W and $FF;
          Dest.Write(B[4 - Count], Count);
        end;
        break;
      end;
    until False;
  until EOF;
end;

{ TColumnBasedFilterItem }

procedure TColumnBasedFilterItem.UndoPNGPrediction(Dest: TStream);
const
  cprNone    = 10;
  cprSub     = 11;
  cprUp      = 12;
  cprAverage = 13;
  cprPaeth   = 14;
  cprOptimum = 15;
var
  x, y: integer;
  Predictor: integer;
  Columns, Rows, SpanWidth: integer;
  Colors: integer;
  BitsPerComponent: integer;
  Offset: integer;
  Temp: array of byte;
  UseOptimal: boolean;
  S, D, L, U, UL: PByte;
  Left, Up, UpLeft, paeth: byte;
begin
  // Set options from dictionary
  if not (Params is TPdfDictionary) then exit;
  with TPdfDictionary(Params) do begin

    // Predictor
    Predictor := IntegerByKeyDefault('Predictor', 1);
    // we don't need to handle prediction if predictor <= 10
    if Predictor <= cprNone then exit;
    UseOptimal := Predictor = cprOptimum;

    // Additional info
    Columns  := IntegerByKeyDefault('Columns', 1);
    Colors := IntegerByKeyDefault('Colors', 1);
    BitsPerComponent := IntegerByKeyDefault('BitsPerComponent', 8);

  end;

  // Calculate offset
  Offset := Colors * BitsPerComponent div 8;
  SpanWidth := Columns * Offset;
  if Predictor = cprOptimum then
    Rows := Dest.Size div (SpanWidth + 1)
  else
    Rows := Dest.Size div SpanWidth;

  // Unpredicted image
  SetLength(Temp, Rows * SpanWidth);

  // Source and Dest pointers
  S := TMemoryStream(Dest).Memory;
  D := @Temp[0];

  // Start unpredicting
  for y := 0 to Rows - 1 do begin

    // The optimal filter for this row is encoded in the first byte
    if UseOptimal then begin
      // In PNG prediction the constants are 10 less
      Predictor := S^ + 10;
      inc(S);
    end;

    // Move data to temp image for this scanline
    Move(S^, D^, SpanWidth);
    inc(S, Spanwidth);

    // Unpredict
    case Predictor of
    cprSub:
      begin
        L := D;
        inc(D, Offset);
        for x := Offset to SpanWidth - 1 do begin
          D^ := (D^ + L^) and $FF;
          inc(D);
          inc(L);
        end;
      end;
    cprUp:
      begin
        if y = 0 then begin
          inc(D, SpanWidth);
          continue;
        end;
        U := D;
        dec(U, SpanWidth);
        for x := 0 to SpanWidth - 1 do begin
          D^ := (D^ + U^) and $FF;
          inc(D);
          inc(U);
        end;
      end;
    cprAverage:
      begin
        L := D;
        dec(L, Offset);
        U := D;
        dec(U, SpanWidth);
        for x := 0 to SpanWidth - 1 do begin
          if y = 0 then
            Up := 0
          else
            Up := U^;
          if x < Offset then
            Left := 0
          else
            Left := L^;
          D^ := (D^ + (Left + Up) div 2) and $FF;
          inc(D);
          inc(L);
          inc(U);
        end;
      end;
    cprPaeth:
      begin
        L := D;
        dec(L, Offset);
        U := D;
        dec(U, SpanWidth);
        UL := U;
        dec(UL, Offset);
        for x := 0 to SpanWidth - 1 do begin
          if y = 0 then
            Up := 0
          else
            Up := U^;
          if x < Offset then
            Left := 0
          else
            Left := L^;
          if (x < Offset) or (y = 0) then
            UpLeft := 0
          else
            UpLeft := UL^;
          Paeth := PaethPredictor(Left, Up, Upleft);
          D^ := (D^ + Paeth) and $FF;
          inc(D);
          inc(L);
          inc(U);
          inc(UL);
        end;
      end;
    else
      raise EPdfError.Create(sIllegalPNGPredictor);
    end;//case
  end;

  // Move temp image back to stream
  Move(Temp[0], TMemoryStream(Dest).Memory^, length(Temp));
  Dest.Size := length(Temp);

end;

{ TFlateDecodeItem }

procedure TFlateDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
// Decode the data in Source with Flate
var
  AStream: TZState;
  PSrc, PDst: pointer;
  AResult, ASize: integer;
const
  cBufferSize = 100 * 1024; // 100 Kb should be enough for single-buffer in most cases
begin
  // Copy to memory or set pointer
  PSrc := TMemoryStream(Source).Memory;
  // Buffer
  GetMem(PDst, cBufferSize);
  try
    // Setup structure
    FillChar(AStream, SizeOf(AStream), 0);
    AStream.NextInput := PSrc;
    AStream.AvailableInput := SourceSize;
    // Initialize decoding
    AResult := InflateInit(AStream);
    if AResult < 0 then
      raise EPdfError.CreateFmt(sPdfFlateDecodeErr, [AResult]);
    // Do decoding in blocks of buffer size
    repeat
      // Point stream to output buffer
      AStream.NextOutput := PDst;
      AStream.AvailableOutput := cBufferSize;
      // Inflate
      AResult := Inflate(AStream, Z_SYNC_FLUSH);
      if AResult < 0 then
        raise EPdfError.CreateFmt(sPdfFlateDecodeErr, [AResult]);
      // and write to the destination stream
      ASize := cBufferSize - AStream.AvailableOutput;
      Dest.Write(PDst^, ASize);
    until AResult = Z_STREAM_END;
    // Finalize
    AResult := InflateEnd(AStream);
    if AResult < 0 then
      raise EPdfError.CreateFmt(sPdfFlateDecodeErr, [AResult]);
  finally
    FreeMem(PDst);
  end;

  // Undo PNG prediction if neccesary
  UndoPNGPrediction(Dest);
end;

{ TLZWDecodeItem }

procedure TLZWDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
// Decode the data in Source with LZW (TIFF variant)
var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  // Copy to memory or set pointer
  SourcePtr := TMemoryStream(Source).Memory;
  // initialize parameter
  ClearCode := 1 shl 8;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeSize := 9;
  CodeMask := (1 shl CodeSize) - 1;

  // init code table
  for i := 0 to ClearCode - 1 do begin
    Prefix[i] := NoLZWCode;
    Suffix[i] := i;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;
  while SourceSize > 0 do begin
    // read code from bit stream
    Inc(Data, Cardinal(SourcePtr^) shl (24 - Bits));
    Inc(Bits, 8);
    while Bits >= CodeSize do begin
      // current code
      Code := (Data and ($FFFFFFFF - CodeMask)) shr (32 - CodeSize);
      // mask it
      Data := Data shl CodeSize;
      Dec(Bits, CodeSize);

      if Code = EOICode then Exit;

      // handling of clear codes
      if Code = ClearCode then begin
        // reset of all variables
        CodeSize := 9;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then begin
        FirstChar := Suffix[Code];
        Dest.Write(FirstChar, 1);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;
      if FreeCode < 4096 then Inc(FreeCode);

      // increase code size if necessary
      if (FreeCode = CodeMask) and
          (CodeSize < 12) then begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Dest.Write(StackPointer^, 1);
      until Cardinal(StackPointer) <= Cardinal(@Stack);
    end;
    Inc(SourcePtr);
    Dec(SourceSize);
  end;
  // Undo PNG prediction if neccesary
  UndoPNGPrediction(Dest);
end;

{ TRunlengthDecodeItem }

procedure TRunlengthDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
begin
  // to do
  raise EPdfError.CreateFmt(sPdfUnknownFilter, ['RunlengthDecode']);
end;

{ TCITTFaxDecodeItem }

procedure TCCITTFaxDecodeItem.CodecBufFill(var SucInBuf: Pointer; var SucInBufLen: Cardinal);
begin
  if assigned(PSrc) then begin
    // Just pass the whole buffer at once
    SucInBuf := PSrc;
    SucInBufLen := FPackedSize;
    PSrc := nil;
  end else begin
    // This should not happen, indicates premature end of data
    SucInBuf := nil;
    SucInBufLen := 0;
  end;
end;

procedure TCCITTFaxDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
var
  x: integer;
  K: integer;
  Codec: TAsCcittFaxDecompressor;
  R: pointer;
  B: PByte;
  ByteAlign: boolean;
  Columns: integer;
  ScanWidth: integer;
  BlackIs1: boolean;
const
  cZeroOrOne: array[boolean] of char = ('0', '1');
  cSpace: char = ' ';
  cCRLF: string = #13#10;
begin
  // Set options from dictionary
  if not (Params is TPdfDictionary) then
    raise EPdfError.Create(sPdfInvFilterParam);
  with TPdfDictionary(Params) do begin

    // K-factor determines if we use G31D (K=0), G32D (K=1) or G42D (K=-1) - See 3.3.5
    K := IntegerByKeyDefault('K', 0);

    // Byte alignment
    ByteAlign := BooleanByKeyDefault('EncodedByteAlign', False);

    // Width and Height of image
    Columns  := IntegerByKeyDefault('Columns', 1728);
    BlackIs1 := BooleanByKeyDefault('BlackIs1', False);

    // Correct destination size to make sure we're byte-aligned
    ScanWidth := (Columns + 7) div 8;
  end;

  // Copy to memory or set pointer
  PSrc := TMemoryStream(Source).Memory;

  // Buffer
  FPackedSize := SourceSize;
  // Joris' implementation
  if K = -1 then
    Codec := TAsCcittFax4Decompressor.Create
  else
    Codec := TAsCcittFax3Decompressor.Create;
  try
    if K = 1 then
      TAsCcittFax3Decompressor(Codec).Options := [acf3o2D];
    Codec.RowPixels := Columns;
    Codec.FillorderLsb2Msb := not ByteAlign;
    Codec.OnInBufFill := CodecBufFill;
    Codec.DecodeStart;
    try
      repeat
        R := nil;
        Codec.DecodeRow(R);
        if assigned(R) and (Codec.Eolcnt = 0) then begin
          if not BlackIs1 then begin
            B := R;
            for x := 0 to ScanWidth - 1 do begin
              B^ := B^ xor $FF;
              inc(B);
            end;
          end;
          Dest.Write(R^, ScanWidth);
        end else
          break;
      until False;
    except
      // dump Dest data
      //Dest.SaveToFile('c:\temp\debug.txt');
    end;
  finally
    Codec.Free;
  end;

end;

{ TDctDecodeItem }

procedure TDctDecodeItem.Decompress(Source, Dest: TMemoryStream; SourceSize: integer);
var
  Jpg: TsdJpegGraphic;
  D: PByte;
begin
  Jpg := TsdJpegGraphic.Create;
  try
{//    Jpg.DctMethod := jdmFloatingPoint;
    Jpg.DctMethod := jdmIntegerFast;
    Jpg.BlockSmoothing := False;
    Jpg.Decompress(Source, Dest);

    // Possible required colorspace conversions
    case Jpg.ColorSpace of
    jcYUV:
      begin
        D := Dest.Memory;
        sdConvertYUVToRGB8bpc(D, D, Dest.Size div 3);
      end;
    jcYUVK:
      begin
        D := Dest.Memory;
        sdConvertYUVKToCMYK8bpc(D, D, Dest.Size div 4);
        // to do: convert YUVK to CMYK
//        raise Exception.Create(sUnsupportedColorspaceConv);
      end;
    end;//case
}
  finally
    Jpg.Free;
  end;
end;

initialization
  // Create global filter types list
  GPdfFilterList := TObjectList.Create;

  // Register the ones in this unit
  RegisterFilterClass('ASCIIHexDecode',  'AHx', TASCIIHexDecodeItem);
  RegisterFilterClass('ASCII85Decode',   'A85', TASCII85DecodeItem);
  RegisterFilterClass('FlateDecode',     'Fl',  TFlateDecodeItem);
  RegisterFilterClass('LZWDecode',       'LZW', TLZWDecodeItem);
  RegisterFilterClass('RunlengthDecode', 'RL',  TRunlengthDecodeItem);
  RegisterFilterClass('CCITTFaxDecode',  'CCF', TCCITTFaxDecodeItem);
  RegisterFilterClass('DCTDecode',       'DCT', TDctDecodeItem);

finalization

  FreeAndNil(GPdfFilterList);

end.
