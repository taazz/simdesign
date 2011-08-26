{ unit sdJpegTypes

  Description:
  This unit provides lowlevel types, records, classes and constants for Jpeg
  and is only dependent on Delphi units, plus Simdesign's sdSortedLists and sdDebug

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 SimDesign B.V.
  More information: www.simdesign.nl or n.haeck@simdesign.nl

  This software may ONLY be used or replicated in accordance with
  the LICENSE found in this source distribution.

}
unit sdJpegTypes;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, Contnrs, sdSortedLists, sdDebug;

type

  // allow external CMS to manage colors in the bitmap (client code needs to determine whether the
  // AMap is actually a TBitmap in Windows, in case of NativeJpg.pas)
  TsdJpegExternalCMSEvent = procedure (Sender: TObject; var AMap: TObject) of object;

  TsdJpegScale = (
    jsFull,  // Read the complete image (DC + AC 1..63)
    jsDiv2,  // Read only 1/2 of the image (DC + AC 1..15)
    jsDiv4,  // Read only 1/4 of the image (DC + AC 1..3)
    jsDiv8   // Read only 1/8 of the image (DC only)
  );

  TsdJpegColorSpace = (
    jcAutoDetect,   // Auto-detect the colorspace from the file
    jcGray,         // 1-Channel grayscale
    jcGrayA,        // 1-Channel grayscale with Alpha channel
    jcRGB,          // (standard) RGB
    jcRGBA,         // (standard) RGB with Alpha channel
    jcYCbCr,        // Jpeg Y-Cb-Cr
    jcYCbCrA,       // Jpeg Y-Cb-Cr with Alpha channel
    jcCMYK,         // CMYK
    jcYCbCrK,       // CMYK represented in 4 channels as YCbCrK
    jcYCCK,         // YCCK
    jcPhotoYCC,     // Photo YCC
    jcPhotoYCCA,    // Photo YCCA
    jcITUCieLAB     // ITU G3FAX CieLAB (for use in colour faxes)
  );

  TsdJpegDCTCodingMethod = (
    dmFast,
    dmAccurate
  );

  // Supported encoding methods in this implementation
  TsdJpegEncodingMethod = (
    emUnspecified,
    emBaselineDCT,
    emExtendedDCT,
    emProgressiveDCT
  );

  TsdQuantizationPrecision = (
    qp8bit,
    qp16bit
  );

  TsdJpegQuality = 1..100;

  TsdCoefBlock = array[0..63] of smallint;
  PsdCoefBlock = ^TsdCoefBlock;
  TsdSampleBlock = array[0..63] of byte;
  PsdSampleBlock = ^TsdSampleBlock;

  // Minimum Coded Unit block (MCU)
  TsdMCUBlock = record
    Values: PsdCoefBlock;
    PPred: Psmallint;
    DCTable: integer;
    ACTable: integer;
    BlockIdx: integer;
    MapIdx: integer;
  end;
  PsdMCUBlock = ^TsdMCUBlock;

  // Huffman code
  TsdHuffmanCode = record
    L: integer;    // Symbol length
    Code: integer; // Associated huffman code
    V: integer;    // Value for huffman code
  end;
  PsdHuffmanCode = ^TsdHuffmanCode;

  // Used to construct a histogram (frequency count) of encoded huffman symbols
  Tsd8bitHuffmanHistogram = array[0..255] of integer;
  Psd8bitHuffmanHistogram = ^Tsd8bitHuffmanHistogram;

  // Quantization table specified in DQT marker
  TsdQuantizationTable = class(TDebugPersistent)
  public
    // Quantization values Q
    FQuant: array[0..63] of word;
    // Precision P
    FPrecision: TsdQuantizationPrecision;
    // transpose
    procedure Transpose;
  end;

  TsdQuantizationTableList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdQuantizationTable;
  public
    property Items[Index: integer]: TsdQuantizationTable read GetItems; default;
  end;

  // Frame component specified in SOF marker
  TsdFrameComponent = class(TPersistent)
  public
    // Horizontal sampling factor H
    FHorzSampling: integer;
    // Vertical sampling factor V
    FVertSampling: integer;
    // Component identifier C (can be ascii)
    FComponentID: integer;
    // Quantization table destination Tq
    FQTable: integer;
  end;

  TsdFrameComponentList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdFrameComponent;
  public
    property Items[Index: integer]: TsdFrameComponent read GetItems; default;
  end;

  // Scan component specified in SOS marker
  TsdScanComponent = class(TPersistent)
  public
    // Index into frame components list, Cidx
    FComponent: integer;
    // DC entropy table destination Td
    FDCTable: integer;
    // AC entropy table destination Ta
    FACTable: integer;
    // Used as predictor in DC coding
    FPredictor: smallint;
  end;

  TsdScanComponentList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdScanComponent;
  public
    property Items[Index: integer]: TsdScanComponent read GetItems; default;
  end;

  // Holds data for one image component in the frame, provides method
  // to add/extract samples to/from the MCU currently being decoded/encoded
  TsdJpegBlockMap = class(TDebugPersistent)
  private
    FCoef: array of smallint;
    FCoefBackup: array of smallint; // used when adjusting brightness/contrast
    FSample: array of byte;
    FFrame: TsdFrameComponent; // Pointer to frame info
    FHorzBlockCount: integer; // Horizontal block count
    FVertBlockCount: integer; // Vertical block count
    FBlockStride: integer; // number of samples per block
    FScanStride: integer; // width of a scanline
  protected
    procedure CreateMap; virtual;
  public
    procedure SetSize(AHorzMcuCount, AVertMcuCount: integer;
      AFrame: TsdFrameComponent; ABlockStride: integer);
    procedure Resize(AHorzBlockCount, AVertBlockCount: integer);
    procedure ReduceBlockSize(ANewSize: integer);
    // Number of blocks in the MCU belonging to this image
    function McuBlockCount(AScanCount: integer): integer;
    // Total number of blocks in image (size / 8x8)
    function TotalBlockCount: integer;
    function GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
    function GetCoefPointer(BlockX, BlockY: integer): pointer;
    function GetSamplePointer(BlockX, BlockY: integer): pointer;
    function FirstCoef: pointer;
    function FirstCoefBackup: pointer;
    function HasCoefBackup: boolean;
    procedure MakeCoefBackup;
    procedure ClearCoefBackup;
    procedure SaveRawValues(const AFileName: string);
    property HorzBlockCount: integer read FHorzBlockCount;
    property VertBlockCount: integer read FVertBlockCount;
    property BlockStride: integer read FBlockStride;
    property ScanStride: integer read FScanStride;
    property Frame: TsdFrameComponent read FFrame;
  end;

  TsdBlockMapList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdJpegBlockMap;
  public
    property Items[Index: integer]: TsdJpegBlockMap read GetItems; default;
  end;

  TsdJpegTile = class
  public
    FMcuIndex: integer;
    FStreamPos: int64;
    FBits: cardinal;
    FBitsLeft: integer;
    FPredictors: array of smallint;
  end;

  TsdJpegTileList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdJpegTile;
  public
    function IndexByMcuIndex(AMcuIndex: integer): integer;
    property Items[Index: integer]: TsdJpegTile read GetItems; default;
  end;

  // Collected component data from markers
  TsdJpegInfo = class(TPersistent)
  public
    // Repository of tables, are updated after DQT or DHT markers
    FDCHuffmanTables: TObjectList{TsdHuffmanTableList};
    FACHuffmanTables: TObjectList{TsdHuffmanTableList};
    FQuantizationTables: TsdQuantizationTableList;
    // List of frames
    FFrames: TsdFrameComponentList;
    // List of scans
    FScans: TsdScanComponentList;
    // Number of image components in frame, Nf
    FFrameCount: integer;
    // Number of image components in scan, Ns
    FScanCount: integer;
    // Maximum of all H_i in current scan, Hmax
    FHorzSamplingMax: integer;
    // Maximum of all V_i in current scan, Vmax
    FVertSamplingMax: integer;
    // Restart interval MCU count (0 means disabled), updated after RST marker, Ri
    FRestartInterval: integer;
    // Image width, X
    FWidth: integer;
    // Image Height, Y
    FHeight: integer;
    // Jpeg encoding method
    FEncodingMethod: TsdJpegEncodingMethod;
    // Sample precision in bits for samples, P;
    FSamplePrecision: integer;
    // Start of spectral selection, Ss
    FSpectralStart: integer;
    // End of spectral selection, Se
    FSpectralEnd: integer;
    // Succ Approximation high bitpos, Ah
    FApproxHigh: integer;
    // Succ Approximation low bitpos, Al
    FApproxLow: integer;
    // Width of the MCU block in pixels
    FMcuWidth: integer;
    // Height of the MCU block in pixels
    FMcuHeight: integer;
    // Horizontal MCU count
    FHorzMcuCount: integer;
    // Vertical MCU count
    FVertMcuCount: integer;
    //
    FWaitForDNL: boolean;
    // Width of a tile in pixels during TileMode
    FTileWidth: integer;
    // Height of a tile in pixels during TileMode
    FTileHeight: integer;
    //
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
  end;

const
  cColorSpaceNames: array[TsdJpegColorSpace] of AnsiString =
  ('AutoDetect', 'Gray', 'GrayA', 'RGB', 'RGBA', 'YCbCr', 'YCbCrA',
   'CMYK', 'CMYK as YCbCrK', 'YCCK', 'PhotoYCC', 'PhotoYCCA', 'ITU CieLAB');

   cDefaultJpgCompressionQuality = 80;

type

  TsdZigZagArray = array[0..63 + 16] of byte;
  PsdZigZagArray = ^TsdZigZagArray;
  TsdIntArray64 = array[0..63] of integer;


const
  // This matrix maps zigzag position to the left/right
  // top/down normal position inside the 8x8 block.

  cJpegInverseZigZag1x1: TsdZigZagArray =
    ( 0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag2x2: TsdZigZagArray =
    ( 0,  1,  2,  0,  3,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag4x4: TsdZigZagArray =
    ( 0,  1,  4,  8,  5,  2,  3,  6,
      9, 12,  0, 13, 10,  7,  0,  0,
      0, 11, 14,  0,  0,  0,  0,  0,
     15,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag8x8: TsdZigZagArray =
    ( 0,  1,  8, 16,  9,  2,  3, 10,
     17, 24, 32, 25, 18, 11,  4,  5,
     12, 19, 26, 33, 40, 48, 41, 34,
     27, 20, 13,  6,  7, 14, 21, 28,
     35, 42, 49, 56, 57, 50, 43, 36,
     29, 22, 15, 23, 30, 37, 44, 51,
     58, 59, 52, 45, 38, 31, 39, 46,
     53, 60, 61, 54, 47, 55, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegForwardZigZag8x8: TsdZigZagArray =
    ( 0,  1,  5,  6, 14, 15, 27, 28,
      2,  4,  7, 13, 16, 26, 29, 42,
      3,  8, 12, 17, 25, 30, 41, 43,
      9, 11, 18, 24, 31, 40, 44, 53,
     10, 19, 23, 32, 39, 45, 52, 54,
     20, 22, 33, 38, 46, 51, 55, 60,
     21, 34, 37, 47, 50, 56, 59, 61,
     35, 36, 48, 49, 57, 58, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegNaturalZigZag8x8: TsdZigZagArray =
    ( 0,  1,  2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 20, 21, 22, 23,
     24, 25, 26, 27, 28, 29, 30, 31,
     32, 33, 34, 35, 36, 37, 38, 39,
     40, 41, 42, 43, 44, 45, 46, 47,
     48, 49, 50, 51, 52, 53, 54, 55,
     56, 57, 58, 59, 60, 61, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  // entry n equals 1 shl (n-1)
  cExtendTest: array[0..15] of integer =
    ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
     $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

  // entry n equals (-1 shl n) + 1
  cExtendOffset: array[0..15] of integer =
   (0, ((-1) shl 1 ) + 1, ((-1) shl 2 ) + 1, ((-1) shl 3 ) + 1, ((-1) shl 4 ) + 1,
       ((-1) shl 5 ) + 1, ((-1) shl 6 ) + 1, ((-1) shl 7 ) + 1, ((-1) shl 8 ) + 1,
       ((-1) shl 9 ) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
       ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

  // These are the sample quantization tables given in JPEG spec section K.1.
  // The spec says that the values given produce "good" quality, and
  // when divided by 2, "very good" quality.

  cStdLuminanceQuantTbl: TsdIntArray64 =
   (16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99);

  cStdChrominanceQuantTbl: TsdIntArray64 =
   (17,  18,  24,  47,  99,  99,  99,  99,
    18,  21,  26,  66,  99,  99,  99,  99,
    24,  26,  56,  99,  99,  99,  99,  99,
    47,  66,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99);

  // These are standard Huffman tables for general use

  cHuffmanBitsDcLum: array[0..15] of byte =
    (0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  cHuffmanValDCLum: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  cHuffmanBitsDCChrom: array[0..15] of byte =
    (0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0);
  cHuffmanValDCChrom: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

  const cHuffmanBitsACLum: array[0..15] of byte =
    (0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7d);
  const cHuffmanValACLum: array[0..161] of byte =
    ( $01, $02, $03, $00, $04, $11, $05, $12,
      $21, $31, $41, $06, $13, $51, $61, $07,
      $22, $71, $14, $32, $81, $91, $a1, $08,
      $23, $42, $b1, $c1, $15, $52, $d1, $f0,
      $24, $33, $62, $72, $82, $09, $0a, $16,
      $17, $18, $19, $1a, $25, $26, $27, $28,
      $29, $2a, $34, $35, $36, $37, $38, $39,
      $3a, $43, $44, $45, $46, $47, $48, $49,
      $4a, $53, $54, $55, $56, $57, $58, $59,
      $5a, $63, $64, $65, $66, $67, $68, $69,
      $6a, $73, $74, $75, $76, $77, $78, $79,
      $7a, $83, $84, $85, $86, $87, $88, $89,
      $8a, $92, $93, $94, $95, $96, $97, $98,
      $99, $9a, $a2, $a3, $a4, $a5, $a6, $a7,
      $a8, $a9, $aa, $b2, $b3, $b4, $b5, $b6,
      $b7, $b8, $b9, $ba, $c2, $c3, $c4, $c5,
      $c6, $c7, $c8, $c9, $ca, $d2, $d3, $d4,
      $d5, $d6, $d7, $d8, $d9, $da, $e1, $e2,
      $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
      $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  cHuffmanBitsACChrom: array[0..15] of byte =
    (0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77);
  cHuffmanValACChrom: array[0..161] of byte =
    ( $00, $01, $02, $03, $11, $04, $05, $21,
      $31, $06, $12, $41, $51, $07, $61, $71,
      $13, $22, $32, $81, $08, $14, $42, $91,
      $a1, $b1, $c1, $09, $23, $33, $52, $f0,
      $15, $62, $72, $d1, $0a, $16, $24, $34,
      $e1, $25, $f1, $17, $18, $19, $1a, $26,
      $27, $28, $29, $2a, $35, $36, $37, $38,
      $39, $3a, $43, $44, $45, $46, $47, $48,
      $49, $4a, $53, $54, $55, $56, $57, $58,
      $59, $5a, $63, $64, $65, $66, $67, $68,
      $69, $6a, $73, $74, $75, $76, $77, $78,
      $79, $7a, $82, $83, $84, $85, $86, $87,
      $88, $89, $8a, $92, $93, $94, $95, $96,
      $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
      $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
      $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
      $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
      $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
      $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
      $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

const
  // Motion Jpeg DHT segment
  cMjpgDHTSeg: packed array[0..415] of byte = (
    $00, $00, $01, $05, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00, $00,
    $00, $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $01,
    $00, $03, $01, $01, $01, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00,

    $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $10, $00,
    $02, $01, $03, $03, $02, $04, $03, $05, $05, $04, $04, $00, $00, $01, $7D,
    $01, $02, $03, $00, $04, $11, $05, $12, $21, $31, $41, $06, $13, $51, $61,
    $07, $22, $71, $14, $32, $81, $91, $A1, $08, $23, $42, $B1, $C1, $15, $52,
    $D1, $F0, $24, $33, $62, $72, $82, $09, $0A, $16, $17, $18, $19, $1A, $25,
    $26, $27, $28, $29, $2A, $34, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,

    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $83,
    $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98, $99,
    $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2, $D3,
    $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, $11, $00, $02,
    $01, $02, $04, $04, $03, $04, $07, $05, $04, $04, $00, $01, $02, $77, $00,

    $01, $02, $03, $11, $04, $05, $21, $31, $06, $12, $41, $51, $07, $61, $71,
    $13, $22, $32, $81, $08, $14, $42, $91, $A1, $B1, $C1, $09, $23, $33, $52,
    $F0, $15, $62, $72, $D1, $0A, $16, $24, $34, $E1, $25, $F1, $17, $18, $19,
    $1A, $26, $27, $28, $29, $2A, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,
    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $82,
    $83, $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98,

    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5,
    $B6, $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2,
    $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA);

resourcestring

  sInternalError                   = 'Internal error';
  sUnsupportedEncoding             = 'Unsupported encoding: SOF%d';
  sMarkerExpected                  = 'Jpeg Marker expected';
  sUnsupportedBitsPerSample        = 'Unsupported bits per sample';
  sInvalidTableClass               = 'Invalid table class in DHT marker';
  sInputStreamChopped              = 'Input stream prematurely chopped';
  sUnexpectedMarkerInEncodedStream = 'Unexpected marker in encoded stream';
  sInvalidFrameRef                 = 'Invalid frame reference in scan component';
  sNoColorTransformation           = 'No color transformation available for current settings';
  sNoDCTCoefficentsAvailable       = 'No DCT coefficients available (compress first)';
  sOperationOnlyFor8x8             = 'Operation can only be performed with LoadScale = jsFull';
  sBitmapIsEmptyCannotSave         = 'Bitmap is empty; cannot save';
  sInvalidFormatForSelectedCS      = 'Invalid bitmap format for selected color space';
  sCommentCannotBeSet              = 'Comment cannot be set before assigning bitmap';
  sDNLMarkerExpected               = 'DNL marker expected';
  sUnsupportedColorSpace           = 'Unsupported color space';
  sOnProvideStripMustBeAssigned    = 'OnProvideStrip must be assigned';
  sOnCreateMapMustBeAssigned       = 'OnCreateMap must be assigned';
  sCannotUseTileMode               = 'Cannot use tilemode with progressive jpeg';
  sRangeErrorInTileLoading         = 'Range error in tiled loading: make sure to select tilemode';

implementation

{ TsdQuantizationTable }

procedure TsdQuantizationTable.Transpose;
var
  x, y, i, j: integer;
  Temp: word;
begin
  // transpose indices in table, but we must do this with the forward zigzag
  for y := 0 to 6 do
    for x := y + 1 to 7 do
    begin
      i := cJpegForwardZigZag8x8[x + y * 8];
      j := cJpegForwardZigZag8x8[x * 8 + y];
      Temp := FQuant[i];
      FQuant[i] := FQuant[j];
      FQuant[j] := Temp;
    end;
end;

{ TsdQuantizationTableList }

function TsdQuantizationTableList.GetItems(Index: integer): TsdQuantizationTable;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdQuantizationTable.Create;
    Put(Index, Result);
  end;
end;

{ TsdFrameComponentList }

function TsdFrameComponentList.GetItems(Index: integer): TsdFrameComponent;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdFrameComponent.Create;
    Put(Index, Result);
  end;
end;

{ TsdScanComponentList }

function TsdScanComponentList.GetItems(Index: integer): TsdScanComponent;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdScanComponent.Create;
    Put(Index, Result);
  end;
end;

{ TsdJpegBlockMap }

procedure TsdJpegBlockMap.ClearCoefBackup;
begin
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.CreateMap;
var
  Count: integer;
begin
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  // Clear the coefficients (since the decoder doesn't always reset them to 0)
  if Count > 0 then
    FillChar(FCoef[0], Count * SizeOf(smallint), 0);
  // Clear backup
  ClearCoefBackup;  
end;

function TsdJpegBlockMap.FirstCoef: pointer;
begin
  Result := @FCoef[0];
end;

function TsdJpegBlockMap.FirstCoefBackup: pointer;
begin
  Result := @FCoefBackup[0];
end;

function TsdJpegBlockMap.GetCoefPointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FCoef[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
var
  X, Y: integer;
begin
  X := FFrame.FHorzSampling * AMcuX;
  Y := FFrame.FVertSampling * AMcuY;
  while AMcuIdx >= FFrame.FHorzSampling do
  begin
    inc(Y);
    dec(AMcuIdx, FFrame.FHorzSampling);
  end;
  inc(X, AMcuIdx);
  Result := @FCoef[X * FBlockStride + Y * FScanStride];
end;

function TsdJpegBlockMap.GetSamplePointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FSample[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.HasCoefBackup: boolean;
begin
  Result := length(FCoefBackup) > 0;
end;

procedure TsdJpegBlockMap.MakeCoefBackup;
var
  Count: integer;
begin
  Count := length(FCoef);
  if Count <= 0 then exit;
  SetLength(FCoefBackup, Count);
  Move(FCoef[0], FCoefBackup[0], Count * SizeOf(smallint));
end;

function TsdJpegBlockMap.McuBlockCount(AScanCount: integer): integer;
begin
  if AScanCount = 1 then
    Result := 1
  else
    Result := FFrame.FHorzSampling * FFrame.FVertSampling;
end;

procedure TsdJpegBlockMap.ReduceBlockSize(ANewSize: integer);
var
  i, j, Count, Stride: integer;
  Sc, Dc: Psmallint;
  Ss, Ds: Pbyte;
begin
  if FBlockstride <> 64 then exit;

  Count := FHorzBlockCount * FVertBlockCount;
  // coefs
  Sc := @FCoef[0]; Dc := Sc;
  Stride := ANewSize * SizeOf(smallint);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Sc^, Dc^, Stride);
        inc(Dc, ANewSize);
      end;
      inc(Sc, 8);
    end;
  end;
  // samples
  Ss := @FSample[0]; Ds := Ss;
  Stride := ANewSize * SizeOf(byte);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Ss^, Ds^, Stride);
        inc(Ds, ANewSize);
      end;
      inc(Ss, 8);
    end;
  end;
  FBlockStride := ANewSize * ANewSize;
  Resize(FHorzBlockCount, FVertBlockCount);
end;

procedure TsdJpegBlockMap.Resize(AHorzBlockCount, AVertBlockCount: integer);
var
  Count: integer;
begin
  FHorzBlockCount := AHorzBlockCount;
  FVertBlockCount := AVertBlockCount;
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.SaveRawValues(const AFileName: string);
var
  i, x, y: integer;
  F: TFileStream;
  Block: PsdCoefBlock;
  procedure WriteS(const S: Utf8String);
  begin
    F.Write(S[1], length(S));
  end;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    for y := 0 to FVertBlockCount - 1 do
    begin
      WriteS(Format('Line %d:', [y]) + #13#10);
      for x := 0 to FHorzBlockCount - 1 do
      begin
        WriteS(Format(' Block %d:', [x]) + #13#10);
        WriteS(' ');
        Block := GetCoefPointer(x, y);
        for i := 0 to 63 do
          WriteS(IntToStr(Block[i]) + ' ');
        WriteS(#13#10);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TsdJpegBlockMap.SetSize(AHorzMcuCount, AVertMcuCount: integer;
  AFrame: TsdFrameComponent; ABlockStride: integer);
begin
  FFrame := AFrame;
  FBlockStride := ABlockStride;
  // Determine block dimensions
  FHorzBlockCount := AHorzMcuCount * FFrame.FHorzSampling;
  FVertBlockCount := AVertMcuCount * FFrame.FVertSampling;
  // Assume the data is valid, we can create the map
  CreateMap;
end;

function TsdJpegBlockMap.TotalBlockCount: integer;
begin
  Result := FHorzBlockCount * FVertBlockCount;
end;

{ TsdBlockMapList }

function TsdBlockMapList.GetItems(Index: integer): TsdJpegBlockMap;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdJpegBlockMap.Create;
    Put(Index, Result);
  end;
end;

{ TsdJpegTileList }

function TsdJpegTileList.GetItems(Index: integer): TsdJpegTile;
begin
  Result := Get(Index);
end;

function TsdJpegTileList.IndexByMcuIndex(AMcuIndex: integer): integer;
var
  Min, Max: integer;
begin
  // Find position for insert - binary method
  Min := 0;
  Max := Count;
  while Min < Max do begin
    Result := (Min + Max) div 2;
    case CompareInteger(Items[Result].FMcuIndex, AMcuIndex) of
    -1: Min := Result + 1;
     0: exit;
     1: Max := Result;
    end;
  end;
  Result := Min;
end;

{ TsdJpegInfo }

procedure TsdJpegInfo.Clear;
begin
  // Clear all data in Info
  FDCHuffmanTables.Clear;
  FACHuffmanTables.Clear;
  FQuantizationTables.Clear;
  FFrames.Clear;
  FScans.Clear;

  FFrameCount := 0;
  FScanCount := 0;
  FHorzSamplingMax := 0;
  FVertSamplingMax := 0;
  FRestartInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FEncodingMethod := emUnspecified;
  FSamplePrecision := 0;
  FSpectralStart := 0;
  FSpectralEnd := 0;
  FApproxHigh := 0;
  FApproxLow := 0;
  FWaitForDNL := False;
end;

constructor TsdJpegInfo.Create;
begin
  inherited Create;
  FDCHuffmanTables := TObjectList.Create;
  FACHuffmanTables := TObjectList.Create;
  FQuantizationTables := TsdQuantizationTableList.Create;
  FFrames := TsdFrameComponentList.Create;
  FScans := TsdScanComponentList.Create;
end;

destructor TsdJpegInfo.Destroy;
begin
  FreeAndNil(FDCHuffmanTables);
  FreeAndNil(FACHuffmanTables);
  FreeAndNil(FQuantizationTables);
  FreeAndNil(FFrames);
  FreeAndNil(FScans);
  inherited;
end;

end.

