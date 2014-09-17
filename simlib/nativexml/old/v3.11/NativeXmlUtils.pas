{ unit NativeXmlUtils

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit NativeXmlUtils;

interface

{$i NativeXml.inc}

uses
  Windows, Classes, Contnrs, SysUtils, sdStringTable;

const

  // Count of different escape characters
  cEscapeCount = 5;

  // These are characters that must be escaped. Note that "&" is first since
  // when another would be replaced first (eg ">" by "&lt;") this could
  // cause the new "&" in "&lt;" to be replaced by "&amp;";
  cXmlEscapePhrases: array[0..cEscapeCount - 1] of AnsiString =
    ('&',
     '<',
     '>',
     '''',
     '"');

  // These are the strings that replace the escape strings - in the same order
  cXmlReplacePhrases: array[0..cEscapeCount - 1] of AnsiString =
    ('&amp;',
     '&lt;',
     '&gt;',
     '&apos;',
     '&quot;');

  // special characters used for whitespace / blanks
  cXmlBlankChars: set of AnsiChar =
    [#9, #10, #13, #32];

  cXmlBlankCharsOrEndTag: set of AnsiChar =
    [#9, #10, #13, #32, '[', '/', '>'];

  // Windows-1252 codepage, used for GUI implementations
  CP_1252: integer = 1252;

  // UTF8 codepage
  CP_UTF8: integer = 65001;

resourcestring

  sPrematureEnd        = 'stream terminated prematurely at %d';
  sInvalidStream       = 'invalid stream';
  sUnsupportedEncoding = 'Unsupported encoding in stream';
  sNotSupported        = 'Feature is not supported yet';
  sIllegalTag          = 'Illegal tag at %d';
  sUnsupportedTag      = 'Unsupported tag at %d';
  sIllegalEndTag       = 'Illegal end tag at %d';
  sQuoteCharExpected   = 'Quote char expected at %d';
  sCannotAddNode       = 'Cannot add node to this type of element';
  sCannotSetName       = 'Cannot set name on this type of element';
  sCannotSetValue      = 'Cannot set value on this type of element';
  sCannotManipulate    = 'Cannot manipulate nodes in this type of element';
  sBeginEndMismatch    = 'Begin and end tag mismatch: %s %s at %d';
  sRootElementNotDefined = 'XML root element not defined.';

type

  // Definition of different methods of string encoding.
  TsdStringEncoding = (
    seUnknown,   // encoding is not yet known (eg before BOM)
    seAnsi,      // Ansi encoding, e.g. "Windows-1252" or other codepage (1 byte per character)
    seUTF8,      // UTF-8 (1, 2, 3 or 4 bytes per character)
    seUTF16BE,   // UTF-16 Big Endian (2 or 4 bytes per character)
    seUTF16LE,   // UTF-16 Little Endian (2 or 4  bytes per character)
    seUCS4BE,    // UCS-4 Big Endian (4 bytes per character)
    seUCS4LE,    // UCS-4 Little Endian (4 bytes per character)
    seUCS4_2143, // UCS-4 unusual octet order - 2143 (4 bytes per character)
    seUCS4_3412, // UCS-4 unusual octet order - 3412 (4 bytes per character)
    seEBCDIC     // Extended Binary Coded Decimal Interchange Code (1 byte per character)
  );

type

  // record with info from a Byte order Mark (BOM)
  TBomInfo = packed record
    BOM: array[0..3] of byte;    // 4 bytes possibly containing the BOM
    Len: integer;                // byte length of the BOM
    Encoding: TsdStringEncoding; // which string encodig does the file have?
    HasBOM: boolean;             // does a file have a BOM?
  end;

const

  cBomInfoListCount = 15;
  // array with Byte Order Mark (BOM) info
  cBomInfoList: array[0..cBomInfoListCount - 1] of TBomInfo =
  ( (BOM: ($3C,$3F,$78,$6D); Len: 4; Encoding: seAnsi;      HasBOM: false),
    (BOM: ($EF,$BB,$BF,$00); Len: 3; Encoding: seUTF8;      HasBOM: true),
    (BOM: ($00,$00,$FE,$FF); Len: 4; Encoding: seUCS4BE;    HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 4; Encoding: seUCS4LE;    HasBOM: true),
    (BOM: ($00,$00,$FF,$FE); Len: 4; Encoding: seUCS4_2143; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 2; Encoding: seUTF16BE;   HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 2; Encoding: seUTF16LE;   HasBOM: true),
    (BOM: ($00,$00,$00,$3C); Len: 4; Encoding: seUCS4BE;    HasBOM: false),
    (BOM: ($3C,$00,$00,$00); Len: 4; Encoding: seUCS4LE;    HasBOM: false),
    (BOM: ($00,$00,$3C,$00); Len: 4; Encoding: seUCS4_2143; HasBOM: false),
    (BOM: ($00,$3C,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: false),
    (BOM: ($00,$3C,$00,$3F); Len: 4; Encoding: seUTF16BE;   HasBOM: false),
    (BOM: ($3C,$00,$3F,$00); Len: 4; Encoding: seUTF16LE;   HasBOM: false),
    (BOM: ($4C,$6F,$A7,$94); Len: 4; Encoding: seEBCDIC;    HasBOM: false)
  );

type

  // TsdElementType enumerates the different kinds of elements that can be found
  // in the XML document.
  TsdElementType = (
    xeElement,     // Normal element <name {attr}>[value][sub-elements]</name>
    xeAttribute,   // Attribute ( name='value' or name="value")
    xeComment,     // Comment <!--{comment}-->
    xeCData,       // literal data <![CDATA[{data}]]>
    xeDeclaration, // XML declaration <?xml{declaration}?>
    xeStylesheet,  // Stylesheet <?xml-stylesheet{stylesheet}?>
    xeDocType,     // DOCTYPE DTD declaration <!DOCTYPE{spec}>
    xeDtdElement,  // <!ELEMENT >
    xeDtdAttList,  // <!ATTLIST >
    xeDtdEntity,   // <!ENTITY >
    xeDtdNotation, // <!NOTATION >
    xeInstruction, // <?...?> processing instruction
    xeCharData,    // Character data in a node
    xeQuotedText,  // "bla" or 'bla'
    xeUnknown,     // Any <data>
    xeEndTag,      // </...>
    xeError
  );

  TsdElementTypes = set of TsdElementType;

{ Utility functions }

// Convert WideString to Utf8String
function sdWideToUTF8(const W: WideString): Utf8String;

// Convert UTF8 string to WideString
function sdUTF8ToWide(const S: Utf8String): WideString;

// Convert Ansi to UTF8 string
function sdAnsiToUTF8(const S: AnsiString; ACodePage: integer): Utf8String;

// Convert UTF8 to Ansi string
function sdUTF8ToAnsi(const S: Utf8String; ACodePage: integer): AnsiString;

// Convert a "WideString" (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
function sdWideToUTF8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;

// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
function sdUtf8ToWideMem(Src: Pbyte; Dst: Pword; Count: integer): integer;

function sdUnEscapeString(const AValue: Utf8String): Utf8String;
function sdEscapeString(const AValue: Utf8String): Utf8String;

procedure sdWriteToStream(S: TStream; const Value: Utf8String);

// Find the codepage based on the encoding string. If no encoding string is
// matched, the function returns a codepage of -1
function sdEncodingToCodePage(const AEncoding: Utf8String): integer;

function Utf8CompareText(const S1, S2: Utf8String): integer;

// type conversions

// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
function sdDateTimeToString(Value: TDateTime): Utf8String;

function sdBoolToString(Value: boolean): Utf8String;

function sdIntToString(Value: integer): Utf8String;

type

  // TFastMemStream copied from sdHugeStreams.pas
  TFastMemStream = class(TStream)
  private
    FMemory: Pointer;
    FPosition: longint;
    FCapacity: longint;
    FSize: longint;
  protected
    procedure SetCapacity(Value: longint);
    procedure SetSize(NewSize: Longint); override;
  public
    destructor Destroy; override;
    procedure Clear;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Memory: Pointer read FMemory;
    property Size: longint read FSize write SetSize;
  end;

  // Delphi's implementation of TStringStream is severely flawed, it does a SetLength
  // on each write, which slows down everything to a crawl. This implementation over-
  // comes this issue.
  TsdUTF8StringStream = class(TMemoryStream)
  public
    constructor Create(const S: Utf8String);
    function DataString: Utf8String;
  end;

implementation

type

  TCodepageInfo = packed record
    Name: Utf8String;
    Codepage: integer;
  end;

const

  // Codepages defined in Windows
  cCodepageInfoCount = 143;
  cCodePageInfo: array[0..cCodepageInfoCount - 1] of TCodepageInfo =
  ( (Name: 'IBM037';                  Codepage:    37), //1
    (Name: 'IBM437';                  Codepage:   437),
    (Name: 'IBM500';                  Codepage:   500),
    (Name: 'ASMO-708';                Codepage:   708),
    (Name: 'ASMO-449+';               Codepage:   709), //5
    (Name: 'BCON V4';                 Codepage:   709),
    (Name: 'Arabic';                  Codepage:   710),
    (Name: 'DOS-720';                 Codepage:   720),
    (Name: 'ibm737';                  Codepage:   737),
    (Name: 'ibm775';                  Codepage:   775), //10
    (Name: 'ibm850';                  Codepage:   850),
    (Name: 'ibm852';                  Codepage:   852),
    (Name: 'IBM855';                  Codepage:   855),
    (Name: 'ibm857';                  Codepage:   857),
    (Name: 'IBM00858';                Codepage:   858),
    (Name: 'IBM860';                  Codepage:   860),
    (Name: 'ibm861';                  Codepage:   861),
    (Name: 'DOS-862';                 Codepage:   862),
    (Name: 'IBM863';                  Codepage:   863),
    (Name: 'IBM864';                  Codepage:   864), //20
    (Name: 'IBM865';                  Codepage:   865),
    (Name: 'cp866';                   Codepage:   866),
    (Name: 'ibm869';                  Codepage:   869),
    (Name: 'IBM870';                  Codepage:   870),
    (Name: 'windows-874';             Codepage:   874),
    (Name: 'cp875';                   Codepage:   875),
    (Name: 'shift_jis';               Codepage:   932),
    (Name: 'gb2312';                  Codepage:   936),
    (Name: 'ks_c_5601-1987';          Codepage:   949),
    (Name: 'big5';                    Codepage:   950),    //30
    (Name: 'IBM1026';                 Codepage:  1026),
    (Name: 'IBM01047';                Codepage:  1047),
    (Name: 'IBM01140';                Codepage:  1140),
    (Name: 'IBM01141';                Codepage:  1141),
    (Name: 'IBM01142';                Codepage:  1142),
    (Name: 'IBM01143';                Codepage:  1143),
    (Name: 'IBM01144';                Codepage:  1144),
    (Name: 'IBM01145';                Codepage:  1145),
    (Name: 'IBM01146';                Codepage:  1146),
    (Name: 'IBM01147';                Codepage:  1147),   //40
    (Name: 'IBM01148';                Codepage:  1148),
    (Name: 'IBM01149';                Codepage:  1149),
    (Name: 'utf-16';                  Codepage:  1200),
    (Name: 'unicodeFFFE';             Codepage:  1201),
    (Name: 'windows-1250';            Codepage:  1250),
    (Name: 'windows-1251';            Codepage:  1251),
    (Name: 'windows-1252';            Codepage:  1252),
    (Name: 'windows-1253';            Codepage:  1253),
    (Name: 'windows-1254';            Codepage:  1254),
    (Name: 'windows-1255';            Codepage:  1255),    //50
    (Name: 'windows-1256';            Codepage:  1256),
    (Name: 'windows-1257';            Codepage:  1257),
    (Name: 'windows-1258';            Codepage:  1258),
    (Name: 'Johab';                   Codepage:  1361),
    (Name: 'macintosh';               Codepage: 10000),
    (Name: 'x-mac-japanese';          Codepage: 10001),
    (Name: 'x-mac-chinesetrad';       Codepage: 10002),
    (Name: 'x-mac-korean';            Codepage: 10003),
    (Name: 'x-mac-arabic';            Codepage: 10004),
    (Name: 'x-mac-hebrew';            Codepage: 10005),  //60
    (Name: 'x-mac-greek';             Codepage: 10006),
    (Name: 'x-mac-cyrillic';          Codepage: 10007),
    (Name: 'x-mac-chinesesimp';       Codepage: 10008),
    (Name: 'x-mac-romanian';          Codepage: 10010),
    (Name: 'x-mac-ukrainian';         Codepage: 10017),
    (Name: 'x-mac-thai';              Codepage: 10021),
    (Name: 'x-mac-ce';                Codepage: 10029),
    (Name: 'x-mac-icelandic';         Codepage: 10079),
    (Name: 'x-mac-turkish';           Codepage: 10081),
    (Name: 'x-mac-croatian';          Codepage: 10082),   //70
    (Name: 'utf-32';                  Codepage: 12000),
    (Name: 'utf-32BE';                Codepage: 12001),
    (Name: 'x-Chinese_CNS';           Codepage: 20000),
    (Name: 'x-cp20001';               Codepage: 20001),
    (Name: 'x_Chinese-Eten';          Codepage: 20002),
    (Name: 'x-cp20003';               Codepage: 20003),
    (Name: 'x-cp20004';               Codepage: 20004),
    (Name: 'x-cp20005';               Codepage: 20005),
    (Name: 'x-IA5';                   Codepage: 20105),
    (Name: 'x-IA5-German';            Codepage: 20106),  //80
    (Name: 'x-IA5-Swedish';           Codepage: 20107),
    (Name: 'x-IA5-Norwegian';         Codepage: 20108),
    (Name: 'us-ascii';                Codepage: 20127),
    (Name: 'x-cp20261';               Codepage: 20261),
    (Name: 'x-cp20269';               Codepage: 20269),
    (Name: 'IBM273';                  Codepage: 20273),
    (Name: 'IBM277';                  Codepage: 20277),
    (Name: 'IBM278';                  Codepage: 20278),
    (Name: 'IBM280';                  Codepage: 20280),
    (Name: 'IBM284';                  Codepage: 20284),  //90
    (Name: 'IBM285';                  Codepage: 20285),
    (Name: 'IBM290';                  Codepage: 20290),
    (Name: 'IBM297';                  Codepage: 20297),
    (Name: 'IBM420';                  Codepage: 20420),
    (Name: 'IBM423';                  Codepage: 20423),
    (Name: 'IBM424';                  Codepage: 20424),
    (Name: 'x-EBCDIC-KoreanExtended'; Codepage: 20833),
    (Name: 'IBM-Thai';                Codepage: 20838),
    (Name: 'koi8-r';                  Codepage: 20866),
    (Name: 'IBM871';                  Codepage: 20871), //100
    (Name: 'IBM880';                  Codepage: 20880),
    (Name: 'IBM905';                  Codepage: 20905),
    (Name: 'IBM00924';                Codepage: 20924),
    (Name: 'EUC-JP';                  Codepage: 20932),
    (Name: 'x-cp20936';               Codepage: 20936),
    (Name: 'x-cp20949';               Codepage: 20949),
    (Name: 'cp1025';                  Codepage: 21025),
    (Name: 'koi8-u';                  Codepage: 21866),
    (Name: 'iso-8859-1';              Codepage: 28591),
    (Name: 'iso-8859-2';              Codepage: 28592),  //110
    (Name: 'iso-8859-3';              Codepage: 28593),
    (Name: 'iso-8859-4';              Codepage: 28594),
    (Name: 'iso-8859-5';              Codepage: 28595),
    (Name: 'iso-8859-6';              Codepage: 28596),
    (Name: 'iso-8859-7';              Codepage: 28597),
    (Name: 'iso-8859-8';              Codepage: 28598),
    (Name: 'iso-8859-9';              Codepage: 28599),
    (Name: 'iso-8859-13';             Codepage: 28603),
    (Name: 'iso-8859-15';             Codepage: 28605),
    (Name: 'x-Europa';                Codepage: 29001),   //120
    (Name: 'iso-8859-8-i';            Codepage: 38598),
    (Name: 'iso-2022-jp';             Codepage: 50220),
    (Name: 'csISO2022JP';             Codepage: 50221),
    (Name: 'iso-2022-jp';             Codepage: 50222),
    (Name: 'iso-2022-kr';             Codepage: 50225),
    (Name: 'x-cp50227';               Codepage: 50227),
    (Name: 'euc-jp';                  Codepage: 51932),
    (Name: 'EUC-CN';                  Codepage: 51936),
    (Name: 'euc-kr';                  Codepage: 51949),
    (Name: 'hz-gb-2312';              Codepage: 52936),   //130
    (Name: 'GB18030';                 Codepage: 54936),
    (Name: 'x-iscii-de';              Codepage: 57002),
    (Name: 'x-iscii-be';              Codepage: 57003),
    (Name: 'x-iscii-ta';              Codepage: 57004),
    (Name: 'x-iscii-te';              Codepage: 57005),
    (Name: 'x-iscii-as';              Codepage: 57006),
    (Name: 'x-iscii-or';              Codepage: 57007),
    (Name: 'x-iscii-ka';              Codepage: 57008),
    (Name: 'x-iscii-ma';              Codepage: 57009),
    (Name: 'x-iscii-gu';              Codepage: 57010),  //140
    (Name: 'x-iscii-pa';              Codepage: 57011),
    (Name: 'utf-7';                   Codepage: 65000),
    (Name: 'utf-8';                   Codepage: 65001)); //143


{ Utility Functions }

function sdWideToUtf8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
var
  W: word;
  DStart: Pbyte;
begin
  DStart := Dst;
  while Count > 0 do
  begin
    W := Src^;
    inc(Src);
    if W <= $7F then
    begin
      Dst^ := byte(W);
      inc(Dst);
    end else
    begin
      if W > $7FF then
      begin
        Dst^ := byte($E0 or (W shr 12));
        inc(Dst);
        Dst^ := byte($80 or ((W shr 6) and $3F));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end else
      begin //  $7F < W <= $7FF
        Dst^ := byte($C0 or (W shr 6));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end;
    end;
    dec(Count);
  end;
  Result := integer(Dst) - integer(DStart);
end;

function sdUtf8ToWideMem(Src: Pbyte; Dst: Pword; Count: integer): integer;
// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: word;
  C: byte;
  DStart: Pword;
  SClose: Pbyte;
begin
  DStart := Dst;
  SClose := Src;
  inc(SClose, Count);
  while integer(Src) < integer(SClose) do
  begin
    // 1st byte
    W := Src^;
    inc(Src);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := Src^;
        inc(Src);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd byte
      C := Src^;
      inc(Src);
      if C and $C0 <> $80 then
        // malformed trail byte
        Continue;
      Dst^ := (W shl 6) or (C and $3F);
      inc(Dst);
    end else
    begin
      Dst^ := W;
      inc(Dst);
    end;
  end;
  Result := (integer(Dst) - integer(DStart)) div 2;
end;

function sdWideToUtf8(const W: WideString): Utf8String;
var
  Count: integer;
begin
  Count := length(W);
  SetLength(Result, Count * 3); // just to be sure
  if Count = 0 then
    exit;

  Count := sdWideToUtf8Mem(@W[1], @Result[1], Count);
  SetLength(Result, Count);
end;

function sdUtf8ToWide(const S: Utf8String): WideString;
var
  Count: Integer;
begin
  Count := length(S);
  SetLength(Result, Count);
  if Count = 0 then
    exit;

  Count := sdUtf8ToWideMem(@S[1], @Result[1], Count);
  SetLength(Result, Count);
end;

function sdAnsiToUTF8(const S: AnsiString; ACodePage: integer): Utf8String;
var
  L: integer;
  WBuf: WideString;
  //Count: integer;
begin
  L := length(S);
  if L > 0 then
  begin
    SetLength(WBuf, L);
    // Windows function MultiByteToWideChar can be used to convert Ansi chars
    // with codepage into UTF16
    {Count := }MultiByteToWideChar(ACodePage, {dwFlags}0, @S[1], L, @WBuf[1], L);
    Result := sdWideToUTF8(WBuf);
  end else
    Result := '';
end;

function sdUTF8ToAnsi(const S: Utf8String; ACodePage: integer): AnsiString;
// Convert UTF8 to Ansi string
var
  L: integer;
  WBuf: WideString;
const
  cDefaultChar: AnsiChar = '?';
begin
  WBuf := sdUTF8ToWide(S);
  L := length(WBuf);
  if L > 0 then
  begin
    SetLength(Result, L);
    {Count := }WideCharToMultiByte(ACodePage, 0, @WBuf[1], L, @Result[1], L, @cDefaultChar, nil);
  end else
    Result := '';
end;

function sdEscapeString(const AValue: Utf8String): Utf8String;
// this function can use some optimization
var
  i: integer;
begin
  Result := AValue;
  for i := 0 to cEscapeCount - 1 do
    Result := StringReplace(Result, cXmlEscapePhrases[i], cXmlReplacePhrases[i], [rfReplaceAll]);
end;

function sdUnEscapeString(const AValue: Utf8String): Utf8String;
// this function can use some optimization
var
  SearchStr, Reference, Replace: Utf8String;
  i, Offset, Code: Integer;
  W: word;
begin
  SearchStr := AValue;
  Result := '';

  while SearchStr <> '' do
  begin
    // find '&'
    Offset := AnsiPos('&', SearchStr);
    if Offset = 0 then
    begin
      // Nothing found
      Result := Result + SearchStr;
      Break;
    end;
    Result := Result + Copy(SearchStr, 1, Offset - 1);
    SearchStr := Copy(SearchStr, Offset, MaxInt);

    // find next ';'
    Offset := AnsiPos(';', SearchStr);
    if Offset = 0 then
    begin
      // Error: encountered a '&' but not a ';'.. we will ignore, just return
      // the unmodified value
      Result := Result + SearchStr;
      Break;
    end;

    // Reference
    Reference := copy(SearchStr, 1, Offset);
    SearchStr := Copy(SearchStr, Offset + 1, MaxInt);
    Replace := Reference;

    // See if it is a character reference
    if copy(Reference, 1, 2) = '&#' then
    begin
      Reference := copy(Reference, 3, length(Reference) - 3);
      if length(Reference) > 0 then
      begin
        if lowercase(Reference[1]) = 'x' then
          // Hex notation
          Reference[1] := '$';
        Code := StrToIntDef(Reference, -1);
        if (Code >= 0) and (Code < $FFFF) then
        begin
          W := Code;
          Replace := sdWideToUtf8(WideChar(W));
        end;
      end;
    end else
    begin
      // Look up default escapes
      for i := 0 to cEscapeCount - 1 do
        if Reference = cXmlReplacePhrases[i] then
        begin
          // Replace
          Replace := cXmlEscapePhrases[i];
          Break;
        end;
    end;

    // New result
    Result := Result + Replace;
  end;
end;

procedure sdWriteToStream(S: TStream; const Value: Utf8String);
begin
  if Length(Value) > 0 then
  begin
    S.Write(Value[1], Length(Value));
  end;
end;

function sdEncodingToCodePage(const AEncoding: Utf8String): integer;
var
  i: integer;
begin
  for i := 0 to cCodePageInfoCount - 1 do
  begin
    if Utf8CompareText(AEncoding, cCodePageInfo[i].Name) = 0 then
    begin
      Result := cCodePageInfo[i].Codepage;
      exit;
    end;
  end;
  // Default to CP_UTF8
  Result := CP_UTF8;
end;

function Utf8CompareText(const S1, S2: Utf8String): integer;
begin
  // AnsiCompareText is case-insensitive
  Result := AnsiCompareText(AnsiString(S1), AnsiString(S2));
end;

function sdDateTimeToString(Value: TDateTime): Utf8String;
// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
begin
  DecodeDate(Value, AYear, AMonth, ADay);
  DecodeTime(Value, AHour, AMin, ASec, AMSec);
  if frac(Value) = 0 then
    Result := Utf8String(Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]))
  else
    Result := Utf8String(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
      [AYear, AMonth, ADay, AHour, AMin, ASec, AMSec]));
end;

function sdBoolToString(Value: boolean): Utf8String;
const
  cBoolValues: array[boolean] of Utf8String = ('False', 'True');
begin
  Result := cBoolValues[Value];
end;

function sdIntToString(Value: integer): Utf8String;
begin
  Result := Utf8String(IntToStr(Value));
end;

{ TFastMemStream }

procedure TFastMemStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

destructor TFastMemStream.Destroy;
begin
  ReallocMem(FMemory, 0);
  inherited;
end;

function TFastMemStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TFastMemStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TFastMemStream.SetCapacity(Value: longint);
var
  // Fibonacci 0,1,1,2,3,5,8,...  FCapacity is Fib2.
  // Fibonacci is a natural growing function where
  // 0 + 1 = 1; 1 + 1 = 2; 1 + 2 = 3; 2 + 3 = 5; etc
  Fib1, Fib3: longint;
begin
  Fib1 := $1000;
  FCapacity := $2000;
  while FCapacity < Value do
  begin
    Fib3 := Fib1 + FCapacity;
    Fib1 := FCapacity;
    FCapacity := Fib3;
  end;
  ReallocMem(FMemory, FCapacity);
end;

procedure TFastMemStream.SetSize(NewSize: longint);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soFromEnd);
end;

function TFastMemStream.Write(const Buffer; Count: Integer): Longint;
var
  NewPos: Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    NewPos := FPosition + Count;
    if NewPos > 0 then
    begin
      if NewPos > FSize then
      begin
        if NewPos > FCapacity then
          SetCapacity(NewPos);
        FSize := NewPos;
      end;
      System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
      FPosition := NewPos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TsdUTF8StringStream }

constructor TsdUTF8StringStream.Create(const S: Utf8String);
begin
  inherited Create;
  SetSize(length(S));
  if Size > 0 then
  begin
    Write(S[1], Size);
    Position := 0;
  end;
end;

function TsdUTF8StringStream.DataString: Utf8String;
begin
  SetLength(Result, Size);
  if Size > 0 then
  begin
    Position := 0;
    Read(Result[1], length(Result));
  end;
end;

end.
