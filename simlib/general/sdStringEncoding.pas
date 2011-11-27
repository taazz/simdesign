{ unit sdStringEncoding

  String Encoding routines (eg UTF-8, Ansi, etc)

  Original code comes from NativeXmlUtils.pas

  Date: 18nov2010
  Author: Nils Haeck M.Sc.
  Copyright (c) 2010 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit sdStringEncoding;

{$ifdef lcl}{$MODE Delphi}{$endif}

interface

uses
{$ifdef WIN32}
  Windows;  // uses MultiByteToWideChar
{$else WIN32}
  // linux: win32-compatible functions
  NativeXmlWin32Compat;
{$endif WIN32}

const

  // UTF8 codepage
  CP_UTF8: integer = 65001;

type

  // Definition of different methods of string encoding.
  TsdStringEncoding = (
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

const

  cStringEncodingNames: array[TsdStringEncoding] of Utf8String =
    ('Ansi', 'UTF8', 'UTF16BE', 'UTF16LE', 'UCS4BE', 'UCS4LE',
    'UCS4_2143', 'UCS4_3412', 'EBCDIC');

  cStringEncodingCodePages: array[TsdStringEncoding] of integer =
    (1250, 65001, 1201, 1200, 0, 0, 0, 0, 0);

// Convert Ansi to Utf8 using buffers
// please note: Utf8Buf can use 3x more size than AnsiBuf in extreme cases.
// Result is the Utf8Buf bytecount
function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer): integer;

// Convert Utf8 to Ansi using buffers
function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer;
  var DefaultCharUsed: boolean): integer;

// determine the character length of the first Utf8 character in the buffer
function sdUtf8CharacterLength(const Buffer): integer;

// Convert a "WideString" (UTF16 LE) buffer to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the Utf8Buf must be at least 1.5 the size of the WideBuf.
// The function returns the number of *bytes* written.
function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer): integer;

// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;

implementation

function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer): integer;
type
  TAnsiCharArray = array[0..0] of AnsiChar;
var
  AnsiIdx, Utf8Idx: integer;
  AnsiCh: AnsiChar;
  WideCh: WideChar;
  Len: integer;
begin
  AnsiIdx := 0;
  Utf8Idx := 0;
  while AnsiIdx < AnsiCount do
  begin
    AnsiCh := TAnsiCharArray(AnsiBuf)[AnsiIdx];
    if ord(AnsiCh) < $80 then
    begin
      // characters < $80: just copy the single characters
      TAnsiCharArray(Utf8Buf)[Utf8Idx] := AnsiCh;
      inc(Utf8Idx);
    end else
    begin
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // MultiByteToWideChar is in the Windows unit of Borland Delphi 7
      MultiByteToWideChar(ACodePage, 0, @AnsiCh, 1, @WideCh, 1);
      Len := sdWideToUtf8Buffer(WideCh, TAnsiCharArray(Utf8Buf)[Utf8Idx], 1);
      inc(Utf8Idx, Len);
    end;
    inc(AnsiIdx);
  end;
  Result := Utf8Idx;
end;

function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer;
  var DefaultCharUsed: boolean): integer;
type
  TAnsiCharArray = array[0..0] of AnsiChar;
var
  AnsiIdx, Utf8Idx: integer;
  Utf8Ch: AnsiChar;
  WideCh: WideChar;
  Len: integer;
  DU: pointer;
const
  cDefaultChar: AnsiChar = '?';
begin
  AnsiIdx := 0;
  Utf8Idx := 0;
  while Utf8Idx < Utf8Count do
  begin
    Utf8Ch := TAnsiCharArray(Utf8Buf)[Utf8Idx];
    if ord(Utf8Ch) < $80 then
    begin
      // characters < $80: just copy the single characters
      DefaultCharUsed := False;
      Len := 1;
      TAnsiCharArray(AnsiBuf)[AnsiIdx] := Utf8Ch;
      inc(AnsiIdx);
    end else
    begin
      Len := sdUtf8CharacterLength(TAnsiCharArray(Utf8Buf)[Utf8Idx]);
      sdUtf8ToWideBuffer(TAnsiCharArray(Utf8Buf)[Utf8Idx], WideCh, 1);
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // WideCharToMultiByte is in the Windows unit of Borland Delphi 7
      DefaultCharUsed := False;
      DU := @DefaultCharUsed;
      WideCharToMultiByte(ACodePage, 0, @WideCh, 1, @TAnsiCharArray(AnsiBuf)[AnsiIdx], 1, @cDefaultChar, @DU);
      DefaultCharUsed := DU <> nil;
      inc(AnsiIdx);
    end;
    inc(Utf8Idx, Len);
  end;
  Result := AnsiIdx;
end;

function sdUtf8CharacterLength(const Buffer): integer;
// determine the character length (1..4 bytes) of the Utf8 character
// in the buffer
type
  TByteArray = array[0..3] of byte;
var
  P0, P1, P2, P3: byte;
begin
  P0 := TByteArray(Buffer)[0];
  Result := 1;
  if P0 < $C0 then // %11000000
  begin
    // regular single byte character
    exit;
  end;
  P1 := TByteArray(Buffer)[1];
  if (P0 and $E0) = $C0 then
  begin
    // could be 2 byte character
    if (P1 and $C0) = $80 then
    begin
      Result := 2;
    end;
    exit;
  end;
  P2 := TByteArray(Buffer)[2];
  if (P0 and $F0) = $E0 then
  begin
    // could be 3 byte character
    if ((P1 and $C0) = $80) and ((P2 and $C0) = $80) then
    begin
      Result := 3;
    end;
    exit;
  end;
  P3 := TByteArray(Buffer)[3];
  if (P0 and $F8) = $F0 then
  begin
    // could be 4 byte character
    // NB 4 byte chars are incompatible with Widechar since
    // they are outside the basic lingual plane
    if    ((P1 and $C0) = $80)
      and ((P2 and $C0) = $80)
      and ((P3 and $C0) = $80) then
    begin
      Result := 4;
    end;
  end;
end;

function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
type
  TWordArray = array[0..0] of word;
  TByteArray = array[0..0] of byte;
var
  W: word;
  WideIdx, Utf8Idx: integer;
begin
  WideIdx := 0;
  Utf8Idx := 0;
  while WideIdx < WideCount do
  begin
    W := TWordArray(WideBuf)[WideIdx];
    if W <= $7F then
    begin
      TByteArray(Utf8Buf)[Utf8Idx] := byte(W);
      inc(Utf8Idx);
    end else
    begin
      if W > $7FF then
      begin
        TByteArray(Utf8Buf)[Utf8Idx] := byte($E0 or (W shr 12));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or ((W shr 6) and $3F));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end else
      begin //  $7F < W <= $7FF
        TByteArray(Utf8Buf)[Utf8Idx] := byte($C0 or (W shr 6));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end;
    end;
    inc(WideIdx);
  end;
  Result := Utf8Idx;
end;

function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;
// Convert an UTF8 buffer to Unicode (UTF16 LE) buffer. This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at WideBuf must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Utf8Buf block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
type
  TWordArray = array[0..0] of word;
  TByteArray = array[0..0] of byte;
var
  W: word;
  C: byte;
  WideIdx, Utf8Idx: integer;
begin
  Utf8Idx := 0;
  WideIdx := 0;
  while Utf8Idx < ByteCount do
  begin
    // 1st byte
    W := TByteArray(Utf8Buf)[Utf8Idx];
    inc(Utf8Idx);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := TByteArray(Utf8Buf)[Utf8Idx];
        inc(Utf8Idx);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd byte
      C := TByteArray(Utf8Buf)[Utf8Idx];
      inc(Utf8Idx);
      if C and $C0 <> $80 then
        // malformed trail byte
        Continue;
      TWordArray(WideBuf)[WideIdx] := (W shl 6) or (C and $3F);
      inc(WideIdx);
    end else
    begin
      TWordArray(WideBuf)[WideIdx] := W;
      inc(WideIdx);
    end;
  end;
  Result := WideIdx;
end;

end.

