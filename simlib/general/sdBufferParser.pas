{ unit sdBufferParser

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.

  NOTE: flush works with an interval:
  FRawFirstIdx       - ?           - FRawLastIdx  (raw stream)
  FFirstIdx          - FCurrentIdx - FLastIdx     (utf8 stream)
}
unit sdBufferParser;

{$ifdef lcl}{$MODE Delphi}{$endif}

interface

uses
  Classes, SysUtils, sdDebug, sdStringEncoding;

type

  // Generic buffer parser. It buffers the source stream into
  // a memory buffer of limited size and reads from the stream chunk-wise.
  // This way, it can do string comparisons in memory, directly on the buffer.
  TsdBufferParser = class(TDebugPersistent)
  protected
    FSource: TStream;
    FChunkSize: integer;
    FRawBuffer: array of byte;      // raw data buffer
    FUtf8Buffer: array of AnsiChar; // utf8 data buffer
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    FRawFirstIdx: integer;
    FRawLastIdx: integer;
    FFirstIdx: integer;
    FCurrentIdx: integer;
    FLastIdx: integer;
    FBasePosition: int64;
    FBaseLineNumber: int64;
    FEndOfStream: boolean;
    FNormaliseEOLEnabled: boolean;
    FOnDebugOut: TsdDebugEvent;
    function LoCase(Ch: AnsiChar): AnsiChar;
    procedure IncCurrentIdxCheck(var BytesAvail: integer);
    function ReadString(AIndex, ACount: integer): Utf8String;
    function ReadNextChunk: integer;
    procedure EncodeChunk;
    function GetPosition: int64;
    function GetLineNumber: int64;
    procedure SetCodePage(const Value: integer);
  public
    constructor Create(ASource: TStream; AChunkSize: integer); virtual;
    destructor Destroy; override;
    // Call flush once in a while, to check if data can be flushed out. Flushing
    // means that the part before the current pointer is removed and the bytes
    // following are moved to 0 position. It is only actually done when enough
    // chunks are read, and the flushing happens chunk-wise.
    procedure Flush(Force: boolean = False);
    // Make at least one byte available from current position
    function MakeDataAvailable: integer;
    // Get the next character from the stream
    function NextChar: AnsiChar;
    // collapse all EOL to #$0A
    procedure NormaliseEOL;
    // Check if the stream at this position contains string S. If so, the stream
    // will be positioned after, if not, it will remain where it is.
    function CheckString(const S: Utf8String): boolean;
    // Move one position back in the stream
    procedure MoveBack;
    // Read a string from the stream until Terminator is found. The string returned
    // will be the part before Terminator, the stream is positioned after Terminator
    function ReadStringUntil(const Terminator: Utf8String): Utf8String;
    // Read a quoted string from the stream, return the unquoted string
    function ReadQuotedString(AQuote: AnsiChar): Utf8String;
    // Read a string from the stream until character AChar is encountered.
    // var EOS will be True if the stream reached the end.
    function ReadStringUntilChar(AChar: AnsiChar): Utf8String;
    // The encoding detected in the source stream (valid after ReadBOM or after
    // the declaration).
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    // CodePage used in text processing
    property CodePage: integer read FCodePage write SetCodePage;
    // Position in the stream in bytes from the start.
    property Position: int64 read GetPosition;
    // Line number in the stream. Lines are detected by analysing the stream
    // for occurances of #13 (CR). The line number is *calculated* when this
    // property is read, so it should not be read very regularly.
    property LineNumber: int64 read GetLineNumber;
    // Is the end of the stream detected?
    property EndOfStream: boolean read FEndOfStream;
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

resourcestring

  sPrematureEnd = 'stream terminated prematurely at pos %d';

implementation

{ TsdBufferParser }

function TsdBufferParser.CheckString(const S: Utf8String): boolean;
// case-insensitive string check
var
  i, Count, StartIdx: integer;
begin
  Count := MakeDataAvailable;
  StartIdx := FCurrentIdx;
  Result := True;
  for i := 1 to length(S) do
  begin
    if FEndOfStream then
    begin
      Result := False;
      exit;
    end;
    // case-insensitive, so we use LoCase in both sides (LoCase is
    // faster than function LowerCase, since it deals directly with chars).
    if LoCase(S[i]) <> LoCase(FUtf8Buffer[FCurrentIdx]) then
    begin
      Result := False;
      // revert
      FCurrentIdx := StartIdx;
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;
end;

constructor TsdBufferParser.Create(ASource: TStream; AChunkSize: integer);
begin
  inherited Create;
  FSource := ASource;
  FChunkSize := AChunkSize;
  SetLength(FRawBuffer, FChunkSize);

  // Read from the stream directly to the raw buffer
  FRawFirstIdx := 0;
  FRawLastIdx := FSource.Read(FRawBuffer[0], FChunkSize);
  FFirstIdx := 0;
  FCurrentIdx := 0;

  // Normalise end-of-line is enabled by default (True)
  FNormaliseEOLEnabled := True;
end;

destructor TsdBufferParser.Destroy;
begin
  SetLength(FRawBuffer, 0);
  SetLength(FUtf8Buffer, 0);
  inherited;
end;

procedure TsdBufferParser.EncodeChunk;

  // local
  procedure EncodeAnsiChunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    // Utf8 buffer might be 3x ansi size at max
    SetLength(FUtf8Buffer, FFirstIdx + 3 * RawLen);
    Utf8Len := sdAnsiToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FFirstIdx], FCodePage, RawLen);
    FLastIdx := FFirstIdx + Utf8Len;
  end;

  // local
  procedure EncodeUtf8Chunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    // buffers
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FFirstIdx + RawLen);
    Move(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FFirstIdx], RawLen);
    Utf8Len := RawLen;
    FLastIdx := FFirstIdx + Utf8Len;
  end;

  // local
  procedure EncodeUtf16Chunk;
  type
    TWordArray = array of word;
  var
    RawLen, Utf8Len: integer;
    i: integer;
    W: word;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;

    // If UTF16 BE (Big Endian), we must swap byte order
    if FEncoding = seUTF16BE then
    begin
      for i := FRawFirstIdx div 2 to FRawLastIdx div 2 - 1 do
      begin
        W := TWordArray(FRawBuffer)[i];
        TWordArray(FRawBuffer)[i] := Swap(W);
      end;
    end;

    // Utf8 buffer might be 2x utf16 size at max
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FFirstIdx + (2 * RawLen));

    // Now convert from UTF16 to UTF8
    Utf8Len := sdWideToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FFirstIdx], RawLen div 2);
    FLastIdx := FFirstIdx + Utf8Len;
  end;
// main
begin
  // call EncodeChunk methods based on encoding
  case FEncoding of
  seAnsi:
    begin
      if (FCodePage = 0) or (FCodePage = CP_UTF8) then
      begin
        EncodeUtf8Chunk;
      end else
      begin
        EncodeAnsiChunk;
      end;
    end;

  seUTF8:
    begin
      EncodeUtf8Chunk;
    end;

  seUTF16BE, seUTF16LE:
    begin
      EncodeUtf16Chunk;
    end;
  end;

  // collapse all end-of-line to a single LineFeed (#$0A)
  if FNormaliseEOLEnabled then
    NormaliseEOL;

end;

procedure TsdBufferParser.Flush(Force: boolean);
var
  i: integer;
  RawLen, Utf8Len: integer;
begin
  // Number of bytes to move
  RawLen := FRawLastIdx - FRawFirstIdx;
  Utf8Len := FLastIdx - FFirstIdx;
  if FCurrentIdx - FFirstIdx > 0 then
  begin
    // Calcuate base line number and base position
    for i := 0 to FFirstIdx - 1 do
    begin
      // linefeed
      if FUtf8Buffer[i] = #$0A then
        inc(FBaseLineNumber);
    end;
    inc(FBasePosition, FFirstIdx);
    // moves
    Move(FRawBuffer[FRawFirstIdx], FRawBuffer[0], RawLen);
    Move(FUtf8Buffer[FFirstIdx], FUtf8Buffer[0], Utf8Len);
    // update current idx
    dec(FCurrentIdx, FFirstIdx);
    // update first/last indices
    FRawFirstIdx := 0;
    FRawLastIdx := RawLen;
    FFirstIdx := 0;
    FLastIdx := Utf8Len;
  end;
end;

function TsdBufferParser.GetLineNumber: int64;
var
  i: integer;
begin
  Result := FBaseLineNumber;
  for i := 0 to FCurrentIdx - 1 do
  begin
    // linefeed
    if FUtf8Buffer[i] = #$0A then
      inc(Result);
  end;
end;

function TsdBufferParser.GetPosition: int64;
begin
  Result := FBasePosition + FCurrentIdx;
end;

procedure TsdBufferParser.IncCurrentIdxCheck(var BytesAvail: integer);
// increment FCurrentIdx and check bytes available
begin
  inc(FCurrentIdx);
  dec(BytesAvail);
  if BytesAvail <= 0 then
    BytesAvail := MakeDataAvailable
end;

function TsdBufferParser.LoCase(Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  case Result of
    'A'..'Z':  inc(Result, Ord('a') - Ord('A'));
  end;
end;

function TsdBufferParser.MakeDataAvailable: integer;
var
  BytesRead: integer;
begin
  Result := FLastIdx - FCurrentIdx;
  while Result < 1 do
  begin
    // We must make data available
    BytesRead := ReadNextChunk;
    Result := FLastIdx - FCurrentIdx;
    // Still no data available?
    if BytesRead = 0 then
    begin
      FEndOfStream := True;
      exit;
    end;
  end;
end;

procedure TsdBufferParser.MoveBack;
begin
  assert(FCurrentIdx > 0);
  dec(FCurrentIdx);
end;

function TsdBufferParser.NextChar: AnsiChar;
begin
  MakeDataAvailable;
  if FEndOfStream then
  begin
    Result := #0;
    exit;
  end;
  Result := FUtf8Buffer[FCurrentIdx];
  inc(FCurrentIdx);
end;

procedure TsdBufferParser.NormaliseEOL;
{//todo
const
  cxD: byte = $0D;
  cxA: byte = $0A;
  cx85_1: byte = ...;
  cx85_2: byte = ...;
  cx2021_1: byte = ...;
  cx2021_2: byte = ...;}
var
  i: integer;
begin
  // collapse all end-of-line to a single LineFeed (#$0A)
  i := FFirstIdx;
  while i < FLastIdx do
  begin
    if FUtf8Buffer[i] = #$0A then
    begin
      if FUtf8Buffer[i - 1] = #$0D then
      begin
        Move(FUtf8Buffer[i], FUtf8Buffer[i - 1], FLastIdx - i);
        dec(FLastIdx);
      end;
    end;
    inc(i);
  end;
end;

function TsdBufferParser.ReadNextChunk: integer;
begin
  SetLength(FRawBuffer, FRawLastIdx + FChunkSize);

  // Read from the stream directly to our chunk
  // Result is the bytes read
  Result := FSource.Read(FRawBuffer[FRawLastIdx], FChunkSize);
  if Result = FChunkSize then
  begin
    FRawFirstIdx := FRawLastIdx;
    FFirstIdx := FLastIdx;
  end;
  if Result > 0 then
  begin
    inc(FRawLastIdx, Result);
    EncodeChunk;
  end;
end;

function TsdBufferParser.ReadQuotedString(AQuote: AnsiChar): Utf8String;
begin
  // It seems that the xml spec simply does not allow double quotes as in
  // Delphi, so we do not need a complicated algo to do this. We can simply
  // search for the quote again as terminator.
  Result := ReadStringUntilChar(AQuote);
end;

function TsdBufferParser.ReadString(AIndex, ACount: integer): Utf8String;
begin
  SetLength(Result, ACount);
  if ACount > 0 then
    Move(FUtf8Buffer[AIndex], Result[1], ACount);
end;

function TsdBufferParser.ReadStringUntil(const Terminator: Utf8String): Utf8String;
var
  Count, MatchLen: integer;
  FirstChar: AnsiChar;
  StartIdx: integer;
begin
  FirstChar := Terminator[1];
  MatchLen := length(Terminator);
  StartIdx := FCurrentIdx;
  Count := MakeDataAvailable;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FCurrentIdx] = FirstChar then
    begin

      if CheckString(Terminator) then
      begin
        // We found the terminating string
        Result := ReadString(StartIdx, FCurrentIdx - StartIdx - MatchLen);
        exit;
      end;

    end;
    IncCurrentIdxCheck(Count);
  end;
  // when left here stream ended prematurely
  DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [GetPosition]));
end;

function TsdBufferParser.ReadStringUntilChar(AChar: AnsiChar): Utf8String;
var
  Count: integer;
  StartIdx: integer;
begin
  Count := MakeDataAvailable;

  StartIdx := FCurrentIdx;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FCurrentIdx] = AChar then
    begin
      // We found AChar
      Result := ReadString(StartIdx, FCurrentIdx - StartIdx);
      // Adjust FCurrentIdx
      inc(FCurrentIdx);
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;

  // Arriving here: end of stream and AChar not reached
  Result := ReadString(StartIdx, FCurrentIdx - StartIdx);
end;

procedure TsdBufferParser.SetCodePage(const Value: integer);
begin
  FCodePage := Value;
  // re-encode the chunk (eg from default UTF-8 codepage to other ansi codepage)
  EncodeChunk;
end;

end.
