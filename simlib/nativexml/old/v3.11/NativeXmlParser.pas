{ unit NativeXmlParser

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit NativeXmlParser;

interface

{$i NativeXml.inc}

uses
  Windows,Classes, Contnrs, SysUtils, NativeXmlUtils;

const
  cParserChunkSize = $100;

type

  // Special parser class to parse XML content. It buffers the source stream into
  // a memory buffer of limited size (64K) and reads from the stream chunk-wise.
  // This way, it can do string comparisons in memory, directly on the buffer.
  TsdBufferParser = class
  private
    FMemSource: TFastMemStream;
    FBuffer: array of byte; // ansi or utf8, depending on when encoding is found
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    FBomInfo: TBomInfo;
    FCapacity: integer;
    FCurrent: Pbyte;
    FLast: Pbyte;
    FCloseIdx: integer;
    FBasePosition: int64;
    FBaseLineNumber: int64;
    //FAnsiContent: array of byte;
    //FWideContent: array of byte;
    FEndOfStream: boolean;
    procedure ReadChunk;
    procedure SetCapacity(ACapacity: integer);
    function GetPosition: int64;
    function GetLineNumber: int64;
  public
    constructor Create(ASource: TStream);
    destructor Destroy; override;
    // Read BOM (begin of file marker) from the file in order to detect which
    // encoding is used.
    procedure ReadBOM;
    // Call flush once in a while, to check if data can be flushed out. Flushing
    // means that the part before the current pointer is removed and the bytes
    // following are moved to 0 position. It is only actually done when current
    // pointer is more than halfway the chunk size.
    procedure Flush(Force: boolean = False);
    // Make at least ACount bytes of data available from Current position.
    // Raises an exception if not successful.
    function MakeDataAvailable(ACount: integer): integer;
    // Check if at least ACount bytes of data are available. If available, the
    // data is made available. If not, the available count is returned.
    function HasDataAvailable(ACount: integer): integer;
    // Read the next character, skip any blanks inbetween. Blanks are
    // #9 (tab), #10 (lf), #13 (cr) and #32 (space)
    function NextCharSkipBlanks: AnsiChar;
    // Get the next character from the stream
    function NextChar: AnsiChar;
    // Skip over any blank characters
    procedure SkipBlanks;
    // Read an new tag from the stream (from the position afer "<")
    function ReadOpenTag: TsdElementType;
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
    // Read a string from the stream until a blank char, or a "/" or a ">" is
    // encountered.
    function ReadStringUntilBlankOrEndTag: Utf8String;
    // Read a string from the stream until character AChar is encountered. If
    // AllowEOF, the function returns gracefully if AChar is not found, but the
    // end of the stream instead.
    function ReadStringUntilChar(AChar: AnsiChar; AllowEOF: boolean): Utf8String;
    // The encoding detected in the source stream (valid after ReadBOM or after
    // the declaration).
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    // CodePage used in text processing
    property CodePage: integer read FCodePage write FCodePage;
    // Info from Byte Order Mark (BOM)
    property BomInfo: TBomInfo read FBomInfo;
    // Position in the stream in bytes from the start.
    property Position: int64 read GetPosition;
    // Line number in the stream. Lines are detected by analysing the stream
    // for occurances of #13 (CR). The line number is *calculated* when this
    // property is read, so it should not be read very regularly.
    property LineNumber: int64 read GetLineNumber;
    // Is the end of the stream detected?
    property EndOfStream: boolean read FEndOfStream;
  end;

implementation

{ TsdBufferParser }

function TsdBufferParser.CheckString(const S: Utf8String): boolean;
var
  i, Count: integer;
  P: PByte;
begin
  Count := length(S);
  MakeDataAvailable(Count);
  P := FCurrent;
  Result := True;
  for i := 1 to Count do
  begin
    if S[i] <> AnsiChar(P^) then
    begin
      Result := False;
      exit;
    end;
    inc(P);
  end;
  FCurrent := P;
end;

{procedure TsdBufferParser.ClearData;
begin
  FSource := nil;
  SetLength(FUtf8Buffer, 0);
  FCurr := nil;
  FLast := nil;
  FClose := 0;
  FCapacity := 0;
  SetLength(FAnsiContent, 0);
  SetLength(FwideContent, 0);
end;}

constructor TsdBufferParser.Create(ASource: TStream);
begin
  inherited Create;
  // For now, we use a memory stream as source copy
  FMemSource := TFastMemStream.Create;
  FMemSource.CopyFrom(ASource, ASource.Size);
  FMemSource.Position := 0;
  FCurrent := FMemSource.Memory;
  FLast := FCurrent;
end;

destructor TsdBufferParser.Destroy;
begin
  SetLength(FBuffer, 0);
  //SetLength(FAnsiContent, 0);
  //SetLength(FwideContent, 0);
  inherited;
end;

procedure TsdBufferParser.Flush(Force: boolean);
var
  i, Count, Dist: integer;
  P: Pbyte;
  Lim: integer;
begin
  Dist := integer(FCurrent) - integer(@FBuffer[0]);
  Lim := cParserChunkSize div 2;
  if Force then
    Lim := 1;
  if Dist > Lim then
  begin
    // Calcuate base line number
    P := @FBuffer[0];
    for i := 0 to Dist - 1 do
    begin
      if P^ = $0A then
        inc(FBaseLineNumber);
      inc(P);
    end;
    // Number of bytes to move
    Count := integer(FLast) - integer(FCurrent);
    Move(FCurrent^, FBuffer[0], Count);
    dec(FCurrent, Dist);
    dec(FLast, Dist);
    dec(FCloseIdx, Dist); // close index in the buffer
    FBasePosition := FBasePosition + Dist;
  end;
end;

function TsdBufferParser.GetLineNumber: int64;
var
  i, Count: integer;
  P: Pbyte;
begin
  Result := FBaseLineNumber;
  Count := integer(FCurrent) - integer(@FBuffer[0]);
  P := @FBuffer[0];
  for i := 0 to Count - 1 do
  begin
    // Linefeed
    if P^ = $0A then
      inc(Result);
    inc(P);
  end;
end;

function TsdBufferParser.GetPosition: int64;
begin
  Result := FBasePosition;
  if length(FBuffer) > 0 then
    inc(Result, integer(FCurrent) - integer(@FBuffer[0]));
end;

function TsdBufferParser.HasDataAvailable(ACount: integer): integer;
begin
  Result := integer(FLast) - integer(FCurrent);
  if Result < ACount then
  begin
    // We must make data available
    ReadChunk;
    Result := integer(FLast) - integer(FCurrent);
    if Result = 0 then
      FEndOfStream := True;
  end;
end;

function TsdBufferParser.MakeDataAvailable(ACount: integer): integer;
begin
  Result := integer(FLast) - integer(FCurrent);
  if Result < ACount then
  begin
    // We must make data available
    ReadChunk;
    Result := integer(FLast) - integer(FCurrent);
    // Still no data available?
    if Result < ACount then
      raise Exception.Create(sPrematureEnd);
  end;
end;

procedure TsdBufferParser.MoveBack;
begin
  dec(FCurrent);
end;

function TsdBufferParser.NextChar: AnsiChar;
var
  Count: integer;
begin
  Count := HasDataAvailable(1);
  if Count = 0 then
  begin
    Result := #0;
    exit;
  end;
  Result := AnsiChar(FCurrent^);
  inc(FCurrent);
end;

function TsdBufferParser.NextCharSkipBlanks: AnsiChar;
var
  Count: integer;
begin
  Count := HasDataAvailable(1);
  while Count > 0 do
  begin
    Result := AnsiChar(FCurrent^);
    inc(FCurrent);
    if not (Result in cXmlBlankChars) then
      exit;
    dec(Count);
    if Count = 0 then
      Count := HasDataAvailable(1);
  end;
  Result := #0;
end;

procedure TsdBufferParser.ReadBOM;
var
  i, j, BytesRead: integer;
  BOM: array[0..3] of byte;
  Found: boolean;
begin
  BytesRead := FMemSource.Read(BOM, 4);

  if BytesRead <> 4 then
    raise Exception.Create(sInvalidStream);

  i := 0;
  Found := False;
  while i < cBomInfoListCount do
  begin
    Found := True;
    for j := 0 to cBomInfoList[i].Len - 1 do
    begin
      if BOM[j] <> cBomInfoList[i].BOM[j] then
      begin
        Found := False;
        break;
      end;
    end;
    if Found then
    begin
      FBomInfo := cBomInfoList[i];
      FEncoding := FBomInfo.Encoding;
      break;
    end;
    inc(i);
  end;

  // None of the known BOMs?
  if not Found then
    raise Exception.Create(sUnsupportedEncoding);

  // Non-supported encodings
  if not (FEncoding in [seAnsi, seUTF8, seUTF16BE, seUTF16LE]) then
    raise Exception.Create(sUnsupportedEncoding);

  // Rewind based on BOM
  if FBomInfo.HasBom then
  begin
    FMemSource.Position := FBomInfo.Len
  end else
  begin
    // No BOM
    FMemSource.Position := 0;
  end;
end;

procedure TsdBufferParser.ReadChunk;
var
  i, CurrentIdx, BytesRead, ByteCount: integer;
  P: Pbyte;
  W: Pword;
begin
  if FCurrent = nil then
    CurrentIdx := 0
  else
    CurrentIdx := integer(FCurrent) - integer(@FBuffer[0]);

  // Read from the stream directly to our chunk
  BytesRead := FMemSource.Read(FBuffer[FCloseIdx], cParserChunkSize);
  P := @FBuffer[FCloseIdx];
  inc(FCloseIdx, BytesRead);

{  case FEncoding of
  seAnsi:
    begin
      // Increase capacity according to chunk size
      SetCapacity(FClose + cParserChunkSize);

      BytesRead := FMemSource.Read(FBuffer[FClose], cParserChunkSize);
      P := @FBuffer[FClose];
      inc(FClose, BytesRead);

      if FCodePage <> CP_UTF8 then
      begin
        // in case ansi, replace all occurances of values >= 128
        // much better would be:
        // MultiByteToWideChar for all ansi characters >= 128
        for i := 0 to BytesRead - 1 do
        begin
          if P^ >= 128 then
            P^ := ord('?');
          inc(P);
        end;
      end;
    end;

  seUTF8:
    begin
      // Increase capacity according to chunk size
      SetCapacity(FClose + cParserChunkSize);

      // Read from the stream directly to our chunk
      BytesRead := FSource.Read(FUtf8Buffer[FClose], cParserChunkSize);
      P := @FUtf8Buffer[FClose];
      inc(FClose, BytesRead);
    end;

  seUTF16BE, seUTF16LE:
    begin
      // Increase capacity according to chunk size, we must take into account
      // that UTF16 chunks can become larger when converted to UTF8. In theory
      // this is only 3/2, but we will use a factor of 2.
      SetCapacity(FClose + cParserChunkSize * 2);

      // Buffer for UTF16 content
      SetLength(FWideContent, cParserChunkSize);

      // Read UTF16 content
      BytesRead := FSource.Read(FWideContent[0], cParserChunkSize);

      // If UTF16 BE (Big Endian), we must swap byte order
      if FEncoding = seUTF16BE then
      begin
        W := @FWideContent[0];
        for i := 0 to BytesRead div 2 - 1 do
        begin
          W^ := Swap(W^);
          inc(W);
        end;
      end;

      // Now convert from UTF16 to UTF8
      ByteCount := sdWideToUtf8Mem(@FWideContent[0], @FUtf8Buffer[FClose], BytesRead div 2);
      inc(FClose, ByteCount);
    end;
  end;}

  // Set pointers
  if FCloseIdx > 0 then
  begin
    FCurrent := @FBuffer[CurrentIdx];
    FLast := @FBuffer[FCloseIdx];
  end else
  begin
    FCurrent := nil;
    FLast := nil;
  end;
end;

function TsdBufferParser.ReadOpenTag: TsdElementType;
var
  Count: integer;
  Ch: AnsiChar;
begin
  Result := xeError;
  Count := MakeDataAvailable(1);
  Ch := AnsiChar(FCurrent^);
  inc(FCurrent);
  case Ch of
  '!':
    begin
      if Count = 0 then
        MakeDataAvailable(1);
      Ch := AnsiChar(FCurrent^);
      inc(FCurrent);
      case Ch of
      '[': if CheckString('CDATA[') then
        Result := xeCData;
      'D': if CheckString('OCTYPE') then
        Result := xeDocType;
      'E':
        begin
          if CheckString('LEMENT') then
            Result := xeDtdElement;
          if CheckString('NTITY') then
            Result := xeDtdEntity;
        end;
      'A': if CheckString('TTLIST') then
        Result := xeDtdAttList;
      'N': if CheckString('OTATION') then
        Result := xeDtdNotation;
      '-': if CheckString('-') then
        Result := xeComment;
      else
        raise Exception.Create(sIllegalTag);
      end;
    end;
  '?':
    begin
      if CheckString('xml') then
      begin
        if CheckString('-stylesheet') then
          Result := xeStyleSheet
        else
          Result := xeDeclaration;
      end else
        Result := xeInstruction;
    end;
  '/': Result := xeEndTag;
  else
    Result := xeElement;
    dec(FCurrent);
  end;
end;

function TsdBufferParser.ReadQuotedString(AQuote: AnsiChar): Utf8String;
begin
  // It seems that the xml spec simply does not allow double quotes as in
  // Delphi, so we do not need a complicated algo to do this. We can simply
  // search for the quote again as terminator.
  Result := ReadStringUntilChar(AQuote, False);
end;

function TsdBufferParser.ReadStringUntil(const Terminator: Utf8String): Utf8String;
var
  Count, MatchCount, MatchLen: integer;
  PStart, PMatch: Pbyte;
  First: byte;
  MatchIdx, StartIdx: integer;
begin
  Count := MakeDataAvailable(1);
  MatchLen := length(Terminator);
  MatchCount := 0;
  PStart := FCurrent;
  First := ord(Terminator[1]);
  repeat
    if PStart^ = First then
    begin

      // Here we do the matching
      PMatch := PStart;
      repeat
        inc(MatchCount);
        if MatchCount = MatchLen then
        begin

          // We found the terminating string
          Count := integer(PStart) - Integer(FCurrent);
          SetLength(Result, Count);
          if Count > 0 then
            Move(FCurrent^, Result[1], Count);

          // Adjust FCurrent
          FCurrent := PMatch;
          inc(FCurrent);
          exit;
        end;

        // Match a character
        inc(PMatch);
        dec(Count);
        if Count <= 0 then
        begin
          MatchIdx := integer(PMatch) - integer(FCurrent);
          StartIdx := integer(PStart) - integer(FCurrent);
          FCurrent := PMatch;
          Count := MakeDataAvailable(1);
          PMatch := FCurrent;
          dec(FCurrent, MatchIdx);
          PStart := FCurrent;
          inc(PStart, StartIdx);
        end;

        if ord(Terminator[MatchCount + 1]) <> PMatch^ then
          break;
      until False;
      MatchCount := 0;
    end;

    inc(PStart);
    dec(Count);
    if Count <= 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurrent);
      FCurrent := PStart;
      Count := MakeDataAvailable(1);
      PStart := FCurrent;
      dec(FCurrent, StartIdx);
    end;
  until Count = 0;

  raise Exception.Create(sPrematureEnd);
end;

function TsdBufferParser.ReadStringUntilBlankOrEndTag: Utf8String;
var
  Count: integer;
  PStart: Pbyte;
  StartIdx: integer;
begin
  Count := MakeDataAvailable(1);
  PStart := FCurrent;
  repeat
    if AnsiChar(PStart^) in cXmlBlankCharsOrEndTag then
    begin
      // We found the termination
      Count := integer(PStart) - Integer(FCurrent);
      SetLength(Result, Count);
      if Count > 0 then
        Move(FCurrent^, Result[1], Count);
      // Adjust FCurr
      FCurrent := PStart;
      exit;
    end;
    inc(PStart);
    dec(Count);
    if Count <= 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurrent);
      FCurrent := PStart;
      Count := MakeDataAvailable(1);
      PStart := FCurrent;
      dec(FCurrent, StartIdx);
    end;
  until Count = 0;
  raise Exception.Create(sPrematureEnd);
end;

function TsdBufferParser.ReadStringUntilChar(AChar: AnsiChar; AllowEOF: boolean): Utf8String;
var
  Count: integer;
  PStart: Pbyte;
  StartIdx: integer;
begin
  if AllowEOF then
  begin
    Count := HasDataAvailable(1);
    if Count = 0 then
    begin
      // End of stream
      Result := '';
      exit;
    end;
  end else
    Count := MakeDataAvailable(1);

  PStart := FCurrent;
  repeat
    if AnsiChar(PStart^) = AChar then
    begin
      // We found AChar
      Count := integer(PStart) - integer(FCurrent);
      SetLength(Result, Count);
      if Count > 0 then
        Move(FCurrent^, Result[1], Count);
      // Adjust FCurr
      FCurrent := PStart;
      inc(FCurrent);
      exit;
    end;
    inc(PStart);
    dec(Count);
    if Count = 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurrent);
      FCurrent := PStart;
      if AllowEOF then
        Count := HasDataAvailable(1)
      else
        Count := MakeDataAvailable(1);
      PStart := FCurrent;
      dec(FCurrent, StartIdx);
    end;
  until Count = 0;

  // Arriving here: end of stream
  Count := integer(PStart) - integer(FCurrent);
  SetLength(Result, Count);
  if Count > 0 then
    Move(FCurrent^, Result[1], Count);
  FCurrent := PStart;
end;

procedure TsdBufferParser.SetCapacity(ACapacity: integer);
begin
  if FCapacity = 0 then
    FCapacity := cParserChunkSize;
  while ACapacity > FCapacity do
    FCapacity := FCapacity * 2;
  SetLength(FBuffer, FCapacity);
end;

procedure TsdBufferParser.SkipBlanks;
begin
  NextCharSkipBlanks;
  MoveBack;
end;

end.
