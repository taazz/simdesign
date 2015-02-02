{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Core<p>

  <b>Description:</b><p>
  Encoding / decoding with the BASE64 scheme

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2007 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgBase64Encode;

{$i simdesign.inc}

interface

uses
  SysUtils, sdDebug;

// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as string, without any linebreaks.
function EncodeBase64(const Source: RawByteString): Utf8String;

// Decode BASE64 data in Source into binary data. The function returns the binary
// data as string. Use a TStringStream to convert this data to a stream. The Source
// string may contain linebreaks and control characters, these will be stripped.
function DecodeBase64(const Source: Utf8String): RawByteString;

implementation

const
  // These characters are used when generating BASE64 chars from buffer data
  cBase64Char: array[0..63] of ansichar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  cBase64PadChar: ansichar = '=';
  cControlChars: set of char = [#9, #10, #13, #32]; {Tab, LF, CR, Space}

function RemoveControlChars(const AValue: Utf8String): Utf8String;
// Remove control characters from string in AValue
var
  i, j: integer;
begin
  Setlength(Result, Length(AValue));
  i := 1;
  j := 1;
  while i <= Length(AValue) do
    if AValue[i] in cControlChars then
      inc(i)
    else
    begin
      Result[j] := AValue[i];
      inc(i);
      inc(j);
    end;
  // Adjust length
  if i <> j then
    SetLength(Result, j - 1);
end;

function EncodeBase64Buf(const Buffer; Count: Integer): Utf8String;
var
  i, j: integer;
  Core: integer;
  Long: cardinal;
  S: PByte;
begin
  // Make sure ASize is always a multiple of 3, and this multiple
  // gets saved as 4 characters
  Core := (Count + 2) div 3;

  // Set the length of the string that stores encoded characters
  SetLength(Result, Core * 4);
  S := @Buffer;
  // Do the loop ACore times
  for i := 0 to Core - 1 do
  begin
    Long := 0;
    for j := 0 to 2 do
    begin
      Long := Long shl 8 + S^;
      inc(S);
    end;
    for j := 0 to 3 do
    begin
      Result[i * 4 + 4 - j] := cBase64Char[Long and $3F];
      Long := Long shr 6;
    end;
  end;
  // For comformity to Base64, we must pad the data instead of zero out
  // if the size is not an exact multiple of 3
  case Core * 3 - Count of
  0:;// nothing to do
  1: // pad one byte
    Result[Core * 4] := cBase64PadChar;
  2: // pad two bytes
    begin
      Result[Core * 4    ] := cBase64PadChar;
      Result[Core * 4 - 1] := cBase64PadChar;
    end;
  end;//case
end;

procedure DecodeBase64Buf(var Source: Utf8String; var Buffer; Count: Integer);
var
  i, j: integer;
  Pos, Core: integer;
  Long: cardinal;
  D: PByte;
  Map: array[Char] of byte;
begin
  // Core * 4 is the number of chars to read - check length
  Core := Length(Source) div 4;
  if Count > Core * 3 then
    raise Exception.Create('Missing data in binary stream');

  // Prepare map
  for i := 0 to 63 do
    Map[cBase64Char[i]] := i;
  D := @Buffer;

  // Check for final padding, and replace with "zeros". There can be
  // at max two pad chars ('=')
  Pos := length(Source);
  if (Pos > 0) and (Source[Pos] = cBase64PadChar) then
  begin
    Source[Pos] := cBase64Char[0];
    dec(Pos);
    if (Pos > 0) and (Source[Pos] = cBase64PadChar) then
      Source[Pos] := cBase64Char[0];
  end;

  // Do this Core times
  for i := 0 to Core - 1 do
  begin
    Long := 0;
    // Unroll the characters
    for j := 0 to 3 do
      Long := Long shl 6 + Map[Source[i * 4 + j + 1]];
    // and unroll the bytes
    for j := 2 downto 0 do
    begin
      // Check overshoot
      if integer(D) - integer(@Buffer) >= Count then
        exit;
      D^ := Long shr (j * 8) and $FF;
      inc(D);
    end;
  end;
end;

function EncodeBase64(const Source: RawByteString): Utf8String;
// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as string, without any linebreaks.
begin
  if length(Source) > 0 then
    Result := EncodeBase64Buf(Source[1], length(Source))
  else
    Result := '';
end;

function DecodeBase64(const Source: Utf8String): RawByteString;
// Decode BASE64 data in Source into binary data. The function returns the binary
// data as string. Use a TStringStream to convert this data to a stream.
var
  Data: Utf8String;
  Size, Pos: integer;
begin
  Data := RemoveControlChars(Source);

  // Determine length of data
  Size := length(Data) div 4;
  if Size * 4 <> length(Data) then
    raise Exception.Create('Error in stream length calculation');
  Size := Size * 3;
  // Check padding chars
  Pos := length(Data);
  if (Pos > 0) and (Data[Pos] = cBase64PadChar) then
  begin
    dec(Pos);
    dec(Size);
    if (Pos > 0) and (Data[Pos] = cBase64PadChar) then
      dec(Size);
  end;
  Setlength(Result, Size);

  // Decode
  if Size > 0 then
    DecodeBase64Buf(Data, Result[1], Size);
end;

end.
