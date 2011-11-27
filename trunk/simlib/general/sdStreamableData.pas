{ unit sdStreamableData
  Version: 2.0

  Description:

  Stream and Compression routines. Saving/Loading data like strings,
  floats, integers etc. from and to Streams, Compression using LZH
  compression schemes for saving diskspace.

  See comments in source code for more detailed documentation.

  Created  on: 06-01-2000
  Author: N. Haeck (NH)

  Modifications:
  18-01-2000 (NH): Added Reading/Writing of Boolean and TStringlist.
  23-08-2001 (NH): Changed order of arguments in StreamReadxxx,
                   Changed method for loading/saving TStringList

  Copyright (c) 2000-2001 Nils Haeck.
}
unit sdStreamableData;

interface

uses
  Windows, SysUtils, Classes;

{ Stream reading/writing }

// StreamReadString reads a string from the stream. The string should not be longer
// than 2 gigabytes.
procedure StreamReadString(S: TStream; var Value: string);
// StreamWriteString writes a string to the stream. The string should not be longer
// than 2 gigabytes.
procedure StreamWriteString(S: TStream; Value: string);

// StreamReadShortString reads a string with max length of 255 chars.
procedure StreamReadShortString(S: TStream; var Value: string);
// StreamWriteShortString writes a string with max length of 255 chars. If the
// string in Value is longer than 255 chars it will be truncated.
procedure StreamWriteShortString(S: TStream; Value: string);

procedure StreamReadByte(S: TStream; var Value: byte);
procedure StreamWriteByte(S: TStream; Value: byte);

procedure StreamReadWord(S: TStream; var Value: word);
procedure StreamWriteWord(S: TStream; Value: word);

procedure StreamReadDword(S: TStream; var Value: dword);
procedure StreamWriteDword(S: TStream; Value: dword);

procedure StreamReadInteger(S: TStream; var Value: longint);
procedure StreamWriteInteger(S: TStream; Value: longint);

procedure StreamReadDouble(S: TStream; var Value: double);
procedure StreamWriteDouble(S: TStream; Value: double);

procedure StreamReadStringList(S: TStream; var Value: TStringList);
procedure StreamWriteStringList(S: TStream; Value: TStringList);

procedure StreamReadBoolean(S: TStream; var Value: boolean);
procedure StreamWriteBoolean(S: TStream; Value: boolean);

procedure StreamReadGuid(S: TStream; var Value: TGUID);
procedure StreamWriteGuid(S: TStream; Value: TGUID);


{function StreamReadLn reads a line from a Stream and returns it in
 the string 'Value' without the $0D$0A (return/newline). If the function
 fails or if EOF it returns false }
function StreamReadLn(S: TStream; var Value: string): boolean;

implementation

//
// Stream reading/writing routines
//

procedure StreamReadString(S: TStream; var Value: string);
var
  Len: integer;
begin
  StreamReadInteger(S, Len);
  SetLength(Value, Len);
  S.ReadBuffer(Value[1], Len);
end;

procedure StreamWriteString(S: TStream; Value: string);
var
  Len: integer;
begin
  Len := length(Value);
  StreamWriteInteger(S, Len);
  S.WriteBuffer(Value[1], Len);
end;

procedure StreamReadShortString(S: TStream; var Value: string);
var
  Len: byte;
begin
  StreamReadByte(S, Len);
  SetLength(Value, Len);
  S.ReadBuffer(Value[1], Len);
end;

procedure StreamWriteShortString(S: TStream; Value: string);
var
  Len: byte;
begin
  // Check length.. do not generate error but truncate to 255 if longer
  if Length(Value) > 255 then
    SetLength(Value, 255);
  Len := length(Value);
  StreamWriteByte(S, Len);
  S.WriteBuffer(Value[1], Len);
end;

procedure StreamReadByte(S: TStream; var Value: byte);
begin
  S.ReadBuffer(Value, SizeOf(byte));
end;

procedure StreamWriteByte(S: TStream; Value: byte);
begin
  S.WriteBuffer(Value, SizeOf(byte));
end;

procedure StreamReadWord(S: TStream; var Value: word);
begin
  S.ReadBuffer(Value, SizeOf(word));
end;

procedure StreamWriteWord(S: TStream; Value: word);
begin
  S.WriteBuffer(Value, SizeOf(word));
end;

procedure StreamReadDword(S: TStream; var Value: dword);
begin
  S.ReadBuffer(Value, SizeOf(dword));
end;

procedure StreamWriteDword(S: TStream; Value: dword);
begin
  S.WriteBuffer(Value, SizeOf(dword));
end;

procedure StreamReadInteger(S: TStream; var Value: longint);
begin
  S.ReadBuffer(Value, SizeOf(longint));
end;

procedure StreamWriteInteger(S: TStream; Value: longint);
begin
  S.WriteBuffer(Value, SizeOf(longint));
end;

procedure StreamReadDouble(S: TStream; var Value: double);
begin
  S.ReadBuffer(Value, SizeOf(double));
end;

procedure StreamWriteDouble(S: TStream; Value: double);
begin
  S.WriteBuffer(Value, SizeOf(double));
end;

procedure StreamReadBoolean(S: TStream; var Value: boolean);
begin
  S.ReadBuffer(Value, SizeOf(boolean));
end;

procedure StreamWriteBoolean(S: TStream; Value: boolean);
begin
  S.WriteBuffer(Value, SizeOf(boolean));
end;

procedure StreamReadGuid(S: TStream; var Value: TGUID);
begin
  S.ReadBuffer(Value, SizeOf(TGUID));
end;

procedure StreamWriteGuid(S: TStream; Value: TGUID);
begin
  S.WriteBuffer(Value, SizeOf(TGUID));
end;





procedure StreamReadStringList(S: TStream; var Value: TStringList);
begin
  Value := TStringList.Create;
  Value.LoadFromStream(S);
end;

procedure StreamWriteStringList(S: TStream; Value: TStringList);
begin
  if not assigned(Value) then
  begin
    Value := TStringList.Create;
    Value.SaveToStream(S);
    Value.Free;
  end else
    Value.SaveToStream(S);
end;

function StreamReadLn(S: TStream; var Value: string): boolean;
var
  Ch: char;
begin
  Value := '';
  Result := false;
  {search sequence #$0D#$0A}
  while S.Read(Ch,1) = 1 do
  begin
    if Ch = #$0D then
    begin
      S.Read(Ch, 1);
      if Ch = #$0A then
      begin
        Result := true;
        exit;
      end else
        Value := Value + #$0D;
    end;
    Value := Value + Ch;
  end;
end;

end.

