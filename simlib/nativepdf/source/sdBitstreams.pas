{ unit sdBitstreams

  Copyright (c) 2005 By Nils Haeck M.Sc. - SimDesign
  More information: www.simdesign.nl or n.haeck@simdesign.nl

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.
}
unit sdBitstreams;

interface

uses
  Classes, SysUtils;

type
  PCardinal = ^Cardinal;

  // A stream reader that accepts bits (0, 1)
  TBitStreamReader = class(TPersistent)
  private
    FCurrent: cardinal;
    FBitPos: integer;
    FStream: TStream;
  public
    constructor Create(AStream: TStream); virtual;
    function ReadBit: boolean;
    function ReadByte: byte;
    function ReadInt(BitCount: integer): integer;
  end;

  // A stream that accepts bits (0, 1)
  TBitStreamWriter = class(TPersistent)
  private
    FBuffer: pointer;
    FBufferSize: integer;
    FCurrent: pcardinal;
    FLeftCount: integer;
    FBitPos: integer;
    FStream: TStream;
    function GetPosition: integer;
    function GetBitPosition: int64;
    procedure SetBitPosition(const Value: int64);
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
    procedure Flush;
    procedure WriteByte(AByte: byte);
    procedure WriteBit(const ABit: boolean);
    procedure WriteBits(Count: integer; Value: boolean);
    procedure WriteInt(AInt: integer; BitCount: integer);
    property Position: integer read GetPosition;
    property BitPosition: int64 read GetBitPosition write SetBitPosition;
  end;

implementation

const
  // Number of cardinals in bitbuffer
  cBitStreamBufferSize = 2048;

{ TBitStreamReader }

constructor TBitStreamReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TBitStreamReader.ReadBit: boolean;
begin
  if FBitPos = 0 then
    FStream.Read(FCurrent, 4);
  Result := (FCurrent and $80000000) <> 0;
  FCurrent := FCurrent shl 1;
  inc(FBitpos);
  if FBitpos = 32 then FBitpos := 0;
end;

function TBitStreamReader.ReadByte: byte;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 7 do begin
    Result := Result shl 1;
    if ReadBit then inc(Result);
  end;
end;

function TBitStreamReader.ReadInt(BitCount: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to BitCount - 1 do begin
    Result := Result shl 1;
    if ReadBit then inc(Result);
  end;
end;

{ TBitStreamWriter }

constructor TBitStreamWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBufferSize := cBitStreamBufferSize;
  GetMem(FBuffer, FBufferSize * SizeOf(cardinal));
  FCurrent := FBuffer;
  FLeftCount := FBufferSize;
end;

destructor TBitStreamWriter.Destroy;
begin
  Flush;
  FreeMem(FBuffer);
  inherited;
end;

procedure TBitStreamWriter.Flush;
begin
  // Flush the last cardinal
  if FBitPos > 0 then begin
    FCurrent^ := FCurrent^ shl (32 - FBitPos);
    FBitPos := 0;
    dec(FLeftCount);
  end;
  // Flush the buffer to the stream
  FStream.Write(FBuffer^, (FBufferSize - FLeftCount) * SizeOf(cardinal));
  FCurrent := FBuffer;
  FLeftCount := FBufferSize;
end;

function TBitStreamWriter.GetBitPosition: int64;
begin
  Result := FStream.Position * 8 + (FBufferSize - FLeftcount) * 32 + FBitPos;
end;

function TBitStreamWriter.GetPosition: integer;
begin
  Result := FStream.Position + (FBufferSize - FLeftcount) * 4 + (FBitPos div 8);
end;

procedure TBitStreamWriter.SetBitPosition(const Value: int64);
var
  ABitPosition: int64;
  AFloor: integer;
begin
  // Checks
  ABitPosition := GetBitPosition;
  if Value > ABitPosition then
    raise Exception.Create('Cannot set bit position further than current');
  if Value < 0 then
    raise Exception.Create('Cannot set negative bit position');
  if Value = ABitPosition then exit;

  // Start by setting the floored position
  AFloor := (Value div 32) * 4;
  if AFloor < FStream.Position then begin
    FStream.Position := AFloor;
    FCurrent := FBuffer;
    FStream.Read(FCurrent^, 4);
    FStream.Position := AFloor;
    FStream.Size := FStream.Position;
    FLeftCount := FBufferSize;
  end else begin
    FCurrent := FBuffer;
    inc(FCurrent, (AFloor - FStream.Position) div 4);
    FLeftCount := FBufferSize + FStream.Position - AFloor;
  end;

  // Set the final bitshift
  FBitPos := Value - AFloor * 8;
  FCurrent^ := FCurrent^ shr (32 - FBitPos);
end;

procedure TBitStreamWriter.WriteBit(const ABit: boolean);
begin
  FCurrent^ := FCurrent^ shl 1;
  if ABit then inc(FCurrent^);
  inc(FBitPos);
  if FBitPos = 32 then begin
    // We now have a full cardinal, flush it
    inc(FCurrent);
    dec(FLeftCount);
    FBitPos := 0;
    if FLeftCount = 0 then Flush;
  end;
end;

procedure TBitStreamWriter.WriteBits(Count: integer; Value: boolean);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    WriteBit(Value);
end;

procedure TBitStreamWriter.WriteByte(AByte: byte);
var
  i: integer;
begin
  for i := 0 to 7 do begin
    WriteBit((AByte and $80) <> 0);
    AByte := AByte shl 1;
  end;
end;

procedure TBitStreamWriter.WriteInt(AInt, BitCount: integer);
var
  i: integer;
begin
  // write Bitcount bits of AInt to the stream
  AInt := AInt shl (32 - BitCount);
  for i := 0 to BitCount - 1 do begin
    WriteBit((AInt and $80000000) <> 0);
    AInt := AInt shl 1;
  end;
end;

end.
