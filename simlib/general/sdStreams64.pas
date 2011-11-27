{ sdStreams64.pas

  - Implementation of 64bit streaming. This type of stream does not allow
  writes of blocks that go over 2GB maximum, however the total stream
  is "unlimited" (64bit value for position). The maximum allowed stream size
  will differ between Windows versions. See "FileCreate"

  In analogy to TStream/THandlestream/TFilestream/TCustomMemoryStream/TMemoryStream
  as found in the "Classes" unit.
  
  - TFastMemStream with improved capacity setting

  Author: Nils Haeck M.Sc.
  copyright (c) 2002 - 2010 SimDesign BV (www.simdesign.nl)
}
unit sdStreams64;

{$ifdef lcl}{$MODE Delphi}{$endif}

{$define STREAMEVENTS} // Undefine this to avoid overhead of events

interface

uses
  Classes, SysUtils, Contnrs, Math;

type

  // Event type used to feedback progress during stream copy operations
  TCopyProgressEvent = procedure (Sender: TObject; const Count, Total: Int64) of object;

{ 64bit streams }

  // TStream64 implements most of the commands like TStream, but on 64bit base.
  TStream64 = class(TObject)
  private
    {$ifdef STREAMEVENTS}
    FOnCopyProgress: TCopyProgressEvent;
    {$endif}
    function GetPosition: int64;
    procedure SetPosition(Pos: int64);
    function GetSize: int64;
    {$ifdef STREAMEVENTS}
    procedure DoCopyProgress(const Count, Total: Int64);
    {$endif}
  protected
    procedure SetSize(NewSize: int64); virtual;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: int64; Origin: Word): int64; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TStream64; Count: int64): int64; overload;
    function CopyFrom(Source: TStream; Count: integer): integer; overload;
    function CopyTo(Dest: TStream; Count: integer): integer;
    procedure Shift(OldPos, NewPos: int64; Count: int64);
    property Position: int64 read GetPosition write SetPosition;
    property Size: int64 read GetSize write SetSize;
    {$ifdef STREAMEVENTS}
    property OnCopyProgress: TCopyProgressEvent read FOnCopyProgress write FOnCopyProgress;
    {$endif}
  end;

  // THandleStream64 is a TStream64 descendant analogous to THandleStream
  THandleStream64 = class(TStream64)
  private
    FHandle: Integer;
  protected
    procedure SetSize(NewSize: int64); override;
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: int64; Origin: Word): int64; override;
    property Handle: Integer read FHandle;
  end;

  // TFileStream64 is a TStream64 descendant analogous to TFileStream and can open
  // and save files to disk
  TFileStream64 = class(THandleStream64)
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

  // TCustomMemoryStream64 abstract class serves as a base for TMemoryStream64
  TCustomMemoryStream64 = class(TStream64)
  private
    FMemory: Pointer;
    FSize, FPosition: Longint;
  protected
    procedure SetPointer(Ptr: Pointer; Size: Longint);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: int64; Origin: Word): int64; override;
    procedure SaveToStream(Stream: TStream64);
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
  end;

  // This stream is taken up for compatibility, but since pointers can only
  // hold up to 32 bits of data, this stream can only hold 2Gb.
  // An exception will be raised if a size bigger than 2Gb is set.
  TMemoryStream64 = class(TCustomMemoryStream64)
  private
    FCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream64);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(NewSize: int64); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  // TMapStream64 implements a memory-mapped file stream. Create it with no
  // filename, like this: AMap := TMapStream64.Create; and it will map
  // to the Windows page file
  TMapStream64 = class(TStream64)
  private
    FPages: array of pointer;
    FPageSize: LongWord;
    FPageIndex: integer;
    FPageOffset: integer;
    FSize: int64;
    procedure PageAlloc(Index: integer);
    procedure PageFree(Index: integer);
    function GetPosition: int64;
  protected
    procedure SetSize(NewSize: int64); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: int64; Origin: Word): int64; override;
  end;

  // Use TStream64Adapter to have a TStream64 descendant use a normal TStream
  // stream as its data.
  TStream64Adapter = class(TStream64)
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: int64; Origin: Word): int64; override;
  end;


resourcestring

  shsPageSizeIsZero   = 'Page size is zero';
  shsStreamReadError  = 'Stream read error';
  shsStreamWriteError = 'Stream write error';

implementation

uses
  sdStreams64Platform, RtlConsts;

{ TStream64 }

function TStream64.GetPosition: int64;
begin
  Result := Seek(0, 1);
end;

procedure TStream64.SetPosition(Pos: int64);
begin
  Seek(Pos, 0);
end;

function TStream64.GetSize: int64;
var
  APos: int64;
begin
  APos := Seek(0, 1);
  Result := Seek(0, 2);
  Seek(APos, 0);
end;

procedure TStream64.SetSize(NewSize: int64);
begin
  // default does nothing
end;

procedure TStream64.ReadBuffer(var Buffer; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(shsStreamReadError);
end;

procedure TStream64.WriteBuffer(const Buffer; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(shsStreamWriteError);
end;

function TStream64.CopyFrom(Source: TStream64; Count: int64): int64;
const
  MaxBufSize = $80000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
   {$ifdef STREAMEVENTS}
   DoCopyProgress(0, Result);
   {$endif}
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
      {$ifdef STREAMEVENTS}
      DoCopyProgress(Result - Count, Result);
      {$endif}
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

function TStream64.CopyFrom(Source: TStream; Count: integer): integer;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
   {$ifdef STREAMEVENTS}
   DoCopyProgress(0, Result);
   {$endif}
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
      {$ifdef STREAMEVENTS}
      DoCopyProgress(Result - Count, Result);
      {$endif}
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

function TStream64.CopyTo(Dest: TStream; Count: integer): integer;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Position := 0;
    Count := Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
   {$ifdef STREAMEVENTS}
   DoCopyProgress(0, Result);
   {$endif}
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      ReadBuffer(Buffer^, N);
      Dest.WriteBuffer(Buffer^, N);
      Dec(Count, N);
      {$ifdef STREAMEVENTS}
      DoCopyProgress(Result - Count, Result);
      {$endif}
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

{$ifdef STREAMEVENTS}
procedure TStream64.DoCopyProgress(const Count, Total: Int64);
begin
  if assigned(FOnCopyProgress) then
    FOnCopyProgress(Self, Count, Total);
end;
{$endif}

procedure TStream64.Shift(OldPos, NewPos, Count: int64);
// Shift data from Oldpos to Newpos over Count bytes
const
  MaxBufSize = $80000;
var
  BufSize, N: Integer;
  Buffer: PChar;
  ShiftSize: int64;
begin
  if (OldPos = NewPos) or (Count <= 0) then exit;
  ShiftSize := Count;
  // Determine buffer size
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  // Get buffer
  GetMem(Buffer, BufSize);
  try
   {$ifdef STREAMEVENTS}
   DoCopyProgress(0, ShiftSize);
   {$endif}

    // Find out how to move
    if OldPos > NewPos then begin
      // Start at beginning of stream
      while Count <> 0 do begin
        if Count > BufSize then N := BufSize else N := Count;
        Position := OldPos;
        ReadBuffer(Buffer^, N);
        Position := NewPos;
        WriteBuffer(Buffer^, N);
        Dec(Count, N);
        inc(OldPos, N);
        inc(NewPos, N);
        {$ifdef STREAMEVENTS}
        DoCopyProgress(ShiftSize - Count, ShiftSize);
        {$endif}
      end;

    end else begin
      // Start at end of stream
      while Count <> 0 do begin
        if Count > BufSize then N := BufSize else N := Count;
        Position := OldPos + Count - N;
        ReadBuffer(Buffer^, N);
        Position := NewPos + Count - N;
        WriteBuffer(Buffer^, N);
        Dec(Count, N);
        {$ifdef STREAMEVENTS}
        DoCopyProgress(ShiftSize - Count, ShiftSize);
        {$endif}
      end;

    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

{ THandleStream64 }

constructor THandleStream64.Create(AHandle: Integer);
begin
  inherited Create;
  FHandle := AHandle;
end;

function THandleStream64.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream64.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream64.Seek(Offset: int64; Origin: Word): int64;
begin
  Result := FileSeek(FHandle, Offset, Origin);
end;

procedure THandleStream64.SetSize(NewSize: int64);
begin
  Seek(NewSize, soFromBeginning);
  sdWin32Check(sdSetEndOfFile(FHandle));
end;

{ TFileStream64 }

constructor TFileStream64.Create(const FileName: string; Mode: Word);
begin
  if Mode = fmCreate then
  begin
    inherited Create(FileCreate(FileName));
    if FHandle < 0 then
      raise EFCreateError.CreateResFmt(@SFCreateError, [FileName]);
  end else
  begin
    inherited Create(FileOpen(FileName, Mode));
    if FHandle < 0 then
      raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
  end;
end;

destructor TFileStream64.Destroy;
begin
  if FHandle >= 0 then FileClose(FHandle);
  inherited;
end;

{ TCustomMemoryStream }

procedure TCustomMemoryStream64.SetPointer(Ptr: Pointer; Size: Longint);
begin
  FMemory := Ptr;
  FSize := Size;
end;

function TCustomMemoryStream64.Read(var Buffer; Count: Longint): Longint;
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

function TCustomMemoryStream64.Seek(Offset: int64; Origin: Word): int64;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TCustomMemoryStream64.SaveToStream(Stream: TStream64);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TCustomMemoryStream64.SaveToFile(const FileName: string);
var
  Stream: TStream64;
begin
  Stream := TFileStream64.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TMemoryStream64 }

const
  MemoryDelta = $2000; { Must be a power of 2 }

destructor TMemoryStream64.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMemoryStream64.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMemoryStream64.LoadFromStream(Stream: TStream64);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then
    Stream.ReadBuffer(FMemory^, Count);
end;

procedure TMemoryStream64.LoadFromFile(const FileName: string);
var
  Stream: TStream64;
begin
  Stream := TFileStream64.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemoryStream64.SetCapacity(NewCapacity: Longint);
begin
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

procedure TMemoryStream64.SetSize(NewSize: int64);
var
  OldPosition: Longint;
begin
  if NewSize > $7FFFFFFF then
    raise EStreamError.CreateRes(@SMemoryStreamError);
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soFromEnd);
end;

function TMemoryStream64.Realloc(var NewCapacity: Longint): Pointer;
begin
  if NewCapacity > 0 then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      sdGlobalFreePtr(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
        Result := sdGlobalAllocPtr(NewCapacity)
      else
        Result := sdGlobalReallocPtr(Memory, NewCapacity);
      if Result = nil then
        raise EStreamError.CreateRes(@SMemoryStreamError);
    end;
  end;
end;

function TMemoryStream64.Write(const Buffer; Count: Longint): Longint;
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

{ TMapStream64 }

constructor TMapStream64.Create;
begin
  inherited Create;
  FPageSize := sdMap_Create;
  if FPageSize <= 0 then
    raise Exception.Create(shsPageSizeIsZero);
end;

destructor TMapStream64.Destroy;
var
  i: integer;
begin
  for i := 0 to length(FPages) - 1 do
    PageFree(i);
  inherited;
end;

function TMapStream64.GetPosition: int64;
begin
  Result := FPageIndex * integer(FPageSize) + FPageOffset;
end;

procedure TMapStream64.PageAlloc(Index: integer);
begin
  // Virtual allocate
  FPages[Index] := sdVirtualAlloc(
    nil,           // contains nil so we will get a location allocated
    FPageSize      // We will open one page
  );
{$ifdef lcl}
{$else}
  if not assigned(FPages[Index]) then
    raise Exception.Create(SysErrorMessage(GetLastError));
{$endif}
end;

procedure TMapStream64.PageFree(Index: integer);
var
  Res: boolean;
begin
  Res := sdVirtualFree(FPages[Index], 0);
{$ifdef lcl}
{$else}
  if Res = False then
    raise Exception.Create(SysErrorMessage(GetLastError));
{$endif}
end;

function TMapStream64.Read(var Buffer; Count: Integer): Longint;
var
  NewPosition: int64;
  S, D: PByte;
  PacketCount: integer;
begin
  NewPosition := GetPosition + Count;
  if NewPosition > FSize then
    Count := FSize - GetPosition;
  Result := Count;
  D := @Buffer;
  while Count > 0 do
  begin
    if FPageOffset = integer(FPageSize) then
    begin
      inc(FPageIndex);
      FPageOffset := 0;
    end;
    PacketCount := Min(Count, integer(FPageSize) - FPageOffset);
    S := @PByteArray(FPages[FPageIndex])[FPageOffset];
    Move(S^, D^, PacketCount);
    dec(Count, PacketCount);
    inc(D, PacketCount);
    inc(FPageOffset, PacketCount);
  end;
end;

function TMapStream64.Seek(Offset: int64; Origin: Word): int64;
begin
  case Origin of
  soFromBeginning: Result := Offset;
  soFromCurrent:   Result := GetPosition + Offset;
  soFromEnd:       Result := FSize + Offset;
  else
    Result := 0;// avoid compiler warning
  end;//case
  if Result > FSize then SetSize(Result);
  // New page index and offset
  FPageIndex  := Result div FPageSize;
  FPageOffset := Result mod FPageSize;
end;

procedure TMapStream64.SetSize(NewSize: int64);
var
  i, PageCount, OldCount: integer;
begin
  if NewSize = FSize then
    exit;
  PageCount := (NewSize + FPageSize - 1) div FPageSize;
  OldCount := length(FPages);
  if PageCount > OldCount then
  begin
    SetLength(FPages, PageCount);
    for i := OldCount to PageCount - 1 do
      PageAlloc(i);
  end;
  if PageCount < OldCount then
  begin
    for i := OldCount - 1 downto PageCount do
      PageFree(i);
    SetLength(FPages, PageCount);
  end;
  FSize := NewSize;
  if FSize < GetPosition then
  begin
    FPageIndex  := FSize div FPageSize;
    FPageOffset := FSize mod FPageSize;
  end;
end;

function TMapStream64.Write(const Buffer; Count: Integer): Longint;
var
  NewPosition: int64;
  S, D: PByte;
  PacketCount: integer;
begin
  NewPosition := GetPosition + Count;
  if NewPosition > FSize then
    SetSize(NewPosition);
  Result := Count;
  S := @Buffer;
  while Count > 0 do
  begin
    if FPageOffset = integer(FPageSize) then
    begin
      inc(FPageIndex);
      FPageOffset := 0;
    end;
    PacketCount := Min(Count, integer(FPageSize) - FPageOffset);
    D := @PByteArray(FPages[FPageIndex])[FPageOffset];
    Move(S^, D^, PacketCount);
    dec(Count, PacketCount);
    inc(S, PacketCount);
    inc(FPageOffset, PacketCount);
  end;
end;

{ TStream64Adapter }

constructor TStream64Adapter.Create(AStream: TStream);
begin
  inherited Create;
  // Create a reference to the stream
  FStream := AStream;
end;

function TStream64Adapter.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TStream64Adapter.Seek(Offset: int64; Origin: Word): int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TStream64Adapter.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

end.
