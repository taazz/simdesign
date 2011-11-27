{ unit sdStorage

  Storage format for backend storage of multiple streams/blobs in one file

  TsdStorage is a non-visual component. It can aid other components in providing
  an easy method to deal with huge amounts of data.

  The TsdStorage component can contain a number of streams. Streams are identified
  by their name (string). Creating a storage from scratch will allow it to contain
  up to cDefaultMemoryCache bytes of data, after that a temporary file will be
  created.

  With LoadFromFile, the storage will reference the data in the file. Any newly
  added streams will be put in the temporary memory. The file must remain accessible
  throughout the session.

  With WorkOnFile, the storage works directly with the given file, thus it cannot
  be a read-only file, and the file must remain accessible throughout the session.
  There is no need for a call to SaveToFile, data is saved automatically.

  With LoadCopyFromFile, the storage will copy all data from the file directly
  to the temporary memory. The file is closed directly after.

  With SaveToFile, the storage will save all data to the file, and closes it
  directly after. If this is the same file as the one used with LoadFromFile,
  the referenced data needs to be copied temporarily to the temporary
  memory, so that the file can be closed and opened in write mode.

  With SaveToStream, the storage will save all data to the stream. The stream
  can be closed directly after.

  Use StreamRead to read a stream from the storage, StreamWrite to write (add or
  update) a stream to the storage, or StreamDelete to delete a stream from the
  storage. Check if a stream is present with StreamExists.

  TsdStorage is threadsafe, it can be safely used from multiple threads due to
  internal locking of the public methods and properties.

  When used with Delphi7 and up, TsdStorage can handle files of any length, for
  lower versions the storage limit is 2Gb.

  Modifications:
  05Aug2006: Fixed bug that caused index to think streams were external
  29May2011: Fixed bug in function TempFileName (caused by unicode XE)

  Copyright (c) 2005 - 2011 by Nils Haeck, SimDesign B.V.

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.
  
  projects:
  - DtpDocuments (default extension: *.dtp)

}
unit sdStorage;

{.$i SimDesign.inc}

interface

uses
  Windows, Classes, SysUtils, SyncObjs;

const

  // Magick text appearing at start of archive
  cStoMagick: array[0..3] of AnsiChar = AnsiString('STOR');


  // Version numbers
  cStoVersionMajor: byte = 1;
  cStoVersionMinor: byte = 1;

  // Default memory cache: if the storage grows beyond this number, a temp
  // file is created to hold it.
  cDefaultMemoryCache = 10 * 1024 * 1024; // 10 Mb memory cache

type

  // An index item used internally by TsdStorage
  TsdIndexItem = record
    FName: Utf8String;
    FExternal: boolean;
    FStart: int64;
    FSize: int64;
  end;

  // The TsdStorage component can contain a number of streams. Streams are identified
  // by their name (string). Creating a storage from scratch will allow it to contain
  // up to cDefaultMemoryCache bytes of data, after that a temporary file will be
  // created.
  TsdStorage = class(TComponent)
  private
    FIsBusy: boolean;
    FAutoSave: boolean;
    FLock: TCriticalSection;
    FMemoryCache: integer;
    FSource: TStream;
    FSourceFileName: Utf8String;
    FSourceIsTemp: boolean; // True if FSource is not the stream specified by user
    FExtern: TStream;
    FExternFileName: Utf8String;
    FItems: array of TsdIndexItem;
    FBuffer: array of byte; // buffer used in copying methods
    function GetFileName: Utf8String;
    procedure SetAutoSave(const Value: boolean);
    procedure SetFileName(const Value: Utf8String);
    procedure SetMemoryCache(const Value: integer);
    function GetStreamCount: integer;
    function GetStreamNames(Index: integer): Utf8String;
  protected
    procedure Lock;
    procedure Unlock;
    procedure ClearStorage;
    procedure ReadHeaderAndIndex(S: TStream);
    procedure WriteHeader;
    procedure WriteIndex;
    procedure SetSourceSize(const ASize: int64);
    procedure CopyStream(Source, Dest: TStream);
    procedure CopyStreamFrom(Source: TStream; Start, Size: int64; Dest: TStream);
    procedure CopyStreamFromTo(Source: TStream; Start, Size: int64; Dest: TStream; DestStart: int64);
    procedure CopyStreamTo(Source, Dest: TStream; Start: int64);
    procedure CopyBlock(S: TStream; OldStart, NewStart, Size: int64);
    function StreamIndexByName(const AName: Utf8String): integer;
    function FirstStreamPosition(var Index: integer): int64;
    function LastStreamPosition: int64;
    function FindSlot(const ASize: int64): int64;
    function CalculateIndexSize: integer;
    procedure DisconnectFromExternal;
    procedure InternalSaveToStream(S: TStream);
    procedure CleanupTempFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Load the storage from FileName. Filename will be opened with read rights,
    // and will remain opened during the life of TsdStorage. Multiple TsdStorage
    // components can open the same file. For slow access media (CD-rom) it is
    // recommended to use LoadCopyFromFile instead. Initial loading is very fast.
    procedure LoadFromFile(const FileName: string);
    // Load the storage from Filename. After loading, the file will be closed
    // again, since only a copy of it is created. Initial loading is slow, because
    // the whole contents (all streams) will be copied to the temp storage.
    procedure LoadCopyFromFile(const FileName: string);
    // Load the storage from stream S. A local copy will be created, so stream
    // S can be safely closed after execution of this method.
    procedure LoadCopyFromStream(S: TStream);
    // Load the storage from FileName, and work directly on it. The file will be
    // opened with read/write access, so no other storage component can open it.
    // Also, this method will raise an exception if the file cannot be opened
    // in read/write mode. If FileName is a non-existant file, it will be created.
    // Note that the storage does not need to be saved, any changes made to the
    // storage are saved instantly.
    procedure WorkOnFile(const FileName: string);
    // Save the storage to FileName. The storage's internal stream will
    // remain unchanged, and just a copy is saved to FileName. The file is closed
    // at the end of execution of this method. If the file already exists, it will
    // be overwritten. If the file cannot be created, an exception is raised.
    procedure SaveToFile(const FileName: string);
    // Save the storage to stream S.
    procedure SaveToStream(S: TStream);
    // Defragment will align all the blocks so that there are no longer any gaps
    // inbetween. You do not need to use this method after any of the SaveTo..
    // commands, but it might be meaningful (at the very end) after WorkOnFile
    // and any additions/deletions/updates of streams.
    procedure Defragment;
    // Read content of stream AName from the storage into S. If AName does not
    // exist, an exception will be raised.
    procedure StreamRead(const AName: Utf8String; S: TStream);
    // Write stream AName with content S to the storage. If AName already exists
    // it will be overwritten.
    procedure StreamWrite(const AName: Utf8String; S: TStream);
    // Delete stream AName from the storage.
    procedure StreamDelete(const AName: Utf8String);
    // Check if stream AName exists in the storage.
    function StreamExists(const AName: Utf8String): boolean;
    // Rename the stream with OldName to NewName
    procedure StreamRename(const OldName, NewName: Utf8String);
    // Clear all the streams from the storage.
    procedure Clear;
    // Use this function to get a unique stream name
    function GetUniqueStreamName(const Extension: Utf8String = '.dat'): Utf8String;
    class function StreamIsUnique(const AName: Utf8String): boolean;
    // Use StreamNames to obtain the name of the stream at Index
    property StreamNames[Index: integer]: Utf8String read GetStreamNames;
  published
    // Set AutoSave to True to automatically save changes to the filename specified
    // in FileName.
    property AutoSave: boolean read FAutoSave write SetAutoSave default False;
    // Set filename to a file containing a storage archive. If the filename is
    // not empty, the file will be loaded. If AutoSave is True, the file will
    // be loaded and any changes will be automatically saved.
    property FileName: Utf8String read GetFileName write SetFileName;
    // Set MemoryCache to the size in bytes that are used at most to store the
    // archive in memory. If the archive size becomes bigger than this number,
    // it will be automatically saved to a temporary file.
    property MemoryCache: integer read FMemoryCache write SetMemoryCache default cDefaultMemoryCache;
    // StreamCount returns the number of streams in the storage.
    property StreamCount: integer read GetStreamCount;
  end;

resourcestring

  sstStreamNotPresent       = 'Stream "%s" not found in the storage';
  sstWarningRecursive       = 'Warning: recursive call to sdStorage from event';
  sstBlocksOverlap          = 'Blocks overlap in copying action';
  sstFileIsNotStorage       = 'File is not a storage file';
  sstFileIsWrongVersion     = 'File is wrong (future) version';
  sstUnableToCreateTempFile = 'Unable to create temporary file';

implementation

const
  cBufferLength = $F000;

procedure ReadUTF8String(S: TStream; var Value: Utf8String);
var
  Count: integer;
begin
  S.Read(Count, SizeOf(Count));
  SetLength(Value, Count);
  if Count > 0 then
    S.Read(Value[1], Count);
end;


procedure WriteUTF8String(S: TStream; const Value: Utf8String);
var
  Count: integer;
begin
  Count := length(Value);
  S.Write(Count, SizeOf(Count));
  if Count > 0 then
    S.Write(Value[1], Count);
end;


function TempFileName: AnsiString;
// Return a unique, fully qualified temporary filename
var
  Buf: array[0..MAX_PATH] of AnsiChar;
  TempPath: AnsiString;
begin
  GetTempPath(MAX_PATH, @Buf);
  TempPath := Buf;
{$ifdef UNICODE}
  if GetTempFileNameA(PAnsiChar(TempPath), '~TMP', 0, Buf) > 0 then
{$else UNICODE}
  if GetTempFileName(PAnsiChar(TempPath), '~TMP', 0, Buf) > 0 then
{$endif UNICODE}
    Result := Buf
  else
    raise Exception.Create(sstUnableToCreateTempfile);
end;


{ TsdStorage }

function TsdStorage.CalculateIndexSize: integer;
var
  i: integer;
begin
  Result := 0;

  // size of pointer to our start for backward comp
  inc(Result, SizeOf(int64));

  // Count of items
  inc(Result, SizeOf(integer));

  // item sizes
  for i := 0 to length(FItems) - 1 do
    // 2x int64 for start, size, 1x integer for length of string + length of string
    inc(Result, 2 * SizeOf(int64) + SizeOf(integer) + Length(FItems[i].FName) );
end;

procedure TsdStorage.CleanupTempFile;
begin
  if FSourceIsTemp and (Pos('.tmp', lowercase(FSourceFileName)) > 0) and FileExists(FSourceFileName) then
    DeleteFile(FSourceFileName);
end;

procedure TsdStorage.Clear;
begin
  Lock;
  try
    ClearStorage;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.ClearStorage;
begin
  FreeAndNil(FSource);
  // Remove any temporary file
  CleanupTempFile;
  FSourceFileName := '';
  FSourceIsTemp := True;
  FreeAndNil(FExtern);
  FExternFileName := '';
  // This clears the index
  SetLength(FItems, 0);
  // Create the source
  FSource := TMemoryStream.Create;
end;

procedure TsdStorage.CopyBlock(S: TStream; OldStart, NewStart,
  Size: int64);
var
  OldPos, NewPos: int64;
  N: integer;
begin
  // Check overlap - should not happen
  if (OldStart < NewStart) and (OldStart + Size > NewStart) then
    raise Exception.Create(sstBlocksOverlap);
  OldPos := OldStart;
  NewPos := NewStart;

  // Copy bufferwise
  while Size > 0 do
  begin
    S.Seek(OldPos, soFromBeginning);
    if Size > cBufferLength then
      N := cBufferLength
    else
      N := Size;
    S.ReadBuffer(FBuffer[0], N);
    S.Seek(NewPos, soFromBeginning);
    S.WriteBuffer(FBuffer[0], N);
    inc(OldPos, N);
    inc(NewPos, N);
    dec(Size, N);
  end;
end;

procedure TsdStorage.CopyStream(Source, Dest: TStream);
var
  Size: int64;
  N: integer;
begin
  Size := Source.Size;
  Dest.Size := Size;
  Source.Seek(0, soFromBeginning);
  Dest.Seek(0, soFromBeginning);
  // Copy bufferwise
  while Size > 0 do
  begin
    if Size > cBufferLength then
      N := cBufferLength
    else
      N := Size;
    Source.ReadBuffer(FBuffer[0], N);
    Dest.WriteBuffer(FBuffer[0], N);
    dec(Size, N);
  end;
  Dest.Seek(0, soFromBeginning);
end;

procedure TsdStorage.CopyStreamFrom(Source: TStream; Start, Size: int64;
  Dest: TStream);
var
  N: integer;
begin
  Dest.Size := Size;
  Source.Seek(Start, soFromBeginning);
  Dest.Seek(0, soFromBeginning);
  // Copy bufferwise
  while Size > 0 do
  begin
    if Size > cBufferLength then
      N := cBufferLength
    else
      N := Size;
    Source.ReadBuffer(FBuffer[0], N);
    Dest.WriteBuffer(FBuffer[0], N);
    dec(Size, N);
  end;
  Dest.Seek(0, soFromBeginning);
end;

procedure TsdStorage.CopyStreamFromTo(Source: TStream; Start, Size: int64;
  Dest: TStream; DestStart: int64);
var
  N: integer;
begin
  Source.Seek(0, soFromBeginning);
  Source.Seek(Start, soFromBeginning);
  Dest.Seek(DestStart, soFromBeginning);
  // Copy bufferwise
  while Size > 0 do
  begin
    if Size > cBufferLength then
      N := cBufferLength
    else
      N := Size;
    Source.ReadBuffer(FBuffer[0], N);
    Dest.WriteBuffer(FBuffer[0], N);
    dec(Size, N);
  end;
end;

procedure TsdStorage.CopyStreamTo(Source, Dest: TStream; Start: int64);
var
  Size: int64;
  N: integer;
begin
  Size := Source.Size;
  Source.Seek(0, soFromBeginning);
  Dest.Seek(Start, soFromBeginning);
  // Copy bufferwise
  while Size > 0 do
  begin
    if Size > cBufferLength then
      N := cBufferLength
    else
      N := Size;
    Source.ReadBuffer(FBuffer[0], N);
    Dest.WriteBuffer(FBuffer[0], N);
    dec(Size, N);
  end;
end;

constructor TsdStorage.Create(AOwner: TComponent);
begin
  inherited;
  FMemoryCache := cDefaultMemoryCache;
  SetLength(FBuffer, cBufferLength);
  FLock := TCriticalSection.Create;
  ClearStorage;
end;

procedure TsdStorage.Defragment;
begin
  Lock;
  try
    // to do
  finally
    Unlock;
  end;
end;

destructor TsdStorage.Destroy;
begin
  CleanupTempFile;
  FreeAndNil(FLock);
  FreeAndNil(FSource);
  FreeAndNil(FExtern);
  inherited;
end;

procedure TsdStorage.DisconnectFromExternal;
var
  i: integer;
  Start: int64;
  M: TMemoryStream;
begin
  // We must copy all streams that are external to the source
  for i := 0 to length(FItems) - 1 do
    if FItems[i].FExternal then
    begin
      M := TMemoryStream.Create;
      try

        // Load from external
        CopyStreamFrom(FExtern, FItems[i].FStart, FItems[i].FSize, M);

        // We try to add it in a slot
        Start := FindSlot(M.Size);
        // Check and copy stream
        if Start + M.Size > LastStreamPosition then
          SetSourceSize(Start + M.Size);
        CopyStreamTo(M, FSource, Start);

        // Update index
        FItems[i].FExternal := False;
        FItems[i].FStart := Start;

      finally
        M.Free;
      end;
    end;

  // Save index
  WriteIndex;

  // Now we can disconnect
  FreeAndNil(FExtern);
  FExternFileName := '';
end;

function TsdStorage.FindSlot(const ASize: int64): int64;
var
  i, j, Temp, Count: integer;
  Sorted: array of integer;
begin
  // Create a sorted list of start positions for non-external items
  SetLength(Sorted, length(FItems));
  Count := 0;
  for i := 0 to length(FItems) - 1 do
    if not FItems[i].FExternal then
    begin
      Sorted[Count] := i;
      inc(Count);
    end;
  for i := 0 to Count - 2 do
    for j := i + 1 to Count - 1 do
      if FItems[Sorted[i]].FStart > FItems[Sorted[j]].FStart then
      begin
        Temp := Sorted[i];
        Sorted[i] := Sorted[j];
        Sorted[j] := Temp;
      end;

  if Count = 0 then
  begin
    // No non-external items? In that case, the slot is at a fixed offset leaving
    // room for our index to grow a bit
    Result := CalculateIndexSize + 512;
    exit;
  end;

  // Worst case: after last item
  with FItems[Sorted[Count - 1]] do
    Result :=  FStart + FSize;

  // Try to find a gap
  for i := 0 to Count - 2 do
    // Is the gap big enough?
    if FItems[Sorted[i + 1]].FStart - FItems[Sorted[i]].FStart - FItems[Sorted[i]].FSize >= ASize then
    begin
      // Right after this item
      Result := FItems[Sorted[i]].FStart + FItems[Sorted[i]].FSize;
      exit;
    end;
end;

function TsdStorage.FirstStreamPosition(var Index: integer): int64;
// Check all non-external items and decide which one is first, and return
// its start position
var
  i: integer;
begin
  Index := -1;
  Result := 0;
  for i := 0 to length(FItems) - 1 do
    if not FItems[i].FExternal then
    begin
      if Index = -1 then
      begin
        Index := i;
        Result := FItems[i].FStart;
      end else
      begin
        if Result > FItems[i].FStart then
        begin
          Index := i;
          Result := FItems[i].FStart;
        end;
      end;
    end;
end;

function TsdStorage.GetFileName: Utf8String;
begin
  if FSourceIsTemp then
    Result := FExternFileName
  else
    Result := FSourceFileName;
end;

function TsdStorage.GetStreamCount: integer;
begin
  Lock;
  try
    Result := length(FItems);
  finally
    Unlock;
  end;
end;

function TsdStorage.GetStreamNames(Index: integer): Utf8string;
begin
  Lock;
  try
    if (Index >= 0) and (Index < length(FItems)) then
      Result := FItems[Index].FName
    else
      Result := '';
  finally
    Unlock;
  end;
end;

function TsdStorage.GetUniqueStreamName(const Extension: Utf8String): Utf8String;
var
  i: integer;
  Res: AnsiString;
begin
  Lock;
  try
    repeat
      Res := 'TMP';
      // 7 digits amounts to 10.000.000 possibilities, for 1Mio items this means
      // a hit chance of 10%. This component isn't suitable for very high
      // numbers of items currently anyway, so that is a safe limitation.
      for i := 0 to 6 do
        Res := Res + AnsiChar(Ord('0') + Random(10));
      Result := Utf8String(Res) + Extension;
    until StreamIndexByName(Result) = -1;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.InternalSaveToStream(S: TStream);
var
  i, Count: integer;
  FTempIndex: array of TsdIndexItem;
  VPos, IndexStart: int64;
const
  cMessage = Utf8String(#13#10 + 'Created with dtpDocuments - Info: www.simdesign.nl' + #13#10#0);
begin
  // A temp index array for the stream
  Count := length(FItems);
  SetLength(FTempIndex, Count);

  // Virtual position
  VPos := 6 + CalculateIndexSize + length(cMessage);
  // Loop through temp index and calculate virtual positions
  for i := 0 to Count - 1 do
  begin
    FTempIndex[i].FStart := VPos;
    FTempIndex[i].FSize := FItems[i].FSize;
    inc(VPos, FItems[i].FSize);
  end;

  // The header is 6 bytes long
  S.Write(cStoMagick, SizeOf(cStoMagick));
  S.Write(cStoVersionMajor, 1);
  S.Write(cStoVersionMinor, 1);

  // Write the index, with a pointer to our start for backward comp
  IndexStart := (Length(cStoMagick) + 2 + SizeOf(int64) + Length(cMessage));
  S.Write(IndexStart, SizeOf(IndexStart));
  // Write an info message
  S.Write(cMessage[1], length(cMessage));

  // Write count
  S.Write(Count, SizeOf(integer));

  // Write all items
  for i := 0 to Count - 1 do
  begin
    S.Write(FTempIndex[i].FStart, SizeOf(int64));
    S.Write(FTempIndex[i].FSize, SizeOf(int64));
    WriteUTF8String(S, FItems[i].FName);
  end;

  // Now read from streams and write to S
  for i := 0 to Count - 1 do
    if FItems[i].FExternal then
      CopyStreamFromTo(FExtern, FItems[i].FStart, FItems[i].FSize, S, FTempIndex[i].FStart)
    else
      CopyStreamFromTo(FSource, FItems[i].FStart, FItems[i].FSize, S, FTempIndex[i].FStart);

end;

function TsdStorage.LastStreamPosition: int64;
// Check all non-external items and decide which one is last, and return
// the next free position
var
  i: integer;
  EndPos: int64;
begin
  Result := 6;
  for i := 0 to length(FItems) - 1 do
    if not FItems[i].FExternal then
    begin
      EndPos := FItems[i].FStart + FItems[i].FSize;
      if Result < EndPos then
        Result := EndPos;
    end;
end;

procedure TsdStorage.LoadCopyFromFile(const FileName: string);
var
  F: TFileStream;
begin
  Lock;
  try
    ClearStorage;
    if not FileExists(FileName) then
      exit;
    try
      F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      try
        SetSourceSize(F.Size);
        CopyStream(F, FSource);
        ReadHeaderAndIndex(FSource);
      finally
        F.Free;
      end;
    except
      ClearStorage;
      raise;
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.LoadCopyFromStream(S: TStream);
begin
  Lock;
  try
    ClearStorage;
    try
      SetSourceSize(S.Size);
      CopyStream(S, FSource);
      ReadHeaderAndIndex(FSource);
    except
      ClearStorage;
      raise;
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.LoadFromFile(const FileName: string);
begin
  Lock;
  try
    ClearStorage;
    if not FileExists(FileName) then
      exit;
    try
      FExtern := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      FExternFileName := Utf8String(FileName);
      ReadHeaderAndIndex(FExtern);
      WriteHeader;
      WriteIndex;
    except
      ClearStorage;
      raise;
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.Lock;
begin
  FLock.Enter;
  if FIsBusy then
    raise Exception.Create(sstWarningRecursive);
  FIsBusy := True;
end;

procedure TsdStorage.ReadHeaderAndIndex(S: TStream);
// Read header and index from an external stream
var
  i, Count: integer;
  AMagick: array[0..3] of ansichar;
  AVersionMajor, AVersionMinor: byte;
  IndexStart: int64;
begin
  // Read the header
  S.Seek(0, soFromBeginning);
  S.Read(AMagick, SizeOf(cStoMagick));
  // The magick signature matches?
  if AMagick <> cStoMagick then
    raise Exception.Create(sstFileIsNotStorage);
  S.Read(AVersionMajor, 1);
  // We do not accept a future version major
  if AVersionMajor > cStoVersionMajor then
    raise Exception.Create(sstFileIsWrongVersion);
  // We do accept any version minor though
  S.Read(AVersionMinor, 1);

  // Read the index pointer
  S.Read(IndexStart, SizeOf(IndexStart));
  S.Seek(IndexStart, soFromBeginning);

  // Read index count
  S.Read(Count, SizeOf(integer));
  SetLength(FItems, Count);

  // Read index items
  for i := 0 to Count - 1 do
  begin
    FItems[i].FExternal := (S <> FSource);
    S.Read(FItems[i].FStart, SizeOf(int64));
    S.Read(FItems[i].FSize, SizeOf(int64));
    ReadUtf8String(S, FItems[i].FName);
  end;
end;

procedure TsdStorage.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  Lock;
  try
    // Check if the source filename perhaps equals the save filename, e.g. we
    // are working on the file
    if (length(FSourceFileName) > 0) and
      (AnsiCompareText(ExpandFileName(FSourceFileName), ExpandFileName(FileName)) = 0) then
      // in that case we can exit; it is already saved
      exit;

    // Check if the external filename perhaps equals the save filename, e.g. we
    // are referencing the file
    if (length(FExternFileName) > 0) and
      (AnsiCompareText(ExpandFileName(FExternFileName), ExpandFileName(FileName)) = 0) then
      // We must first disconnect from the file we loaded
      DisconnectFromExternal;

    // We can now create the file
    F := TFileStream.Create(FileName, fmCreate);
    try
      // And we can save everything to it
      InternalSaveToStream(F);
    finally
      F.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.SaveToStream(S: TStream);
begin
  Lock;
  try
    // no checks neccessary here
    InternalSaveToStream(S);
  finally
    Unlock;
  end;
end;

procedure TsdStorage.SetAutoSave(const Value: boolean);
begin
  if FAutoSave <> Value then
  begin
    FAutoSave := Value;
    if length(GetFileName) > 0 then
    begin
      if FAutoSave then
        WorkOnFile(string(GetFileName))
      else
        LoadFromFile(string(GetFileName));
    end;
  end;
end;

procedure TsdStorage.SetFileName(const Value: Utf8String);
begin
  if GetFileName <> Value then
  begin
    if FAutoSave then
      WorkOnFile(string(Value))
    else
      LoadFromFile(string(Value));
  end;
end;

procedure TsdStorage.SetMemoryCache(const Value: integer);
begin
  if FMemoryCache <> Value then
  begin
    Lock;
    try
      FMemoryCache := Value;
      SetSourceSize(LastStreamPosition);
    finally
      Unlock;
    end;
  end;
end;

procedure TsdStorage.SetSourceSize(const ASize: int64);
// Here we check if we must update the source size, and if so, if we must perhaps
// create the temp file
var
  F: TFileStream;
begin
  // Equal streams: do nothing
  if ASize = FSource.Size then
    exit;

  // Smaller: just set new size
  if ASize < FSource.Size then
  begin
    FSource.Size := ASize;
    exit;
  end;

  // Do we need to create a temp file?
  if (length(FSourceFileName) = 0) and (ASize > FMemoryCache) then
  begin
    // Create a temp file
    F := nil;
    try
      FSourceFileName := Utf8String(TempFileName);
      F := TFileStream.Create(string(FSourceFileName), fmCreate);
      CopyStream(FSource, F);
      FreeAndNil(FSource);
      FSource := F;
    except
      F.Free;
      ClearStorage;
      raise Exception.Create(sstUnableToCreateTempFile);
    end;
  end;

  // Update the source size
  FSource.Size := ASize;
end;

procedure TsdStorage.StreamDelete(const AName: Utf8String);
var
  Idx, i: integer;
begin
  Lock;
  try
    // Find the stream..
    Idx := StreamIndexByName(AName);

    // Not present? Just exit
    if Idx < 0 then
      exit;

    // Remove element from index array
    for i := Idx to length(FItems) - 2 do
      FItems[i] := FItems[i + 1];
    SetLength(FItems, length(FItems) - 1);
    // Reduce source size if this was last element
    SetSourceSize(LastStreamPosition);

    // Save index
    WriteIndex;

  finally
    Unlock;
  end;
end;

function TsdStorage.StreamExists(const AName: Utf8String): boolean;
begin
  Lock;
  try
    Result := StreamIndexByName(AName) >= 0;
  finally
    Unlock;
  end;
end;

function TsdStorage.StreamIndexByName(const AName: Utf8String): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to length(FItems) - 1 do
    if AnsiCompareText(AnsiString(FItems[i].FName), AnsiString(AName)) = 0 then
    begin
      Result := i;
      exit;
    end;
end;

class function TsdStorage.StreamIsUnique(const AName: Utf8String): boolean;
begin
  Result := copy(AName, 1, 3) = 'TMP';
end;

procedure TsdStorage.StreamRead(const AName: Utf8String; S: TStream);
var
  Idx: integer;
begin
  Lock;
  try
    // Find the stream..
    Idx := StreamIndexByName(AName);
    // Not present?
    if Idx < 0 then
      raise Exception.CreateFmt(sstStreamNotPresent, [AName]);
    // Is it external?
    if FItems[Idx].FExternal then
    begin
      // External
      CopyStreamFrom(FExtern, FItems[Idx].FStart, FItems[Idx].FSize, S);
    end else
    begin
      // Source
      CopyStreamFrom(FSource, FItems[Idx].FStart, FItems[Idx].FSize, S);
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.StreamRename(const OldName, NewName: Utf8String);
var
  Idx: integer;
begin
  Lock;
  try
    // Find the stream, if it doesn't exist we exit
    Idx := StreamIndexByName(OldName);
    if Idx < 0 then
      exit;

    // Rename
    FItems[Idx].FName := NewName;

    // And save index
    WriteIndex;

  finally
    Unlock;
  end;
end;

procedure TsdStorage.StreamWrite(const AName: Utf8String; S: TStream);
var
  Idx: integer;
  Start: int64;
begin
  Lock;
  try
    // Find the stream..
    Idx := StreamIndexByName(AName);
    // Not present or external?
    if (Idx < 0) or FItems[Idx].FExternal then
    begin

      // Add the stream

      // We try to add it in a slot
      Start := FindSlot(S.Size);

      // Check and copy stream
      if Start + S.Size > LastStreamPosition then
        SetSourceSize(Start + S.Size);
      CopyStreamTo(S, FSource, Start);

      // Add a new record if neccesary
      if (Idx < 0) then
      begin
        Idx := length(FItems);
        SetLength(FItems, Idx + 1);
      end;

      // Update index
      FItems[Idx].FName := AName;
      FItems[Idx].FExternal := False;
      FItems[Idx].FStart := Start;
      FItems[Idx].FSize := S.Size;

      // Save index
      WriteIndex;

    end else
    begin

      // Update the stream

      if FItems[Idx].FSize >= S.Size then
      begin

        // We can add it in-place
        CopyStreamTo(S, FSource, FItems[Idx].FStart);
        // Update index
        FItems[Idx].FSize := S.Size;

      end else
      begin

        // We must add it in a slot (doesn't fit)
        Start := FindSlot(S.Size);

        // Check and copy stream
        if Start + S.Size > LastStreamPosition then
          SetSourceSize(Start + S.Size);
        CopyStreamTo(S, FSource, Start);

        // Update index
        FItems[Idx].FStart := Start;
        FItems[Idx].FSize := S.Size;

      end;

      // Save index
      WriteIndex;

    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.Unlock;
begin
  FIsBusy := False;
  FLock.Leave;
end;

procedure TsdStorage.WorkOnFile(const FileName: string);
begin
  Lock;
  try
    ClearStorage;
    try
      FSourceIsTemp := False;
      FSourceFileName := Utf8String(FileName);
      FreeAndNil(FSource);
      if not FileExists(FileName) then
      begin
        FSource := TFileStream.Create(FileName, fmCreate);
        WriteHeader;
        WriteIndex;
      end else
      begin
        FSource := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
        ReadHeaderAndIndex(FSource);
      end;
    except
      ClearStorage;
      raise;
    end;
  finally
    Unlock;
  end;
end;

procedure TsdStorage.WriteHeader;
begin
  // The header is 6 bytes in length
  FSource.Seek(0, soFromBeginning);
  FSource.Write(cStoMagick, Length(cStoMagick));
  FSource.Write(cStoVersionMajor, 1);
  FSource.Write(cStoVersionMinor, 1);
end;

procedure TsdStorage.WriteIndex;
// Write non-external index items to the source stream; but make sure that we
// have room at the start.
var
  i, IndexSize, FirstData, Count: integer;
  IndexStart, Start: int64;
begin
  IndexSize := CalculateIndexSize;
  repeat
    FirstData := FirstStreamPosition(i);
    if (FirstData > 0) and (FirstData - IndexSize < 6) then
    begin
      // we must make room for the index, we do this by moving the first item
      // backwards

      // We must add it in a new slot
      Start := FindSlot(FItems[i].FSize);

      // Check and copy block
      if Start + FItems[i].FSize > LastStreamPosition then
        SetSourceSize(Start + FItems[i].FSize);
      CopyBlock(FSource, FItems[i].FStart, Start, FItems[i].FSize);

      // Update index
      FItems[i].FStart := Start;

    end else
      break;
  until False;

  // Write the index, with a pointer to our start for backward comp
  IndexStart := 6 + SizeOf(int64);
  FSource.Seek(6, soFromBeginning);
  FSource.Write(IndexStart, SizeOf(IndexStart));

  // Count all items that are non-external
  Count := 0;
  for i := 0 to length(FItems) - 1 do
    if not FItems[i].FExternal then
      inc(Count);

  // Write count
  FSource.Write(Count, SizeOf(integer));

  // Write all items that are non-external
  for i := 0 to length(FItems) - 1 do
    if not FItems[i].FExternal then
    begin
      FSource.Write(FItems[i].FStart, SizeOf(int64));
      FSource.Write(FItems[i].FSize, SizeOf(int64));
      WriteUtf8String(FSource, FItems[i].FName);
    end;
end;

end.
