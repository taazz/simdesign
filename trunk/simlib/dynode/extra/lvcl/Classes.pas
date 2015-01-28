Unit Classes;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Classes.pas
   Just put the LVCL directory in your Project/Options/Directories/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TComponent+TFileStream+TList+TMemoryStream+TPersistent+TReader
       +TResourceStream+TStream+TStringList
   - compatible with the standard .DFM files
   - only use existing properties in your DFM, otherwise you'll get error on startup
   - TList and TStringList are simplier than standard ones
   - TStrings is not implemented
   - TMemoryStream use faster Delphi heap manager, not the slow GlobalAlloc()
   - TThread simple implementation (on Windows only)
   - Cross-Platform: it can be used on (Cross)Kylix under Linux (tested)

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (c)2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are (c)2001 Paul Toth - http://tothpaul.free.fr
  All Rights Reserved.

}

{ $define debug} // send error messages from TReader in a Console window

{$WARNINGS OFF}

Interface

uses
 SysUtils,
{$ifdef Win32}
 Windows;
{$else}
 Types,
 LibC;
{$endif}

type
  EClassesError = class(Exception);

  TNotifyEvent = procedure(Sender:TObject) of object;

  TPointerList = array of Pointer;

  TList = class
  protected
    fList: TPointerList;
  private
    fCount: integer;
    fSize: integer;
    fOwnObjects: boolean;
    function GetItem(index: integer): pointer;
    procedure SetItem(index: integer; value: pointer);
    procedure Grow;
    procedure FreeObjects;
  public
    destructor Destroy; override;
    function Add(Item: pointer): integer;
    procedure Insert(index: integer; item: pointer);
    procedure Remove(item: pointer);
    procedure Delete(index: integer);
    function  IndexOf(item: pointer): integer;
    procedure Clear;
    property Count: integer read fCount;
    property Items[index: integer]: pointer read GetItem write SetItem; default;
    // can be used in order to speed up code a little bit (but no index check)
    property List: TPointerList read fList;
  end;

  TObjectList = class(TList)
  public
    constructor Create;
  end;

  TStringList = class
  private
    fListStr: array of AnsiString;
    // fListObj[] is allocated only if objects are used (not nil)
    fListObj: array of TObject;
    fCount: integer;
    fSize : integer;
    fCaseSensitive: boolean;
    function GetItem(index: integer): string;
    procedure SetItem(index: integer; const value: string);
    function GetObject(index: integer): TObject;
    procedure SetObject(index: integer; value: TObject);
    function GetText: string;
    procedure SetText(const Value: string);
  protected
  public
    function Add(const s: string): integer;
    function AddObject(const s: string; AObject: TObject): integer;
    procedure Delete(index: integer);
    function IndexOf(const s: string): integer;
    function IndexOfObject(item: pointer): integer;
    function IndexOfName(const Name: string; const Separator: string='='): integer;
    function ValueOf(const Name: string; const Separator: string='='): string;
    function NameOf(const Value: string; const Separator: string='='): string;
    procedure Clear;
    function TextLen: integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property Count: integer read fCount;
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    property Strings[index: integer]: string read GetItem write SetItem; default;
    property Objects[index: integer]: TObject read GetObject write SetObject;
    property Text: string read GetText write SetText;
  end;

  TStrings = TStringList; // for easy debugging

const
  fmCreate = $FFFF;

  // used in TStream.Seek()
  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
  TStream = class
  protected
    procedure SetPosition(value: integer); virtual;
    function GetPosition: integer; virtual;
    function GetSize: cardinal; virtual;
    procedure SetSize(const Value: cardinal); virtual;
  public
    function Read(var Buffer; Count: integer): integer; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: integer);
    function Write(const Buffer; Count: integer): integer; virtual; abstract;
    function Seek(Offset: integer; Origin: Word): integer; virtual; abstract;
    procedure Clear;
    procedure LoadFromStream(aStream: TStream); virtual;
    procedure SaveToStream(aStream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function CopyFrom(Source: TStream; Count: integer): integer;
    property Size: cardinal read GetSize write SetSize;
    property Position: integer read GetPosition write SetPosition;
  end;

  TStringStream = class(TStream)
  private
    FDataString: string;
    FPosition: Integer;
  protected
    procedure SetSize(const Value: cardinal); override;
  public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): string;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: string);
    property DataString: string read FDataString;
  end;

  TFileStream = class(TStream)
  private
    fHandle: integer; // file handle
    fFileName: string;
  protected
{$ifdef Linux} // this special function use stat() instead of seek()
    function GetSize: cardinal; override; {$endif}
    procedure SetSize(const Value: cardinal); override;
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
    function Read(var Buffer; count: integer): integer; override;
    function Write(const Buffer; Count: integer): integer; override;
    function Seek(Offset: integer; Origin: Word): integer; override;
    property Handle: integer read fHandle;
  end;

  TMemoryStream = class(TStream)
  protected
    fPosition, fSize, fCapacity: integer;
    procedure SetPosition(value: integer); override;
    function GetPosition: integer; override;
    function GetSize: cardinal; override;
    procedure SetSize(const Value: cardinal); override;
    procedure SetCapacity(const Value: integer);
  public
    Memory: pointer;
    destructor Destroy; override;
    function Read(var Buffer; count: integer): integer; override;
    function Write(const Buffer; Count: integer): integer; override;
    function Seek(Offset: integer; Origin: Word): integer; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  TResourceStream = class(TMemoryStream)
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
  end;

{$ifdef Win32}
  TFilerFlag = (ffInherited, ffChildPos, ffInline);
  TFilerFlags = set of TFilerFlag;

  PValueType = ^TValueType;
  TValueType = (vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended,
    vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString, vaInt64,
    vaUTF8String);

  TComponent = class;

  TReader = class
  private
    fHandle: HGlobal;
    fStart: integer;
    fPointer: pByte;
    fSize: integer;
    fPosition: integer;
    fNotifyLoaded: TList;
    procedure SetPosition(Value: integer);
  public
    constructor Create(const ResourceName: string);
    destructor Destroy; override;
    procedure Loading(AComponent: TComponent);
    function Read(var Data; DataSize: integer): integer;
    function EndOfList: boolean;
    function ReadValueType: TValueType;
    function BooleanProperty: boolean;
    function IntegerProperty: integer;
    function StringProperty: string;
    function ColorProperty: integer;
    function BinaryProperty(var Size: integer):pointer;
    procedure IdentProperty(var aValue; aTypeInfo: pointer);
    procedure SetProperty(var ASet; aTypeInfo: pointer);
    function ReadByte: byte;
    function ReadWord: word;
    function ReadInteger: integer;
    function ReadString: string;
    function ReadShortString: shortstring;
    function ReadUTF8String: string;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: integer);
    property Size: integer read fSize;
    property Position: integer read fPosition write SetPosition;
  end;

  /// in LVCL, TPersistent don't have any RTTI information compiled within
  // - RTTI is not needed with LVCL and will increase code size
  // - if you need RTTI, you should use {$M+} explicitely
  TPersistent = class
  protected
    function SubProperty(const Name: string): TPersistent; virtual;
    procedure ReadProperty(const Name: string; Reader: TReader); virtual;
  end;

  TPersistentClass = class of TPersistent;

  TComponent = class(TPersistent)
  private
    fOwner: TComponent;
    fComponents: TObjectList;
  protected
    fCompName: string;
    /// Provides the interface for a method that changes the parent of the component
    procedure SetParentComponent(Value: TComponent); virtual;
    /// Returns the parent of the component
    function GetParentComponent: TComponent; virtual;
  public
    /// Allocates memory and constructs a safely initialized instance of a component
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure ReadProperties(Reader: TReader);
    procedure Loaded; virtual;
    /// Indicates the component that is responsible for streaming and freeing this component
    property Owner: TComponent read fOwner;
    /// Get of set the parent of the component
    property ParentComponent: TComponent read GetParentComponent write SetParentComponent;
    /// Indicates the number of components owned by the component
    function ComponentCount: integer;
    /// if not nil, lists all components owned by the component
    property Components: TObjectList read fComponents;
  end;

  TComponentClass = class of TComponent;

  /// minimal Threading implementation, using direct Windows API
  TThread = class
  private
    FHandle,
    FThreadID: THandle;
    FFinished,
    FTerminated,
    FSuspended,
    FCreateSuspended,
    FFreeOnTerminate: Boolean;
    FOnTerminate: TNotifyEvent;
    procedure SetSuspended(Value: Boolean);
  protected
    procedure Execute; virtual; abstract;
  public
    constructor Create(CreateSuspended: Boolean);
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Resume;
    procedure Suspend;
    function WaitFor: LongWord;
    procedure Terminate;
    property Handle: THandle read FHandle;
    property ThreadID: THandle read FThreadID;
    property Suspended: Boolean read FSuspended write SetSuspended;
    property Terminated: Boolean read FTerminated;
    property FreeOnTerminate: Boolean read FFreeOnTerminate
        write FFreeOnTerminate;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;


procedure RegisterClasses(const AClasses: array of TPersistentClass);

{$endif}


implementation


procedure TList.FreeObjects;
var i: integer;
begin
  if fOwnObjects then
    for i := 0 to fCount-1 do
      TObject(fList[i]).Free;
end;

destructor TList.Destroy;
begin
  FreeObjects;
  inherited; // will do Finalize(fList) in FinalizeRecord
end;

function TList.GetItem(index: integer): pointer;
{$ifdef PUREPASCAL}
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    result := nil else
    result := fList[Index];
end;
{$else}
asm
    cmp edx,[eax].TList.fCount
    mov eax,[eax].TList.fList
    jae @e
    mov eax,[eax+edx*4]
    ret
@e: xor eax,eax
end;
{$endif}

procedure TList.SetItem(index: integer; value: pointer);
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    exit;
  if fOwnObjects then
    TObject(fList[Index]).Free;
  fList[Index] := Value;
end;

procedure TList.Grow;
begin
  if fSize>64 then
    inc(fSize,fSize shr 2) else
    inc(fSize,16);
  Setlength(fList,fSize); // will set all new entries to nil
end;

function TList.Add(Item: pointer): integer;
begin
  if fCount=fSize then
    Grow;
  fList[fCount] := Item;
  result := fCount;
  inc(fCount);
end;

procedure TList.Insert(index: integer; item: pointer);
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    exit;
  if fCount=fSize then
    Grow;
  if index < FCount then
    Move(FList[index], FList[index+1], (FCount-index)*SizeOf(item));
  fList[index] := Item;
  inc(fCount);
end;

procedure TList.Remove(item: pointer);
begin
  Delete(IndexOf(item));
end;

procedure TList.Delete(index: integer);
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    exit;
  if fOwnObjects then
    TObject(fList[index]).Free;
  Dec(FCount);
  if index < FCount then
    Move(FList[index + 1], FList[index], (FCount - index) * SizeOf(Pointer));
end;

function TList.IndexOf(item: pointer): integer;
begin
  if self<>nil then
  for result := 0 to fCount-1 do
    if fList[result]=item then exit;
  result := -1;
end;

procedure TList.Clear;
begin
  if fOwnObjects then
    FreeObjects;
  fCount := 0;
  fSize  := 0;
  SetLength(fList,0);
end;

{ TObjectList }

constructor TObjectList.Create;
begin
  inherited;
  fOwnObjects := true; // do all the magic :)
end;

{ TStringList }

function TStringList.GetItem(index: integer): string;
{$ifdef PUREPASCAL}
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    result := '' else
    result := fListStr[index];
end;
{$else}
asm
    cmp edx,[eax].TStringList.fCount
    mov eax,[eax].TStringList.fListStr
    jae @e
    mov edx,[eax+edx*4]
    mov eax,ecx
@z: jmp System.@LStrLAsg
@e: xor edx,edx // source='' -> result := ''
    jmp @z
end;
{$endif}

procedure TStringList.SetItem(index: integer; const value: string);
begin
  if (self<>nil) and (cardinal(index)<cardinal(fCount)) then
    fListStr[index] := value;
end;

function TStringList.GetObject(index: integer): TObject;
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) or
     (index>=length(fListObj)) then
    result := nil else
    result := fListObj[index];
end;

procedure TStringList.SetObject(index: integer; value: TObject);
begin
  if (self<>nil) and (cardinal(index)<cardinal(fCount)) and (value<>nil) then begin
    if high(fListObj)<>fSize then
      SetLength(fListObj,fSize+1);
    fListObj[index] := value;
  end;
end;

function TStringList.Add(const s: string): integer;
begin
  result := AddObject(s,nil);
end;

function TStringList.AddObject(const s: string; AObject: TObject): integer;
begin
  if fCount=fSize then begin
   if fSize>64 then
     inc(fSize,fSize shr 2) else
     inc(fSize,16);
   Setlength(fListStr,fSize+1);
  end;
  fListStr[fCount] := s;
  result := fCount;
  inc(fCount);
  if AObject<>nil then
    Objects[result] := AObject;
end;

procedure TStringList.Delete(index: integer);
var L: integer;
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    exit;
  fListStr[index] := ''; // avoid GPF
  Dec(FCount);
  if index<FCount then begin
    L := (FCount-index)*4;
    Move(FListStr[index + 1], FListStr[index], L);
    if FListObj<>nil then
      Move(FListObj[index + 1], FListObj[index], L);
  end;
  pointer(fListStr[FCount]) := nil; // avoid GPF
end;

function TStringList.IndexOf(const s: string): integer;
begin
  if fCaseSensitive then begin
    for result := 0 to fCount-1 do
    if fListStr[result]=s then
      exit;
  end else
    for result := 0 to fCount-1 do
    if SameText(fListStr[result],s) then
      exit;
  result := -1;
end;

function TStringList.IndexOfObject(item: pointer): integer;
begin
  if fListObj<>nil then
  for result := 0 to fCount-1 do
    if fListObj[result]=item then
      exit;
  result := -1;
end;

function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var c: char;
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    c := p^;
    if up^<>c then
      if c in ['a'..'z'] then begin
        dec(c,32);
        if up^<>c then
          exit;
      end else exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;

function TStringList.IndexOfName(const Name: string; const Separator: string='='): integer;
var L: integer;
    Tmp: string;
begin
  if self<>nil then begin
    Tmp := UpperCase(Name)+Separator;
    L := length(Tmp);
    if L>1 then
      for result := 0 to fCount-1 do
        if IdemPChar(pointer(fListStr[result]),pointer(Tmp)) then
          exit;
  end;
  result := -1;
end;

function TStringList.ValueOf(const Name: string; const Separator: string='='): string;
var i: integer;
begin
  i := IndexOfName(Name,Separator);
  if i>=0 then
    result := copy(fListStr[i],length(Name)+length(Separator)+1,maxInt) else
    result := '';
end;

function TStringList.NameOf(const Value: string; const Separator: string='='): string;
var i,j,L: integer;
    P: PAnsiChar;
begin
  L := length(Separator)-1;
  for i := 0 to fCount-1 do begin
    j := pos(Separator,fListStr[i]);
    if j=0 then continue;
    P := PChar(pointer(fListStr[i]))+j+L;
    while P^=' ' do inc(P); // trim left value
    if StrIComp(P,pointer(Value))=0 then begin
      result := copy(fListStr[i],1,j-1);
      exit;
    end;
  end;
  result := '';
end;

procedure TStringList.Clear;
begin
  if (self=nil) or (fCount<=0) then exit;
  fCount := 0;
  fSize := 0;
  Finalize(fListStr);
  Finalize(fListObj);
end;

procedure TStringList.LoadFromFile(const FileName: string);
var F: system.text;
    s: string;
    buf: array[0..4095] of byte;
begin
  Clear;
{$I-}
  Assign(F,FileName);
  SetTextBuf(F,buf);
  Reset(F);
  if ioresult<>0 then exit;
  while not eof(F) do begin
    readln(F,s);
    Add(s);
  end;
  ioresult;
  Close(F);
  ioresult;
{$I+}
end;

procedure TStringList.SaveToFile(const FileName: string);
var F: system.text;
    i: integer;
    buf: array[0..4095] of byte;
begin
{$I-}
  Assign(F,FileName);
  SetTextBuf(F,buf);
  rewrite(F);
  if ioresult<>0 then exit;
  for i := 0 to FCount-1 do
    writeln(F,FListStr[i]);
  ioresult;
  Close(F);
  ioresult;
{$I+}
end;

function TStringList.TextLen: integer;
var i: integer;
begin
  result := fCount*2; // #13#10 size
  for i := 0 to fCount-1 do
    if integer(fListStr[i])<>0 then
      inc(result,pInteger(integer(fListStr[i])-4)^); // fast add length(List[i])
end;

function TStringList.GetText: string;
var i,V,L: integer;
    P: PChar;
begin
  // much faster than for i := 0 to Count-1 do result := result+List[i]+#13#10;
  SetLength(result,TextLen);
  P := pointer(result);
  for i := 0 to fCount-1 do begin
    V := integer(fListStr[i]);
    if V<>0 then begin
      L := pInteger(V-4)^;  // L := length(List[i])
      move(pointer(V)^,P^,L);
      inc(P,L);
    end;
    PWord(P)^ := 13+10 shl 8;
    inc(P,2);
  end;
end;

procedure TStringList.SetText(const Value: string);
function GetNextLine(d: pChar; out next: pChar): string;
begin
  next := d;
  while not (d^ in [#0,#10,#13]) do inc(d);
  SetString(result,next,d-next);
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  if d^=#0 then
    next := nil else
    next := d;
end;
var P: PAnsiChar;
begin
  Clear;
  P := pointer(Value);
  while P<>nil do
    Add(GetNextLine(P,P));
end;

{ TStream }

procedure TStream.Clear;
begin
  Position := 0;
  Size := 0;
end;

function TStream.CopyFrom(Source: TStream; Count: integer): integer;
const
  MaxBufSize = $F000*4; // 240KB buffer (should be fast enough ;)
var
  BufSize, N: integer;
  Buffer: PChar;
begin
  if Count=0 then begin  // Count=0 for whole stream copy
    Source.Position := 0;
    Count := Source.Size;
  end;
  result := Count;
  if Count>MaxBufSize then
    BufSize := MaxBufSize else
    BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count<>0 do begin
      if Count>BufSize then
        N := BufSize else
        N := Count;
      if Source.Read(Buffer^, N)<>N then
        break; // stop on any read error
      if Write(Buffer^, N)<>N then
        break; // stop on any write error
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

function TStream.GetPosition: integer;
begin
  Result := Seek(0, soFromCurrent);
end;

function TStream.GetSize: cardinal;
var Pos: cardinal;
begin
  Pos := Seek(0, soFromCurrent);
  Result := Seek(0, soFromEnd);
  Seek(Pos, soFromBeginning);
end;

procedure TStream.SetPosition(value: integer);
begin
  Seek(Value, soFromBeginning);
end;

procedure TStream.SetSize(const Value: cardinal);
begin
  // default = do nothing  (read-only streams, etc)
  // descendents should implement this method
end;

procedure TStream.LoadFromFile(const FileName: string);
var F: TFileStream;
begin
  F := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TStream.LoadFromStream(aStream: TStream);
begin
  CopyFrom(aStream,0); // Count=0 for whole stream copy
end;

procedure TStream.ReadBuffer(var Buffer; Count: integer);
begin
  Read(Buffer,Count);
end;

procedure TStream.SaveToFile(const FileName: string);
var F: TFileStream;
begin
  F := TFileStream.Create(FileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TStream.SaveToStream(aStream: TStream);
begin
  aStream.CopyFrom(self,0); // Count=0 for whole stream copy
end;

{ TStringStream }

{ TStringStream }

constructor TStringStream.Create(const AString: string);
begin
  inherited Create;
  FDataString := AString;
end;

function TStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

function TStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TStringStream.ReadString(Count: Longint): string;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PChar(@FDataString[FPosition + 1]), Len);
  Inc(FPosition, Len);
end;

procedure TStringStream.WriteString(const AString: string);
begin
  Write(PChar(AString)^, Length(AString));
end;

procedure TStringStream.SetSize(const Value: cardinal);
begin
  SetLength(FDataString, Value);
  if FPosition > Value then FPosition := Value;
end;

{ TFileStream }

constructor TFileStream.Create(const FileName: string; Mode: Word);
begin
  fFileName := FileName;
  if Mode=fmCreate then
    fHandle := FileCreate(FileName) else
    fHandle := FileOpen(FileName,Mode);
  if fHandle<0 then
    raise EStreamError.Create(FileName);
end;

destructor TFileStream.Destroy;
begin
  FileClose(fHandle);
  inherited;
end;

function TFileStream.Read(var Buffer; Count: integer): integer;
begin
  if (fHandle<0) or (Count<=0) then
    result := 0 else
    result := FileRead(fHandle,Buffer,Count);
end;

function TFileStream.Write(const Buffer; Count: integer): integer;
begin
  if (fHandle<0) or (Count<=0) then
    result := 0 else
    result := FileWrite(fHandle,Buffer,Count);
end;

function TFileStream.Seek(Offset: integer; Origin: Word): integer;
begin
  if fHandle<0 then
    result := 0 else
    result := FileSeek(fHandle,offset,Origin);
end;

{$ifdef Linux}
function TFileStream.GetSize: cardinal;
var st: TStatBuf;
begin
  if stat(PChar(fFileName),st)=0 then
    result := st.st_size else
    result := 0;
end;
{$endif}

procedure TFileStream.SetSize(const Value: cardinal);
begin
  Seek(Value, soFromBeginning);
{$ifdef Win32}
  if not SetEndOfFile(FHandle) then
{$else}
  if ftruncate(FHandle, Value)=-1 then
{$endif}
    raise EStreamError.Create('SetSize');
end;


{$ifdef Win32}

{ TReader }

constructor TReader.Create(const ResourceName: string);
var res: THandle;
begin
  res := FindResource(hInstance,pointer(ResourceName),RT_RCDATA);
  if res=0 then exit;
  fHandle := LoadResource(hInstance,res);
  if fHandle=0 then exit;
  fPointer := LockResource(fHandle);
  if fPointer<>nil then
    fSize := SizeOfResource(hInstance,res);
  fStart := integer(fPointer);
  fNotifyLoaded := TList.Create;
end;

destructor TReader.Destroy;
var i: integer;
begin
  UnlockResource(fHandle);
  FreeResource(fHandle);
  for i := 0 to fNotifyLoaded.Count-1 do
    TComponent(fNotifyLoaded.fList[i]).Loaded;
  fNotifyLoaded.Free;
  inherited;
end;

procedure TReader.SetPosition(Value: integer);
begin
  fPosition := Value;
  fPointer := Pointer(fStart+Value);
end;

procedure TReader.Loading(AComponent: TComponent);
begin
  fNotifyLoaded.Add(AComponent);
end;

function TReader.Read(var Data; DataSize: integer): integer;
begin
  if fPosition+DataSize<fSize then
    result := DataSize else
    result := fSize-fPosition;
  if result<=0 then exit;
  move(fPointer^,Data,result);
  Inc(fPosition,result);
  inc(fPointer,result);
end;

function TReader.EndOfList: boolean;
begin
  result := (fPosition<fSize) and (fPointer^=0);
  if result then begin
    inc(fPosition);
    inc(fPointer);
  end;
end;

function TReader.ReadValueType: TValueType;
begin
  result := PValueType(fPointer)^;
  inc(fPosition);
  inc(fPointer);
end;

function TReader.BooleanProperty: boolean;
var ValueType: TValueType;
begin
  ValueType := ReadValueType;
  case ValueType of
    vaFalse  : result := False;
    vaTrue   : result := True;
    else raise EClassesError.Create('boolean?');
  end;
end;

function TReader.IntegerProperty: integer;
var ValueType: TValueType;
begin
  ValueType := ReadValueType;
  case ValueType of
    vaInt8  : result := ShortInt(ReadByte);
    vaInt16 : result := ReadWord;
    vaInt32 : result := ReadInteger;
    else raise EClassesError.Create('ordinal?');
  end;
end;

function TReader.StringProperty: string;
var ValueType: TValueType;
begin 
  ValueType := ReadValueType;
  case ValueType of
    vaString,
    vaIdent:      result := ReadString;
    vaUTF8String: result := ReadUTF8String;
    else raise EClassesError.Create('string?');
  end;
end;

function TReader.ColorProperty: integer;
var ValueType: TValueType;
begin
  ValueType := ReadValueType;
  case ValueType of
    vaInt16 : result := ReadWord;
    vaInt32 : result := ReadInteger;
    vaIdent : result := IdentToColor(pointer(ReadString));
    else raise EClassesError.Create('color?');
  end;
end;

function TReader.BinaryProperty(var Size: integer): pointer;
var ValueType: TValueType;
begin
  ValueType := ReadValueType;
  case ValueType of
   vaBinary : begin
     Size := ReadInteger;
     GetMem(result,Size);
     Read(result^,Size);
   end;
   else raise EClassesError.Create('binary?');
  end;
end;

function GetEnumNameValue(aTypeInfo: pointer; const aValue: shortstring): integer;
asm
    movzx ecx,byte ptr [eax+1]
    push ebx
    push esi
    push edi
    mov eax,[eax+ecx+9+2] // BaseType
    movzx ebx,byte ptr [edx]
    mov eax,[eax] 
    xor edi,edi
    movzx ecx,byte ptr [eax+1]
    or ebx,ebx
    mov esi,[eax+ecx+2+5]      // esi=MaxValue
    lea eax,[eax+ecx+2+9+4]    // eax=NameList
    jz @z // aValue='' -> return -1
@1: movzx ecx,byte ptr [eax]
    cmp ecx,ebx
    jz @0 // same length
@2: cmp edi,esi
    lea edi,edi+1
    lea eax,eax+ecx+1 // next short string
    jne @1
@z: or eax,-1
@e: pop edi
    pop esi
    pop ebx
    ret
@0: push eax
    push edx
@n: inc eax
    inc edx
    mov ch,[eax]
    xor ch,[edx]
    and ch,$DF // case insensitive search
    jne @d
    dec cl
    jnz @n
    pop edx
    pop edx
    mov eax,edi
    jmp @e
@d: pop edx
    pop eax
    mov ecx,ebx
    jmp @2
end;

procedure TReader.IdentProperty(var aValue; aTypeInfo: pointer);
var ValueType: TValueType;
    V: integer;
begin
  ValueType := ReadValueType;
  if ValueType=vaIdent then begin
     V := GetEnumNameValue(aTypeInfo,ReadShortString);
     if cardinal(V)<255 then begin
       integer(aValue) := V;
       exit;
     end;
  end;
  raise EClassesError.Create('ident?');
end;

procedure TReader.SetProperty(var ASet; aTypeInfo: pointer);
var s: ShortString;
    i: integer;
begin
  if ReadValueType<>vaSet then
    raise EClassesError.Create('set?');
  integer(ASet) := 0;
  repeat
    s := ReadShortString;
    if s[0]=#0 then break;
    i := GetEnumNameValue(aTypeInfo,s);
    if i>=0 then
      integer(ASet) := integer(ASet) or (1 shl i);
  until false;
end;

function TReader.ReadByte:byte;
begin
  result := fPointer^;
  inc(fPosition);
  inc(fPointer);
end;

function TReader.ReadWord:word;
begin
  result := pWord(fPointer)^;
  inc(fPosition,2);
  inc(fPointer,2);
end;

function TReader.ReadInteger: integer;
begin
  result := pInteger(fPointer)^;
  inc(fPosition,4);
  inc(fPointer,4);
end;

function TReader.ReadString: string;
var L: integer;
begin
  L := fPointer^;
  SetString(result,PChar(fPointer)+1,fPointer^);
  inc(L);
  inc(fPosition,L);
  inc(fPointer,L);
end;

function TReader.ReadShortString: shortstring;
var L: integer;
begin
  L := fPointer^+1;
  move(fPointer^,result,L);
  inc(fPosition,L);
  inc(fPointer,L);
end;

function UTF8ToPChar(dest: PAnsiChar; source: PChar; count: integer): integer;
// faster than UTF8Decode(), which use a WideString as temp buffer
// this version assumes that we use WinAnsi code page in LVCL
var c: cardinal;
    begd: pChar;
    endSource: PChar;
begin
  result := 0;
  if source=nil then exit;
  begd := dest;
  endSource := source+count;
  repeat
    c := byte(source^); inc(source);
    if byte(c) and $80=0 then begin
      dest^ := chr(byte(c)); inc(dest);
    end else begin
      if source>=endsource then break;
      if c and $20=0 then begin
        c := c shl 6+byte(source^)-$00003080; inc(source);
        dest^ := chr(c); inc(dest);  // #128..#255 -> direct copy
      end else
        inc(source,2); // don't decode any char above #255
    end;
  until source>=endsource;
  result := dest-begd;
end;

function TReader.ReadUTF8String: string;
// this faster version assumes that we use WinAnsi code page in LVCL
var L,L2: integer;
begin
  L := ReadInteger;
  SetLength(result,L);
  if L=0 then exit;
  L2 := UTF8ToPChar(pointer(result),pointer(fPointer),L);
  if L<>L2 then
    SetLength(result,L2);
  inc(fPointer, L);
  inc(fPosition, L);
end;

procedure TReader.ReadPrefix(var Flags: TFilerFlags; var AChildPos: integer);
begin
  byte(Flags) := 0;
  if (fSize>1) and (fPointer^ and $F0 = $F0) then begin
    byte(Flags) := ReadByte and $0F;
    if ffChildPos in Flags then
      AChildPos := ReadInteger;
  end;
end;

{ TPersistent }

function TPersistent.SubProperty(const Name: string): TPersistent;
begin
  result := nil;
end;

procedure TPersistent.ReadProperty(const Name: string; Reader: TReader);
// default behavior is to read the property value from Reader and ignore it
var {$ifdef debug}
  ValueType: TValueType;
  Value: string;
{$endif}
  i: integer;
begin
  if self=nil then exit;
  i := pos('.',Name);
  if i>0 then
    SubProperty(Copy(Name,1,i-1)).ReadProperty(copy(Name,i+1,200),Reader) else
  with Reader do begin
{$ifdef debug}
   ValueType := ReadValueType;
   case ValueType of
    vaInt8   : Value := IntToStr(ReadByte);
    vaInt16  : Value := IntToStr(ReadWord);
    vaString : Value := ReadString;
    vaIdent  : Value := '"'+ReadString+'"';
    vaFalse  : Value := '"FALSE"';
    vaTrue   : Value := '"TRUE"';
    vaBinary : begin
                i := ReadInteger; Value := '('+IntToStr(i)+' bytes)';
                inc(fPointer,i); inc(fPosition,i);
               end;
    else OutputDebugString(pChar('Bad ValueType '+IntToStr(ord(ValueType))));
   end;
//   writeln(ClassName+'.'+Name+'='+Value);
   {$else}
   case ReadValueType of // no handler -> ignore this property
     vaInt8:   ReadByte;
     vaInt16:  ReadWord;
     vaIdent, vaString: ReadShortString;
     vaFalse, vaTrue: ;
     vaBinary: begin
       i := ReadInteger;
       inc(fPointer,i);
       inc(fPosition,i);
     end;
   else
     raise EClassesError.Create('Unknown value type');
   end;
  {$endif}
  end; // with Reader do
end;

var
  RegisteredClasses: TList = nil;

function FindClass(const AClass: ShortString): TPersistentClass;
var i: integer;
begin
  if RegisteredClasses=nil then
    RegisteredClasses := TList.Create else
  for i := 0 to RegisteredClasses.Count-1 do begin
    result := RegisteredClasses.fList[i];
    if PShortString(PInteger(integer(result)+vmtClassName)^)^=AClass then
      exit;
  end;
  result := nil;
end;

procedure RegisterClasses(const AClasses: array of TPersistentClass);
var i: integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    if FindClass(PShortString(PInteger(integer(AClasses[i])+vmtClassName)^)^)=nil then
      RegisteredClasses.Add(AClasses[i]);
end;

function CreateComponent(const AClass: ShortString; AOwner: TComponent): TComponent;
var RC: TPersistentClass;
begin
  RC := FindClass(AClass);
  if (RC=nil) or not RC.InheritsFrom(TComponent) then
    raise EClassesError.CreateFmt('%s?',[AClass]);
  result := TComponent(RC.NewInstance);
  result.Create(AOwner);
end;


{ TComponent }

constructor TComponent.Create(AOwner: TComponent);
begin
  if AOwner=nil then exit;
  if AOwner.fComponents=nil then
    AOwner.fComponents := TObjectList.Create;
  AOwner.fComponents.Add(self);
  fOwner := AOwner;
end;

procedure TComponent.ReadProperties(Reader: TReader);
var
  Flags: TFilerFlags;
  position: integer;
  Child: TComponent;
  field: ^TComponent;
  Name: shortstring;
begin
  while not Reader.EndOfList do
    ReadProperty(Reader.ReadString, Reader);
  while not Reader.EndOfList do begin
    Reader.ReadPrefix(Flags,Position);
    Name := Reader.ReadShortString;  // read ClassName
    Child := CreateComponent(Name, Self);
    Child.SetParentComponent(Self);
    Reader.Loading(Child);
    Name := Reader.ReadShortString; 
    Child.fCompName := Name;
    Child.ReadProperties(Reader);
    field := FieldAddress(Name);
    if field<>nil then
      field^ := Child;
  end;
end;

procedure TComponent.Loaded;
begin
end;

procedure TComponent.SetParentComponent(Value:TComponent);
begin
end;

function TComponent.GetParentComponent:TComponent;
begin
  result := fOwner;
end;

destructor TComponent.Destroy;
begin
  fComponents.Free; // free all contained components
  inherited;
end;

function TComponent.ComponentCount: integer;
begin
  if (self=nil) or (fComponents=nil) then
    result := 0 else
    result := fComponents.Count;
end;

{$endif}

{ TMemoryStream }

procedure TMemoryStream.SetCapacity(const Value: integer);
begin
  if self=nil then
    exit;
  fCapacity := Value;
  ReallocMem(Memory,fCapacity);
  if fPosition>=fCapacity then // adjust Position if truncated
    fPosition := fCapacity-1;
  if fSize>=fCapacity then     // adjust Size if truncated
    fSize := fCapacity-1;
end;

procedure TMemoryStream.SetSize(const Value: cardinal);
begin
  if Value>fCapacity then
    SetCapacity(Value+16384); // reserve some space for inplace growing
  fSize := Value;
end;

destructor TMemoryStream.Destroy;
begin
  if Memory<>nil then
    Freemem(Memory);
  inherited;
end;

function TMemoryStream.GetPosition: integer;
begin
  result := fPosition;
end;

function TMemoryStream.Read(var Buffer; count: integer): integer;
begin
  if (self<>nil) and (Memory<>nil) then
  if (FPosition>=0) and (Count>0) then begin
    result := FSize - FPosition;
    if result>0 then begin
      if result>Count then result := Count;
      Move((pChar(Memory)+FPosition)^, Buffer, result);
      Inc(FPosition, result);
      Exit;
    end;
  end;
  result := 0;
end;

function TMemoryStream.Seek(Offset: integer; Origin: Word): integer;
begin
  result := Offset; // default is soFromBeginning
  case Origin of  
    soFromEnd:       inc(result,fSize);
    soFromCurrent:   inc(result,fPosition);
  end;
  if result<=fSize then
    fPosition := result else begin
    result := fSize;
    fPosition := fSize;
  end;
end;

procedure TMemoryStream.SetPosition(value: integer);
begin
  if value>fSize then
    value := fSize;
  fPosition := value;
end;

function TMemoryStream.GetSize: cardinal;
begin
  result := fSize;
end;

function TMemoryStream.Write(const Buffer; Count: integer): integer;
var Pos: integer;
begin
  if (FPosition>=0) and (Count>0) then begin
    Pos := FPosition+Count;
    if Pos>FSize then begin
      if Pos>FCapacity then
        if Pos>65536 then // growing by 16KB chunck up to 64KB, then by 1/4 of size
          SetCapacity(Pos+Pos shr 2) else
          SetCapacity(Pos+16384);
      FSize := Pos;
    end;
    Move(Buffer, (pChar(Memory)+FPosition)^, Count);
    FPosition := Pos;
    result := Count;
  end else
    result := 0;
end;

procedure TMemoryStream.LoadFromStream(aStream: TStream);
var L: integer;
begin
  if aStream=nil then exit;
  L := aStream.Size;
  SetCapacity(L);
  aStream.Position := 0;
  if (L<>0) and (aStream.Read(Memory^,L)<>L) then
    raise EStreamError.Create('Load');
  fPosition := 0;
  fSize := L;
end;

procedure TMemoryStream.SaveToStream(aStream: TStream);
begin
  if (self<>nil) and (FSize<>0) and (aStream<>nil) and (Memory<>nil) then
    aStream.Write(Memory^, FSize);
end;


{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle;
  const ResName: string; ResType: PChar);
// just a copy from resource to local TMemoryStream -> shorter code
var HResInfo: THandle;
    HGlobal: THandle;
begin
  HResInfo := FindResource(Instance,PChar(ResName),ResType);
  if HResInfo=0 then
    exit;
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal=0 then
    exit;
  Write(LockResource(HGlobal)^,SizeOfResource(Instance, HResInfo));
  FPosition := 0; 
end;


{$ifdef Win32}

{ TThread }

function ThreadProc(Thread: TThread): Integer;
var FreeThread: Boolean;
begin
  if not Thread.FTerminated then
  try
    result := 0; // default ExitCode
    try
      Thread.Execute;
    except
      on Exception do
        result := -1;
    end;
  finally
    FreeThread := Thread.FFreeOnTerminate;
    Thread.FFinished := True;
    if Assigned(Thread.OnTerminate) then
      Thread.OnTerminate(Thread);
    if FreeThread then
      Thread.Free;
    EndThread(result);   
  end;
end;

procedure TThread.AfterConstruction;
begin
  if not FCreateSuspended then
    Resume;
end;

constructor TThread.Create(CreateSuspended: Boolean);
begin
  IsMultiThread := true; // for FastMM4 locking, e.g.
  inherited Create;
  FSuspended := CreateSuspended;
  FCreateSuspended := CreateSuspended;
  FHandle := BeginThread(nil, 0, @ThreadProc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
  if FHandle = 0 then
    raise Exception.Create(SysErrorMessage(GetLastError));
  SetThreadPriority(FHandle, THREAD_PRIORITY_NORMAL); 
end;

destructor TThread.Destroy;
begin
  if (FThreadID <> 0) and not FFinished then begin
    Terminate;
    if FCreateSuspended then
      Resume;
    WaitFor;
  end;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  inherited Destroy;
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle) = 1 then // returns the thread's previous suspend count
    FSuspended := False;
end;

procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend else
      Resume;
end;

procedure TThread.Suspend;
var OldSuspend: Boolean;
begin
  OldSuspend := FSuspended;
  FSuspended := True;
  if Integer(SuspendThread(FHandle))<0 then
    FSuspended := OldSuspend;
end;

procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: LongWord;
begin
  if GetCurrentThreadID<>MainThreadID then
    WaitForSingleObject(FHandle, INFINITE);
  GetExitCodeThread(FHandle, result);
end;


initialization

finalization
  RegisteredClasses.Free;
{$endif}
end.


