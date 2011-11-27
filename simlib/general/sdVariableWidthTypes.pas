{ unit sdVariableWidthTypes

  This unit defines types cardinal, integer, floating point, rational and
  character types that have variable width (1..N bytes per type):

  TsdCardinal
  TsdInteger
  TsdFloat
  TsdRational
  TsdChar

  The idea here is that the fixed width types often cause confusion (ie:
  is "integer" 16bit, 32bit or 64bit, is word 16bit or 32 bit, is dword 32bit or 64bit,
  etc), and causes unnecessary memory hogging.
  A variable-width type can consume anything from 1 byte to N bytes, depending on
  the information/accuracy used.

  Example: any
      cardinal in range [0..$7F] will use 1 byte in streams,
      cardinal [$80       .. $3FFF] will use 2 bytes,
      cardinal [$4000     .. $1FFFFF] will use 3 bytes,
      cardinal [$200000   .. $0FFFFFFF] will use 4 bytes,
      cardinal [$10000000 .. $07FFFFFFFF] will use 5 bytes, etc

  This way, processing of small values is very efficient, while functionality
  for possibly huge values is not compromised.

  Floating-point variable-width types use only one byte for values '0' and '1',
  and 2..N bytes for more complex values.

  TsdChar values allow all unicode chars (not just widechar)

  This unit also allows rational types (ie value = num / den)

  copyright (c) 2011 Nils Haeck (www.simdesign.nl)

}
unit sdVariableWidthTypes;

interface

uses
  Classes, SysUtils, sdDebug;

const
  // currently, the 'extended' value has biggest value of 10 bytes, thus
  // we use a little overhead, 16 bytes. 16 bytes also have 128bit precision
  // ie for UUID etc
  cScratchSizeBytes = 16;

type
  // scratch byte array
  TsdScratch = array[0..cScratchSizeBytes - 1] of byte;

  // variable-width type ancestor
  TsdVariableWidthType = class(TDebugPersistent)
  private
    FData: TsdScratch;
    FCount: cardinal;
    function GetAsCardinal: cardinal; virtual;
    procedure SetAsCardinal(const Value: cardinal); virtual;
    function GetAsInteger: integer; virtual;
    procedure SetAsInteger(const Value: integer); virtual;
    function GetAsInt64: int64; virtual;
    procedure SetAsInt64(const Value: int64); virtual;
    function GetAsByte: byte; virtual;
    function GetAsChar: char; virtual;
    function GetAsDouble: double; virtual;
    function GetAsExtended: extended; virtual;
    function GetAsSingle: single; virtual;
    function GetAsUtf8String: utf8string; virtual;
    function GetAsWideChar: widechar; virtual;
    function GetAsWord: word; virtual;
    procedure SetAsWord(const Value: word); virtual;
    procedure SetAsByte(const Value: byte); virtual;
    procedure SetAsChar(const Value: char); virtual;
    procedure SetAsDouble(const Value: double); virtual;
    procedure SetAsExtended(const Value: extended); virtual;
    procedure SetAsSingle(const Value: single); virtual;
    procedure SetAsUtf8String(const Value: utf8string); virtual;
    procedure SetAsWideChar(const Value: widechar); virtual;
  public
    constructor CreateDebug(AOwner: TDebugComponent); override;
    property AsCardinal: cardinal read GetAsCardinal write SetAsCardinal;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsInt64: int64 read GetAsInt64 write SetAsInt64;
    property AsByte: byte read GetAsByte write SetAsByte;
    property AsWord: word read GetAsWord write SetAsWord;
    property AsSingle: single read GetAsSingle write SetAsSingle;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsExtended: extended read GetAsExtended write SetAsExtended;
    property AsChar: char read GetAsChar write SetAsChar;
    property AsWideChar: widechar read GetAsWideChar write SetAsWideChar;
    property AsUtf8String: utf8string read GetAsUtf8String write SetAsUtf8String;
    // read the value from the stream. Result is the number of *bytes* read.
    function ReadFromStream(S: TStream): integer; virtual; abstract;
    // write the value to the stream. Result is the number of *bytes* written.
    function WriteToStream(S: TStream): integer; virtual; abstract;
  end;

  // natural number [0..N-1]
  TsdCardinal = class(TsdVariableWidthType)
  private
    function GetAsCardinal: cardinal; override;
    procedure SetAsCardinal(const Value: cardinal); override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(const Value: integer); override;
    function GetAsInt64: int64; override;
    procedure SetAsInt64(const Value: int64); override;
  public
    function ReadFromStream(S: TStream): integer; override;
    function WriteToStream(S: TStream): integer; override;
  end;

resourcestring

  sNotImplemented = 'not implemented';
  sConstantViolatesSubrangeBounds = 'constant violates subrange bounds';

implementation

{ TsdVariableWidthType }

constructor TsdVariableWidthType.CreateDebug(AOwner: TDebugComponent);
begin
  inherited;
  FCount := 0;
end;

function TsdVariableWidthType.GetAsByte: byte;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsCardinal: cardinal;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsChar: char;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsDouble: double;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsExtended: extended;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsInt64: int64;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsInteger: integer;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsSingle: single;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsUtf8String: utf8string;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsWideChar: widechar;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

function TsdVariableWidthType.GetAsWord: word;
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsByte(const Value: byte);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsCardinal(const Value: cardinal);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsChar(const Value: char);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsDouble(const Value: double);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsExtended(const Value: extended);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsInt64(const Value: int64);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsInteger(const Value: integer);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsSingle(const Value: single);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsUtf8String(const Value: utf8string);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsWideChar(const Value: widechar);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

procedure TsdVariableWidthType.SetAsWord(const Value: word);
begin
// functionality in descendants
  raise Exception.Create(sNotImplemented);
end;

{ TsdCardinal }

function TsdCardinal.GetAsCardinal: cardinal;
begin
  System.Move(FData[0], Result, 4);
end;

function TsdCardinal.GetAsInt64: int64;
begin
  System.Move(FData[0], Result, 8);
end;

function TsdCardinal.GetAsInteger: integer;
begin
  System.Move(FData[0], Result, 4);
end;

function TsdCardinal.ReadFromStream(S: TStream): integer;
var
  C: cardinal;
  B: byte;
  Bits, Index: integer;
begin
  Result := 0;
  C := 0;
  Index := 0;
  Bits := 0;
  repeat
    S.Read(B, 1);
    inc(Result);
    if B > 0 then
    begin
      inc(C, (B and $7F) shl Bits);
      inc(Bits, 7);
      if Bits >= 8 then
      begin
        dec(Bits, 8);
        FData[Index] := C and $FF;
        inc(Index);
        C := C shr 8;
      end;
    end;
  until(B and $80) = 0;
  while C > 0 do
  begin
    FData[Index] := C and $FF;
    inc(Index);
    C := C shr 8;
  end;
  FCount := Index;
end;

procedure TsdCardinal.SetAsCardinal(const Value: cardinal);
begin
  FCount := 4;
  System.Move(Value, FData[0], FCount);
  while (FCount > 0) and (FData[FCount - 1] = 0) do
    dec(FCount);
end;

procedure TsdCardinal.SetAsInt64(const Value: int64);
begin
  if Value < 0 then
  begin
    DoDebugOut(Self, wsFail, sConstantViolatesSubrangeBounds);
    exit;
  end;
  FCount := 8;
  System.Move(Value, FData[0], FCount);
  while (FCount > 0) and (FData[FCount - 1] = 0) do
    dec(FCount);
end;

procedure TsdCardinal.SetAsInteger(const Value: integer);
begin
  if Value < 0 then
  begin
    DoDebugOut(Self, wsFail, sConstantViolatesSubrangeBounds);
    exit;
  end;
  FCount := 4;
  System.Move(Value, FData[0], FCount);
  while (FCount > 0) and (FData[FCount - 1] = 0) do
    dec(FCount);
end;

function TsdCardinal.WriteToStream(S: TStream): integer;
var
  C, CShift: cardinal;
  B: byte;
  SBits, IBits, Index: cardinal;
  HasCompleted: boolean;
begin
  Result := 0;
  C := 0;
  Index := 0;
  IBits := 0;
  SBits := 0;
  HasCompleted := False;
  repeat
    if Index < FCount then
      inc(C, FData[Index] shl IBits);
    CShift := (C shr SBits);
    B := CShift and $7F;
    inc(Index);
    inc(IBits, 8);
    inc(SBits, 7);
    if SBits >= 8 then
    begin
      C := C shr 8;
      dec(SBits, 8);
      dec(IBits, 8);
    end;
    if (Index >= FCount) and (CShift < $80) then
      HasCompleted := True
    else
      B := B or $80;
    S.Write(B, 1);
    inc(Result);
  until HasCompleted;
end;

end.
