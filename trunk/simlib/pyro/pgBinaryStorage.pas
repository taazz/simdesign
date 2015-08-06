{ Project: Pyro
  Module: Pyro Core

  Description:
    Implementation of storage of elements in binary format

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgBinaryStorage;

interface

uses
  Classes, SysUtils, pgDocument, Pyro;

type

  // reference implementation for binary storage of containers
  TpgBinaryStorage = class(TpgStorage)
  private
    FInfo: byte; // Contains marker and format info
    FMarker: TpgStorageMarker;
    FDigits: TpgStorageDigits;
    procedure WriteIntegerPart(const Value: integer);
    function ReadIntegerPart: integer;
  protected
    function ReadMarker: TpgStorageMarker; override;
    procedure RewindMarker; override;
    procedure WriteMarker(AMarker: TpgStorageMarker); override;
    procedure ReadElementInfo(var AInfoId, AElementId: longword); override;
    procedure WriteElementInfo(AInfoId, AElementId: longword); override;
    procedure ReadPropInfo(var AInfoId: longword); override;
    procedure WritePropInfo(AInfoId: longword); override;
  public
    function ReadBool: boolean; override;
    procedure WriteBool(const Value: boolean); override;
    function ReadInt: integer; override;
    procedure WriteInt(const Value: integer); override;
    function ReadFloat: double; override;
    procedure WriteFloat(const Value: double); override;
    function ReadString: Utf8String; override;
    procedure WriteString(const Value: Utf8String); override;
  end;

implementation

const

  cInfoFromMarker: array[TpgStorageMarker] of byte =
   ($C0, $80, $90, $A0, $B0, $00, $10, $20, $30, $40, $D0);

{ TpgBinaryStorage }

function TpgBinaryStorage.ReadBool: boolean;
var
  Value: integer;
begin
  Value := ReadInt;
  if Value = 0 then
    Result := False
  else if Value = 1 then
    Result := True
  else
    raise Exception.Create(sFatalReadError);
end;

procedure TpgBinaryStorage.ReadElementInfo(var AInfoId, AElementId: longword);
begin
  AInfoId := ReadInt;
  AElementId := ReadInt;
end;

function TpgBinaryStorage.ReadFloat: double;
var
  S: single;
  D: double;
begin
  ReadMarker;
  if FMarker <> smFloatValue then
    raise Exception.CreateFmt(sUnexpectedMarker, [cMarkerNames[FMarker], GetPositionInfo]);
  case FDigits of
  sdValueIsZero: Result := 0;
  sdValueIsOne:  Result := 1;
  sdSingle:
    begin
      Stream.Read(S, 4);
      Result := S;
    end;
  sdDouble:
    begin
      Stream.Read(D, 8);
      Result := D;
    end;
  else
    raise Exception.Create(sFatalReadError);
  end;
end;

function TpgBinaryStorage.ReadInt: integer;
begin
  ReadMarker;
  if FMarker <> smIntValue then
    raise Exception.CreateFmt(sUnexpectedMarker, [cMarkerNames[FMarker], GetPositionInfo]);
  Result := ReadIntegerPart;
end;

function TpgBinaryStorage.ReadIntegerPart: integer;
var
  P1: byte;
  P2: word;
  P4: longword;
begin
  case FDigits of
  sdValueIsZero: Result := 0;
  sdValueIsOne:  Result := 1;
  sdPositive8bits, sdNegative8bits:
    begin
      Stream.Read(P1, 1);
      Result := P1;
    end;
  sdPositive16bits, sdNegative16bits:
    begin
      Stream.Read(P2, 2);
      Result := P2;
    end;
  sdPositive32bits, sdNegative32bits:
    begin
      Stream.Read(P4, 4);
      Result := P4;
    end;
  else
    raise Exception.Create(sFatalReadError);
  end;
  if FDigits in [sdNegative8bits, sdNegative16bits, sdNegative32bits] then
    Result := -Result;
end;

function TpgBinaryStorage.ReadMarker: TpgStorageMarker;
var
  D: byte;
begin
  if Stream.Read(FInfo, 1) <> 1 then
    raise Exception.Create(sUnexpectedEndOfFile);
  case FInfo and $F0 of
  $00: FMarker := smIntValue;
  $10: FMarker := smFloatValue;
  $20: FMarker := smStringValue;
  $30: FMarker := smIntListValue;
  $40: FMarker := smFloatListValue;
  $80: FMarker := smElementStart;
  $90: FMarker := smElementClose;
  $A0: FMarker := smPropStart;
  $B0: FMarker := smPropClose;
  $C0: FMarker := smFileVersion;
  $D0: FMarker := smEndOfFile;
  else
    raise Exception.Create(sFatalReadError);
  end;
  Result := FMarker;
  D := FInfo and $0F;
  if D <= integer(high(TpgStorageDigits)) then
    FDigits := TpgStorageDigits(D)
  else
    raise Exception.Create(sFatalReadError);
end;

procedure TpgBinaryStorage.ReadPropInfo(var AInfoId: longword);
begin
  AInfoId := ReadInt;
end;

function TpgBinaryStorage.ReadString: Utf8String;
var
  L: integer;
begin
  ReadMarker;
  if FMarker <> smStringValue then
    raise Exception.CreateFmt(sUnexpectedMarker, [cMarkerNames[FMarker], GetPositionInfo]);
  L := ReadIntegerPart;
  SetLength(Result, L);
  if L > 0 then
    Stream.Read(Result[1], L);
end;

procedure TpgBinaryStorage.RewindMarker;
begin
  Stream.Seek(-1, soFromCurrent);
end;

procedure TpgBinaryStorage.WriteBool(const Value: boolean);
begin
  if Value then
    WriteInt(1)
  else
    WriteInt(0);
end;

procedure TpgBinaryStorage.WriteElementInfo(AInfoId, AElementId: longword);
begin
  WriteInt(AInfoId);
  WriteInt(AElementId);
end;

procedure TpgBinaryStorage.WriteFloat(const Value: double);
var
  S: single;
  D: double;
begin
  FInfo := cInfoFromMarker[smFloatValue];
  if Value = 0 then
  begin
    FInfo := FInfo or integer(sdValueIsZero);
    Stream.Write(FInfo, 1);
  end else if Value = 1 then
  begin
    FInfo := FInfo or integer(sdValueIsOne);
    Stream.Write(FInfo, 1);
  end else
  begin
    S := Value;
    D := S;
    if D = Value then
    begin
      FInfo := FInfo or integer(sdSingle);
      Stream.Write(FInfo, 1);
      Stream.Write(S, 4);
    end else
    begin
      FInfo := FInfo or integer(sdDouble);
      Stream.Write(FInfo, 1);
      Stream.Write(Value, 8);
    end;
  end;
end;

procedure TpgBinaryStorage.WriteInt(const Value: integer);
begin
  FInfo := cInfoFromMarker[smIntValue];
  WriteIntegerPart(Value);
end;

procedure TpgBinaryStorage.WriteIntegerPart(const Value: integer);
var
  P1: byte;
  P2: word;
  P4: longword;
  Compl: integer;
begin
  if Value = 0 then
  begin
    FInfo := FInfo or integer(sdValueIsZero);
    Stream.Write(FInfo, 1);
  end else if Value = 1 then
  begin
    FInfo := FInfo or integer(sdValueIsOne);
    Stream.Write(FInfo, 1);
  end else if Value > 0 then
  begin
    if Value < 255 then
    begin
      FInfo := FInfo or integer(sdPositive8bits);
      P1 := Value;
      Stream.Write(FInfo, 1);
      Stream.Write(P1, 1);
    end else if Value < 65535 then
    begin
      FInfo := FInfo or integer(sdPositive16bits);
      P2 := Value;
      Stream.Write(FInfo, 1);
      Stream.Write(P2, 2);
    end else
    begin
      FInfo := FInfo or integer(sdPositive32bits);
      P4 := Value;
      Stream.Write(FInfo, 1);
      Stream.Write(P4, 4);
    end;
  end else
  begin
    Compl := -Value;
    if Compl < 255 then
    begin
      FInfo := FInfo or integer(sdNegative8bits);
      P1 := Compl;
      Stream.Write(FInfo, 1);
      Stream.Write(P1, 1);
    end else if Compl < 65535 then
    begin
      FInfo := FInfo or integer(sdNegative16bits);
      P2 := Compl;
      Stream.Write(FInfo, 1);
      Stream.Write(P2, 2);
    end else
    begin
      FInfo := FInfo or integer(sdNegative32bits);
      P4 := Compl;
      Stream.Write(FInfo, 1);
      Stream.Write(P4, 4);
    end;
  end;
end;

procedure TpgBinaryStorage.WriteMarker(AMarker: TpgStorageMarker);
begin
  FInfo := cInfoFromMarker[AMarker];
  Stream.Write(FInfo, 1);
end;

procedure TpgBinaryStorage.WritePropInfo(AInfoId: longword);
begin
  WriteInt(AInfoId);
end;

procedure TpgBinaryStorage.WriteString(const Value: Utf8String);
var
  L: integer;
begin
  FInfo := cInfoFromMarker[smStringValue];
  L := length(Value);
  WriteIntegerPart(L);
  if L > 0 then
    Stream.Write(Value[1], L);
end;

end.
