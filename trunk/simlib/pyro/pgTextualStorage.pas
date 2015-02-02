{ Project: Pyro
  Module: Pyro Core

  Description:
  Special storage class storing a container in textual form (comparable to DFM)

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgTextualStorage;

interface

uses
  Classes, SysUtils, pgDocument, pgBase64Encode, sdDebug, Pyro;

type

  TSetOfChar = set of Char;

  // reference implementation for textual storage of containers
  TpgTextualStorage = class(TpgStorage)
  private
    //FMarker: TpgStorageMarker;
    FLevel: integer;
    FRewindPos: int64;
    FCurrentElementInfo: TpgElementInfo;
    function RString(const ALength: integer): Utf8String; overload;
    function RString: Utf8String; overload;
    function RString(const ATerminators: TSetOfChar): Utf8String; overload;
    procedure WString(const Value: Utf8String);
    procedure WIndent(Level: integer);
  protected
    procedure StartLoad; override;
    procedure StartSave; override;
    function ReadMarker: TpgStorageMarker; override;
    procedure RewindMarker; override;
    procedure WriteMarker(AMarker: TpgStorageMarker); override;
    procedure ReadElementInfo(var AInfoId, AElementId: longword); override;
    procedure WriteElementInfo(AInfoId, AElementId: longword); override;
    procedure ReadPropInfo(var AInfoId: longword); override;
    procedure WritePropInfo(AInfoId: longword); override;
    procedure SkipProp; override;
  public
    function ReadBool: boolean; override;
    procedure WriteBool(const Value: boolean); override;
    function ReadInt: integer; override;
    procedure WriteInt(const Value: integer); override;
    function ReadFloat: double; override;
    procedure WriteFloat(const Value: double); override;
    function ReadString: Utf8String; override;
    procedure WriteString(const Value: Utf8String); override;
    function ReadBinary: RawByteString; override;
    procedure WriteBinary(const Value: RawByteString); override;
  end;

implementation

const

  cQuoteChar = #$27; // single quote (')

{ TpgTextualStorage }

function TpgTextualStorage.ReadBinary: RawByteString;
begin
  Result := DecodeBase64(ReadString);
end;

function TpgTextualStorage.ReadBool: boolean;
begin
  if RString = 'true' then
    Result := True
  else
    Result := False;
end;

procedure TpgTextualStorage.ReadElementInfo(var AInfoId, AElementId: longword);
var
  ClassName: Utf8String;
begin
  ClassName := RString;
  FCurrentElementInfo := GetElementInfoByName(ClassName);
  if assigned(FCurrentElementInfo) then
    AInfoID := FCurrentElementInfo.ID
  else
    AInfoID := 0;
  RString; //'id'
  RString; //'='
  AElementId := StrToIntDef(RString, 0);
end;

function TpgTextualStorage.ReadFloat: double;
begin
  Result := StrToFloatDef(RString, 0);
end;

function TpgTextualStorage.ReadInt: integer;
begin
  Result := StrToIntDef(RString, 0);
end;

function TpgTextualStorage.ReadMarker: TpgStorageMarker;
var
  S: Utf8String;
begin
  FRewindPos := Stream.Position;
  S := RString;
  if S = 'object' then
  begin
    Result := smElementStart
  end else
    if S = 'end' then
    begin
      Result := smElementClose;
    end else
    begin
      if length(S) > 0 then
      begin
        Result := smPropStart;
        Stream.Position := FRewindPos;
      end else
        Result := smEndOfFile;
    end;
end;

procedure TpgTextualStorage.ReadPropInfo(var AInfoId: longword);
var
  Info: TpgPropInfo;
begin
  AInfoId := 0;
  if not assigned(FCurrentElementInfo) then
    exit;
  Info := GetPropInfoByName(RString, FCurrentElementInfo.ElementClass);
  RString; // skip '='
  if not assigned(Info) then
    exit;
  AInfoId := Info.Id;
end;

function TpgTextualStorage.ReadString: Utf8String;
begin
  Result := AnsiDequotedStr(RString, cQuoteChar);
end;

procedure TpgTextualStorage.RewindMarker;
begin
  Stream.Position := FRewindPos;
end;

function TpgTextualStorage.RString(const ALength: integer): Utf8String;
// Read string of length ALength from stream
begin
  SetLength(Result, ALength);
  if ALength > 0 then
    Stream.Read(Result[1], ALength);
end;

function TpgTextualStorage.RString: Utf8String;
// Read string until the standard terminator chars, then skip the terminator
// chars.
const
  cTerminators: TSetOfChar = [#$0A, #$0D, #$20];
begin
  Result := RString(cTerminators);
end;

function TpgTextualStorage.RString(const ATerminators: TSetOfChar): Utf8String;
// Read up till first terminator char, and skip over additonal ones in ATerminators
var
  Ch: Char;
  Len: integer;
begin
  Result := '';
  // Read characters up till first terminator, add to result
  repeat
    Len := Stream.Read(Ch, 1);
    if (Len > 0) and not (Ch in ATerminators) then
      Result := Result + Ch;
  until (Ch in ATerminators) or (Len = 0);
  // Skip over additional terminators
  while (Len > 0) and (Ch in ATerminators) do
  begin
    Len := Stream.Read(Ch, 1);
  end;
  if Len > 0 then
    Stream.Seek(-1, soCurrent);
end;

procedure TpgTextualStorage.SkipProp;
// Since we do not have a true terminator for endprop, the #13#10 functions as such
// and we read up till that
begin
  RString([#$0A,#$0D]);
end;

procedure TpgTextualStorage.StartLoad;
begin
  // Rewind
  inherited;
  RString(3); // skip UTF8 BOM
end;

procedure TpgTextualStorage.StartSave;
const
  cUtf8BOM: AnsiString = #$EF#$BB#$BF;
begin
  // Rewind
  inherited;
  // Write BOM marker for UTF8
  WString(Utf8String(cUtf8Bom));
end;

procedure TpgTextualStorage.WIndent(Level: integer);
var
  i: integer;
begin
  // Write indentation
  for i := 0 to Level - 1 do
    WString('  ');
end;

procedure TpgTextualStorage.WriteBinary(const Value: RawByteString);
begin
  WriteString(EncodeBase64(Value));
end;

procedure TpgTextualStorage.WriteBool(const Value: boolean);
begin
  case Value of
  True: WString(' true');
  False: WString(' false');
  end;
end;

procedure TpgTextualStorage.WriteElementInfo(AInfoId, AElementId: longword);
var
  Info: TpgElementInfo;
  ClassName: Utf8String;
begin
  ClassName := 'Unknown';
  Info := GetElementInfoById(AInfoId);
  if assigned(Info) then
    ClassName := Info.Name;
  WString(Format(' %s id = %d'#13#10, [ClassName, AElementId]));
end;

procedure TpgTextualStorage.WriteFloat(const Value: double);
begin
  WString(' ' + FloatToStr(Value));
end;

procedure TpgTextualStorage.WriteInt(const Value: integer);
begin
  WString(' ' + IntToStr(Value));
end;

procedure TpgTextualStorage.WriteMarker(AMarker: TpgStorageMarker);
begin
  case AMarker of
  smElementStart:
    begin
      WIndent(FLevel);
      WString('object');
      inc(FLevel);
    end;
  smElementClose:
    begin
      dec(FLevel);
      WIndent(FLevel);
      WString('end'#13#10);
    end;
  smPropStart:
    begin
      WIndent(FLevel);
    end;
  smPropClose:
    begin
      WString(#13#10);
    end;
  end;
end;

procedure TpgTextualStorage.WritePropInfo(AInfoId: longword);
var
  Info: TpgPropInfo;
  PropName: Utf8String;
begin
  PropName := 'Unknown';
  Info := GetPropInfo(AInfoId);
  if assigned(Info) then
    PropName := Info.Name;
  WString(PropName + ' =');
end;

procedure TpgTextualStorage.WriteString(const Value: Utf8String);
var
  S: Utf8String;
begin
  S := AnsiQuotedStr(Value, cQuoteChar);
  WString(' ' + S);
end;

procedure TpgTextualStorage.WString(const Value: Utf8String);
var
  Count: integer;
begin
  Count := length(Value);
  if Count > 0 then
  Stream.Write(Value[1], Count);
end;

end.
