{ Unit sdWidestrings

  This unit provides helper objects that can deal with widestring.

  Created: 23-Sept-2004

  Copyright (c) 2004 By Nils Haeck M.Sc. - SimDesign
  More information: www.simdesign.nl or n.haeck@simdesign.nl

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.

  Contributor: JohnF (JF)
  JF 2010-05-26: Modified GetLineLengthFrom

}
unit sdWidestrings;

interface

uses
  Classes, Contnrs, SysUtils, Math;

type

  TsdWidestringObj = class
  private
    FValue: widestring;
  public
    property Value: widestring read FValue write FValue;
  end;

  // TsdWidestringList is the equivalent (however simple and without all options)
  // of a TStringList, but then supporting Widestring.
  TsdWidestringList = class(TPersistent)
  private
    FStrings: TObjectList;
    FUpdateCount: integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    function GetCount: integer;
    function GetStrings(Index: integer): widestring;
    procedure SetStrings(Index: integer; const Value: widestring);
    procedure DoChange;
    procedure DoChanging;
    function GetText: widestring;
    procedure SetText(const Value: widestring);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetUpdateState(Updating: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(const S: widestring): integer; virtual;
    procedure AddStrings(Strings: TsdWidestringList); virtual;
    procedure Clear; virtual;
    property Count: integer read GetCount;
    property Strings[Index: integer]: widestring read GetStrings write SetStrings; default;
    property Text: widestring read GetText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

function CopyWide(const WS: widestring; Index, Count: integer): widestring;
function GetLineLengthFrom(const AText: widestring; var APos: integer): integer;

resourcestring

  swsListIndexOutOfBounds = 'List index out of bounds';

implementation

function CopyWide(const WS: widestring; Index, Count: integer): widestring;
var
  L: integer;
begin
  Result := '';
  L := length(WS);
  if (Index > L) or (Index < 1) then
    exit;
  Count := Min(L - Index + 1, Count);
  if Count = 0 then
    exit;
  SetLength(Result, Count);
  Move(WS[Index], Result[1], Count * 2);
end;

function GetLineLengthFrom(const AText: widestring; var APos: integer): integer;
var
  L: integer;
begin
  Result := -1;
  L := length(AText);
  if APos > L then
  begin
    if (L <> 0) and (ord(AText[APos - 1]) in [10,13]) then
      Result:= -2; // -2 signals calling routine to add empty span
    exit;
  end;
  Result := 0;
  while not (APos > L) and not (ord(AText[APos]) in [10,13]) do
  begin
    inc(Result);
    inc(APos);
  end;
  if AText[APos] = #13 then Inc(APos);
  if AText[APos] = #10 then Inc(APos);
end;

{ TsdWidestringList }

function TsdWidestringList.Add(const S: widestring): integer;
var
  Obj: TsdWidestringObj;
begin
  BeginUpdate;
  try
    Obj := TsdWidestringObj.Create;
    Obj.Value := S;
    if assigned(FStrings) then
      Result := FStrings.Add(Obj)
    else
      Result := -1;
  finally
    EndUpdate;
  end;
end;

procedure TsdWidestringList.AddStrings(Strings: TsdWidestringList);
var
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    Add(Strings[i]);
end;

procedure TsdWidestringList.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TsdWidestringList then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TsdWidestringList(Source));
    finally
      EndUpdate;
    end;
    exit;
  end else if Source is TStrings then
  begin
    // Add a ASCII TStrings list
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(Source).Count - 1 do
        Add(TStrings(Source).Strings[i]);
    finally
      EndUpdate;
    end;
    exit;
  end;
  inherited Assign(Source);
end;

procedure TsdWidestringList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TsdWidestringList.Clear;
begin
  if assigned(FStrings) then
    FStrings.Clear;
end;

constructor TsdWidestringList.Create;
begin
  inherited Create;
  FStrings := TObjectList.Create;
end;

destructor TsdWidestringList.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TsdWidestringList.DoChange;
begin
  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TsdWidestringList.DoChanging;
begin
  if assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TsdWidestringList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TsdWidestringList.GetCount: integer;
begin
  if assigned(FStrings) then
    Result := FStrings.Count
  else
    Result := 0;
end;

function TsdWidestringList.GetStrings(Index: integer): widestring;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TsdWidestringObj(FStrings[Index]).Value
  else
    raise Exception.Create(swsListIndexOutOfBounds);
end;

function TsdWidestringList.GetText: widestring;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 2 do
    Result := Result + Strings[i] + #13#10;
  if Count > 0 then
    Result := Result + Strings[Count - 1];
end;

procedure TsdWidestringList.SetStrings(Index: integer; const Value: widestring);
var
  Obj: TsdWidestringObj;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Obj := TsdWidestringObj(FStrings[Index]);
    if Obj.Value <> Value then
    begin
      BeginUpdate;
      try
        Obj.Value := Value;
      finally
        EndUpdate;
      end;
    end;
  end else
    raise Exception.Create(swsListIndexOutOfBounds);
end;

procedure TsdWidestringList.SetText(const Value: widestring);
var
  P, Start: PWideChar;
  S: widestring;
  Len: integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        Len := 0;
        while not (ord(P^) in [0, 10, 13]) do
        begin
          inc(Len);
          inc(P);
        end;
        SetLength(S, Len);
        if Len > 0 then
          Move(Start^, S[1], Len * 2);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TsdWidestringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    DoChanging
  else
    DoChange;
end;

end.
