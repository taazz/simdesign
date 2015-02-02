{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Core<p>

  <b>Description:</b><p>
  Utility routines for parsing text.

  <Author: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2004 - 2011 SimDesign BV

  Creation Date:
  13Dec2004

  Modified:
  14apr2011: placed parsing funcs in TpgParser
  19may2011: string > Utf8String
}
unit pgParser;

{$i simdesign.inc}

interface

uses
  SysUtils, sdDebug, Math, Pyro;

type

  // lowlevel parser for SVG scene parsing
  TpgParser = class(TDebugObject)
  public

    // Parse an integer from the string Value starting at NextPos, and adjust NextPos
    // to the position after the integer
    function pgParseInteger(const Value: Utf8String; var NextPos: integer): integer;

    // Parse a floating point from the string Value starting at Nextpos, and adjust NextPos
    // to the position after the integer
    function pgParseNumber(const Value: Utf8String; var NextPos: integer): double; overload;
    function pgParseNumber(const Value: Utf8String): double; overload;

    // Parse a list of numbers which can be separated by comma or space
    procedure pgParseNumberArray(const Value: Utf8String; var Numbers: array of double; var Count: integer);

    // Parse a list of lengths which can be separated by comma or space
    procedure pgParseLengthArray(const Value: Utf8String; var Units: array of TpgLengthUnits;
      var Sizes: array of double; var Count: integer);

  end;

// Break Source string on Match, return first part and store second part in
// Second, trim the result if TrimResult is true, if the match does not exist then
// only return a non-empty string if MustExist is False
function BreakString(const Source, Match: Utf8String; var Second: Utf8String; TrimResult, MustExist: boolean): Utf8String;

// Condition the string by replacing commas, CR and LF by spaces
function pgConditionListString(const Value: Utf8String): Utf8String;

// Skip commas and whitespace chars
procedure SkipCommaWS(const Value: Utf8String; var NextPos: integer);

resourcestring
  spsInvalidNumber         = 'Invalid number (%s)';
  spsInvalidUnitSpecifier  = 'Invalid unit specifier';

implementation

{ TpgParser }

function TpgParser.pgParseNumber(const Value: Utf8String): double;
var
  NextPos: integer;
begin
  NextPos := 1;
  Result := pgParseNumber(Value, NextPos);
end;

function TpgParser.pgParseNumber(const Value: Utf8String; var NextPos: integer): double;
var
  Sign: integer;
  NumStr: Utf8String;
begin
  Result := 0;
  // Sign
  if length(Value) < NextPos then
  begin
    DoDebugOut(Self, wsFail, spsInvalidNumber);
    exit;
  end;
  Sign := 1;
  if Value[NextPos] in ['-','+'] then
  begin
    if Value[NextPos] = '-' then Sign := -1;
    inc(NextPos);
  end;
  if length(Value) < NextPos then
  begin
    DoDebugOut(Self, wsFail, spsInvalidNumber);
    exit;
  end;

  // Base
  NumStr := '';
  while length(Value) >= NextPos do
  begin
    if not (Value[NextPos] in ['0'..'9']) then
      break;
    NumStr := NumStr + Value[NextPos];
    inc(NextPos);
  end;

  if (length(NumStr) = 0) and ((length(Value) < NextPos) or (Value[NextPos] <> '.')) then
  begin
    // cases like 'none'
    DoDebugOut(Self, wsWarn, Format(spsInvalidNumber, [Value]));
    Result := 0;
    exit;
  end;
  Result := StrToIntDef(NumStr, 0) * Sign;
  if length(Value) < NextPos then
    exit;
  if Value[NextPos] = '.' then
    inc(NextPos);
  if length(Value) < NextPos then
    exit;

  // Fraction
  NumStr := '';
  while length(Value) >= NextPos do
  begin
    if not (Value[NextPos] in ['0'..'9']) then
      break;
    NumStr := NumStr + Value[NextPos];
    inc(NextPos);
  end;
  if length(NumStr) > 0 then
    Result := Result + Sign * StrToInt(NumStr) / IntPower(10, length(NumStr));

  // Scientific
  if length(Value) < NextPos + 1 then
    exit;
  if not ((Value[NextPos] in ['e', 'E']) and (Value[NextPos + 1] in ['0'..'9', '+', '-'])) then
    exit;
  NextPos := NextPos + 1;
  Result := Result * IntPower(1, pgParseInteger(Value, NextPos));
end;

function TpgParser.pgParseInteger(const Value: Utf8String; var NextPos: integer): integer;
var
  Sign: integer;
  NumStr: Utf8String;
begin
  Result := 0;
  if length(Value) <= NextPos then
  begin
    DoDebugOut(Self, wsWarn, Format(spsInvalidNumber, [Value]));
    exit;
  end;

  Sign := 1;
  if Value[NextPos] in ['-','+'] then
  begin
    if Value[NextPos] = '-' then Sign := -1;
    inc(NextPos);
  end;
  if length(Value) < NextPos then
  begin
    DoDebugOut(Self, wsWarn, Format(spsInvalidNumber, [Value]));
    exit;
  end;

  // Base
  NumStr := '';
  while length(Value) >= NextPos do
  begin
    if not (Value[NextPos] in ['0'..'9']) then
      break;
    NumStr := NumStr + Value[NextPos];
    inc(NextPos);
  end;
  if length(NumStr) = 0 then
  begin
    DoDebugOut(Self, wsWarn, Format(spsInvalidNumber, [Value]));
    exit;
  end;
  Result := Sign * StrToInt(NumStr);
end;

procedure TpgParser.pgParseNumberArray(const Value: Utf8String; var Numbers: array of double; var Count: integer);
var
  MaxCount, APos: integer;
  NumStr, Next: Utf8String;
begin
  Next := pgConditionListString(Value);
  MaxCount := Count;
  FillChar(Numbers[0], MaxCount * SizeOf(double), 0);
  Count := 0;
  while Count < MaxCount do
  begin
    NumStr := BreakString(Next, ' ', Next, True, False);
    if length(NumStr) = 0 then
      break;
    APos := 1;
    Numbers[Count] := pgParseNumber(NumStr, APos);
    inc(Count);
  end;
end;

procedure TpgParser.pgParseLengthArray(const Value: Utf8String; var Units: array of TpgLengthUnits;
  var Sizes: array of double; var Count: integer);
var
  MaxCount: integer;
  NumStr, Next: Utf8String;
begin
  Next := pgConditionListString(Value);
  MaxCount := Count;
  Count := 0;
  while Count < MaxCount do begin
    NumStr := BreakString(Next, ' ', Next, True, False);
    if length(NumStr) = 0 then
      break;
    //todo.. where did it go? pgParseLength(NumStr, Units[Count], Sizes[Count]);
    inc(Count);
  end;
end;

{ local functions}

function BreakString(const Source, Match: Utf8String; var Second: Utf8String; TrimResult, MustExist: boolean): Utf8String;
var
  APos: integer;
begin
  Result := '';
  APos := AnsiPos(Match, Source);
  if APos > 0 then
  begin
    Result := copy(Source, 1, APos - 1);
    Second := copy(Source, APos + 1, length(Source));
    if TrimResult then
    begin
      Result := trim(Result);
      Second := trim(Second);
    end;
  end else
  begin
    if MustExist then
    begin
      if TrimResult then
        Second := trim(Source)
      else
        Second := Source;
    end else
    begin
      if TrimResult then
        Result := trim(Source)
      else
        Result := Source;
      Second := '';
    end;
  end;
end;

function pgConditionListString(const Value: Utf8String): Utf8String;
begin
  Result := StringReplace(Value, ',', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #$A, ' ', [rfReplaceAll]);
  Result := trim(StringReplace(Result, #$D, ' ', [rfReplaceAll]));
end;

procedure SkipCommaWS(const Value: Utf8String; var NextPos: integer);
begin
  while (length(Value) >= NextPos)
    and (Value[NextPos] in [',', #9, #10, #13, #32]) do
    inc(NextPos);
end;

end.
