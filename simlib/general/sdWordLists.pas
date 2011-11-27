unit sdWordLists;

interface

uses
  Classes, SysUtils;

type

  TsdWordList = class(TPersistent)
  private
    FWords: array of string;
    FBreakChar: char;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetWords(Index: integer): string;
    function GetCount: integer;
    procedure SetWords(Index: integer; const Value: string);
  public
    constructor Create(const Value: string; ABreakChar: char = ' '); virtual;
    procedure Clear;
    procedure Add(const AWord: string);
    procedure Delete(Index: integer);
    function IndexOf(const AWord: string): integer;
    property AsString: string read GetAsString write SetAsString;
    property Words[Index: integer]: string read GetWords write SetWords; default;
    property Count: integer read GetCount;
  end;

function BreakString(const Break: char; var Source: string; Leave: boolean): string;

function AlphaNumOnly(const Value: string): string;

implementation

function BreakString(const Break: char; var Source: string; Leave: boolean): string;
var
  Index: integer;
begin
  Index := Pos(Break, Source);
  if Index > 0 then
  begin
    Result := copy(Source, 1, Index - 1);
    Source := copy(Source, Index + 1, length(Source));
  end else
  begin
    if Leave then
    begin
      Result := ''
    end else
    begin
      Result := Source;
      Source := '';
    end;
  end;
end;

function AlphaNumOnly(const Value: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(Value) do
    if upcase(Value[i]) in ['A'..'Z', '0'..'9'] then
      Result := Result + Value[i];
end;

{ TsdWordList }

procedure TsdWordList.Add(const AWord: string);
begin
  SetLength(FWords, length(FWords) + 1);
  FWords[length(FWords) - 1] := trim(AWord);
end;

procedure TsdWordList.Clear;
begin
  SetLength(FWords, 0);
end;

constructor TsdWordList.Create(const Value: string; ABreakChar: char);
begin
  inherited Create;
  FBreakChar := ABreakChar;
  SetAsString(Value);
end;

procedure TsdWordList.Delete(Index: integer);
var
  i: integer;
begin
  if (Index >= 0) and (Index < length(FWords)) then begin
    for i := Index + 1 to length(FWords) - 1 do
      FWords[i - 1] := FWords[i];
    SetLength(FWords, length(FWords) - 1);
  end;
end;

function TsdWordList.GetAsString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to length(FWords) - 1 do begin
    if (length(Result) > 0) and (length(FWords[i]) > 0) then Result := Result + FBreakChar;
    Result := Result + FWords[i];
  end;
end;

function TsdWordList.GetCount: integer;
begin
  Result := length(FWords);
end;

function TsdWordList.GetWords(Index: integer): string;
begin
  if (Index >= 0) and (Index < length(FWords)) then
    Result := FWords[Index]
  else
    Result := '';
end;

function TsdWordList.IndexOf(const AWord: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to length(FWords) - 1 do
    if lowercase(FWords[i]) = lowercase(AWord) then begin
      Result := i;
      exit;
    end;
end;

procedure TsdWordList.SetAsString(const Value: string);
var
  Line: string;
begin
  Clear;
  Line := trim(Value);
  while length(Line) > 0 do begin
    Add(BreakString(FBreakChar, Line, False));
    Line := trim(Line);
  end;
end;

procedure TsdWordList.SetWords(Index: integer; const Value: string);
begin
  if Index < 0 then exit;
  if Index >= length(FWords) then SetLength(FWords, Index + 1);
  FWords[Index] := Value;
end;

end.
