{ unit sdPdfUtil

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements utility procedures and functions that are used to
  parse and write PDF streams. Information on these methods can be found in:
  chapter 3.1: Syntax - Lexical Conventions

  Author: Nils Haeck M.Sc.

  Changes:
    04Jan2004 - Created

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfUtil;

interface

uses
  Classes, SysUtils, Math;

type
  // PDF-related exception that can be raised in PDF handling code
  EPdfError = class(Exception);

// Find the positon of Phrase in S, starting at current stream position and up to
// Limit or end of the stream (whichever is first). Returns the first position of
// Phrase
function PosInStream(S: TStream; Phrase: string; Limit: integer = 0): integer;

// Find the positon of Phrase in S, starting at current stream position and moving
// backwards until beginning of stream or S.Position = Limit (whichever is first).
// Returns the first occurrance of Phrase
function PosInStreamBackwards(S: TStream; Phrase: string; Limit: integer = 0): integer;

// Read a line from the stream starting at its position until first EOL. Adjust
// the position to the first byte after the EOL marker(s).
function ReadLineFromPdfStream(S: TStream): string;

// Read a token from the stream starting at its position until first whitespace.
// Adjust the position to the first byte after the whitespace
function ReadTokenFromPdfStream(S: TStream): string;

// Read a token from the stream starting at its position backwards until first whitespace.
// Adjust the position to the first byte after the whitespace
function ReadTokenFromPdfStreamBackwards(S: TStream): string;

// Read the next character which is not a whitespace or EOL
function ReadNextNonWS(S: TStream): char;

// Read the next token and convert to integer
function ReadIntegerFromPdf(S: TStream): integer;

// Skip the whitespaces in S and position the stream at the first non-white space
procedure SkipWhitespace(S: TStream);

const

  cPdfDelimiter  = ['(', ')', '<', '>', '[', ']', '{', '}', '/', '%'];
  cPdfWhiteSpace = [#0, #9, #10, #12, #13, #32];
  cPdfEOLChars   = [#10, #13];


implementation

uses
  sdPdfObjects;

function PosInStream(S: TStream; Phrase: string; Limit: integer = 0): integer;
// Find the positon of Phrase in S, starting at current stream position and up to
// Limit or end of the stream (whichever is first). Returns the first position of
// Phrase
var
  AIndex: integer;
  Ch: char;
begin
  Result := -1;
  if not assigned(S) then exit;
  AIndex := 1;
  if Limit = 0 then Limit := S.Size;
  while AIndex <= length(Phrase) do begin
    if S.Read(Ch, 1) = 0 then exit;
    if S.Position > Limit then exit;
    if Phrase[AIndex] = Ch then
      inc(AIndex)
    else
      AIndex := 1;
  end;
  Result := S.Position - AIndex + 1;
end;

function PosInStreamBackwards(S: TStream; Phrase: string; Limit: integer = 0): integer;
// Find the positon of Phrase in S, starting at current stream position and moving
// backwards until beginning of stream or S.Position = Limit (whichever is first).
// Returns the first occurrance of Phrase
var
  AIndex: integer;
  Ch: char;
begin
  Result := -1;
  if not assigned(S) then exit;
  Limit := Max(0, Limit);
  AIndex := length(Phrase);
  while AIndex > 0 do begin
    if S.Position <= Limit then exit;
    // Read a character backwards
    S.Seek(-1, soFromCurrent);
    S.Read(Ch, 1);
    S.Seek(-1, soFromCurrent);
    if Phrase[AIndex] = Ch then
      dec(AIndex)
    else
      AIndex := length(Phrase);
  end;
  Result := S.Position + 1;
end;

function ReadLineFromPdfStream(S: TStream): string;
// Read a line from the stream starting at its position until first EOL. Adjust
// the position to the first byte after the EOL marker(s).
var
  Ch: char;
begin
  // We can make this code more efficient later, by preallocating some room
  // for the result string
  Result := '';
  repeat
    if S.Read(Ch, 1) = 0 then exit;
    if Ch in cPdfEOLChars then exit;
    Result := Result + Ch;
  until False;
  repeat
    if S.Read(Ch, 1) = 0 then exit;
    if not (Ch in cPdfEOLChars) then begin
      S.Seek(-1, soFromCurrent);
      exit;
    end;
  until False;
end;

function ReadTokenFromPdfStream(S: TStream): string;
// Read a token from the stream starting at its position until first whitespace.
// Adjust the position to the first byte after the whitespace
var
  Ch: char;
// main
begin
  // We can make this code more efficient later, by preallocating some room
  // for the result string
  Result := '';
  repeat
    if S.Read(Ch, 1) = 0 then exit;
    if Ch in cPdfWhiteSpace then begin
      if length(Result) > 0 then begin
        // If we already have data, encountering a WS means end of token
        S.Seek(-1, soFromCurrent);
        break;
      end;
      continue;
    end;
    if Ch in cPdfDelimiter then begin
      S.Seek(-1, soFromCurrent);
      break;
    end;
    if Ch = '%' then begin
      // Skip comments
      ReadLineFromPdfStream(S);
      continue;
    end;
    Result := Result + Ch;
  until False;
end;

function ReadTokenFromPdfStreamBackwards(S: TStream): string;
// Read a token from the stream starting at its position backwards until first whitespace.
// Adjust the position to the first byte after the whitespace
var
  Ch: char;
// local
procedure SkipWhitespace;
begin
  repeat
    if S.Position <= 0 then exit;
    // Read a character backwards
    S.Seek(-1, soFromCurrent);
    S.Read(Ch, 1);
    S.Seek(-1, soFromCurrent);
    if not (Ch in cPdfWhiteSpace) then begin
      S.Seek(1, soFromCurrent);
      exit;
    end;
  until False;
end;
// main
begin
  // We can make this code more efficient later, by preallocating some room
  // for the result string
  Result := '';
  repeat
    if S.Position <= 0 then exit;
    // Read a character backwards
    S.Seek(-1, soFromCurrent);
    S.Read(Ch, 1);
    S.Seek(-1, soFromCurrent);
    if Ch in cPdfWhiteSpace then begin
      if length(Result) > 0 then break;
    end else
      Result := Ch + Result;
  until False;
  SkipWhiteSpace;
end;

function ReadNextNonWS(S: TStream): char;
// Read the next character which is not a whitespace or EOL
var
  Count: integer;
begin
  Result := #0;
  repeat
    Count := S.Read(Result, 1);
    if not
      ((Result in cPdfWhiteSpace) or
       (Result in cPdfEOLChars)   or
       (Result = '%')) then exit;
    // Deal with comments
    if Result = '%' then
      ReadLineFromPdfStream(S);
  until Count = 0;
  // Arriving here means no char could be read
  raise EPdfError.Create(sPdfUnexpectedEOS);
end;

function ReadIntegerFromPdf(S: TStream): integer;
// Read the next token and convert to integer
var
  ANumber: TPdfNumber;
begin
  ANumber:= TPdfNumber.Create;
  try
    ANumber.LoadFromPdf(nil, S);
    Result := ANumber.AsInteger;
  finally
    ANumber.Free;
  end;
end;

procedure SkipWhitespace(S: TStream);
// Skip the whitespaces in S and position the stream at the first non-white space
var
  Ch: char;
begin
  repeat
    if S.Read(Ch, 1) = 0 then exit;
    if not (Ch in cPdfWhiteSpace) then begin
      S.Seek(-1, soFromCurrent);
      exit;
    end;
  until False;
end;

end.
