unit sdMapOddBPP;

interface

uses
  sdMapIterator;

procedure Convert8bitTo1bit(Src, Dst: TsdMapIterator; ALimit: integer = $80);
procedure Convert1bitTo8bit(Src, Dst: TsdMapIterator; Val0: byte = $00; Val1: byte = $FF);

procedure Convert8bitTo2bit(Src, Dst: TsdMapIterator);
procedure Convert2bitTo8bit(Src, Dst: TsdMapIterator);

implementation

procedure Convert8bitTo1bit(Src: TsdMapIterator; Dst: TsdMapIterator; ALimit: integer);
var
  x, y, Bit: integer;
  SP, DP: Pbyte;
const
  cBitMask: array[0..7] of byte = ($01, $02, $04, $08, $10, $20, $40, $80);
begin
  SP := Src.First;
  for y := 0 to Src.Height - 1 do
  begin
    DP := Dst.Map;
    inc(DP, y * Dst.ScanStride);
    DP^ := 0;
    Bit := 7;
    for x := 0 to Src.Width - 1 do
    begin
      if Bit < 0 then
      begin
        Bit := 7;
        inc(DP);
        DP^ := 0;
      end;
      if SP^ >= ALimit then
        inc(DP^, cBitMask[Bit]);
      dec(Bit);
      SP := Src.Next;
    end;
  end;
end;

procedure Convert1bitTo8bit(Src: TsdMapIterator; Dst: TsdMapIterator; Val0, Val1: byte);
var
  x, y, Bit: integer;
  SP, DP: Pbyte;
const
  cBitMask: array[0..7] of byte = ($01, $02, $04, $08, $10, $20, $40, $80);
begin
  for y := 0 to Src.Height - 1 do
  begin
    SP := Src.Map;
    inc(SP, y * Src.ScanStride);
    DP := Dst.Map;
    inc(DP, y * Dst.ScanStride);
    Bit := 7;
    for x := 0 to Src.Width - 1 do
    begin
      if SP^ and cBitMask[Bit] > 0 then
        DP^ := Val1
      else
        DP^ := Val0;
      dec(Bit);
      if Bit < 0 then
      begin
        Bit := 7;
        inc(SP);
      end;
      inc(DP);
    end;
  end;
end;

procedure Convert8bitTo2bit(Src, Dst: TsdMapIterator);
var
  x, y, Twip: integer;
  SP, DP: Pbyte;
const
  cTwipShift: array[0..3] of byte = (6, 4, 2, 0);
begin
  SP := Src.First;
  for y := 0 to Src.Height - 1 do
  begin
    DP := Dst.Map;
    inc(DP, y * Dst.ScanStride);
    DP^ := 0;
    Twip := 3;
    for x := 0 to Src.Width - 1 do
    begin
      if Twip < 0 then
      begin
        Twip := 3;
        inc(DP);
        DP^ := 0;
      end;
      inc(DP^, (SP^ shr 6) shl cTwipShift[Twip]);
      dec(Twip);
      SP := Src.Next;
    end;
  end;
end;

procedure Convert2bitTo8bit(Src, Dst: TsdMapIterator);
var
  x, y, Twip: integer;
  SP, DP: Pbyte;
const
  cTwipShift: array[0..3] of byte = (6, 4, 2, 0);
  cTwipVal: array[0..3] of byte = ($00, $55, $AA, $FF);
begin
  for y := 0 to Src.Height - 1 do
  begin
    SP := Src.Map;
    inc(SP, y * Src.ScanStride);
    DP := Dst.Map;
    inc(DP, y * Dst.ScanStride);
    Twip := 3;
    for x := 0 to Src.Width - 1 do
    begin
      DP^ := cTwipVal[(SP^ shr cTwipShift[Twip]) and $03];
      inc(DP);
      dec(TWip);
      if Twip < 0 then
      begin
        Twip := 3;
        inc(SP);
      end;
    end;
  end;
end;

end.
