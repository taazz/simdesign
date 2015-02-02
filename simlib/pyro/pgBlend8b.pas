{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Render<p>

  <b>Description:</b><p>
    8bits/channel blending operations

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgBlend8b;

interface

{$i simdesign.inc}

uses
  pgBlend, Pyro, sdDebug;

// adjust alpha

procedure Pas_Org_4Ch_x_x_8b_AdjustAlpha(PColSrc, PColDst: pointer; Mul: integer);
procedure Pas_Pre_4Ch_x_x_8b_AdjustAlpha(PColSrc, PColDst: pointer; Mul: integer);

// blending

// These should have a buffer of length Count (x32bits) in case a cover array is defined

procedure Pas_Org_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
procedure Pas_Pre_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);

// blend with color operation

// These should have a buffer of length Count (x32bits) without cover, and 2 * Count
// with cover.

procedure Pas_x_4Ch_Add_x_8b_ColorOp(var Info: TpgBlendInfo);

// Color operations

procedure Pas_x_4Ch_Add_x_8b_ColorOpOver(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpAdd(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpSub(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpDiv(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpModulate(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpMax(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpMin(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpDifference(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpExclusion(PColA, PColB, PColDst: pointer);
procedure Pas_x_4Ch_Add_x_8b_ColorOpAverage(PColA, PColB, PColDst: pointer);

// Clearing

procedure Asm_FillLongword(var X; Count: Integer; Value: Longword);

implementation

// Adjust alpha

procedure Pas_Org_4Ch_x_x_8b_AdjustAlpha(PColSrc, PColDst: pointer; Mul: integer);
var
  A: byte;
begin
  PpgColorARGB(PColDst)^ := PpgColorARGB(PColSrc)^;
  A := PpgColorARGB(PColSrc).A;
  if A = $FF then
    PpgColorARGB(PColDst).A := Mul
  else
    PpgColorARGB(PColDst).A := pgIntMul(Mul, A);
end;

procedure Pas_Pre_4Ch_x_x_8b_AdjustAlpha(PColSrc, PColDst: pointer; Mul: integer);
var
  i: integer;
  Ps, Pd: pbyte;
begin
  if (Mul = $FF) then begin
    PpgColorARGB(PColDst)^ := PpgColorARGB(PColSrc)^;
    exit;
  end;
  Ps := pbyte(PColSrc);
  Pd := pbyte(PColDst);
  for i := 0 to 3 do begin
    Pd^ := pgIntMul(Mul, Ps^);
    inc(Pd); inc(Ps);
  end;
end;

// Blending

procedure Pas_Org_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
var
  i, Count: integer;
  PSrc, PDst, PBuf: PpgColorARGB;
  PCvr: PByte;
  Sa, Da, Ra, InvRa, RaD, IncSrc: integer;
  Dc, Sc: Pbyte;
  t1, t2: integer;
begin
  Count := Info.Count;
  PSrc := Info.PColSrc;
  PDst := Info.PColDst;

  // Adjust for cover
  IncSrc := Info.IncSrc;
  if Info.PCvrSrc <> nil then begin
    // Put src into buf
    PBuf := Info.Buffer;
    PCvr := Info.PCvrSrc;
    if (Info.IncSrc = 0) and (Info.IncCvr = 0) then
      Count := 1
    else
      IncSrc := 1;
    while Count > 0 do begin
      Pas_Org_4Ch_x_x_8b_AdjustAlpha(PSrc, PBuf, PCvr^);
      inc(PSrc, Info.IncSrc);
      inc(PBuf);
      inc(PCvr, Info.IncCvr);
      dec(Count);
    end;
    // Readjust some values
    PSrc := Info.Buffer;
    Count := Info.Count;
  end;

  while Count > 0 do begin
    // source alpha
    Sa := PSrc.A;
    if Sa = $FF then begin
      // Full cover: dest = source
      PDst^ := PSrc^;
    end else begin
      // If Sa = $00 we just keep dest
      if Sa > $00 then begin
        // Dest alpha
        Da := PDst.A;
        if Da = $00 then begin
          // nothing to cover: dest = source
          PDst^ := PSrc^;
        end else begin
          if Da = $FF then begin
            // We can shortcut for Da = $FF because resulting alpha is always $FF

            // Pointers to source and dest color, set dest colors
            Sc := pbyte(PSrc); Dc := pbyte(PDst);
            for i := 0 to 2 do begin

              t1 := Sa * Sc^ + $80;
              t2 := ($FF - Sa) * Dc^ + $80;
              Dc^ := (t1 shr 8 + t1) shr 8 + (t2 shr 8 + t2) shr 8;

              inc(Sc); inc(Dc);
            end;
            // Set dest alpha
            Dc^ := $FF;

          end else begin
            // Resulting alpha Ra = Sa + Da - Sa * Da
            Ra := Sa + Da - pgIntMul(Sa, Da);
            InvRa := (255 * 256) div Ra; // shl 8

            // Pointers to source and dest color, set dest colors
            Sc := pbyte(PSrc); Dc := pbyte(PDst);
            for i := 0 to 2 do begin

              // blending formula for original, non-premultiplied colors:
              // R' = Sa * (S - (Ra * D)) + (Ra * D)
              // R = R' / Ra
              RaD := pgIntMul(Da, Dc^);
              Dc^ := (Sa * (Sc^ - RaD) div 255 + RaD) * InvRa shr 8;

              inc(Sc); inc(Dc);
            end;
            // Set dest alpha
            Dc^ := Ra;
          end;
        end;
      end;
    end;
    inc(PDst);
    inc(PSrc, IncSrc);
    dec(Count);
  end;
end;

procedure Pas_Pre_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
var
  t, Count: integer;
  PSrc, PDst, PBuf: PpgColorARGB;
  PCvr: PByte;
  Sa, Da, IncSrc: integer;
  Dc, Sc: Pbyte;
begin
  Count := Info.Count;
  PSrc := Info.PColSrc;
  PDst := Info.PColDst;

  // Adjust for cover
  IncSrc := Info.IncSrc;
  if Info.PCvrSrc <> nil then begin
    // Put src into buf
    PBuf := Info.Buffer;
    PCvr := Info.PCvrSrc;
    if (Info.IncSrc = 0) and (Info.IncCvr = 0) then
      Count := 1
    else
      IncSrc := 1;
    while Count > 0 do begin
      Pas_Pre_4Ch_x_x_8b_AdjustAlpha(PSrc, PBuf, PCvr^);
      inc(PSrc, Info.IncSrc);
      inc(PBuf);
      inc(PCvr, Info.IncCvr);
      dec(Count);
    end;
    // Re-adjust some values
    PSrc := Info.Buffer;
    Count := Info.Count;
  end;

  while Count > 0 do begin
    // source alpha
    Sa := PSrc.A;
    if Sa = $FF then begin
      // Full cover: dest = source
      PDst^ := PSrc^;
    end else begin
      // If Sa = $00 we just keep dest
      if Sa > $00 then begin
        // Dest alpha
        Da := PDst.A;
        if Da = $00 then begin
          // nothing to cover: dest = source
          PDst^ := PSrc^;
        end else begin
          // Resulting color: R = S + D - Sa * D
          Sc := pbyte(PSrc); Dc := pbyte(PDst);
{         for i := 0 to 3 do begin
            Dc^ := Sc^ + Dc^ - IntMul(Sa, Dc^);
            inc(Sc); inc(Dc);
          end;}
          // optimized (unrolled) version:
          t := Sa * Dc^ + $80;
          Dc^ := Sc^ + Dc^ - (t shr 8 + t) shr 8;
          inc(Sc); inc(Dc);
          t := Sa * Dc^ + $80;
          Dc^ := Sc^ + Dc^ - (t shr 8 + t) shr 8;
          inc(Sc); inc(Dc);
          t := Sa * Dc^ + $80;
          Dc^ := Sc^ + Dc^ - (t shr 8 + t) shr 8;
          inc(Sc); inc(Dc);
          t := Sa * Dc^ + $80;
          Dc^ := Sc^ + Dc^ - (t shr 8 + t) shr 8;
        end;
      end;
    end;
    inc(PDst);
    inc(PSrc, IncSrc);
    dec(Count);
  end;
end;

// Blend with color operation

procedure Pas_x_4Ch_Add_x_8b_ColorOp(var Info: TpgBlendInfo);
var
  Count: integer;
  PSrc, PDst, PBuf: PpgColorARGB;
  InfoOut: TpgBlendInfo;
begin
  // Create a list of source colors based on the operation
  Count := Info.Count;
  PSrc := Info.PColSrc;
  PDst := Info.PColDst;
  PBuf := Info.Buffer;

  if (Info.IncSrc = 0) then Count := 1;
  while Count > 0 do begin
    Info.ColorOp(PSrc, PDst, PBuf);
    inc(PSrc, Info.IncSrc);
    inc(PDst);
    inc(PBuf);
    dec(Count);
  end;

  // Readjust some values
  InfoOut := Info;
  InfoOut.PColSrc := Info.Buffer;
  InfoOut.Buffer := PBuf;

  // Call normal blend
  TpgBlendFunc(Info.BlendFunc)(InfoOut);
end;

// Color operations

procedure Pas_x_4Ch_Add_x_8b_ColorOpOver(PColA, PColB, PColDst: pointer);
// For reference only: OpOver is standard, so not used
begin
  PpgColorARGB(PColDst)^ := PpgColorARGB(PColA)^;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpAdd(PColA, PColB, PColDst: pointer);
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpSub(PColA, PColB, PColDst: pointer);
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ - Pb^;
    if D < 0 then Pd^ := 0 else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpDiv(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpModulate(PColA, PColB, PColDst: pointer);
var
  i: integer;
  Pa, Pb, Pd: pbyte;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    Pd^ := pgIntMul(Pa^, Pb^);
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpMax(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpMin(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpDifference(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpExclusion(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Pas_x_4Ch_Add_x_8b_ColorOpAverage(PColA, PColB, PColDst: pointer);
// to do
var
  i: integer;
  Pa, Pb, Pd: pbyte;
  D: integer;
begin
  Pa := PColA;
  Pb := PColB;
  Pd := PColDst;
  for i := 0 to 3 do begin
    D := Pa^ + Pb^;
    if D > $FF then Pd^ := $FF else Pd^ := D;
    inc(Pa); inc(Pb); inc(Pd);
  end;
end;

procedure Asm_FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit

        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;


end.
