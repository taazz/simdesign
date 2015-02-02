{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Render<p>

  <b>Description:</b><p>
  8bit/channel mmx blending operations

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgBlend8bMmx;

{$i simdesign.inc}

interface

uses
  pgBlend, pgBlend8b, Pyro;

// blending

procedure Mmx_Org_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
procedure Mmx_Pre_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);

// clearing

procedure Mmx_FillLongword(var X; Count: Integer; Value: Longword);

implementation

procedure EMMS;
begin
  if glMmxActive then
  asm
    EMMS
  end;
end;

function Mmx_Org_Blend(F, B: TpgColor32): TpgColor32;
asm
  { This is an implementation of the merge formula, as described
    in a paper by Bruce Wallace in 1981. Merging is associative,
    that is, A over (B over C) = (A over B) over C. The formula is,

      Ra = Fa + Ba - Fa * Ba
      Rc = (Fa (Fc - Bc * Ba) + Bc * Ba) / Ra

    where

      Rc is the resultant color,  Ra is the resultant alpha,
      Fc is the foreground color, Fa is the foreground alpha,
      Bc is the background color, Ba is the background alpha.
  }

        TEST      EAX,$FF000000  // foreground completely transparent =>
        JZ        @1             // result = background
        TEST      EDX,$FF000000  // background completely transparent =>
        JZ        @2             // result = foreground
        CMP       EAX,$FF000000  // foreground completely opaque =>
        JNC       @2             // result = foreground

        PXOR      MM3,MM3
        PUSH      ESI
        MOVD      MM0,EAX        // MM0  <-  Fa Fr Fg Fb
        PUNPCKLBW MM0,MM3        // MM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
        MOVD      MM1,EDX        // MM1  <-  Ba Br Bg Bb
        PUNPCKLBW MM1,MM3        // MM1  <-  00 Ba 00 Br 00 Bg 00 Bb
        SHR       EAX,24         // EAX  <-  00 00 00 Fa
        MOVQ      MM4,MM0        // MM4  <-  00 Fa 00 Fr 00 Fg 00 Fb
        SHR       EDX,24         // EDX  <-  00 00 00 Ba
        MOVQ      MM5,MM1        // MM5  <-  00 Ba 00 Br 00 Bg 00 Bb
        MOV       ECX,EAX        // ECX  <-  00 00 00 Fa
        PUNPCKHWD MM4,MM4        // MM4  <-  00 Fa 00 Fa 00 Fg 00 Fg
        ADD       ECX,EDX        // ECX  <-  00 00 Sa Sa
        PUNPCKHDQ MM4,MM4        // MM4  <-  00 Fa 00 Fa 00 Fa 00 Fa
        MUL       EDX            // EAX  <-  00 00 Pa **
        PUNPCKHWD MM5,MM5        // MM5  <-  00 Ba 00 Ba 00 Bg 00 Bg
        MOV       ESI,$FF        // ESI  <-  00 00 00 00 FF
        PUNPCKHDQ MM5,MM5        // MM5  <-  00 Ba 00 Ba 00 Ba 00 Ba
        DIV       ESI
        SUB       ECX,EAX        // ECX  <-  00 00 00 Ra
        MOV       EAX,$ffff
        CDQ
        PMULLW    MM1,MM5        // MM1  <-  B * Ba
        PSRLW     MM1,8
        DIV       ECX
        PMULLW    MM0,MM4        // MM0  <-  F * Fa
        PSRLW     MM0,8
        PMULLW    MM4,MM1        // MM4  <-  B * Ba * Fa
        PSRLW     MM4,8
        SHL       ECX,24
        PADDUSW   MM1,MM0        // MM1  <-  B * Ba + F * Fa
        PSUBUSW   MM1,MM4        // MM1  <-  B * Ba + F * Fa - B * Ba * Fa
        MOVD      MM2,EAX        // MM2  <-  Qa = 1 / Ra
        PUNPCKLWD MM2,MM2        // MM2  <-  00 00 00 00 00 Qa 00 Qa
        PUNPCKLWD MM2,MM2        // MM2  <-  00 Qa 00 Qa 00 Qa 00 Qa
        PMULLW    MM1,MM2
        PSRLW     MM1,8
        PACKUSWB  MM1,MM3        // MM1  <-  00 00 00 00 xx Rr Rg Rb
        MOVD      EAX,MM1        // EAX  <-  xx Rr Rg Rb
        AND       EAX,$00FFFFFF  // EAX  <-  00 Rr Rg Rb
        OR        EAX,ECX        // EAX  <-  Ra Rr Rg Rb
        POP ESI
        RET
@1:     MOV       EAX,EDX
@2:
end;

procedure Mmx_Org_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
var
  Count: integer;
  PSrc, PDst, PBuf: PpgColor32;
  PCvr: PByte;
  IncSrc: integer;
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
    PDst^ := Mmx_Org_Blend(PSrc^, PDst^);
    inc(PDst);
    inc(PSrc, IncSrc);
    dec(Count);
  end;
  EMMS;
end;

function Mmx_Pre_Blend(F, B: TpgColor32): TpgColor32;
asm
  // blend premultiplied colors
  // EAX <- F
  // EDX <- B
  // Rc = Fc + Bc - (Fa * Bc)

        CMP       EAX,$FF000000  // foreground completely opaque =>
        JNC       @2             // result = foreground
        TEST      EAX,$FF000000  // foreground completely transparent =>
        JZ        @1             // result = background
        TEST      EDX,$FF000000  // background completely transparent =>
        JZ        @2             // result = foreground

        PXOR      MM3,MM3       // MM3 <- 00 00 00 00  00 00 00 00
        MOVD      MM0,EAX       // MM0 <- 00 00 00 00  Fa Fr Fg Fb
        PUNPCKLBW MM0,MM3       // MM0 <- 00 Fa 00 Fr  00 Fg 00 Fb  Fc
        MOVQ      MM1,MM0       // MM1 <- 00 Fa 00 Fr  00 Fg 00 Fb  Fc
        PUNPCKHWD MM1,MM1       // MM1 <- 00 Fa 00 Fa  00 Fr 00 Fr
        MOVD      MM2,EDX       // MM2 <- 00 00 00 00  Ba Br Bg Bb
        PUNPCKHDQ MM1,MM1       // MM1 <- 00 Fa 00 Fa  00 Fa 00 Fa  Fa
        MOV       EAX,$80808080
        PUNPCKLBW MM2,MM3       // MM2 <- 00 Ba 00 Br  00 Bg 00 Bb  Bc
        MOVD      MM4,EAX       // MM4 <- 00 00 00 00  80 80 80 80
        PMULLW    MM1,MM2       // MM1 <- Pa ** Pr **  Pg ** Pb **  Fa * Bc
        PUNPCKLBW MM4,MM3       // MM4 <- 00 80 00 80  00 80 00 80  $80
        PADDW     MM1,MM4       // MM1 <- Ta ** Tr **  Tg ** Tb **  t = Fa * Bc + $80
        MOVQ      MM4,MM1       // MM4 <- Ta ** Tr **  Tg ** Tb **  t
        PSRLW     MM1,8         // MM1 <- 00 Ta 00 Tr  00 Tg 00 Tb  t shr 8
        PADDW     MM4,MM1       // MM4 <- Ta ** Tr **  Tg ** Tb **  t shr 8 + t
        PSRLW     MM4,8         // MM4 <- 00 Ta 00 Tr  00 Tg 00 Tb  (t shr 8 + t) shr 8
        PADDW     MM0,MM2       // MM0 <- Ra ** Rr **  Rg ** Rb **  R = Fc + Bc
        PSUBW     MM0,MM4       // MM0 <- Ra ** Rr **  Rg ** Rb **  R = Fc + Bc - t term
        PACKUSWB  MM0,MM3       // MM0 <- 00 00 00 00  Ra Rr Rg Rb
        MOVD      EAX,MM0
        RET
@1:     MOV       EAX,EDX
@2:
end;

procedure Mmx_Pre_4Ch_Add_Std_8b_Blend(var Info: TpgBlendInfo);
var
  Count, IncSrc: integer;
  PSrc, PDst, PBuf: PpgColor32;
  PCvr: PByte;
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
    PDst^ := Mmx_Pre_Blend(PSrc^, PDst^);
    inc(PDst);
    inc(PSrc, IncSrc);
    dec(Count);
  end;
  EMMS;
end;

procedure Mmx_FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
        CMP        EDX, 0
        JBE        @Exit

        PUSH       EDI
        PUSH       EBX
        MOV        EBX, EDX
        MOV        EDI, EDX

        SHR        EDI, 1
        SHL        EDI, 1
        SUB        EBX, EDI
        JE         @QLoopIni

        MOV        [EAX], ECX
        ADD        EAX, 4
        DEC        EDX
        JZ         @ExitPOP
   @QLoopIni:
        MOVD       MM1, ECX
        PUNPCKLDQ  MM1, MM1
        SHR        EDX, 1
    @QLoop:
        MOVQ       [EAX], MM1
        ADD        EAX, 8
        DEC        EDX
        JNZ        @QLoop
        EMMS
    @ExitPOP:
        POP        EBX
        POP        EDI
    @Exit:
end;

end.
