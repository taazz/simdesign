{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Render<p>

  <b>Description:</b><p>
  Low-level blending functions

  Creation Date:
  25Sep2005

  Function prefixes:

  for a name like 1_2_3_4_5_6_function

  1 = Pas: Pascal native implementation
      Asm: Assembler
      Mmx: MMX/SSE optimized

  2 = Org: Original color values
      Pre: Premultiplied color values: the colors are already premultiplied with alpha

  3 = 1Ch = 1-channel (Y, where Y could be e.g. gray)
      2Ch = 2-channel (AY where A means alpha, Y could be e.g. gray)
      3Ch = 3-channel (XYZ, where XYZ can be e.g. RGB or any other model)
      4Ch = 4-channel (AXYZ where A means alpha, XYZ can be e.g. RGB or any other model)

  4 = Add: additive color model (like RGB)
      Sub: substractive color model (like CMY)

  5 = Std: standard edge model
      Tch: touching edge model (uses dest cover)

  6 = 8b:  8 bits per channel
      16b: 16 bits per channel.

  function templates

  TpgBlendInfo = record
    PColSrc: pointer;    // pointer to source color
    PColDst: pointer;    // pointer to destination color
    ColCount: integer;   // number of colors to process in a row
    IncSrc: integer;     // increment of source pointer (so if IncSrc = 0 then same source)
    PCvrSrc: pbyte;      // pointer to source cover array, or nil if no cover
    PCvrDst: pbyte;      // pointer to destination cover array, or nil if not used
    IncCvr: integer;     // if 0, same cover used
    ColorOp: TpgColorOp; // additional color operation.. or nil if "over"
    Buffer: pointer;     // in case the algo needs a blending buffer this should point to it
  end;

  TpgBlend = procedure (var Info: TpgBlendInfo);
    Blends Src to Dest color

  TpgAdjustAlpha = procedure (PColSrc, PColDest: pointer; Mul: integer);
    Adjusts alphas in Src with factor Mul and puts result in Dst

  TpgColorOp = procedure (PColA, PColB, PColDst: pointer);
    Performs some color operation on A and B, puts result in Dst

  In all cases, Dest and Src must already contain valid colors, alphas and covers

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2005 - 2006 SimDesign BV
}
unit pgBlend;

{$i simdesign.inc}

interface

uses
  SysUtils, Pyro, pgColor, sdDebug;

var
  glMMXActive: Boolean;

type

  TpgColorOp = (
    coOver,          // Default over: A
    coAdd,           // A + B
    coSub,           // A - B
    coDiv,           // A / B
    coModulate,      // A * B
    coMax,           // max(A, B)
    coMin,           // min(A, B)
    coDifference,    // abs(A - B)
    coExclusion,     // A + (1 - A) * B
    coAverage        // (A + B) / 2
  );

  // Performs some color operation on A and B, puts result in Dst
  TpgColorOpFunc = procedure (PColA, PColB, PColDst: pointer);

  // A record to be passed to the blending function with information
  TpgBlendInfo = record
    PColSrc: pointer;    // pointer to source color or color array
    PColDst: pointer;    // pointer to destination color or color array
    Count: integer;      // number of colors to process in a row
    IncSrc: integer;     // increment of source pointer (so if IncSrc = 0 then same source)
    PCvrSrc: pbyte;      // pointer to source cover array, or nil if no cover
    PCvrDst: pbyte;      // pointer to destination cover array, or nil if not used
    IncCvr: integer;     // if 0, same cover used, otherwise 1
    Buffer: pointer;     // in case the algo needs a blending buffer this should point to it
    ColorOp: TpgColorOpFunc; // additional color operation.. or nil if "over"
    BlendFunc: pointer;  // Pointer to a selected blending function (TpgBlendFunc)
  end;

  // Blends Src to Dest color, or color operation
  TpgBlendFunc = procedure (var Info: TpgBlendInfo);

  // Adjusts alphas in Src with factor Mul and puts result in Dst
  TpgAdjustAlphaFunc = procedure (PColSrc, PColDst: pointer; Mul: integer);

  TpgFillLongwordFunc = procedure(var X; Count: Integer; Value: Longword);

// Select blend
function SelectBlendFunc(const AInfo: TpgColorInfo; AEdgeMode: TpgEdgeMode;
  var BlendInfo: TpgBlendInfo; AColorOp: TpgColorOp = coOver): TpgBlendFunc;

procedure FillLongword(var X; Count: Integer; Value: Longword);

procedure AlphaBlendBlock(
  Dst: pointer;
  DstScanStride: integer;
  DstX, DstY, DstWidth, DstHeight: integer;
  Src: pointer;
  SrcScanStride: integer;
  SrcX, SrcY: integer;
  SrcAlpha: byte;
  const ColorInfo: TpgColorInfo;
  ColorOp: TpgColorOp);

resourcestring

  sBitsPerChannelOpNotImplemented = 'Operation not implemented for selected bits per channel';
  sChannelsOpNotImplemented       = 'Operation not implemented for this number of channels';
  sSubstractiveOpNotImplemented   = 'Operation not implemented for substractive color model';
  sAlphaModeOpNotImplemented      = 'Operation not implemented for this alpha mode';
  sUnknownRenderMode              = 'Unknown render mode: no operation available';
  sColorOpNotImplemented          = 'Color operation not implemented';

implementation

uses
  pgCPUInfo, pgBlend8bMmx, pgBlend8b;

function SelectBlendFunc(const AInfo: TpgColorInfo; AEdgeMode: TpgEdgeMode;
  var BlendInfo: TpgBlendInfo; AColorOp: TpgColorOp): TpgBlendFunc;
// Try to select the best matching blender, raise an exception if a blender doesn't exist
// and no "suitable" substitute can be found
begin
  Result := nil;
  FillChar(BlendInfo, SizeOf(BlendInfo), 0);
  // Checks
  // right now skip edgemode info
  // Color model
  case AInfo.ColorModel of
  cmAdditive:
    // Bits per channel
    case AInfo.BitsPerChannel of
    bpc8bits:
      case AInfo.Channels of
      1: raise Exception.Create(sChannelsOpNotImplemented);
      2: raise Exception.Create(sChannelsOpNotImplemented);
      3: raise Exception.Create(sChannelsOpNotImplemented);
      4:
        begin
          case AInfo.AlphaMode of
          amOriginal:
            {$ifdef d7up}if glMmxActive then
              Result := Mmx_Org_4Ch_Add_Std_8b_Blend
            else{$endif}
              Result := Pas_Org_4Ch_Add_Std_8b_Blend;
          amPremultiplied:
            {$ifdef d7up}if glMmxActive then
              Result := Mmx_Pre_4Ch_Add_Std_8b_Blend
            else{$endif}
              Result := Pas_Pre_4Ch_Add_Std_8b_Blend;
          amDropAlpha:
            raise Exception.Create(sAlphaModeOpNotImplemented);
          else
            raise Exception.Create(sAlphaModeOpNotImplemented);
          end;
          // Color operation
          if AColorOp <> coOver then begin
            // The colorop routine will have a reference to the original blend function
            BlendInfo.BlendFunc := pointer(Result);
            Result := Pas_x_4Ch_Add_x_8b_ColorOp;
            case AColorOp of
            coAdd       : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpAdd;
            coSub       : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpSub;
            coDiv       : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpDiv;
            coModulate  : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpModulate;
            coMax       : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpMax;
            coMin       : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpMin;
            coDifference: BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpDifference;
            coExclusion : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpExclusion;
            coAverage   : BlendInfo.ColorOp := Pas_x_4Ch_Add_x_8b_ColorOpAverage;
            else
              // Unknown mode as of yet
              raise Exception.Create(sColorOpNotImplemented);
            end;
          end;
        end;
      else
        raise Exception.Create(sChannelsOpNotImplemented);
      end;
    bpc16bits:
      raise Exception.Create(sBitsPerChannelOpNotImplemented);
    else
      raise Exception.Create(sBitsPerChannelOpNotImplemented);
    end;
  cmSubstractive:
    raise Exception.Create(sSubstractiveOpNotImplemented);
  end;
  if not assigned(Result) then
    raise Exception.Create(sUnknownRenderMode);
end;

procedure FillLongword(var X; Count: Integer; Value: Longword);
begin
  {$ifdef d7up}if glMMXActive then
    Mmx_FillLongword(X, Count, Value)
  else{$endif}
    Asm_FillLongword(X, Count, Value);
end;

procedure AlphaBlendBlock(
  Dst: pointer;
  DstScanStride: integer;
  DstX, DstY, DstWidth, DstHeight: integer;
  Src: pointer;
  SrcScanStride: integer;
  SrcX, SrcY: integer;
  SrcAlpha: byte;
  const ColorInfo: TpgColorInfo;
  ColorOp: TpgColorOp);
var
  y: integer;
  BlendFunc: TpgBlendFunc;
  BlendInfo: TpgBlendInfo;
  S, D: Pbyte;
  CellStride: integer;
  Buffer: array of byte;
begin
  // Checks
  if (SrcAlpha = 0) or (DstWidth <= 0) or (DstHeight <= 0) then exit;

  // Setup blend function
  BlendFunc := SelectBlendFunc(ColorInfo, emStandard, BlendInfo, ColorOp);

  // Setup blend info
  BlendInfo.IncCvr := 0;
  BlendInfo.IncSrc := 1;
  BlendInfo.Count := DstWidth;
  if SrcAlpha < $FF then
    BlendInfo.PCvrSrc := @SrcAlpha
  else
    BlendInfo.PCvrSrc := nil;
  CellStride := pgColorElementSize(ColorInfo);
  SetLength(Buffer, CellStride * 2 * DstWidth);
  BlendInfo.Buffer := @Buffer[0];

  // Move to start of source
  S := Src;
  inc(S, SrcY * SrcScanStride + SrcX * CellStride);
  // Move to start of dest
  D := Dst;
  Inc(D, DstY * DstScanStride + DstX * CellStride);
  for y := 0 to DstHeight - 1 do
  begin
    BlendInfo.PColSrc := S;
    BlendInfo.PColDst := D;
    BlendFunc(BlendInfo);
    inc(S, SrcScanStride);
    inc(D, DstScanStride);
  end;
end;

{ MMX Detection }

procedure DetectCPUInfo;
begin
  glMMXActive := HasMMX;
end;

initialization
  DetectCPUInfo;

end.
