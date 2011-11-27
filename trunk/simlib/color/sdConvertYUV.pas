{  unit sdConvertYUV

   This unit implements color conversion between RGB and YUV.

   Copyright (c) 2004 by SimDesign B.V. - Author Nils Haeck

   NB: this is NOT the correct code for JPEG YUV<->RGB conversion.

   It is NOT allowed to publish or copy this software without express permission
   of the author!
}
unit sdConvertYUV;

interface

uses
  Windows;

{ Formulae:

  Y  =  0.257R + 0.504G + 0.098B + 16
  Cb = -0.148R - 0.291G + 0.439B + 128
  Cr =  0.439R - 0.368G - 0.071B + 128

  G = 1.164(Y-16) - 0.391(Cb-128) -0.813(Cr-128)
  R = 1.164(Y-16) + 1.596(Cr-128)
  B = 1.164(Y-16) + 2.018(Cb-128)

  or

  R = 1.164Y           + 1.596Cr - 222.912
  G = 1.164Y - 0.391Cb - 0.813Cr + 135.488
  B = 1.164Y + 2.018Cb           - 276.928

  R, G and B range from 0 to 255.
  Y ranges from 16 to 235.
  Cb and Cr range from 16 to 240.}


// Convert individual maps (channels) of RGB to individual channels of YUV. RGB and YUV
// maps must be the same, so the RGB values get replaced by YUV values.
procedure ConvertRGBToYUVInPlace(MapR, MapG, MapB: PByte; Count: integer);

procedure ConvertYUVToRGBInPlace(MapR, MapG, MapB: PByte; Count: integer);

procedure ConvertRGBToGRdBdInPlace(MapR, MapG, MapB: PByte; Count: integer);

procedure ConvertGRdBdToRGBInPlace(MapR, MapG, MapB: PByte; Count: integer);

var

  ByteClip: array[-255..511] of integer;

implementation

var
  YR, YG, YB, UR, UG, UB, VR, VG, VB: array[0..255] of integer;
  RY, RV, GY, GU, GV, BY, BU: array[0..255] of integer;

const
  cPrecision = 10;
  cScaleFact = 1 shl cPrecision;
  cBias      = 1 shl (cPrecision - 1);

procedure ConvertRGBToYUVInPlace(MapR, MapG, MapB: PByte; Count: integer);
var
  i: integer;
  Y, U, V: byte;
begin
  for i := 0 to Count - 1 do begin
    Y := ByteClip[(YR[MapR^] + YG[MapG^] + YB[MapB^] + cBias) div cScaleFact];
    U := ByteClip[(UR[MapR^] + UG[MapG^] + UB[MapB^] + cBias) div cScaleFact];
    V := ByteClip[(VR[MapR^] + VG[MapG^] + VB[MapB^] + cBias) div cScaleFact];
    MapR^ := Y; MapG^ := U; MapB^ := V;
    inc(MapR); inc(MapG); inc(MapB);
  end;
end;

procedure ConvertYUVToRGBInPlace(MapR, MapG, MapB: PByte; Count: integer);
var
  i: integer;
  R, G, B: byte;
begin
  for i := 0 to Count - 1 do begin
    R := ByteClip[(RY[MapR^]             + RV[MapB^] + cBias) div cScaleFact];
    G := ByteClip[(GY[MapR^] + GU[MapG^] + GV[MapB^] + cBias) div cScaleFact];
    B := ByteClip[(BY[MapR^] + BU[MapG^]             + cBias) div cScaleFact];
    MapR^ := R; MapG^ := G; MapB^ := B;
    inc(MapR); inc(MapG); inc(MapB);
  end;
end;

procedure ConvertRGBToGRdBdInPlace(MapR, MapG, MapB: PByte; Count: integer);
var
  i: integer;
  R, G, B: byte;
begin
  for i := 0 to Count - 1 do begin
    R := MapR^; G := MapG^; B := MapB^;
    MapR^ := G;
    MapG^ := R - G + 128;
    MapB^ := B - G + 128;
  end;
end;

procedure ConvertGRdBdToRGBInPlace(MapR, MapG, MapB: PByte; Count: integer);
var
  i: integer;
  G, Rd, Bd: byte;
begin
  for i := 0 to Count - 1 do begin
    G := MapR^; Rd := MapG^; Bd := MapB^;
    MapR^ := Rd + G - 128;
    MapG^ := G;
    MapB^ := Bd + G - 128;
  end;
end;


procedure InitTables;
var
  i: integer;
begin
  // Conversion tables
  for i := 0 to 255 do begin
    // RGB to YUV
    YR[i] := round(( 0.257 * i + 16 ) * cScaleFact);
    UR[i] := round((-0.148 * i + 128) * cScaleFact);
    VR[i] := round(( 0.439 * i + 128) * cScaleFact);
    YG[i] := round(  0.504 * i * cScaleFact);
    UG[i] := round( -0.291 * i * cScaleFact);
    VG[i] := round( -0.368 * i * cScaleFact);
    YB[i] := round(  0.098 * i * cScaleFact);
    UB[i] := round(  0.439 * i * cScaleFact);
    VB[i] := round( -0.071 * i * cScaleFact);
    // YUV to RGB
    RY[i] := round(( 1.164 * i - 222.912) * cScaleFact);
    GY[i] := round(( 1.164 * i + 135.488) * cScaleFact);
    BY[i] := round(( 1.164 * i - 276.928) * cScaleFact);
    GU[i] := round( -0.391 * i * cScaleFact);
    BU[i] := round(  2.018 * i * cScaleFact);
    RV[i] := round(  1.596 * i * cScaleFact);
    GV[i] := round( -0.813 * i * cScaleFact);
  end;
  // Byteclip
  for i := Low(ByteClip) to High(ByteClip) do
    if i < 0 then
      ByteClip[i] := 0
    else if i > 255 then
      ByteClip[i] := 255
    else
      ByteClip[i] := I;
end;

initialization

  InitTables;

end.
