{ unit sdPdfFilterTables

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit defines code tables for CITTFAX filters

  Author: Nils Haeck M.Sc.

  Changes:
    21Sep2005 - Created

  copyright (c) 2005 by Simdesign B.V.

}
unit sdPdfFilterTables;

interface

type
  TCodeEntry = record
    Code, Len: Cardinal;
  end;

const // CCITT code tables
  WhiteCodes: array[0..103] of TCodeEntry = (
    (Code : $0035; Len : 8),
    (Code : $0007; Len : 6),
    (Code : $0007; Len : 4),
    (Code : $0008; Len : 4),
    (Code : $000B; Len : 4),
    (Code : $000C; Len : 4),
    (Code : $000E; Len : 4),
    (Code : $000F; Len : 4),
    (Code : $0013; Len : 5),
    (Code : $0014; Len : 5),
    (Code : $0007; Len : 5),
    (Code : $0008; Len : 5),
    (Code : $0008; Len : 6),
    (Code : $0003; Len : 6),
    (Code : $0034; Len : 6),
    (Code : $0035; Len : 6),
    (Code : $002A; Len : 6),
    (Code : $002B; Len : 6),
    (Code : $0027; Len : 7),
    (Code : $000C; Len : 7),
    (Code : $0008; Len : 7),
    (Code : $0017; Len : 7),
    (Code : $0003; Len : 7),
    (Code : $0004; Len : 7),
    (Code : $0028; Len : 7),
    (Code : $002B; Len : 7),
    (Code : $0013; Len : 7),
    (Code : $0024; Len : 7),
    (Code : $0018; Len : 7),
    (Code : $0002; Len : 8),
    (Code : $0003; Len : 8),
    (Code : $001A; Len : 8),
    (Code : $001B; Len : 8),
    (Code : $0012; Len : 8),
    (Code : $0013; Len : 8),
    (Code : $0014; Len : 8),
    (Code : $0015; Len : 8),
    (Code : $0016; Len : 8),
    (Code : $0017; Len : 8),
    (Code : $0028; Len : 8),
    (Code : $0029; Len : 8),
    (Code : $002A; Len : 8),
    (Code : $002B; Len : 8),
    (Code : $002C; Len : 8),
    (Code : $002D; Len : 8),
    (Code : $0004; Len : 8),
    (Code : $0005; Len : 8),
    (Code : $000A; Len : 8),
    (Code : $000B; Len : 8),
    (Code : $0052; Len : 8),
    (Code : $0053; Len : 8),
    (Code : $0054; Len : 8),
    (Code : $0055; Len : 8),
    (Code : $0024; Len : 8),
    (Code : $0025; Len : 8),
    (Code : $0058; Len : 8),
    (Code : $0059; Len : 8),
    (Code : $005A; Len : 8),
    (Code : $005B; Len : 8),
    (Code : $004A; Len : 8),
    (Code : $004B; Len : 8),
    (Code : $0032; Len : 8),
    (Code : $0033; Len : 8),
    (Code : $0034; Len : 8),
    (Code : $001B; Len : 5),
    (Code : $0012; Len : 5),
    (Code : $0017; Len : 6),
    (Code : $0037; Len : 7),
    (Code : $0036; Len : 8),
    (Code : $0037; Len : 8),
    (Code : $0064; Len : 8),
    (Code : $0065; Len : 8),
    (Code : $0068; Len : 8),
    (Code : $0067; Len : 8),
    (Code : $00CC; Len : 9),
    (Code : $00CD; Len : 9),
    (Code : $00D2; Len : 9),
    (Code : $00D3; Len : 9),
    (Code : $00D4; Len : 9),
    (Code : $00D5; Len : 9),
    (Code : $00D6; Len : 9),
    (Code : $00D7; Len : 9),
    (Code : $00D8; Len : 9),
    (Code : $00D9; Len : 9),
    (Code : $00DA; Len : 9),
    (Code : $00DB; Len : 9),
    (Code : $0098; Len : 9),
    (Code : $0099; Len : 9),
    (Code : $009A; Len : 9),
    (Code : $0018; Len : 6),
    (Code : $009B; Len : 9),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

  BlackCodes: array[0..103] of TCodeEntry = (
    (Code : $0037; Len : 10),
    (Code : $0002; Len : 3),
    (Code : $0003; Len : 2),
    (Code : $0002; Len : 2),
    (Code : $0003; Len : 3),
    (Code : $0003; Len : 4),
    (Code : $0002; Len : 4),
    (Code : $0003; Len : 5),
    (Code : $0005; Len : 6),
    (Code : $0004; Len : 6),
    (Code : $0004; Len : 7),
    (Code : $0005; Len : 7),
    (Code : $0007; Len : 7),
    (Code : $0004; Len : 8),
    (Code : $0007; Len : 8),
    (Code : $0018; Len : 9),
    (Code : $0017; Len : 10),
    (Code : $0018; Len : 10),
    (Code : $0008; Len : 10),
    (Code : $0067; Len : 11),
    (Code : $0068; Len : 11),
    (Code : $006C; Len : 11),
    (Code : $0037; Len : 11),
    (Code : $0028; Len : 11),
    (Code : $0017; Len : 11),
    (Code : $0018; Len : 11),
    (Code : $00CA; Len : 12),
    (Code : $00CB; Len : 12),
    (Code : $00CC; Len : 12),
    (Code : $00CD; Len : 12),
    (Code : $0068; Len : 12),
    (Code : $0069; Len : 12),
    (Code : $006A; Len : 12),
    (Code : $006B; Len : 12),
    (Code : $00D2; Len : 12),
    (Code : $00D3; Len : 12),
    (Code : $00D4; Len : 12),
    (Code : $00D5; Len : 12),
    (Code : $00D6; Len : 12),
    (Code : $00D7; Len : 12),
    (Code : $006C; Len : 12),
    (Code : $006D; Len : 12),
    (Code : $00DA; Len : 12),
    (Code : $00DB; Len : 12),
    (Code : $0054; Len : 12),
    (Code : $0055; Len : 12),
    (Code : $0056; Len : 12),
    (Code : $0057; Len : 12),
    (Code : $0064; Len : 12),
    (Code : $0065; Len : 12),
    (Code : $0052; Len : 12),
    (Code : $0053; Len : 12),
    (Code : $0024; Len : 12),
    (Code : $0037; Len : 12),
    (Code : $0038; Len : 12),
    (Code : $0027; Len : 12),
    (Code : $0028; Len : 12),
    (Code : $0058; Len : 12),
    (Code : $0059; Len : 12),
    (Code : $002B; Len : 12),
    (Code : $002C; Len : 12),
    (Code : $005A; Len : 12),
    (Code : $0066; Len : 12),
    (Code : $0067; Len : 12),
    (Code : $000F; Len : 10),
    (Code : $00C8; Len : 12),
    (Code : $00C9; Len : 12),
    (Code : $005B; Len : 12),
    (Code : $0033; Len : 12),
    (Code : $0034; Len : 12),
    (Code : $0035; Len : 12),
    (Code : $006C; Len : 13),
    (Code : $006D; Len : 13),
    (Code : $004A; Len : 13),
    (Code : $004B; Len : 13),
    (Code : $004C; Len : 13),
    (Code : $004D; Len : 13),
    (Code : $0072; Len : 13),
    (Code : $0073; Len : 13),
    (Code : $0074; Len : 13),
    (Code : $0075; Len : 13),
    (Code : $0076; Len : 13),
    (Code : $0077; Len : 13),
    (Code : $0052; Len : 13),
    (Code : $0053; Len : 13),
    (Code : $0054; Len : 13),
    (Code : $0055; Len : 13),
    (Code : $005A; Len : 13),
    (Code : $005B; Len : 13),
    (Code : $0064; Len : 13),
    (Code : $0065; Len : 13),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

// this table is reverse-engineered from the autogenerated fax3sm_winnt.c file
// that is part of LibTiff

{  TIFFFaxMainTable:
0   0000000 12,7,0, // S_EOL
1         1 3,1,0,  // S_V0
2       010 5,3,1,  // S_VL
4       100 2,3,0,  // S_Horiz
6       110 4,3,1,  // S_VR
8      1000 1,4,0,  // S_Pass
16   010000 5,6,2,  // S_VLx2
32  0100000 5,7,3,  // S_VLx3
48   110000 4,6,2,  // S_VRx2
64  1000000 6,7,0,  // S_Ext
96  1100000 4,7,3,  // S_VRx3
}

  cS_EOL     = 1;
  cS_V0      = 2;
  cS_VL      = 3;
  cS_Horiz   = 4;
  cS_VR      = 5;
  cS_Pass    = 6;
  cS_VLx2    = 7;
  cS_VLx3    = 8;
  cS_VRx2    = 9;
  cS_Ext     = 10;
  cS_VRx3    = 11;

  MainCodes: array[0..10] of TCodeEntry = (
    (Code : $0000; Len : 7),
    (Code : $0001; Len : 1),
    (Code : $0002; Len : 3),
    (Code : $0004; Len : 3),
    (Code : $0006; Len : 3),
    (Code : $0008; Len : 4),
    (Code : $0010; Len : 6),
    (Code : $0020; Len : 7),
    (Code : $0030; Len : 6),
    (Code : $0040; Len : 7),
    (Code : $0060; Len : 7)
  );

  // 256 bytes to make bit reversing easy,
  // this is actually not much more than writing bit manipulation code, but much faster
  ReverseTable: array[0..255] of Byte = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0,
    $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8,
    $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4,
    $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
    $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2,
    $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
    $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6,
    $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
    $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1,
    $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9,
    $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5,
    $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
    $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3,
    $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
    $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7,
    $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
    $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
  );

  G3_EOL = -1;
  G3_INVALID = -2;

implementation

end.
