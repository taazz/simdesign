{ unit sdPdfColors

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements color spaces and color conversions as defined in PDF
  chapter 4.5: Color Spaces

  Author: Nils Haeck M.Sc.

  Changes:
    13Jan2004 - Created
    26Sep2005 - Added palette handling for csIndexed

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfColors;

interface

uses
  Classes, SysUtils, sdPdfObjects, sdPdfUtil, Math;

const
  // We represent any color by a max of cColorChannelCount channels
  cMaxColorChannelCount = 4;

type

  // This enumeration defines the allowed color spaces within PDF color specifications
  TPdfColorSpace = (
    csUnknown,
    csCalRGB,
    csCalGray,
    csLab,
    csICCBased,
    csDeviceRGB,
    csDeviceCMYK,
    csDeviceGray,
    csSeparation,
    csDeviceN,
    csIndexed,
    csPattern
  );

  // We represent any color by a max of cColorChannelCount channels,
  // with values between 0.0 and 1.0
  TPdfColorStruct = array[0..cMaxColorChannelCount - 1] of PdfFloat;
  PPdfColorStruct = ^TPdfColorStruct;

  // PDF color class that holds a colorspace and color channel values
  TPdfColor = class(TPersistent)
  private
    FChannels: TPdfColorStruct; // color values in ranges 0..1 for each channel
    FSpace: TPdfColorSpace;     // color space type
    FDict: TPdfObject;          // pointer to color space dictionary if any
    function GetChannelCount: integer;
    function GetChannels: PPdfColorStruct;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function AsWindowsRGB: integer;
    function Equals(AColor: TPdfColor): boolean;
    procedure IndexTable(var BaseSpace: TPdfColorSpace; var BaseDict: TPdfObject;
      var HiVal: integer; var Table: TPdfObject);
    property ChannelCount: integer read GetChannelCount;
    property Channels: PPdfColorStruct read GetChannels;
    property Space: TPdfColorSpace read FSpace write FSpace;
    property Dict: TPdfObject read FDict write FDict;
  end;

  TPdfColor8bit = array[0..cMaxColorChannelCount - 1] of byte;

  TPdfPalette8bit = array of TPdfColor8bit;

const

  cColorSpaceNames: array[TPdfColorSpace] of string =
   ('Unknown', 'CalRGB', 'CalGray', 'Lab', 'ICCBased', 'DeviceRGB', 'DeviceCMYK',
    'DeviceGray', 'Separation', 'DeviceN', 'Indexed', 'Pattern');

// Return the Base, HiVal and Table properties for indexed color spaces
procedure PdfIndexTableFromValue(Value: TPdfObject; var Space: TPdfColorSpace;
  var Dict: TPdfObject; var HiVal: integer; var Table: TPdfObject);

function PdfColorSpaceFromString(const Name: string; IsInline: boolean = False): TPdfColorSpace;

// Return the color space in Space and a pointer to the dictionary for the colorspace
// (CIE only) from the value in the image dictionary "ColorSpace"
procedure PdfColorSpaceFromValue(Value: TPdfObject; var Space: TPdfColorSpace;
    var Dict: TPdfObject; IsInline: boolean = False);

// Return the number of channels in the given colorspace Space
function ChannelCountFromColorspace(Space: TPdfColorSpace; Dict: TPdfObject): integer;

// Read the table with palette entries
procedure PdfReadPalette8bit(ChannelCount, HiVal: integer; Table: TPdfObject; var Palette: TPdfPalette8bit);

function PdfHexStringToBinString(const Info: string): string;

resourcestring
  sCannotResolveChannelCount = 'Cannot resolve channel count';
  sCannotReadPaletteTable    = 'Cannot read palette table';

implementation

procedure PdfIndexTableFromValue(Value: TPdfObject; var Space: TPdfColorSpace;
  var Dict: TPdfObject; var HiVal: integer; var Table: TPdfObject);
var
  AName: string;
begin
  Space := csUnknown;
  HiVal := 0;
  Table := nil;
  if not (Value is TpdfArray) then
    raise EPdfError.Create(sPdfInvalidColorSpace);
  AName := TPdfArray(Value)[0].AsString;
  if not (AName = 'Indexed') then
    raise EPdfError.Create(sPdfInvalidColorSpace);
  // Base color space
  PdfColorSpacefromValue(TPdfArray(Value)[1], Space, Dict);
  // HiVal
  HiVal := TPdfArray(Value)[2].AsInteger;
  // Table
  Table := TPdfArray(Value)[3];
end;

procedure PdfColorSpaceFromValue(Value: TPdfObject; var Space: TPdfColorSpace;
    var Dict: TPdfObject; IsInline: boolean = False);
// Return the color space in Space and a pointer to the dictionary for the colorspace
// (CIE only) from the value in the image dictionary "ColorSpace"
var
  AName: string;
begin
  Dict := nil;
  AName := '';
  Value := DerefPdf(Value);
  if Value is TPdfName then
    AName := Value.AsString
  else
    if Value is TPdfArray then
      AName := TPdfArray(Value)[0].AsString;
  Space := PdfColorSpaceFromString(AName, IsInline);
  if Space = csIndexed then
    Dict := Value
  else
    if Value is TPdfArray then
      Dict := DerefPdf(TPdfArray(Value)[1]);
end;

function PdfColorSpaceFromString(const Name: string; IsInline: boolean = False): TPdfColorSpace;
begin
  if Name = 'DeviceRGB'  then Result := csDeviceRGB else
  if Name = 'DeviceGray' then Result := csDeviceGray else
  if Name = 'DeviceCMYK' then Result := csDeviceCMYK else
  if Name = 'CalRGB'     then Result := csCalRGB else
  if Name = 'CalGray'    then Result := csCalGray else
  if Name = 'Lab'        then Result := csLab else
  if Name = 'ICCBased'   then Result := csICCBased else
  if Name = 'Indexed'    then Result := csIndexed else
  if Name = 'Pattern'    then Result := csPattern else
  if Name = 'Separation' then Result := csSeparation else
  if Name = 'DeviceN'    then Result := csDeviceN else
  // abbreviations in inline images
  if IsInline then begin
    if Name = 'RGB'  then Result := csDeviceRGB else
    if Name = 'G'    then Result := csDeviceGray else
    if Name = 'CMYK' then Result := csDeviceCMYK else
    if Name = 'I'    then Result := csIndexed else
      Result := csUnknown;
  end else
    // arriving here means unknown color space
    Result := csUnknown;
end;

// Return the number of channels in the given colorspace Space
function ChannelCountFromColorspace(Space: TPdfColorSpace; Dict: TPdfObject): integer;
var
  Names: TPdfArray;
begin
  case Space of
  csCalRGB:     Result := 3;
  csCalGray:    Result := 1;
  csLab:        Result := 3;
  csDeviceRGB:  Result := 3;
  csDeviceCMYK: Result := 4;
  csDeviceGray: Result := 1;
  csIndexed:    Result := 1;
  csICCBased:
    begin
      Dict := DerefPdf(Dict);
      if not (Dict is TPdfStream) then
        raise EPdfError.Create(sCannotResolveChannelCount);
      Result := TPdfStream(Dict).Dict.IntegerByKey('N');
    end;
  // To do: handle these based on Dict
  csSeparation:
    // separation always has one teint
    Result := 1;
  csDeviceN:
    begin
      // The number of teints depends on the Names array in the dictionary
      Result := 0;
      if not (Dict is TPdfDictionary) then exit;
      Names := TPdfDictionary(Dict).ArrayByKey('Names');
      if not assigned(Names) then exit;
      Result := Names.ElementCount;
    end;
{  csPattern:}
  else
    Result := 0;
  end;//case
end;

procedure PdfReadPalette8bit(ChannelCount, HiVal: integer; Table: TPdfObject; var Palette: TPdfPalette8bit);
var
  i, j, Idx: integer;
  Info: string;
begin
  // Table can either be a string or stream. In that case the stream is converted to string
  Table := DerefPdf(Table);
  if Table is TPdfStream then
    Info := TPdfStream(Table).StreamToString
  else
    if Table is TPdfString then
      Info := PdfHexStringToBinString(Info)
    else
      raise EPdfError.Create(sCannotReadPaletteTable);

  SetLength(Palette, HiVal + 1);

  Idx := 1;
  for i := 0 to HiVal - 1 do begin
    for j := 0 to ChannelCount - 1 do begin
      Palette[i][j] := ord(Info[Idx]);
      inc(Idx);
    end;
  end;

end;

function PdfHexStringToBinString(const Info: string): string;
var
  Idx: integer;
begin
  Result := '';
  Idx := 1;
  while Idx < length(Info) do begin
    if not (Info[Idx] in ['0'..'9', 'A'..'F', 'a'..'f']) then begin
      inc(Idx);
      continue;
    end;
    // Hex entries
    Result := Result + chr(StrToIntDef('$' + copy(Info, Idx, 2), 0));
    inc(Idx, 2);
  end;
end;

function RoundLimit(Value: PdfFloat; Lwr, Upr: integer): integer;
begin
  Result := Min(Upr, Max(Lwr, round(Value)));
end;

function ConvertDeviceCMYKtoDeviceRGB(const AColor: TPdfColorStruct): TPdfColorStruct;
// Convert DeviceCMYK to DeviceRGB, see 6.4.2
begin
  Result[0] := 1.0 - Min(1.0, AColor[0] + AColor[3]);
  Result[1] := 1.0 - Min(1.0, AColor[1] + AColor[3]);
  Result[2] := 1.0 - Min(1.0, AColor[2] + AColor[3]);
end;

function ConvertDeviceGraytoDeviceRGB(const AColor: TPdfColorStruct): TPdfColorStruct;
begin
  Result[0] := AColor[0];
  Result[1] := AColor[0];
  Result[2] := AColor[0];
end;

function ConvertDeviceRGBtoInt(const AColor: TPdfColorStruct): integer;
// Convert to integer, with RGB as R shl 16 + G shl 8 + B
begin
  Result :=
    RoundLimit(AColor[0] * 255, 0, 255) shl 16 +
    RoundLimit(AColor[1] * 255, 0, 255) shl  8 +
    RoundLimit(AColor[2] * 255, 0, 255);
end;

{ TPdfColor }

function TPdfColor.AsWindowsRGB: integer;
var
  Gray: byte;
begin
  // Convert to device RGB
  case Space of
{  csCalRGB,
  csCalGray,
  csLAB,
  csICCBased,}
  csDeviceRGB:
    Result := ConvertDeviceRGBtoInt(FChannels);
  csDeviceCMYK:
    Result := ConvertDeviceRGBtoInt(ConvertDeviceCMYKtoDeviceRGB(FChannels));
  csDeviceGray:
    begin
      Gray := RoundLimit(FChannels[0] * 255, 0, 255);
      Result := Gray shl 16 + Gray shl 8 + Gray;
    end;
{ csSeparation,
  csDeviceN,
  csIndexed,
  csPattern}
  else
    raise EPdfError.Create(sPdfCantConvertColor);
  end;//
end;

procedure TPdfColor.Assign(Source: TPersistent);
begin
  if Source is TPdfColor then begin
    FChannels := TPdfColor(Source).FChannels;
    FSpace    := TPdfColor(Source).FSpace;
    FDict     := TPdfColor(Source).FDict;
  end else
    inherited;
end;

constructor TPdfColor.Create;
begin
  inherited Create;
  // Set to black in DeviceGray
  FSpace := csDeviceGray;
  FChannels[0] := 1;
end;

function TPdfColor.Equals(AColor: TPdfColor): boolean;
var
  i: integer;
begin
  Result := False;
  if assigned(AColor) and (Space = AColor.Space) then begin
    for i := 0 to ChannelCount - 1 do
      if Channels[i] <> AColor.Channels[i] then
        exit;
  end;
  Result := True;
end;

function TPdfColor.GetChannelCount: integer;
// The number of channels differs for each color space
begin
  Result := ChannelCountFromColorspace(FSpace, FDict);
end;

function TPdfColor.GetChannels: PPdfColorStruct;
begin
  Result := @FChannels;
end;

procedure TPdfColor.IndexTable(var BaseSpace: TPdfColorSpace;
  var BaseDict: TPdfObject; var HiVal: integer; var Table: TPdfObject);
var
  AName: string;
begin
  BaseSpace := csUnknown;
  BaseDict := nil;
  HiVal := 0;
  Table := nil;
  if not (FSpace = csIndexed) then
    raise EPdfError.Create(sPdfInvalidColorSpace);
  if not (FDict is TpdfArray) then
    raise EPdfError.Create(sPdfInvalidColorSpace);
  AName := TPdfArray(FDict)[0].AsString;
  // Base color space
  PdfColorSpacefromValue(TPdfArray(FDict)[1], BaseSpace, BaseDict);
  // HiVal
  HiVal := TPdfArray(FDict)[2].AsInteger;
  // Table
  Table := TPdfArray(FDict)[3];
end;

end.
