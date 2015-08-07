unit sdPdfColorConversion;

interface

uses
  Classes, SysUtils, sdPdfImages, Math;

procedure PdfColorConvertPromote1bitTo8bit(Pdf: TObject; AShape: TPdfImageShape);

resourcestring
  sUnsupportedChannelCount = 'Unsupported channel count';

implementation

uses
  sdPdfdocument, sdPdfObjects, sdPdfColors;

procedure PdfColorConvertPromote1bitTo8bit(Pdf: TObject; AShape: TPdfImageShape);
var
  i, yi, W: integer;
  NewMap: TMemoryStream;
  P, Q: PByte;
  B: word;
  LoVal: byte;
  HiVal: byte;
  ReqSize: integer;
begin
  if not AShape.BitsPerComponent = 1 then exit;
  // Old map size
  ReqSize := AShape.Width * AShape.Height * AShape.ChannelCount div 8;
  if AShape.Source.Size < ReqSize then
    AShape.Source.Size := ReqSize;



  // We convert this to 8bpc
  NewMap := TMemoryStream.Create;
  try
    // 8bpc, wxh with N channels
    NewMap.SetSize(AShape.Width * AShape.Height * AShape.ChannelCount);
    P := AShape.Source.Memory;
    Q := NewMap.Memory;
    case AShape.ChannelCount of
    1: // e.g. grayscale
      begin

        // Decode arrays
        if length(AShape.Decode) = 2 then begin
          LoVal := Max(0, Min(255, round(AShape.Decode[0] * 255)));
          HiVal := Max(0, Min(255, round(AShape.Decode[1] * 255)));
        end else begin
          LoVal := $00;
          HiVal := $FF;
        end;

        // Loop through scanlines
        for yi := 0 to AShape.Height - 1 do begin
          W := AShape.Width;
          repeat
            B := P^;
            for i := 0 to Min(W, 8) - 1 do begin
              if (B and $80) = 0 then
                // Black
                Q^ := LoVal
              else
                // White
                Q^ := HiVal;
              inc(Q);
              B := B shl 1;
            end;
            inc(P);
            dec(W, 8);
          until W <= 0;
        end;
      end;
    else
      TPdfDocument(Pdf).DoDebugMessage(dlWarning, sUnsupportedChannelCount);
      TPdfDocument(Pdf).DoDebugMessage(dlInfo, Format('Channels: %d; Colorspace: %s',
        [AShape.ChannelCount, cColorSpaceNames[AShape.Color.Space]]));
      exit;
    end;
    // Now we write back the map
    AShape.Source.Position := 0;
    AShape.Source.CopyFrom(NewMap, NewMap.Size);
    AShape.BitsPerComponent := 8;
  finally
    NewMap.Free;
  end;
end;

end.
