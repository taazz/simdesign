{ unit sdPdfFonts

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements basic font handling as defined in PDF
  chapter 5.4: Font data structures
          5.5: Simple fonts
          5.6: Composite fonts

  Fonts are specified as TPdfDictionary objects, and the routines in this unit
  can be used to extract specific information from them.

  Author: Nils Haeck M.Sc.

  Changes:
    13Jan2004 - Created

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfFonts;

interface

uses
  Classes, sdPdfObjects, sdPdfUtil, Graphics, SysUtils;

type

  TPdfNumberMap256 = array[0..255] of PdfFloat;

  TPdfFont = class(TPersistent)
  private
    FDict: TPdfDictionary; // pointer to font dictionary
    FWidths: TPdfNumberMap256;
    FGDIFont: TFont; // owned substitute GDI font for rendering with GDI
    function GetWidths(Index: integer): PdfFloat;
    procedure SetDict(const Value: TPdfDictionary);
    function GetGDIFont: TFont; virtual;
  protected
    procedure GetGlyphWidthsFromFontFile; virtual;
  public
    function WritingMode: integer; virtual;
    property Dict: TPdfDictionary read FDict write SetDict;
    property Widths[Index: integer]: PdfFloat read GetWidths;
    property GDIFont: TFont read GetGDIFont;
  end;

  TPdfType1Font = class(TPdfFont)
  end;

  TPdfType3Font = class(TPdfFont);

  TPdfTrueTypeFont = class(TPdfFont);


// Get a pointer to a named font from a resource dictionary
function PdfFontDictFromResourceDict(ADict: TPdfDictionary; AName: string): TPdfDictionary;

// Create the correct font descendant based on the dictionary information
function PdfCreateFontFromDict(ADict: TPdfDictionary): TPdfFont;

implementation

function PdfFontDictFromResourceDict(ADict: TPdfDictionary; AName: string): TPdfDictionary;
// Get a pointer to a named font from a resource dictionary
var
  Fonts: TPdfDictionary;
begin
  Result := nil;
  if not assigned(ADict) then exit;
  Fonts := ADict.DictionaryByKey('Font');
  if not assigned(Fonts) then exit;
  Result := Fonts.DictionaryByKey(AName);
end;

function PdfCreateFontFromDict(ADict: TPdfDictionary): TPdfFont;
var
  Subtype: string;
begin
  Result := nil;
  if not assigned(ADict) then exit;
  // Find the subtype
  Subtype := ADict.StringByKey('Subtype');

  // Based on this we create a specialized Font object
  if Subtype = 'Type1' then
    Result := TPdfType1Font.Create
  else if Subtype = 'Type3' then
    Result := TPdfType3Font.Create
  else if Subtype = 'TrueType' then
    Result := TPdfTrueTypeFont.Create
  else
    // All others (for now) are just the generic font type
    Result := TPdfFont.Create;
  // Set the dictionary
  Result.Dict := ADict;
end;

{ TPdfFont }

function TPdfFont.GetGDIFont: TFont;
var
  AName, AFontName: string;
  APos: integer;
begin
  if not assigned(FGDIFont) then begin
    AName := Dict.StringByKeyDefault('BaseFont', '');
    FGDIFont := TFont.Create;
    if length(AName) > 0 then begin
      APos := Pos('+', AName);
      if APos > 0 then
        AFontName := copy(AName, APos + 1, length(AName))
      else
        AFontName := AName;
      APos := Pos(',', AFontName);
      if APos > 0 then
        AFontName := copy(AFontName, 1, APos - 1);
      FGDIFont.Name := AFontName;
      // Decorations
      AName := lowercase(AName);
      if Pos('bold', AName) > 0 then
        FGDIFont.Style := FGDIFont.Style + [fsBold];
      if Pos('italic', AName) > 0 then
        FGDIFont.Style := FGDIFont.Style + [fsItalic];
    end else begin
      // No other clues
      FGDIFont.Name := 'Times';
    end;
  end;
  Result := FGDIFont;
end;

procedure TPdfFont.GetGlyphWidthsFromFontFile;
// in case there's no widths defined, we must get this information from the font itself
var
  i: integer;
begin
  // TODO This method must be overridden in descendant type
  // this is a crude method
  for i := 0 to 255 do FWidths[i] := 400;
end;

function TPdfFont.GetWidths(Index: integer): PdfFloat;
begin
  if (Index >= 0) and (Index < 256) then
    Result := FWidths[Index]
  else
    Result := 0;
end;

procedure TPdfFont.SetDict(const Value: TPdfDictionary);
var
  i: integer;
  ADescriptor: TPdfDictionary;
  AWidths: TPdfArray;
  AWidth: PdfFloat;
  AFirst, ALast: integer;
  AName: string;
  FontFileStream: TPdfStream;
  S: TMemoryStream;
begin
  if FDict <> Value then begin
    FDict := Value;
    if not assigned(FDict) then exit;

    // Read widths - first set the "missing width" to all items
    ADescriptor := FDict.DictionaryByKey('FontDescriptor');
    if assigned(ADescriptor) then begin
      // MissingWidth entry
      AWidth := ADescriptor.NumberByKeyDefault('MissingWidth', 0);
      for i := 0 to 255 do
        FWidths[i] := AWidth;
    end;

    // Read widths array
    AFirst := FDict.IntegerByKeyDefault('FirstChar', 0);
    ALast  := FDict.IntegerByKeyDefault('LastChar', 0);
    AWidths := FDict.ArrayByKey('Widths');
    if assigned(AWidths) then begin
      for i := 0 to AWidths.ElementCount - 1 do
        if i + AFirst <= ALast then
          FWidths[i + AFirst] := AWidths[i].AsNumber;
    end else begin
      // in case there's no widths defined, we must get this information from the font
      // itself
      GetGlyphWidthsFromFontFile;
    end;

    // Test!: Save the font
    if assigned(ADescriptor) then begin
      if ClassType = TPdfType1Font then
        AName := 'FontFile'
      else if Classtype = TPdfTrueTypeFont then
        AName := 'FontFile2'
      else
        AName := 'FontFile3';

      FontFileStream := ADescriptor.StreamByKey(AName);
      if assigned(FontFileStream) then begin
        // Save
        S := TMemoryStream.Create;
        try
          FontFileStream.DecompressToStream(S);
//          S.SaveToFile('c:\temp\test.txt');
        finally
          S.Free;
        end;
      end;
    end;

  end;
end;

function TPdfFont.WritingMode: integer;
// The ancestor font class is always horizontal (no chinese etc)
begin
  Result := 0;
end;

end.
