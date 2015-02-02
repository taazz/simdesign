{ Project: Pyro
  Module: Pyro Core

  Description:
  ImageView class

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgImage;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, pgViewPort, pgBitmap, pgPath, pgUriReference, pgResource,
  pgRaster, pgDocument, Pyro, sdDebug;

type

  // Special resource type that loads images
  TpgImageResource = class(TpgResource)
  private
    FBitmap: TpgBitmap;
    function GetBitmap: TpgBitmap;
  protected
    function IsChanged: boolean; override;
  public
    destructor Destroy; override;
    procedure Load; override;
    procedure Save; override;
    // Bitmap will return a reference to the bitmap for raster images
    property Bitmap: TpgBitmap read GetBitmap;
  end;

  // TpgImageProp references a TpgImageResource element, which contains the data
  TpgImageProp = class(TpgCountedRefProp)
  private
    function GetBitmap: TpgBitmap;
  protected
    function CheckReference: TpgImageResource;
  public
    // Load the image from file AFileName. If Embed is set to True, the image
    // is embedded in the pyro document, otherwise it will be just referenced
    // unless the bitmap gets changed. Referenced images must still exist in the
    // file system next time the pyro document is loaded!
    procedure LoadFromFile(const AFileName: string; Embed: boolean);
    procedure LoadFromURI(const AURI: string);
    property Bitmap: TpgBitmap read GetBitmap;
  end;

  // Specialized viewport for images. It overrides the way the viewbox Min/Max
  // are established (uses the underlying bitmap if no viewbox is defined)
  TpgImageView = class(TpgViewPort)
  private
    function GetImage: TpgImageProp;
  protected
    procedure GetViewBoxProps(var AMinX, AMinY, AWidth, AHeight: double); override;  
  public
    property Image: TpgImageProp read GetImage;
  end;

implementation

{ TpgImageResource }

destructor TpgImageResource.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TpgImageResource.GetBitmap: TpgBitmap;
begin
  if not Loaded and (URI.Value <> '') then
    Load;
  if not assigned(FBitmap) then
    FBitmap := TpgBitmap.Create;
  Result := FBitmap;
end;

function TpgImageResource.IsChanged: boolean;
begin
  Result := False;
  if assigned(FBitmap) then
    Result := FBitmap.Changed;
end;

procedure TpgImageResource.Load;
var
  SS: TStringStream;
  RasterClass: TpgRasterClass;
begin
  // this loads the uri into raw data
  inherited;

  // See if we can load the image as bitmap
  RasterClass := FindRasterClassByMimeType(MimeType.Value);
  if not assigned(RasterClass) then
  begin
    DoDebugOut(Self, wsFail, sUnknownRasterImageType);
    exit;
  end;

  // Make sure we have a bitmap
  if not assigned(FBitmap) then
    FBitmap := TpgBitmap.Create;

  // Load the bitmap
  SS := TStringStream.Create(Data.Value);
  try
    LoadImageFromStream(SS, FBitmap, RasterClass);
  finally
    SS.Free;
  end;
  FBitmap.Changed := False;
end;

procedure TpgImageResource.Save;
var
  M: TMemoryStream;
begin
  if not assigned(FBitmap) then
    exit;
  URI.Value := 'data';
  // Save the bitmap as png
  MimeType.Value := 'image/png';
  M := TMemoryStream.Create;
  try
    SaveImageToStream(M, FBitmap, 'image/png');
    Data.SetBinary(M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

{ TpgImageProp }

function TpgImageProp.CheckReference: TpgImageResource;
var
  Document: TpgDocument;
begin
  Result := TpgImageResource(Reference);
  if not assigned(Result) then
  begin
    Document := GetDocument;
    if assigned(Document) then
      // Create in the owning container
      Result := TpgImageResource(Document.NewElement(TpgImageResource))
    else
      // Create in the wild
      Result := TpgImageResource.Create(nil);
    Reference := Result;
  end;
end;

function TpgImageProp.GetBitmap: TpgBitmap;
var
  Ref: TpgImageResource;
begin
  Result := nil;
  Ref := CheckReference;
  if assigned(Ref) then
    Result := Ref.Bitmap;
end;

procedure TpgImageProp.LoadFromFile(const AFileName: string; Embed: boolean);
var
  Ref: TpgImageResource;
  M: TMemoryStream;
begin
  if Embed then
  begin
    Ref := CheckReference;
    Ref.URI.Value := 'data';
    Ref.MimeType.Value := TpgURIReference.ExtensionToMimeType(ExtractFileExt(AFileName));
    // Copy the file's content to the data
    M := TMemoryStream.Create;
    try
      M.LoadFromFile(AFileName);
      Ref.Data.SetBinary(M.Memory, M.Size);
    finally
      M.Free;
    end;
    // Load the image resource (this fills the bitmap)
    Ref.Load;
  end else
    LoadFromURI('file://' + UriEncode(AFileName));
end;

procedure TpgImageProp.LoadFromURI(const AURI: string);
var
  Ref: TpgImageResource;
begin
  // Check if we have a TpgResource reference
  Ref := CheckReference;
  Ref.URI.Value := AURI;
  Ref.Load;
end;

{ TpgImageView }

function TpgImageView.GetImage: TpgImageProp;
begin
  Result := TpgImageProp(PropById(piImage));
end;

procedure TpgImageView.GetViewBoxProps(var AMinX, AMinY, AWidth,
  AHeight: double);
begin
  // if we have explicitly defined viewbox, we use that, otherwise use the
  // bitmap's dimensions
  inherited;
  if ViewBox.ExistsLocal then
    exit;
  if assigned(Image.Bitmap) then
  begin
    // Use implicitly defined values
    AWidth := Image.Bitmap.Width;
    AHeight := Image.Bitmap.Height;
  end;
end;

initialization

  RegisterElement(eiImageResource, TpgImageResource, 'ImageResource');
  RegisterElement(eiImageView, TpgImageView, 'ImageView');
  RegisterProp(piImage, TpgImageProp, 'Image', TpgImageView, [pfStored]);

end.
