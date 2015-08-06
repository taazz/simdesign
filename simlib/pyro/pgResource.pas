{ Project: Pyro
  Module: Pyro Core

  Description:
  Specific element class that can contain binary data

  Author<: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgResource;

interface

uses
  Classes, SysUtils, pgDocument, pgUriReference, Pyro;

type

  TpgResource = class(TpgRefElement)
  private
    FLoaded: boolean;
    function GetData: TpgBinaryProp;
    function GetUri: TpgStringProp;
    function GetMimeType: TpgStringProp;
  protected
    procedure DoBeforeSave; override;
    function IsChanged: boolean; virtual;
  public
    procedure Load; virtual;
    procedure Save; virtual;
    property URI: TpgStringProp read GetUri;
    property MimeType: TpgStringProp read GetMimeType;
    property Data: TpgBinaryProp read GetData;
    property Loaded: boolean read FLoaded;
  end;

implementation

{ TpgResource }

procedure TpgResource.DoBeforeSave;
// Here we make sure that changes to the resource are saved, and if the
// resource isn't located locally then we can remove the raw data
begin
  if IsChanged then
    Save;
  if (URI.Value <> 'data') then
  begin
    // Remove raw data
    PropById(piData).Delete;
    FLoaded := False;
  end;
end;

function TpgResource.GetData: TpgBinaryProp;
begin
  Result := TpgBinaryProp(PropById(piData));
end;

function TpgResource.GetMimeType: TpgStringProp;
begin
  Result := TpgStringProp(PropById(piMimeType));
end;

function TpgResource.GetUri: TpgStringProp;
begin
  Result := TpgStringProp(PropById(piURI));
end;

function TpgResource.IsChanged: boolean;
begin
  Result := False;
end;

procedure TpgResource.Load;
var
  URIRef: TpgURIReference;
  URIValue: Utf8String;
  M: TMemoryStream;
  MType: Utf8String;
begin
  URIRef := TpgURIReference.Create;
  M := TMemoryStream.Create;
  try
    URIValue := URI.Value;
    if URIValue <> 'data' then
    begin
      URIRef.Parse(URIValue, '');
      URIRef.LoadResource(M, MType);
      MimeType.Value := MType;
      Data.SetBinary(M.Memory, M.Size);
    end;
    FLoaded := True;
  finally
    URIRef.Free;
    M.Free;
  end;
end;

procedure TpgResource.Save;
begin
// default does nothing
end;

initialization

  RegisterElement(eiResource, TpgResource, 'Resource');
  RegisterProp(piData, TpgBinaryProp, 'Data', TpgResource, [pfStored]);
  RegisterProp(piURI, TpgStringProp, 'URI', TpgResource, [pfStored]);
  RegisterProp(piMimeType, TpgStringProp, 'MimeType', TpgResource, [pfStored]);

end.
