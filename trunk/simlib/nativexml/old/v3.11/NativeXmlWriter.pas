{ unit NativeXmlWriter

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit NativeXmlWriter;

interface

{$i NativeXml.inc}

uses
  Classes, SysUtils, NativeXmlUtils;

type

  // TsdBufferWriter is a buffered stream that takes another stream (ASource)
  // and writes only buffer-wise to it, and writes to the stream are first
  // done to the buffer. This stream type can only support writing.
  // The BufferWriter only expects UTF8 formatted data and writes any
  // string encoding based on property Encoding.
  TsdBufferWriter = class(TFastMemStream)
  private
    FSource: TStream;
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
  public
    // Create the buffered writer stream by passing the destination stream in ASource,
    // this destination stream must already be initialized.
    constructor Create(ASource: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    property CodePage: integer read FCodePage write FCodePage;
  end;

implementation

{ TsdBufferWriter }

constructor TsdBufferWriter.Create(ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
end;

function TsdBufferWriter.Read(var Buffer; Count: Integer): Longint;
begin
  // not implemented
  Result := 0;
end;

function TsdBufferWriter.Write(const Buffer; Count: Integer): Longint;
begin
  if FEncoding = seUtf8 then
  begin
    Result := FSource.Write(Buffer, Count);
  end else
  begin
    // todo: write source when Encoding <> seUtf8
    Result := 0;
  end;
end;

end.
