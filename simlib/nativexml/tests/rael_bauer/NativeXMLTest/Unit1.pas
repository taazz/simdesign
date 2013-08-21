unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NativeXML;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetContentFile: string;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function Convert(Source: string): Boolean;
var
  XML: TNativeXML;
  S: string;
  P: Integer;
  WS: WideString;

  procedure RunNodes(Root: TXMLNode; Level: Integer);
  var
    I: Integer;
    K: Integer;
    Ext: string;
  begin
    { process node }
    if Root.Name = 'en-note' then
      Root.Name := 'body';
      
    if Root.Name = 'en-media' then
    begin
      Root.Name := 'img';
      Ext := '.dat';
      Root.AttributeValueByName['src'] := Root.AttributeValueByName['hash'] + Ext; // doesn't work
      //Root.AttributeAdd('src', Root.AttributeValueByName['hash'] + Ext);   // works

      K := Root.AttributeIndexByname('type');
      if K>-1 then
        Root.AttributeDelete(K);
      K := Root.AttributeIndexByname('hash');
      if K>-1 then
        Root.AttributeDelete(K);
    end;

    for I := 0 to Root.NodeCount - 1 do
      RunNodes(Root.Nodes[I], Level+1);
  end;

begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(Source);
    if Assigned(XML.Root) then
      RunNodes(XML.Root, 0);

    Xml.XmlFormat := xfReadable;
    Form1.Memo2.Text := XML.WriteToString;

  finally
    XML.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile(GetContentFile);
  Convert(GetContentFile);
end;

function TForm1.GetContentFile: string;
begin
  Result := ExtractFilePath(Application.ExeName)+'\content.xml';
end;

end.
 