unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NativeXml, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
  FXml: TNativeXml;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
begin
  // create nativexml document FXml with rootname 'root'
  FXml := TNativeXml.CreateName('root');
  try
     //FXml.Att
    //save the xml document
    FXml.SaveToFile('xmltest.xml');
  finally

  // free the xml document
    FXml.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
// button2: test a (faulty?) doctype svg file. It could complain but not crash with 'index out of bounds'
begin
  FXml := TNativeXml.Create(nil);
  FXml.LoadFromFile('d:/simdesign/source/simlib/nativexml/demos//xml_test_files/basic.xml');
end;

end.