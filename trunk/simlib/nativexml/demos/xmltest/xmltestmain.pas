{ test routines for NativeXml

  author: Nils Haeck M.Sc.

}
unit xmltestmain;

interface

uses
  Classes, Controls, SysUtils, StdCtrls, Forms, Dialogs, Menus, Windows,

  // NativeXml component
  NativeXml,
  sdDebug,
  NativeXmlNodes,
  NativeXmlCodepages;

type
  TfrmMain = class(TForm)
    mmDebug: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuParse: TMenuItem;
    mnuParseCanonical: TMenuItem;
    mnuTest1: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    mnuTest2: TMenuItem;
    mnuTest3: TMenuItem;
    mnuTest4: TMenuItem;
    mnuTest5: TMenuItem;
    mnuTest6: TMenuItem;
    mnuTest7: TMenuItem;
    mnuTest8: TMenuItem;
    mnuTest9: TMenuItem;
    mnuTest10: TMenuItem;
    mnuTest11: TMenuItem;
    mnuTest12: TMenuItem;
    mnuTest13: TMenuItem;
    mnuTest14: TMenuItem;
    mnuTest15: TMenuItem;
    mnuTest16: TMenuItem;
    mnuTest17: TMenuItem;
    mnuTest18: TMenuItem;
    mnuTest19: TMenuItem;
    mnuTest20: TMenuItem;
    Options1: TMenuItem;
    mnuDebugOutput: TMenuItem;
    mnuTest21: TMenuItem;
    mnuTest22: TMenuItem;
    mnuTest23: TMenuItem;
    mnuTest24: TMenuItem;
    mnuTest25: TMenuItem;
    mnuTest26: TMenuItem;
    mnuSaveOutput: TMenuItem;
    mnuTest27: TMenuItem;
    mnuTest28: TMenuItem;
    mnuTest29: TMenuItem;
    mnuTest30: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuParseClick(Sender: TObject);
    procedure mnuParseCanonicalClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuTest3Click(Sender: TObject);
    procedure mnuTest5Click(Sender: TObject);
    procedure mnuTest9Click(Sender: TObject);
    procedure mnuTest10Click(Sender: TObject);
    procedure mnuTest13Click(Sender: TObject);
    procedure mnuTest14Click(Sender: TObject);
    procedure mnuTest15Click(Sender: TObject);
    procedure mnuTest16Click(Sender: TObject);
    procedure mnuTest18Click(Sender: TObject);
    procedure mnuTest19Click(Sender: TObject);
    procedure mnuTest20Click(Sender: TObject);
    procedure mnuTest22Click(Sender: TObject);
    procedure mnuTest23Click(Sender: TObject);
    procedure mnuTest26Click(Sender: TObject);
    procedure mnuTest27Click(Sender: TObject);
    procedure mnuTest28Click(Sender: TObject);
//    procedure mnuTest29Click(Sender: TObject);
//    procedure mnuTest30Click(Sender: TObject);
  private
    FXml: TNativeXml;
    procedure XmlNodeNew(Sender: TObject; ANode: TXmlNode);
    procedure XmlNodeLoaded(Sender: TObject; ANode: TXmlNode);
    procedure Debug(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
    function WriteNodeList(AList: TsdNodeList): Utf8String;
  public
    // testing procedures for user questions
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FXml := TNativeXml.Create(Self);
  mmDebug.Clear;
  FXml.OnNodeNew := XmlNodeNew;
  FXml.OnNodeLoaded := XmlNodeLoaded;
  FXml.OnDebugOut := Debug;
end;


procedure TfrmMain.FormDestroy;
begin
  FXml.Free;
end;


function TfrmMain.WriteNodeList(AList: TsdNodeList): Utf8String;
var
  i: integer;
begin
  for i := 0 to AList.Count - 1 do
  begin
    Result := Result + Format('type=%s name=%s value=%s depth=%d',
      [AList[i].ElementTypeName, AList[i].Name, AList[i].Value, AList[i].TreeDepth]);
  end;
end;

procedure TfrmMain.XmlNodeLoaded(Sender: TObject; ANode: TXmlNode);
begin
  mmDebug.Lines.Add(
    Format('loaded: type "%s" level %d, name "%s", value "%s", subnodes: %d',
     [ANode.ElementTypeName, ANode.TreeDepth, ANode.Name, ANode.Value, ANode.NodeCount]));
end;

procedure TfrmMain.XmlNodeNew(Sender: TObject; ANode: TXmlNode);
begin
  mmDebug.Lines.Add(
    Format('new: type "%s" level %d', [ANode.ElementTypeName, ANode.TreeDepth]));
end;

procedure TfrmMain.mnuParseClick(Sender: TObject);
begin
  FXml.Clear;
  FXml.OnNodeNew := nil;
  FXml.OnNodeLoaded := nil;
  FXml.OnDebugOut := Debug;

  FXml.XmlFormat := xfReadable;
  FXml.IndentString := '';
  FXml.EolStyle := esWindows;
  //FXml.PreserveWhiteSpace := True;
  //FXml.FixStructuralErrors := True;
  FXml.LoadFromFile('..\..\..\..\..\..\admin\simdesign\registrations\mailgroup\abc-view.xml');

  mmDebug.Lines.Add('done');

  FXml.ExternalEncoding := seUtf8;
  FXml.SaveToFile('output.xml');
  mmDebug.Lines.Add('done');
end;

procedure TfrmMain.mnuParseCanonicalClick(Sender: TObject);
begin
  FXml.LoadFromFile('..\..\xml_test_files\sample_with_entity_references.svg');

  // class method
  FXml.Canonicalize;

  FXml.XmlFormat := xfReadable;
  FXml.SaveToFile('output.xml');
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  FXml.AbortParsing := True;
end;

procedure TfrmMain.Debug(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if mnuDebugOutput.Checked then
  begin
    mmDebug.Lines.Add(Format('%s: [%s] %s', [Sender.ClassName, cWarnStyleNames[WarnStyle], AMessage]));
    if mnuSaveOutput.Checked then
      mmDebug.Lines.SaveToFile('debugoutput.txt');
  end;
end;



procedure TfrmMain.mnuTest3Click(Sender: TObject);
var
  Xml: TNativeXml;
  DatBirth: TDateTime;
  Year, Month, Day, Hour, Mn, Sec, Msec: word;
begin
//
  Xml := TNativeXml.CreateName('root');
  try
    DatBirth := EncodeDate(2011, 1, 31) + EncodeTime(13, 59, 0, 0) ;
    Xml.Root.WriteDateTime('DATBIRTH', DATBIRTH);
    DatBirth := Xml.Root.ReadDateTime('DATBIRTH', 0);
    DecodeDate(DatBirth, Year, Month, Day);
    DecodeTime(DatBirth, Hour, Mn, Sec, Msec);


    // Send the XML to the Queue
    mmDebug.Lines.Add(Xml.WriteToString);
    mmDebug.Lines.Add(Format('year=%d, month=%d, day=%d, hour=%d, min=%d, sec=%d',
      [Year, Month, Day, Hour, Mn, Sec]));

  finally
    Xml.Free;
  end;
end;

{procedure TfrmMain.mnuTest4Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.CreateName('root');
  try
    Xml.Root.NodesAdd([

      //a comment node
      Xml.NodeNewTextType('AComment1', 'My comment', xeComment)

    ]);
    mmDebug.Lines.Add(Xml.WriteToString);
  finally
    Xml.Free;
  end;
end;}

procedure TfrmMain.mnuTest5Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.CreateName('root');
  try
    Xml.Root.AttributeAdd('Length', '1''0"');
    mmDebug.Lines.Add(Xml.WriteToString);
    mmDebug.Lines.Add(Xml.Root.AttributeByName['Length'].Value);
  finally
    Xml.Free;
  end;
end;

{procedure TfrmMain.mnuTest6Click(Sender: TObject);
var
  lXMLDoc: TNativeXml;
  lstr: Utf8String;
begin
  lXMLDoc := TNativeXml.CreateName('test');
  lXMLDoc.Root.NodeNew('node1').Value:= sdAnsiToUtf8('e(�c(r(�����', 1250);
  lXMLDoc.XmlFormat := xfReadable;
  lXMLDoc.Root.NodesAdd([ lXMLDoc.NodeNewText('node2','hi') ]);
  lstr := lXMLDoc.WriteToString;

  FreeAndNil(lXMLDoc);
  lXMLDoc:= TNativeXml.Create(nil);
  lXMLDoc.ReadFromString(lStr);

  ShowMessage(sdUtf8ToAnsi(lXMLDoc.Root.NodeByName('node1').Value, 1250));

  lXMLDoc.Free;

end;}

{procedure TfrmMain.mnuTest7Click(Sender: TObject);
var
  lXMLDoc: TNativeXml;
begin
  lXMLDoc := TNativeXml.CreateName('root');
  lXMLDoc.Root.NodeNew('node1').Value:= sdAnsiToUtf8('e(�c(r(�����', 1250);
  lXMLDoc.XmlFormat := xfReadable;
  lXMLDoc.Root.NodesAdd([ lXMLDoc.NodeNewText('node2','hi') ]);
  lXMLDoc.ExternalEncoding := seUTF16BE;
  lXMLDoc.SaveToFile('testUTF16BE.xml');

  lXMLDoc.Free;
end;}

{procedure TfrmMain.mnuTest8Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.CreateName('root');
  try
    Xml.Root.NodesAdd([
      Xml.NodeNewTextAttr('parent', 'somevalue', [],
        [Xml.NodeNew('child1'), Xml.NodeNew('child2'), Xml.NodeNew('child3')]
      )]);

    Xml.XmlFormat := xfReadable;
    Xml.SaveToFile('parentchild.xml');
    mmDebug.Lines.Add(Xml.WriteToString);

    mmDebug.Lines.Add(Format('%d childcontainers', [Xml.Root.NodeByName('parent').ContainerCount]));

  finally
    Xml.Free;
  end;
end;}

procedure TfrmMain.mnuTest9Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.CreateName('root');
  try
    // test WriteAttributeInteger/Float/String/DateTime/Bool
    Xml.Root.WriteAttributeInteger('name1', 5, 0);
    Xml.Root.WriteAttributeFloat('name2', 5.5, 0);
    Xml.Root.WriteAttributeString('name3', 'bla', '');
    Xml.Root.WriteAttributeDateTime('name4',
      sdStringToDateTime('2011-02-11 09:04:33'), sdStringToDateTime('2011-02-11 09:04:32'));
    Xml.Root.WriteAttributeBool('name5',True, False);

    Xml.XmlFormat := xfReadable;
    mmDebug.Lines.Add(Xml.WriteToString);
  finally
    Xml.Free;
  end;
end;

{$ifdef useNativeXmlOld}
procedure TfrmMain.mnuTest10Click(Sender: TObject);
var
  Xml: TNativeXml;
  XmlOld: TNativeXmlOld;
  i: integer;
begin
//
  Xml := TNativeXml.CreateName('root');
  XmlOld := TNativeXmlOld.CreateName('root');

  //
  for i := 1 to 5 do
  begin
    Xml.Root.WriteString('Freetext', Format('Line %d', [i]));
    XmlOld.Root.WriteString('Freetext', Format('Line %d', [i]));
  end;

  mmDebug.Lines.Add(Xml.WriteToString);
  mmDebug.Lines.Add(XmlOld.WriteToString);


  Xml.Free;
  XmlOld.Free;
end;
{$else}
procedure TfrmMain.mnuTest10Click(Sender: TObject);
begin
end;
{$endif}

{procedure TfrmMain.mnuTest11Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.CreateName('description');
  try
    Xml.Root.NodesAdd([

      //freetext nodes
      Xml.NodeNewText('freetext', 'line 1'),
      Xml.NodeNewText('freetext', 'line 2'),
      Xml.NodeNewText('freetext', 'line 3')

    ]);

    Xml.XmlFormat := xfReadable;
    mmDebug.Lines.Add(Xml.WriteToString);
  finally
    Xml.Free;
  end;
end;}

{procedure TfrmMain.mnuTest12Click(Sender: TObject);
var
  Xml: TNativeXml;
  Res1: boolean;
  NL: TsdNodeList;
begin
  Xml := TNativeXml.CreateName('description');
  try
    NL := TsdNodeList.Create(False);
    Xml.Root.NodesAdd([

      //freetext nodes
      Xml.NodeNewTextAttr('freetext', 'line1',
       [Xml.AttrText('bla1', '1')]),
      Xml.NodeNewTextAttr('freetext', 'line1',
       [Xml.AttrText('bla1', '2')]),
      Xml.NodeNewText('freetext', 'line2'),
      Xml.NodeNewText('freetext', 'line3')

    ]);

    Xml.XmlFormat := xfReadable;

    Res1 := Xml.Root.Nodes[0].IsEqualTo(Xml.Root.Nodes[1],
      [xcNodeName, xcAttribValues], NL);
    mmDebug.Lines.Add(Format('name compare = %d', [integer(Res1)]));
//    mmDebug.Lines.Add(Xml.WriteToString);
    mmDebug.Lines.Add(WriteNodeList(NL));
    NL.Free;
  finally
    Xml.Free;
  end;
end;}

procedure TfrmMain.mnuTest13Click(Sender: TObject);
var
  FileName: string;
  XmlNew: TNativeXml;
  //XmlOld: TNativeXmlOld;
  Tick1, Tick2, Tick3: longword;
  FS: TFileStream;
  FileSize: int64;
  // local
  procedure RunStats(Name: string; Size: int64; Ticks: longword);
  begin
    mmDebug.Lines.Add(Format('%s: %d ms for %d bytes, %3.2f Mb/sec',
      [Name, Ticks, Size, Size / (Ticks * 1000)]));
  end;
begin
  // compare parsing speed of TNativeXml and TNativeXmlOld
  FileName := 'c:\trunk\admin\simdesign\registrations\mailgroup\abc-view.xml';
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    FileSize := FS.Size;
  finally
    FS.Free;
  end;

  XmlNew := TNativeXml.Create(nil);
  //XmlOld := TNativeXmlOld.Create;
  try
    Tick1 := GetTickCount;
    mmDebug.Lines.Add('starting run 1');
    XmlNew.LoadFromFile(FileName);
    Tick2 := GetTickCount;
    mmDebug.Lines.Add('starting run 2');
    //XmlOld.LoadFromFile(FileName);
    Tick3 := GetTickCount;
    mmDebug.Lines.Add('finished');

    RunStats('NativeXml 3.2x', FileSize, Tick2 - Tick1);
    RunStats('NativeXml 3.10', FileSize, Tick3 - Tick2);
  finally
    XmlNew.Free;
    //XmlOld.Free;
  end;
end;

procedure TfrmMain.mnuTest14Click(Sender: TObject);
var
  myxml: TNativeXml;
  //myxml2: TNativeXmlOld;
begin
  myxml:=TNativeXML.Create(nil);
  myxml.OnDebugOut := Debug;
  myxml.LoadFromFile('test.xml');
  myxml.XmlFormat:=xfReadable; // just for better readability. the problem occures without this, too
  myxml.SaveToFile('out1.xml');
  myxml.Free;
  //myxml2:=TNativeXMLOld.Create();
  //myxml2.LoadFromFile('test.xml');
  //myxml2.XmlFormat:=xfoReadable; // just for better readability. the problem occures without this, too
  //myxml2.SaveToFile('out2.xml');
  //myxml2.Free;
end;

procedure TfrmMain.mnuTest15Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
  // create xml document with declaration, doctype and root, with rootname ='root'
  Xml := TNativeXml.CreateEx(True, True, Self);
  Xml.XmlFormat := xfReadable;

  // DocType is already there because of CreateEx, just add doctype properties
  //Xml.DocTypeDeclaration.Name := 'blabla';
  //Xml.DocTypeDeclaration.ExternalID.Value := 'SYSTEM';
  //Xml.DocTypeDeclaration.SystemLiteral.Value := 'blabla.dtd';

  mmDebug.Lines.Add(Xml.WriteToString);
  Xml.Free;
end;

procedure TfrmMain.mnuTest16Click(Sender: TObject);
var
  S, E, R: Utf8String;
begin
// sdEscapeString and sdReplaceString

//  S := 'Hi & there, "miaw"';
//  S := 'Hi & there, <miaw>';
  S := 'Osia;gnie;to <%B>';
//  S := 'Hi &copy; there, miaw &quot;';

  mmDebug.Lines.Add('original:');
  mmDebug.Lines.Add(S);
  mmDebug.Lines.Add('escaped:');
  E := sdEscapeString(S);
  mmDebug.Lines.Add(E);
  mmDebug.Lines.Add('replaced:');
  R := sdReplaceString(E);
  mmDebug.Lines.Add(R);
end;

{procedure TfrmMain.mnuTest17Click(Sender: TObject);
// binary xml test
var
  Xml: TNativeXml;
  Bxm: TsdBinaryXml;
  //Table: TsdStringTable;
begin
  Xml := TNativeXml.Create(nil);
  Xml.OnDebugOut := Debug;
  try
    //Xml.XmlFormat := xfPreserve;
    Xml.PreserveWhiteSpace := True;
    //Xml.LoadFromFile('..\..\xml_test_files\error.xml');
    //Xml.LoadFromFile('..\..\xml_test_files\basic.xml');

    Xml.LoadFromFile('..\..\..\..\..\..\admin\simdesign\registrations\mailgroup\abc-view.xml');
    //Xml.LoadFromFile('..\..\..\..\..\..\admin\simdesign\registrations\mailgroup\simdesign.xml');
    //Xml.LoadFromFile('test1.xml');
    //Xml.LoadFromFile('abc-view.xml');
    //Xml.LoadFromFile('..\..\xml_test_files\sample_with_entity_references.svg');

    Debug(Self, wsInfo, 'loaded input');

    Xml.SymbolTable.SaveToFile('stringtable.txt');
    Debug(Self, wsInfo, 'saved stringtable');

    Xml.CharSet := 'UTF-8';
    Xml.SaveToFile('output.xml');
    Debug(Self, wsInfo, 'written output xml');

    Bxm := TsdBinaryXml.Create(Self);
    try
      Bxm.OnDebugOut := Debug;
      Bxm.OnEncode := ZLibEncode;
      Bxm.OnDecode := ZLibDecode;
      Bxm.Document := Xml;
      Bxm.SaveToFile('binary_output_zlib.txt');
    finally
      Bxm.Free;
    end;
    Debug(Self, wsInfo, 'saved binary output');

    Xml.SymbolTable.SaveToFile('stringtable2.txt');
    Debug(Self, wsInfo, 'saved stringtable2');

  finally
    Xml.Free;
  end;

  Xml := TNativeXml.Create(nil);
  Xml.OnDebugOut := Debug;
  try
    //Xml.XmlFormat := xfPreserve;
    Xml.PreserveWhiteSpace := True;

    Bxm := TsdBinaryXml.Create(Self);
    try
      Bxm.OnDebugOut := Debug;
      Bxm.OnEncode := ZLibEncode;
      Bxm.OnDecode := ZLibDecode;
      Bxm.Document := Xml;
      Bxm.LoadFromFile('binary_output_zlib.txt');
    finally
      Bxm.Free;
    end;
    Debug(Self, wsInfo, 'reloaded binary output (zlib)');

    Xml.SaveToFile('reloaded_output.xml');
    Debug(Self, wsInfo, 'written reloaded xml');

    // no compression
    Bxm := TsdBinaryXml.Create(Self);
    try
      Bxm.OnDebugOut := Debug;
      Bxm.OnEncode := nil;
      Bxm.OnDecode := nil;
      Bxm.Document := Xml;
      Bxm.SaveToFile('binary_output_none.txt');
    finally
      Bxm.Free;
    end;
    Debug(Self, wsInfo, 'saved binary output (none)');

    // aes encryption + zlib compression (aesz)
    Bxm := TsdBinaryXml.Create(Self);
    try
      Bxm.OnDebugOut := Debug;
      Bxm.OnEncode := AeszEncode;
      Bxm.OnDecode := AeszDecode;
      Bxm.Document := Xml;
      Bxm.SaveToFile('binary_output_aesz.txt');
      Debug(Self, wsInfo, 'saved binary output (aes + zlib)');
      Bxm.LoadFromFile('binary_output_aesz.txt');
      Debug(Self, wsInfo, 'loaded binary output (aes + zlib)');

      Bxm.OnEncode := nil;
      Bxm.SaveToFile('binary_output_reloaded_none.txt');
      Debug(Self, wsInfo, 'saved reloaded binary output (none)');

    finally
      Bxm.Free;
    end;

    Debug(Self, wsInfo, 'done.');

  finally
    Xml.Free;
  end;
end;}

procedure TfrmMain.mnuTest18Click(Sender: TObject);
begin
  FXml.Clear;
  FXml.OnNodeNew := nil;
  FXml.OnNodeLoaded := nil;
  FXml.OnDebugOut := Debug;

  FXml.XmlFormat := xfReadable;
  FXml.IndentString := '  ';
  FXml.EolStyle := esLinux;

  // load sample with entity references
  FXml.LoadFromFile('..\..\xml_test_files\sample_with_entity_references.svg');

  FXml.Canonicalize;

  FXml.SaveToFile('output.xml');
  mmDebug.Lines.Add('done');
end;

procedure TfrmMain.mnuTest19Click(Sender: TObject);
var
  XmlDocument: TNativeXml;
  Node{, SubNode}: TXmlNode;
begin
  XmlDocument := TNativeXml.Create(nil);       // creates no declaration

  // nh: New is deprecated just for backward compat, use CreateEx and Clear instead

  XmlDocument.New;                             // this creates declaration
  XmlDocument.XmlFormat := xfReadable;
  XmlDocument.FloatSignificantDigits := 9;
  XmlDocument.Root.Name := 'Root';
  Node := XmlDocument.Root.NodeNew('XMLParent');
  {SubNode := }Node.NodeNew('XMLChild');
  {SubNode := }Node.NodeNew('XMLChild-2');
  {SubNode := }Node.NodeNew('XMLChild-3');
  XmlDocument.SaveToFile('test1.xml');
end;

procedure TfrmMain.mnuTest20Click(Sender: TObject);
// date, time, datetime processing in v3.33
var
  Xml: TNativeXml;
  DateNode, TimeNode, DateTimeNode: TXmlNode;
begin
  Xml := TNativeXml.CreateEx(False, False, Self);
  try
    Xml.XmlFormat := xfReadable;
    Xml.UseLocalBias := False;
    Xml.SplitSecondDigits := 0;
    DateNode := Xml.Root.NodeNew('date');
    TimeNode := Xml.Root.NodeNew('time');
    DateTimeNode := Xml.Root.NodeNew('datetime');
    //
    DateNode.ValueAsDate := Now;
    TimeNode.ValueAsTime := Now;
    DateTimeNode.ValueAsDateTime := Now;
    Xml.SaveToFile('test1.xml');
    //
    Xml.SplitSecondDigits := 1;
    Xml.UseLocalBias := True;
    DateNode.ValueAsDate := Now;
    TimeNode.ValueAsTime := Now;
    DateTimeNode.ValueAsDateTime := Now;
    Xml.SaveToFile('test2.xml');
    //
    Xml.SplitSecondDigits := 6; //deliberate error
    Xml.UseLocalBias := False;
    DateNode.ValueAsDate := Now;
    TimeNode.ValueAsTime := Now;
    DateTimeNode.ValueAsDateTime := Now;
    Xml.SaveToFile('test3.xml');
  finally
    Xml.Free;
  end;
end;

{procedure TfrmMain.mnuTest21Click(Sender: TObject);
// test of NativeXml v4.01 changes
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    Xml.XmlFormat := xfPreserve; // in future maybe default?
    Xml.EolStyle := esCRLF;

    Xml.LoadFromFile('sample.xml');
    Xml.SaveToFile('encoding_unspecified.xml');

    Debug(Self, wsInfo, format('eolstyle=%d', [ord(Xml.EolStyle)]));

    // test encodings
    Xml.ExternalEncoding := seUTF8;
    Xml.SaveToFile('encoding_utf8.xml');
    Xml.ExternalEncoding := seUTF16BE;
    Xml.SaveToFile('encoding_utf16be.xml');
    Xml.ExternalEncoding := seUTF16LE;
    Xml.SaveToFile('encoding_utf16le.xml');
    //utf32 is really never used and nativexml does not support it
{    Xml.ExternalEncoding := seUTF32BE;
    Xml.SaveToFile('encoding_utf32be.xml');
    Xml.ExternalEncoding := seUTF32LE;
    Xml.SaveToFile('encoding_utf32le.xml');

    /* test charset */
    Xml.Charset := 'windows-1252';
    Xml.SaveToFile('encoding_windows1252.xml');

    Xml.ExternalEncoding := seUTF8;
    Xml.XmlFormat := xfCompact;
    Xml.SaveToFile('compact_utf8_before.xml');

    Xml.LoadFromFile('sample.xml');
    Xml.SaveToFile('compact_after.xml');
    Xml.XmlFormat := xfReadable;
    Xml.SaveToFile('readable_after.xml');

    Xml.BinaryMethod := bmDefault;
    Xml.SaveToBinaryFile('binary_none.txt');
    Xml.BinaryMethod := bmZLib;
    Xml.SaveToBinaryFile('binary_zlib.txt');
    Xml.LoadFromBinaryFile('binary_zlib.txt');

// needs USEAES
    Xml.BinaryMethod := bmAesz;
    Xml.SaveToBinaryFile('binary_aesz.txt');

    // fix structural errors enhancement
    Xml.XmlFormat := xfPreserve;
    Xml.FixStructuralErrors := True;
//    Xml.FixStructuralErrors := False;
    //Xml.LoadFromFile('xml_orig.html');
    //Xml.SaveToFile('xml_new.html');


  finally
    Xml.Free;
  end;

end;}

procedure TfrmMain.mnuTest22Click(Sender: TObject);
var
  Xml: TNativeXml;
  Node, Sub: TXmlNode;
  StrRoot, StrTest: WideString;
begin
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    Xml.XmlFormat := xfPreserve;
    Xml.LoadFromFile('test_utf16.xml');
    Xml.SaveToFile('test_saved.xml');

  finally
    Xml.Free;
  end;

  StrRoot := 'ROOT';
  StrTest := 'TEST';

  Xml := TNativeXml.CreateName(StrRoot);
  try
    Xml.OnDebugOut := Debug;
    Node := Xml.Root.NodeNew(StrTest);
    Sub := Node.NodeNew('NAME');
    Sub.ValueUnicode := 'Giovanni Praet';
    Sub := Node.NodeNew('DATE');
    Sub.ValueAsDateTime := Now;
    Xml.Charset := 'utf-16';
    Xml.XmlFormat := xfReadable;
    Xml.SaveToFile('test_saved2.xml');

  finally
    Xml.Free;
  end;

end;

procedure TfrmMain.mnuTest23Click(Sender: TObject);
// test DTD element and entity (4.02 changes)
var
  Xml: TNativeXml;
  S: string;
  R: TXmlNode;
  Count: integer;
begin
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    Xml.XmlFormat := xfPreserve;
    Xml.LoadFromFile('../../xml_test_files/dtd_entity.xml');
    Xml.SaveToFile('test1.xml');
    Xml.LoadFromFile('../../xml_test_files/sample_with_entity_references.svg');
    Xml.SaveToFile('test2.xml');

    // mem leak?
    S := Xml.Root.WriteToString; // fixed in 4.02

    // test attributesclear / attributevaluebyname / elementcount / elements / elementsclear
    R := Xml.Root;
    R.AttributesClear;
    Count := R.AttributeCount;
    Debug(Self, wsInfo, format('attribute count=%d', [Count]));
    Count := R.ContainerCount;
    Debug(Self, wsInfo, format('element count=%d', [Count]));
    R.AttributeValueByName['bla'] := 'test';
    Count := R.AttributeCount;
    Debug(Self, wsInfo, format('attribute count=%d', [Count]));
    Debug(Self, wsInfo, R.AttributeValueByName['bla']);
    if R.ContainerCount > 0 then
    begin
      S := R.Containers[0].Name;
      Debug(Self, wsInfo, S);
    end;
    //R.ContainersClear;  todo
    Debug(Self, wsInfo, R.AttributeValueByName['bla']); // fixed in 4.02

  finally
    Xml.Free;
  end;
end;

{procedure TfrmMain.mnuTest24Click(Sender: TObject);
var
  Xml: TNativeXml;
begin
// test of binary xml after changing to 4.02
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    // in 4.02 we use XmlFormat := xfPreserve by default too
    Xml.XmlFormat := xfPreserve;
    //Xml.LoadFromFile('../../xml_test_files/sample_stylesheet.xml');
    Xml.LoadFromFile('../../xml_test_files/sample_with_entity_references.svg');
    //Xml.LoadFromFile('../../xml_test_files/basic.xml');
    //Xml.LoadFromFile('../../xml_test_files/sample_big_output.xml');

    Xml.EolStyle := esCRLF; // this is also the default, but eolstyle may get changed by
    //the parser
    Xml.Root.NodeNew('node123').Value := 'bla1' + #13#10 + 'bla2' + #13#10 + 'bla3';
    Xml.SaveToFile('test.xml');
    Xml.SaveToBinaryFile('test.bxm');

    Xml.EolStyle := esLF; // end-of-line for Linux
    Xml.SaveToFile('test_for_linux.xml');
    Xml.EolStyle := esCR; // end-of-line for Mac
    Xml.SaveToFile('test_for_mac.xml');
  finally
    Xml.Free;
  end;

  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    Xml.LoadFromFile('test.bxm');
    Xml.SaveToFile('test2.xml');
  finally
    Xml.Free;
  end;
end;}

{procedure TfrmMain.mnuTest25Click(Sender: TObject);
var
  myXML: TNativeXml;
begin
// test that shows a problem: first load, then set xfReadable: triggers TNativeXml.RemoveWhiteSpace.
// Not a problem, except the routine RemoveWhiteSpace is buggy (see the tests)
  myXML := TNativeXml.Create(Self);
  try
    myXML.OnDebugOut := Debug;
    myXML.LoadFromFile('test_hardworking.xml');
    myXML.SaveToBinaryFile('test_binary1.bxm');
    //myXML.XmlFormat := xfCompact;
    myXML.XmlFormat := xfReadable;
    myXML.SaveToFile('test_readable.xml'); //bleibt das 3.Attribut erhalten?
    myXML.SaveToBinaryFile('test_binary2.bxm');

    myXML.Clear;
    myXML.LoadFromBinaryFile('test_binary2.bxm');
    myXML.SaveToFile('test_after.xml')
  finally
    myXML.Free;
  end;
end;}

procedure TfrmMain.mnuTest26Click(Sender: TObject);
var
  Xml: TNativeXml;
  Node, Sub: TXmlNode;
begin
  XML := TNativeXml.Create(Self);
  try
    XML.OnDebugOut := Debug;
    XML.LoadFromFile('normal_references.xml');
    Node := XML.Root;
    Sub := Node.NodeByName('name');
    if assigned(Sub) then
    begin
      Debug(Self, wsInfo, Sub.Value);
    end;
  finally
    XML.Free;
  end;
end;

procedure TfrmMain.mnuTest27Click(Sender: TObject);
// test 27: comparison of NodeRemove vs NodeLineRemove
var
  Xml: TNativeXml;
  HistNode, FileNode: TXmlNode;
begin
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    Xml.LoadFromFile('test_flavien.xml');
    Xml.SaveToFile('test_f2.xml');

    // save with NodeRemove
    HistNode := Xml.Root;
    FileNode := HistNode.NodeByName('File');
    HistNode.NodeRemove(FileNode);
    Xml.SaveToFile('test_f3.xml');

    Xml.LoadFromFile('test_flavien.xml');

    // save with NodeRemoveEx
    HistNode := Xml.Root;
    FileNode := HistNode.NodeByName('File');
    HistNode.NodeRemove(FileNode);
    Xml.SaveToFile('test_f4.xml');

    // save with xfReadable
    Xml.XmlFormat := xfReadable;
    Xml.SaveToFile('test_f5.xml');
  finally
    Xml.Free;
  end;
end;

procedure TfrmMain.mnuTest28Click(Sender: TObject);
// test 28: start with an ansi encoded (windows 1252) xml file, then convert to utf-8
var
  Xml: TNativeXml;
begin
//
  Xml := TNativeXml.Create(Self);
  try
    Xml.OnDebugOut := Debug;
    // start with a windows-1252-encoded file (ansi)
    Xml.LoadFromFile('encoding_windows_1252.xml');

    // now switch to utf-8 and save the unicode result
    Xml.CharSet := 'utf-8';
    Xml.SaveToFile('encoding_utf_8.xml');

    // finally, to verify, convert back and write the windows-1252-encoded file
    Xml.CharSet := 'windows-1252';
    Xml.SaveToFile('encoding_windows_1252_again.xml');
  finally
    Xml.Free;
  end;
end;

{procedure TfrmMain.mnuTest29Click(Sender: TObject);
// test 29: check if AttributeIndexByName works for TsdDeclaration
var
  Xml: TNativeXml;
  Idx: integer;
begin
//
  // create xml document with declaration, doctype and root, with rootname ='root'
  Xml := TNativeXml.CreateEx(True, True, Self);
  try
    Xml.OnDebugOut := Debug;

//    Idx := Xml.Declaration.AttributeIndexByName('encoding'); todo
    Debug(Self, wsInfo, Format('Idx = %d', [Idx]));

    // load a windows-1252-encoded file (ansi)
    Xml.LoadFromFile('encoding_windows_1252.xml');
//    Idx := Xml.Declaration.AttributeIndexByName('encoding'); todo
    Debug(Self, wsInfo, Format('Idx = %d', [Idx]));

  finally
    Xml.Free;
  end;
end;}

{procedure TfrmMain.mnuTest30Click(Sender: TObject);
var
  Scratch: TNativeXml; // a scratch document
  ADoc: TNativeXml; // the actual document that is used
  Node: TXmlNode;
begin
  // create a scratch document
  Scratch := TNativeXml.Create(nil);
  try
    Node := TsdElement.CreateParent(Scratch, Scratch.Root);
    Node.Name := 'Message';
    Node.WriteAttributeString('Origen', '1');
    Node.NodeNew('Another_node');

    // the actual document in a later stage, with all kinds of options..
    // However, ensure that the Root is created otherwise it cannot be copied from
    ADoc := TNativeXml.CreateEx(nil, True, False, True, 'bla');
    try
      //ADoc.Root.NodeAdd(Node); // NodeAdd() should NOT be used across two TNativeXml instances!
      // Instead, use Assign(). It works well across multiple documents
      ADoc.Root.Assign(Node);

      // verify..
      ADoc.SaveToFile('test.xml');
    finally
      ADoc.Free;
    end;
  finally
    Scratch.Free;
  end;
end;}

end.
