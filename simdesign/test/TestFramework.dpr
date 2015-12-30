program TestFramework;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  pgElement,
  pgScene,
  pgProperties,
  pgStorage,
  pgSVGImport,
  pgTextualStorage,
  pgText in '..\..\Source\Engine\pgText.pas',
  pgCommandPath in '..\..\Source\Engine\pgCommandPath.pas';

type

  TInfo = class
  public
    procedure SvgWarning(Sender: TObject; AMessage: string);
  end;

var
  Scene: TpgScene;
  E1, E2: TpgStyleable;
  Store: TpgStorage;
  M: TMemoryStream;
  SVGImporT: TpgSVGImport;
  Info: TInfo;
{ TInfo }

procedure TInfo.SvgWarning(Sender: TObject; AMessage: string);
begin
  WriteLn(AMessage);
end;

begin
  Scene := TpgScene.Create(nil);
  SVGImport := TpgSVGImport.Create;
  M := TMemoryStream.Create;
  Info := TInfo.Create;
  try
{    E1 := TpgStyleable(Scene.NewElement(TpgStyleable));
    Writeln(E1.Name.Value);
    E1.Name.Value := 'Hello world';
    Writeln(E1.Name.Value);
    E2 := TpgStyleable(Scene.NewElement(TpgStyleable));
    Writeln(E2.Name.Value);
    E2.Clone.Reference := E1;
    Writeln(E2.Name.Value);
    E1.Name.Value := 'Something else';
    Writeln(E1.Name.Value);
    Writeln(E2.Name.Value);
    E2.Name.Value := 'blalba';
    if E2.Name.ExistsLocal then
      Writeln('E2.Name exists');
    M := TMemoryStream.Create;
    Store := TpgBinaryStorage.Create(Scene, M);
    try
      Scene.WriteToStorage(Store);
      M.SaveToFile('c:\temp\format.data');
      Scene.ReadFromStorage(Store);
    finally
      Store.Free;
      M.Free;
    end;}
    // Test SVG
    M.LoadFromFile('C:\AProgram\Delphi\Projects\Dev\Pyrografx\Data\Svg_Examples\demo\matrix.svg');
    SVGImport.OnWarning := Info.SvgWarning;
    SVGImport.ImportScene(Scene, M);
    M.Clear;
    Store := TpgTextualStorage.Create(Scene, M);
    Scene.WriteToStorage(Store);
    M.SaveToFile('c:\temp\format.txt');
    Store.Free;
    ReadLn;
  finally
    SVGImport.Free;
    Scene.Free;
    M.Free;
  end;
end.
