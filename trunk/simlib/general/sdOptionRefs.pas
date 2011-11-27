{ Unit sdOptionRefs

  This unit implements an easy way of storing and retrieving option parameters
  for an application.

  By creating a table with references to the options, you can easily save and
  load them in one command.

  Suppose you have a boolean option FSortByName
  - You don't want to change this kind of var in a complete structure due to access time.
  - You can then add a *reference* option
  Opt.Add(FSortByName, 'Sorting', 'SortByName');

  Do this for all your options. With method
   Opt.SaveToIni(AFile: ustring)
  and
   Opt.LoadFromIni(AFile: ustring)

  You can thus easily save/load your options!

  The option FSortByName will be saved in the INI as
  [Sorting]
  SortByName=0

  etc..

  Storable formats:
  boolean, byte, word, integer, ustring,
  TColor (cast to integer)
  Enumerated types (cast to byte)
  TFont (properties Name, Size, Color, Style(bold/italic)

  Copyright (c) 2002 - 2010 by Nils Haeck (Simdesign)
  For more information see www.simdesign.nl

  Projects that use this unit:
    DtpDocuments
    AbcView

}
unit sdOptionRefs;

{.$i simdesign.inc}

interface

uses
  Windows, Classes, Contnrs, SysUtils, Graphics, IniFiles, Registry;

type
  // Exception class used within TOptionsManager
  EOptionsError = class(Exception);

  // Available data types for storage
  TOptionsDataType =
    (odBoolean, odByte, odWord, odInteger, odString, odSingle, odDouble, odDateTime,
     odPoint, odFont, odRect, odStrings);

  TOptionsManager = class;

  // TOptionsItem represents one option from the Items[] list of TOptionsManager
  TOptionsItem = class
  private
    FDataType: TOptionsDataType;
    FIdent: Utf8String;
    FSection: Utf8String;
  protected
  public
    Ref: pointer;
    property DataType: TOptionsDataType read FDataType write FDataType;
    property Ident: Utf8String read FIdent write FIdent;
    procedure ReadFromIni(Ini: TCustomIniFile);
    procedure ReadFromReg(Reg: TRegistry; ASection: Utf8String);
    property Section: Utf8String read FSection write FSection;
    procedure WriteToIni(Ini: TCustomIniFile);
    procedure WriteToReg(Reg: TRegistry; ASection: Utf8String);
  end;

  TOptionsLoadMethod = (
    lmIniFile,
//    lmRegistry,
    lmXmlFile
  );

  // Use TOptionsManager to centrally provide loading and storing functionality for
  // all kinds of global options within the application.
  TOptionsManager = class(TComponent)
  private
    FItems: TObjectList;
    FSaveOnExit: boolean;
    FMethod: TOptionsLoadMethod;
    FFileName: string;
  protected
    function AddGeneric(ARef: pointer; ADataType: TOptionsDataType;
      ASection, AIdent: Utf8String): integer;
    function GetCount: integer;
    function GetItems(Index: integer): TOptionsItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(var ABool: boolean;      const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var AByte: byte;         const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var AWord: word;         const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var AInteger: integer;   const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var AColor: TColor;      const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var AString: Utf8String; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var ADouble: double;     const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var ARect: TRect;        const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(var ASingle: single;     const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    function Add(AObject: TObject;        const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer; overload;
    procedure Delete(Index: integer);
    procedure LoadFromIni(AIniFile: Utf8String);
//    procedure LoadFromReg(const AKey: Utf8String);
    procedure SaveToIni(AIniFile: Utf8String);
//    procedure SaveToReg(const AKey: Utf8String);
    procedure SaveToXml(AXmlFile: Utf8String);
    procedure Save;
    procedure RemoveObjectRefs;
    property Count: integer read GetCount;
    property Items[Index: integer]: TOptionsItem read GetItems; default;
    property SaveOnExit: boolean read FSaveOnExit write FSaveOnExit;
  end;

resourcestring

  sCannotOpenRegKey = 'Cannot open registry key %s';
  sReferenceNotAssigned = 'Reference is not assigned';

implementation

{ TOptionsItem }

procedure TOptionsItem.ReadFromIni(Ini: TCustomIniFile);
begin
  with Ini do begin
    case DataType of
    odBoolean:
      boolean(Ref^) := Ini.ReadBool(Section, Ident, boolean(Ref^));
    odByte:
      byte(Ref^) := Ini.ReadInteger(Section, Ident, byte(Ref^));
    odWord:
      word(Ref^) := Ini.ReadInteger(Section, Ident, word(Ref^));
    odInteger:
      integer(Ref^) := Ini.ReadInteger(Section, Ident, integer(Ref^));
    odString:
      Utf8String(Ref^) := Ini.ReadString(Section, Ident, Utf8String(Ref^));
    odSingle:
      single(Ref^) := Ini.ReadFloat(Section, Ident, single(Ref^));
    odDouble:
      double(Ref^) := Ini.ReadFloat(Section, Ident, double(Ref^));
    odRect:
      with TRect(Ref^) do
      begin
        Left   := Ini.ReadInteger(Section, Ident + '_Left'  , Left);
        Top    := Ini.ReadInteger(Section, Ident + '_Top'   , Top);
        Right  := Ini.ReadInteger(Section, Ident + '_Right' , Right);
        Bottom := Ini.ReadInteger(Section, Ident + '_Bottom', Bottom);
      end;
    odFont:
      with TObject(Ref) as TFont do
      begin
        Name := Ini.ReadString(Section, Ident + '_Name', Name);
        Size := Ini.ReadInteger(Section, Ident + '_Size', Size);
        Color := Ini.ReadInteger(Section, Ident + '_Color', Color);
        if Ini.ReadBool(Section, Ident + '_Bold', fsBold in Style) then
          Style := Style + [fsBold]
        else
          Style := Style - [fsBold];
        if Ini.ReadBool(Section, Ident + '_Italic', fsItalic in Style) then
          Style := Style + [fsItalic]
        else
          Style := Style - [fsItalic];
      end;
    else
      raise EOptionsError.Create('Unknown option type');
    end;//case
  end;
end;

procedure TOptionsItem.ReadFromReg(Reg: TRegistry; ASection: Utf8String);
begin
  with Reg do
  begin
    // Open the registry key. If it does not exist, don't try to create it, just leave
    if not Reg.OpenKey(ASection, False) then
      exit;
    try
      case DataType of
      odBoolean:
        boolean(Ref^) := Reg.ReadBool(Ident);
      odByte:
        byte(Ref^) := Reg.ReadInteger(Ident);
      odWord:
        word(Ref^) := Reg.ReadInteger(Ident);
      odInteger:
        integer(Ref^) := Reg.ReadInteger(Ident);
      odString:
        Utf8String(Ref^) := Reg.ReadString(Ident);
      odSingle:
        single(Ref^) := Reg.ReadFloat(Ident);
      odDouble:
        double(Ref^) := Reg.ReadFloat(Ident);
      odRect:
        with TRect(Ref^) do
        begin
          Left   := Reg.ReadInteger(Ident + '_Left');
          Top    := Reg.ReadInteger(Ident + '_Top');
          Right  := Reg.ReadInteger(Ident + '_Right');
          Bottom := Reg.ReadInteger(Ident + '_Bottom');
        end;
      odFont:
        with TObject(Ref) as TFont do
        begin
          Name := Reg.ReadString(Ident + '_Name');
          Size := Reg.ReadInteger(Ident + '_Size');
          Color := Reg.ReadInteger(Ident + '_Color');
          if Reg.ReadBool(Ident + '_Bold') then
            Style := Style + [fsBold]
          else
            Style := Style - [fsBold];
          if Reg.ReadBool(Ident + '_Italic') then
            Style := Style + [fsItalic]
          else
            Style := Style - [fsItalic];
        end;
      else
        raise EOptionsError.Create('Unknown option type');
      end;//case
    except
      On E: Exception do begin
        // We catch the exceptions here. Usually unknown options due to versioning.
        // It should not break the flow of the calling code. Default values will
        // be used.
      end;
    end;
  end;
end;

procedure TOptionsItem.WriteToIni(Ini: TCustomIniFile);
begin
  with Ini do
  begin
    case DataType of
    odBoolean:
      Ini.WriteBool(Section, Ident, boolean(Ref^));
    odByte:
      Ini.WriteInteger(Section, Ident, byte(Ref^));
    odWord:
      Ini.WriteInteger(Section, Ident, word(Ref^));
    odInteger:
      Ini.WriteInteger(Section, Ident, integer(Ref^));
    odString:
      Ini.WriteString(Section, Ident, Utf8String(Ref^));
    odSingle:
      Ini.WriteFloat(Section, Ident, single(Ref^));
    odDouble:
      Ini.WriteFloat(Section, Ident, double(Ref^));
    odRect:
      with TRect(Ref^) do
      begin
        Ini.WriteInteger(Section, Ident + '_Left'  , Left);
        Ini.WriteInteger(Section, Ident + '_Top'   , Top);
        Ini.WriteInteger(Section, Ident + '_Right' , Right);
        Ini.WriteInteger(Section, Ident + '_Bottom', Bottom);
      end;
    odFont:
      with TObject(Ref) as TFont do
      begin
        Ini.WriteString(Section, Ident + '_Name', Name);
        Ini.WriteInteger(Section, Ident + '_Size', Size);
        Ini.WriteInteger(Section, Ident + '_Color', Color);
        Ini.WriteBool(Section, Ident + '_Bold',   fsBold in Style);
        Ini.WriteBool(Section, Ident + '_Italic', fsItalic in Style);
      end;
    else
      raise EOptionsError.Create('Unknown option type');
    end;//case
  end;
end;

procedure TOptionsItem.WriteToReg(Reg: TRegistry; ASection: Utf8String);
begin
  with Reg do
  begin
    if not OpenKey(ASection, True) then
      raise EOptionsError.CreateFmt(sCannotOpenRegKey, [ASection]);
    case DataType of
    odBoolean:
      Reg.WriteBool(Ident, boolean(Ref^));
    odByte:
      Reg.WriteInteger(Ident, byte(Ref^));
    odWord:
      Reg.WriteInteger(Ident, word(Ref^));
    odInteger:
      Reg.WriteInteger( Ident, integer(Ref^));
    odString:
      Reg.WriteString(Ident, Utf8String(Ref^));
    odSingle:
      Reg.WriteFloat(Ident, single(Ref^));
    odDouble:
      Reg.WriteFloat(Ident, double(Ref^));
    odRect:
      with TRect(Ref^) do
      begin
        Reg.WriteInteger(Ident + '_Left'  , Left);
        Reg.WriteInteger(Ident + '_Top'   , Top);
        Reg.WriteInteger(Ident + '_Right' , Right);
        Reg.WriteInteger(Ident + '_Bottom', Bottom);
      end;
    odFont:
      with TObject(Ref) as TFont do
      begin
        Reg.WriteString(Ident + '_Name', Name);
        Reg.WriteInteger(Ident + '_Size', Size);
        Reg.WriteInteger(Ident + '_Color', Color);
        Reg.WriteBool(Ident + '_Bold',   fsBold in Style);
        Reg.WriteBool(Ident + '_Italic', fsItalic in Style);
      end;
    else
      raise EOptionsError.Create('Unknown option type');
    end;//case
  end;
  Reg.CloseKey;
end;

{ TOptionsManager }

function TOptionsManager.Add(var ABool: boolean; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@ABool, odBoolean, ASection, AIdent);
end;

function TOptionsManager.Add(var AByte: byte; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@AByte, odByte, ASection, AIdent);
end;

function TOptionsManager.Add(var AColor: TColor; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@AColor, odInteger, ASection, AIdent);
end;

function TOptionsManager.Add(var ADouble: double; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@ADouble, odDouble, ASection, AIdent);
end;

function TOptionsManager.Add(var AInteger: integer; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@AInteger, odInteger, ASection, AIdent);
end;

function TOptionsManager.Add(AObject: TObject; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  if AObject.ClassType = TFont then begin
    Result := AddGeneric(AObject, odFont, ASection, AIdent);
    exit;
  end;
  // if we arrive here, no savable object was found
  raise EOptionsError.Create('Unknown option type');
end;

function TOptionsManager.Add(var ARect: TRect; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@ARect, odRect, ASection, AIdent);
end;

function TOptionsManager.Add(var ASingle: single; const ASection: Utf8String; AIdent: Utf8String): integer;
begin
  Result := AddGeneric(@ASingle, odSingle, ASection, AIdent);
end;

function TOptionsManager.Add(var AString: Utf8String; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@AString, odString, ASection, AIdent);
end;

function TOptionsManager.Add(var AWord: word; const ASection: Utf8String = ''; AIdent: Utf8String = ''): integer;
begin
  Result := AddGeneric(@AWord, odWord, ASection, AIdent);
end;

function TOptionsManager.AddGeneric(ARef: pointer; ADataType: TOptionsDataType;
  ASection, AIdent: Utf8String): integer;
var
  Item: TOptionsItem;
begin
  if length(ASection) = 0 then
    ASection := 'Options';
  if length(AIdent) = 0 then
    AIdent := Format('Option%.4d', [Count]);
  if assigned(ARef) then
  begin
    Item := TOptionsItem.Create;
    with Item do
    begin
      Ref := ARef;
      Section := ASection;
      Ident := AIdent;
      DataType := ADataType;
    end;
    Result := FItems.Add(Item);
  end else
  begin
    raise EOptionsError.Create(sReferenceNotAssigned);
  end;
end;

constructor TOptionsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create;
end;

procedure TOptionsManager.Delete(Index: integer);
begin
  if assigned(FItems) and (Index >= 0) and (Index < FItems.Count) then
    FItems.Delete(Index);
end;

destructor TOptionsManager.Destroy;
begin
  if FSaveOnExit then
    Save;
  if assigned(FItems) then
    FreeAndNil(FItems);
  inherited;
end;

function TOptionsManager.GetCount: integer;
begin
  Result := 0;
  if assigned(FItems) then
    Result := FItems.Count;
end;

function TOptionsManager.GetItems(Index: integer): TOptionsItem;
begin
  Result := nil;
  if assigned(FItems) and (Index >= 0) and (Index < FItems.Count) then
    Result := TOptionsItem(FItems[Index]);
end;

procedure TOptionsManager.LoadFromIni(AIniFile: Utf8String);
var
  i: integer;
  Ini: TMemIniFile;
begin
  FMethod := lmIniFile;
  FFileName := AIniFile;
  Ini := TMemIniFile.Create(AIniFile);
  try
    for i := 0 to Count - 1 do
      Items[i].ReadFromIni(Ini);
  finally
    Ini.Free;
  end;
end;

{procedure TOptionsManager.LoadFromReg(const AKey: Utf8String);
var
  i: integer;
  Reg: TRegistry;
begin
  FMethod := lmRegistry;
  FFileName := AKey;
  // This opens HKEY_CURRENT_USER
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    // And load the data from it
    for i := 0 to Count - 1 do
      Items[i].ReadFromReg(Reg, AKey + '\' +Items[i].Section);
  finally
    Reg.Free;
  end;
end;}

procedure TOptionsManager.RemoveObjectRefs;
var
  i: integer;
begin
  i := 0;
  while i < Count do
  begin
    if assigned(Items[i]) then
      if Items[i].DataType in [odFont] then
        Delete(i)
      else
        inc(i);
  end;
end;

procedure TOptionsManager.Save;
begin
  if length(FFileName) > 0 then
  begin
    case FMethod of
    lmIniFile:  SaveToIni(FFileName);
//    lmRegistry: SaveToReg(FFileName);
    lmXmlFile:  SaveToXml(FFileName);
    end;
  end;
end;

procedure TOptionsManager.SaveToIni(AIniFile: Utf8String);
var
  i: integer;
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(AIniFile);
  try
    for i := 0 to Count - 1 do
      Items[i].WriteToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

{procedure TOptionsManager.SaveToReg(const AKey: Utf8String);
var
  i: integer;
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    for i := 0 to Count - 1 do
      Items[i].WriteToReg(Reg, AKey + '\' + Items[i].Section);
  finally
    Reg.Free;
  end;
end;}

procedure TOptionsManager.SaveToXml(AXmlFile: Utf8String);
begin
//todo
end;

end.
