{ Project: Pyro
  Module: Core Document Engine

  Description:
    A ViewPort is a Group descendant implementing a viewport transform, consisting
    of a viewport and viewbox definition

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgViewPort;

{$i simdesign.inc}

interface

uses
  SysUtils, Contnrs,
  pgColor, pgTransform, pgDocument, pgPath, Pyro;

{ former pgSizeable.pas }

type

  TpgLengthProp = class(TpgFloatProp)
  private
    FUnits: TpgLengthUnits;
    procedure SetUnits(const Value: TpgLengthUnits);
  protected
    class function GetDirection: TpgCartesianDirection; virtual;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
  public
    property Units: TpgLengthUnits read FUnits write SetUnits;
    property Direction: TpgCartesianDirection read GetDirection;
    // Convert the value + units combination to device units, depending on
    // settings in the device (DPI).
    function ToDevice(const AInfo: TpgDeviceInfo): double;
  end;

  TpgHLengthProp = class(TpgLengthProp)
  protected
    class function GetDirection: TpgCartesianDirection; override;
  end;

  TpgVLengthProp = class(TpgLengthProp)
  protected
    class function GetDirection: TpgCartesianDirection; override;
  end;

  TpgFloatListProp = class;

  TpgFloatItem = class
  private
    FOwner: TpgFloatListProp;
    FValue: double;
    procedure SetValue(const Value: double);
  public
    constructor Create(AOwner: TpgFloatListProp);
    property Value: double read FValue write SetValue;
  end;

  TpgFloatProtectList = class(TObjectList)
  private
    FOwner: TpgFloatListProp;
    function GetItems(Index: integer): TpgFloatItem;
  protected
    procedure AddInternal(Value: double);
    procedure CopyFrom(AList: TpgFLoatProtectList);
  public
    constructor Create(AOwner: TpgFloatListProp);
    property Items[Index: integer]: TpgFloatItem read GetItems; default;
    procedure Add(AItem: TpgFloatItem);
  end;

  TpgFloatListProp = class(TpgStoredProp)
  private
    FValues: TpgFloatProtectList;
    procedure SetValues(const Value: TpgFloatProtectList);
  protected
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
    constructor CreateInternal(AId: longword); override;
  public
    destructor Destroy; override;
    property Values: TpgFloatProtectList read FValues write SetValues;
    procedure Add(Value: double);
  end;

  TpgLengthListProp = class;

  //todo: no ancestor class?
  TpgLengthItem = class
  private
    FOwner: TpgLengthListProp;
    FValue: double;
    FUnits: TpgLengthUnits;
    procedure SetUnits(const Value: TpgLengthUnits);
    procedure SetValue(const Value: double);
  public
    constructor Create(AOwner: TpgLengthListProp);
    property Units: TpgLengthUnits read FUnits write SetUnits;
    property Value: double read FValue write SetValue;
    function ToDevice(const AInfo: TpgDeviceInfo): double;
  end;

  TpgLengthProtectList = class(TObjectList)
  private
    FOwner: TpgLengthListProp;
    function GetItems(Index: integer): TpgLengthItem;
  protected
    procedure AddInternal(Value: double; Units: TpgLengthUnits);
    procedure CopyFrom(AList: TpgLengthProtectList);
  public
    constructor Create(AOwner: TpgLengthListProp);
    property Items[Index: integer]: TpgLengthItem read GetItems; default;
    procedure Add(AItem: TpgLengthItem);
  end;

  TpgLengthListProp = class(TpgStoredProp)
  private
    FValues: TpgLengthProtectList;
    procedure SetValues(const Value: TpgLengthProtectList);
  protected
    class function GetDirection: TpgCartesianDirection; virtual;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
    constructor CreateInternal(AId: longword); override;
  public
    destructor Destroy; override;
    property Direction: TpgCartesianDirection read GetDirection;
    property Values: TpgLengthProtectList read FValues write SetValues;
    procedure Add(Value: double; Units: TpgLengthUnits);
  end;

  TpgHLengthListProp = class(TpgLengthListProp)
  protected
    class function GetDirection: TpgCartesianDirection; override;
  end;

  TpgVLengthListProp = class(TpgLengthListProp)
  protected
    class function GetDirection: TpgCartesianDirection; override;
  end;

  TpgSizeable = class(TpgStyleable)
  protected
    procedure GetViewPortSize(var Width, Height: double; const AInfo: TpgDeviceInfo);
    function GetFontHeight(const AInfo: TpgDeviceInfo): double;
    function ResolveLength(const AValue: double; AUnits: TpgLengthUnits;
      ADirection: TpgCartesianDirection; const AInfo: TpgDeviceInfo): double;
  public
  end;

{ former pgPaintable.pas }

type

  TpgFillRuleProp = class(TpgIntProp)
  private
    function GetValue: TpgFillRule;
    procedure SetValue(const Value: TpgFillRule);
  public
    property Value: TpgFillRule read GetValue write SetValue;
  end;

  // Property specifying paint, which can be none, a solid color, or a reference
  // to a paint server (like gradient or pattern).
  TpgPaintProp = class(TpgRefProp)
  private
    FColor: TpgColor;
    FPaintStyle: TpgPaintStyle;
    function GetAsColor32: TpgColor32;
    procedure SetAsColor32(const Value: TpgColor32);
    procedure SetPaintStyle(const Value: TpgPaintStyle);
  protected
    function GetColorInfo: PpgColorInfo;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
  public
    // Color stores the color in general format, with the current colorspace
    // on use in the parent paintable.
    property Color: TpgColor read FColor;
    // Selected paint style (ptNone, ptColor, ptReference)
    property PaintStyle: TpgPaintStyle read FPaintStyle write SetPaintStyle;
    // Get and set the color as an ARGB 4Channel 8bpc non-premultiplied color
    property AsColor32: TpgColor32 read GetAsColor32 write SetAsColor32;
    property ColorInfo: PpgColorInfo read GetColorInfo;
  end;

  TpgPaintable = class(TpgSizeable)
  private
    function GetFill: TpgPaintProp;
    function GetStroke: TpgPaintProp;
    function GetStrokeWidth: TpgLengthProp;
    function GetFillRule: TpgFillRuleProp;
    function GetFontSize: TpgVLengthProp;
    function GetFillOpacity: TpgFloatProp;
    function GetFontFamily: TpgStringProp;
    function GetLetterSpacing: TpgHLengthProp;
    function GetOpacity: TpgFloatProp;
    function GetStrokeDashArray: TpgLengthListProp;
    function GetStrokeDashOffset: TpgLengthProp;
    function GetStrokeMiterLimit: TpgFloatProp;
    function GetStrokeOpacity: TpgFloatProp;
    function GetWordSpacing: TpgHLengthProp;
  protected
    function GetColorInfo: PpgColorInfo; virtual;
  public
    // Play the fill path into the TpgPath object APath
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); virtual;
    property Fill: TpgPaintProp read GetFill;
    property Stroke: TpgPaintProp read GetStroke;
    property StrokeWidth: TpgLengthProp read GetStrokeWidth;
    property FillRule: TpgFillRuleProp read GetFillRule;
    property FontSize: TpgVLengthProp read GetFontSize;

    property Opacity: TpgFloatProp read GetOpacity;
    property FillOpacity: TpgFloatProp read GetFillOpacity;
    property FontFamily: TpgStringProp read GetFontFamily;
//    property FontStretch: TpgFontStretchProp read GetFontStretch;
//    property FontStyle: TpgFontStyleProp read GetFontStyle;
//    property FontWeight: TpgFontWeightProp read GetFontWeight;
    property LetterSpacing: TpgHLengthProp read GetLetterSpacing;
    property WordSpacing: TpgHLengthProp read GetWordSpacing;
    property StrokeOpacity: TpgFloatProp read GetStrokeOpacity;
//    property StrokeLineCap: TpgLineCapProp read GetStrokeLineCap;
//    property StrokeLineJoin: TpgLineJoinProp read GetStrokeLineJoin;
    property StrokeMiterLimit: TpgFloatProp read GetStrokeMiterLimit;
    property StrokeDashArray: TpgLengthListProp read GetStrokeDashArray;
    property StrokeDashOffset: TpgLengthProp read GetStrokeDashOffset;
//    property TextAnchor: TpgTextAnchorProp read GetTextAnchor;

  end;

{ former pgGraphic.pas }

type

  TpgEditorOptionsProp = class(TpgIntProp)
  private
    function GetValue: TpgEditorOptions;
    procedure SetValue(const Value: TpgEditorOptions);
  public
    property Value: TpgEditorOptions read GetValue write SetValue;
  end;

  TpgTransformProp = class(TpgStoredProp)
  private
    FValue: TpgTransform;
    procedure SetValue(const Value: TpgTransform);
  protected
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
  public
    destructor Destroy; override;
    property Value: TpgTransform read FValue write SetValue;
  end;

  TpgGraphic = class(TpgPaintable)
  private
    function GetTransform: TpgTransformProp;
    function GetEditorOptions: TpgEditorOptionsProp;
  protected
  public
    property Transform: TpgTransformProp read GetTransform;
    property EditorOptions: TpgEditorOptionsProp read GetEditorOptions;
  end;

  // Group element class. TpgGroup has [efAllowElements] set, so this element can
  // hold a group of subelements.
  TpgGroup = class(TpgGraphic)
  protected
    constructor CreateInternal(AOwner: TpgDocument; AParent: TpgElement); override;
  end;

{ pgViewPort.pas }

  TpgViewBoxProp = class(TpgStoredProp)
  private
    FMinX: double;
    FMinY: double;
    FWidth: double;
    FHeight: double;
    procedure SetHeight(const Value: double);
    procedure SetMinX(const Value: double);
    procedure SetMinY(const Value: double);
    procedure SetWidth(const Value: double);
  protected
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
  public
    property MinX: double read FMinX write SetMinX;
    property MinY: double read FMinY write SetMinY;
    property Width: double read FWidth write SetWidth;
    property Height: double read FHeight write SetHeight;
  end;

  TpgPreserveAspectProp = class(TpgIntProp)
  private
    function GetValue: TpgPreserveAspect;
    procedure SetValue(const Value: TpgPreserveAspect);
  public
    property Value: TpgPreserveAspect read GetValue write SetValue;
  end;

  TpgMeetOrSliceProp = class(TpgIntProp)
  private
    function GetValue: TpgMeetOrSlice;
    procedure SetValue(const Value: TpgMeetOrSlice);
  published
    property Value: TpgMeetOrSlice read GetValue write SetValue;
  end;

  TpgBaseViewPort = class(TpgGroup)
  private
    function GetViewBox: TpgViewBoxProp;
    function GetMeetOrSlice: TpgMeetOrSliceProp;
    function GetPreserveAspect: TpgPreserveAspectProp;
  protected
    procedure GetViewBoxProps(var AMinX, AMinY, AWidth, AHeight: double); virtual;
  public
    property PreserveAspect: TpgPreserveAspectProp read GetPreserveAspect;
    property MeetOrSlice: TpgMeetOrSliceProp read GetMeetOrSlice;
    property ViewBox: TpgViewBoxProp read GetViewBox;
  end;

  TpgViewPort = class(TpgBaseViewPort)
  private
    function GetHeight: TpgVLengthProp;
    function GetWidth: TpgHLengthProp;
    function GetX: TpgHLengthProp;
    function GetY: TpgVLengthProp;
  public
    function BuildViewBoxTransform(const AInfo: TpgDeviceInfo): TpgTransform; virtual;
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
    property X: TpgHLengthProp read GetX;
    property Y: TpgVLengthProp read GetY;
    property Width: TpgHLengthProp read GetWidth;
    property Height: TpgVLengthProp read GetHeight;
  end;

implementation

{ TpgLengthProp }

procedure TpgLengthProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgLengthProp then
    FUnits := TpgLengthProp(AProp).FUnits;
end;

class function TpgLengthProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdUnknown;
end;

procedure TpgLengthProp.Read(AStorage: TpgStorage);
begin
  inherited;
  FUnits := TpgLengthUnits(AStorage.ReadInt);
end;

procedure TpgLengthProp.SetUnits(const Value: TpgLengthUnits);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FUnits <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgLengthProp(CallerProperty(Caller)).FUnits := Value;
    DoAfterChange(Caller);
  end;
end;

function TpgLengthProp.ToDevice(const AInfo: TpgDeviceInfo): double;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if Caller is TpgSizeable then
    Result := TpgSizeable(Caller).ResolveLength(GetValue, FUnits, GetDirection, AInfo)
  else
    raise Exception.Create('unable to resolve length through caller');
end;

procedure TpgLengthProp.Write(AStorage: TpgStorage);
begin
  inherited;
  AStorage.WriteInt(integer(FUnits));
end;

{ TpgHLengthProp }

class function TpgHLengthProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdHorizontal;
end;

{ TpgVLengthProp }

class function TpgVLengthProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdVertical;
end;

{ TpgFloatItem }

constructor TpgFloatItem.Create(AOwner: TpgFloatListProp);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TpgFloatItem.SetValue(const Value: double);
var
  ACaller: TpgElement;
  MustWrite: boolean;
begin
  if FValue <> Value then
  begin
    FOwner.GetCaller(ACaller, MustWrite);
    FOwner.DoBeforeChange(ACaller);
    FValue := Value;
    FOwner.DoAfterChange(ACaller);
  end;
end;

{ TpgFloatProtectList }

procedure TpgFloatProtectList.Add(AItem: TpgFloatItem);
begin
  raise Exception.Create(sIllegalUseDirectAdd);
end;

procedure TpgFloatProtectList.AddInternal(Value: double);
var
  Item: TpgFloatItem;
begin
  Item := TpgFloatItem.Create(FOwner);
  Item.FValue := Value;
  inherited Add(Item);
end;

procedure TpgFloatProtectList.CopyFrom(AList: TpgFLoatProtectList);
var
  i: integer;
begin
  Clear;
  for i := 0 to AList.Count - 1 do
    AddInternal(AList[i].Value);
end;

constructor TpgFloatProtectList.Create(AOwner: TpgFloatListProp);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TpgFloatProtectList.GetItems(Index: integer): TpgFloatItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TpgFloatListProp }

procedure TpgFloatListProp.Add(Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgFloatListProp;
begin
  GetCaller(Caller, MustWrite);
  DoBeforeChange(Caller);
  Prop := TpgFloatListProp(CallerProperty(Caller));
  Prop.Values.AddInternal(Value);
  DoAfterChange(Caller);
end;

procedure TpgFloatListProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgFloatListProp then
    FValues.CopyFrom(TpgFloatListProp(AProp).FValues);
end;

constructor TpgFloatListProp.CreateInternal(AId: longword);
begin
  inherited;
  FValues := TpgFloatProtectList.Create(Self);
end;

destructor TpgFloatListProp.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TpgFloatListProp.Read(AStorage: TpgStorage);
var
  i, Count: integer;
begin
  inherited;
  Count := AStorage.ReadInt;
  for i := 0 to Count - 1 do
    FValues.AddInternal(AStorage.ReadFloat);
end;

procedure TpgFloatListProp.SetValues(const Value: TpgFloatProtectList);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgFloatListProp;
begin
  GetCaller(Caller, MustWrite);
  if (FValues <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    Prop := TpgFloatListProp(CallerProperty(Caller));
    FreeAndNil(Prop.FValues);
    Prop.FValues := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgFloatListProp.Write(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  AStorage.WriteInt(FValues.Count);
  for i := 0 to FValues.Count - 1 do
    AStorage.WriteFloat(FValues[i].Value);
end;

{ TpgLengthItem }

constructor TpgLengthItem.Create(AOwner: TpgLengthListProp);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TpgLengthItem.SetUnits(const Value: TpgLengthUnits);
var
  ACaller: TpgElement;
  MustWrite: boolean;
begin
  if FUnits <> Value then
  begin
    FOwner.GetCaller(ACaller, MustWrite);
    FOwner.DoBeforeChange(ACaller);
    FUnits := Value;
    FOwner.DoAfterChange(ACaller);
  end;
end;

procedure TpgLengthItem.SetValue(const Value: double);
var
  ACaller: TpgElement;
  MustWrite: boolean;
begin
  if FValue <> Value then
  begin
    FOwner.GetCaller(ACaller, MustWrite);
    FOwner.DoBeforeChange(ACaller);
    FValue := Value;
    FOwner.DoAfterChange(ACaller);
  end;
end;

function TpgLengthItem.ToDevice(const AInfo: TpgDeviceInfo): double;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  FOwner.GetCaller(Caller, MustWrite);
  if Caller is TpgSizeable then
    Result := TpgSizeable(Caller).ResolveLength(FValue, FUnits, FOwner.GetDirection, AInfo)
  else
    raise Exception.Create('unable to resolve length through caller');
end;

{ TpgLengthProtectList }

procedure TpgLengthProtectList.Add(AItem: TpgLengthItem);
begin
  raise Exception.Create(sIllegalUseDirectAdd);
end;

procedure TpgLengthProtectList.AddInternal(Value: double; Units: TpgLengthUnits);
var
  Item: TpgLengthItem;
begin
  Item := TpgLengthItem.Create(FOwner);
  Item.FValue := Value;
  Item.FUnits := Units;
  inherited Add(Item);
end;

procedure TpgLengthProtectList.CopyFrom(AList: TpgLengthProtectList);
var
  i: integer;
begin
  Clear;
  for i := 0 to AList.Count - 1 do
    AddInternal(AList[i].Value, AList[i].Units);
end;

constructor TpgLengthProtectList.Create(AOwner: TpgLengthListProp);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TpgLengthProtectList.GetItems(Index: integer): TpgLengthItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TpgLengthListProp }

procedure TpgLengthListProp.Add(Value: double; Units: TpgLengthUnits);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgLengthListProp;
begin
  GetCaller(Caller, MustWrite);
  DoBeforeChange(Caller);
  Prop := TpgLengthListProp(CallerProperty(Caller));
  Prop.Values.AddInternal(Value, Units);
  DoAfterChange(Caller);
end;

procedure TpgLengthListProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgLengthListProp then
    FValues.CopyFrom(TpgLengthListProp(AProp).FValues);
end;

constructor TpgLengthListProp.CreateInternal(AId: longword);
begin
  inherited;
  FValues := TpgLengthProtectList.Create(Self);
end;

destructor TpgLengthListProp.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

class function TpgLengthListProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdUnknown;
end;

procedure TpgLengthListProp.Read(AStorage: TpgStorage);
var
  i, Count: integer;
begin
  inherited;
  Count := AStorage.ReadInt;
  for i := 0 to Count - 1 do
    FValues.AddInternal(
      AStorage.ReadFloat,
      TpgLengthUnits(AStorage.ReadInt));
end;

procedure TpgLengthListProp.SetValues(const Value: TpgLengthProtectList);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgLengthListProp;
begin
  GetCaller(Caller, MustWrite);
  if (FValues <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    Prop := TpgLengthListProp(CallerProperty(Caller));
    FreeAndNil(Prop.FValues);
    Prop.FValues := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgLengthListProp.Write(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  AStorage.WriteInt(FValues.Count);
  for i := 0 to FValues.Count - 1 do
  begin
    AStorage.WriteFloat(FValues[i].Value);
    AStorage.WriteInt(integer(FValues[i].Units));
  end;
end;

{ TpgHLengthListProp }

class function TpgHLengthListProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdHorizontal;
end;

{ TpgVLengthListProp }

class function TpgVLengthListProp.GetDirection: TpgCartesianDirection;
begin
  Result := cdVertical;
end;

{ TpgSizeable }

function TpgSizeable.GetFontHeight(const AInfo: TpgDeviceInfo): double;
begin
  Result := TpgVLengthProp(PropById(piFontSize)).ToDevice(AInfo);
end;

procedure TpgSizeable.GetViewPortSize(var Width, Height: double; const AInfo: TpgDeviceInfo);
var
  P: TpgElement;
  VB: TpgViewBoxProp;
begin
  // In case *we* are the viewport.. we start the chain up here
  P := Self;
  repeat
    if P is TpgViewPort then
      break;
    P := P.Parent;
  until not assigned(P);

  if not assigned(P) then
    raise Exception.Create(sCannotResolveViewPortUnits);

  if TpgViewPort(P).ViewBox.ExistsLocal then
  begin
    VB := TpgViewPort(P).ViewBox;
    Width := VB.Width;
    Height := VB.Height;
  end else
  begin
    Width := TpgViewPort(P).Width.ToDevice(AInfo);
    Height := TpgViewPort(P).Height.ToDevice(AInfo);
  end;
end;

function TpgSizeable.ResolveLength(const AValue: double; AUnits: TpgLengthUnits;
  ADirection: TpgCartesianDirection; const AInfo: TpgDeviceInfo): double;
var
  Width, Height: double;
  // local
  function GetDPI: double;
  begin
    Result := 0;
    case ADirection of
    cdUnknown,
    cdHorizontal: Result := AInfo.DPI.X;
    cdVertical:   Result := AInfo.DPI.Y;
    end;
  end;
// main
begin
  case AUnits of
  luNone: Result := AValue;
  luPerc:
    begin
      GetViewPortSize(Width, Height, AInfo);
      case ADirection of
      cdHorizontal: Result := AValue * 0.01 * Width;
      cdVertical: Result := AValue * 0.01 * Height;
      cdUnknown: Result := AValue * 0.01 * sqrt(Width * Height);
      else
        raise Exception.Create(sInvalidUnitSpec);
      end;
    end;
  luEms: Result := AValue * GetFontHeight(AInfo);//GetFontEmHeight?;
  luExs: Result := AValue * GetFontHeight(AInfo);//GetFontExHeight?;
  luCm: Result := AValue * GetDPI / 2.54;
  luMm: Result := AValue * GetDPI / 25.4;
  luIn: Result := AValue * GetDPI;
  luPt: Result := AValue * GetDPI / 72;
  luPc: Result := AValue * GetDPI / 6;
  else
    raise Exception.Create(sInvalidUnitSpec);
  end;
end;

{ TpgFillRuleProp }

function TpgFillRuleProp.GetValue: TpgFillRule;
begin
  Result := TpgFillRule(inherited GetValue);
end;

procedure TpgFillRuleProp.SetValue(const Value: TpgFillRule);
begin
  inherited SetValue(integer(Value));
end;

{ TpgPaintProp }

procedure TpgPaintProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgPaintProp then
  begin
    FColor := pgConvertColor(
      TpgPaintProp(AProp).GetColorInfo^,
      GetColorInfo^,
      @TpgPaintProp(AProp).FColor);
    FPaintStyle := TpgPaintProp(AProp).FPaintStyle;
  end;
end;

function TpgPaintProp.GetAsColor32: TpgColor32;
begin
  Result := pgColorTo4Ch8b(GetColorInfo^, cARGB_8b_Org, @FColor);
end;

function TpgPaintProp.GetColorInfo: PpgColorInfo;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if Caller is TpgPaintable then
  begin
    Result := TpgPaintable(Caller).GetColorInfo;
  end else
    // If no info, we use ARGB 8bpc Org
    Result := @cARGB_8b_Org;
end;

procedure TpgPaintProp.Read(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  for i := 0 to 7 do
    FColor[i] := AStorage.ReadInt;
  FPaintStyle := TpgPaintStyle(AStorage.ReadInt);
end;

procedure TpgPaintProp.SetAsColor32(const Value: TpgColor32);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgPaintProp;
begin
  GetCaller(Caller, MustWrite);
  if (GetAsColor32 <> Value) or (FPaintStyle <> psColor) or MustWrite then
  begin
    DoBeforeChange(Caller);
    Prop := TpgPaintProp(CallerProperty(Caller));
    Prop.FColor :=
      pgConvertColor(cARGB_8b_Org, GetColorInfo^, @Value);
    Prop.FPaintStyle := psColor;
    Prop.Reference := nil;
    DoAfterChange(Caller);
  end;
end;

procedure TpgPaintProp.SetPaintStyle(const Value: TpgPaintStyle);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FPaintStyle <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgPaintProp(CallerProperty(Caller)).FPaintStyle := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgPaintProp.Write(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  // should be changed to binary writing
  for i := 0 to 7 do
    AStorage.WriteInt(FColor[i]);
  AStorage.WriteInt(integer(FPaintStyle));
end;

{ TpgPaintable }

function TpgPaintable.GetColorInfo: PpgColorInfo;
begin
  // For now we return this until there is a colorspace property
  Result :=  @cARGB_8b_Org;
end;

function TpgPaintable.GetFill: TpgPaintProp;
begin
  Result := TpgPaintProp(PropById(piFill));
end;

function TpgPaintable.GetFillOpacity: TpgFloatProp;
begin
  Result := TpgFloatProp(PropById(piFillOpacity));
end;

function TpgPaintable.GetFillRule: TpgFillRuleProp;
begin
  Result := TpgFillRuleProp(PropById(piFillRule));
end;

function TpgPaintable.GetFontFamily: TpgStringProp;
begin
  Result := TpgStringProp(PropById(piFontFamily));
end;

function TpgPaintable.GetFontSize: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piFontSize));
end;

function TpgPaintable.GetLetterSpacing: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piLetterSpacing));
end;

function TpgPaintable.GetOpacity: TpgFloatProp;
begin
  Result := TpgFloatProp(PropById(piOpacity));
end;

function TpgPaintable.GetStroke: TpgPaintProp;
begin
  Result := TpgPaintProp(PropById(piStroke));
end;

function TpgPaintable.GetStrokeDashArray: TpgLengthListProp;
begin
  Result := TpgLengthListProp(PropById(piStrokeDashArray));
end;

function TpgPaintable.GetStrokeDashOffset: TpgLengthProp;
begin
  Result := TpgLengthProp(PropById(piStrokeDashOffset));
end;

function TpgPaintable.GetStrokeMiterLimit: TpgFloatProp;
begin
  Result := TpgFloatProp(PropById(piStrokeMiterLimit));
end;

function TpgPaintable.GetStrokeOpacity: TpgFloatProp;
begin
  Result := TpgFloatProp(PropById(piStrokeOpacity));
end;

function TpgPaintable.GetStrokeWidth: TpgLengthProp;
begin
  Result := TpgLengthProp(PropById(piStrokeWidth));
end;

function TpgPaintable.GetWordSpacing: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piWordSpacing));
end;

procedure TpgPaintable.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
begin
// default does nothing
end;

{ TpgEditorOptionsProp }

function TpgEditorOptionsProp.GetValue: TpgEditorOptions;
var
  ISet: TIntegerSet;
  e: TpgEditorOption;
begin
  Result := [];
  ISet := TIntegerSet(inherited GetValue);
  for e := low(TpgEditorOption) to High(TpgEditorOption) do
    if integer(e) in ISet then
      Include(Result, e);
end;

procedure TpgEditorOptionsProp.SetValue(const Value: TpgEditorOptions);
var
  ISet: TIntegerSet;
  e: TpgEditorOption;
begin
  ISet := [];
  for e := low(TpgEditorOption) to High(TpgEditorOption) do
    if e in Value then
      Include(ISet, integer(e));
  inherited SetValue(integer(ISet));
end;

{ TpgTransformProp }

procedure TpgTransformProp.CopyFrom(AProp: TpgProp);
begin
  if AProp is TpgTransformProp then begin
    FreeAndNil(FValue);
    if assigned(TpgTransformProp(AProp).Value) then begin
      FValue := TpgTransformClass(TpgTransformProp(AProp).Value.ClassType).Create;
      FValue.Assign(TpgTransformProp(AProp).Value);
    end;
  end;
end;

destructor TpgTransformProp.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TpgTransformProp.Read(AStorage: TpgStorage);
var
  Id: longword;
  Info: TpgTransformInfo;
begin
  inherited;
  FreeAndNil(FValue);
  // Read type
  Id := AStorage.ReadInt;
  Info := GetTransformInfoById(Id);
  if not assigned(Info) then exit;
  FValue := Info.TransformClass.Create;
  FValue.Read(AStorage);
end;

procedure TpgTransformProp.SetValue(const Value: TpgTransform);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgTransformProp;
begin
  GetCaller(Caller, MustWrite);
  if (FValue <> Value) or MustWrite then begin
    DoBeforeChange(Caller);
    Prop := TpgTransformProp(CallerProperty(Caller));
    FreeAndNil(Prop.FValue);
    Prop.FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgTransformProp.Write(AStorage: TpgStorage);
// more types to follow
var
  Info: TpgTransformInfo;
begin
  inherited;
  if not assigned(FValue) then begin
    AStorage.WriteInt(0);
    exit;
  end;
  Info := GetTransformInfoByClass(TpgTransformClass(FValue.ClassType));
  if not assigned(Info) then
    raise Exception.Create(sUnregisteredTransform);
  // Write id
  AStorage.WriteInt(Info.Id);
  // Let transform write itself
  FValue.Write(AStorage);
end;

{ TpgGraphic }

function TpgGraphic.GetEditorOptions: TpgEditorOptionsProp;
begin
  Result := TpgEditorOptionsProp(PropById(piEditorOptions));
end;

function TpgGraphic.GetTransform: TpgTransformProp;
begin
  Result := TpgTransformProp(PropById(piTransform));
end;

{ TpgGroup }

constructor TpgGroup.CreateInternal(AOwner: TpgDocument; AParent: TpgElement);
begin
  inherited CreateInternal(AOwner, AParent);
  Flags := Flags + [efAllowElements];
end;

{ TpgBaseViewPort}

function TpgBaseViewPort.GetMeetOrSlice: TpgMeetOrSliceProp;
begin
  Result := TpgMeetOrSliceProp(PropById(piMeetOrSlice));
end;

function TpgBaseViewPort.GetPreserveAspect: TpgPreserveAspectProp;
begin
  Result := TpgPreserveAspectProp(PropById(piPreserveAspect));
end;

function TpgBaseViewPort.GetViewBox: TpgViewBoxProp;
begin
  Result := TpgViewBoxProp(PropById(piViewBox));
end;

procedure TpgBaseViewPort.GetViewBoxProps(var AMinX, AMinY, AWidth, AHeight: double);
begin
  if ViewBox.ExistsLocal then
  begin
    AMinX := ViewBox.MinX;
    AMinY := ViewBox.MinY;
    AWidth := ViewBox.Width;
    AHeight := ViewBox.Height;
  end else
  begin
    AMinX := 0;
    AMinY := 0;
    AWidth := 0;
    AHeight := 0;
  end;
end;

{ TpgViewPort }

function TpgViewPort.GetHeight: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piVPHeight));
end;

function TpgViewPort.BuildViewBoxTransform(const AInfo: TpgDeviceInfo): TpgTransform;
var
  PA: TpgPreserveAspect;
  VBMinX, VBMinY, VBWidth, VBHeight: double;
begin
  // Make sure to use PreserveAspect = XMidYMid if no value available
  if PreserveAspect.ExistsLocal then
    PA := PreserveAspect.Value
  else
    PA := paXMidYMid;

  // Viewbox props
  GetViewBoxProps(VBMinX, VBMinY, VBWidth, VBHeight);
  
  // Build viewbox transform
  Result := pgTransform.BuildViewBoxTransform(
    X.ToDevice(AInfo), Y.ToDevice(AInfo),
    Width.ToDevice(AInfo), Height.ToDevice(AInfo),
    VBMinX, VBMinY, VBWidth, VBHeight,
    PA,
    MeetOrSlice.Value);
end;

function TpgViewPort.GetWidth: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piVPWidth));
end;

function TpgViewPort.GetX: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piVPX));
end;

function TpgViewPort.GetY: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piVPY));
end;

procedure TpgViewPort.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  VBMinX, VBMinY, VBWidth, VBHeight: double;
begin
  GetViewBoxProps(VBMinX, VBMinY, VBWidth, VBHeight);
  if (VBWidth > 0) and (VBHeight > 0) then
    APath.Rectangle(-VBMinX, -VBMinY, VBWidth, VBHeight, 0, 0)
  else
    APath.Rectangle(0, 0, Width.ToDevice(AInfo), Height.ToDevice(AInfo), 0, 0);
end;

{ TpgViewBoxProp }

procedure TpgViewBoxProp.Read(AStorage: TpgStorage);
begin
  inherited;
  FMinX := AStorage.ReadFloat;
  FMinY := AStorage.ReadFloat;
  FWidth := AStorage.ReadFloat;
  FHeight := AStorage.ReadFloat;
end;

procedure TpgViewBoxProp.SetHeight(const Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FHeight <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgViewBoxProp(CallerProperty(Caller)).FHeight := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgViewBoxProp.SetMinX(const Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FMinX <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgViewBoxProp(CallerProperty(Caller)).FMinX := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgViewBoxProp.SetMinY(const Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FMinY <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgViewBoxProp(CallerProperty(Caller)).FMinY := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgViewBoxProp.SetWidth(const Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if (FWidth <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgViewBoxProp(CallerProperty(Caller)).FWidth := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgViewBoxProp.Write(AStorage: TpgStorage);
begin
  inherited;
  AStorage.WriteFloat(FMinX);
  AStorage.WriteFloat(FMinY);
  AStorage.WriteFloat(FWidth);
  AStorage.WriteFloat(FHeight);
end;

{ TpgPreserveAspectProp }

function TpgPreserveAspectProp.GetValue: TpgPreserveAspect;
begin
  Result := TpgPreserveAspect(inherited GetValue);
end;

procedure TpgPreserveAspectProp.SetValue(const Value: TpgPreserveAspect);
begin
  inherited SetValue(integer(Value));
end;

{ TpgMeetOrSliceProp }

function TpgMeetOrSliceProp.GetValue: TpgMeetOrSlice;
begin
  Result := TpgMeetOrSlice(inherited GetValue);
end;

procedure TpgMeetOrSliceProp.SetValue(const Value: TpgMeetOrSlice);
begin
  inherited SetValue(integer(Value));
end;

initialization

  // paintable
  RegisterProp(piFill, TpgPaintProp, 'Fill', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piFillRule, TpgFillRuleProp, 'FillRule', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piStroke, TpgPaintProp, 'Stroke', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piStrokeWidth, TpgLengthProp, 'StrokeWidth', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piFontSize, TpgVLengthProp, 'FontSize', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piOpacity, TpgFloatProp, 'Opacity', TpgPaintable, [pfStored, pfInherit], FloatToStr(1.0));
  RegisterProp(piFillOpacity, TpgFloatProp, 'FillOpacity', TpgPaintable, [pfStored, pfInherit], FloatToStr(1.0));
  RegisterProp(piFontFamily, TpgStringProp, 'FontFamily', TpgPaintable, [pfStored, pfInherit]);
//    property FontStretch: TpgFontStretchProp read GetFontStretch;
//    property FontStyle: TpgFontStyleProp read GetFontStyle;
//    property FontWeight: TpgFontWeightProp read GetFontWeight;
  RegisterProp(piLetterSpacing, TpgHLengthProp, 'LetterSpacing', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piWordSpacing, TpgHLengthProp, 'WordSpacing', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piStrokeOpacity, TpgFloatProp, 'StrokeOpacity', TpgPaintable, [pfStored, pfInherit], FloatToStr(1.0));
//    property StrokeLineCap: TpgLineCapProp read GetStrokeLineCap;
//    property StrokeLineJoin: TpgLineJoinProp read GetStrokeLineJoin;
  RegisterProp(piStrokeMiterLimit, TpgFloatProp, 'StrokeMiterLimit', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piStrokeDashArray, TpgLengthListProp, 'StrokeDashArray', TpgPaintable, [pfStored, pfInherit]);
  RegisterProp(piStrokeDashOffset, TpgLengthProp, 'StrokeDashOffset', TpgPaintable, [pfStored, pfInherit]);
//    property TextAnchor: TpgTextAnchorProp read GetTextAnchor;}

  // group
  RegisterProp(piTransform, TpgTransformProp, 'Transform', TpgGraphic, [pfStored]);
  RegisterProp(piEditorOptions, TpgEditorOptionsProp, 'EditorOptions', TpgGraphic, [pfStored]);

  RegisterElement(eiGroup, TpgGroup, 'Group');

  // viewport
  RegisterElement(eiViewPort, TpgViewPort, 'ViewPort');
  RegisterProp(piPreserveAspect, TpgPreserveAspectProp, 'PreserveAspect', TpgBaseViewPort, [pfStored]);
  RegisterProp(piMeetOrSlice, TpgMeetOrSliceProp, 'MeetOrSlice', TpgBaseViewPort, [pfStored]);
  RegisterProp(piVPX, TpgHLengthProp, 'X', TpgViewPort, [pfStored]);
  RegisterProp(piVPY, TpgVLengthProp, 'Y', TpgViewPort, [pfStored]);
  RegisterProp(piVPWidth, TpgHLengthProp, 'Width', TpgViewPort, [pfStored]);
  RegisterProp(piVPHeight, TpgVLengthProp, 'Height', TpgViewPort, [pfStored]);
  RegisterProp(piViewBox, TpgViewBoxProp, 'ViewBox', TpgBaseViewPort, [pfStored]);

end.
