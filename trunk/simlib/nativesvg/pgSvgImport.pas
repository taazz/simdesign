{ Project: Pyro
  Module: Pyro SVG

  Description:
  Import of SVG (Scalable Vector Graphics) into the Pyro document object model.
  Pyro's scene definition and object model is already SVG oriented.

  Creation Date:
  09dec2006

  Modified:
  14apr2011: placed private FParser in class TpgSvgImport
  19may2011: string > Utf8String

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgSvgImport;

interface

uses
  Classes, SysUtils,

  // NativeXml component
  NativeXml, NativeXmlC14n,

  // Simdesign general units
  sdSortedLists, sdDebug,

  // Pyro component
  pgScene, pgDocument, pgViewPort, pgParser, 
  pgShape, pgCommandPath, pgImage, Pyro, pgSVGNamedColors;

type

  TSvgAttributeType = (
    atEnum,
    atLength,
    atLengthList,
    atFloatList,
    atString,
    atViewBox,
    atAspect,
    atTransform,
    atPaint,
    atPath,
    atImage,
    atFloat,
    atStyle
  );

  TSvgTransformCommand = (
    tcNone,
    tcMatrix,
    tcTranslate,
    tcScale,
    tcRotate,
    tcSkewX,
    tcSkewY
  );

  TSvgNamedItem = class
  private
    FData: pointer;
    FName: Utf8String;
  public
    property Name: Utf8String read FName write FName;
    property Data: pointer read FData write FData;
  end;

  TSvgNamedList = class(TSortedList)
  private
    function GetItems(Index: integer): TSvgNamedItem;
  public
    property Items[Index: integer]: TSvgNamedItem read GetItems; default;
  end;

  // TpgSvgImport class: use method ImportScene to import an SVG graphic from
  // AStream into the AScene parameter
  TpgSvgImport = class(TsdDebugComponent)
  private
    FScene: TpgScene;
    FParser: TpgParser;
    FElementTypes: TSvgNamedList;
    FAttributeTypes: TSvgNamedList;
    function ElementCompare(Item1, Item2: TObject; Info: pointer): integer;
    function AttributeCompare(Item1, Item2: TObject; Info: pointer): integer;
  protected
    procedure PrepareLists;
    function ParseAttribute(const AName: Utf8String; ASrcPos: int64; AElement: TpgElement;
      var AttributeType: TSvgAttributeType): TpgPropInfo;
    procedure ParseElement(AElement: TpgElement; ANode: TXmlNode);
    procedure ParseProp(AElement: TpgElement; AttrType: TSvgAttributeType; APropInfo: TpgPropInfo; const AValue: Utf8String);
    procedure ParsePropLength(AProp: TpgLengthProp; const AValue: Utf8String);
    procedure ParsePropLengthList(AProp: TpgLengthListProp; const AValue: Utf8String);
    procedure ParsePropPointList(AProp: TpgFloatListProp; const AValue: Utf8String);
    procedure ParsePropTransform(AProp: TpgTransformProp; const AValue: Utf8String);
    procedure ParsePropTransformParams(const ACommand, AParams: Utf8String;
      var ACommandId: TSvgTransformCommand; var ANumbers: array of double);
    procedure ParsePropViewBox(AProp: TpgViewBoxProp; const AValue: Utf8String);
    procedure ParsePropPaint(AProp: TpgPaintProp; const AValue: Utf8String);
    procedure ParsePropPath(AProp: TpgPathProp; const AValue: Utf8String);
    procedure ParsePropImage(AProp: TpgImageProp; const AValue: Utf8String);
    procedure ParseAspect(AElement: TpgElement; AValue: Utf8String);
    procedure ParseStyle(AElement: TpgElement; AValue: Utf8String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImportScene(AScene: TpgScene; AStream: TStream);

    // Parse an SVG style length specifier, return true if successful. Result is put
    // in Units and Size.
    function pgSvgParseLength(const Value: Utf8String; var Units: TpgLengthUnits; var Size: double): boolean;

    // Parse an SVG style path description
    function pgSvgParseCommandPath(const Value: Utf8String; CommandPath: TpgCommandPath): boolean;

    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

resourcestring

  sSVGRootExpected = 'SVG root node expected';

implementation

uses
  pgPaintServer,
  pgColor, pgTransform, pgText;

type

  TCommandPathAccess = class(TpgCommandPath);
  TElementAccess = class(TpgElement);

  TSvgElementRec = record
    Name: Utf8String;
    ElementClass: TpgElementClass;
  end;
  PSvgElementRec = ^TSvgElementRec;

  TSvgAttributeRec = record
    Name: Utf8String;            // Name of the attribute
    PropId: integer;         // Pyro property Id
    AType: TSvgAttributeType;
  end;
  PSvgAttributeRec = ^TSvgAttributeRec;

const

  // Table with element names in SVG and matching element classes in Pyro
  cSvgElementTypeCount = 12;
  cSvgElementTypes: array[0..cSvgElementTypeCount - 1] of TSvgElementRec =
    (
      (Name: 'svg';      ElementClass: TpgViewPort),
      (Name: 'g';        ElementClass: TpgGroup),
      (Name: 'rect';     ElementClass: TpgRectangle),
      (Name: 'circle';   ElementClass: TpgCircle),
      (Name: 'ellipse';  ElementClass: TpgEllipse),
      (Name: 'line';     ElementClass: TpgLine),
      (Name: 'polyline'; ElementClass: TpgPolyLineShape),
      (Name: 'polygon';  ElementClass: TpgPolygonShape),
      (Name: 'path';     ElementClass: TpgPathShape),
      (Name: 'text';     ElementClass: TpgText),
      (Name: 'tspan';    ElementClass: TpgTextSpan),
      (Name: 'image';    ElementClass: TpgImageView)
    );

  cSvgAttributeTypeCount = 48;
  cSvgAttributeTypes: array[0..cSvgAttributeTypeCount - 1] of TSvgAttributeRec =
    (
      // styleable
     (Name: 'id';                  PropId: piName;             AType: atString),
     (Name: 'style';               PropId: piStyle;            AType: atStyle),

      // group
     (Name: 'transform';           PropId: piTransform;        AType: atTransform),

      // viewport
     (Name: 'x';                   PropId: piVPX;              AType: atLength),
     (Name: 'y';                   PropId: piVPY;              AType: atLength),
     (Name: 'width';               PropId: piVPWidth;          AType: atLength),
     (Name: 'height';              PropId: piVPHeight;         AType: atLength),
     (Name: 'viewBox';             PropId: piViewBox;          AType: atViewBox),
     (Name: 'preserveAspectRatio'; PropId: piPreserveAspect;   AType: atAspect),

      // paintable
     (Name: 'fill';                PropId: piFill;             AType: atPaint),
     (Name: 'fill-rule';           PropId: piFillRule;         AType: atEnum),
     (Name: 'fill-opacity';        PropId: piFillOpacity;      AType: atFloat),
     (Name: 'stroke';              PropId: piStroke;           AType: atPaint),
     (Name: 'stroke-width';        PropId: piStrokeWidth;      AType: atLength),
     (Name: 'stroke-opacity';      PropId: piStrokeOpacity;    AType: atFloat),
     (Name: 'stroke-miterlimit';   PropId: piStrokeMiterLimit; AType: atFloat),
     (Name: 'stroke-dasharray';    PropId: piStrokeDashArray;  AType: atLengthList),
     (Name: 'stroke-dashoffset';   PropId: piStrokeDashOffset; AType: atLength),
     (Name: 'opacity';             PropId: piOpacity;          AType: atFloat),
     (Name: 'font-size';           PropId: piFontSize;         AType: atLength),
     (Name: 'font-family';         PropId: piFontFamily;       AType: atString),
     (Name: 'letter-spacing';      PropId: piLetterSpacing;    AType: atLengthList),
     (Name: 'word-spacing';        PropId: piWordSpacing;      AType: atLengthList),

      // rect
     (Name: 'x';                   PropId: piRectX;            AType: atLength),
     (Name: 'y';                   PropId: piRectY;            AType: atLength),
     (Name: 'rx';                  PropId: piRectRx;           AType: atLength),
     (Name: 'ry';                  PropId: piRectRy;           AType: atLength),
     (Name: 'width';               PropId: piRectWidth;        AType: atLength),
     (Name: 'height';              PropId: piRectHeight;       AType: atLength),

     // circle
     (Name: 'cx';                  PropId: piCircleCx;         AType: atLength),
     (Name: 'cy';                  PropId: piCircleCy;         AType: atLength),
     (Name: 'r';                   PropId: piCircleR;          AType: atLength),

     // ellipse
     (Name: 'cx';                  PropId: piEllipseCx;        AType: atLength),
     (Name: 'cy';                  PropId: piEllipseCy;        AType: atLength),
     (Name: 'rx';                  PropId: piEllipseRx;        AType: atLength),
     (Name: 'ry';                  PropId: piEllipseRy;        AType: atLength),

     // line
     (Name: 'x1';                  PropId: piLineX1;           AType: atLength),
     (Name: 'y1';                  PropId: piLineY1;           AType: atLength),
     (Name: 'x2';                  PropId: piLineX2;           AType: atLength),
     (Name: 'y2';                  PropId: piLineY2;           AType: atLength),

     // polyline, polygon
     (Name: 'points';              PropId: piPoints;            AType: atFloatList),

     // path
     (Name: 'd';                   PropId: piPath;              AType: atPath),

     // text
     (Name: 'x';                   PropId: piTxtX;              AType: atLengthList),
     (Name: 'y';                   PropId: piTxtY;              AType: atLengthList),
     (Name: 'dx';                  PropId: piTxtDx;             AType: atLengthList),
     (Name: 'dy';                  PropId: piTxtDy;             AType: atLengthList),
     (Name: 'chardata';            PropId: piText;              AType: atString),

     // image
     (Name: 'xlink:href';          PropId: piImage;             AType: atImage)
    );
{//    property FontStretch: TpgFontStretchProp read GetFontStretch;
//    property FontStyle: TpgFontStyleProp read GetFontStyle;
//    property FontWeight: TpgFontWeightProp read GetFontWeight;
//    property StrokeLineCap: TpgLineCapProp read GetStrokeLineCap;
//    property StrokeLineJoin: TpgLineJoinProp read GetStrokeLineJoin;}

type

  TLengthUnitsInfo = record
    Name: Utf8String;
    Units: TpgLengthUnits;
  end;

const

  cSvgLengthUnitsCount = 10;
  cSvgLengthUnitsInfo: array[0..cSvgLengthUnitsCount - 1] of TLengthUnitsInfo =
    ((Name: ''; Units: luNone),
     (Name: 'px'; Units: luNone),
     (Name: 'cm'; Units: luCm),
     (Name: 'mm'; Units: luMm),
     (Name: 'in'; Units: luIn),
     (Name: '%'; Units: luPerc),
     (Name: 'em'; Units: luEms),
     (Name: 'ex'; Units: luExs),
     (Name: 'pt'; Units: luPt),
     (Name: 'pc'; Units: luPc));

type

  TTransformInfo = record
    Name: Utf8String;
    Cmd: TSvgTransformCommand;
  end;

const

  cSvgTransformInfoCount = 6;
  cSvgTransformInfo: array[0..cSvgTransformInfoCount - 1] of TTransformInfo =
   ((Name: 'matrix'; Cmd: tcMatrix),
    (Name: 'translate'; Cmd: tcTranslate),
    (Name: 'scale'; Cmd: tcScale),
    (Name: 'rotate'; Cmd: tcRotate),
    (Name: 'skewX'; Cmd: tcSkewX),
    (Name: 'skewY'; Cmd: tcSkewY));

type

  TAspectInfo = record
    Name: Utf8String;
    Value: TpgPreserveAspect;
  end;

const

  cSvgAspectInfoCount = 10;
  cSvgAspectInfo: array[0..cSvgAspectInfoCount - 1] of TAspectInfo =
   ( (Name: 'none'; Value: paNone),
     (Name: 'xMinYMin'; Value: paXMinYMin),
     (Name: 'xMinYMid'; Value: paXMinYMid),
     (Name: 'xMinYMax'; Value: paXMinYMax),
     (Name: 'xMidYMin'; Value: paXMidYMin),
     (Name: 'xMidYMid'; Value: paXMidYMid),
     (Name: 'xMidYMax'; Value: paXMidYMax),
     (Name: 'xMaxYMin'; Value: paXMaxYMin),
     (Name: 'xMaxYMid'; Value: paXMaxYMid),
     (Name: 'xMaxYMax'; Value: paXMaxYMax));

const

  cCommandChar: array[TpgPathCommandStyle] of char =
    (' ', 'Z', 'M', 'm', 'L', 'l', 'H', 'h', 'V', 'v', 'C', 'c', 'S', 's',
     'Q', 'q', 'T', 't', 'A', 'a');

  cCommandCount: array[TpgPathCommandStyle] of word =
    (0, 0, 2, 2, 2, 2, 1, 1, 1, 1, 6, 6, 4, 4,
     4, 4, 2, 2, 7, 7);

function TpgSvgImport.pgSvgParseLength(const Value: Utf8String; var Units: TpgLengthUnits; var Size: double): boolean;
var
  i, Pos: integer;
  UnitStr: Utf8String;
begin
  Result := True;
  Pos := 1;
  Size := FParser.pgParseNumber(Value, Pos);
  UnitStr := lowercase(trim(copy(Value, Pos, length(Value))));
  for i := 0 to cSvgLengthUnitsCount - 1 do
    if cSvgLengthUnitsInfo[i].Name = UnitStr then
    begin
      Units := cSvgLengthUnitsInfo[i].Units;
      exit;
    end;
  // still here?
  Result := False;
end;

function pgSvgGetUriFragment(ARef: Utf8String): Utf8String;
begin
 Result := '';
 if length(ARef) = 0 then
   exit;
 if ARef[1] = '#' then
   Result := copy(ARef, 2, length(ARef));
end;

function TpgSvgImport.pgSvgParseCommandPath(const Value: Utf8String; CommandPath: TpgCommandPath): boolean;
var
  Pos, Count: integer;
  Command: TpgPathCommandStyle;
  Numbers: array[0..6] of double;

  // local
  procedure PerformCommand;
  begin
    TCommandPathAccess(CommandPath).AddCommand(Command);
    TCommandPathAccess(CommandPath).AddValues(Count, Numbers);
    Count := 0;
  end;

// main
begin
  Result := True;
  CommandPath.Clear;
  Pos := 1;
  Count := 0;
  Command := pcUnknown;
  while Pos <= length(Value) do
  begin
    case Value[Pos] of
    'Z',
    'z': Command := pcClosePath;
    'M': Command := pcMoveToAbs;
    'm': Command := pcMoveToRel;
    'L': Command := pcLineToAbs;
    'l': Command := pcLineToRel;
    'H': Command := pcLineToHorAbs;
    'h': Command := pcLineToHorRel;
    'V': Command := pcLineToVerAbs;
    'v': Command := pcLineToVerRel;
    'C': Command := pcCurveToCubicAbs;
    'c': Command := pcCurveToCubicRel;
    'S': Command := pcCurveToCubicSmoothAbs;
    's': Command := pcCurveToCubicSmoothRel;
    'Q': Command := pcCurveToQuadraticAbs;
    'q': Command := pcCurveToQuadraticRel;
    'T': Command := pcCurveToQuadraticSmoothAbs;
    't': Command := pcCurveToQuadraticSmoothRel;
    'A': Command := pcArcToAbs;
    'a': Command := pcArcToRel;
    '0'..'9', '+', '-', '.':
      begin
        if cCommandCount[Command] = 0 then
        begin
          Result := False;
          exit;
        end;
        if cCommandCount[Command] = Count then
          PerformCommand;
        Numbers[Count] := FParser.pgParseNumber(Value, Pos);
        dec(Pos);
        inc(Count);
        if cCommandCount[Command] = Count then
          PerformCommand;
      end;
    ' ', ',', #9, #10, #13:; // do nothing
    else
      Result := False;
      exit;
    end;//case
    
    if (Command <> pcUnknown) and (cCommandCount[Command] = Count) then
    begin
      PerformCommand;
      Command := pcUnknown;
    end;
    inc(Pos);
  end;
end;

{ TSvgNamedList }

function TSvgNamedList.GetItems(Index: integer): TSvgNamedItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TpgSvgImport }

function TpgSvgImport.AttributeCompare(Item1, Item2: TObject; Info: pointer): integer;
begin
  Result := AnsiCompareText(TSvgNamedItem(Item1).Name, TSvgNamedItem(Item2).Name);
end;

constructor TpgSvgImport.Create;
begin
  inherited Create(AOwner);
  FParser := TpgParser.Create;
  FParser.OnDebugOut := DoDebugOut;
end;

destructor TpgSvgImport.Destroy;
begin
  FreeAndNil(FParser);
  FreeAndNil(FElementTypes);
  FreeAndNil(FAttributeTypes);
  inherited;
end;

function TpgSvgImport.ElementCompare(Item1, Item2: TObject; Info: pointer): integer;
begin
  Result := AnsiCompareText(TSvgNamedItem(Item1).Name, TSvgNamedItem(Item2).Name);
end;

procedure TpgSvgImport.ImportScene(AScene: TpgScene; AStream: TStream);
var
  Xml: TNativeXml;
begin
  FScene := AScene;
  FScene.Clear;
  FScene.BeginUpdate;
  if not assigned(FElementTypes) then
    PrepareLists;

  Xml := TNativeXml.Create(Self);
  try
    // load from stream
    Xml.LoadFromStream(AStream);

    // class method: canonicalize
    TNativeXmlC14n.Canonicalize(Xml);

    if not assigned(Xml.Root) or (Xml.Root.Name <> 'svg') then
    begin
      DoDebugOut(Self, wsFail, sSVGRootExpected);
      exit;
    end;

    // SVG defaults
    FScene.ViewPort.Fill.AsColor32 := clBlack32;
    FScene.ViewPort.StrokeWidth.Value := 1;
    FScene.ViewPort.FontSize.Value := 12;

    // arbitrary.. must redo this so it uses bounding box
    FScene.ViewPort.Width.Value := 800;
    FScene.ViewPort.Height.Value := 600;

    ParseElement(FScene.ViewPort, Xml.Root);
  finally
    Xml.Free;
  end;
  FScene.EndUpdate;
end;

procedure TpgSvgImport.ParseAspect(AElement: TpgElement; AValue: Utf8String);
var
  i: integer;
  Aspect, MeetOrSlice: Utf8String;
begin
  if not (AElement is TpgViewPort) then
    exit;
  Aspect := BreakString(AValue, ' ', MeetOrSlice, True, False);
  for i := 0 to cSvgAspectInfoCount - 1 do
    if cSvgAspectInfo[i].Name = ASpect then
    begin
      TpgViewPort(AElement).PreserveAspect.Value := cSvgAspectInfo[i].Value;
      break;
    end;
  if length(MeetOrSlice) = 0 then
    MeetOrSlice := Aspect;

  if MeetOrSlice = 'meet' then
    TpgViewPort(AElement).MeetOrSlice.Value := msMeet
  else if MeetOrSlice = 'slice' then
    TpgViewPort(AElement).MeetOrSlice.Value := msSlice;
end;

function TpgSvgImport.ParseAttribute(const AName: Utf8String; ASrcPos: int64; AElement: TpgElement;
  var AttributeType: TSvgAttributeType): TpgPropInfo;
var
  i, Index, Count: integer;
  Item: TSvgNamedItem;
  Attr: PSvgAttributeRec;
  Found: boolean;
begin
  Item := TSvgNamedItem.Create;
  Item.Name := AName;
  FAttributeTypes.FindMultiple(Item, Index, Count);
  Item.Free;
  // Use the attribute that our element descends from
  Found := False;
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Attr := FAttributeTypes[i + Index].Data;
    AttributeType := Attr.AType;
    Result := GetPropInfo(Attr.PropId);
    if not assigned(Result) then
      continue;
    if AElement is Result.MinElementClass then
    begin
      Found := True;
      break;
    end;
  end;
  if not Found then
  begin
    Result := nil;
    DoDebugOut(Self, wsWarn, Format('Unknown property "%s" for class %s at %d', [AName, AElement.ClassName, ASrcPos]))
  end;
end;

procedure TpgSvgImport.ParseElement(AElement: TpgElement; ANode: TXmlNode);
var
  i, Index: integer;
  Item: TSvgNamedItem;
  ElementRec: PSvgElementRec;
  PropInfo: TpgPropInfo;
  SubElement: TpgElement;
  AttType: TSvgAttributeType;
  Found: boolean;
  CharData: Utf8String;
begin
  // Parse properties
  for i := 0 to ANode.AttributeCount - 1 do
  begin
    PropInfo := ParseAttribute(ANode.AttributeName[i], ANode.Attributes[i].SourcePos,
      AElement, AttType);
    if assigned(PropInfo) then
      ParseProp(AElement, AttType, PropInfo, ANode.AttributeValue[i])
  end;

  // Parse chardata
  CharData := trim(ANode.Value);
  if length(CharData) > 0 then
  begin
    PropInfo := ParseAttribute('chardata', ANode.SourcePos, AElement, AttType);
    if assigned(PropInfo) then
      ParseProp(AElement, AttType, PropInfo, CharData);
  end;

  // Parse sub-elements
  for i := 0 to ANode.NodeCount - 1 do
  begin
    if ANode.Nodes[i].ElementType <> xeElement then
      continue;

    Item := TSvgNamedItem.Create;
    try
      Item.Name := ANode.Nodes[i].Name;
      Found := FElementTypes.Find(Item, Index);

      if Found then
      begin
        ElementRec := FElementTypes[Index].Data;
        // Create sub item
        SubElement := ElementRec.ElementClass.Create(AElement);
        // Parse sub item
        DoDebugOut(Self, wsInfo, Format('parsing class "%s"', [SubElement.ClassName]));
        ParseElement(SubElement, ANode.Nodes[i]);
      end else
      begin
        DoDebugOut(Self, wsWarn, Format('unknown element "%s" at pos %d',
          [ANode.Nodes[i].Name, ANode.Nodes[i].SourcePos]));
      end;
    finally
      Item.Free;
    end;
  end;
end;

procedure TpgSvgImport.ParseProp(AElement: TpgElement; AttrType: TSvgAttributeType;
  APropInfo: TpgPropInfo; const AValue: Utf8String);
var
  Prop: TpgProp;
  MsgValue: Utf8String;
begin
  Prop := TElementAccess(AElement).PropById(APropInfo.Id);
  case AttrType of
  atString:
    TpgStringProp(Prop).Value := AValue;
  atFloat:
    TpgFloatProp(Prop).Value := FParser.pgParseNumber(AValue);
  atLength:
    ParsePropLength(TpgLengthProp(Prop), AValue);
  atLengthList:
    ParsePropLengthList(TpgLengthListProp(Prop), AValue);
  atFloatList:
    ParsePropPointList(TpgFloatListProp(Prop), AValue);
  atTransform:
    ParsePropTransform(TpgTransformProp(Prop), AValue);
  atViewBox:
    ParsePropViewBox(TpgViewBoxProp(Prop), AValue);
  atPaint:
    ParsePropPaint(TpgPaintProp(Prop), AValue);
  atPath:
    ParsePropPath(TpgPathProp(Prop), AValue);
  atImage:
    ParsePropImage(TpgImageProp(Prop), AValue);
  atAspect:
    ParseAspect(AElement, AValue);
  atStyle:
    ParseStyle(AElement, AValue);
  else
    if length(AValue) < 20 then
      MsgValue := AValue
    else
      MsgValue := copy(AValue, 1, 20) + '...';
    DoDebugOut(Self, wsWarn, Format('Error unknown attribute type %d (%s)', [integer(AttrType), MsgValue]));
  end;
end;

procedure TpgSvgImport.ParsePropImage(AProp: TpgImageProp; const AValue: Utf8String);
begin
  AProp.LoadFromURI(AValue);
end;

procedure TpgSvgImport.ParsePropLength(AProp: TpgLengthProp; const AValue: Utf8String);
var
  Units: TpgLengthUnits;
  Size: double;
begin
  Units := luNone;
  Size := 0;
  if pgSvgParseLength(AValue, Units, Size) then
  begin
    AProp.Units := Units;
    AProp.Value := Size;
  end else
  begin
    DoDebugOut(Self, wsWarn, Format('Error parsing length attribute string "%s"', [AValue]));
  end;
end;

procedure TpgSvgImport.ParsePropLengthList(AProp: TpgLengthListProp; const AValue: Utf8String);
var
  Next, Chunk: Utf8String;
  Units: TpgLengthUnits;
  Value: double;
begin
  Next := pgConditionListString(AValue);
  while length(Next) > 0 do
  begin
    Chunk := BreakString(Next, ' ', Next, True, False);
    if length(Chunk) = 0 then
      break;
    if pgSvgParseLength(Chunk, Units, Value) then
      AProp.Add(Value, Units)
    else
      DoDebugOut(Self, wsWarn, Format('error parsing length attribute string "%s"', [Chunk]));
  end;
end;

procedure TpgSvgImport.ParsePropPaint(AProp: TpgPaintProp; const AValue: Utf8String);
var
  Count: integer;
  Color: TpgColorARGB;
  RGB: array[0..2] of double;
  Name, S, URI: Utf8String;
  Ref: TpgElement;
begin
  FillChar(Color, SizeOf(Color), 0);
  if length(AValue) = 0 then
    exit;
  if AValue = 'none' then
  begin
    // no paint
    AProp.PaintStyle := psNone;
    exit;
  end;
  if AValue[1] = '#' then
  begin
    AProp.PaintStyle := psColor;
    S := copy(AValue, 2, length(AValue));
    if length(S) = 3 then
    begin
      Color.R := StrToInt('$' + S[1] + S[1]);
      Color.G := StrToInt('$' + S[2] + S[2]);
      Color.B := StrToInt('$' + S[3] + S[3]);
      Color.A := 255;
    end else if length(S) = 6 then
    begin
      Color.R := StrToInt('$' + copy(S, 1, 2));
      Color.G := StrToInt('$' + copy(S, 3, 2));
      Color.B := StrToInt('$' + copy(S, 5, 2));
      Color.A := 255;
    end else
      DoDebugOut(Self, wsWarn, Format('Invalid color value in paint prop (%s)', [S]));

    AProp.AsColor32 := TpgColor32(Color);
    exit;
  end;
  if pos('rgb(', AValue) = 1 then
  begin
    Count := 3;
    FParser.pgParseNumberArray(copy(AValue, 5, length(AValue) - 5), RGB, Count);
    Color.R := pgLimit(round(RGB[0]), 0, 255);
    Color.G := pgLimit(round(RGB[1]), 0, 255);
    Color.B := pgLimit(round(RGB[2]), 0, 255);
    Color.A := 255;
    AProp.PaintStyle := psColor;
    AProp.AsColor32 := TpgColor32(Color);
    exit;
  end;
  if pos('url(', AValue) = 1 then
  begin
    // Find URI string
    URI := BreakString(copy(AValue, 5, length(AValue)), ')', S, True, True);
    AProp.PaintStyle := psUnknown;
    Name := pgSvgGetUriFragment(URI);
    Ref := FScene.ElementByName(Name);
    if Ref is TpgPaintServer then
      AProp.Reference := Ref
    else
      AProp.Reference := nil;
    if not assigned(AProp.Reference) then
    begin
      if length(S) > 0 then
        ParsePropPaint(AProp, S)
      else
        DoDebugOut(Self, wsWarn, Format('Invalid reference in paint prop (%s)', [S]));
    end;
    exit;
  end;
  if AValue = 'currentColor' then
  begin
    // to do: reference to current color
    exit;
  end;
  // named color?
  if pgSvgFromNamedColor32(AValue, TpgColor32(Color)) then
  begin
    AProp.PaintStyle := psColor;
    AProp.AsColor32 := TpgColor32(Color);
    exit;
  end;
  // still here?
  DoDebugOut(Self, wsWarn, Format('Error parsing paint attribute string "%s"', [AValue]));
end;

procedure TpgSvgImport.ParsePropPath(AProp: TpgPathProp; const AValue: Utf8String);
var
  P: TpgCommandPath;
begin
  P := TpgCommandPath.Create;
  if pgSvgParseCommandPath(AValue, P) then
    AProp.Value := P
  else
  begin
    DoDebugOut(Self, wsWarn, 'Error in path parser');
    P.Free;
  end;
end;

procedure TpgSvgImport.ParsePropPointList(AProp: TpgFloatListProp; const AValue: Utf8String);
var
  Next, Chunk: Utf8String;
  Value: double;
begin
  Next := pgConditionListString(AValue);
  while length(Next) > 0 do
  begin
    Chunk := BreakString(Next, ' ', Next, True, False);
    if length(Chunk) = 0 then
      break;
    Value := FParser.pgParseNumber(Chunk);
    AProp.Add(Value);
  end;
end;

procedure TpgSvgImport.ParsePropTransform(AProp: TpgTransformProp; const AValue: Utf8String);
var
  Transform, Params, Next: Utf8String;
  Numbers: array[0..5] of double;
  Command: TSvgTransformCommand;
  T: TpgAffineTransform;
begin
  Next := AValue;
  T := TpgAffineTransform.Create;
  // Parse the list of transforms
  repeat
    // get transform type
    Transform := BreakString(Next, '(', Next, True, True);
    // do we have anything?
    if length(Transform) = 0 then
      break;
    // get parameters
    Params := BreakString(Next, ')', Next, True, True);

    // parse command and parameters
    ParsePropTransformParams(Transform, Params, Command, Numbers);

    // remove comma
    Next := Trim(Next);
    if (length(Next) > 0) and (Next[1] = ',') then
      Next := trim(copy(Next, 2, length(Next)));

    case Command of
    tcMatrix:
      T.MultiplyMatrix(
        Numbers[0], Numbers[1], Numbers[2],
        Numbers[3], Numbers[4], Numbers[5]);
    tcTranslate:
      T.Translate(Numbers[0], Numbers[1]);
    tcScale:
      T.Scale(Numbers[0], Numbers[1]);
    tcRotate:
      T.Rotate(Numbers[0], Numbers[1], Numbers[2]);
    tcSkewX:
      T.SkewX(Numbers[0]);
    tcSkewY:
      T.SkewY(Numbers[0]);
    else
      DoDebugOut(Self, wsWarn, Format('Unrecognised transform %s(%s)', [Transform, Params]));
      T.Free;
      exit;
    end;//case

  until False;
  // The property becomes owner, we don't have to free T
  AProp.Value := T;
end;

procedure TpgSvgImport.ParsePropTransformParams(const ACommand, AParams: Utf8String;
  var ACommandId: TSvgTransformCommand; var ANumbers: array of double);
var
  i, Count: integer;
begin
  Count := 6;
  ACommandId := tcNone;
  FParser.pgParseNumberArray(AParams, ANumbers, Count);
  if ACommand = 'matrix' then
  begin
    ACommandId := tcMatrix;
    if Count <> 6 then
      DoDebugOut(Self, wsWarn, 'Illegal number of arguments in matrix');
    exit;
  end;
  ACommandId := tcNone;
  for i := 0 to cSvgTransformInfoCount - 1 do
    if cSvgTransformInfo[i].Name = ACommand then
    begin
      ACommandId := cSvgTransformInfo[i].Cmd;
      break;
    end;
  // additional checks
  case ACommandId of
  tcMatrix:
    if Count <> 6 then
      DoDebugOut(Self, wsWarn, 'Illegal number of arguments in matrix');
  tcScale:
    if Count = 1 then
      ANumbers[1] := ANumbers[0];
  end;
end;

procedure TpgSvgImport.ParsePropViewBox(AProp: TpgViewBoxProp; const AValue: Utf8String);
var
  Pos: integer;
begin
  Pos := 1;
  AProp.MinX := FParser.pgParseNumber(AValue, Pos);
  SkipCommaWS(AValue, Pos);
  AProp.MinY := FParser.pgParseNumber(AValue, Pos);
  SkipCommaWS(AValue, Pos);
  AProp.Width := FParser.pgParseNumber(AValue, Pos);
  SkipCommaWS(AValue, Pos);
  AProp.Height := FParser.pgParseNumber(AValue, Pos);
end;

procedure TpgSvgImport.ParseStyle(AElement: TpgElement; AValue: Utf8String);
var
  NameValue: Utf8String;
  AttName, AttValue: Utf8String;
  PropInfo: TpgPropInfo;
  AttType: TSvgAttributeType;
begin
  repeat
    NameValue := BreakString(AValue, ';', AValue, True, False);
    if length(NameValue) > 0 then
    begin
      // Split the attribute element
      AttName := BreakString(NameValue, ':', AttValue, True, True);
      if (length(AttName) > 0) and (length(AttValue) > 0) then
      begin
        PropInfo := ParseAttribute(AttName, 0 {todo: where?}, AElement, AttType);
        if assigned(PropInfo) then
          ParseProp(AElement, AttType, PropInfo, AttValue)
      end;
    end;
  until length(AValue) = 0;
end;

procedure TpgSvgImport.PrepareLists;
var
  i: integer;
  Item: TSvgNamedItem;
begin
  // Prepare element type list
  FElementTypes := TSvgNamedList.Create;
  FElementTypes.OnCompare := ElementCompare;
  for i := 0 to cSvgElementTypeCount - 1 do
  begin
    Item := TSvgNamedItem.Create;
    Item.Name := cSvgElementTypes[i].Name;
    Item.Data := @cSvgElementTypes[i];
    FElementTypes.Add(Item);
  end;
  // Prepare attribute type list
  FAttributeTypes := TSvgNamedList.Create;
  FAttributeTypes.OnCompare := AttributeCompare;
  for i := 0 to cSvgAttributeTypeCount - 1 do
  begin
    Item := TSvgNamedItem.Create;
    Item.Name := cSvgAttributeTypes[i].Name;
    Item.Data := @cSvgAttributeTypes[i];
    FAttributeTypes.Add(Item);
  end;
end;

end.
