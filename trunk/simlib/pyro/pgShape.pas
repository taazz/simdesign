{ Project: Pyro
  Module: Pyro Core

  Description:
  Shapes: defining a path for fill and stroke. Some specific shapes like rectangle,
  ellipse, etc.

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgShape;

{$i simdesign.inc}

interface

uses
  SysUtils, pgViewPort, pgPath, Pyro, pgDocument, pgCommandPath,
  sdDebug;

type

  TpgShape = class(TpgGraphic)
  public
  end;

  TpgRectangle = class(TpgShape)
  private
    function GetHeight: TpgVLengthProp;
    function GetRx: TpgHLengthProp;
    function GetRy: TpgVLengthProp;
    function GetWidth: TpgHLengthProp;
    function GetX: TpgHLengthProp;
    function GetY: TpgVLengthProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property X: TpgHLengthProp read GetX;
    property Y: TpgVLengthProp read GetY;
    property Width: TpgHLengthProp read GetWidth;
    property Height: TpgVLengthProp read GetHeight;
    property Rx: TpgHLengthProp read GetRx;
    property Ry: TpgVLengthProp read GetRy;
  end;

  TpgCircle = class(TpgShape)
  private
    function GetCx: TpgHLengthProp;
    function GetCy: TpgVLengthProp;
    function GetR: TpgLengthProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property Cx: TpgHLengthProp read GetCx;
    property Cy: TpgVLengthProp read GetCy;
    property R: TpgLengthProp read GetR;
  end;

  TpgEllipse = class(TpgShape)
  private
    function GetCx: TpgHLengthProp;
    function GetCy: TpgVLengthProp;
    function GetRx: TpgHLengthProp;
    function GetRy: TpgVLengthProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property Cx: TpgHLengthProp read GetCx;
    property Cy: TpgVLengthProp read GetCy;
    property Rx: TpgHLengthProp read GetRx;
    property Ry: TpgVLengthProp read GetRy;
  end;

  TpgLine = class(TpgShape)
  private
    function GetX1: TpgHLengthProp;
    function GetX2: TpgHLengthProp;
    function GetY1: TpgVLengthProp;
    function GetY2: TpgVLengthProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property X1: TpgHLengthProp read GetX1;
    property Y1: TpgVLengthProp read GetY1;
    property X2: TpgHLengthProp read GetX2;
    property Y2: TpgVLengthProp read GetY2;
  end;

  TpgPolyLineShape = class(TpgShape)
  private
    function GetPoints: TpgFloatListProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property Points: TpgFloatListProp read GetPoints;
  end;

  TpgPolygonShape = class(TpgPolyLineShape)
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  end;

  TpgPathProp = class(TpgStoredProp)
  private
    FValue: TpgCommandPath;
    procedure SetValue(const Value: TpgCommandPath);
  protected
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
  public
    destructor Destroy; override;
    property Value: TpgCommandPath read FValue write SetValue;
  end;

  TpgPathShape = class(TpgShape)
  private
    function GetPath: TpgPathProp;
  public
    procedure PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo); override;
  published
    property Path: TpgPathProp read GetPath;
  end;

implementation

{ TpgRectangle }

function TpgRectangle.GetHeight: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piRectHeight));
end;

function TpgRectangle.GetRx: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piRectRx));
end;

function TpgRectangle.GetRy: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piRectRy));
end;

function TpgRectangle.GetWidth: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piRectWidth));
end;

function TpgRectangle.GetX: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piRectX));
end;

function TpgRectangle.GetY: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piRectY));
end;

procedure TpgRectangle.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  Res: boolean;
begin
  Res := APath.Rectangle(
    X.ToDevice(AInfo), Y.ToDevice(AInfo),
    Width.ToDevice(AInfo), Height.ToDevice(AInfo),
    Rx.ToDevice(AInfo), Ry.ToDevice(AInfo));
  if not Res then
    raise Exception.Create(sIllegalPropertyValue);
end;

{ TpgCircle }

function TpgCircle.GetCx: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piCircleCx));
end;

function TpgCircle.GetCy: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piCircleCy));
end;

function TpgCircle.GetR: TpgLengthProp;
begin
  Result := TpgLengthProp(PropById(piCircleR));
end;

procedure TpgCircle.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  Res: boolean;
  Rad: double;
begin
  Rad := R.ToDevice(AInfo);
  Res := APath.Ellipse(
    Cx.ToDevice(AInfo), Cy.ToDevice(AInfo),
    Rad, Rad);
  if not Res then
    raise Exception.Create(sIllegalPropertyValue);
end;

{ TpgEllipse }

function TpgEllipse.GetCx: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piEllipseCx));
end;

function TpgEllipse.GetCy: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piEllipseCy));
end;

function TpgEllipse.GetRx: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piEllipseRx));
end;

function TpgEllipse.GetRy: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piEllipseRy));
end;

procedure TpgEllipse.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  Res: boolean;
begin
  Res := APath.Ellipse(
    Cx.ToDevice(AInfo), Cy.ToDevice(AInfo),
    Rx.ToDevice(AInfo), Ry.ToDevice(AInfo));
  if not Res then
    raise Exception.Create(sIllegalPropertyValue);
end;

{ TpgLine }

function TpgLine.GetX1: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piLineX1));
end;

function TpgLine.GetX2: TpgHLengthProp;
begin
  Result := TpgHLengthProp(PropById(piLineX2));
end;

function TpgLine.GetY1: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piLineY1));
end;

function TpgLine.GetY2: TpgVLengthProp;
begin
  Result := TpgVLengthProp(PropById(piLineY2));
end;

procedure TpgLine.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
begin
  APath.MoveTo(X1.ToDevice(AInfo), Y1.ToDevice(AInfo));
  APath.LineTo(X2.ToDevice(AInfo), Y2.ToDevice(AInfo));
end;

{ TpgPathProp }

destructor TpgPathProp.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TpgPathProp.Read(AStorage: TpgStorage);
var
  i, Count: integer;
  ValueBuf: array of double;
  CommandBuf: array of TpgCommandPathItemR;
begin
  if not assigned(FValue) then
    FValue := TpgCommandPath.Create;
  FValue.Clear;
  Count := AStorage.ReadInt;
  if Count > 0 then
  begin
    SetLength(ValueBuf, Count);
    for i := 0 to Count - 1 do
      ValueBuf[i] := AStorage.ReadFloat;
    TpgCommandPath(FValue).SetValues(@ValueBuf[0], Count);
  end;
  Count := AStorage.ReadInt;
  if Count > 0 then
  begin
    SetLength(CommandBuf, Count);
    for i := 0 to Count - 1 do begin
      CommandBuf[i].Command := TpgPathCommandStyle(AStorage.ReadInt);
      CommandBuf[i].Index := AStorage.ReadInt;
    end;
    TpgCommandPath(FValue).SetCommandItems(@CommandBuf[0], Count);
  end;
end;

procedure TpgPathProp.SetValue(const Value: TpgCommandPath);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgPathProp;
begin
  GetCaller(Caller, MustWrite);
  if (FValue <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    Prop := TpgPathProp(CallerProperty(Caller));
    FreeAndNil(Prop.FValue);
    Prop.FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgPathProp.Write(AStorage: TpgStorage);
var
  i, Count: integer;
  FirstValue: Pdouble;
  FirstCommand: PpgCommandPathItemR;
begin
  Count := TpgCommandPath(FValue).ValueCount;
  AStorage.WriteInt(Count);
  FirstValue := TpgCommandPath(FValue).FirstValue;
  for i := 0 to Count - 1 do
  begin
    AStorage.WriteFloat(FirstValue^);
    inc(FirstValue);
  end;
  Count := TpgCommandPath(FValue).CommandCount;
  AStorage.WriteInt(Count);
  FirstCommand := TpgCommandPath(FValue).FirstCommand;
  for i := 0 to Count - 1 do
  begin
    AStorage.WriteInt(integer(FirstCommand.Command));
    AStorage.WriteInt(FirstCommand.Index);
    inc(FirstCommand);
  end;
end;

{ TpgPolyLineShape }

function TpgPolyLineShape.GetPoints: TpgFloatListProp;
begin
  Result := TpgFloatListProp(PropById(piPoints));
end;

procedure TpgPolyLineShape.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  i: integer;
  List: TpgFloatProtectList;
begin
  inherited;
  if not Points.ExistsLocal then
    exit;
  List := Points.Values;
  if List.Count < 2 then
    exit;
  APath.MoveTo(List[0].Value, List[1].Value);
  for i := 1 to List.Count div 2 - 1 do
    APath.LineTo(List[i * 2].Value, List[i * 2 + 1].Value);
end;

{ TpgPolygonShape }

procedure TpgPolygonShape.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
begin
  inherited;
  // at least two points for close to make sense
  if Points.Values.Count >= 4 then
    APath.ClosePath;
end;

{ TpgPathShape }

function TpgPathShape.GetPath: TpgPathProp;
begin
  Result := TpgPathProp(PropById(piPath));
end;

procedure TpgPathShape.PlayFillPath(APath: TpgPath; const AInfo: TpgDeviceInfo);
var
  CP: TpgCommandPath;
begin
  CP := GetPath.Value;
  if assigned(CP) then
    TpgCommandPath(CP).PlayToPath(APath);
end;

initialization

  // Rectangle
  RegisterElement(eiRectangle, TpgRectangle, 'Rectangle');
  RegisterProp(piRectX, TpgHLengthProp, 'X', TpgRectangle, [pfStored]);
  RegisterProp(piRectY, TpgVLengthProp, 'Y', TpgRectangle, [pfStored]);
  RegisterProp(piRectWidth, TpgHLengthProp, 'Width', TpgRectangle, [pfStored]);
  RegisterProp(piRectHeight, TpgVLengthProp, 'Height', TpgRectangle, [pfStored]);
  RegisterProp(piRectRx, TpgHLengthProp, 'Rx', TpgRectangle, [pfStored]);
  RegisterProp(piRectRy, TpgVLengthProp, 'Ry', TpgRectangle, [pfStored]);

  // Circle
  RegisterElement(eiCircle, TpgCircle, 'Circle');
  RegisterProp(piCircleCx, TpgHLengthProp, 'Cx', TpgCircle, [pfStored]);
  RegisterProp(piCircleCy, TpgVLengthProp, 'Cy', TpgCircle, [pfStored]);
  RegisterProp(piCircleR, TpgLengthProp, 'R', TpgCircle, [pfStored]);

  // Ellipse
  RegisterElement(eiEllipse, TpgEllipse, 'Ellipse');
  RegisterProp(piEllipseCx, TpgHLengthProp, 'Cx', TpgEllipse, [pfStored]);
  RegisterProp(piEllipseCy, TpgVLengthProp, 'Cy', TpgEllipse, [pfStored]);
  RegisterProp(piEllipseRx, TpgHLengthProp, 'Rx', TpgEllipse, [pfStored]);
  RegisterProp(piEllipseRy, TpgVLengthProp, 'Ry', TpgEllipse, [pfStored]);

  // Line
  RegisterElement(eiLine, TpgLine, 'Line');
  RegisterProp(piLineX1, TpgHLengthProp, 'X1', TpgLine, [pfStored]);
  RegisterProp(piLineY1, TpgVLengthProp, 'Y1', TpgLine, [pfStored]);
  RegisterProp(piLineX2, TpgHLengthProp, 'X2', TpgLine, [pfStored]);
  RegisterProp(piLineY2, TpgVLengthProp, 'Y2', TpgLine, [pfStored]);

  // PolyLine, polygon
  RegisterElement(eiPolyLine, TpgPolyLineShape, 'PolyLine');
  RegisterElement(eiPolygon, TpgPolygonShape, 'Polygon');
  RegisterProp(piPoints, TpgFloatListProp, 'Points', TpgPolyLineShape, [pfStored]);

  // PathShape
  RegisterElement(eiPathShape, TpgPathShape, 'Path');
  RegisterProp(piPath, TpgPathProp, 'Path', TpgPathShape, [pfStored]);

end.
