{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Core<p>

  <b>Description:</b><p>
  Special path type that stores a list of path commands

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgCommandPath;

{$i simdesign.inc}

interface

uses
  Classes, Pyro, pgPath, pgGeometry, sdDebug;

type

  TpgPathCommandStyle = (
    pcUnknown,
    pcClosePath,
    pcMoveToAbs,
    pcMoveToRel,
    pcLineToAbs,
    pcLineToRel,
    pcLineToHorAbs,
    pcLineToHorRel,
    pcLineToVerAbs,
    pcLineToVerRel,
    pcCurveToCubicAbs,
    pcCurveToCubicRel,
    pcCurveToCubicSmoothAbs,
    pcCurveToCubicSmoothRel,
    pcCurveToQuadraticAbs,
    pcCurveToQuadraticRel,
    pcCurveToQuadraticSmoothAbs,
    pcCurveToQuadraticSmoothRel,
    pcArcToAbs,
    pcArcToRel
  );

  TpgCommandPathItemR = packed record
    Command: TpgPathCommandStyle;
    Index: word;
  end;
  PpgCommandPathItemR = ^TpgCommandPathItemR;

  TpgCommandPath = class(TPersistent)
  private
    FValues: array of double;
    FValueCount: integer;
    FCommands: array of TpgCommandPathItemR;
    FCommandCount: integer;
  protected
    procedure AddCommand(ACommand: TpgPathCommandStyle);
    procedure AddValues(ACount: integer; AValues: array of double);
    function GetValueCount: integer;
    function GetCommandCount: integer;
  public
    procedure Clear;
    procedure ClosePath;
    procedure MoveToAbs(X, Y: double);
    procedure MoveToRel(X, Y: double);
    procedure LineToAbs(X, Y: double);
    procedure LineToRel(X, Y: double);
    procedure LineToHorAbs(X: double);
    procedure LineToHorRel(X: double);
    procedure LineToVerAbs(Y: double);
    procedure LineToVerRel(Y: double);
    procedure CurveToCubicAbs(C1x, C1y, C2x, C2y, X, Y: double);
    procedure CurveToCubicRel(C1x, C1y, C2x, C2y, X, Y: double);
    procedure CurveToCubicSmoothAbs(C2x, C2y, X, Y: double);
    procedure CurveToCubicSmoothRel(C2x, C2y, X, Y: double);
    procedure CurveToQuadraticAbs(Cx, Cy, X, Y: double);
    procedure CurveToQuadraticRel(Cx, Cy, X, Y: double);
    procedure CurveToQuadraticSmoothAbs(X, Y: double);
    procedure CurveToQuadraticSmoothRel(X, Y: double);
    // Create an arc from current location to X, Y, using an arc that has radius
    // in X of Rx and radius in Y of Ry, and a rotation of Angle (in degrees).
    // Since there are basically 4 solutions for an ellipse through 2 points, the
    // flags indicate which one to take. if LargeArc = True, the largest arclength
    // solution will be chosen. If Sweep = True, the solution which walks in positive
    // arc direction will be chosen.
    procedure ArcToAbs(Rx, Ry, Angle: double; LargeArc, Sweep: boolean; X, Y: double);
    procedure ArcToRel(Rx, Ry, Angle: double; LargeArc, Sweep: boolean; X, Y: double);
    // set values
    procedure SetValues(AFirst: Pdouble; ACount: integer);
    // Sets all command items in one go
    procedure SetCommandItems(AFirst: PpgCommandPathItemR; ACount: integer);
    function FirstValue: Pdouble;
    function FirstCommand: PpgCommandPathItemR;
    procedure PlayToPath(APath: TpgPath);
    //
    property ValueCount: integer read GetValueCount;
    property CommandCount: integer read GetCommandCount;
  end;

implementation

type

  TDoubleArray = array[0..MaxInt div SizeOf(double) - 1] of double;
  PDoubleArray = ^TDoubleArray;

function BoolToInt(ABool: boolean): integer;
begin
  if ABool then
    Result := 1
  else
    Result := 0;
end;

{ TpgCommandPath }

procedure TpgCommandPath.AddCommand(ACommand: TpgPathCommandStyle);
begin
  if length(FCommands) <= FCommandCount then begin
    // Increase capacity
    SetLength(FCommands, (FCommandCount * 3) div 2 + 4);
  end;
  with FCommands[FCommandCount] do begin
    Command := ACommand;
    Index := FValueCount;
  end;
  inc(FCommandCount);
end;

procedure TpgCommandPath.AddValues(ACount: integer; AValues: array of double);
begin
  if ACount <= 0 then exit;
  if length(FValues) < FValueCount + ACount then begin
    // Increase capacity
    SetLength(FValues, ((FValueCount + ACount)* 3) div 2 + 4);
  end;
  Move(AValues[0], FValues[FValueCount], ACount * SizeOf(double));
  inc(FValueCount, ACount);
end;

procedure TpgCommandPath.ArcToAbs(Rx, Ry, Angle: double; LargeArc,
  Sweep: boolean; X, Y: double);
begin
  AddCommand(pcArcToAbs);
  AddValues(7, [Rx, Ry, Angle, BoolToInt(LargeArc), BoolToInt(Sweep), X, Y]);
end;

procedure TpgCommandPath.ArcToRel(Rx, Ry, Angle: double; LargeArc,
  Sweep: boolean; X, Y: double);
begin
  AddCommand(pcArcToRel);
  AddValues(7, [Rx, Ry, Angle, BoolToInt(LargeArc), BoolToInt(Sweep), X, Y]);
end;

procedure TpgCommandPath.Clear;
begin
  FValueCount := 0;
  FCommandCount := 0;
end;

procedure TpgCommandPath.ClosePath;
begin
  AddCommand(pcClosePath);
end;

procedure TpgCommandPath.CurveToCubicAbs(C1x, C1y, C2x, C2y, X, Y: double);
begin
  AddCommand(pcCurveToCubicAbs);
  AddValues(6, [C1x, C1y, C2x, C2y, X, Y]);
end;

procedure TpgCommandPath.CurveToCubicRel(C1x, C1y, C2x, C2y, X, Y: double);
begin
  AddCommand(pcCurveToCubicRel);
  AddValues(6, [C1x, C1y, C2x, C2y, X, Y]);
end;

procedure TpgCommandPath.CurveToCubicSmoothAbs(C2x, C2y, X, Y: double);
begin
  AddCommand(pcCurveToCubicSmoothAbs);
  AddValues(4, [C2x, C2y, X, Y]);
end;

procedure TpgCommandPath.CurveToCubicSmoothRel(C2x, C2y, X, Y: double);
begin
  AddCommand(pcCurveToCubicSmoothRel);
  AddValues(4, [C2x, C2y, X, Y]);
end;

procedure TpgCommandPath.CurveToQuadraticAbs(Cx, Cy, X, Y: double);
begin
  AddCommand(pcCurveToQuadraticAbs);
  AddValues(4, [Cx, Cy, X, Y]);
end;

procedure TpgCommandPath.CurveToQuadraticRel(Cx, Cy, X, Y: double);
begin
  AddCommand(pcCurveToQuadraticRel);
  AddValues(4, [Cx, Cy, X, Y]);
end;

procedure TpgCommandPath.CurveToQuadraticSmoothAbs(X, Y: double);
begin
  AddCommand(pcCurveToQuadraticSmoothAbs);
  AddValues(2, [X, Y]);
end;

procedure TpgCommandPath.CurveToQuadraticSmoothRel(X, Y: double);
begin
  AddCommand(pcCurveToQuadraticSmoothRel);
  AddValues(2, [X, Y]);
end;

function TpgCommandPath.FirstCommand: PpgCommandPathItemR;
begin
  if FCommandCount > 0 then
    Result := @FCommands[0]
  else
    Result := nil;
end;

function TpgCommandPath.FirstValue: Pdouble;
begin
  if FValueCount > 0 then
    Result := @FValues[0]
  else
    Result := nil;
end;

function TpgCommandPath.GetCommandCount: integer;
begin
  Result := FCommandCount;
end;

function TpgCommandPath.GetValueCount: integer;
begin
  Result := FValueCount;
end;

procedure TpgCommandPath.LineToAbs(X, Y: double);
begin
  AddCommand(pcLineToAbs);
  AddValues(2, [X, Y]);
end;

procedure TpgCommandPath.LineToHorAbs(X: double);
begin
  AddCommand(pcLineToHorAbs);
  AddValues(1, [X]);
end;

procedure TpgCommandPath.LineToHorRel(X: double);
begin
  AddCommand(pcLineToHorRel);
  AddValues(1, [X]);
end;

procedure TpgCommandPath.LineToRel(X, Y: double);
begin
  AddCommand(pcLineToRel);
  AddValues(2, [X, Y]);
end;

procedure TpgCommandPath.LineToVerAbs(Y: double);
begin
  AddCommand(pcLineToVerAbs);
  AddValues(1, [Y]);
end;

procedure TpgCommandPath.LineToVerRel(Y: double);
begin
  AddCommand(pcLineToVerRel);
  AddValues(1, [Y]);
end;

procedure TpgCommandPath.MoveToAbs(X, Y: double);
begin
  AddCommand(pcMoveToAbs);
  AddValues(2, [X, Y]);
end;

procedure TpgCommandPath.MoveToRel(X, Y: double);
begin
  AddCommand(pcMoveToRel);
  AddValues(2, [X, Y]);
end;

procedure TpgCommandPath.PlayToPath(APath: TpgPath);
var
  i: integer;
  FInitialPoint: TpgPoint;
  FCurrentPoint: TpgPoint;
  FPreviousPoint: TpgPoint;
  ValueList: PDoubleArray;
  // local
  procedure DoClosePath;
  begin
    APath.ClosePath;
    FCurrentPoint := FInitialPoint;
  end;
  function ConvertToAbs(const APoint: TpgPoint): TpgPoint;
  begin
    Result.X := APoint.X + FCurrentPoint.X;
    Result.Y := APoint.Y + FCurrentPoint.Y;
  end;
  procedure DoMoveTo(const APoint: TpgPoint);
  begin
    APath.MoveTo(APoint.X, APoint.Y);
    FInitialPoint := APoint;
    FCurrentPoint := APoint;
  end;
  procedure DoLineTo(const APoint: TpgPoint);
  begin
    APath.LineTo(APoint.X, APoint.Y);
    FPreviousPoint := FCurrentPoint;
    FCurrentPoint := APoint;
  end;
  procedure DoCurveToCubic(const C1, C2, P: TpgPoint);
  begin
    APath.CurveToCubic(C1.X, C1.Y, C2.X, C2.Y, P.X, P.Y);
    FPreviousPoint := C2;
    FCurrentPoint := P;
  end;
  procedure DoCurveToQuadratic(const C, P: TpgPoint);
  begin
    APath.CurveToQuadratic(C.X, C.Y, P.X, P.Y);
    FPreviousPoint := C;
    FCurrentPoint := P;
  end;
  procedure DoArcTo(const Rx, Ry, Angle: double; LargeArc, Sweep: boolean; P: TpgPoint);
  begin
    APath.ArcTo(Rx, Ry, Angle, LargeArc, Sweep, P.X, P.Y);
    FPreviousPoint := FCurrentPoint;
    FCurrentPoint := P;
  end;
// main
begin
  // Play the command list to the pathset
  APath.Clear;
  for i := 0 to FCommandCount - 1 do begin
    {$R-}
    ValueList := @FValues[FCommands[i].Index];
    {$R+}
    case FCommands[i].Command of
    pcClosePath:
      DoClosePath;
    pcMoveToAbs:
      DoMoveTo(pgPoint(ValueList[0], ValueList[1]));
    pcMoveToRel:
      DoMoveTo(ConvertToAbs(pgPoint(ValueList[0], ValueList[1])));
    pcLineToAbs:
      DoLineTo(pgPoint(ValueList[0], ValueList[1]));
    pcLineToRel:
      DoLineTo(ConvertToAbs(pgPoint(ValueList[0], ValueList[1])));
    pcLineToHorAbs:
      DoLineTo(pgPoint(ValueList[0], FCurrentPoint.Y));
    pcLineToHorRel:
      DoLineTo(ConvertToAbs(pgPoint(ValueList[0], 0)));
    pcLineToVerAbs:
      DoLineTo(pgPoint(FCurrentPoint.X, ValueList[0]));
    pcLineToVerRel:
      DoLineTo(ConvertToAbs(pgPoint(0, ValueList[0])));
    pcCurveToCubicAbs:
      DoCurveToCubic(
        pgPoint(ValueList[0], ValueList[1]),
        pgPoint(ValueList[2], ValueList[3]),
        pgPoint(ValueList[4], ValueList[5]));
    pcCurveToCubicRel:
      DoCurveToCubic(
        ConvertToAbs(pgPoint(ValueList[0], ValueList[1])),
        ConvertToAbs(pgPoint(ValueList[2], ValueList[3])),
        ConvertToAbs(pgPoint(ValueList[4], ValueList[5])));
    pcCurveToCubicSmoothAbs:
      DoCurveToCubic(
        pgReflectPoint(FPreviousPoint, FCurrentPoint),
        pgPoint(ValueList[0], ValueList[1]),
        pgPoint(ValueList[2], ValueList[3]));
    pcCurveToCubicSmoothRel:
      DoCurveToCubic(
        pgReflectPoint(FPreviousPoint, FCurrentPoint),
        ConvertToAbs(pgPoint(ValueList[0], ValueList[1])),
        ConvertToAbs(pgPoint(ValueList[2], ValueList[3])));
    pcCurveToQuadraticAbs:
      DoCurveToQuadratic(
        pgPoint(ValueList[0], ValueList[1]),
        pgPoint(ValueList[2], ValueList[3]));
    pcCurveToQuadraticRel:
      DoCurveToQuadratic(
        ConvertToAbs(pgPoint(ValueList[0], ValueList[1])),
        ConvertToAbs(pgPoint(ValueList[2], ValueList[3])));
    pcCurveToQuadraticSmoothAbs:
      DoCurveToQuadratic(
        pgReflectPoint(FPreviousPoint, FCurrentPoint),
        pgPoint(ValueList[0], ValueList[1]));
    pcCurveToQuadraticSmoothRel:
      DoCurveToQuadratic(
        pgReflectPoint(FPreviousPoint, FCurrentPoint),
        ConvertToAbs(pgPoint(ValueList[0], ValueList[1])));
    pcArcToAbs:
      DoArcTo(
        ValueList[0], ValueList[1], ValueList[2],
        round(ValueList[3]) = 1, round(ValueList[4]) = 1,
        pgPoint(ValueList[5], ValueList[6]));
    pcArcToRel:
      DoArcTo(
        ValueList[0], ValueList[1], ValueList[2],
        round(ValueList[3]) = 1, round(ValueList[4]) = 1,
        ConvertToAbs(pgPoint(ValueList[5], ValueList[6])));
    end;//case
  end;
end;

procedure TpgCommandPath.SetCommandItems(AFirst: PpgCommandPathItemR; ACount: integer);
begin
  SetLength(FCommands, ACount);
  FCommandCount := ACount;
  if ACount = 0 then exit;
  Move(AFirst^, FCommands[0], ACount * SizeOf(TpgCommandPathItemR));
end;

procedure TpgCommandPath.SetValues(AFirst: Pdouble; ACount: integer);
begin
  SetLength(FValues, ACount);
  FValueCount := ACount;
  if ACount = 0 then exit;
  Move(AFirst^, FValues[0], ACount * SizeOf(double));
end;

end.
