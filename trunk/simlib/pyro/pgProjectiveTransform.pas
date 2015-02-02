{ Project: Pyro
  Module: Pyro Render

  Description:
  Specific transformtype projective transform

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgProjectiveTransform;

interface

uses
  Classes, pgViewPort, pgTransform, pgDocument, Pyro, sdDebug;

type

  // Abstract base class for transforms operating in u,v space (u and v are
  // generalized coordinates between 0 and 1)
  TpgUVTransform = class(TpgTransform)
  private
    FIsValid: boolean;
    FHeight: double;
    FMinY: double;
    FMinX: double;
    FWidth: double;
    procedure SetHeight(const Value: double);
    procedure SetMinX(const Value: double);
    procedure SetMinY(const Value: double);
    procedure SetWidth(const Value: double);
  protected
    procedure UpdateTransform; virtual; abstract;
    function UVMapping: TpgMatrix;
  public
    constructor Create; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    property IsValid: boolean read FIsValid write FIsValid;
    // MinX of the viewbox mapping into the uv projection
    property MinX: double read FMinX write SetMinX;
    // MinY of the viewbox mapping into the projection
    property MinY: double read FMinY write SetMinY;
    // Width of the viewbox mapping into the projection
    property Width: double read FWidth write SetWidth;
    // Height of the viewbox mapping into the projection
    property Height: double read FHeight write SetHeight;
  end;

  // Projective transformation of domain into a 4-point area described by
  // the points Points[0] through Points[3].
  TpgProjectiveTransform = class(TpgUVTransform)
  private
    FPoints: array[0..3] of TpgPoint;
    FMatrix: TpgMatrix3x3;
    FInverseMatrix: TpgMatrix3x3;
    function GetPoints(Index: integer): PpgPoint;
  protected
    procedure UpdateTransform; override;
  public
    procedure Assign(ASource: TPersistent); override;
    function Invert: boolean; override;
    function GetPixelScale(ADirection: TpgCartesianDirection): double; override;
    function Transform(const APoint: TpgPoint): TpgPoint; override;
    function InverseTransform(const APoint: TpgPoint; var AInverse: TpgPoint): boolean; override;
    function IsLinear: boolean; override;
    procedure TransformPoints(ASource, ADest: PpgPoint; ACount: integer); override;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure SetPoint(Index: integer; const Value: TpgPoint);
    // Array of 4 points, P[0] through P[3], describing the 4 corner points of
    // the projection. P[0] corresponds to topleft, P[1] to topright, P[2] to
    // botttomleft, and P[3] to bottomright.
    property Points[Index: integer]: PpgPoint read GetPoints;
  end;

  TpgCurvedTransform = class(TpgProjectiveTransform)
  private
    FDeltaY: double;
    FDeltaX: double;
    FInverted: boolean;
    procedure SetDeltaX(const Value: double);
    procedure SetDeltaY(const Value: double);
  protected
    function CurveTransform(const APoint: TpgPoint): TpgPoint; virtual;
    function InverseCurveTransform(const APoint: TpgPoint): TpgPoint; virtual;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    function Invert: boolean; override;
    function InverseTransform(const APoint: TpgPoint; var AInverse: TpgPoint): boolean; override;
    function Transform(const APoint: TpgPoint): TpgPoint; override;
    function IsLinear: boolean; override;
    procedure TransformPoints(ASource, ADest: PpgPoint; ACount: integer); override;
    // Additional offset in X halfway down the height of the box, given in box
    // coordinates
    property DeltaX: double read FDeltaX write SetDeltaX;
    // Additional offset in Y halway down the width of the box, given in box
    // coordinates
    property DeltaY: double read FDeltaY write SetDeltaY;
  end;

procedure InvertMatrix3x3(var M: TpgMatrix3x3);
function Matrix2x3To3x3(const M: TpgMatrix): TpgMatrix3x3;
function MatrixMultiply3x3(const M1, M2: TpgMatrix3x3): TpgMatrix3x3;

const
  cIdentityMatrix3x3: TpgMatrix3x3 = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

implementation

function Det2x2(a1, a2, b1, b2: double): double; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function Det3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3: double): double; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint3x3(var M: TpgMatrix3x3);
var
  a1, a2, a3: double;
  b1, b2, b3: double;
  c1, c2, c3: double;
begin
  a1 := M[0, 0]; a2:= M[1, 0]; a3 := M[2, 0];
  b1 := M[0, 1]; b2:= M[1, 1]; b3 := M[2, 1];
  c1 := M[0, 2]; c2:= M[1, 2]; c3 := M[2, 2];

  M[0, 0] :=   Det2x2(b2, b3, c2, c3);
  M[1, 0] := - Det2x2(a2, a3, c2, c3);
  M[2, 0] :=   Det2x2(a2, a3, b2, b3);

  M[0, 1] := - Det2x2(b1, b3, c1, c3);
  M[1, 1] :=   Det2x2(a1, a3, c1, c3);
  M[2, 1] := - Det2x2(a1, a3, b1, b3);

  M[0, 2] :=   Det2x2(b1, b2, c1, c2);
  M[1, 2] := - Det2x2(a1, a2, c1, c2);
  M[2, 2] :=   Det2x2(a1, a2, b1, b2);
end;

function Determinant3x3(const M: TpgMatrix3x3): double;
begin
  Result := Det3x3(M[0, 0], M[0, 1], M[0, 2],
                   M[1, 0], M[1, 1], M[1, 2],
                   M[2, 0], M[2, 1], M[2, 2]);
end;

procedure Scale3x3(var M: TpgMatrix3x3; Factor: double);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;

procedure InvertMatrix3x3(var M: TpgMatrix3x3);
var
  Det: double;
begin
  Det := Determinant3x3(M);
  if Abs(Det) < 1E-5 then M := cIdentityMatrix3x3
  else
  begin
    Adjoint3x3(M);
    Scale3x3(M, 1 / Det);
  end;
end;

function Matrix2x3To3x3(const M: TpgMatrix): TpgMatrix3x3;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0, 0] := M.A;
  Result[1, 0] := M.B;
  Result[0, 1] := M.C;
  Result[1, 1] := M.D;
  Result[0, 2] := M.E;
  Result[1, 2] := M.F;
  Result[2, 2] := 1;
end;

function MatrixMultiply3x3(const M1, M2: TpgMatrix3x3): TpgMatrix3x3;
var
  r, c: integer;
begin
  for r := 0 to 2 do
    for c := 0 to 2 do
      Result[r, c] :=
        M1[r, 0] * M2[0, c] +
        M1[r, 1] * M2[1, c] +
        M1[r, 2] * M2[2, c];
end;

{ TpgUVTransform }

procedure TpgUVTransform.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TpgUVTransform then begin
    FMinX := TpgUVTransform(ASource).FMinX;
    FMinY := TpgUVTransform(ASource).FMinY;
    FWidth := TpgUVTransform(ASource).FWidth;
    FHeight := TpgUVTransform(ASource).FHeight;
    FIsValid := False;
  end;
end;

constructor TpgUVTransform.Create;
begin
  inherited Create;
  FWidth := 1;
  FHeight := 1;
end;

procedure TpgUVTransform.Read(AStorage: TpgStorage);
begin
  FMinX := AStorage.ReadFloat;
  FMinY := AStorage.ReadFloat;
  FWidth := AStorage.ReadFloat;
  FHeight := AStorage.ReadFloat;
end;

procedure TpgUVTransform.SetHeight(const Value: double);
begin
  FHeight := Value;
  FIsValid := False;
end;

procedure TpgUVTransform.SetMinX(const Value: double);
begin
  FMinX := Value;
  FIsValid := False;
end;

procedure TpgUVTransform.SetMinY(const Value: double);
begin
  FMinY := Value;
  FIsValid := False;
end;

procedure TpgUVTransform.SetWidth(const Value: double);
begin
  FWidth := Value;
  FIsValid := False;
end;

function TpgUVTransform.UVMapping: TpgMatrix;
var
  T: TpgAffineTransform;
begin
  // UV matrix
  T := BuildViewboxTransform(0, 0, 1, 1, FMinX, FMinY, FWidth, FHeight, paNone, msUnknown);
  Result := T.Matrix^;
  T.Free;
end;

procedure TpgUVTransform.Write(AStorage: TpgStorage);
begin
  AStorage.WriteFloat(FMinX);
  AStorage.WriteFloat(FMinY);
  AStorage.WriteFloat(FWidth);
  AStorage.WriteFloat(FHeight);
end;

{ TpgProjectiveTransform }

procedure TpgProjectiveTransform.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TpgProjectiveTransform then
    Move(TpgProjectiveTransform(ASource).FPoints[0], FPoints[0], 4 * SizeOf(TpgPoint));
end;

function TpgProjectiveTransform.GetPixelScale(ADirection: TpgCartesianDirection): double;
var
  i: integer;
  X, Y: double;
  P: array[0..2] of TpgPoint;
begin
  if not FIsValid then UpdateTransform;
  // Transform 3 points in center of FOV
  P[0] := pgPoint(FMinX + FWidth / 2, FMinY + Height / 2);
  P[1] := P[0]; P[1].X := P[1].X + 0.001;
  P[2] := P[0]; P[2].Y := P[2].Y + 0.001;
  for i := 0 to 2 do
    P[i] := Transform(P[i]);

  X := (P[1].X - P[0].X) * 1000;
  Y := (P[2].Y - P[0].Y) * 1000;

  case ADirection of
  cdHorizontal: Result := abs(X);
  cdVertical:   Result := abs(Y);
  cdUnknown:    Result := sqrt(abs(X * Y));
  else
    Result := 1;
  end;
end;

function TpgProjectiveTransform.GetPoints(Index: integer): PpgPoint;
begin
  Result := @FPoints[Index];
  FIsValid := False;
end;

function TpgProjectiveTransform.InverseTransform(const APoint: TpgPoint; var AInverse: TpgPoint): boolean;
var
  X, Y, Z: double;
begin
  Result := False;
  if not FIsValid then UpdateTransform;

  X := APoint.X; Y := APoint.Y;
  Z := FInverseMatrix[2, 0] * X + FInverseMatrix[2, 1] * Y + FInverseMatrix[2, 2];
  if Z = 0 then Exit
  else if Z = 1 then begin

    AInverse.X := FInverseMatrix[0, 0] * X + FInverseMatrix[0, 1] * Y + FInverseMatrix[0, 2];
    AInverse.Y := FInverseMatrix[1, 0] * X + FInverseMatrix[1, 1] * Y + FInverseMatrix[1, 2];

  end else begin

    Z := 1 / Z;
    AInverse.X := (FInverseMatrix[0, 0] * X + FInverseMatrix[0, 1] * Y + FInverseMatrix[0, 2]) * Z;
    AInverse.Y := (FInverseMatrix[1, 0] * X + FInverseMatrix[1, 1] * Y + FInverseMatrix[1, 2]) * Z;

  end;
  Result := True;
end;

function TpgProjectiveTransform.Invert: boolean;
var
  Temp: TpgMatrix3x3;
begin
  if not FIsValid then UpdateTransform;
  Temp := FMatrix;
  FMatrix := FInverseMatrix;
  FInverseMatrix := Temp;
  Result := True;
end;

function TpgProjectiveTransform.IsLinear: boolean;
begin
  Result := (FMatrix[2, 0] = 0) and (FMatrix[2, 1] = 0);
end;

procedure TpgProjectiveTransform.Read(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  for i := 0 to 3 do begin
    FPoints[i].X := AStorage.ReadFloat;
    FPoints[i].Y := AStorage.ReadFloat;
  end;
end;

procedure TpgProjectiveTransform.SetPoint(Index: integer; const Value: TpgPoint);
begin
  FPoints[Index] := Value;
end;

function TpgProjectiveTransform.Transform(const APoint: TpgPoint): TpgPoint;
var
  X, Y, Z: double;
begin
  if not FIsValid then UpdateTransform;

  X := APoint.X; Y := APoint.Y;

  if IsLinear then begin

    Result.X := FMatrix[0, 0] * X + FMatrix[0, 1] * Y + FMatrix[0, 2];
    Result.Y := FMatrix[1, 0] * X + FMatrix[1, 1] * Y + FMatrix[1, 2];

  end else begin

    Z := FMatrix[2, 0] * X + FMatrix[2, 1] * Y + FMatrix[2, 2];
    if Z = 0 then Exit;
    Z := 1 / Z;

    Result.X := (FMatrix[0, 0] * X + FMatrix[0, 1] * Y + FMatrix[0, 2]) * Z;
    Result.Y := (FMatrix[1, 0] * X + FMatrix[1, 1] * Y + FMatrix[1, 2]) * Z;

  end;
end;

procedure TpgProjectiveTransform.TransformPoints(ASource, ADest: PpgPoint; ACount: integer);
var
  i: integer;
  X, Y, Z, M00, M01, M02, M10, M11, M12, M20, M21, M22: double;
begin
  if not FIsValid then UpdateTransform;

  // Cache matrix elements
  M00 := FMatrix[0, 0];
  M01 := FMatrix[0, 1];
  M02 := FMatrix[0, 2];
  M10 := FMatrix[1, 0];
  M11 := FMatrix[1, 1];
  M12 := FMatrix[1, 2];

  if IsLinear then begin

    for i := 0 to ACount - 1 do begin
      X := ASource.X; Y := ASource.Y;
      ADest.X := M00 * X + M01 * Y + M02;
      ADest.Y := M10 * X + M11 * Y + M12;
      inc(ASource);
      inc(ADest);
    end;

  end else begin

    M20 := FMatrix[2, 0];
    M21 := FMatrix[2, 1];
    M22 := FMatrix[2, 2];

    for i := 0 to ACount - 1 do begin
      X := ASource.X; Y := ASource.Y;
      Z := M20 * X + M21 * Y + M22;
      if Z = 0 then continue;
      Z := 1 / Z;
      ADest.X := (M00 * X + M01 * Y + M02) * Z;
      ADest.Y := (M10 * X + M11 * Y + M12) * Z;
      inc(ASource);
      inc(ADest);
    end;

  end;
end;

procedure TpgProjectiveTransform.UpdateTransform;
var
  Dx1, Dx2, Px, Dy1, Dy2, Py: double;
  g, h, k: double;
  R: TpgMatrix3x3;
  M: TpgMatrix;
begin
  Px  := FPoints[0].X - FPoints[1].X + FPoints[2].X - FPoints[3].X;
  Py  := FPoints[0].Y - FPoints[1].Y + FPoints[2].Y - FPoints[3].Y;

  if (abs(px) < 1E-12) and (abs(py) < 1E-12) then begin

    // affine mapping
    FMatrix[0, 0] := FPoints[1].X - FPoints[0].X;
    FMatrix[0, 1] := FPoints[2].X - FPoints[1].X;
    FMatrix[0, 2] := FPoints[0].X;

    FMatrix[1, 0] := FPoints[1].Y - FPoints[0].Y;
    FMatrix[1, 1] := FPoints[2].Y - FPoints[1].Y;
    FMatrix[1, 2] := FPoints[0].Y;

    FMatrix[2, 0] := 0;
    FMatrix[2, 1] := 0;
    FMatrix[2, 2] := 1;

  end else begin

    // projective mapping
    Dx1 := FPoints[1].X - FPoints[2].X;
    Dx2 := FPoints[3].X - FPoints[2].X;
    Dy1 := FPoints[1].Y - FPoints[2].Y;
    Dy2 := FPoints[3].Y - FPoints[2].Y;
    k := Dx1 * Dy2 - Dx2 * Dy1;
    if k <> 0 then begin

      g := (Px * Dy2 - Py * Dx2) / k;
      h := (Dx1 * Py - Dy1 * Px) / k;

      FMatrix[0, 0] := FPoints[1].X - FPoints[0].X + g * FPoints[1].X;
      FMatrix[0, 1] := FPoints[3].X - FPoints[0].X + h * FPoints[3].X;
      FMatrix[0, 2] := FPoints[0].X;

      FMatrix[1, 0] := FPoints[1].Y - FPoints[0].Y + g * FPoints[1].Y;
      FMatrix[1, 1] := FPoints[3].Y - FPoints[0].Y + h * FPoints[3].Y;
      FMatrix[1, 2] := FPoints[0].Y;

      FMatrix[2, 0] := g;
      FMatrix[2, 1] := h;
      FMatrix[2, 2] := 1;

    end else begin

      FillChar(FMatrix, SizeOf(FMatrix), 0);

    end;
  end;

  // UV matrix
  M := UVMapping;
  R := Matrix2x3To3x3(M);
  FMatrix := MatrixMultiply3x3(FMatrix, R);

  FInverseMatrix := FMatrix;
  InvertMatrix3x3(FInverseMatrix);

  FIsValid := True;
end;

procedure TpgProjectiveTransform.Write(AStorage: TpgStorage);
var
  i: integer;
begin
  inherited;
  for i := 0 to 3 do begin
    AStorage.WriteFloat(FPoints[i].X);
    AStorage.WriteFloat(FPoints[i].Y);
  end;
end;

{ TpgCurvedTransform }

procedure TpgCurvedTransform.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TpgCurvedTransform then begin
    FDeltaX := TpgCurvedTransform(ASource).FDeltaX;
    FDeltaY := TpgCurvedTransform(ASource).FDeltaY;
  end;
end;

function TpgCurvedTransform.CurveTransform(const APoint: TpgPoint): TpgPoint;
var
  u, v: double;
begin
  if (FWidth = 0) or (FHeight = 0) or ((FDeltaX = 0) and (FDeltaY = 0))then begin
    Result := APoint;
    exit;
  end;
  if FDeltaX = 0 then
    Result.X := APoint.X
  else begin
    v := (APoint.Y - FMinY) / FHeight;
    Result.X := APoint.X + FDeltaX * (1 - 4 * sqr(v - 0.5));
  end;
  if FDeltaY = 0 then
    Result.Y := APoint.Y
  else begin
    u := (APoint.X - FMinX) / FWidth;
    Result.Y := APoint.Y + FDeltaY * (1 - 4 * sqr(u - 0.5));
  end;
end;

function TpgCurvedTransform.InverseCurveTransform(
  const APoint: TpgPoint): TpgPoint;
var
  u, v: double;
begin
  if (FWidth = 0) or (FHeight = 0) or ((FDeltaX = 0) and (FDeltaY = 0))then begin
    Result := APoint;
    exit;
  end;
  if FDeltaX = 0 then
    Result.X := APoint.X
  else begin
    v := (APoint.Y - FMinY) / FHeight;
    Result.X := APoint.X - FDeltaX * (1 - 4 * sqr(v - 0.5));
  end;
  if FDeltaY = 0 then
    Result.Y := APoint.Y
  else begin
    u := (APoint.X - FMinX) / FWidth;
    Result.Y := APoint.Y - FDeltaY * (1 - 4 * sqr(u - 0.5));
  end;
end;

function TpgCurvedTransform.InverseTransform(const APoint: TpgPoint; var AInverse: TpgPoint): boolean;
begin
  Result := inherited InverseTransform(APoint, AInverse);
  AInverse := InverseCurveTransform(AInverse);
end;

function TpgCurvedTransform.Invert: boolean;
begin
  FInverted := not FInverted;
  Result := True;
end;

function TpgCurvedTransform.IsLinear: boolean;
begin
  Result := inherited IsLinear;
  if not Result then exit;
  Result := (FDeltaX = 0) and (FDeltaY = 0);
end;

procedure TpgCurvedTransform.Read(AStorage: TpgStorage);
begin
  inherited;
  FDeltaX := AStorage.ReadFloat;
  FDeltaY := AStorage.ReadFloat;
end;

procedure TpgCurvedTransform.SetDeltaX(const Value: double);
begin
  FDeltaX := Value;
  FIsValid := False;
end;

procedure TpgCurvedTransform.SetDeltaY(const Value: double);
begin
  FDeltaY := Value;
  FIsValid := False;
end;

function TpgCurvedTransform.Transform(const APoint: TpgPoint): TpgPoint;
begin
  if FInverted then
    InverseTransform(APoint, Result)
  else
    Result := inherited Transform(CurveTransform(APoint));
end;

procedure TpgCurvedTransform.TransformPoints(ASource, ADest: PpgPoint; ACount: integer);
var
  i: integer;
begin
  if (not FInverted) and
   ((FWidth = 0) or (FHeight = 0) or ((FDeltaX = 0) and (FDeltaY = 0))) then begin
    inherited TransformPoints(ASource, ADest, ACount);
    exit;
  end;
  for i := 0 to ACount - 1 do begin
    ADest^ := Transform(ASource^);
    inc(ASource);
    inc(ADest);
  end;
end;

procedure TpgCurvedTransform.Write(AStorage: TpgStorage);
begin
  inherited;
  AStorage.WriteFloat(FDeltaX);
  AStorage.WriteFloat(FDeltaY);
end;

initialization

  RegisterTransform(tiProjective, TpgProjectiveTransform, 'Projective');
  RegisterTransform(tiCurved, TpgCurvedTransform, 'Curved');


end.
