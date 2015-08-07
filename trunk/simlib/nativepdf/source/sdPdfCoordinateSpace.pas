{ unit sdPdfCoordinateSpace

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements coordinate spaces, matrices and transforms
  chapter 4.2: Coordinate systems

  Author: Nils Haeck M.Sc.

  Changes:
    13Jan2004 - Created

  copyright (c) 2004 by Simdesign B.V.

}
unit sdPdfCoordinateSpace;

interface

uses
  Classes, SysUtils, sdPdfObjects;

type

  // A 2x3 matrix structure used for 2D transforms, how it is described in the
  // PDF reference.
  // A matrix multiplication is written as:
  //                         [a b 0]
  // [Xb Yb 1] = [Xa Ya 1] * [c d 0]
  //                         [e f 1]
  // Use the Elements field to access by array (A=0, B=1, ..., F=5)
  // A matrix can be created from a TPdfArray with PdfMatrixFromArray()
  TPdfMatrixStruct = packed record
    case Integer of
    0: (A: PdfFloat;
        B: PdfFloat;
        C: PdfFloat;
        D: PdfFloat;
        E: PdfFloat;
        F: PdfFloat);
    1: (Elements: array[0..5] of PdfFloat);
  end;

  TPdfPoint = packed record
    X, Y: PdfFloat;
  end;

const
  // Identity matrix
  cPdfIdentityMatrix: TPdfMatrixStruct =
    (A: 1; B: 0; C: 0; D: 1; E: 0; F: 0);

function PdfMatrixFromArray(MatrixArray: TPdfArray): TPdfMatrixStruct;
function PdfMatrixFromOperator(Operator: TPdfOperator): TPdfMatrixStruct;

// Create a matrix with elements A through F
function PdfMatrix(A, B, C, D, E, F: PdfFloat): TPdfMatrixStruct;

// create a vector with elements X and Y
function PdfPoint(X, Y: PdfFloat): TPdfPoint;

// Multipy matrices A and B (Result = A x B)
function PdfMatrixMultiply(const A, B: TPdfMatrixStruct): TPdfMatrixStruct; overload;

// Multiply matrices A, B and C (Result = A x B x C)
function PdfMatrixMultiply(const A, B, C: TPdfMatrixStruct): TPdfMatrixStruct; overload;

// Multiply a position vector with a matrix: Result = V x M  (V = [X Y 1])
function PdfMatrixMulPosVector(const V: TPdfPoint; const M: TPdfMatrixStruct): TPdfPoint;

// Add translation to transformation
function PdfMatrixTranslate(const M: TPdfMatrixStruct; Tx, Ty: PdfFloat): TPdfMatrixStruct;

// Convert to a string for debugging
function PdfMatrixToString(const M: TPdfMatrixStruct): string;

implementation

function PdfMatrixFromArray(MatrixArray: TPdfArray): TPdfMatrixStruct;
var
  i: integer;
begin
  if not assigned(MatrixArray) then
    Result := cPdfIdentityMatrix
  else
    for i := 0 to 5 do
      Result.Elements[i] := MatrixArray.Elements[i].AsNumber;
end;

function PdfMatrixFromOperator(Operator: TPdfOperator): TPdfMatrixStruct;
var
  i: integer;
begin
  if not assigned(Operator) then
    Result := cPdfIdentityMatrix
  else
    for i := 0 to 5 do
      Result.Elements[i] := Operator.Args[i].AsNumber;
end;

function PdfMatrix(A, B, C, D, E, F: PdfFloat): TPdfMatrixStruct;
// Create a matrix with elements A through F
begin
  Result.A := A; Result.B := B; Result.C := C;
  Result.D := D; Result.E := E; Result.F := F;
end;

function PdfPoint(X, Y: PdfFloat): TPdfPoint;
// create a vector with elements X and Y
begin
  Result.X := X;
  Result.Y := Y;
end;

function PdfMatrixMultiply(const A, B: TPdfMatrixStruct): TPdfMatrixStruct;
begin
  Result.A := A.A * B.A + A.B * B.C;
  Result.B := A.A * B.B + A.B * B.D;
  Result.C := A.C * B.A + A.D * B.C;
  Result.D := A.C * B.B + A.D * B.D;
  Result.E := A.E * B.A + A.F * B.C + B.E;
  Result.F := A.E * B.B + A.F * B.D + B.F;
end;

function PdfMatrixMultiply(const A, B, C: TPdfMatrixStruct): TPdfMatrixStruct; overload;
begin
  Result := PdfMatrixMultiply(A, PdfMatrixMultiply(B, C));
end;

function PdfMatrixMulPosVector(const V: TPdfPoint; const M: TPdfMatrixStruct): TPdfPoint;
begin
  Result.X := V.X * M.A + V.Y * M.C + M.E;
  Result.Y := V.X * M.B + V.Y * M.D + M.F;
end;

function PdfMatrixTranslate(const M: TPdfMatrixStruct; Tx, Ty: PdfFloat): TPdfMatrixStruct;
// Translate matrix over Tx and Ty
begin
  Result := PdfMatrixMultiply(PdfMatrix(1, 0, 0, 1, Tx, Ty), M);
end;

function PdfMatrixToString(const M: TPdfMatrixStruct): string;
var
  i: integer;
begin
  Result := '[';
  for i := 0 to 5 do begin
    Result := Result + Format('%5.3f', [M.Elements[i]]);
    if i < 5 then Result := Result + ',';
  end;
  Result := Result + ']';
end;

end.
