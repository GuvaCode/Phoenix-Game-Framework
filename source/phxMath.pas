////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxMath;
//< Vector and matrix math

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  Math,

  phxTypes;

const MinSingle   =  -1.5e-45;
const MaxSingle   =   3.4e+38;

// Swap two values
procedure Swap(var A: Single; var B: Single);

////////////////////////////////////////////////////////////////////////////////
// Trigometric                                                                //
////////////////////////////////////////////////////////////////////////////////

const DEG_TO_RAD = PI / 180;
const RAD_TO_DEG = 180 / PI;
const PIDiv180  =  0.017453292519943295769236907684886;
const _180DivPI = 57.295779513082320876798154814105000;


// Fast, table lookup versions of sinus and cosinus
function Sin256(I: Integer): Single; overload;
function Sin256(I: Single ): Single; overload;
function Cos256(I: Integer): Single; overload;
function Cos256(I: Single ): Single; overload;

////////////////////////////////////////////////////////////////////////////////
// 2D Vector Math                                                             //
////////////////////////////////////////////////////////////////////////////////

// Get the magnitude (length) of a vector
function VectorMagnitude(const Vector: TVector2i ): Single; overload;
function VectorMagnitude(const Vector: TVector2f ): Single; overload;

// The squared magnitude
function VectorMagnitudeSqr(const Vector: TVector2i): Single;   overload;
function VectorMagnitudeSqr(const Vector: TVector2f): Single;   overload;

// Normalize a vector
function VectorNormalize(const Vector: TVector2i ): TVector2i ; overload;
function VectorNormalize(const Vector: TVector2f ): TVector2f ; overload;

// Negate a vector
function VectorNegate(const Vector: TVector2i ): TVector2i; overload;
function VectorNegate(const Vector: TVector2f ): TVector2f; overload;

// Vector addition
function VectorAdd(const V1, V2: TVector2i ): TVector2i; overload;
function VectorAdd(const V1, V2: TVector2f ): TVector2f; overload;

// Vector subtracting
function VectorSub(const V1, V2: TVector2i ): TVector2i; overload;
function VectorSub(const V1, V2: TVector2f ): TVector2f; overload;

// Multiplication
function VectorMul(const V1: TVector2i ; Scalar: Single): TVector2i; overload;
function VectorMul(const V1: TVector2f ; Scalar: Single): TVector2f; overload;

// Division
function VectorDiv(const V1: TVector2i ; Scalar: Single): TVector2i; overload;
function VectorDiv(const V1: TVector2f ; Scalar: Single): TVector2f; overload;

// Dot product
function VectorDot(const V1, V2: TVector2i ): Single; overload;
function VectorDot(const V1, V2: TVector2f ): Single; overload;

// Cross product
function VectorCross(const V1, V2: TVector2i ): Single; overload;
function VectorCross(const V1, V2: TVector2f ): Single; overload;

// Vector reflection, V is the vector to reflect, N is the wall normal
function VectorReflect(const V, N: TVector2f ): TVector2f; overload;
function VectorReflect(const V, N: TVector2i ): TVector2i; overload;

////////////////////////////////////////////////////////////////////////////////
// 3D Vector Math                                                                //
////////////////////////////////////////////////////////////////////////////////

// Magnitude of a vector
function VectorMagnitude(const Vector: TVector3i ): Single;   overload;
function VectorMagnitude(const Vector: TVector3f ): Single;   overload;

// The squared magnitude
function VectorMagnitudeSqr(const Vector: TVector3i): Single;   overload;
function VectorMagnitudeSqr(const Vector: TVector3f): Single;   overload;

// Normalize a vector
function VectorNormalize(const Vector: TVector3i ): TVector3i ; overload;
function VectorNormalize(const Vector: TVector3f ): TVector3f ; overload;

// Negate a vector
function VectorNegate(const Vector: TVector3i ): TVector3i; overload;
function VectorNegate(const Vector: TVector3f ): TVector3f; overload;

// Vector addition
function VectorAdd(const V1, V2: TVector3i ): TVector3i; overload;
function VectorAdd(const V1, V2: TVector3f ): TVector3f; overload;

// Vector subtracting
function VectorSub(const V1, V2: TVector3i ): TVector3i; overload;
function VectorSub(const V1, V2: TVector3f ): TVector3f; overload;

// Multiplication
function VectorMul(const V1: TVector3i ; Scalar: Single): TVector3i; overload;
function VectorMul(const V1: TVector3f ; Scalar: Single): TVector3f; overload;

// Division
function VectorDiv(const V1: TVector3i ; Scalar: Single): TVector3i; overload;
function VectorDiv(const V1: TVector3f ; Scalar: Single): TVector3f; overload;

// Dot product
function VectorDot(const V1, V2: TVector3i ): Single; overload;
function VectorDot(const V1, V2: TVector3f ): Single; overload;

// Cross product
function VectorCross(const V1, V2: TVector3i ): TVector3i; overload;
function VectorCross(const V1, V2: TVector3f ): TVector3f; overload;

// Vector reflection, V is the vector to reflect, N is the wall normal
function VectorReflect(const V, N: TVector3f ): TVector3f; overload;
function VectorReflect(const V, N: TVector3i ): TVector3i; overload;

////////////////////////////////////////////////////////////////////////////////
// Matrix Math                                                                //
////////////////////////////////////////////////////////////////////////////////


function Matrix_CreateFrustum(Left, Right, Bottom, Top, zNear, zFar: Single): TMatrix4f;
// Should be totally same as glOrtho
function Matrix_CreateOrthographic(Left, Right, Bottom, Top, zNear, zFar: Single): TMatrix4f;
function Matrix_CreateOrthographicLH(Width, Height, zNear, zFar: Single): TMatrix4f;
// Should be totally same as gluPerspective
function Matrix_CreatePerspective(Fov, Aspect, zNear, zFar: Single): TMatrix4f;

// Create a identity matrix
function Matrix_CreateIdentity: TMatrix4f;
// Create an translation matrix
function Matrix_CreateTranslation(const X, Y, Z: Single): TMatrix4f; overload;
// Create an translation matrix
function Matrix_CreateTranslation(const Translation: TVector3f): TMatrix4f; overload;
// Create an translation matrix
function Matrix_CreateTranslation(const Translation: TVector2f): TMatrix4f; overload;
// Creates a rotation matrix
function Matrix_CreateRotation(const Angles: TVector3f): TMatrix4f; overload;
// Creates a rotation matrix
function Matrix_CreateRotation(const AngleX, AngleY, AngleZ: Single): TMatrix4f; overload;
// Creates a matrix for rotation around the X-axis.
function Matrix_CreateRotationX(const AngleX: Single): TMatrix4f; overload;
// Creates a matrix for rotation around the Y-axis.
function Matrix_CreateRotationY(const AngleY: Single): TMatrix4f; overload;
// Creates a matrix for rotation around the Z-axis.
function Matrix_CreateRotationZ(const AngleZ: Single): TMatrix4f; overload;
// Creates a rotation matrix
function Matrix_CreateRotation(const Angle: Single; const Axis : TVector3f): TMatrix4f; overload;
// Create an scale matrix
function Matrix_CreateScale(const X, Y, Z: Single): TMatrix4f; overload;
// Create an scale matrix
function Matrix_CreateScale(const Scale : TVector3f): TMatrix4f; overload;
// Create an scale matrix
function Matrix_CreateScale(const Scale : TVector2f): TMatrix4f; overload;

// Multiply two matrices
function Matrix_Multiply(const M1, M2: TMatrix4f ): TMatrix4f; overload;
// Multiply three matrices
function Matrix_Multiply(const M1, M2, M3: TMatrix4f ): TMatrix4f; overload;
// Multiply four matrices
function Matrix_Multiply(const M1, M2, M3, M4: TMatrix4f ): TMatrix4f; overload;

// Transform a vector by a matrix
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f; overload;
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector2i): TVector2i; overload;
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f; overload;
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector4f): TVector4f; overload;

function Matrix_Transform(const Matrix: TMatrix4f; const X,Y,Z: Single): TVector3f; overload;


// Inverse transform a vector by a matrix
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f; overload;
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector2i): TVector2i; overload;
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f; overload;
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector4f): TVector4f; overload;

// Rotate a vector by a matrix
function Matrix_Rotate   (const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f; overload;
function Matrix_Rotate   (const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f; overload;

function Matrix_RotateInv(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f; overload;
function Matrix_RotateInv(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f; overload;



// Get elements from a matrix
function Matrix_GetForward    (const Matrix: TMatrix4f): TVector3f;
function Matrix_GetUp         (const Matrix: TMatrix4f): TVector3f;
function Matrix_GetRight      (const Matrix: TMatrix4f): TVector3f;
function Matrix_GetTranslation(const Matrix: TMatrix4f): TVector3f;

function Matrix_GetRotationX(const Matrix: TMatrix4f): Single;
function Matrix_GetRotationY(const Matrix: TMatrix4f): Single;
function Matrix_GetRotationZ(const Matrix: TMatrix4f): Single;


// Converts a 4x4 matrix to a 3x3 matrix
function Matrix3f(const Matrix: TMatrix4f): TMatrix3f;
// Divide a matrix with a scalar
function Matrix_Div(const Matrix: TMatrix3f; const Scalar: Single): TMatrix3f;
// Calculates the inverse transpose of a matrix
function Matrix_InverseTranspose(const Matrix: TMatrix3f): TMatrix3f;

function PointInRect(const P: TVector2f; const R: TRectf): Boolean; overload;
function PointInRect(const P: TVector2i; const R: TRecti): Boolean; overload;
function PointInRect(const X,Y: Single ; const R: TRectf): Boolean; overload;
function PointInRect(const X,Y: Integer; const R: TRecti): Boolean; overload;

function RectInRect(const A, B: TRectf): Boolean; overload;
function RectInRect(const A, B: TRecti): Boolean; overload;

function OverlapRect(const A, B: TRectf): Boolean; overload;
function OverlapRect(const A, B: TRecti): Boolean; overload;

// Returns true if A is contained inside B
function ContainsRect(const A, B: TRectf): Boolean; overload;
function ContainsRect(const A, B: TRecti): Boolean; overload;

////////////////////////////////////////////////////////////////////////////////
// Collision testing                                                          //
////////////////////////////////////////////////////////////////////////////////

const
  // Constant determing the threshold velocity when two objects are in standstill
  COLLISION_VELOCITY_THRESHOLD = 1.0E-7;

type

// Generic collision parameter
//------------------------------------------------------------------------------
 TCollisionParameter = record
  // Time of the collision
  Time  : Single;
  // Normal of the collision
  Normal: TVector2f;
end;

//------------------------------------------------------------------------------
function IntervalIntersect(const A, B: TRangef; const Axis, Displacement: TVector2f; var First, Last, MTD: TCollisionParameter): Boolean;  overload;




implementation

// Swaps two values
//------------------------------------------------------------------------------
procedure Swap(var A: Single; var B: Single);
var T: Single;
begin
  T:= A;
  A:= B;
  B:= T;
end;

////////////////////////////////////////////////////////////////////////////////
// Trigometric                                                                //
////////////////////////////////////////////////////////////////////////////////

var SinTable : Array[0..255] of Single;
var CosTable : Array[0..255] of Single;

//------------------------------------------------------------------------------
procedure InitSinCosTable;
var Counter: Integer;
var Angle: Single;
var Delta: Single;
begin
  Angle:=0;
  Delta:= PI / 128;
  for Counter:= 0 to 255 do begin
    SinTable[Counter] := Sin( Angle );
    CosTable[Counter] := Cos( Angle);

    Angle:= Angle +  Delta;
  end;
end;

//------------------------------------------------------------------------------
function Cos256(I: Integer): Single;
begin
  Result := CosTable[I and 255];
end;

//------------------------------------------------------------------------------
function Sin256(I: Integer): Single;
begin
  Result := SinTable[I and 255];
end;

//------------------------------------------------------------------------------
function Cos256(I: Single): Single;
begin
  Result := CosTable[Trunc(I) and 255];
end;

//------------------------------------------------------------------------------
function Sin256(I: Single): Single;
begin
  Result := SinTable[Trunc(I) and 255];
end;


////////////////////////////////////////////////////////////////////////////////
// 2D Vector Math                                                             //
////////////////////////////////////////////////////////////////////////////////

// Magnitude
//------------------------------------------------------------------------------
function VectorMagnitude(const Vector: TVector2i ): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y);
end;

//------------------------------------------------------------------------------
function VectorMagnitude(const Vector: TVector2f): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y);
end;

// MagnitudeSqr
//------------------------------------------------------------------------------
function VectorMagnitudeSqr(const Vector: TVector2i ): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y);
end;

//------------------------------------------------------------------------------
function VectorMagnitudeSqr(const Vector: TVector2f): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y);
end;


// Normalize a vector
//------------------------------------------------------------------------------
function VectorNormalize(const Vector: TVector2i ): TVector2i ;
var AMagnitude: Single;
begin
  AMagnitude:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) );

  if AMagnitude = 0 then
  begin
    Result:= Vector;
  end else
  begin
    Result.X:=Trunc(Vector.X / AMagnitude);
    Result.Y:=Trunc(Vector.Y / AMagnitude);
  end;
end;

//------------------------------------------------------------------------------
function VectorNormalize(const Vector: TVector2f ): TVector2f ;
var AMagnitude: Single;
begin
  AMagnitude:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) );

  if AMagnitude = 0 then
  begin
    Result:= Vector;
  end else
  begin
    Result.X:=Vector.X / AMagnitude;
    Result.Y:=Vector.Y / AMagnitude;
  end;
end;

// Negate a vector
//------------------------------------------------------------------------------
function VectorNegate(const Vector: TVector2i ): TVector2i;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
end;

//------------------------------------------------------------------------------
function VectorNegate(const Vector: TVector2f ): TVector2f;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
end;

// Vector addition
//------------------------------------------------------------------------------
function VectorAdd(const V1, V2: TVector2i ): TVector2i;
begin
  Result.X:= V1.X + V2.X;
  Result.Y:= V1.Y + V2.Y;
end;

//------------------------------------------------------------------------------
function VectorAdd(const V1, V2: TVector2f ): TVector2f;
begin
  Result.X:= V1.X + V2.X;
  Result.Y:= V1.Y + V2.Y;
end;

// Vector subtracting
//------------------------------------------------------------------------------
function VectorSub(const V1, V2: TVector2i ): TVector2i;
begin
  Result.X:= V1.X - V2.X;
  Result.Y:= V1.Y - V2.Y;
end;

//------------------------------------------------------------------------------
function VectorSub(const V1, V2: TVector2f ): TVector2f;
begin
  Result.X:= V1.X - V2.X;
  Result.Y:= V1.Y - V2.Y;
end;

// Multiplication
//------------------------------------------------------------------------------
function VectorMul(const V1: TVector2i ; Scalar: Single): TVector2i;
begin
  Result.X:= Trunc(V1.X * Scalar);
  Result.Y:= Trunc(V1.Y * Scalar);
end;

//------------------------------------------------------------------------------
function VectorMul(const V1: TVector2f ; Scalar: Single): TVector2f;
begin
  Result.X:= V1.X * Scalar;
  Result.Y:= V1.Y * Scalar;
end;

// Division
//------------------------------------------------------------------------------
function VectorDiv(const V1: TVector2i ; Scalar: Single): TVector2i;
begin
  Result.X:= Trunc(V1.X / Scalar);
  Result.Y:= Trunc(V1.Y / Scalar);
end;

//------------------------------------------------------------------------------
function VectorDiv(const V1: TVector2f ; Scalar: Single): TVector2f;
begin
  Result.X:= V1.X / Scalar;
  Result.Y:= V1.Y / Scalar;
end;

// Dot product
//------------------------------------------------------------------------------
function VectorDot(const V1, V2: TVector2i ): Single;
begin
  Result:= (V1.X * V2.X) + (V1.Y * V2.Y);
end;

//------------------------------------------------------------------------------
function VectorDot(const V1, V2: TVector2f ): Single;
begin
  Result:= (V1.X * V2.X) + (V1.Y * V2.Y);
end;

// Cross product
//------------------------------------------------------------------------------
function VectorCross(const V1, V2: TVector2i ): Single;
begin
  Result:=( V1.X * V2.Y) - (V1.Y * V2.X);
end;

//------------------------------------------------------------------------------
function VectorCross(const V1, V2: TVector2f ): Single;
begin
  Result:=( V1.X * V2.Y) - (V1.Y * V2.X);
end;

// Vector reflection, V is the vector to reflect, N is the wall normal
//------------------------------------------------------------------------------
function VectorReflect(const V, N: TVector2f ): TVector2f;
var d: Single;
begin
  d:= VectorDot(N, V);

  Result.X:= V.X - 2 * N.X * d;
  Result.Y:= V.Y - 2 * N.Y * d;
end;

//------------------------------------------------------------------------------
function VectorReflect(const V, N: TVector2i ): TVector2i; overload;
var d: Single;
begin
  d:= VectorDot(N, V);

  Result.X:= V.X - Trunc(2 * N.X * d);
  Result.Y:= V.Y - Trunc(2 * N.Y * d);
end;


////////////////////////////////////////////////////////////////////////////////
// 3D Vector Math                                                                //
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
function VectorMagnitude(const Vector: TVector3i): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function VectorMagnitude(const Vector: TVector3f): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;


// The squared magnitude
//------------------------------------------------------------------------------
function VectorMagnitudeSqr(const Vector: TVector3i): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function VectorMagnitudeSqr(const Vector: TVector3f): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;


// Normalize a vector
//------------------------------------------------------------------------------
function VectorNormalize(const Vector: TVector3i): TVector3i;
var MagInv: Single;
begin
  MagInv:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) + Sqr(Vector.Z));

  if MagInv = 0 then
  begin
    Result:= Vector;
    Exit;
  end;

  MagInv:= 1 / MagInv;

  Result.X:=Trunc(Vector.X * MagInv);
  Result.Y:=Trunc(Vector.Y * MagInv);
  Result.Z:=Trunc(Vector.Z * MagInv);
end;

//------------------------------------------------------------------------------
function VectorNormalize(const Vector: TVector3f): TVector3f;
var MagInv: Single;
begin
  MagInv:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) + Sqr(Vector.Z));

  if MagInv = 0 then
  begin
    Result:= Vector;
    Exit;
  end;

  MagInv:= 1 / MagInv;

  Result.X:=Vector.X * MagInv;
  Result.Y:=Vector.Y * MagInv;
  Result.Z:=Vector.Z * MagInv;
end;

// Negate a vector
//------------------------------------------------------------------------------
function VectorNegate(const Vector: TVector3i ): TVector3i;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
  Result.Z:= -Vector.Z;
end;

//------------------------------------------------------------------------------
function VectorNegate(const Vector: TVector3f ): TVector3f;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
  Result.Z:= -Vector.Z;
end;

// Vector addition
//------------------------------------------------------------------------------
function VectorAdd(const V1, V2: TVector3i ): TVector3i;
begin
  Result.X:= V1.X + V2.X;
  Result.Y:= V1.Y + V2.Y;
  Result.Z:= V1.Z + V2.Z;
end;

//------------------------------------------------------------------------------
function VectorAdd(const V1, V2: TVector3f ): TVector3f;
begin
  Result.X:= V1.X + V2.X;
  Result.Y:= V1.Y + V2.Y;
  Result.Z:= V1.Z + V2.Z;
end;

// Vector subtracting
//------------------------------------------------------------------------------
function VectorSub(const V1, V2: TVector3i ): TVector3i;
begin
  Result.X:= V1.X - V2.X;
  Result.Y:= V1.Y - V2.Y;
  Result.Z:= V1.Z - V2.Z;
end;

//------------------------------------------------------------------------------
function VectorSub(const V1, V2: TVector3f ): TVector3f;
begin
  Result.X:= V1.X - V2.X;
  Result.Y:= V1.Y - V2.Y;
  Result.Z:= V1.Z - V2.Z;
end;

// Multiplication
//------------------------------------------------------------------------------
function VectorMul(const V1: TVector3i ; Scalar: Single): TVector3i;
begin
  Result.X:= Trunc(V1.X * Scalar);
  Result.Y:= Trunc(V1.Y * Scalar);
  Result.Z:= Trunc(V1.Z * Scalar);
end;

//------------------------------------------------------------------------------
function VectorMul(const V1: TVector3f ; Scalar: Single): TVector3f;
begin
  Result.X:= V1.X * Scalar;
  Result.Y:= V1.Y * Scalar;
  Result.Z:= V1.Z * Scalar;
end;

// Division
//------------------------------------------------------------------------------
function VectorDiv(const V1: TVector3i ; Scalar: Single): TVector3i;
begin
  Result.X:= Trunc(V1.X / Scalar);
  Result.Y:= Trunc(V1.Y / Scalar);
  Result.Z:= Trunc(V1.Z / Scalar);
end;

//------------------------------------------------------------------------------
function VectorDiv(const V1: TVector3f ; Scalar: Single): TVector3f;
begin
  Result.X:= V1.X / Scalar;
  Result.Y:= V1.Y / Scalar;
  Result.Z:= V1.Z / Scalar;
end;


// Dot product
//------------------------------------------------------------------------------
function  VectorDot(const V1, V2: TVector3i ): Single;
begin
  Result:= (V1.X * V2.X) + (V1.Y * V2.Y) + (V1.Z * V2.Z);
end;

//------------------------------------------------------------------------------
function VectorDot(const V1, V2: TVector3f ): Single;
begin
  Result:= (V1.X * V2.X) + (V1.Y * V2.Y) + (V1.Z * V2.Z);
end;

// Cross product
//------------------------------------------------------------------------------
function VectorCross(const V1, V2: TVector3i ): TVector3i;
begin
  Result.X:= V1.Y * V2.Z - V1.Z * V2.Y;
  Result.Y:= V1.Z * V2.X - V1.X * V2.Z;
  Result.Z:= V1.X * V2.Y - V1.Y * V2.X;
end;

//------------------------------------------------------------------------------
function VectorCross(const V1, V2: TVector3f ): TVector3f;
begin
  Result.X:= V1.Y * V2.Z - V1.Z * V2.Y;
  Result.Y:= V1.Z * V2.X - V1.X * V2.Z;
  Result.Z:= V1.X * V2.Y - V1.Y * V2.X;
end;

//------------------------------------------------------------------------------
function VectorReflect(const V, N: TVector3f ): TVector3f;
var d: Single;
begin
  d:= VectorDot(N, V);

  Result.X:= V.X - 2 * N.X * d;
  Result.Y:= V.Y - 2 * N.Y * d;
  Result.Z:= V.Z - 2 * N.Z * d;
end;

//------------------------------------------------------------------------------
function VectorReflect(const V, N: TVector3i ): TVector3i;
var d: Single;
begin
  d:= VectorDot(N, V);

  Result.X:= V.X - Trunc(2 * N.X * d);
  Result.Y:= V.Y - Trunc(2 * N.Y * d);
  Result.Z:= V.Z - Trunc(2 * N.Z * d);
end;



{$REGION 'TMatrix'}



// void Frustum(float left, float right, float bottom, float top, float nearZ, float farZ, float *m)
//{
//   float x = (2.0F*nearZ) / (right-left);
//   float y = (2.0F*nearZ) / (top-bottom);
//   float a = (right+left) / (right-left);
//   float b = (top+bottom) / (top-bottom);
//   float c = -(farZ+nearZ) / ( farZ-nearZ);
//   float d = -(2.0F*farZ*nearZ) / (farZ-nearZ);
//
//#define M(row,col)  m[col*4+row]
//   M(0,0) = x;     M(0,1) = 0.0F;  M(0,2) = a;      M(0,3) = 0.0F;
//   M(1,0) = 0.0F;  M(1,1) = y;     M(1,2) = b;      M(1,3) = 0.0F;
//   M(2,0) = 0.0F;  M(2,1) = 0.0F;  M(2,2) = c;      M(2,3) = d;
//   M(3,0) = 0.0F;  M(3,1) = 0.0F;  M(3,2) = -1.0F;  M(3,3) = 0.0F;
//#undef M
//}
// Build a glFrustum matrix.
//------------------------------------------------------------------------------
function Matrix_LoadFrustum(Left, Right, Bottom, Top, zNear, zFar: Single): TMatrix4f;
var x,y, a, b, c, d: Single;
begin
   x:= (2.0 * znear) / (right - left);
   y:= (2.0 * znear) / (top - bottom);
   a:= (right + left) / (right - left);
   b:= (top + bottom) / (top - bottom);
   c:= -(zfar + znear) / ( zfar - znear);
   d:= -(2.0 * zfar * znear) / (zfar - znear);

   Result.v[00]:= x;    Result.v[04]:= 0.0;  Result.v[08]:= a;     Result.v[12]:= 0.0;
   Result.v[01]:= 0.0;  Result.v[05]:= y;    Result.v[09]:= b;     Result.v[13]:= 0.0;
   Result.v[02]:= 0.0;  Result.v[06]:= 0.0;  Result.v[10]:= c;     Result.v[14]:= d;
   Result.v[03]:= 0.0;  Result.v[07]:= 0.0;  Result.v[11]:= -1.0;  Result.v[15]:= 0.0;
end;

////////////////////////////////////////////////////////////////////////////////
// Matrix                                                                     //
////////////////////////////////////////////////////////////////////////////////

const
  M11 = 0;
  M12 = 1;
  M13 = 2;
//  M14 = 3;

  M21 = 4;
  M22 = 5;
  M23 = 6;
  //M24 = 7;

  M31 = 8;
  M32 = 9;
  M33 = 10;
  //M34 = 11;

  //M41 = 12;
  //M42 = 13;
  //M43 = 14;
  M44 = 14;



// void Frustum(float left, float right, float bottom, float top, float nearZ, float farZ, float *m)
//{
//   float x = (2.0F*nearZ) / (right-left);
//   float y = (2.0F*nearZ) / (top-bottom);
//   float a = (right+left) / (right-left);
//   float b = (top+bottom) / (top-bottom);
//   float c = -(farZ+nearZ) / ( farZ-nearZ);
//   float d = -(2.0F*farZ*nearZ) / (farZ-nearZ);
//
//#define M(row,col)  m[col*4+row]
//   M(0,0) = x;     M(0,1) = 0.0F;  M(0,2) = a;      M(0,3) = 0.0F;
//   M(1,0) = 0.0F;  M(1,1) = y;     M(1,2) = b;      M(1,3) = 0.0F;
//   M(2,0) = 0.0F;  M(2,1) = 0.0F;  M(2,2) = c;      M(2,3) = d;
//   M(3,0) = 0.0F;  M(3,1) = 0.0F;  M(3,2) = -1.0F;  M(3,3) = 0.0F;
//#undef M
//}
// Build a glFrustum matrix.
//------------------------------------------------------------------------------
function Matrix_CreateFrustum(Left, Right, Bottom, Top, zNear, zFar: Single): TMatrix4f;
var x,y, a, b, c, d: Single;
begin
   x:= (2.0 * znear) / (right - left);
   y:= (2.0 * znear) / (top - bottom);
   a:= (right + left) / (right - left);
   b:= (top + bottom) / (top - bottom);
   c:= -(zfar + znear) / ( zfar - znear);
   d:= -(2.0 * zfar * znear) / (zfar - znear);

   Result.v[00]:= x;    Result.v[04]:= 0.0;  Result.v[08]:= a;     Result.v[12]:= 0.0;
   Result.v[01]:= 0.0;  Result.v[05]:= y;    Result.v[09]:= b;     Result.v[13]:= 0.0;
   Result.v[02]:= 0.0;  Result.v[06]:= 0.0;  Result.v[10]:= c;     Result.v[14]:= d;
   Result.v[03]:= 0.0;  Result.v[07]:= 0.0;  Result.v[11]:= -1.0;  Result.v[15]:= 0.0;
end;

// procedure(left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble);
// Implemented according to http://glprogramming.com/blue/ch05.html#id5511492
//------------------------------------------------------------------------------
function Matrix_CreateOrthographic(Left, Right, Bottom, Top, zNear, zFar: Single): TMatrix4f;
var tx: Single;
var ty: Single;
var tz: Single;
begin
  tx:= - (right + Left  ) / (right - left  );
  ty:= - (top   + bottom) / (top   - bottom);
  tz:= - (zfar  + znear ) / (zfar  - znear );

  Result:= Matrix4f_Identity;
  Result.v[0 ]:= 2 / (right - left);
  Result.v[5 ]:= 2 / (top   - bottom);
  Result.v[10]:= 2 / (zfar  - znear);

  Result.v[12]:= tx;
  Result.v[13]:= ty;
  Result.v[14]:= tz;
end;

//------------------------------------------------------------------------------
function Matrix_CreateOrthographicLH(Width, Height, zNear, zFar: Single): TMatrix4f;
var tx: Single;
var ty: Single;
var tz: Single;
begin
  tx:= - (Width) / (Width );
  ty:= - (Height) / (- Height);
  tz:= - (zfar  + znear ) / (zfar  - znear );

  Result:= Matrix4f_Identity;
  Result.v[0 ]:= 2 / (Width);
  Result.v[5 ]:= 2 / (- Height);
  Result.v[10]:= 2 / (zfar  - znear);

  Result.v[12]:= tx;
  Result.v[13]:= ty;
  Result.v[14]:= tz;
end;

// http://steinsoft.net/index.php?site=Programming/Code%20Snippets/OpenGL/gluperspective
//------------------------------------------------------------------------------
function Matrix_CreatePerspective(Fov, Aspect, zNear, zFar: Single): TMatrix4f;
var XMin, XMax: Single;
var YMin, YMax: Single;
begin
   ymax:= zNear * tan(fov * PI / 360.0);
   ymin:= -ymax;

   xmin:= ymin * aspect;
   xmax:= ymax * aspect;

   Result:= Matrix_LoadFrustum(xmin, xmax, ymin, ymax, zNear, zFar);
end;

//------------------------------------------------------------------------------
function Matrix_CreateIdentity: TMatrix4f;
begin
  Result:= Matrix4f_Identity;
end;

//------------------------------------------------------------------------------
function Matrix_CreateTranslation(const X, Y, Z: Single): TMatrix4f;
begin
  Result:= Matrix4f_Identity;

  Result.v[12] := X;
  Result.v[13] := Y;
  Result.v[14] := Z;
end;

//------------------------------------------------------------------------------
function Matrix_CreateTranslation(const Translation: TVector3f): TMatrix4f;
begin
  Result:= Matrix4f_Identity;

  Result.v[12] := Translation.X;
  Result.v[13] := Translation.Y;
  Result.v[14] := Translation.Z;
end;

//------------------------------------------------------------------------------
function Matrix_CreateTranslation(const Translation: TVector2f): TMatrix4f;
begin
  Result:= Matrix4f_Identity;

  Result.v[12] := Translation.X;
  Result.v[13] := Translation.Y;
  Result.v[14] := 0;
end;


//------------------------------------------------------------------------------
function Matrix_CreateRotation(const Angles: TVector3f): TMatrix4f;
var sx, sy, sz, cx, cy, cz: Single;
begin
  sz:= sin(Angles.Z * DEG_TO_RAD);
  cz:= cos(Angles.Z * DEG_TO_RAD);

  sy:= sin(Angles.Y * DEG_TO_RAD);
  cy:= cos(Angles.Y * DEG_TO_RAD);

  sx:= sin(Angles.X * DEG_TO_RAD);
  cx:= cos(Angles.X * DEG_TO_RAD);

  Result:= Matrix4f_Identity;

  Result.v[0 ]:= cy * cz;
  Result.v[1 ]:= cy * sz;
  Result.v[2 ]:= -sy;
  Result.v[4 ]:= (sx * sy * cz) + (cx * -sz);
  Result.v[5 ]:= (sx * sy * sz) + (cx *  cz);
  Result.v[6 ]:= sx * cy;
  Result.v[8 ]:= (cx * sy * cz) + (-sx * -sz);
  Result.v[9 ]:= (cx * sy * sz) + (-sx *  cz);
  Result.v[10]:= cx * cy;
end;

//------------------------------------------------------------------------------
function Matrix_CreateRotation(const AngleX, AngleY, AngleZ: Single): TMatrix4f;
var sx, sy, sz, cx, cy, cz: Single;
begin
  sz:= sin(AngleZ * DEG_TO_RAD);
  cz:= cos(AngleZ * DEG_TO_RAD);

  sy:= sin(AngleY * DEG_TO_RAD);
  cy:= cos(AngleY * DEG_TO_RAD);

  sx:= sin(AngleX * DEG_TO_RAD);
  cx:= cos(AngleX * DEG_TO_RAD);

  Result:= Matrix4f_Identity;

  Result.v[0 ]:= cy * cz;
  Result.v[1 ]:= cy * sz;
  Result.v[2 ]:= -sy;
  Result.v[4 ]:= (sx * sy * cz) + (cx * -sz);
  Result.v[5 ]:= (sx * sy * sz) + (cx *  cz);
  Result.v[6 ]:= sx * cy;
  Result.v[8 ]:= (cx * sy * cz) + (-sx * -sz);
  Result.v[9 ]:= (cx * sy * sz) + (-sx *  cz);
  Result.v[10]:= cx * cy;
end;

//------------------------------------------------------------------------------
function Matrix_CreateRotationX(const AngleX: Single): TMatrix4f;
var ca, sa: Single;
begin
  ca:= Cos(AngleX * DEG_TO_RAD);
  sa:= Sin(AngleX * DEG_TO_RAD);

  Result:= Matrix4f_Identity;

  Result.v[M11]:=   1;
  Result.v[M22]:=  ca;
  Result.v[M23]:=  sa;
  Result.v[M32]:= -sa;
  Result.v[M33]:=  ca;
  Result.v[M44]:=   1;
end;

//------------------------------------------------------------------------------
function Matrix_CreateRotationY(const AngleY: Single): TMatrix4f;
var ca, sa: Single;
begin
  ca:= Cos(AngleY * DEG_TO_RAD);
  sa:= Sin(AngleY * DEG_TO_RAD);

  Result:= Matrix4f_Identity;
  Result.v[M11]:=  ca;
  Result.v[M13]:= -sa;
  Result.v[M22]:=   1;
  Result.v[M31]:=  sa;
  Result.v[M33]:=  ca;
  Result.v[M44]:=   1;
end;

//------------------------------------------------------------------------------
function Matrix_CreateRotationZ(const AngleZ: Single): TMatrix4f;
var ca, sa: Single;
begin
  ca:= Cos(AngleZ * DEG_TO_RAD);
  sa:= Sin(AngleZ * DEG_TO_RAD);

  Result:= Matrix4f_Identity;
  Result.v[M11]:=  ca;
  Result.v[M12]:=  sa;
  Result.v[M21]:= -sa;
  Result.v[M22]:=  ca;
  Result.v[M33]:=   1;
end;

// Creates a rotation matrix about the specified axis.
// The axis must be a unit vector. The angle must be in degrees.
//
// Let u = axis of rotation = (x, y, z)
//
//             | x^2(1 - c) + c  xy(1 - c) + zs  xz(1 - c) - ys   0 |
// Ru(angle) = | yx(1 - c) - zs  y^2(1 - c) + c  yz(1 - c) + xs   0 |
//             | zx(1 - c) - ys  zy(1 - c) - xs  z^2(1 - c) + c   0 |
//             |      0              0                0           1 |
//
// where,
//	c = cos(angle)
//      s = sin(angle)
//------------------------------------------------------------------------------
function Matrix_CreateRotation(const Angle: Single; const Axis : TVector3f): TMatrix4f;
var x,y,z,c,s: Single;
begin
  x:= axis.x;
  y:= axis.y;
  z:= axis.z;
  c:= cos(Angle * DEG_TO_RAD);
  s:= sin(Angle * DEG_TO_RAD);

  Result.v[0]:= (x * x) * (1.0 - c) + c;
  Result.v[1]:= (x * y) * (1.0 - c) + (z * s);
  Result.v[2]:= (x * z) * (1.0 - c) - (y * s);
  Result.v[3]:= 0.0;

  Result.v[4]:= (y * x) * (1.0 - c) - (z * s);
  Result.v[5]:= (y * y) * (1.0 - c) + c;
  Result.v[6]:= (y * z) * (1.0 - c) + (x * s);
  Result.v[7]:= 0.0;

  Result.v[8 ]:= (z * x) * (1.0 - c) + (y * s);
  Result.v[9 ]:= (z * y) * (1.0 - c) - (x * s);
  Result.v[10]:= (z * z) * (1.0 - c) + c;
  Result.v[11]:= 0.0;

  Result.v[12]:= 0.0;
  Result.v[13]:= 0.0;
  Result.v[14]:= 0.0;
  Result.v[15]:= 1.0;
end;

//------------------------------------------------------------------------------
function Matrix_CreateScale(const X, Y, Z: Single): TMatrix4f;
begin
  Result:= Matrix4f_Zero;
  Result.v[00] := X;
  Result.v[05] := Y;
  Result.v[10] := Z;
  Result.v[15] := 1;
end;

//------------------------------------------------------------------------------
function Matrix_CreateScale(const Scale : TVector3f): TMatrix4f;
begin
  Result:= Matrix4f_Zero;
  Result.v[00] := Scale.X;
  Result.v[05] := Scale.Y;
  Result.v[10] := Scale.Z;
  Result.v[15] := 1;
end;

//------------------------------------------------------------------------------
function Matrix_CreateScale(const Scale : TVector2f): TMatrix4f;
begin
  Result:= Matrix4f_Zero;
  Result.v[00] := Scale.X;
  Result.v[05] := Scale.Y;
  Result.v[10] := 1;
  Result.v[15] := 1;
end;




const
  M_00 = 0;
  M_01 = 1;
  M_02 = 2;
  //M_03 = 3;

  M_10 = 4;
  M_11 = 5;
  M_12 = 6;
 // M_13 = 7;

  M_20 = 8;
  M_21 = 9;
  M_22 = 10;
  M_23 = 11;

  M_30 = 12;
  M_31 = 13;
  M_32 = 14;
  M_33 = 14;

//-----------------------------------------------------------------------------
function Matrix_Multiply( const M1, M2: TMatrix4f ): TMatrix4f;
begin
  Result.v[0 ]:= m1.v[ 0] * m2.v[ 0] + m1.v[ 4] * m2.v[ 1] + m1.v[ 8] * m2.v[2];
  Result.v[1 ]:= m1.v[ 1] * m2.v[ 0] + m1.v[ 5] * m2.v[ 1] + m1.v[ 9] * m2.v[2];
  Result.v[2 ]:= m1.v[ 2] * m2.v[ 0] + m1.v[ 6] * m2.v[ 1] + m1.v[10] * m2.v[2];
  Result.v[3 ]:= 0;

  Result.v[4 ]:= m1.v[ 0] * m2.v[ 4] + m1.v[ 4] * m2.v[5] + m1.v[8 ] * m2.v[6];
  Result.v[5 ]:= m1.v[ 1] * m2.v[ 4] + m1.v[ 5] * m2.v[5] + m1.v[9 ] * m2.v[6];
  Result.v[6 ]:= m1.v[ 2] * m2.v[ 4] + m1.v[ 6] * m2.v[5] + m1.v[10] * m2.v[6];
  Result.v[7 ]:= 0;

  Result.v[8 ]:= m1.v[ 0] * m2.v[ 8] + m1.v[ 4] * m2.v[ 9] + m1.v[ 8] * m2.v[10];
  Result.v[9 ]:= m1.v[ 1] * m2.v[ 8] + m1.v[ 5] * m2.v[ 9] + m1.v[ 9] * m2.v[10];
  Result.v[10]:= m1.v[ 2] * m2.v[ 8] + m1.v[ 6] * m2.v[ 9] + m1.v[10] * m2.v[10];
  Result.v[11]:= 0;

  Result.v[12]:= m1.v[ 0] * m2.v[12] + m1.v[ 4] * m2.v[13] + m1.v[ 8] * m2.v[14] + m1.v[12];
  Result.v[13]:= m1.v[ 1] * m2.v[12] + m1.v[ 5] * m2.v[13] + m1.v[ 9] * m2.v[14] + m1.v[13];
  Result.v[14]:= m1.v[ 2] * m2.v[12] + m1.v[ 6] * m2.v[13] + m1.v[10] * m2.v[14] + m1.v[14];
  Result.v[15]:= 1;
end;

//-----------------------------------------------------------------------------
function Matrix_Multiply( const M1, M2, M3: TMatrix4f ): TMatrix4f;
var t: TMatrix4f;
begin
  // m1.v * m2.v
  t.v[0 ]:= m1.v[ 0] * m2.v[ 0] + m1.v[ 4] * m2.v[ 1] + m1.v[ 8] * m2.v[2];
  t.v[1 ]:= m1.v[ 1] * m2.v[ 0] + m1.v[ 5] * m2.v[ 1] + m1.v[ 9] * m2.v[2];
  t.v[2 ]:= m1.v[ 2] * m2.v[ 0] + m1.v[ 6] * m2.v[ 1] + m1.v[10] * m2.v[2];
  t.v[3 ]:= 0;

  t.v[4 ]:= m1.v[ 0] * m2.v[ 4] + m1.v[ 4] * m2.v[5] + m1.v[8 ] * m2.v[6];
  t.v[5 ]:= m1.v[ 1] * m2.v[ 4] + m1.v[ 5] * m2.v[5] + m1.v[9 ] * m2.v[6];
  t.v[6 ]:= m1.v[ 2] * m2.v[ 4] + m1.v[ 6] * m2.v[5] + m1.v[10] * m2.v[6];
  t.v[7 ]:= 0;

  t.v[8 ]:= m1.v[ 0] * m2.v[ 8] + m1.v[ 4] * m2.v[ 9] + m1.v[ 8] * m2.v[10];
  t.v[9 ]:= m1.v[ 1] * m2.v[ 8] + m1.v[ 5] * m2.v[ 9] + m1.v[ 9] * m2.v[10];
  t.v[10]:= m1.v[ 2] * m2.v[ 8] + m1.v[ 6] * m2.v[ 9] + m1.v[10] * m2.v[10];
  t.v[11]:= 0;

  t.v[12]:= m1.v[ 0] * m2.v[12] + m1.v[ 4] * m2.v[13] + m1.v[ 8] * m2.v[14] + m1.v[12];
  t.v[13]:= m1.v[ 1] * m2.v[12] + m1.v[ 5] * m2.v[13] + m1.v[ 9] * m2.v[14] + m1.v[13];
  t.v[14]:= m1.v[ 2] * m2.v[12] + m1.v[ 6] * m2.v[13] + m1.v[10] * m2.v[14] + m1.v[14];
  t.v[15]:= 1;

  // (m1.v * m2.v) * m3.v
  Result.v[0 ]:= t.v[ 0] * m3.v[ 0] + t.v[ 4] * m3.v[ 1] + t.v[ 8] * m3.v[2];
  Result.v[1 ]:= t.v[ 1] * m3.v[ 0] + t.v[ 5] * m3.v[ 1] + t.v[ 9] * m3.v[2];
  Result.v[2 ]:= t.v[ 2] * m3.v[ 0] + t.v[ 6] * m3.v[ 1] + t.v[10] * m3.v[2];
  Result.v[3 ]:= 0;

  Result.v[4 ]:= t.v[ 0] * m3.v[ 4] + t.v[ 4] * m3.v[5] + t.v[8 ] * m3.v[6];
  Result.v[5 ]:= t.v[ 1] * m3.v[ 4] + t.v[ 5] * m3.v[5] + t.v[9 ] * m3.v[6];
  Result.v[6 ]:= t.v[ 2] * m3.v[ 4] + t.v[ 6] * m3.v[5] + t.v[10] * m3.v[6];
  Result.v[7 ]:= 0;

  Result.v[8 ]:= t.v[ 0] * m3.v[ 8] + t.v[ 4] * m3.v[ 9] + t.v[ 8] * m3.v[10];
  Result.v[9 ]:= t.v[ 1] * m3.v[ 8] + t.v[ 5] * m3.v[ 9] + t.v[ 9] * m3.v[10];
  Result.v[10]:= t.v[ 2] * m3.v[ 8] + t.v[ 6] * m3.v[ 9] + t.v[10] * m3.v[10];
  Result.v[11]:= 0;

  Result.v[12]:= t.v[ 0] * m3.v[12] + t.v[ 4] * m3.v[13] + t.v[ 8] * m3.v[14] + t.v[12];
  Result.v[13]:= t.v[ 1] * m3.v[12] + t.v[ 5] * m3.v[13] + t.v[ 9] * m3.v[14] + t.v[13];
  Result.v[14]:= t.v[ 2] * m3.v[12] + t.v[ 6] * m3.v[13] + t.v[10] * m3.v[14] + t.v[14];
  Result.v[15]:= 1;
end;

//-----------------------------------------------------------------------------
function Matrix_Multiply( const M1, M2, M3, M4: TMatrix4f ): TMatrix4f;
begin
  Result:= Matrix_Multiply(Matrix_Multiply(Matrix_Multiply(M1, M2), M3), M4);
end;


//------------------------------------------------------------------------------
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f;
begin
  Result.X:= Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4] + Matrix.v[12];
  Result.Y:= Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5] + Matrix.v[13];
end;

//------------------------------------------------------------------------------
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector2i): TVector2i;
begin
  Result.X:= Trunc(Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4] + Matrix.v[12]);
  Result.Y:= Trunc(Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5] + Matrix.v[13]);
end;

//------------------------------------------------------------------------------
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f;
begin
  Result.X:= Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4] + Vector.Z * Matrix.v[8 ] + Matrix.v[12];
  Result.Y:= Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5] + Vector.Z * Matrix.v[9 ] + Matrix.v[13];
  Result.Z:= Vector.X * Matrix.v[2] + Vector.Y * Matrix.v[6] + Vector.Z * Matrix.v[10] + Matrix.v[14];
end;

//------------------------------------------------------------------------------
function Matrix_Transform(const Matrix: TMatrix4f; const Vector: TVector4f): TVector4f;
begin
  Result.X:= Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4] + Vector.Z * Matrix.v[8 ] + Vector.W * Matrix.v[12];
  Result.Y:= Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5] + Vector.Z * Matrix.v[9 ] + Vector.W * Matrix.v[13];
  Result.Z:= Vector.X * Matrix.v[2] + Vector.Y * Matrix.v[6] + Vector.Z * Matrix.v[10] + Vector.W * Matrix.v[14];
  Result.W:= Vector.X * Matrix.v[3] + Vector.Y * Matrix.v[7] + Vector.Z * Matrix.v[11] + Vector.W * Matrix.v[15];
end;

//------------------------------------------------------------------------------
function Matrix_Transform(const Matrix: TMatrix4f; const X,Y,Z: Single): TVector3f;
begin
  Result.X:= X * Matrix.v[0] + Y * Matrix.v[4] + Z * Matrix.v[8 ] + Matrix.v[12];
  Result.Y:= X * Matrix.v[1] + Y * Matrix.v[5] + Z * Matrix.v[9 ] + Matrix.v[13];
  Result.Z:= X * Matrix.v[2] + Y * Matrix.v[6] + Z * Matrix.v[10] + Matrix.v[14];
end;



// Inverse transform a vector by a matrix
//------------------------------------------------------------------------------
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f;
var Temp: TVector2f;
begin
  Temp.X:= Vector.X - Matrix.v[M_30];
  Temp.Y:= Vector.Y - Matrix.v[M_31];

  Result.X:= Temp.X * Matrix.v[M_00] + Temp.Y * Matrix.v[M_01];
  Result.Y:= Temp.X * Matrix.v[M_10] + Temp.Y * Matrix.v[M_11];
end;
{
x = x0 * matrix[0][0] + y0 * matrix[0][1] + z0 * matrix[0][2];
y = x0 * matrix[1][0] + y0 * matrix[1][1] + z0 * matrix[1][2];
z = x0 * matrix[2][0] + y0 * matrix[2][1] + z0 * matrix[2][2];
}
//------------------------------------------------------------------------------
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f;
var Temp: TVector3f;
begin
  Temp.X:= Vector.X - Matrix.v[M_30];
  Temp.Y:= Vector.Y - Matrix.v[M_31];
  Temp.Z:= Vector.Z - Matrix.v[M_32];

  Result.X:= Temp.X * Matrix.v[M_00] + Temp.Y * Matrix.v[M_01] + Temp.Z * Matrix.v[M_02];
  Result.Y:= Temp.X * Matrix.v[M_10] + Temp.Y * Matrix.v[M_11] + Temp.Z * Matrix.v[M_12];
  Result.Z:= Temp.X * Matrix.v[M_20] + Temp.Y * Matrix.v[M_21] + Temp.Z * Matrix.v[M_22];
end;

//------------------------------------------------------------------------------
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector2i): TVector2i;
var Temp: TVector2f;
begin
  Temp.X:= Vector.X - Matrix.v[M_30];
  Temp.Y:= Vector.Y - Matrix.v[M_31];

  Result.X:= Round(Temp.X * Matrix.v[M_00] + Temp.Y * Matrix.v[M_01]);
  Result.Y:= Round(Temp.X * Matrix.v[M_10] + Temp.Y * Matrix.v[M_11]);
end;

//------------------------------------------------------------------------------
function Matrix_TransformInv(const Matrix: TMatrix4f; const Vector: TVector4f): TVector4f;
begin
  Result.X:= Vector.X * Matrix.v[M_00] + Vector.Y * Matrix.v[M_01] + Vector.Z * Matrix.v[M_02] -  Vector.W * Matrix.v[M_30];
  Result.Y:= Vector.X * Matrix.v[M_10] + Vector.Y * Matrix.v[M_11] + Vector.Z * Matrix.v[M_12] -  Vector.W * Matrix.v[M_31];
  Result.Z:= Vector.X * Matrix.v[M_20] + Vector.Y * Matrix.v[M_21] + Vector.Z * Matrix.v[M_22] -  Vector.W * Matrix.v[M_32];
  Result.W:= Vector.X * Matrix.v[M_20] + Vector.Y * Matrix.v[M_22] + Vector.Z * Matrix.v[M_23] -  Vector.W * Matrix.v[M_33];
end;


// Rotate a vector by a matrix
//------------------------------------------------------------------------------
function Matrix_Rotate(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f;
begin
  Result.X:= Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4] + Vector.Z * Matrix.v[8 ];
  Result.Y:= Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5] + Vector.Z * Matrix.v[9 ];
  Result.Z:= Vector.X * Matrix.v[2] + Vector.Y * Matrix.v[6] + Vector.Z * Matrix.v[10];
end;

//------------------------------------------------------------------------------
function Matrix_Rotate(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f;
begin
  Result.X:= Vector.X * Matrix.v[0] + Vector.Y * Matrix.v[4];
  Result.Y:= Vector.X * Matrix.v[1] + Vector.Y * Matrix.v[5];
end;

//------------------------------------------------------------------------------
function Matrix_RotateInv(const Matrix: TMatrix4f; const Vector: TVector3f): TVector3f;
begin
  Result.X:= Vector.X * Matrix.v[M_00] + Vector.Y * Matrix.v[M_01] + Vector.Z * Matrix.v[M_02];
  Result.Y:= Vector.X * Matrix.v[M_10] + Vector.Y * Matrix.v[M_11] + Vector.Z * Matrix.v[M_12];
  Result.Z:= Vector.X * Matrix.v[M_20] + Vector.Y * Matrix.v[M_21] + Vector.Z * Matrix.v[M_22];
end;

//------------------------------------------------------------------------------
function Matrix_RotateInv(const Matrix: TMatrix4f; const Vector: TVector2f): TVector2f;
begin
  Result.X:= Vector.X * Matrix.v[M_00] + Vector.Y * Matrix.v[M_01];
  Result.Y:= Vector.X * Matrix.v[M_10] + Vector.Y * Matrix.v[M_11];
end;

//------------------------------------------------------------------------------
function Matrix_GetForward(const Matrix: TMatrix4f): TVector3f;
begin
  Result.X:= Matrix.v[02];
  Result.Y:= Matrix.v[06];
  Result.Z:= Matrix.v[10];
end;

//------------------------------------------------------------------------------
function Matrix_GetUp(const Matrix: TMatrix4f): TVector3f;
begin
  Result.X:= Matrix.v[1];
  Result.Y:= Matrix.v[5];
  Result.Z:= Matrix.v[9];
end;

//------------------------------------------------------------------------------
function Matrix_GetRight(const Matrix: TMatrix4f): TVector3f;
begin
  Result.X:= Matrix.v[0];
  Result.Y:= Matrix.v[4];
  Result.Z:= Matrix.v[8];
end;

//------------------------------------------------------------------------------
function Matrix_GetTranslation(const Matrix: TMatrix4f): TVector3f;
begin
  Result.X:= Matrix.v[12];
  Result.Y:= Matrix.v[13];
  Result.Z:= Matrix.v[14];
end;

//------------------------------------------------------------------------------
function Matrix_GetRotationX(const Matrix: TMatrix4f): Single;
var Direction: TVector3f;
begin
  Direction:= Matrix_Rotate(Matrix, Vector3f_AxisX);

  Result:= ArcTan2(Direction.Y, Direction.Z) * RAD_TO_DEG;
end;

//------------------------------------------------------------------------------
function Matrix_GetRotationY(const Matrix: TMatrix4f): Single;
var Direction: TVector3f;
begin
  Direction:= Matrix_Rotate(Matrix, Vector3f_AxisX);

  Result:= ArcTan2(Direction.X, Direction.Z) * RAD_TO_DEG;
end;

//------------------------------------------------------------------------------
function Matrix_GetRotationZ(const Matrix: TMatrix4f): Single;
var Direction: TVector3f;
begin
  Direction:= Matrix_Rotate(Matrix, Vector3f_AxisX);

  Result:= ArcTan2(Direction.Y, Direction.X) * RAD_TO_DEG;
end;

// Converts a 4x4 matrix to a 3x3 matrix
//------------------------------------------------------------------------------
function Matrix3f(const Matrix: TMatrix4f): TMatrix3f;
begin
  Result.m[0][0]:= Matrix.v[M_00];
  Result.m[0][1]:= Matrix.v[M_01];
  Result.m[0][2]:= Matrix.v[M_02];

  Result.m[1][0]:= Matrix.v[M_10];
  Result.m[1][1]:= Matrix.v[M_11];
  Result.m[1][2]:= Matrix.v[M_12];

  Result.m[2][0]:= Matrix.v[M_20];
  Result.m[2][1]:= Matrix.v[M_21];
  Result.m[2][2]:= Matrix.v[M_22];
end;

//------------------------------------------------------------------------------
function Matrix_Div(const Matrix: TMatrix3f; const Scalar: Single): TMatrix3f;
begin
  Result.m[0][0]:= Matrix.m[0][0] / Scalar;
  Result.m[0][1]:= Matrix.m[0][1] / Scalar;
  Result.m[0][2]:= Matrix.m[0][2] / Scalar;

  Result.m[1][0]:= Matrix.m[1][0] / Scalar;
  Result.m[1][1]:= Matrix.m[1][1] / Scalar;
  Result.m[1][2]:= Matrix.m[1][2] / Scalar;

  Result.m[2][0]:= Matrix.m[2][0] / Scalar;
  Result.m[2][1]:= Matrix.m[2][1] / Scalar;
  Result.m[2][2]:= Matrix.m[2][2] / Scalar;
end;

//------------------------------------------------------------------------------
function Matrix_InverseTranspose(const Matrix: TMatrix3f): TMatrix3f;
var Determinant: Single;
begin
  Determinant:=
    + Matrix.m[0][0] * (Matrix.m[1][1] * Matrix.m[2][2] - Matrix.m[1][2] * Matrix.m[2][1])
    - Matrix.m[0][1] * (Matrix.m[1][0] * Matrix.m[2][2] - Matrix.m[1][2] * Matrix.m[2][0])
    + Matrix.m[0][2] * (Matrix.m[1][0] * Matrix.m[2][1] - Matrix.m[1][1] * Matrix.m[2][0]);

  Result.m[0][0]:= + (Matrix.m[1][1] * Matrix.m[2][2] - Matrix.m[2][1] * Matrix.m[1][2]);
  Result.m[0][1]:= - (Matrix.m[1][0] * Matrix.m[2][2] - Matrix.m[2][0] * Matrix.m[1][2]);
  Result.m[0][2]:= + (Matrix.m[1][0] * Matrix.m[2][1] - Matrix.m[2][0] * Matrix.m[1][1]);
  Result.m[1][0]:= - (Matrix.m[0][1] * Matrix.m[2][2] - Matrix.m[2][1] * Matrix.m[0][2]);
  Result.m[1][1]:= + (Matrix.m[0][0] * Matrix.m[2][2] - Matrix.m[2][0] * Matrix.m[0][2]);
  Result.m[1][2]:= - (Matrix.m[0][0] * Matrix.m[2][1] - Matrix.m[2][0] * Matrix.m[0][1]);
  Result.m[2][0]:= + (Matrix.m[0][1] * Matrix.m[1][2] - Matrix.m[1][1] * Matrix.m[0][2]);
  Result.m[2][1]:= - (Matrix.m[0][0] * Matrix.m[1][2] - Matrix.m[1][0] * Matrix.m[0][2]);
  Result.m[2][2]:= + (Matrix.m[0][0] * Matrix.m[1][1] - Matrix.m[1][0] * Matrix.m[0][1]);

	Result:= Matrix_Div(Result, Determinant);
end;


{$ENDREGION}



//------------------------------------------------------------------------------
function PointInRect(const P: TVector2f; const R: TRectf): Boolean;
begin
  Result:= (P.X >= R.Left  ) and
           (P.X <  R.Right ) and
           (P.Y >= R.Top   ) and
           (P.Y <  R.Bottom);
end;

//------------------------------------------------------------------------------
function PointInRect(const P: TVector2i; const R: TRecti): Boolean;
begin
  Result:= (P.X >= R.Left  ) and
           (P.X <  R.Right ) and
           (P.Y >= R.Top   ) and
           (P.Y <  R.Bottom);
end;

//------------------------------------------------------------------------------
function PointInRect(const X,Y: Single ; const R: TRectf): Boolean; overload;
begin
  Result:= (X >= R.Left  ) and
           (X <  R.Right ) and
           (Y >= R.Top   ) and
           (Y <  R.Bottom);
end;

//------------------------------------------------------------------------------
function PointInRect(const X,Y: Integer; const R: TRecti): Boolean; overload;
begin
  Result:= (X >= R.Left  ) and
           (X <  R.Right ) and
           (Y >= R.Top   ) and
           (Y <  R.Bottom);
end;


//------------------------------------------------------------------------------
function RectInRect(const A, B: TRectf): Boolean;
begin
  Result:= (A.Left   >= B.Left  ) and
           (A.Right  <= B.Right ) and
           (A.Top    >= B.Top   ) and
           (A.Bottom <= B.Bottom);
end;



//------------------------------------------------------------------------------
function RectInRect(const A, B: TRecti): Boolean;
begin
  Result:= (A.Left   >= B.Left  ) and
           (A.Right  <= B.Right ) and
           (A.Top    >= B.Top   ) and
           (A.Bottom <= B.Bottom);
end;

//------------------------------------------------------------------------------
function OverlapRect(const A, B: TRectf): Boolean;
begin
  Result:= (A.Left   < B.Right ) and
           (A.Right  > B.Left  ) and
           (A.Top    < B.Bottom) and
           (A.Bottom > B.Top   );
end;

//------------------------------------------------------------------------------
function OverlapRect(const A, B: TRecti): Boolean;
begin
  Result:= (A.Left   < B.Right ) and
           (A.Right  > B.Left  ) and
           (A.Top    < B.Bottom) and
           (A.Bottom > B.Top   );
end;

// Returns true if A is contained inside B
//------------------------------------------------------------------------------
function ContainsRect(const A, B: TRectf): Boolean;
begin
  Result:= (B.Left   > A.Left  ) and
           (B.Right  < A.Right ) and
           (B.Top    > A.Top   ) and
           (B.Bottom < A.Bottom);
end;

//------------------------------------------------------------------------------
function ContainsRect(const A, B: TRecti): Boolean;
begin
  Result:= (B.Left   > A.Left  ) and
           (B.Right  < A.Right ) and
           (B.Top    > A.Top   ) and
           (B.Bottom < A.Bottom);
end;



////////////////////////////////////////////////////////////////////////////////
// Collision testing                                                          //
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
function IntervalIntersect(const A, B: TRangef; const Axis, Displacement: TVector2f; var First, Last, MTD: TCollisionParameter): Boolean;
var c,e: Single;
var d0, d1: Single;
var Intersect: Boolean;
var Sep     : TVector2f;
var SepLength: Single;
var AxisLengthSquared: Single;
var v: single;
var t0, t1, sign: single;
var minb: Single;
var maxB: Single;
const tmax = 1.0;
begin
  //---------------------------------------------------
  // projection calculations
  //---------------------------------------------------
  c:= (A.Max + A.Min) * 0.5;
  e:= (A.Max - A.Min) * 0.5;

  // reduce the problem of a single point versus a larger interval
  minb:= B.Min - e;
  maxb:= B.Max + e;

  //---------------------------------------------------
  // intersection test calculations
  //---------------------------------------------------

  // the two potential overlaps
  d0:= minb - c;
  d1:= maxb - c;

  // are the objects separated along that axis?
  Intersect:= (d0 <= 0.0) and (d1 >= 0.0);

  // nope, so check if the intersection vector is the new MTD or not
  if (Intersect) then
  begin
    // Find the MTD along that axis
    // then update the global MTD with it if it is smaller.
    Sep:= Axis;

    // square length of the axis
    AxisLengthSquared:= VectorMagnitudeSqr(Axis);//VectorDot(Axis, Axis);

    // take the minimum direction as a potential separation vector
    if (Abs(d0) < Abs(d1)) then
    begin
      Sep.X:= Sep.X * -d0 / AxisLengthSquared;
      Sep.Y:= Sep.Y * -d0 / AxisLengthSquared;
    end else
    begin
      Sep.X := Sep.X  * -d1 / AxisLengthSquared;
      Sep.Y := Sep.Y  * -d1 / AxisLengthSquared;
    end;

    SepLength:= VectorMagnitudeSqr(Sep);

    // if sepration vector along that axis smaller than MTD,
    // then it is the new MTD
  	if (SepLength < MTD.Time) or (MTD.Time < 0.0) then
    begin
      MTD.Normal:= Sep;
      MTD.Time  := SepLength;
    end;

  end;

  //---------------------------------------------------
  // collision test calculations
  //---------------------------------------------------

  v:= Vectordot(Displacement, Axis);

  // objects not moving. So they can only intersect.
  if (abs(v) < COLLISION_VELOCITY_THRESHOLD) then
  begin
    Result:= Intersect;
    Exit;
  end;

  //---------------------------------------------------
  // time of intersection along that axis
  //---------------------------------------------------

  t0   := -(MinB - c) / v;
  t1   := -(MaxB - c) / v;
  sign:= -1.0;

  //---------------------------------------------------
  // Update the overall times of collision
  //---------------------------------------------------

  // order the times of collision properly
  if (t0 > t1) then
  begin
    Swap(t0, t1);
    Sign:= 1.0;
  end;


  // check bounds. collision to far behind in time,
  // or too far forward in time, no collisoins possible.
  if (t0 > tmax) or (t1 < 0.0) then
  begin
    Result:= False;
    Exit;
  end;

  // make sure the near intersection in withing bounds
  if (t0 > Last.Time) then
  begin
    Result:= False;
    Exit;
  end;

  // make sure the far intersection in withing bounds
  if (t1 < First.Time) then
  begin
    Result:= False;
    Exit;
  end;

  // then squeeze the global intersection interval
  if (t1 < Last.Time)  then
  begin
    Last.Time    := t1;
    Last.Normal.X:= Axis.X * Sign;
    Last.Normal.Y:= Axis.Y * Sign;
  end;

  if (t0 > First.Time)  then
  begin
    First.Time  := t0;
    First.Normal.X:= -Axis.X * Sign;
    First.Normal.Y:= -Axis.Y * Sign;
  end;

  Result:= True;
end;





initialization
  InitSinCosTable();
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end.
