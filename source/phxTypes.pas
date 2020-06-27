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
unit phxTypes;
//< Common Phoenix types

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils;

const
  EPSILON = 1.192092896e-7;


type

//------------------------------------------------------------------------------
TPHXShiftState = (
  // The control key is pressed
  ssCtrl,
  // The alt key is pressed
  ssAlt,
  // The shift key is pressed
  ssShift,
  // The caps lock key is pressed
  ssCaps
);

// Set of shift states
TPHXShiftStates = set of TPHXShiftState;

// Virtual key codes
//------------------------------------------------------------------------------
TPHXVirtualKey = (
  VK_UNKNOWN = 0,

  VK_SPACE = 32,

  VK_1 = 49,
  VK_2 = 50,
  VK_3 = 51,
  VK_4 = 52,
  VK_5 = 53,
  VK_6 = 54,
  VK_7 = 55,
  VK_8 = 56,
  VK_9 = 57,
  VK_0 = 58,

  VK_TILDE = 96,

  VK_A = 97,
  VK_B = 98,
  VK_C = 99,
  VK_D = 100,
  VK_E = 101,
  VK_F = 102,
  VK_G = 103,
  VK_H = 104,
  VK_I = 105,
  VK_J = 106,
  VK_K = 107,
  VK_L = 108,
  VK_M = 109,
  VK_N = 110,
  VK_O = 111,
  VK_P = 112,
  VK_Q = 113,
  VK_R = 114,
  VK_S = 115,
  VK_T = 116,
  VK_U = 117,
  VK_V = 118,
  VK_W = 119,
  VK_X = 120,
  VK_Y = 121,
  VK_Z = 122,

  VK_SPECIAL      = 256,
  VK_ESC          = 256+1,
  VK_F1           = 256+2,
  VK_F2           = 256+3,
  VK_F3           = 256+4,
  VK_F4           = 256+5,
  VK_F5           = 256+6,
  VK_F6           = 256+7,
  VK_F7           = 256+8,
  VK_F8           = 256+9,
  VK_F9           = 256+10,
  VK_F10          = 256+11,
  VK_F11          = 256+12,
  VK_F12          = 256+13,
  VK_F13          = 256+14,
  VK_F14          = 256+15,
  VK_F15          = 256+16,
  VK_F16          = 256+17,
  VK_F17          = 256+18,
  VK_F18          = 256+19,
  VK_F19          = 256+20,
  VK_F20          = 256+21,
  VK_F21          = 256+22,
  VK_F22          = 256+23,
  VK_F23          = 256+24,
  VK_F24          = 256+25,
  VK_F25          = 256+26,
  VK_UP           = 256+27,
  VK_DOWN         = 256+28,
  VK_LEFT         = 256+29,
  VK_RIGHT        = 256+30,
  VK_LSHIFT       = 256+31,
  VK_RSHIFT       = 256+32,
  VK_LCTRL        = 256+33,
  VK_RCTRL        = 256+34,
  VK_LALT         = 256+35,
  VK_RALT         = 256+36,
  VK_TAB          = 256+37,
  VK_RETURN       = 256+38,
  VK_BACKSPACE    = 256+39,
  VK_INSERT       = 256+40,
  VK_DEL          = 256+41,
  VK_PAGEUP       = 256+42,
  VK_PAGEDOWN     = 256+43,
  VK_HOME         = 256+44,
  VK_END          = 256+45,
  VK_NUM_0        = 256+46,
  VK_NUM_1        = 256+47,
  VK_NUM_2        = 256+48,
  VK_NUM_3        = 256+49,
  VK_NUM_4        = 256+50,
  VK_NUM_5        = 256+51,
  VK_NUM_6        = 256+52,
  VK_NUM_7        = 256+53,
  VK_NUM_8        = 256+54,
  VK_NUM_9        = 256+55,
  VK_NUM_DIVIDE   = 256+56,
  VK_NUM_MULTIPLY = 256+57,
  VK_NUM_SUBTRACT = 256+58,
  VK_NUM_ADD      = 256+59,
  VK_NUM_DECIMAL  = 256+60,
  VK_NUM_EQUAL    = 256+61,
  VK_NUM_ENTER    = 256+62,
  VK_LAST         = 256+63
);

// Mouse button enumeration
//------------------------------------------------------------------------------
TPHXMouseButton = (
  PHX_MOUSE_BUTTON_NONE = 0,
  PHX_MOUSE_BUTTON_1 = 1,
  PHX_MOUSE_BUTTON_2 = 2,
  PHX_MOUSE_BUTTON_3 = 3,
  PHX_MOUSE_BUTTON_4 = 4,
  PHX_MOUSE_BUTTON_5 = 5,
  PHX_MOUSE_BUTTON_6 = 6,
  PHX_MOUSE_BUTTON_7 = 7,
  PHX_MOUSE_BUTTON_8 = 8
);

// Joystick index enumeration
//------------------------------------------------------------------------------
TPHXJoystickIndex = (
  PHX_JOYSTICK_1 = 0,
  PHX_JOYSTICK_2 = 1,
  PHX_JOYSTICK_3 = 2,
  PHX_JOYSTICK_4 = 3,
  PHX_JOYSTICK_5 = 4,
  PHX_JOYSTICK_6 = 5,
  PHX_JOYSTICK_7 = 6,
  PHX_JOYSTICK_8 = 7
);

// Joystick axes enumeration
//------------------------------------------------------------------------------
TPHXJoystickAxis = (
  PHX_JOYSTICK_AXIS_1 = 0,
  PHX_JOYSTICK_AXIS_2 = 1,
  PHX_JOYSTICK_AXIS_3 = 2,
  PHX_JOYSTICK_AXIS_4 = 3,
  PHX_JOYSTICK_AXIS_5 = 4,
  PHX_JOYSTICK_AXIS_6 = 5,
  PHX_JOYSTICK_AXIS_7 = 6,
  PHX_JOYSTICK_AXIS_8 = 7
);
// Joystick button enumeration
//------------------------------------------------------------------------------
TPHXJoystickButton = (
  PHX_JOYSTICK_BUTTON_NONE = 0,
  PHX_JOYSTICK_BUTTON_1,
  PHX_JOYSTICK_BUTTON_2,
  PHX_JOYSTICK_BUTTON_3,
  PHX_JOYSTICK_BUTTON_4,
  PHX_JOYSTICK_BUTTON_5,
  PHX_JOYSTICK_BUTTON_6,
  PHX_JOYSTICK_BUTTON_7,
  PHX_JOYSTICK_BUTTON_8,
  PHX_JOYSTICK_BUTTON_9,
  PHX_JOYSTICK_BUTTON_10,
  PHX_JOYSTICK_BUTTON_11,
  PHX_JOYSTICK_BUTTON_12,
  PHX_JOYSTICK_BUTTON_13,
  PHX_JOYSTICK_BUTTON_14,
  PHX_JOYSTICK_BUTTON_15,
  PHX_JOYSTICK_BUTTON_16
);

const
  mbNone = TPHXMouseButton.PHX_MOUSE_BUTTON_NONE;
  // Left mouse button
  mbLeft = TPHXMouseButton.PHX_MOUSE_BUTTON_1;
  // Right mouse button
  mbRight = TPHXMouseButton.PHX_MOUSE_BUTTON_2;
  // Middle mouse button
  mbMiddle = TPHXMouseButton.PHX_MOUSE_BUTTON_3;

// Converts a virtual key code to a string
function VirtualKeyToString(const Key: TPHXVirtualKey): String;
// Converts a mouse button to a string
function MouseButtonToString(const Button: TPHXMouseButton): String;

type

PByteArray = ^TByteArray;
TByteArray = array[0..$00FFFFFF] of Byte;

// List of integer values
PIntegerList = ^TIntegerList;
TIntegerList = array[0..$00FFFFFF] of Integer;

// List of single values
PSingleList = ^TSingleList;
TSingleList = array[0..$00FFFFFF] of Single;

type

// Two dimensional integer vector
//------------------------------------------------------------------------------
TVector2i = record
  public
    X : Integer;
    Y : Integer;
  public
    // Creates a new vector
    class function Create(const AX, AY: Integer): TVector2i; overload; static;
    // Creates a new vector
    class function Create(const AX, AY: Single): TVector2i; overload; static;
    // Creates a empty vector
    class function Zero: TVector2i; static;
    // Creates a vector in the X-Axis
    class function AxisX: TVector2i; static;
    // Creates a vector in the Y-Axis
    class function AxisY: TVector2i; static;
  public
    // Get the magnitude of the vector
    function Magnitude: Single;
    // Returns the squared magnitude of this vector
    function MagnitudeSqr: Single;
    // Normalise the vector
    function Normalize: TVector2i;
    // Negate the vector
    function Negate: TVector2i;
  public
    // Vector addition
    class operator Add(const A: TVector2i; const B: TVector2i): TVector2i;
    // Vector subtraction
    class operator Subtract(const A: TVector2i; const B: TVector2i): TVector2i;
    // Vector multiplication with a scalar
    class operator Multiply(const A: TVector2i; const B: Integer): TVector2i;
  end;

PVector2i = ^TVector2i;

PVectorList2i = ^TVectorList2i;
TVectorList2i = array[0..$00FFFFFF] of TVector2i;

// Two dimensional single precision float vector
//------------------------------------------------------------------------------
TVector2f = record
  public
    X: Single;
    Y: Single;
  public
    // Creates a new vector
    class function Create(const AX, AY: Single): TVector2f; static;
    // Creates a empty vector
    class function Zero: TVector2f; static;
    // Creates a vector in the X-Axis
    class function AxisX: TVector2f; static;
    // Creates a vector in the Y-Axis
    class function AxisY: TVector2f; static;
  public
    // Get the magnitude of the vector
    function Magnitude: Single;
    // Returns the squared magnitude of this vector
    function MagnitudeSqr: Single;
    // Normalise the vector
    function Normalize: TVector2f;
    // Negate the vector
    function Negate: TVector2f;
  public
    // Addition
    class operator Add(const A: TVector2f; const B: TVector2f): TVector2f;
    // Subtraction
    class operator Subtract(const A: TVector2f; const B: TVector2f): TVector2f;
    // Multiplication with a scalar
    class operator Multiply(const A: TVector2f; const B: Single): TVector2f;
  end;

PVector2f = ^TVector2f;

PVectorList2f = ^TVectorList2f;
TVectorList2f = array[0..$00FFFFFF] of TVector2f;

// Two dimensional double precision float vector
//------------------------------------------------------------------------------
TVector2d = record
  public
    X: Double;
    Y: Double;
  public
    // Creates a new vector
    class function Create(const AX, AY: Double): TVector2d; static;
    // Creates a empty vector
    class function Zero: TVector2d; static;
    // Creates a vector in the X-Axis
    class function AxisX: TVector2d; static;
    // Creates a vector in the Y-Axis
    class function AxisY: TVector2d; static;
  public
    // Get the magnitude of the vector
    function Magnitude: Double;
    // Returns the squared magnitude of this vector
    function MagnitudeSqr: Double;
    // Normalise the vector
    function Normalize: TVector2d;
    // Negate the vector
    function Negate: TVector2d;
  public
    // Addition
    class operator Add(const A: TVector2d; const B: TVector2d): TVector2d;
    // Subtraction
    class operator Subtract(const A: TVector2d; const B: TVector2d): TVector2d;
    // Multiplication with a scalar
    class operator Multiply(const A: TVector2d; const B: Double): TVector2d;
  end;

PVector2d = ^TVector2d;

PVectorList2d = ^TVectorList2d;
TVectorList2d = array[0..$00FFFFFF] of TVector2d;

// Three diminsional integer vector
//------------------------------------------------------------------------------
TVector3i = record
  public
    X : Integer;
    Y : Integer;
    Z : Integer;
  public
    // Creates a new vector
    class function Create(const AX, AY, AZ: Integer): TVector3i; overload; static;
    // Creates a new vector
    class function Create(const AX, AY, AZ: Single): TVector3i; overload; static;
    // Creates a empty vector
    class function Zero: TVector3i; static;
    // Creates a vector in the X-Axis
    class function AxisX: TVector3i; static;
    // Creates a vector in the Y-Axis
    class function AxisY: TVector3i; static;
    // Creates a vector in the Z-Axis
    class function AxisZ: TVector3i; static;
  public
    // Get the magnitude of the vector
    function Magnitude: Single;
    // Returns the squared magnitude of this vector
    function MagnitudeSqr: Single;
    // Returns the normalised version of this vector
    function Normalize: TVector3i;
  public
    // Vector addition
    class operator Add(const A: TVector3i; const B: TVector3i): TVector3i;
    // Vector subtraction
    class operator Subtract(const A: TVector3i; const B: TVector3i): TVector3i;
    // Vector multiplication with a scalar
    class operator Multiply(const A: TVector3i; const B: Integer): TVector3i;
  end;

PVector3i = ^TVector3i;

PVectorList3i = ^TVectorList2i;
TVectorList3i = array[0..$00FFFFFF] of TVector3i;

// Three dimensional single precision vector
//------------------------------------------------------------------------------
TVector3f = record
  public
    X : Single;
    Y : Single;
    Z : Single;
  public
    // Creates a new vector
    class function Create(const AX, AY, AZ: Single): TVector3f; static;
    // Creates a empty vector
    class function Zero: TVector3f; static;
    // Creates a vector in the X-Axis
    class function AxisX: TVector3f; static;
    // Creates a vector in the Y-Axis
    class function AxisY: TVector3f; static;
    // Creates a vector in the Z-Axis
    class function AxisZ: TVector3f; static;
  public
    // Returns the magnitude of this vector
    function Magnitude: Single;
    // Returns the squared magnitude of this vector
    function MagnitudeSqr: Single;
    // Returns the normalised version of this vector
    function Normalize: TVector3f;
    // Negate the vector
    function Negate: TVector3f;
    // Returns the dot product of this and another vector
    function Dot(const Vector: TVector3f): Single;
    // Returns the cross product of this and another vector
    function Cross(const Vector: TVector3f): TVector3f;
  public
    // Vector addition
    class operator Add(const A: TVector3f; const B: TVector3f): TVector3f;
    // Vector subtraction
    class operator Subtract(const A: TVector3f; const B: TVector3f): TVector3f;
    // Multiply two vectors, returns the vector dot
    class operator Multiply(const A, B: TVector3f): Single;
    // Vector multiplication with a scalar
    class operator Multiply(const A: TVector3f; const B: Single): TVector3f;
  end;

PVector3f = ^TVector3f;

PVectorList3f = ^TVectorList3f;
TVectorList3f = array[0..$00FFFFFF] of TVector3f;

// Four diminsional integer vector
//------------------------------------------------------------------------------
TVector4i = record
  public
    X : Integer;
    Y : Integer;
    Z : Integer;
    W : Integer;
  public
  end;

PVector4i = ^TVector4i;

PVectorList4i = ^TVectorList4i;
TVectorList4i = array[0..$00FFFFFF] of TVector4i;

// Four diminsional float vector
//------------------------------------------------------------------------------
TVector4f = record
  public
    X : Single;
    Y : Single;
    Z : Single;
    W : Single;
  public
  end;

PVector4f = ^TVector4f;

PVectorList4f = ^TVectorList3f;
TVectorList4f = array[0..$00FFFFFF] of TVector4f;


function Vector2i(const X, Y: Integer): TVector2i; overload;
function Vector2i(const X, Y: Single): TVector2i; overload;

function Vector2f(const X, Y: Single): TVector2f; overload;
function Vector2f(const Value: TVector3f): TVector2f; overload;

function Vector3i(const X, Y, Z: Integer): TVector3i; overload;
function Vector3i(const X, Y, Z: Single): TVector3i; overload;

function Vector3f(const X, Y, Z: Single): TVector3f; overload;
function Vector3f(const Value: TVector2f): TVector3f; overload;

function Vector4i(const X, Y, Z, W: Integer): TVector4i; overload;
function Vector4i(const X, Y, Z, W: Single): TVector4i; overload;

function Vector4f(const X, Y, Z, W: Single): TVector4f; overload;

type

// Range describes a span between a minimum and maximum value
//------------------------------------------------------------------------------
TRangef = record
  public
    // Minumum value
    Min: Single;
    // Maximum value
    Max: Single;
  public
    // Create a new range
    class function Create(const AMin, AMax: Single): TRangef; static;
  end;

// Create a new range
function Rangef(const AMin, AMax: Single): TRangef;


type

// Elements for one dimension array access for a matrix
TMatrix3v = array[0..8] of Single;
// Elements for two dimension array access for a matrix
TMatrix3m = array[0..2, 0..2] of Single;

// Three dimensional matrix
//------------------------------------------------------------------------------
TMatrix3f = record
  public
    // Returns the zero matrix
    class function Zero: TMatrix3f; static; inline;
    // Returns the identity matrix
    class function Identity: TMatrix3f; static; inline;
  public
    case Integer of
    // Element access
    0: (M11, M12, M13: Single;
        M21, M22, M23: Single;
        M31, M32, M33: Single);
    // One dimensional array
    1: (v: TMatrix3v);
    // Two dimensional array
    2: (m: TMatrix3m);
  end;

// Pointer to a TMatrix3
PMatrix3 = ^TMatrix3f;

// Elements for one dimension array access for a matrix
TMatrix4fv = array[0..15] of Single;
// Elements for two dimension array access for a matrix
TMatrix4fm = array[0..3, 0..3] of Single;

// Four dimensional matrix
//------------------------------------------------------------------------------
TMatrix4f = record
  public
    // Returns the zero matrix
    class function Zero: TMatrix4f; static; inline;
    // Returns the identity matrix
    class function Identity: TMatrix4f; static; inline;
  public
    // Multiply two matricies
    class operator Multiply(const A: TMatrix4f; const B: TMatrix4f): TMatrix4f;
    // Transform a vector by this matrix
    class operator Multiply(const A: TMatrix4f; const B: TVector3f): TVector3f;
  public
    case Integer of
    // Element access
    0: (M11, M12, M13, M14: Single;
        M21, M22, M23, M24: Single;
        M31, M32, M33, M34: Single;
        M41, M42, M43, M44: Single);
    // One dimensional array
    1: (v: TMatrix4fv);
    // Two dimensional array
    2: (m: TMatrix4fm);
  end;

// Pointer to a TMatrix4f
PMatrix4 = ^TMatrix4f;


// RGBA color
TColor32 = Cardinal;

// 24bit color (RGB)
//------------------------------------------------------------------------------
TColor3b = record
  public
    Red: Byte;
    Green: Byte;
    Blue: Byte;
  public
  end;

// Pointer to 24bit color (RGB)
PColor3b = ^TColor3b;

// 32 bit color (RGBA)
//------------------------------------------------------------------------------
TColor4b = record
  public
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
  public
  end;

// Pointer to 32bit color (RGBA)
PColor4b = ^TColor4b;

// Three channel floating point color (RGB)
//------------------------------------------------------------------------------
TColor3f = record
  public
    // The red component.
    Red: Single;
    // The green component.
    Green: Single;
    // The blue component.
    Blue: Single;
  public
  end;

PColor3f = ^TColor3f;

PColorList3f = ^TColorList3f;
TColorList3f = array[0..$00FFFFFF] of TColor3f;

// Four channel floating point color (RGBA)
//------------------------------------------------------------------------------
TColor4f = record
  public
    // The red component.
    Red: Single;
    // The green component.
    Green: Single;
    // The blue component.
    Blue: Single;
    // The alpha component.
    Alpha: Single;
  public
    // Create a color
    class function Create(const Color: TColor32): TColor4f; overload; static;
    // Create a color
    class function Create(const Color: TColor32; const Alpha: Single): TColor4f; overload; static;
    // Create a color
    class function Create(const Red, Green, Blue: Single): TColor4f; overload; static;
    // Create a color
     class function Create(const Red, Green, Blue, Alpha: Single): TColor4f; overload; static;
  public
    // Color comparison
    class operator Equal(const A: TColor4f; const B: TColor4f): Boolean;
    // Color comparison
    class operator NotEqual(const A: TColor4f; const B: TColor4f): Boolean;
    // Color scaling
    class operator Multiply(const A: TColor4f; const B: Single): TColor4f;
             {
    // Color comparison
    class operator Equal(A: TColor4f; B: TColor4f): Boolean;
    // Color addition
    class operator Add(A: TColor4f; B: TColor4f): TColor4f;
    // Color subtraction
    class operator Subtract(A: TColor4f; B: TColor4f): TColor4f;
                          }
    // Convert the color to a RGBA color
    function ToColor: TColor32;
  public
    // Constant color
    class function None: TColor4f; overload; static;
  end;

// Pointer to TColor4f
PColor4f = ^TColor4f;

PColorList4f = ^TColorList4f;
TColorList4f = array[0..$00FFFFFF] of TColor4f;

const

// Constant colors
clrNone       : TColor4f = (Red: 000/255; Green: 000/255; Blue: 000/255; Alpha: 000/255);
clrBlack      : TColor4f = (Red: 000/255; Green: 000/255; Blue: 000/255; Alpha: 255/255);
clrMaroon     : TColor4f = (Red: 128/255; Green: 000/255; Blue: 000/255; Alpha: 255/255);
clrGreen      : TColor4f = (Red: 000/255; Green: 128/255; Blue: 000/255; Alpha: 255/255);
clrOlive      : TColor4f = (Red: 128/255; Green: 128/255; Blue: 000/255; Alpha: 255/255);
clrNavy       : TColor4f = (Red: 000/255; Green: 000/255; Blue: 128/255; Alpha: 255/255);
clrPurple     : TColor4f = (Red: 128/255; Green: 000/255; Blue: 128/255; Alpha: 255/255);
clrTeal       : TColor4f = (Red: 000/255; Green: 128/255; Blue: 128/255; Alpha: 255/255);
clrGray       : TColor4f = (Red: 128/255; Green: 128/255; Blue: 128/255; Alpha: 255/255);
clrSilver     : TColor4f = (Red: 192/255; Green: 192/255; Blue: 192/255; Alpha: 255/255);
clrRed        : TColor4f = (Red: 255/255; Green: 000/255; Blue: 000/255; Alpha: 255/255);
clrLime       : TColor4f = (Red: 000/255; Green: 255/255; Blue: 000/255; Alpha: 255/255);
clrYellow     : TColor4f = (Red: 255/255; Green: 255/255; Blue: 000/255; Alpha: 255/255);
clrBlue       : TColor4f = (Red: 000/255; Green: 000/255; Blue: 255/255; Alpha: 255/255);
clrFuchsia    : TColor4f = (Red: 255/255; Green: 000/255; Blue: 255/255; Alpha: 255/255);
clrAqua       : TColor4f = (Red: 000/255; Green: 255/255; Blue: 255/255; Alpha: 255/255);
clrWhite      : TColor4f = (Red: 255/255; Green: 255/255; Blue: 255/255; Alpha: 255/255);
clrMoneyGreen : TColor4f = (Red: 192/255; Green: 220/255; Blue: 192/255; Alpha: 255/255);
clrSkyBlue    : TColor4f = (Red: 166/255; Green: 202/255; Blue: 240/255; Alpha: 255/255);
clrCream      : TColor4f = (Red: 255/255; Green: 251/255; Blue: 240/255; Alpha: 255/255);
clrMedGray    : TColor4f = (Red: 160/255; Green: 160/255; Blue: 164/255; Alpha: 255/255);
clrCyan       : TColor4f = (Red: 000/255; Green: 255/255; Blue: 255/255; Alpha: 255/255);
clrMagenta    : TColor4f = (Red: 255/255; Green: 000/255; Blue: 255/255; Alpha: 255/255);

function Color4f(const Red, Green, Blue, Alpha: Single  ): TColor4f; overload;
function Color4f(const Color: TColor4f;  Alpha: Single  ): TColor4f; overload;
function Color4f(const Color: TColor32): TColor4f; overload;
function Color4f(const Color: TColor32; Alpha: Single): TColor4f; overload;

// Convert a color to a cardinal value
function ColorToRGBA(const Value: TColor4f): TColor32;
// Convert a color to a cardinal value
function ColorToRGB (const Value: TColor4f): TColor32;
// Convert a color to a hexadecimal string
function ColorToHex(const Value: TColor4f): String;
// Convert a hexadecimal string to a color
function HexToColor(const Value: String): TColor4f;

// Convert a color to and from a string
function ColorToString(const Value: TColor4f): String;
// Convert a color to and from a string
function StringToColor(const Value: String  ): TColor4f;

function SameColor(const ColorA, ColorB: TColor4f): Boolean;

type

// Axis aligned bounding box with integer coordinates
//------------------------------------------------------------------------------
TBoxi = record
  MinX : Integer;
  MaxX : Integer;
  MinY : Integer;
  MaxY : Integer;
  MinZ : Integer;
  MaxZ : Integer;
end;

PBoxi = ^TBoxi;

// Axis aligned bounding box with float coordinates
//------------------------------------------------------------------------------
TBoxf = record
  MinX : Single;
  MaxX : Single;
  MinY : Single;
  MaxY : Single;
  MinZ : Single;
  MaxZ : Single;
end;

PBoxf = ^TBoxf;




{$REGION 'TRect'}

// Integer rectangle
//------------------------------------------------------------------------------
TRectI = record
  public
    Left  : Integer;
    Top   : Integer;
    Right : Integer;
    Bottom: Integer;
  public
    // Create a new rect
    class function Create(const ALeft, ATop, ARight, ABottom: Integer): TRectI; static;
  public
    // Inflate the rext
    function Inflate(const DX, DY: Integer): TRectI;

    // Get the width of the rect
    function GetWidth: Integer;
    // Get the height of the rect
    function GetHeight: Integer;

    // Get the width of the rect
    property Width: Integer read GetWidth;
    // Get the height of the rect
    property Height: Integer read GetHeight;
  end;

PRectI = ^TRectI;

// Create a new rect
function Recti(const Left, Top, Right, Bottom: Integer): TRectI;

type

// Floating point rectangle
//------------------------------------------------------------------------------
TRectf = record
  public
    Left  : Single;
    Top   : Single;
    Right : Single;
    Bottom: Single;
  public
    // Create a new rect
    class function Create(const ALeft, ATop, ARight, ABottom: Single): TRectf; static;
  public
    // Multiplication by scalar
    class operator Multiply(const A: TRectF; const B: Single): TRectF;
    // Division by scalar
    class operator Divide(const A: TRectF; const B: Single): TRectF;
  public
    // Inflate the rext
    function Inflate(const DX, DY: Single): TRectf;
    // Get the width of the rect
    function GetWidth: Single;
    // Get the height of the rect
    function GetHeight: Single;

    // Get the width of the rect
    property Width: Single read GetWidth;
    // Get the height of the rect
    property Height: Single read GetHeight;
  end;
PRectf = ^TRectf;

// Creates a new rect
function Rectf(const Left, Top, Right, Bottom: Single): TRectf;


{$ENDREGION}

type

// Viewport struct
//------------------------------------------------------------------------------
TViewport = record
  public
    // Horisontal position of the viewport
    X: Integer;
    // Vertical position of the viewport
    Y: Integer;
    // Horisontal size of the viewport
    Width: Integer;
    // Vertical size of the viewport
    Height: Integer;
    // The minimum depth of the clip volume.
    MinDepth: Single;
    // The maximum depth of the clip volume.
    MaxDepth: Single;
  private
    function GetBounds: TRectf;
  public
    // Creates a new viewport struct with default deapth values
    class function Create(const AX, AY, AWidth, AHeight: Integer): TViewport; overload; static;
    // Creates a new viewport struct
    class function Create(const AX, AY, AWidth, AHeight: Integer; const AMinDepth, AMaxDepth: Single): TViewport; overload; static;

    // Returns the bounding rectangle for the viewport
    property Bounds: TRectf read GetBounds;
  end;

PViewport = ^TViewport;

// Integer size struct
//------------------------------------------------------------------------------
TSizei = record
  public
    // Size in the horisontal dimension
    Width: Integer;
    // Size in the vertical dimension
    Height: Integer;
  public
    // Creates a new size struct
    class function Create(const AWidth, AHeight: Integer): TSizei; overload; static;
  end;

// Floating size struct
//------------------------------------------------------------------------------
TSizef = record
  public
     // Size in the horisontal dimension
   Width: Single;
    // Size in the vertical dimension
    Height: Single;
  public
    // Creates a new size struct
    class function Create(const AWidth, AHeight: Single): TSizef; overload; static;
  end;

//  Defines the type of primitive to be rendered.
//------------------------------------------------------------------------------
TPHXPrimitiveType = (
  // Draw the verticies as points.
  PHX_POINTS,
  // Draw the verticies as lines
  PHX_LINES,
  // Draw the verticeis as triangles
  PHX_TRIANGLES,
  // Draw the verticies as a triangle strip
  PHX_TRIANGLE_STRIP,
  // Draw the verticies as a triangle fan
  PHX_TRIANGLE_FAN
);

// Changes the current blend mode
//------------------------------------------------------------------------------
TPHXBlendMode = (
  // No blendmode
  bmNormal,
  // Alpha blending
  bmAlpha,
  // Additive blending
  bmAdditive,
  // Multiplication blendinjg
  bmMultiply,
  bmModulate,
  bmScreen
);

{$REGION 'TPHXVertex'}


// http://www.gamedev.net/topic/147325-mimicing-dxquotflexible-vertex-formatquot/#j_content
//http://www.gamedev.net/topic/367617-flexible-vertex-format-on-the-fly/

// http://www.gamedev.net/topic/381492-vertex-declarations-in-opengl-and-directx-/

// TPHXVertexUseage
TPHXVertexComponent = (
  vcPosition,
  vcNormal,
  vcColor,
  // Texture coordinate 1
  vcCoord1,
  // Texture coordinate 2
  vcCoord2
);

TVertexComponents = set of TPHXVertexComponent;

TPHXVertexOffsets = array[TPHXVertexComponent] of Integer;

TVertexKind = (
  // Single
  vctFloat,
  vctDouble,
  vctFloat3,
  vctFloat4,
  vctColor,
  vctMatrix3,
  vctMatrix4
);

// http://openflipper.org/Documentation/latest/classACG_1_1VertexDeclaration.html
// http://gamedev.stackexchange.com/questions/27394/what-is-a-better-abstraction-layer-for-d3d9-and-opengl-vertex-data-management
// http://www.gamedev.net/topic/381492-vertex-declarations-in-opengl-and-directx-/

//------------------------------------------------------------------------------
TPHXVertexDeclaration = record
  public
    //
    Components: TVertexComponents;
    // Size of the components
    Size: Integer;
    // Offset of each vertex component
    Offsets: TPHXVertexOffsets;

    function GetVertexSize: Integer;
  public
    class function Create: TPHXVertexDeclaration; overload; static;
    class function Create(Components: TVertexComponents): TPHXVertexDeclaration; overload; static;

    procedure Add(const Component: TPHXVertexComponent; const Offset: Integer);
  public
  end;

const
  fvfStandard = [vcPosition, vcNormal, vcColor, vcCoord1];



const
  // Offset of texture cordinates in the vertex structure
  VertexOffsetTexCoord: Pointer = Pointer(0);
  // Offset of colors in the vertex structure
  VertexOffsetColor: Pointer = Pointer(SizeOf(TVector2f));
  // Offset of normals corrdinates in the vertex structure
  VertexOffsetNormal: Pointer = Pointer(SizeOf(TVector2f) + SizeOf(TColor4f));
   // Offset of position in the vertex structure
  VertexOffsetPosition: Pointer = Pointer(SizeOf(TVector2f) + SizeOf(TColor4f) + SizeOf(TVector3f));

type

// Phoenix vertex format, comparable with the interleaved format: GL_T2F_C4F_N3F_V3F
TPHXVertex = record
  public
    // The texture coordinate
    TexCoord: TVector2f;
    // The color
    Color: TColor4f;
    // The normal
    Normal: TVector3f;
    // The position
    Position: TVector3f;
  public
    // Return the vertex declaration for the vertex
    class function Declaration: TPHXVertexDeclaration; static;
  end;

// Pointer to a vertex
PPHXVertex = ^TPHXVertex;

PVertexList = ^TVertexList;
TVertexList = array[0..$00FFFFFF] of TPHXVertex;

// Container for verticies
//------------------------------------------------------------------------------
TPHXVertexList = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PVertexList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXVertex;
    procedure SetItem(Index: Integer; const Value: TPHXVertex);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create an empty vertex list
    constructor Create; overload;
    // Create an vertex list with a default capacity
    constructor Create(ACapacity : Integer); overload;
    // Free the allocated data
    destructor Destroy; override;

    // Clear the vertex list
    procedure Clear;

    // Add a new vertex to the list
    procedure Add(const Value: TPHXVertex ); overload;

    // Enshures room for a number verticies and returns the index of the first vertex
    function Alloc(NumVerticies: Integer): Integer;

    // Transform all verticies by a matrix
    procedure Transform(const Matrix: TMatrix4f);

    // The current number of items in the vertex list
    property Count: Integer read FCount write SetCount;
    // The capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;

    // Pointer to the internal list
    property List: PVertexList read FList;
    // The items in the vertex list
    property Items[Index: Integer]: TPHXVertex read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXIndex'}

PPHXIndex = ^TPHXIndex;
TPHXIndex = type Cardinal;

PIndexList = ^TIndexList;
TIndexList = Array[0..$00FFFFFF] of TPHXIndex;

// Container for vertex indicies
//------------------------------------------------------------------------------
TPHXIndexList = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PIndexList;

    procedure Grow;

    function  GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; const Value: Cardinal);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create an empty vertex
    constructor Create; overload; virtual;
    // Create an vertex list with a default capacity
    constructor Create(ACapacity : Integer); overload;
    // Free the allocated data
    destructor Destroy; override;

    // Clear the index list
    procedure Clear;

    // Add a new index to the list
    procedure Add(const Value: Cardinal); overload;

    // Enshures room for a number indicies and returns the index of the first element
    function Alloc(NumIndicies: Integer): Integer;

    // The current number of items in the index list
    property Count: Integer read FCount write SetCount;
    // The capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;

    // Pointer to the internal list
    property List: PIndexList read FList;
    // The items in the vertex list
    property Items[Index: Integer]: Cardinal read GetItem Write SetItem; default;
  end;

{$ENDREGION}

const

{$REGION 'Vector constants'}

Vector2i_Zero  : TVector2i = (X: 0; Y: 0);
Vector2i_One   : TVector2i = (X: 1; Y: 1);
Vector2i_AxisX : TVector2i = (X: 1; Y: 0);
Vector2i_AxisY : TVector2i = (X: 0; Y: 1);

Vector2f_Zero  : TVector2f = (X: 0.0; Y: 0.0);
Vector2f_One   : TVector2f = (X: 1.0; Y: 1.0);
Vector2f_AxisX : TVector2f = (X: 1.0; Y: 0.0);
Vector2f_AxisY : TVector2f = (X: 0.0; Y: 1.0);

Vector3i_Zero  : TVector3i = (X: 0; Y: 0; Z: 0);
Vector3i_One   : TVector3i = (X: 1; Y: 1; Z: 1);
Vector3i_AxisX : TVector3i = (X: 1; Y: 0; Z: 0);
Vector3i_AxisY : TVector3i = (X: 0; Y: 1; Z: 0);
Vector3i_AxisZ : TVector3i = (X: 0; Y: 0; Z: 1);


Vector3f_Zero  : TVector3f = (X: 0.0; Y: 0.0; Z: 0.0);
Vector3f_One   : TVector3f = (X: 1.0; Y: 1.0; Z: 1.0);
Vector3f_AxisX : TVector3f = (X: 1.0; Y: 0.0; Z: 0.0);
Vector3f_AxisY : TVector3f = (X: 0.0; Y: 1.0; Z: 0.0);
Vector3f_AxisZ : TVector3f = (X: 0.0; Y: 0.0; Z: 1.0);

{$ENDREGION}

{$REGION 'Matrix constants'}

//------------------------------------------------------------------------------
Matrix3f_Zero: TMatrix3f = (v:(
  0, 0, 0,
  0, 0, 0,
  0, 0, 0
));

//------------------------------------------------------------------------------
Matrix3f_Identity: TMatrix3f = (v:(
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
));

//------------------------------------------------------------------------------
Matrix4f_Zero: TMatrix4f = (v:(
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0
));

//------------------------------------------------------------------------------
Matrix4f_Identity : TMatrix4f = (v:(
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1
));

{$ENDREGION}



function GreatestCommonDivisior(const A, B: Integer): Integer;


// Writes a string to a stream
procedure StreamWriteString(Stream: TStream; const Value: WideString ); overload;
// Writes a string to a stream
procedure StreamWriteString(Stream: TStream; const Value: AnsiString ); overload;

// Read a string to a stream
procedure StreamReadString(Stream: TStream; out Value: WideString ); overload;
// Read a string to a stream
procedure StreamReadString(Stream: TStream; out Value: AnsiString ); overload;
{$IFNDEF FPC}
// Read a string to a stream
procedure StreamReadString(Stream: TStream; out Value: String ); overload;
{$ENDIF}

implementation

uses phxMath;

//------------------------------------------------------------------------------
function GreatestCommonDivisior(const A, B: Integer): Integer;
begin
  if B = 0 then
  begin
    Result:= A;
  end else
  begin
    result:= GreatestCommonDivisior(B, A mod b);
  end;
end;

//------------------------------------------------------------------------------
function VirtualKeyToString(const Key: TPHXVirtualKey): String;
begin
  Result:= Chr( Ord(Key) );

  case Key of
    VK_UNKNOWN      : Result:= 'Unknown';
    VK_SPACE        : Result:= 'Spacebar';
    VK_SPECIAL      : Result:= 'Special';
    VK_ESC          : Result:= 'Escape';
    VK_F1           : Result:= 'F1';
    VK_F2           : Result:= 'F2';
    VK_F3           : Result:= 'F3';
    VK_F4           : Result:= 'F4';
    VK_F5           : Result:= 'F5';
    VK_F6           : Result:= 'F6';
    VK_F7           : Result:= 'F7';
    VK_F8           : Result:= 'F8';
    VK_F9           : Result:= 'F9';
    VK_F10          : Result:= 'F10';
    VK_F11          : Result:= 'F11';
    VK_F12          : Result:= 'F12';
    VK_F13          : Result:= 'F13';
    VK_F14          : Result:= 'F14';
    VK_F15          : Result:= 'F15';
    VK_F16          : Result:= 'F16';
    VK_F17          : Result:= 'F17';
    VK_F18          : Result:= 'F18';
    VK_F19          : Result:= 'F19';
    VK_F20          : Result:= 'F20';
    VK_F21          : Result:= 'F21';
    VK_F22          : Result:= 'F22';
    VK_F23          : Result:= 'F23';
    VK_F24          : Result:= 'F24';
    VK_F25          : Result:= 'F25';
    VK_UP           : Result:= 'Up Arrow';
    VK_DOWN         : Result:= 'Down Arrow';
    VK_LEFT         : Result:= 'Left Arrow';
    VK_RIGHT        : Result:= 'Right Arrow';
    VK_LSHIFT       : Result:= 'Shift';
    VK_RSHIFT       : Result:= 'Shift';
    VK_LCTRL        : Result:= 'Ctrl';
    VK_RCTRL        : Result:= 'Ctrl';
    VK_LALT         : Result:= 'Alt';
    VK_RALT         : Result:= 'Alt';
    VK_TAB          : Result:= 'Tab';
    VK_RETURN       : Result:= 'Return';
    VK_BACKSPACE    : Result:= 'Backspace';
    VK_INSERT       : Result:= 'Insert';
    VK_DEL          : Result:= 'Delete';
    VK_PAGEUP       : Result:= 'Page Up';
    VK_PAGEDOWN     : Result:= 'Page Down';
    VK_HOME         : Result:= 'Home';
    VK_END          : Result:= 'End';
    VK_NUM_0        : Result:= 'Num Pad 0';
    VK_NUM_1        : Result:= 'Num Pad 1';
    VK_NUM_2        : Result:= 'Num Pad 2';
    VK_NUM_3        : Result:= 'Num Pad 3';
    VK_NUM_4        : Result:= 'Num Pad 4';
    VK_NUM_5        : Result:= 'Num Pad 5';
    VK_NUM_6        : Result:= 'Num Pad 6';
    VK_NUM_7        : Result:= 'Num Pad 7';
    VK_NUM_8        : Result:= 'Num Pad 8';
    VK_NUM_9        : Result:= 'Num Pad 9';
    VK_NUM_DIVIDE   : Result:= 'Num Pad /';
    VK_NUM_MULTIPLY : Result:= 'Num Pad *';
    VK_NUM_SUBTRACT : Result:= 'Num Pad -';
    VK_NUM_ADD      : Result:= 'Num Pad +';
    VK_NUM_DECIMAL  : Result:= 'Num Pad ,';
    VK_NUM_EQUAL    : Result:= 'Num Pad =';
    VK_NUM_ENTER    : Result:= 'Num Pad Return';
  end;
end;

//------------------------------------------------------------------------------
function MouseButtonToString(const Button: TPHXMouseButton): String;
begin
  Result:= '';

  case Button of
    PHX_MOUSE_BUTTON_1   : Result:= 'Left Mouse';
    PHX_MOUSE_BUTTON_2  : Result:= 'Right Mouse';
    PHX_MOUSE_BUTTON_3  : Result:= 'Middle Mouse';
    PHX_MOUSE_BUTTON_4      : Result:= 'Mouse Button 4';
    PHX_MOUSE_BUTTON_5      : Result:= 'Mouse Button 5';
    PHX_MOUSE_BUTTON_6      : Result:= 'Mouse Button 6';
    PHX_MOUSE_BUTTON_7      : Result:= 'Mouse Button 7';
    PHX_MOUSE_BUTTON_8      : Result:= 'Mouse Button 8';
  end;
end;

(*
//------------------------------------------------------------------------------
procedure TStreamEx.WriteString(const Text: string);
begin
  StreamWriteString(Self, TExt);
end;

//------------------------------------------------------------------------------
procedure TStreamEx.ReadString(out Text: String);
begin
  StreamReadString(Self, Text);
end;
*)

// Writes a string to a stream
//------------------------------------------------------------------------------
procedure StreamWriteString(Stream: TStream; const Value: WideString );
var Header: array[1..9] of AnsiChar;
var Len   : Cardinal;
var Data  : WideString;
begin
  Header:= 'PHXSTRING';
  Data  := WideString(Value);
  Len   := Length(Value);

  Stream.Write(Header, 9);
  Stream.Write(Len   , 4);

  if Len > 0 then
  begin
    Stream.WriteBuffer(Data[1], 2 * Len);
  end;
end;

//------------------------------------------------------------------------------
procedure StreamWriteString(Stream: TStream; const Value: AnsiString );
begin
  StreamWriteString(Stream, WideString(Value));
end;

//------------------------------------------------------------------------------
procedure StreamReadString(Stream: TStream; out Value: WideString );
var Header: array[1..9] of AnsiChar;
var Len   : Cardinal;
var Data  : WideString;
begin
  Header:= #0#0#0#0#0#0#0#0#0;
  Data  := '';
  Len   := 0;

  Stream.Read(Header, 9);
  Stream.Read(Len   , 4);

  if (Header <> 'PHXSTRING') then
  begin
    raise EStreamError.Create('Not a valid string.');
  end;
  if (Stream.Position + Len) > Stream.Size then
  begin
    raise EStreamError.Create('String length larger then stream length');
  end;

  if Len > 0 then
  begin
    SetLength(Data, len);

    Stream.ReadBuffer(Data[1], 2 * Len);

    Value:= WideString(Data);
  end else
  begin
    Value:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure StreamReadString(Stream: TStream; out Value: AnsiString );
var Data: WideString;
begin
  StreamReadString(Stream, Data);

  Value:= AnsiString(Data);
end;

{$IFNDEF FPC}
//------------------------------------------------------------------------------
procedure StreamReadString(Stream: TStream; out Value: String );
var Data: WideString;
begin
  StreamReadString(Stream, Data);

  Value:= String(Data);
end;

{$ENDIF}




{$REGION 'TVector2i'}

// Vector2i
//==============================================================================
function Vector2i(const X, Y: Integer): TVector2i;
begin
  Result.X:= X;
  Result.Y:= Y;
end;

//------------------------------------------------------------------------------
function Vector2i(const X, Y: Single): TVector2i;
begin
  Result.X:= Trunc(X);
  Result.Y:= Trunc(Y);
end;

//------------------------------------------------------------------------------
class function TVector2i.Create(const AX, AY: Integer): TVector2i;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

//------------------------------------------------------------------------------
class function TVector2i.Create(const AX, AY: Single): TVector2i;
begin
  Result.X:= Trunc(AX);
  Result.Y:= Trunc(AY);
end;

//------------------------------------------------------------------------------
class function TVector2i.Zero: TVector2i;
begin
  Result.X:= 0;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2i.AxisX: TVector2i;
begin
  Result.X:= 1;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2i.AxisY: TVector2i;
begin
  Result.X:= 0;
  Result.Y:= 1;
end;

//------------------------------------------------------------------------------
function TVector2i.Negate: TVector2i;
begin
  Result.x:= -x;
  Result.y:= -y;
end;

//------------------------------------------------------------------------------
function TVector2i.Normalize: TVector2i;
var M: Single;
begin
  M:= Magnitude();

  if M >= EPSILON then
  begin
    Result.X:= Trunc(X / M);
    Result.Y:= Trunc(Y / M);
  end else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;

//------------------------------------------------------------------------------
class operator TVector2i.Add(const A: TVector2i; const B: TVector2i): TVector2i;
begin
  Result.X:= A.X + B.X;
  Result.Y:= A.Y + B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2i.Subtract(const A: TVector2i; const B: TVector2i): TVector2i;
begin
  Result.X:= A.X - B.X;
  Result.Y:= A.Y - B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2i.Multiply(const A: TVector2i; const B: Integer): TVector2i;
begin
  Result.X:= A.X * B;
  Result.Y:= A.Y * B;
end;

//------------------------------------------------------------------------------
function TVector2i.Magnitude: Single;
begin
  Result:=Sqrt( Sqr(X) + Sqr(Y));
end;

//------------------------------------------------------------------------------
function TVector2i.MagnitudeSqr: Single;
begin
  Result:=Sqr(X) + Sqr(Y);
end;

{$ENDREGION}

{$REGION 'TVector2f'}

// TVector2f
//==============================================================================
function Vector2f(const X, Y: Single): TVector2f;
begin
  Result.X:= X;
  Result.Y:= Y;
end;

//------------------------------------------------------------------------------
function Vector2f(const Value: TVector3f): TVector2f; overload;
begin
  Result.X:= Value.X;
  Result.Y:= Value.Y;
end;

//------------------------------------------------------------------------------
class function TVector2f.Create(const AX, AY: Single): TVector2f;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

//------------------------------------------------------------------------------
class function TVector2f.Zero: TVector2f;
begin
  Result.X:= 0;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2f.AxisX: TVector2f;
begin
  Result.X:= 1;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2f.AxisY: TVector2f;
begin
  Result.X:= 0;
  Result.Y:= 1;
end;

//------------------------------------------------------------------------------
function TVector2f.Magnitude: Single;
begin
  Result:=Sqrt( Sqr(X) + Sqr(Y));
end;

//------------------------------------------------------------------------------
function TVector2f.MagnitudeSqr: Single;
begin
  Result:=Sqr(X) + Sqr(Y);
end;

//------------------------------------------------------------------------------
function TVector2f.Negate: TVector2f;
begin
  Result.x:= -x;
  Result.y:= -y;
end;

//------------------------------------------------------------------------------
function TVector2f.Normalize: TVector2f;
var M: Single;
begin
  M:= Magnitude();

  if M >= EPSILON then
  begin
    Result.X:= X / M;
    Result.Y:= Y / M;
  end else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;

//------------------------------------------------------------------------------
class operator TVector2f.Add(const A: TVector2f; const B: TVector2f): TVector2f;
begin
  Result.X:= A.X + B.X;
  Result.Y:= A.Y + B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2f.Subtract(const A: TVector2f; const B: TVector2f): TVector2f;
begin
  Result.X:= A.X - B.X;
  Result.Y:= A.Y - B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2f.Multiply(const A: TVector2f; const B: Single): TVector2f;
begin
  Result.X:= A.X * B;
  Result.Y:= A.Y * B;
end;



{$ENDREGION}

{$REGION 'TVector2f'}

// TVector2f
//==============================================================================
class function TVector2d.Create(const AX, AY: Double): TVector2d;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

//------------------------------------------------------------------------------
class function TVector2d.Zero: TVector2d;
begin
  Result.X:= 0;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2d.AxisX: TVector2d;
begin
  Result.X:= 1;
  Result.Y:= 0;
end;

//------------------------------------------------------------------------------
class function TVector2d.AxisY: TVector2d;
begin
  Result.X:= 0;
  Result.Y:= 1;
end;

//------------------------------------------------------------------------------
function TVector2d.Magnitude: Double;
begin
  Result:=Sqrt( Sqr(X) + Sqr(Y));
end;

//------------------------------------------------------------------------------
function TVector2d.MagnitudeSqr: Double;
begin
  Result:=Sqr(X) + Sqr(Y);
end;

//------------------------------------------------------------------------------
function TVector2d.Negate: TVector2d;
begin
  Result.x:= -x;
  Result.y:= -y;
end;

//------------------------------------------------------------------------------
function TVector2d.Normalize: TVector2d;
var M: Single;
begin
  M:= Magnitude();

  if M >= EPSILON then
  begin
    Result.X:= X / M;
    Result.Y:= Y / M;
  end else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;

//------------------------------------------------------------------------------
class operator TVector2d.Add(const A: TVector2d; const B: TVector2d): TVector2d;
begin
  Result.X:= A.X + B.X;
  Result.Y:= A.Y + B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2d.Subtract(const A: TVector2d; const B: TVector2d): TVector2d;
begin
  Result.X:= A.X - B.X;
  Result.Y:= A.Y - B.Y;
end;

//------------------------------------------------------------------------------
class operator TVector2d.Multiply(const A: TVector2d; const B: Double): TVector2d;
begin
  Result.X:= A.X * B;
  Result.Y:= A.Y * B;
end;

{$ENDREGION}



{$REGION 'TVector3i'}

// TVector3i
//==============================================================================
function Vector3i(const X, Y, Z: Integer ): TVector3i;
begin
  Result.X:= X;
  Result.Y:= Y;
  Result.Z:= Z;
end;

//------------------------------------------------------------------------------
function Vector3i(const X, Y, Z: Single): TVector3i; overload;
begin
  Result.X:= Trunc(X);
  Result.Y:= Trunc(Y);
  Result.Z:= Trunc(Z);
end;

//------------------------------------------------------------------------------
class function TVector3i.Create(const AX, AY, AZ: Integer): TVector3i;
begin
  Result.X:= AX;
  Result.Y:= AY;
  Result.Z:= AZ;
end;

//------------------------------------------------------------------------------
class function TVector3i.Create(const AX, AY, AZ: Single): TVector3i;
begin
  Result.X:= Trunc(AX);
  Result.Y:= Trunc(AY);
  Result.Z:= Trunc(AZ);
end;

//------------------------------------------------------------------------------
class function TVector3i.Zero: TVector3i;
begin
  Result.X:= 0;
  Result.Y:= 0;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3i.AxisX: TVector3i;
begin
  Result.X:= 1;
  Result.Y:= 0;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3i.AxisY: TVector3i;
begin
  Result.X:= 0;
  Result.Y:= 1;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3i.AxisZ: TVector3i;
begin
  Result.X:= 0;
  Result.Y:= 0;
  Result.Z:= 1;
end;

//------------------------------------------------------------------------------
class operator TVector3i.Add(const A: TVector3i; const B: TVector3i): TVector3i;
begin
  Result.X:= A.X + B.X;
  Result.Y:= A.Y + B.Y;
  Result.Z:= A.Z + B.Z;
end;

//------------------------------------------------------------------------------
class operator TVector3i.Subtract(const A: TVector3i; const B: TVector3i): TVector3i;
begin
  Result.X:= A.X - B.X;
  Result.Y:= A.Y - B.Y;
  Result.Z:= A.Z - B.Z;
end;

//------------------------------------------------------------------------------
class operator TVector3i.Multiply(const A: TVector3i; const B: Integer): TVector3i;
begin
  Result.X:= A.X * B;
  Result.Y:= A.Y * B;
  Result.Z:= A.Z * B;
end;

//------------------------------------------------------------------------------
function TVector3i.Magnitude: Single;
begin
  Result:=Sqrt( Sqr(X) + Sqr(Y) + Sqr(Z) );
end;

//------------------------------------------------------------------------------
function TVector3i.MagnitudeSqr: Single;
begin
  Result:=Sqr(X) + Sqr(Y) + Sqr(Z);
end;

//------------------------------------------------------------------------------
function TVector3i.Normalize: TVector3i;
var Magnitude: Single;
begin
  Magnitude:= Sqrt( Sqr(X) + Sqr(Y) + Sqr(Z));

  if Magnitude <> 0 then
  begin
    Result.X:= Trunc(X / Magnitude);
    Result.Y:= Trunc(Y / Magnitude);
    Result.Z:= Trunc(Z / Magnitude);
  end else
  // Magnitude is zero, return the empty vector
  begin
    Result:= TVector3i.Zero;
  end;
end;

{$ENDREGION}

{$REGION 'TVector3f'}

// TVector3f
//==============================================================================
function Vector3f(const X, Y, Z: Single): TVector3f;
begin
  Result.X:= X;
  Result.Y:= Y;
  Result.Z:= Z;
end;

//------------------------------------------------------------------------------
function Vector3f(const Value: TVector2f): TVector3f; overload;
begin
  Result.X:= Value.X;
  Result.Y:= Value.Y;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3f.Create(const AX, AY, AZ: Single): TVector3f;
begin
  Result.X:= AX;
  Result.Y:= AY;
  Result.Z:= AZ;
end;

//------------------------------------------------------------------------------
class function TVector3f.Zero: TVector3f;
begin
  Result.X:= 0;
  Result.Y:= 0;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3f.AxisX: TVector3f;
begin
  Result.X:= 1;
  Result.Y:= 0;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3f.AxisY: TVector3f;
begin
  Result.X:= 0;
  Result.Y:= 1;
  Result.Z:= 0;
end;

//------------------------------------------------------------------------------
class function TVector3f.AxisZ: TVector3f;
begin
  Result.X:= 0;
  Result.Y:= 0;
  Result.Z:= 1;
end;

//------------------------------------------------------------------------------
function TVector3f.Magnitude: Single;
begin
  Result:= Sqrt( Sqr(X) + Sqr(Y) + Sqr(Z));
end;

//------------------------------------------------------------------------------
function TVector3f.MagnitudeSqr: Single;
begin
  Result:= Sqr(X) + Sqr(Y) + Sqr(Z);
end;

//------------------------------------------------------------------------------
function TVector3f.Normalize: TVector3f;
var Magnitude: Single;
begin
  Magnitude:= Sqrt( Sqr(X) + Sqr(Y) + Sqr(Z));

  if Magnitude <> 0 then
  begin
    Result.X:= X / Magnitude;
    Result.Y:= Y / Magnitude;
    Result.Z:= Z / Magnitude;
  end else
  // Magnitude is zero, return the empty vector
  begin
    Result:= TVector3f.Zero;
  end;
end;

//------------------------------------------------------------------------------
function TVector3f.Negate: TVector3f;
begin
  Result.X:= - Self.X;
  Result.Y:= - Self.Y;
  Result.Z:= - Self.Z;
end;

//------------------------------------------------------------------------------
function TVector3f.Dot(const Vector: TVector3f): Single;
begin
  Result:= X * Vector.X + Y * Vector.Y + Z * Vector.Z;
end;

//------------------------------------------------------------------------------
function TVector3f.Cross(const Vector: TVector3f): TVector3f;
begin
  Result.X:= y * Vector.z - z * Vector.y;
	Result.Y:= z * Vector.x - x * Vector.z;
	Result.Z:= x * Vector.y - y * Vector.x;
end;

//------------------------------------------------------------------------------
class operator TVector3f.Add(const A: TVector3f; const B: TVector3f): TVector3f;
begin
  Result.X:= A.X + B.X;
  Result.Y:= A.Y + B.Y;
  Result.Z:= A.Z + B.Z;
end;

//------------------------------------------------------------------------------
class operator TVector3f.Subtract(const A: TVector3f; const B: TVector3f): TVector3f;
begin
  Result.X:= A.X - B.X;
  Result.Y:= A.Y - B.Y;
  Result.Z:= A.Z - B.Z;
end;

//------------------------------------------------------------------------------
class operator TVector3f.Multiply(const A, B: TVector3f): Single;
begin
  Result:= A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

//------------------------------------------------------------------------------
class operator TVector3f.Multiply(const A: TVector3f; const B: Single): TVector3f;
begin
  Result.X:= A.X * B;
  Result.Y:= A.Y * B;
  Result.Z:= A.Z * B;
end;

{

//------------------------------------------------------------------------------
function Magnitude(const Vector: TVector3i): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function Magnitude(const Vector: TVector3f): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function Magnitude(const Vector: TVector3b): Single;
begin
  Result:=Sqrt(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;



// The squared magnitude
//------------------------------------------------------------------------------
function MagnitudeSqr(const Vector: TVector3i): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function MagnitudeSqr(const Vector: TVector3f): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

//------------------------------------------------------------------------------
function MagnitudeSqr(const Vector: TVector3b): Single;
begin
  Result:=(Vector.X * Vector.X + Vector.Y * Vector.Y + Vector.Z * Vector.Z);
end;

// Normalize a vector
//------------------------------------------------------------------------------
function Normalize(const Vector: TVector3i): TVector3i;
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
function Normalize(const Vector: TVector3f): TVector3f;
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

//------------------------------------------------------------------------------
function Normalize(const Vector: TVector3b): TVector3b;
var MagInv: Single;
begin
  MagInv:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) + Sqr(Vector.Z));

  if MagInv = 0 then
  begin
    Result:= Vector;
  end else
  begin
    MagInv:= 1 / MagInv;

    Result.X:=Trunc(Vector.X * MagInv);
    Result.Y:=Trunc(Vector.Y * MagInv);
    Result.Z:=Trunc(Vector.Z * MagInv);
  end;
end;




// Negate a vector
//------------------------------------------------------------------------------
function Negate(const Vector: TVector3i ): TVector3i;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
  Result.Z:= -Vector.Z;
end;

//------------------------------------------------------------------------------
function Negate(const Vector: TVector3f ): TVector3f;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
  Result.Z:= -Vector.Z;
end;

//------------------------------------------------------------------------------
function Negate(const Vector: TVector3b ): TVector3b;
begin
  Result.X:= -Vector.X;
  Result.Y:= -Vector.Y;
  Result.Z:= -Vector.Z;
end;




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

//------------------------------------------------------------------------------
function VectorMagnitude(const Vector: TVector3b): Single;
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

//------------------------------------------------------------------------------
function VectorMagnitudeSqr(const Vector: TVector3b): Single;
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

//------------------------------------------------------------------------------
function VectorNormalize(const Vector: TVector3b): TVector3b;
var MagInv: Single;
begin
  MagInv:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y) + Sqr(Vector.Z));

  if MagInv = 0 then
  begin
    Result:= Vector;
  end else
  begin
    MagInv:= 1 / MagInv;

    Result.X:=Trunc(Vector.X * MagInv);
    Result.Y:=Trunc(Vector.Y * MagInv);
    Result.Z:=Trunc(Vector.Z * MagInv);
  end;
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

//------------------------------------------------------------------------------
function VectorNegate(const Vector: TVector3b ): TVector3b;
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

//------------------------------------------------------------------------------
function VectorAdd(const V1, V2: TVector3b ): TVector3b;
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

//------------------------------------------------------------------------------
function VectorSub(const V1, V2: TVector3b ): TVector3b;
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

//------------------------------------------------------------------------------
function VectorMul(const V1: TVector3b ; Scalar: Single): TVector3b;
begin
  Result.X:= Trunc(V1.X * Scalar);
  Result.Y:= Trunc(V1.Y * Scalar);
  Result.Z:= Trunc(V1.Z * Scalar);
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

//------------------------------------------------------------------------------
function VectorDiv(const V1: TVector3b ; Scalar: Single): TVector3b;
begin
  Result.X:= Trunc(V1.X / Scalar);
  Result.Y:= Trunc(V1.Y / Scalar);
  Result.Z:= Trunc(V1.Z / Scalar);
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

//------------------------------------------------------------------------------
function VectorDot(const V1, V2: TVector3b ): Single;
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
function VectorCross(const V1, V2: TVector3b ): TVector3b;
begin
  Result.X:= V1.Y * V2.Z - V1.Z * V2.Y;
  Result.Y:= V1.Z * V2.X - V1.X * V2.Z;
  Result.Z:= V1.X * V2.Y - V1.Y * V2.X;
end;
          }
{$ENDREGION}

{$REGION 'TVector4i'}

// TVector4i
//==============================================================================
function Vector4i(const X, Y, Z, W: Integer): TVector4i;
begin
  Result.X:= X;
  Result.Y:= Y;
  Result.Z:= Z;
  Result.W:= W;
end;

//------------------------------------------------------------------------------
function Vector4i(const X, Y, Z, W: Single): TVector4i;
begin
  Result.X:= Round(X);
  Result.Y:= Round(Y);
  Result.Z:= Round(Z);
  Result.W:= Round(W);
end;

{$ENDREGION}

{$REGION 'TVector4f'}

// TVector4i
//==============================================================================
function Vector4f(const X, Y, Z, W: Single): TVector4f;
begin
  Result.X:= X;
  Result.Y:= Y;
  Result.Z:= Z;
  Result.W:= W;
end;

{$ENDREGION}

{$REGION 'TRangef'}

// TRangef
//==============================================================================
function Rangef(const AMin, AMax: Single): TRangef;
begin
  Result.Min:= AMin;
  Result.Max:= AMax;
end;

//------------------------------------------------------------------------------
class function TRangef.Create(const AMin, AMax: Single): TRangef;
begin
  Result.Min:= AMin;
  Result.Max:= AMax;
end;

{$ENDREGION}

{$REGION 'TMatrix3'}

//------------------------------------------------------------------------------
class function TMatrix3f.Zero: TMatrix3f;
begin
  Result:= Matrix3f_Zero;
end;

//------------------------------------------------------------------------------
class function TMatrix3f.Identity: TMatrix3f;
begin
  Result:= Matrix3f_Identity;
end;

{$ENDREGION}

{$REGION 'TMatrix4f'}

//------------------------------------------------------------------------------
class function TMatrix4f.Zero: TMatrix4f;
begin
  Result:= Matrix4f_Zero;
end;

//------------------------------------------------------------------------------
class function TMatrix4f.Identity: TMatrix4f;
begin
  Result:= Matrix4f_Identity;
end;

//------------------------------------------------------------------------------
class operator TMatrix4f.Multiply(const A: TMatrix4f; const B: TMatrix4f): TMatrix4f;
begin
  Result:= Matrix_Multiply(A, B);
end;

//------------------------------------------------------------------------------
class operator TMatrix4f.Multiply(const A: TMatrix4f; const B: TVector3f): TVector3f;
begin
  Result:= Matrix_Transform(A, B);
end;



{$ENDREGION}





{$REGION 'TColor4f'}

// TColor4f
//------------------------------------------------------------------------------
class function TColor4f.Create(const Color: TColor32): TColor4f;
var R, G, B, A: Byte;
begin
  R := Byte(Color      );
  G := Byte(Color shr  8);
  B := Byte(Color shr 16);
  A := Byte(Color shr 24);

  Result.Red  := R / 255;
  Result.Green:= G / 255;
  Result.Blue := B / 255;
  Result.Alpha:= A / 255;
end;

//------------------------------------------------------------------------------
class function TColor4f.Create(const Color: TColor32; const Alpha: Single): TColor4f;
var R, G, B: Byte;
begin
  R := Byte(Color      );
  G := Byte(Color shr  8);
  B := Byte(Color shr 16);

  Result.Red  := R / 255;
  Result.Green:= G / 255;
  Result.Blue := B / 255;
  Result.Alpha:= Alpha;
end;

//------------------------------------------------------------------------------
class function TColor4f.Create(const Red, Green, Blue: Single): TColor4f;
begin
  Result.Red  := Red;
  Result.Green:= Green;
  Result.Blue := Blue;
  Result.Alpha:= 1.0;
end;

//------------------------------------------------------------------------------
class function TColor4f.Create(const Red, Green, Blue, Alpha: Single): TColor4f;
begin
  Result.Red  := Red;
  Result.Green:= Green;
  Result.Blue := Blue;
  Result.Alpha:= Alpha;
end;

//------------------------------------------------------------------------------
function TColor4f.ToColor: TColor32;
var r, g, b, a: Byte;
begin
  r:= Trunc(Red   * 255);
  g:= Trunc(Green * 255);
  b:= Trunc(Blue  * 255);
  a:= Trunc(Alpha * 255);

  Result:= (r or (g shl 8) or (b shl 16) or (a shl 24));
end;

//------------------------------------------------------------------------------
class operator TColor4f.Equal(const A: TColor4f; const B: TColor4f): Boolean;
begin
  Result:= (A.Red = B.Red) and (A.Green = B.Green) and (A.Blue = B.Blue) and (A.Alpha = B.Alpha);
end;

//------------------------------------------------------------------------------
class operator TColor4f.NotEqual(const A, B: TColor4f): Boolean;
begin
  Result:= (A.Red <> B.Red) or (A.Green <> B.Green) or (A.Blue <> B.Blue) or (A.Alpha <> B.Alpha);
end;

//------------------------------------------------------------------------------
class operator TColor4f.Multiply(const A: TColor4f; const B: Single): TColor4f;
begin
  Result.Red  := A.Red   * B;
  Result.Green:= A.Green * B;
  Result.Blue := A.Blue  * B;
  Result.Alpha:= A.Alpha * B;
end;

//------------------------------------------------------------------------------
class function TColor4f.None: TColor4f;
begin
  Result:= clrNone;
end;

//------------------------------------------------------------------------------
function Color4f(const Red, Green, Blue, Alpha: Single): TColor4f;
begin
  Result.Red  := Red;
  Result.Green:= Green;
  Result.Blue := Blue;
  Result.Alpha:= Alpha;
end;

//------------------------------------------------------------------------------
function Color4f(const Color: TColor4f;  Alpha: Single): TColor4f;
begin
  Result.Red  := Color.Red;
  Result.Green:= Color.Green;
  Result.Blue := Color.Blue;
  Result.Alpha:= Alpha;
end;

//------------------------------------------------------------------------------
function Color4f(const Color: TColor32): TColor4f;
var R,G,B,A: Byte;
begin
  R := Byte(Color      );
  G := Byte(Color shr  8);
  B := Byte(Color shr 16);
  A := Byte(Color shr 24);

  Result.Red  := R / 255;
  Result.Green:= G / 255;
  Result.Blue := B / 255;
  Result.Alpha:= A / 255;
end;

//------------------------------------------------------------------------------
function Color4f(const Color: TColor32; Alpha: Single): TColor4f;
var R,G,B: Byte;
begin
  R := Byte(Color      );
  G := Byte(Color shr  8);
  B := Byte(Color shr 16);

  Result.Red  := R / 255;
  Result.Green:= G / 255;
  Result.Blue := B / 255;
  Result.Alpha:= Alpha;
end;

//------------------------------------------------------------------------------
function ColorToRGBA(const Value: TColor4f): TColor32;
var r, g, b, a: Byte;
begin
  r:= Trunc(Value.Red   * 255);
  g:= Trunc(Value.Green * 255);
  b:= Trunc(Value.Blue  * 255);
  a:= Trunc(Value.Alpha * 255);

  Result:= (r or (g shl 8) or (b shl 16) or (a shl 24));
end;

//------------------------------------------------------------------------------
function ColorToRGB(const Value: TColor4f): TColor32;
var R, G, B: Byte;
begin
  r:= Trunc(Value.Red   * 255);
  g:= Trunc(Value.Green * 255);
  b:= Trunc(Value.Blue  * 255);

  Result:= (r or (g shl 8) or (b shl 16));
end;


//------------------------------------------------------------------------------
function ColorToHex(const Value: TColor4f): String;
begin
  Result:= '0x' +
           IntToHex(Trunc(Value.Red   * 255), 2) +
           IntToHex(Trunc(Value.Green * 255), 2) +
           IntToHex(Trunc(Value.Blue  * 255), 2) +
           IntToHex(Trunc(Value.Alpha * 255), 2);
end;

// Convert a hexadecimal string to a color
//------------------------------------------------------------------------------
function HexToColor(const Value: String): TColor4f;
var R, G, B, A: Byte;
begin
  // RGBA : 0xRRGGBBAA
  if Length(Value) = 10 then
  begin
    R:= StrToInt('$' + Copy(Value, 3, 2));
    G:= StrToInt('$' + Copy(Value, 5, 2));
    B:= StrToInt('$' + Copy(Value, 7, 2));
    A:= StrToInt('$' + Copy(Value, 9, 2));
  end else
  // RGB: 0xRRGGBB
  if Length(Value) = 8 then
  begin
    R:= StrToInt('$' + Copy(Value, 3, 2));
    G:= StrToInt('$' + Copy(Value, 5, 2));
    B:= StrToInt('$' + Copy(Value, 7, 2));
    A:= 255;
  end else
  // Unknown format
  begin
    R:= 0;
    G:= 0;
    B:= 0;
    A:= 0;
  end;

  Result.Red  := R / 255;
  Result.Green:= G / 255;
  Result.Blue := B / 255;
  Result.Alpha:= A / 255;
end;

//------------------------------------------------------------------------------
function SameColor(const ColorA, ColorB: TColor4f): Boolean;
begin
  Result:= (ColorA.Red   = ColorB.Red) and
           (ColorA.Green = ColorB.Green) and
           (ColorA.Blue  = ColorB.Blue) and
           (ColorA.Alpha = ColorB.Alpha);
end;

//------------------------------------------------------------------------------
function ColorToString(const Value: TColor4f): String;
begin
  if SameColor(Value, clrNone)       then Result:= 'None'       else
  if SameColor(Value, clrBlack)      then Result:= 'Black'      else
  if SameColor(Value, clrMaroon)     then Result:= 'Maroon'     else
  if SameColor(Value, clrGreen)      then Result:= 'Green'      else
  if SameColor(Value, clrOlive)      then Result:= 'Olive'      else
  if SameColor(Value, clrPurple)     then Result:= 'Purple'     else
  if SameColor(Value, clrTeal)       then Result:= 'Teal'       else
  if SameColor(Value, clrGray)       then Result:= 'Gray'       else
  if SameColor(Value, clrSilver)     then Result:= 'Silver'     else
  if SameColor(Value, clrRed)        then Result:= 'Red'        else
  if SameColor(Value, clrLime)       then Result:= 'Lime'       else
  if SameColor(Value, clrYellow)     then Result:= 'Yellow'     else
  if SameColor(Value, clrBlue)       then Result:= 'Blue'       else
  if SameColor(Value, clrFuchsia)    then Result:= 'Fuchsia'    else
  if SameColor(Value, clrAqua)       then Result:= 'Aqua'       else
  if SameColor(Value, clrWhite)      then Result:= 'White'      else
  if SameColor(Value, clrMoneyGreen) then Result:= 'MoneyGreen' else
  if SameColor(Value, clrSkyBlue)    then Result:= 'SkyBlue'    else
  if SameColor(Value, clrCream)      then Result:= 'Cream'      else
  if SameColor(Value, clrMedGray)    then Result:= 'MedGray'    else
  Result:=ColorToHex(Value);
end;

//------------------------------------------------------------------------------
function StringToColor(const Value: String  ): TColor4f;
begin
  if SameText(Value , 'None')       then Result:= clrNone       else
  if SameText(Value , 'Black')      then Result:= clrBlack      else
  if SameText(Value , 'Maroon')     then Result:= clrMaroon     else
  if SameText(Value , 'Green')      then Result:= clrGreen      else
  if SameText(Value , 'Olive')      then Result:= clrOlive      else
  if SameText(Value , 'Purple')     then Result:= clrPurple     else
  if SameText(Value , 'Teal')       then Result:= clrTeal       else
  if SameText(Value , 'Gray')       then Result:= clrGray       else
  if SameText(Value , 'Silver')     then Result:= clrSilver     else
  if SameText(Value , 'Red')        then Result:= clrRed        else
  if SameText(Value , 'Lime')       then Result:= clrLime       else
  if SameText(Value , 'Yellow')     then Result:= clrYellow     else
  if SameText(Value , 'Blue')       then Result:= clrBlue       else
  if SameText(Value , 'Fuchsia')    then Result:= clrFuchsia    else
  if SameText(Value , 'Aqua')       then Result:= clrAqua       else
  if SameText(Value , 'White')      then Result:= clrWhite      else
  if SameText(Value , 'MoneyGreen') then Result:= clrMoneyGreen else
  if SameText(Value , 'SkyBlue')    then Result:= clrSkyBlue    else
  if SameText(Value , 'Cream')      then Result:= clrCream      else
  if SameText(Value , 'MedGray')    then Result:= clrMedGray    else

  Result:=HexToColor(Value);
end;

{$ENDREGION}


{$REGION 'TViewport'}

class function TViewport.Create(const AX, AY, AWidth, AHeight: Integer): TViewport;
begin
  Result.X       := AX;
  Result.Y       := AY;
  Result.Width   := AWidth;
  Result.Height  := AHeight;
  Result.MinDepth:=    0.1;
  Result.MaxDepth:= 1000.0;
end;

class function TViewport.Create(const AX, AY, AWidth, AHeight: Integer; const AMinDepth, AMaxDepth: Single): TViewport;
begin
  Result.X       := AX;
  Result.Y       := AY;
  Result.Width   := AWidth;
  Result.Height  := AHeight;
  Result.MinDepth:= AMinDepth;
  Result.MaxDepth:= AMaxDepth;
end;

function TViewport.GetBounds: TRectf;
begin
  Result.Left  := X;
  Result.Right := X + Width;
  Result.Top   := Y;
  Result.Bottom:= Y + Height;
end;

{$ENDREGION}

{$REGION 'TSizei'}

// TSizei
//==============================================================================
class function TSizei.Create(const AWidth, AHeight: Integer): TSizei;
begin
  Result.Width := AWidth;
  Result.Height:= AHeight;
end;

{$ENDREGION}


{$REGION 'TSizef'}

// TSizef
//==============================================================================
class function TSizef.Create(const AWidth, AHeight: Single): TSizef;
begin
  Result.Width := AWidth;
  Result.Height:= AHeight;
end;

{$ENDREGION}


{$REGION 'TRectI'}

// TRectI
//==============================================================================
function Recti(const Left, Top, Right, Bottom: Integer): TRectI;
begin
  Result.Left  := Left;
  Result.Top   := Top;
  Result.Right := Right;
  Result.Bottom:= Bottom;
end;

//------------------------------------------------------------------------------
class function TRectI.Create(const ALeft, ATop, ARight, ABottom: Integer): TRectI;
begin
  Result.Left  := ALeft;
  Result.Top   := ATop;
  Result.Right := ARight;
  Result.Bottom:= ABottom;
end;

//------------------------------------------------------------------------------
function TRectI.Inflate(const DX, DY: Integer): TRectI;
begin
  Result.Left  := Left   - DX;
  Result.Right := Right  + DX;
  Result.Top   := Top    - DY;
  Result.Bottom:= Bottom + DY;
end;

//------------------------------------------------------------------------------
function TRectI.GetWidth: Integer;
begin
  Result:= Right - Left;
end;
//------------------------------------------------------------------------------
function TRectI.GetHeight: Integer;
begin
  Result:= Bottom - Top;
end;

{$ENDREGION}

{$REGION 'TRectF'}

// TRectF
//==============================================================================
function Rectf(const Left, Top, Right, Bottom: Single): TRectf;
begin
  Result.Left  := Left;
  Result.Top   := Top;
  Result.Right := Right;
  Result.Bottom:= Bottom;
end;

//------------------------------------------------------------------------------
class function TRectF.Create(const ALeft, ATop, ARight, ABottom: Single): TRectF;
begin
  Result.Left  := ALeft;
  Result.Top   := ATop;
  Result.Right := ARight;
  Result.Bottom:= ABottom;
end;

//------------------------------------------------------------------------------
function TRectf.Inflate(const DX, DY: Single): TRectf;
begin
  Result.Left  := Left   - DX;
  Result.Right := Right  + DX;
  Result.Top   := Top    - DY;
  Result.Bottom:= Bottom + DY;
end;

//------------------------------------------------------------------------------
function TRectf.GetWidth: Single;
begin
  Result:= Right - Left;
end;

//------------------------------------------------------------------------------
function TRectf.GetHeight: Single;
begin
  Result:= Bottom - Top;
end;

//------------------------------------------------------------------------------
class operator TRectF.Multiply(const A: TRectF; const B: Single): TRectF;
begin
  Result.Left  := A.Left   * B;
  Result.Top   := A.Top    * B;
  Result.Right := A.Right  * B;
  Result.Bottom:= A.Bottom * B;
end;

//------------------------------------------------------------------------------
class operator TRectF.Divide(const A: TRectF; const B: Single): TRectF;
begin
  Result.Left  := A.Left   / B;
  Result.Top   := A.Top    / B;
  Result.Right := A.Right  / B;
  Result.Bottom:= A.Bottom / B;
end;

{$ENDREGION}

{$REGION 'TPHXVertexDeclaration'}

const
  DefaultOffset: TPHXVertexOffsets = (
   //vcPosition,
   0,
   // vcNormal,
   0,
   // vcColor,
   0,
   //vcTexture1,
   0,
   // vcTexture2
   0
);

VertexOffset: TPHXVertexOffsets = (
  // Offset of texture cordinates in the vertex structure
  0,
  // Offset of colors in the vertex structure
  SizeOf(TVector2f),
  // Offset of normals corrdinates in the vertex structure
  SizeOf(TVector2f) + SizeOf(TColor4f),
   // Offset of position in the vertex structure
  SizeOf(TVector2f) + SizeOf(TColor4f) + SizeOf(TVector3f),
  0

);



{
  if vcPosition in Declaration.Components then
  begin
    glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, Declaration.Size, Declaration.Offset[vcPosition]);
  end;


glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetPosition);
glVertexAttribPointer(FAttrib_Normal  , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetNormal);
glVertexAttribPointer(FAttrib_Texcoord, 2, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetTexCoord);
glVertexAttribPointer(FAttrib_Color   , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetColor);


}


// TPHXVertexDeclaration
//==============================================================================
class function TPHXVertexDeclaration.Create: TPHXVertexDeclaration;
begin
  Result.Components:= [];
  Result.Offsets   := DefaultOffset;
  Result.Size      := 0;
end;

//------------------------------------------------------------------------------
class function TPHXVertexDeclaration.Create(Components: TVertexComponents): TPHXVertexDeclaration;
begin
  Result.Components:= Components;
  Result.Offsets   := DefaultOffset;
  Result.Size      := Result.GetVertexSize;
end;

//------------------------------------------------------------------------------
procedure TPHXVertexDeclaration.Add(const Component: TPHXVertexComponent; const Offset: Integer);
begin
  Include(Components, Component);

  Offsets[Component]:= Offset;
end;

//------------------------------------------------------------------------------
function TPHXVertexDeclaration.GetVertexSize: Integer;
var Index: TPHXVertexComponent;
begin
  Result:= 0;

  for Index := Low(TPHXVertexComponent) to High(TPHXVertexComponent) do
  begin
    if Index in Components then
    begin
      case Index of
        vcPosition: Inc(Result, 3 * SizeOf(Single));
        vcNormal  : Inc(Result, 3 * SizeOf(Single));
        vcColor   : Inc(Result, 4 * SizeOf(Single));
        vcCoord1  : Inc(Result, 2 * SizeOf(Single));
        vcCoord2: Inc(Result, 2 * SizeOf(Single));
      end;
    end;
  end;

end;

{$ENDREGION}

{$REGION 'TPHXVertex'}

const

Vertex_Declaration : TPHXVertexDeclaration =
(
  Components: [vcPosition, vcNormal, vcColor, vcCoord1];
  Size      : SizeOf(TPHXVertex);
  Offsets   : (
    //vcPosition
    SizeOf(TVector2f) + SizeOf(TColor4f) + SizeOf(TVector3f),
    //vcNormal,
    SizeOf(TVector2f) + SizeOf(TColor4f),
    //vcColor,
    SizeOf(TVector2f),
    //vcCoord1,
    0,
    //vcCoord2
    0
  );
);
        (*
    // The texture coordinate
    TexCoord: TVector2f;
    // The color
    Color: TColor4f;
    // The normal
    Normal: TVector3f;
    // The position
    Position: TVector3f;
    *)

// TPHXVertex
//==============================================================================
class function TPHXVertex.Declaration: TPHXVertexDeclaration;
var Temp: PPHXVertex;
begin
  Result:= Vertex_Declaration;

  Result.Components:= [vcPosition, vcNormal, vcColor, vcCoord1];
  Result.Size      := SizeOf(TPHXVertex);

  Temp:= nil;
  Result.Offsets[vcPosition]:= Integer(@Temp^.Position) - Integer(Temp);
  Result.Offsets[vcNormal  ]:= Integer(@Temp^.Normal  ) - Integer(Temp);
  Result.Offsets[vcColor   ]:= Integer(@Temp^.Color   ) - Integer(Temp);
  Result.Offsets[vcCoord1  ]:= Integer(@Temp^.TexCoord) - Integer(Temp);
end;
type
TVertex = record
  Position: TVector3f;
  Normal: TVector3f;
  Texture: TVector2f;
end;

procedure Test;
var Decl: TPHXVertexDeclaration;
var V: TVertex;
begin
  Decl:= TPHXVertex.Declaration;

  Decl:= TPHXVertexDeclaration.Create([vcPosition, vcNormal, vcCoord1]);
  Decl.Offsets[vcPosition]:= Integer(@V.Position) - Integer(@V);
  Decl.Offsets[vcNormal  ]:= Integer(@V.Normal  ) - Integer(@V);
  Decl.Offsets[vcCoord1  ]:= Integer(@V.Texture ) - Integer(@V);

  if Decl.Size <> SizeOf(TVertex) then
  begin
    Exit;
  end;
//  Decl.Size;

  if vcPosition in Decl.Components then
  begin
   // glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, Declaration.Size, Declaration.Offset[vcPosition]);
  end;


end;
{$ENDREGION}

{$REGION 'TPHXVertexList'}

// TPHXVertexList
//==============================================================================
constructor TPHXVertexList.Create;
begin

end;

//------------------------------------------------------------------------------
constructor TPHXVertexList.Create(ACapacity: Integer);
begin
   SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXVertexList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.Add(const Value: TPHXVertex);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
function TPHXVertexList.Alloc(NumVerticies: Integer): Integer;
begin
  Result:= FCount;

  Inc(FCount, NumVerticies);

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.Transform(const Matrix: TMatrix4f);
var Index: Integer;
begin
  for Index:=0 to Count - 1 do
  begin
    FList^[Index].Position:= Matrix_Transform(Matrix, FList^[Index].Position);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCapacity + Delta);
end;
//------------------------------------------------------------------------------
procedure TPHXVertexList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXVertex));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXVertexList.GetItem(Index: Integer): TPHXVertex;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXVertexList.SetItem(Index: Integer; const Value: TPHXVertex);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXIndexList'}

// TPHXIndexList
//==============================================================================
constructor TPHXIndexList.Create;
begin

end;

//------------------------------------------------------------------------------
constructor TPHXIndexList.Create(ACapacity: Integer);
begin
   SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXIndexList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.Add(const Value: Cardinal);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
function TPHXIndexList.Alloc(NumIndicies: Integer): Integer;
begin
  Result:= FCount;

  Inc(FCount, NumIndicies);

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(Capacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(Cardinal));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXIndexList.GetItem(Index: Integer): Cardinal;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXIndexList.SetItem(Index: Integer; const Value: Cardinal);
begin
  FList^[Index]:= Value;
end;


{$ENDREGION}








end.
