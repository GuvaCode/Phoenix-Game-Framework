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
unit phxParticleGraphs;
//< Particle graphs

interface

{$I phxConfig.inc}

uses SysUtils, Classes, Math,

  phxTypes,
  phxClasses,
  phxMath;

const

// Number of points in the particle graphs
PHXPARTICLE_GRAPH_COUNT = 32;   //  1024;

type

// Interpolation type
//------------------------------------------------------------------------------
TPHXPathInterpolation = (
  // Linear interpolation
  ipLinear = 0,
  // Cubic interpolation
  ipCubic  = 1,
  // No interpolation steps between the items in the path
  ipNone = 2
);

{$REGION 'TPHXGraphKey1i'}

// Graph key for Integers
//------------------------------------------------------------------------------
TPHXGraphKey1i = record
  // Absolute time of the key in the interval 0.0 - 1.0
  Time: Single;
  // Value of the key
  Value: Integer;
end;

PGraphKeyList1i = ^TGraphKeyList1i;
TGraphKeyList1i = array[0..$00FFFFFF] of TPHXGraphKey1i;

//------------------------------------------------------------------------------
TPHXGraphKeyList1i = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PGraphKeyList1i;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXGraphKey1i;
    procedure SetItem(Index: Integer; const Value: TPHXGraphKey1i);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Load the path items from a file
    procedure LoadFromFile(const FileName: String);
    // Save the path items to a file
    procedure SaveToFile(const FileName: String);
    // Load the path items from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the path items to a stream
    procedure SaveToStream(Stream: TStream);

    // Clears the list
    procedure Clear;

    // Add a new key
    procedure Add(const Value: TPHXGraphKey1i ); overload;
    // Add a new key
    procedure Add(const Time: Single; const Value: Integer); overload;

    // Delete the key at the index
    procedure Delete(Index: Integer);

    // Sort all items by their time
    procedure SortByTime;

    // Number of items in the list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of items
    property List: PGraphKeyList1i read FList;
    // Gets and sets a items
    property Items[Index: Integer]: TPHXGraphKey1i read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXGraphKey1f'}

// Graph key for Single
//------------------------------------------------------------------------------
TPHXGraphKey1f = record
  // Absolute time of the key in the interval 0.0 - 1.0
  Time: Single;
  // Value of the key
  Value: Single;
end;

PGraphKeyList1f = ^TGraphKeyList1f;
TGraphKeyList1f = array[0..$00FFFFFF] of TPHXGraphKey1f;

//------------------------------------------------------------------------------
TPHXGraphKeyList1f = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PGraphKeyList1f;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXGraphKey1f;
    procedure SetItem(Index: Integer; const Value: TPHXGraphKey1f);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Load the path items from a file
    procedure LoadFromFile(const FileName: String);
    // Save the path items to a file
    procedure SaveToFile(const FileName: String);
    // Load the path items from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the path items to a stream
    procedure SaveToStream(Stream: TStream);

    // Clears the list
    procedure Clear;

    // Add a new key
    procedure Add(const Value: TPHXGraphKey1f ); overload;
    // Add a new key
    procedure Add(const Time: Single; const Value: Single); overload;

    // Delete the key at the index
    procedure Delete(Index: Integer);

    // Sort all items by their time
    procedure SortByTime;

    // Number of items in the list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of items
    property List: PGraphKeyList1f read FList;
    // Gets and sets a items
    property Items[Index: Integer]: TPHXGraphKey1f read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXGraphKey3f'}

// Graph key for TVector3f
//------------------------------------------------------------------------------
TPHXGraphKey3f = record
  // Absolute time of the key in the interval 0.0 - 1.0
  Time: Single;
  // Value of the key
  Value: TVector3f;
end;

PGraphKeyList3f = ^TGraphKeyList3f;
TGraphKeyList3f = array[0..$00FFFFFF] of TPHXGraphKey3f;

//------------------------------------------------------------------------------
TPHXGraphKeyList3f = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PGraphKeyList3f;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXGraphKey3f;
    procedure SetItem(Index: Integer; const Value: TPHXGraphKey3f);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Load the path items from a file
    procedure LoadFromFile(const FileName: String);
    // Save the path items to a file
    procedure SaveToFile(const FileName: String);
    // Load the path items from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the path items to a stream
    procedure SaveToStream(Stream: TStream);

    // Clears the list
    procedure Clear;

    // Add a new key
    procedure Add(const Value: TPHXGraphKey3f ); overload;
    // Add a new key
    procedure Add(const Time: Single; const Value: TVector3f); overload;
    // Add a new key
    procedure Add(const Time: Single; const X, Y, Z: Single); overload;

   // Delete the key at the index
    procedure Delete(Index: Integer);

    // Sort all items by their time
    procedure SortByTime;

    // Number of items in the list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of items
    property List: PGraphKeyList3f read FList;
    // Gets and sets a items
    property Items[Index: Integer]: TPHXGraphKey3f read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXGraphKey3f'}

// Graph key for TColor4f
//------------------------------------------------------------------------------
TPHXGraphKeyCf = record
  // Absolute time of the key in the interval 0.0 - 1.0
  Time: Single;
  // Color of the key
  Value: TColor4f;
end;

PGraphKeyListCf = ^TGraphKeyListCf;
TGraphKeyListCf = array[0..$00FFFFFF] of TPHXGraphKeyCf;

//------------------------------------------------------------------------------
TPHXGraphKeyList4c = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PGraphKeyListCf;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXGraphKeyCf;
    procedure SetItem(Index: Integer; const Value: TPHXGraphKeyCf);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Load the path items from a file
    procedure LoadFromFile(const FileName: String);
    // Save the path items to a file
    procedure SaveToFile(const FileName: String);
    // Load the path items from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the path items to a stream
    procedure SaveToStream(Stream: TStream);

    // Clears the list
    procedure Clear;

    // Add a new key
    procedure Add(const Value: TPHXGraphKeyCf ); overload;
    // Add a new key
    procedure Add(const Time: Single; const Value: TColor4f); overload;
    // Add a new key
    procedure Add(const Time: Single; const R, G, B, A: Single); overload;

    // Delete the key at the index
    procedure Delete(Index: Integer);

    // Sort all items by their time
    procedure SortByTime;

    // Number of items in the list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of items
    property List: PGraphKeyListCf read FList;
    // Gets and sets a items
    property Items[Index: Integer]: TPHXGraphKeyCf read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXGraph'}

// Abstract particle graph
//------------------------------------------------------------------------------
TPHXParticleGraph = class
  private
    // Interpolation method
    FInterpolation: TPHXPathInterpolation;
    // Name of the graph
    FName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Load the graph from a reader.
    procedure LoadGraph(Reader: TPHXReader); virtual;
    // Save the graph to a writer.
    procedure SaveGraph(Writer: TPHXWriter); virtual;

    // Get and set the name of the graph
    property Name: String read FName write FName;
    // Gets and sets the interpolation mode
    property Interpolation: TPHXPathInterpolation read FInterpolation write FInterpolation;
  end;

{$ENDREGION}

{$REGION 'TPHXGraph1i'}

// A graph is a interpolation class for interpolating values
//------------------------------------------------------------------------------
TPHXGraph1i = class(TPHXParticleGraph)
  private
    // List of track keys
    FKeys: TPHXGraphKeyList1i;
    // Number of expanded values
    FCount: Integer;
    // List of expanded values
    FValues: PIntegerList;

    // Minumum key value
    FMinValue: Integer;
    // Maximum key value
    FMaxValue: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Load the graph from a reader.
    procedure LoadGraph(Reader: TPHXReader); override;
    // Save the graph to a writer.
    procedure SaveGraph(Writer: TPHXWriter); override;

    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand; overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer); overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer; const Values: PIntegerList); overload;

    // List of keys in the graph
    property Keys: TPHXGraphKeyList1i read FKeys;
    // List of expanded values
    property Values: PIntegerList read FValues;

    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

{$ENDREGION}

{$REGION 'TPHXGraph1f'}

// A graph is a interpolation class for interpolating values
//------------------------------------------------------------------------------
TPHXGraph1f = class(TPHXParticleGraph)
  private
    // List of track keys
    FKeys: TPHXGraphKeyList1f;
    // Number of expanded values
    FCount : Integer;
    // List of expanded values
    FValues: PSingleList;

    // Minumum key value
    FMinValue: Single;
    // Maximum key value
    FMaxValue: Single;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Load the graph from a reader.
    procedure LoadGraph(Reader: TPHXReader); override;
    // Save the graph to a writer.
    procedure SaveGraph(Writer: TPHXWriter); override;

    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand; overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer); overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer; const Values: PSingleList); overload;

    // List of keys in the graph
    property Keys: TPHXGraphKeyList1f read FKeys;
    // Number of expanded values
    property Count: Integer read FCount;
    // List of expanded values
    property Values: PSingleList read FValues;

    property MinValue: Single read FMinValue write FMinValue;
    property MaxValue: Single read FMaxValue write FMaxValue;
  end;

{$ENDREGION}

{$REGION 'TPHXGraph3f'}

// A graph is a interpolation class for interpolating values
//------------------------------------------------------------------------------
TPHXGraph3f = class(TPHXParticleGraph)
  private
    // List of track keys
    FKeys: TPHXGraphKeyList3f;
    // Number of expanded values
    FCount : Integer;
    // List of expanded values
    FValues: PVectorList3f;

    // Minumum key value
    FMinValue: Single;
    // Maximum key value
    FMaxValue: Single;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Load the graph from a reader.
    procedure LoadGraph(Reader: TPHXReader); override;
    // Save the graph to a writer.
    procedure SaveGraph(Writer: TPHXWriter); override;

    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand; overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer); overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer; const Values: PVectorList3f); overload;

    // List of keys in the graph
    property Keys: TPHXGraphKeyList3f read FKeys;
    // Number of expanded values
    property Count: Integer read FCount;
     // List of expanded values
    property Values: PVectorList3f read FValues;

    property MinValue: Single read FMinValue write FMinValue;
    property MaxValue: Single read FMaxValue write FMaxValue;
  end;

{$ENDREGION}

{$REGION 'TPHXGraph4c'}

// A graph is a generic interpolation class for interpolating values
//------------------------------------------------------------------------------
TPHXGraphCf = class(TPHXParticleGraph)
  private
    // List of track keys
    FKeys : TPHXGraphKeyList4c;
    // Number of expanded values
    FCount : Integer;
    // List of expanded values
    FValues: PColorList4f;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Load the graph from a reader.
    procedure LoadGraph(Reader: TPHXReader); override;
    // Save the graph to a writer.
    procedure SaveGraph(Writer: TPHXWriter); override;

    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand; overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer); overload;
    // Expand the path and fill the list with the values. Range is the maximum value
    procedure Expand(const Count: Integer; const Values: PColorList4f); overload;

    // List of keys in the graph
    property Keys: TPHXGraphKeyList4c read FKeys;
    // Number of expanded values
    property Count: Integer read FCount;
     // List of expanded values
    property Values: PColorList4f read FValues;
  end;


{$ENDREGION}

//------------------------------------------------------------------------------
TPHXParticleGraphs = class(TPersistent)
  private
    // Graph for the absolute visibility
    FAlpha: TPHXGraph1f;
    // Graph for the scale
    FScale: TPHXGraph1f;
    // Graph for the absolute color
    FColor: TPHXGraphCf;
    // Graph for the particle velocity factor
    FVelocity: TPHXGraph3f;
    // Graph for the particle spin factor
    FSpin: TPHXGraph1f;
    // Graph for the emittor emission count
    FEmissionCount: TPHXGraph1f;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Initialize;

    // Load the graphs from a stream.
    procedure LoadGraphs(Reader: TPHXReader);
    // Save the graphs to a stream.
    procedure SaveGraphs(Writer: TPHXWriter);

    // Graph for the visibility
    property Alpha: TPHXGraph1f read FAlpha;
    // Graph for the scale
    property Scale: TPHXGraph1f read FScale; // Todo: write SetGraphScale
    // Graph for the color
    property Color: TPHXGraphCf read FColor;
    // Graph for the particle velocity
    property Velocity: TPHXGraph3f read FVelocity;
    // Graph for the particle spin
    property Spin: TPHXGraph1f read FSpin;
    // Graph for the emittor emission count
    property EmissionCount: TPHXGraph1f read FEmissionCount;
  end;







// Linear interpolation
Function LinearInterpolate(const Y1, Y2: Integer  ; const MU: Single): Integer; overload;
Function LinearInterpolate(const Y1, Y2: Single   ; const MU: Single): Single; overload;
Function LinearInterpolate(const Y1, Y2: TVector2f; const MU: Single): TVector2f; overload;
Function LinearInterpolate(const Y1, Y2: TVector3f; const MU: Single): TVector3f; overload;
Function LinearInterpolate(const Y1, Y2: TColor4f ; const MU: Single): TColor4f; overload;
Function LinearInterpolate(const Y1, Y2: TColor3f ; const MU: Single): TColor3f; overload;

// Cubic interpolation
Function CubicInterpolate(const Y0, Y1, Y2, Y3: Integer  ; const MU: Single): Integer; overload;
Function CubicInterpolate(const Y0, Y1, Y2, Y3: Single   ; const MU: Single): Single; overload;
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TVector2f; const MU: Single): TVector2f; overload;
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TVector3f; const MU: Single): TVector3f; overload;
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TColor4f ; const MU: Single): TColor4f; overload;
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TColor3f ; const MU: Single): TColor3f; overload;

implementation

//------------------------------------------------------------------------------
procedure QuickSortByTime(SortList: PGraphKeyList1i; L, R: Integer); overload;
var I, J: Integer;
var P, T: TPHXGraphKey1i;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while ( SortList^[I].Time - P.Time) < 0 do
        Inc(I);
      while (SortList^[J].Time - P.Time) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByTime(SortList, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
procedure QuickSortByTime(SortList: PGraphKeyList1f; L, R: Integer); overload;
var I, J: Integer;
var P, T: TPHXGraphKey1f;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while ( SortList^[I].Time - P.Time) < 0 do
        Inc(I);
      while (SortList^[J].Time - P.Time) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByTime(SortList, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
procedure QuickSortByTime(SortList: PGraphKeyList3f; L, R: Integer); overload;
var I, J: Integer;
var P, T: TPHXGraphKey3f;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while ( SortList^[I].Time - P.Time) < 0 do
        Inc(I);
      while (SortList^[J].Time - P.Time) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByTime(SortList, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
procedure QuickSortByTime(SortList: PGraphKeyListCf; L, R: Integer); overload;
var I, J: Integer;
var P, T: TPHXGraphKeyCf;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while ( SortList^[I].Time - P.Time) < 0 do
        Inc(I);
      while (SortList^[J].Time - P.Time) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByTime(SortList, L, J);
    L := I;
  until I >= R;
end;


{$REGION 'Linear interpolation'}

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: Integer; const MU: Single): Integer; overload;
begin
  Result:= Round(Y1 *(1 - mu) + Y2 * mu);
end;

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: Single; const MU: Single): Single; overload;
begin
  Result:= Y1 *(1 - mu) + Y2 * mu
end;

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: TVector3f; const MU: Single): TVector3f; overload;
begin
  Result.X:= Y1.X *(1 - mu) + Y2.X * mu;
  Result.Y:= Y1.Y *(1 - mu) + Y2.Y * mu;
  Result.Z:= Y1.Z *(1 - mu) + Y2.Z * mu;
end;

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: TVector2f; const MU: Single): TVector2f; overload;
begin
  Result.X:= Y1.X *(1 - mu) + Y2.X * mu;
  Result.Y:= Y1.Y *(1 - mu) + Y2.Y * mu;
end;

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: TColor4f; const MU: Single): TColor4f; overload;
begin
  Result.Red  := Y1.Red   * (1 - mu) + Y2.Red   * mu;
  Result.Green:= Y1.Green * (1 - mu) + Y2.Green * mu;
  Result.Blue := Y1.Blue  * (1 - mu) + Y2.Blue  * mu;
  Result.Alpha:= Y1.Alpha * (1 - mu) + Y2.Alpha * mu;
end;

//------------------------------------------------------------------------------
Function LinearInterpolate(const Y1, Y2: TColor3f; const MU: Single): TColor3f; overload;
begin
  Result.Red  := Y1.Red   * (1 - mu) + Y2.Red   * mu;
  Result.Green:= Y1.Green * (1 - mu) + Y2.Green * mu;
  Result.Blue := Y1.Blue  * (1 - mu) + Y2.Blue  * mu;
end;

{$ENDREGION}

{$REGION 'Cubic interpolation'}

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: Integer; const MU: Single): Integer; overload;
var a0,a1,a2,a3,mu2: Single;
begin
   mu2:= mu*mu;
   a0 := y3 - y2 - y0 + y1;
   a1 := y0 - y1 - a0;
   a2 := y2 - y0;
   a3 := y1;

   Result:= Round(a0*mu*mu2+a1*mu2+a2*mu+a3);
end;

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: Single; const MU: Single): Single; overload;
var a0,a1,a2,a3,mu2: Single;
begin
   mu2:= mu*mu;
   a0 := y3 - y2 - y0 + y1;
   a1 := y0 - y1 - a0;
   a2 := y2 - y0;
   a3 := y1;

   Result:= a0*mu*mu2+a1*mu2+a2*mu+a3;
end;

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TVector3f; const MU: Single): TVector3f; overload;
var a0,a1,a2,a3,mu2: Single;
begin
  mu2:= mu*mu;

  a0 := y3.X - y2.X - y0.X + y1.X;
  a1 := y0.X - y1.X - a0;
  a2 := y2.X - y0.X;
  a3 := y1.X;

  Result.X:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Y - y2.Y - y0.Y + y1.Y;
  a1 := y0.Y - y1.Y - a0;
  a2 := y2.Y - y0.Y;
  a3 := y1.Y;

  Result.Y:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Z - y2.Z - y0.Z + y1.Z;
  a1 := y0.Z - y1.Z - a0;
  a2 := y2.Z - y0.Z;
  a3 := y1.Z;

  Result.Z:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;
end;

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TVector2f; const MU: Single): TVector2f; overload;
var a0,a1,a2,a3,mu2: Single;
begin
  mu2:= mu*mu;

  a0 := y3.X - y2.X - y0.X + y1.X;
  a1 := y0.X - y1.X - a0;
  a2 := y2.X - y0.X;
  a3 := y1.X;

  Result.X:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Y - y2.Y - y0.Y + y1.Y;
  a1 := y0.Y - y1.Y - a0;
  a2 := y2.Y - y0.Y;
  a3 := y1.Y;

  Result.Y:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;
end;

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TColor4f; const MU: Single): TColor4f; overload;
var a0,a1,a2,a3,mu2: Single;
begin
  mu2:= mu*mu;

  a0 := y3.Red - y2.Red - y0.Red + y1.Red;
  a1 := y0.Red - y1.Red - a0;
  a2 := y2.Red - y0.Red;
  a3 := y1.Red;

  Result.Red:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Green - y2.Green - y0.Green + y1.Green;
  a1 := y0.Green - y1.Green - a0;
  a2 := y2.Green - y0.Green;
  a3 := y1.Green;

  Result.Green:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Blue - y2.Blue - y0.Blue + y1.Blue;
  a1 := y0.Blue - y1.Blue - a0;
  a2 := y2.Blue - y0.Blue;
  a3 := y1.Blue;

  Result.Blue:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Alpha - y2.Alpha - y0.Alpha + y1.Alpha;
  a1 := y0.Alpha - y1.Alpha - a0;
  a2 := y2.Alpha - y0.Alpha;
  a3 := y1.Alpha;

  Result.Alpha:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;
end;

//------------------------------------------------------------------------------
Function CubicInterpolate(const Y0, Y1, Y2, Y3: TColor3f; const MU: Single): TColor3f; overload;
var a0,a1,a2,a3,mu2: Single;
begin
  mu2:= mu*mu;

  a0 := y3.Red - y2.Red - y0.Red + y1.Red;
  a1 := y0.Red - y1.Red - a0;
  a2 := y2.Red - y0.Red;
  a3 := y1.Red;

  Result.Red:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Green - y2.Green - y0.Green + y1.Green;
  a1 := y0.Green - y1.Green - a0;
  a2 := y2.Green - y0.Green;
  a3 := y1.Green;

  Result.Green:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;

  a0 := y3.Blue - y2.Blue - y0.Blue + y1.Blue;
  a1 := y0.Blue - y1.Blue - a0;
  a2 := y2.Blue - y0.Blue;
  a3 := y1.Blue;

  Result.Blue:= a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3;
end;

{$ENDREGION}




{$REGION 'TPHXGraphKeyList1i'}

// TPHXGraphKeyList1i
//==============================================================================
constructor TPHXGraphKeyList1i.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphKeyList1i.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.LoadFromFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SaveToFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^, SizeOf(TPHXGraphKey1f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^, SizeOf(TPHXGraphKey1f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.Add(const Value: TPHXGraphKey1i );
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.Add(const Time: Single; const Value: Integer);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time  := Time;
  FList^[Count - 1].Value := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXGraphKey1i));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SortByTime;
begin
  if (FList <> nil) and (Count > 0) then
  begin
    QuickSortByTime(FList, 0, Count - 1);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXGraphKey1i));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXGraphKeyList1i.GetItem(Index: Integer): TPHXGraphKey1i;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1i.SetItem(Index: Integer; const Value: TPHXGraphKey1i);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXGraphKeyList1f'}

// TPHXGraphKeyList1f
//==============================================================================
constructor TPHXGraphKeyList1f.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphKeyList1f.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.LoadFromFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SaveToFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^, SizeOf(TPHXGraphKey1f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^, SizeOf(TPHXGraphKey1f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.Add(const Value: TPHXGraphKey1f );
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.Add(const Time: Single; const Value: Single);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time  := Time;
  FList^[Count - 1].Value := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXGraphKey1f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SortByTime;
begin
  if (FList <> nil) and (Count > 0) then
  begin
    QuickSortByTime(FList, 0, Count - 1);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXGraphKey1f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXGraphKeyList1f.GetItem(Index: Integer): TPHXGraphKey1f;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList1f.SetItem(Index: Integer; const Value: TPHXGraphKey1f);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXGraphKeyList3f'}

// TPHXGraphKeyList3f
//==============================================================================
constructor TPHXGraphKeyList3f.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphKeyList3f.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.LoadFromFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SaveToFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^, SizeOf(TPHXGraphKey3f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^, SizeOf(TPHXGraphKey3f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Add(const Value: TPHXGraphKey3f );
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Add(const Time: Single; const Value: TVector3f);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time  := Time;
  FList^[Count - 1].Value := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Add(const Time: Single; const X, Y, Z: Single);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time   := Time;
  FList^[Count - 1].Value.X:= X;
  FList^[Count - 1].Value.Y:= Y;
  FList^[Count - 1].Value.Z:= Z;
end;
//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXGraphKey3f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SortByTime;
begin
  if (FList <> nil) and (Count > 0) then
  begin
    QuickSortByTime(FList, 0, Count - 1);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXGraphKey3f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXGraphKeyList3f.GetItem(Index: Integer): TPHXGraphKey3f;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList3f.SetItem(Index: Integer; const Value: TPHXGraphKey3f);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXGraphKeyList4c'}

// TPHXColorKeyList
//==============================================================================
constructor TPHXGraphKeyList4c.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphKeyList4c.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.LoadFromFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SaveToFile(const FileName: String);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^, SizeOf(TPHXGraphKeyCf) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^, SizeOf(TPHXGraphKeyCf) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Add(const Value: TPHXGraphKeyCf );
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Add(const Time: Single; const Value: TColor4f);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time  := Time;
  FList^[Count - 1].Value := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Add(const Time, R, G, B, A: Single);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Time  := Time;
  FList^[Count - 1].Value.Red  := R;
  FList^[Count - 1].Value.Green:= G;
  FList^[Count - 1].Value.Blue := B;
  FList^[Count - 1].Value.Blue := A;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXGraphKeyCf));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SortByTime;
begin
  if (FList <> nil) and (Count > 0) then
  begin
    QuickSortByTime(FList, 0, Count - 1);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXGraphKeyCf));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXGraphKeyList4c.GetItem(Index: Integer): TPHXGraphKeyCf;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXGraphKeyList4c.SetItem(Index: Integer; const Value: TPHXGraphKeyCf);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION }



{$REGION 'TPHXParticleGraph'}

// TPHXParticleGraph
//==============================================================================
constructor TPHXParticleGraph.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXParticleGraph.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleGraph.LoadGraph(Reader: TPHXReader);
begin
  FName:= Reader.ReadString;

  FInterpolation:= TPHXPathInterpolation(Reader.ReadCardinal);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleGraph.SaveGraph(Writer: TPHXWriter);
begin
  Writer.WriteString(FName);

  Writer.WriteCardinal( ord(FInterpolation));
end;

{$ENDREGION }

{$REGION 'TPHXGraph1i'}

// TPHXGraph
//==============================================================================
constructor TPHXGraph1i.Create;
begin
  inherited Create;

  FKeys:= TPHXGraphKeyList1i.Create;
  FMinValue:= 0;
  FMaxValue:= 100;
end;

//------------------------------------------------------------------------------
destructor TPHXGraph1i.Destroy;
begin
  ReallocMem(FValues, 0);

  FKeys.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1i.LoadGraph(Reader: TPHXReader);
begin
  inherited LoadGraph(Reader);

  Keys.LoadFromStream(Reader.Stream);

  Reader.Read(FMinValue, SizeOf(FMinValue));
  Reader.Read(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1i.SaveGraph(Writer: TPHXWriter);
begin
  inherited SaveGraph(Writer);

  Keys.SaveToStream(Writer.Stream);

  Writer.Write(FMinValue, SizeOf(FMinValue));
  Writer.Write(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1i.Expand;
begin
  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1i.Expand(const Count: Integer);
begin
  if Count <> FCount then
  begin
    FCount:= Count;

    ReallocMem(FValues, FCount * SizeOf(Integer));
  end;

  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1i.Expand(const Count: Integer; const Values: PIntegerList);
var Time : Single;
var Delta: Single;
var ValIndex: Integer;
var KeyIndex: Integer;
var I0      : Integer;
var I1      : Integer;
var I2      : Integer;
var I3      : Integer;
var IntPosition: Single;
var IntDelta   : Single;
begin
  // No points in the input array
  if (Count = 0) then Exit;

  // If there are no keys, fill the value array with zero
  if (Keys.Count = 0) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= 1;
    end;
  end else
  // If there is only one key, fill the array with the value of the first key
  if (Keys.Count = 1) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= Keys.List^[0].Value;
    end;
  end else
  // Interpolate all values
  begin
    Delta:= 1 / Count;
    Time := 0;

    KeyIndex:= 0;

    for ValIndex:= 0 to Count - 1 do
    begin

      // Move to the next key, if avaiable
      if (KeyIndex < Keys.Count - 1) and (Time > Keys.List^[KeyIndex+1].Time) then
      begin
        Inc(KeyIndex);
      end;

      // If we havn't reached the fist key's time, use the key value
      if (Time < Keys.List^[0].Time) then
      begin
        Values^[ValIndex]:= Keys.List^[0].Value;
      end else
      // Interpolate the value from the keys
      begin
        case Interpolation of
          // Linear interpolation
          ipLinear:
          begin
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= LinearInterpolate(Keys.List^[I1].Value, Keys.List^[I2].Value, IntPosition);
          end;
          // Cubic interpolation
          ipCubic:
          begin
            I0:= Max(0, Min(KeyIndex - 1, Keys.Count - 1));
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));
            I3:= Max(0, Min(KeyIndex + 2, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= CubicInterpolate(Keys.List^[I0].Value, Keys.List^[I1].Value, Keys.List^[I2].Value, Keys.List^[I3].Value, IntPosition);
          end;
          // No interpolation
          ipNone:
          begin
            Values^[ValIndex]:= Keys.List^[KeyIndex].Value;
          end;
        end;
      end;
      // Trim the value, this is needed because the cubic interpolation can generate
      // values outside the allowed range
      if Values^[ValIndex] < FMinValue then Values^[ValIndex]:= FMinValue;
      if Values^[ValIndex] > FMaxValue then Values^[ValIndex]:= FMaxValue;

      Time:= Time + Delta;
    end;
  end;
end;

{$ENDREGION }

{$REGION 'TPHXGraph1f'}

// TPHXGraph
//==============================================================================
constructor TPHXGraph1f.Create;
begin
  inherited Create;

  FKeys:= TPHXGraphKeyList1f.Create;
  FMinValue:= 0.0;
  FMaxValue:= 1.0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraph1f.Destroy;
begin
  ReallocMem(FValues, 0);

  FKeys.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1f.LoadGraph(Reader: TPHXReader);
begin
  inherited LoadGraph(Reader);

  Keys.LoadFromStream(Reader.Stream);

  Reader.Read(FMinValue, SizeOf(FMinValue));
  Reader.Read(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1f.SaveGraph(Writer: TPHXWriter);
begin
  inherited SaveGraph(Writer);

  Keys.SaveToStream(Writer.Stream);

  Writer.Write(FMinValue, SizeOf(FMinValue));
  Writer.Write(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1f.Expand;
begin
  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1f.Expand(const Count: Integer);
begin
  if Count <> FCount then
  begin
    FCount:= Count;

    ReallocMem(FValues, FCount * SizeOf(Single));
  end;

  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph1f.Expand(const Count: Integer; const Values: PSingleList);
var Time : Single;
var Delta: Single;
var ValIndex: Integer;
var KeyIndex: Integer;
var I0      : Integer;
var I1      : Integer;
var I2      : Integer;
var I3      : Integer;
var IntPosition: Single;
var IntDelta   : Single;
begin

  // No points in the input array
  if (Count = 0) then Exit;

  // If there are no keys, fill the value array with zero
  if (Keys.Count = 0) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= 1.0;
    end;
  end else
  // If there is only one key, fill the array with the value of the first key
  if (Keys.Count = 1) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= Keys.List^[0].Value;
    end;
  end else
  // Interpolate all values
  begin
    Delta:= 1 / Count;
    Time := 0;

    KeyIndex:= 0;

    for ValIndex:= 0 to Count - 1 do
    begin

      // Move to the next key, if avaiable
      if (KeyIndex < Keys.Count - 1) and (Time > Keys.List^[KeyIndex+1].Time) then
      begin
        Inc(KeyIndex);
      end;

      // If we havn't reached the fist key's time, use the key value
      if (Time < Keys.List^[0].Time) then
      begin
        Values^[ValIndex]:= Keys.List^[0].Value;
      end else
      // Interpolate the value from the keys
      begin
        case Interpolation of
          // Linear interpolation
          ipLinear:
          begin
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= LinearInterpolate(Keys.List^[I1].Value, Keys.List^[I2].Value, IntPosition);
          end;
          // Cubic interpolation
          ipCubic:
          begin
            I0:= Max(0, Min(KeyIndex - 1, Keys.Count - 1));
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));
            I3:= Max(0, Min(KeyIndex + 2, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= CubicInterpolate(Keys.List^[I0].Value, Keys.List^[I1].Value, Keys.List^[I2].Value, Keys.List^[I3].Value, IntPosition);
          end;
          // No interpolation
          ipNone:
          begin
            Values^[ValIndex]:= Keys.List^[KeyIndex].Value;
          end;
        end;
      end;
      // Trim the value, this is needed because the cubic interpolation can generate
      // values outside the allowed range
      if Values^[ValIndex] < FMinValue then Values^[ValIndex]:= FMinValue;
      if Values^[ValIndex] > FMaxValue then Values^[ValIndex]:= FMaxValue;

      Time:= Time + Delta;
    end;
  end;
end;

{$ENDREGION }

{$REGION 'TPHXGraph3f'}

// TPHXGraph
//==============================================================================
constructor TPHXGraph3f.Create;
begin
  inherited Create;

  FKeys:= TPHXGraphKeyList3f.Create;
  FMinValue:= 0.0;
  FMaxValue:= 1.0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraph3f.Destroy;
begin
  ReallocMem(FValues, 0);

  FKeys.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGraph3f.LoadGraph(Reader: TPHXReader);
begin
  inherited LoadGraph(Reader);

  Keys.LoadFromStream(Reader.Stream);

  Reader.Read(FMinValue, SizeOf(FMinValue));
  Reader.Read(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph3f.SaveGraph(Writer: TPHXWriter);
begin
  inherited SaveGraph(Writer);

  Keys.SaveToStream(Writer.Stream);

  Writer.Write(FMinValue, SizeOf(FMinValue));
  Writer.Write(FMaxValue, SizeOf(FMaxValue));
end;

//------------------------------------------------------------------------------
procedure TPHXGraph3f.Expand;
begin
  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph3f.Expand(const Count: Integer);
begin
  if Count <> FCount then
  begin
    FCount:= Count;

    ReallocMem(FValues, FCount * SizeOf(TVector3f));
  end;

  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraph3f.Expand(const Count: Integer; const Values: PVectorList3f);
var Time : Single;
var Delta: Single;
var ValIndex: Integer;
var KeyIndex: Integer;
var I0      : Integer;
var I1      : Integer;
var I2      : Integer;
var I3      : Integer;
var IntPosition: Single;
var IntDelta   : Single;
begin
  // No points in the input array
  if (Count = 0) then Exit;

  // If there are no keys, fill the value array with zero
  if (Keys.Count = 0) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex].X:= 1.0;
      Values^[ValIndex].Y:= 1.0;
      Values^[ValIndex].Z:= 1.0;
    end;
  end else
  // If there is only one key, fill the array with the value of the first key
  if (Keys.Count = 1) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= Keys.List^[0].Value;
    end;
  end else
  // Interpolate all values
  begin
    Delta:= 1 / Count;
    Time := 0;

    KeyIndex:= 0;

    for ValIndex:= 0 to Count - 1 do
    begin

      // Move to the next key, if avaiable
      if (KeyIndex < Keys.Count - 1) and (Time > Keys.List^[KeyIndex+1].Time) then
      begin
        Inc(KeyIndex);
      end;

      // If we havn't reached the fist key's time, use the key value
      if (Time < Keys.List^[0].Time) then
      begin
        Values^[ValIndex]:= Keys.List^[0].Value;
      end else
      // Interpolate the value from the keys
      begin
        case Interpolation of
          // Linear interpolation
          ipLinear:
          begin
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= LinearInterpolate(Keys.List^[I1].Value, Keys.List^[I2].Value, IntPosition);
          end;
          // Cubic interpolation
          ipCubic:
          begin
            I0:= Max(0, Min(KeyIndex - 1, Keys.Count - 1));
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));
            I3:= Max(0, Min(KeyIndex + 2, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= CubicInterpolate(Keys.List^[I0].Value, Keys.List^[I1].Value, Keys.List^[I2].Value, Keys.List^[I3].Value, IntPosition);
          end;
          // No interpolation
          ipNone:
          begin
            Values^[ValIndex]:= Keys.List^[KeyIndex].Value;
          end;
        end;
      end;
      // Trim the value, this is needed because the cubic interpolation can generate
      // values outside the allowed range
      if Values^[ValIndex].X < FMinValue then Values^[ValIndex].X:= FMinValue;
      if Values^[ValIndex].Y < FMinValue then Values^[ValIndex].Y:= FMinValue;
      if Values^[ValIndex].Z < FMinValue then Values^[ValIndex].Z:= FMinValue;

      if Values^[ValIndex].X > FMaxValue then Values^[ValIndex].X:= FMaxValue;
      if Values^[ValIndex].Y > FMaxValue then Values^[ValIndex].Y:= FMaxValue;
      if Values^[ValIndex].Z > FMaxValue then Values^[ValIndex].Z:= FMaxValue;

      Time:= Time + Delta;
    end;
  end;
end;

{$ENDREGION }

{$REGION 'TPHXGraphCf'}

// TPHXColorGraph
//==============================================================================
constructor TPHXGraphCf.Create;
begin
  inherited Create;

  FKeys:= TPHXGraphKeyList4c.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphCf.Destroy;
begin
  ReallocMem(FValues, 0);

  FKeys.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphCf.LoadGraph(Reader: TPHXReader);
begin
  inherited LoadGraph(Reader);

  Keys.LoadFromStream(Reader.Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphCf.SaveGraph(Writer: TPHXWriter);
begin
  inherited SaveGraph(Writer);

  Keys.SaveToStream(Writer.Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphCf.Expand;
begin
  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphCf.Expand(const Count: Integer);
begin
  if Count <> FCount then
  begin
    FCount:= Count;

    ReallocMem(FValues, Count * SizeOf(TColor4f));
  end;

  Expand(FCount, FValues);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphCf.Expand(const Count: Integer; const Values: PColorList4f);
var Time : Single;
var Delta: Single;
var ValIndex: Integer;
var KeyIndex: Integer;
var I0      : Integer;
var I1      : Integer;
var I2      : Integer;
var I3      : Integer;
var IntPosition: Single;
var IntDelta   : Single;
begin

  // No points in the input array
  if (Count = 0) then Exit;

  // If there are no keys, fill the value array with zero
  if (Keys.Count = 0) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex].Red  := 1.0;
      Values^[ValIndex].Green:= 1.0;
      Values^[ValIndex].Blue := 1.0;
//      Values^[ValIndex].Alpha:= 1.0;
    end;
  end else
  // If there is only one key, fill the array with the value of the first key
  if (Keys.Count = 1) then
  begin
    for ValIndex:= 0 to Count - 1 do
    begin
      Values^[ValIndex]:= Keys.List^[0].Value;
    end;
  end else
  // Interpolate all values
  begin
    Delta:= 1 / Count;
    Time := 0;

    KeyIndex:= 0;

    for ValIndex:= 0 to Count - 1 do
    begin

      // Move to the next key, if avaiable
      if (KeyIndex < Keys.Count - 1) and (Time > Keys.List^[KeyIndex+1].Time) then
      begin
        Inc(KeyIndex);
      end;

      // If we havn't reached the fist key's time, use the key value
      if (Time < Keys.List^[0].Time) then
      begin
        Values^[ValIndex]:= Keys.List^[0].Value;
      end else
      // Interpolate the value from the keys
      begin
        case Interpolation of
          // Linear interpolation
          ipLinear:
          begin
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= LinearInterpolate(Keys.List^[I1].Value, Keys.List^[I2].Value, IntPosition);
          end;
          // Cubic interpolation
          ipCubic:
          begin
            I0:= Max(0, Min(KeyIndex - 1, Keys.Count - 1));
            I1:= Max(0, Min(KeyIndex    , Keys.Count - 1));
            I2:= Max(0, Min(KeyIndex + 1, Keys.Count - 1));
            I3:= Max(0, Min(KeyIndex + 2, Keys.Count - 1));

            IntDelta:= Keys.List^[I2].Time -  Keys.List^[I1].Time;

            // Calculate the interpolation position
            if IntDelta > 0 then
            begin
              IntPosition:= (Time - Keys.List^[I1].Time) / IntDelta;
            end else
            begin
              IntPosition:= 1;
            end;

            Values^[ValIndex]:= CubicInterpolate(Keys.List^[I0].Value, Keys.List^[I1].Value, Keys.List^[I2].Value, Keys.List^[I3].Value, IntPosition);
          end;
          // No interpolation
          ipNone:
          begin
            Values^[ValIndex]:= Keys.List^[KeyIndex].Value;
          end;
        end;
      end;

      // Trim the value, this is needed because the cubic interpolation can generate
      // values outside the allowed range
      if Values^[ValIndex].Red   < 0.0 then Values^[ValIndex].Red  := 0.0;
      if Values^[ValIndex].Green < 0.0 then Values^[ValIndex].Green:= 0.0;
      if Values^[ValIndex].Blue  < 0.0 then Values^[ValIndex].Blue := 0.0;

      if Values^[ValIndex].Red   > 1.0 then Values^[ValIndex].Red  := 1.0;
      if Values^[ValIndex].Green > 1.0 then Values^[ValIndex].Green:= 1.0;
      if Values^[ValIndex].Blue  > 1.0 then Values^[ValIndex].Blue := 1.0;

      Time:= Time + Delta;
    end;
  end;
end;



{$ENDREGION }

// TPHXParticleGraphs
//==============================================================================
constructor TPHXParticleGraphs.Create;
begin
  // Graph for the scale
  FScale:= TPHXGraph1f.Create;
  // Graph for the color
  FColor:= TPHXGraphCf.Create;
  // Graph for the visibility
  FAlpha:= TPHXGraph1f.Create;
  // Graph for the particle velocity
  FVelocity:= TPHXGraph3f.Create;
  // Graph for the particle spin
  FSpin:= TPHXGraph1f.Create;
  // Graph for the emittor emission count
  FEmissionCount:= TPHXGraph1f.Create;

  FScale.MinValue:= 0;
  FScale.MaxValue:= 10000;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleGraphs.Destroy;
begin
  // Graph for the scale
  FScale.Free;
  // Graph for the color
  FColor.Free;
  // Graph for the visibility
  FAlpha.Free;
  // Graph for the particle spin
  FSpin.Free;
  // Graph for the particle velocity
  FVelocity.Free;
  // Graph for the emittor emission count
  FEmissionCount.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleGraphs.Initialize;
begin
  // Initialize the particle graphs
  FAlpha        .Expand(PHXPARTICLE_GRAPH_COUNT);
  FScale        .Expand(PHXPARTICLE_GRAPH_COUNT);
  FColor        .Expand(PHXPARTICLE_GRAPH_COUNT);
  FVelocity     .Expand(PHXPARTICLE_GRAPH_COUNT);
  FSpin         .Expand(PHXPARTICLE_GRAPH_COUNT);
  FEmissionCount.Expand(PHXPARTICLE_GRAPH_COUNT);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleGraphs.LoadGraphs(Reader: TPHXReader);
begin
  FAlpha        .LoadGraph(Reader);
  FScale        .LoadGraph(Reader);
  FColor        .LoadGraph(Reader);
  FVelocity     .LoadGraph(Reader);
  FSpin         .LoadGraph(Reader);
  FEmissionCount.LoadGraph(Reader);

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleGraphs.SaveGraphs(Writer: TPHXWriter);
begin
  FAlpha        .SaveGraph(Writer);
  FScale        .SaveGraph(Writer);
  FColor        .SaveGraph(Writer);
  FVelocity     .SaveGraph(Writer);
  FSpin         .SaveGraph(Writer);
  FEmissionCount.SaveGraph(Writer);
end;





end.
