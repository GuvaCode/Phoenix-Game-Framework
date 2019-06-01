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
unit phxShape;
{<
  @abstract(Convex shapes and collision testing)

  Uses the separating Axis Theorem (SAT) for testing for collisions

}
interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxLogger,
  phxTypes,
  phxClasses,
  phxDevice,
  phxCanvas;


const

// File extension for the shape files
PHXSHAPE_EXT = '.phxshp';
// Version
PHXSHAPE_VERSION = 6;

// Constant determining if two contacts are parallel to the current axis
PHXSHAPE_CONTACT_THRESHOLD = 1.0E-3;

type

// The supported shape types
//------------------------------------------------------------------------------
TPHXShapeKind = (
  // Point shape
  PHX_SHAPE_POINT,
  // Box shape
  PHX_SHAPE_BOX,
  // Convex polygon shape
  PHX_SHAPE_POLYGON,
  // Circle shape
  PHX_SHAPE_CIRCLE,
  // Line shape
  PHX_SHAPE_LINE
);

// The shape file header.
//------------------------------------------------------------------------------
TPHXShapeHeader = record
  // The id of the image file, should always be PHXSHAPE.
  Ident: Array[1..8] of AnsiChar;
  // The file version.
  Version: Integer;
  // The type of shape in the file
  Shape: TPHXShapeKind;
end;

{$REGION 'TPHXShape'}

// Abstract shape class
//------------------------------------------------------------------------------
TPHXShape = class
  private
    FName         : String;
    FKind         : TPHXShapeKind;
    FCollisionAxes: TPHXVectorList2f;
  protected
    // @exclude
    procedure LoadShape(Reader: TPHXReader); virtual; abstract;
    // @exclude
    procedure SaveShape(Writer: TPHXWriter); virtual; abstract;
    // @exclude
    function GetBoundingBox: TRectf; virtual; abstract;
  public
    // Default contstructor
    constructor Create; virtual;
    // Destroys the shape
    destructor Destroy; override;

    // Load the shape from a file
    procedure LoadFromFile(const FileName: String);
    // Load the shape from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the shape to a file
    procedure SaveToFile(const FileName: String);
    // Save the shape to a stream
    procedure SaveToStream(Stream: TStream);

    // Create a clone of this shape
    function Clone: TPHXShape; virtual; abstract;
    // Copy data from another shape, only valid for the same object type
    procedure Assign(Shape: TPHXShape); virtual;

    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); virtual; abstract;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); virtual; abstract;

    // Projects the shape on a axis, returns the interval min and max position
    procedure Project(const Axis: TVector2f; out Projection: TRangef); virtual; abstract;

    // Collides this shape with  another shape
    function Collide(const Shape: TPHXShape): Boolean; overload;
    // Collides this shape with  another shape
    function Collide(const Shape: TPHXShape; const Displacement: TVector2f): Boolean; overload;

    // Returns true if the point is inside the shape
    function PointInside(const Point: TVector2f): Boolean; virtual; abstract;

    // Draw the shape
    procedure Render(Canvas: TPHXCanvas); overload;
    // Draw the shape
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); overload; virtual; abstract;

    // Get and set the name of the shape
    property Name: String read FName write FName;
    // Get the type of shape
    property Kind: TPHXShapeKind read FKind;
    // List of axes to test for collisions using SAT
    property CollisionAxes: TPHXVectorList2f read FCollisionAxes;
    // The bounding box of the shape
    property BoundingBox: TRectf read GetBoundingBox;
  public
    class function Collide(const A: TPHXShape; const B: TPHXShape): Boolean; overload;
    // Collide two shapes using Swept SAT, Displacement is the relative distance
    // moved of the shapes since the last collision test.
    // Returns true if the objects are colliding
    class function Collide(const A: TPHXShape; const B: TPHXShape; const Displacement: TVector2f): Boolean; overload;
  end;

{$ENDREGION}

{$REGION 'TPHXPoint'}

// @abstract(A point shape)
// Doesnt generate any additional SAT axes to test
//------------------------------------------------------------------------------
TPHXPoint = class(TPHXShape)
  private
    FPosition  : TVector2f;

    procedure SetPosition(const Value: TVector2f);
    procedure SetPositionX(const Value: Single);
    procedure SetPositionY(const Value: Single);
  protected
    procedure LoadShape(Reader: TPHXReader); override;
    procedure SaveShape(Writer: TPHXWriter); override;

    function GetBoundingBox: TRectf;  override;
  public
    // Creates a new box
    constructor Create; overload; override;

    // Create a clone of this box
    function Clone: TPHXShape; override;
    // Copy data from another box
    procedure Assign(Shape: TPHXShape); override;

    // Returns true if the point is inside the point
    function PointInside(const Point: TVector2f): Boolean; override;


    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); override;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); override;

    // Draw the box
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f ); override;

    // Projects the box on a axis, returns the projection as a min and max interval
    procedure Project(const Axis: TVector2f; out Projection: TRangef); override;

    // Center position of the box
    property Position: TVector2f read FPosition write SetPosition;
    property PositionX: Single read FPosition.X write SetPositionX;
    property PositionY: Single read FPosition.Y write SetPositionY;
  end;

{$ENDREGION}

{$REGION 'TPHXBox'}

// @abstract(Box shape)
// We have a spererate shape for boxes as its only required to test two axes as
// the two parallel sides gives the same result
//------------------------------------------------------------------------------
TPHXBox = class(TPHXShape)
  private
    FCenter  : TVector2f;
    FSize    : TVector2f;
    FHalfSize: TVector2f;

    procedure SetSize(const Value: TVector2f);
    procedure SetSizeX(const Value: Single);
    procedure SetSizeY(const Value: Single);
  protected
    procedure LoadShape(Reader: TPHXReader); override;
    procedure SaveShape(Writer: TPHXWriter); override;

    function GetBoundingBox: TRectf;  override;
  public
    // Creates a new box
    constructor Create; overload; override;
    constructor Create(const ASize: Single); reintroduce; overload;
    constructor Create(const AWidth: Single; const AHeight: Single); reintroduce; overload;

    // Create a clone of this box
    function Clone: TPHXShape; override;
    // Copy data from another box
    Procedure Assign(Shape: TPHXShape); override;

    // Returns true if the point is inside the box
    function PointInside(const Point: TVector2f): Boolean; override;

    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); override;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); override;

    // Draw the box
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); override;

    // Projects the box on a axis, returns the projection as a min and max interval
    procedure Project(const Axis: TVector2f; out Projection: TRangef); override;

    // Center position of the box
    property Center: TVector2f read FCenter write FCenter;
    // Center position of the box
    property CenterX: Single read FCenter.X write FCenter.X;
    // Center position of the box
    property CenterY: Single read FCenter.Y write FCenter.Y;

    // size of the box
    property Size: TVector2f read FSize write SetSize;
    // Center position of the box
    property SizeX: Single read FCenter.X write SetSizeX;
    // Center position of the box
    property SizeY: Single read FCenter.Y write SetSizeY;


    // Width of the box
    property Width: Single read FSize.X write SetSizeX;
    // Height of the box
    property Height: Single read FSize.Y write SetSizeY;
  end;

{$ENDREGION}

{$REGION 'TPHXPolygon'}

// @abstract(Convex polygon shape)
//------------------------------------------------------------------------------
TPHXPolygon = class(TPHXShape)
  private
    FSize: Integer;
    FList: PVectorList2f;
    procedure CreateAxes;

    function GetCenter: TVector2f;
    function GetPoint(const Index: Integer): TVector2f;

    procedure SetPoint(const Index: Integer; const AValue: TVector2f);
    procedure SetSize (const AValue: Integer);
  protected
    procedure LoadShape(Reader: TPHXReader); override;
    procedure SaveShape(Writer: TPHXWriter); override;

    function GetBoundingBox: TRectf;  override;
  public
    // Creates a new polygon
    constructor Create; overload; override;
    // Creates a new polygon
    constructor Create(const Points: array of TVector2f); reintroduce; overload;
    // Destroys the shape
    destructor Destroy; override;

    // Create a polygon as a box with 4 vertices
    procedure CreateBox(const Width, Height: Single); overload;
    // Create a polygon as a box with 4 vertices
    procedure CreateBox(const Rect: TRectf); overload;
    // Create a circle
    procedure CreateCircle(const Radius: Single; const NumVertices: Integer);

    // Add a new point to the polygon
    Procedure Add(const Point: TVector2f); overload;
    // Add a new point to the polygon
    procedure Add(const X, Y: Single); overload;
    // Delete the point at index
    procedure Delete(Index: Integer);

    // Create a clone of this box
    function Clone: TPHXShape; override;
    // Copy data from another box
    Procedure Assign(Shape: TPHXShape); override;

    // Returns true if the point is inside the polygon
    function PointInside(const Point: TVector2f): Boolean; override;

    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); override;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); override;

    // Draw the box
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); override;

    // Projects the polygon on a axis, returns the projection as a min and max interval
    procedure Project(const Axis: TVector2f; out Projection: TRangef); override;

    // Number of verticies in the polygon
    property Size: Integer read FSize write SetSize;
    // Return a pointer to the internal list, do not modify
    Property List: PVectorList2f read FList;
    // The points in the polygon
    property Points[const Index: Integer]: TVector2f read GetPoint write SetPoint; default;
    // Calculate and return the center point of the polygon
    property Center: TVector2f read GetCenter;
  end;

{$ENDREGION}

{$REGION 'TPHXLine'}

// @abstract(Line shape)
//------------------------------------------------------------------------------
TPHXLine = class(TPHXShape)
  private
    FMin: TVector2f;
    FMax: TVector2f;
    procedure CreateAxes;
    procedure SetMax(const Value: TVector2f);
    procedure SetMin(const Value: TVector2f);

    procedure SetMaxX(const Value: Single);
    procedure SetMaxY(const Value: Single);
    procedure SetMinX(const Value: Single);
    procedure SetMinY(const Value: Single);
  protected
    procedure LoadShape(Reader: TPHXReader); override;
    procedure SaveShape(Writer: TPHXWriter); override;

    function GetBoundingBox: TRectf;  override;
  public
    // Creates a new line
    constructor Create; overload; override;

    // Create a clone of this line
    function Clone: TPHXShape; override;
    // Copy data from another line
    Procedure Assign(Shape: TPHXShape); override;

    // Returns true if the point is inside the line
    function PointInside(const Point: TVector2f): Boolean; override;


    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); override;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); override;

    // Draw the line
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); override;

    // Projects the line on a axis, returns the projection as a min and max interval
    procedure Project(const Axis: TVector2f; out Projection: TRangef); override;

    // Start position of the line
    property Min: TVector2f read FMin write SetMin;
    // Start X - position of the line
    property MinX: Single read FMin.X write SetMinX;
    // Start Y - position of the line
    property MinY: Single read FMin.Y write SetMinY;

    // End position of the line
    property Max: TVector2f read FMax write SetMax;
    // End X- position of the line
    property MaxX: Single read FMax.X write SetMaxX;
    // End Y -position of the line
    property MaxY: Single read FMax.Y write SetMaxY;
  end;

{$ENDREGION}

{$REGION 'TPHXCircle'}

// @abstract(Circle  shape)
// Approximation of a circle, approximated using a Dodekagon (12 sided polygon, 6 SAT axes).
//------------------------------------------------------------------------------
TPHXCircle = class(TPHXShape)
  private
    FCenter: TVector2f;
    FRadius: Single;
  protected
    procedure LoadShape(Reader: TPHXReader); override;
    procedure SaveShape(Writer: TPHXWriter); override;

    function GetBoundingBox: TRectf;  override;
  public
    // Creates a new circle
    constructor Create; overload; override;
    constructor Create(const Radius: Single); reintroduce; overload;

    // Create a clone of this circle
    function Clone: TPHXShape; override;
    // Copy data from another circle
    procedure Assign(Shape: TPHXShape); override;

    // Returns true if the point is inside the circle
    function PointInside(const Point: TVector2f): Boolean; override;

    // Transforms the shape by a matrix
    procedure Transform(const Matrix: TMatrix4f); override;
    // Translate the shape by a vector
    procedure Translate(const Vector: TVector2f); override;

    // Draw the circle
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); override;

    // Projects the circle on a axis, returns the projection as a min and max interval
    procedure Project(const Axis: TVector2f; out Projection: TRangef); override;

    // The center position of the circle
    property Center: TVector2f read FCenter write FCenter;
    // The center position of the circle
    property CenterX: Single read FCenter.X write FCenter.X;
    // The center position of the circle
    property CenterY: Single read FCenter.Y write FCenter.Y;
    // The radius of the circle
    property Radius: Single read FRadius write FRadius;
  end;   

{$ENDREGION}

{$REGION 'TPHXShapeList'}

PShapeList = ^TShapeList;
TShapeList = array[0..$00FFFFFF] of TPHXShape;

// List of shapes
//------------------------------------------------------------------------------
TPHXShapeList = class
  private
    FList: TList;

    function GetCount: Integer;
    function GetList: PShapeList;
    function GetItem(Index: Integer): TPHXShape;
  public
    // Create a neew shape list
    constructor Create;
    // Frees the list and all shapes
    destructor Destroy; override;

    // Load the shapes from a file
    procedure LoadFromFile(const FileName: String);
    // Load the shapes from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the shapes to a file
    procedure SaveToFile(const FileName: String);
    // Save the shapes to a stream
    procedure SaveToStream(Stream: TStream);

    // Clear the list and destroy all shapes
    procedure Clear;

    // Add a shape from a file
    function LoadShape(const FileName: String): TPHXShape;

    // Add a existing shape to the list, the list will free the shape on destruction
    function Add(Shape: TPHXShape): TPHXShape;
    // Create and add a new point to the list
    function AddPoint(const Name: String): TPHXPoint;
    // Create and add a new box to the list
    function AddBox(const Name: String): TPHXBox;
    // Create and add a new polygon to the list
    function AddPolygon(const Name: String): TPHXPolygon;
    // Create and add a new circle to the list
    function AddCircle(const Name: String): TPHXCircle;
    // Create and add a new line to the list
    function AddLine(const Name: String): TPHXLine;

    // Delete and free a shape
    procedure Delete(const Index: Integer);
    // Remove and free a shape
    procedure Remove(const Shape: TPHXShape);

    // Returns the index of a shape by name
    function IndexOf(const Name: String): Integer; overload;
    // Returns the index of a shape
    function IndexOf(const Shape: TPHXShape): Integer; overload;

    // Find a shape by name
    function Find(const Name: String): TPHXShape; overload;
    // Find a shape by name
    function Find(const Name: String; out Shape: TPHXShape): Boolean; overload;

    // Return the shape at a position using the PointInside function
    function ShapeAt(const X,Y: Single): TPHXShape;

    // Draw all shapes
    procedure Render(Canvas: TPHXCanvas); overload;
    // Draw all shapes
    procedure Render(Canvas: TPHXCanvas; const Offset: TVector2f); overload;

    // Get and set the number of items in the list
    property Count: Integer read GetCount;
    // Get a pointer to the internal shape list
    property List: PShapeList read GetList;
    // Get a item from the list
    Property Shapes[Index: Integer]: TPHXShape read GetItem; default;
  end;

{$ENDREGION}

 // Create the shape fir a kind
function CreateShape(Kind: TPHXShapeKind): TPHXShape;
// Create and load a shape from a file
function LoadShape(const FileName: String): TPHXShape; overload;
// Create and load a shape from a stream
function LoadShape(Stream: TStream): TPHXShape; overload;

implementation

uses phxMath;
{$REGION 'TPHXShapeFactory'}

//------------------------------------------------------------------------------
function CreateShape(Kind: TPHXShapeKind): TPHXShape;
begin
  Result:= nil;

  case Kind of
    PHX_SHAPE_POINT:
    begin
      Result:= TPHXPoint.Create;
    end;
    PHX_SHAPE_BOX:
    begin
      Result:= TPHXBox.Create;
    end;
    PHX_SHAPE_POLYGON:
    begin
      Result:= TPHXPolygon.Create;
    end;
    PHX_SHAPE_LINE:
    begin
      Result:= TPHXLine.Create;
    end;
    PHX_SHAPE_CIRCLE:
    begin
      Result:= TPHXCircle.Create;
    end;
  end;
  Assert( Assigned(Result), 'Unknown shape type');
end;

//------------------------------------------------------------------------------
function LoadShape(const FileName: String): TPHXShape;
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    Result:= LoadShape(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
function LoadShape(Stream: TStream): TPHXShape;
var Header: TPHXShapeHeader;
var Position: Int64;
begin
  Position:= Stream.Position;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));
  Stream.Read(Header.Shape  , SizeOf(Header.Shape));

  if (Header.Ident <> 'PHXSHAPE') then
  begin
    raise Exception.Create('Not a valid Phoenix Shape file.');
  end;

  if (Header.Version <> PHXSHAPE_VERSION) then
  begin
    raise Exception.CreateFmt('Shape version missmatch [File: %d Code: %d].', [Header.Version, PHXSHAPE_VERSION]);
  end;

  Stream.Seek(Position, soFromBeginning);

  Result:= nil;
  case Header.Shape of
    PHX_SHAPE_POINT:
    begin
      Result:= TPHXPoint.Create;
      Result.LoadFromStream(Stream);
    end;
    PHX_SHAPE_BOX:
    begin
      Result:= TPHXBox.Create;
      Result.LoadFromStream(Stream);
    end;
    PHX_SHAPE_POLYGON:
    begin
      Result:= TPHXPolygon.Create;
      Result.LoadFromStream(Stream);
    end;
    PHX_SHAPE_LINE:
    begin
      Result:= TPHXLine.Create;
      Result.LoadFromStream(Stream);
    end;
    PHX_SHAPE_CIRCLE:
    begin
      Result:= TPHXCircle.Create;
      Result.LoadFromStream(Stream);
    end;
  end;
  Assert( Assigned(Result), 'Unknown shape type');
end;


{$ENDREGION}


{$REGION 'TPHXShape.Collide'}

//------------------------------------------------------------------------------
class function TPHXShape.Collide(const A, B: TPHXShape): Boolean;
begin
  Result:= TPHXShape.Collide(A, B, Vector2f_Zero);
end;

//------------------------------------------------------------------------------
class function TPHXShape.Collide(const A: TPHXShape; const B: TPHXShape; const Displacement: TVector2f): Boolean;
var Index: Integer;

var Axis: TVector2f;

var First: TCollisionParameter;
var Last : TCollisionParameter;
var MTD  : TCollisionParameter;

var ProjectA: TRangef;
var ProjectB: TRangef;

{$IFDEF COLLISION_DEBUG}
var Edge  : TVector2f;
var Bounds: TRectf;
var Center: TVector2f;
{$ENDIF}
begin

  // Check if the shapes are empty
  if (A.CollisionAxes.Count = 0) and (B.CollisionAxes.Count = 0) then
  begin
    Result:= False;
    Exit;
  end;

  // Reset the mininum translation distance (MDT)
  MTD.Normal.X:=  0.0;
  MTD.Normal.Y:=  0.0;
  MTD.Time    := -1.0;

  // Reset the first collision
  First.Time    := 0.0;
  First.Normal.X:= 0.0;
  First.Normal.Y:= 0.0;

  // Reset the last collision
  Last.Time    := 1.0;
  Last.Normal.X:= 0.0;
  Last.Normal.Y:= 0.0;

  {$IFDEF COLLISION_DEBUG}
  Bounds:= Rectf(MaxSingle, MaxSingle,MinSingle,MinSingle);
  A.ExpandBounds(Bounds);
  Center.X:= (Bounds.Right   + Bounds.Left) * 0.5;
  Center.Y:= (Bounds.Bottom  + Bounds.Top ) * 0.5;
  {$ENDIF}

  // Test #1: Separation axes of shape A
  for Index:=0 to A.CollisionAxes.Count - 1 do
  begin
    // Calculate the normal of the edge
    Axis:= A.CollisionAxes.List^[Index];

    // Project both shapes on the axes
    A.Project(Axis, ProjectA);
    B.Project(Axis, ProjectB);

    // Check if the projections are intersecting
    if not IntervalIntersect(ProjectA, ProjectB, Axis, Displacement, First, Last, MTD) then
    begin
      Result:= False;
      Exit;
    end;

    {$IFDEF COLLISION_DEBUG}
    Edge.X:= -Axis.Y;
    Edge.Y:=  Axis.X;

    RenderProjection( Center, Axis, Edge,  50, MinA, MaxA, 1, clrRed );
    RenderProjection( Center, Axis, Edge,  51, MinB, MaxB, 1, clrBlue);
    {$ENDIF}
  end;

  // Test #2: Separation axes of shape B
  for Index:=0 to B.CollisionAxes.Count - 1 do
  begin
    // Calculate the normal of the edge
    Axis:= B.CollisionAxes.List^[Index];

    // Project both shapes on the axes
    A.Project(Axis, ProjectA);
    B.Project(Axis, ProjectB);

    // Check if the projections are intersecting
    if not IntervalIntersect(ProjectA, ProjectB, Axis, Displacement, First, Last, MTD) then
    begin
      Result:= False;
      Exit;
    end;

    {$IFDEF COLLISION_DEBUG}
    Edge.X:= -Axis.Y;
    Edge.Y:=  Axis.X;

    RenderProjection( Center, Axis, Edge,  50, MinA, MaxA, 1, clrRed );
    RenderProjection( Center, Axis, Edge,  51, MinB, MaxB, 1, clrBlue);
    {$ENDIF}
  end;
    (*
  // we found a collision forwad in time.
  // use the first collision parameters
  if (First.Time > 0.0) then
  begin
    Collision.Distance.X:= Displacement.X * First.Time;;
    Collision.Distance.Y:= Displacement.Y * First.Time;;
    Collision.Normal    := Normalize(First.Normal);  // SPELLING Normalise
    Collision.Time      := First.Time;
  end else
  // else it;s an intersection.
  // return a negative number,
  // which is the depth of intersection.
  begin
    Collision.Distance  := MTD.Normal;
    Collision.Normal    := Normalize(MTD.Normal);
    Collision.Time      := 0;
  end;
  *)
  Result:= True;
end;

{$ENDREGION}


{$REGION 'TPHXShape'}

// TPHXShape
//==============================================================================
constructor TPHXShape.Create;
begin
  FCollisionAxes:= TPHXVectorList2f.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXShape.Destroy;
begin
  FCollisionAxes.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXShape.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShape.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShape.LoadFromStream(Stream: TStream);
var Header: TPHXShapeHeader;
var Reader: TPHXReader;
begin
  Header.Ident  := #0#0#0#0#0#0#0#0;
  Header.Version:= 0;
  Header.Shape  := PHX_SHAPE_LINE;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));
  Stream.Read(Header.Shape  , SizeOf(Header.Shape));

  if (Header.Ident <> 'PHXSHAPE') then
  begin
    TPHXLogger.Error('TPHXShape.LoadFromStream', 'Not a valid shape.');

    raise Exception.Create('Not a valid Phoenix Shape file.');
  end;

  if (Header.Version <> PHXSHAPE_VERSION) then
  begin
    TPHXLogger.Error('TPHXShape.LoadFromStream', 'Shape version missmatch [File: %d Code: %d].', [Header.Version, PHXSHAPE_VERSION]);

    raise Exception.CreateFmt('Shape version missmatch [File: %d Code: %d].', [Header.Version, PHXSHAPE_VERSION]);
  end;

  if (Header.Shape <> Kind) then
  begin
    raise Exception.CreateFmt('Shape type missmatch [File: %d Code: %d].', [ Ord(Header.Shape), Ord(Kind)]);
  end;

  Reader:= TPHXReader.Create(Stream);
  try
    FName:= Reader.ReadString;

    LoadShape(Reader);
  finally
    Reader.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShape.SaveToStream(Stream: TStream);
var Header: TPHXShapeHeader;
var Writer: TPHXWriter;
begin
  Header.Ident  :='PHXSHAPE';
  Header.Version:= PHXSHAPE_VERSION;
  Header.Shape  := Kind;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));
  Stream.Write(Header.Shape  , SizeOf(Header.Shape));


  Writer:= TPHXWriter.Create(Stream);
  try
    Writer.WriteString(FName);

    SaveShape(Writer);
  finally
    Writer.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShape.Assign(Shape: TPHXShape);
begin
  Assert( Shape is ClassType, 'Shape is not a ' + ClassName );

  FName:= Shape.FName;
//    FShapeType    : TPHXShapeType;
//    FCollisionAxes: TPHXVectorList2f;
end;

//------------------------------------------------------------------------------
function TPHXShape.Collide(const Shape: TPHXShape): Boolean;
begin
  Result:= TPHXShape.Collide(Self, Shape, Vector2f_Zero);
end;

//------------------------------------------------------------------------------
function TPHXShape.Collide(const Shape: TPHXShape; const Displacement: TVector2f): Boolean;
begin
  Result:= TPHXShape.Collide(Self, Shape, Displacement);
end;

//------------------------------------------------------------------------------
procedure TPHXShape.Render(Canvas: TPHXCanvas);
begin
  Render(Canvas, Vector2f_Zero);
end;


{$ENDREGION}

{$REGION 'TPHXPoint'}

// TPHXPoint
//==============================================================================
constructor TPHXPoint.Create;
begin
  inherited Create;

  FName:= 'Point';
  FKind:= PHX_SHAPE_POINT;

  // Setup the collision axes
  FCollisionAxes.Count:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.LoadShape(Reader: TPHXReader);
begin
  FPosition:= Reader.ReadVector2f;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.SaveShape(Writer: TPHXWriter);
begin
  Writer.WriteVector2f(FPosition);
end;

//------------------------------------------------------------------------------
function TPHXPoint.Clone: TPHXShape;
begin
  Result:= TPHXPoint.Create;

  TPHXPoint(Result).FName    := FName;
  TPHXPoint(Result).FPosition:= FPosition;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.Assign(Shape: TPHXShape);
begin
  inherited Assign(Shape);
  
  FPosition:= TPHXPoint(Shape).FPosition;
end;

//------------------------------------------------------------------------------
function TPHXPoint.GetBoundingBox: TRectf;
begin
  Result.Right := FPosition.X;
  Result.Left  := FPosition.X;
  Result.Bottom:= FPosition.Y;
  Result.Top   := FPosition.Y;
end;

// Returns true if the point is inside the point
//------------------------------------------------------------------------------
function TPHXPoint.PointInside(const Point: TVector2f): Boolean;
begin
  Result:= (Point.X >= FPosition.X - 2) and
           (Point.X <= FPosition.X + 2) and
           (Point.Y >= FPosition.Y - 2) and
           (Point.Y <= FPosition.Y + 2);
end;


// Transforms the shape by a matrix
//------------------------------------------------------------------------------
procedure TPHXPoint.Transform(const Matrix: TMatrix4f);
begin
  FPosition.X:= FPosition.X + Matrix.v[12];
  FPosition.Y:= FPosition.Y + Matrix.v[13];
end;

// Translate the shape by a vector
//------------------------------------------------------------------------------
procedure TPHXPoint.Translate(const Vector: TVector2f);
begin
  FPosition.X:= FPosition.X + Vector.X;
  FPosition.Y:= FPosition.Y + Vector.Y;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.Render(Canvas: TPHXCanvas; const Offset: TVector2f);
begin
  Canvas.Rectangle(Offset.X + FPosition.X - 2,
                   Offset.Y + FPosition.Y - 2,
                   Offset.X + FPosition.X + 2,
                   Offset.Y + FPosition.Y + 2);
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.Project(const Axis: TVector2f; out Projection: TRangef);
var p: Single;
begin
  p:= VectorDot(Position, Axis);

	Projection.Min:= p;
	Projection.Max:= p;
end;


//------------------------------------------------------------------------------
procedure TPHXPoint.SetPosition(const Value: TVector2f);
begin
  FPosition := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.SetPositionX(const Value: Single);
begin
  FPosition.X := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXPoint.SetPositionY(const Value: Single);
begin
  FPosition.Y := Value;
end;

{$ENDREGION}

{$REGION 'TPHXBox'}

// TPHXBox
//==============================================================================
constructor TPHXBox.Create;
begin
  inherited Create;
  FName:= 'Box';
  FKind:= PHX_SHAPE_BOX;

  // Setup the collision axes
  FCollisionAxes.Count:= 2;
  FCollisionAxes.List^[0]:= Vector2f_AxisX;
  FCollisionAxes.List^[1]:= Vector2f_AxisY;

  SetSizeX(10);
  SetSizeY(10);
end;

//------------------------------------------------------------------------------
constructor TPHXBox.Create(const ASize: Single);
begin
  inherited Create;

  FName:= 'Box';
  FKind:= PHX_SHAPE_BOX;

  // Setup the collision axes
  FCollisionAxes.Count:= 2;
  FCollisionAxes.List^[0]:= Vector2f_AxisX;
  FCollisionAxes.List^[1]:= Vector2f_AxisY;

  SetSizeX(ASize);
  SetSizeY(ASize);
end;

//------------------------------------------------------------------------------
constructor TPHXBox.Create(const AWidth: Single; const AHeight: Single);
begin
  inherited Create;

  FName:= 'Box';
  FKind:= PHX_SHAPE_BOX;

  // Setup the collision axes
  FCollisionAxes.Count:= 2;
  FCollisionAxes.List^[0]:= Vector2f_AxisX;
  FCollisionAxes.List^[1]:= Vector2f_AxisY;

  SetSizeX(AWidth);
  SetSizeY(AHeight);
end;   

//------------------------------------------------------------------------------
procedure TPHXBox.LoadShape(Reader: TPHXReader);
begin
  FCenter:= Reader.ReadVector2f;
  FSize  := Reader.ReadVector2f;

  FHalfSize.X:= FSize.X * 0.5;
  FHalfSize.Y:= FSize.Y * 0.5;
end;

//------------------------------------------------------------------------------
procedure TPHXBox.SaveShape(Writer: TPHXWriter);
begin
  Writer.WriteVector2f(FCenter);
  Writer.WriteVector2f(FSize);
 end;

//------------------------------------------------------------------------------
function TPHXBox.Clone: TPHXShape;
begin
  Result:= TPHXBox.Create;

  TPHXBox(Result).FName      := FName;
  TPHXBox(Result).FCenter    := FCenter;
  TPHXBox(Result).FSize      := FSize;
  TPHXBox(Result).FHalfSize.X:= FHalfSize.X;
  TPHXBox(Result).FHalfSize.Y:= FHalfSize.Y;
end;


//------------------------------------------------------------------------------
procedure TPHXBox.Assign(Shape: TPHXShape);
begin
  inherited Assign(Shape);
  
  FCenter  := TPHXBox(Shape).Center;
  FSize    := TPHXBox(Shape).FSize;
  FHalfSize:= TPHXBox(Shape).FHalfSize;
end;

//------------------------------------------------------------------------------
function TPHXBox.GetBoundingBox: TRectf;
begin
  Result.Right := Center.X + FHalfSize.X;
  Result.Left  := Center.X - FHalfSize.X;
  Result.Bottom:= Center.Y + FHalfSize.Y;
  Result.Top   := Center.Y - FHalfSize.Y;
end;

//------------------------------------------------------------------------------
function TPHXBox.PointInside(const Point: TVector2f): Boolean;
begin
  Result:= (Point.X >= Center.X - FHalfSize.X) and
           (Point.X <= Center.X + FHalfSize.X) and
           (Point.Y >= Center.Y - FHalfSize.Y) and
           (Point.Y <= Center.Y + FHalfSize.Y);
end;



// Transforms the shape by a matrix
//------------------------------------------------------------------------------
procedure TPHXBox.Transform(const Matrix: TMatrix4f);
begin
  FCenter.X:= FCenter.X + Matrix.v[12];
  FCenter.Y:= FCenter.Y + Matrix.v[13];
end;

// Translate the shape by a vector
//------------------------------------------------------------------------------
procedure TPHXBox.Translate(const Vector: TVector2f);
begin
  FCenter.X:= FCenter.X + Vector.X;
  FCenter.Y:= FCenter.Y + Vector.Y;
end;


//------------------------------------------------------------------------------
procedure TPHXBox.Render(Canvas: TPHXCanvas; const Offset: TVector2f);
begin
  Canvas.Rectangle(Offset.X + Center.X - FHalfSize.X,
                   Offset.Y + Center.Y - FHalfSize.Y,
                   Offset.X + Center.X + FHalfSize.X,
                   Offset.Y + Center.Y + FHalfSize.Y);
end;

//------------------------------------------------------------------------------
procedure TPHXBox.Project(const Axis: TVector2f; out Projection: TRangef);
var p: Single;
var r: Single;
var dx: Single;
var dy: Single;
begin
  dx:= Abs( VectorDot(Axis, Vector2f_AxisX) ) ;
  dy:= Abs( VectorDot(Axis, Vector2f_AxisY) );

  p:= VectorDot(Center, Axis);
  r:= FHalfSize.x * dx +  FHalfSize.y * dy;

	Projection.Min:= p - r;
	Projection.Max:= p + r
end;

//------------------------------------------------------------------------------
procedure TPHXBox.SetSize(const Value: TVector2f);
begin
  FSize := Value;

  FHalfSize.X:= FSize.X * 0.5;
  FHalfSize.Y:= FSize.X * 0.5;
end;

//------------------------------------------------------------------------------
procedure TPHXBox.SetSizeX(const Value: Single);
begin
  FSize.X := Value;

  FHalfSize.X:= Value * 0.5;
end;
//------------------------------------------------------------------------------
procedure TPHXBox.SetSizeY(const Value: Single);
begin
  FSize.Y := Value;

  FHalfSize.Y:= Value * 0.5;
end;

{$ENDREGION}

{$REGION 'TPHXPolygon'}

// TPHXPolygon
//==============================================================================
constructor TPHXPolygon.Create;
begin
  inherited Create;

  FName:= 'Polygon';
  FKind:= PHX_SHAPE_POLYGON;
  FSize:= 0;
  FList:= nil;
end;


//------------------------------------------------------------------------------
constructor TPHXPolygon.Create(const Points: array of TVector2f);
var Index: Integer;
begin
  inherited Create;

  FName:= 'Polygon';
  FKind:= PHX_SHAPE_POLYGON;

  SetSize( Length(Points) );

  for Index := 0 to Size - 1 do
  begin
    FList^[Index]:= Points[Index];
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXPolygon.Destroy;
begin
  SetSize(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.CreateAxes;
var I,J: Integer;
var Edge: TVector2f;
var Axis: TVector2f;
begin
  CollisionAxes.Count:= Size;
  // Test #1: Separation axes of polygon A
  J:= Size - 1;
  For I:=0 to Size-1 do
  begin
    // Calculate the edge vertex
    Edge.X:= FList^[J].X - FList^[I].X;
    Edge.Y:= FList^[J].Y - FList^[I].Y;

    Edge:= VectorNormalize(Edge);

    // Calculate the normal of the edge
    Axis.X:= -Edge.Y;
    Axis.Y:=  Edge.X;

    CollisionAxes.List^[I]:= Axis;

    J:= I;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.CreateBox(const Width, Height: Single);
var W2: Single;
var H2: Single;
begin
  W2:= Width  * 0.5;
  H2:= Height * 0.5;

  SetSize(4);

  FList^[0].X:= - W2;  FList^[0].Y:= - H2;
  FList^[1].X:=   W2;  FList^[1].Y:= - H2;
  FList^[2].X:=   W2;  FList^[2].Y:=   H2;
  FList^[3].X:= - W2;  FList^[3].Y:=   H2;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.CreateBox(const Rect: TRectf);
begin
  SetSize(4);

  FList^[0].X:= Rect.Left ; FList^[0].Y:= Rect.Top;
  FList^[1].X:= Rect.Right; FList^[1].Y:= Rect.Top;
  FList^[2].X:= Rect.Right; FList^[2].Y:= Rect.Bottom;
  FList^[3].X:= Rect.Left ; FList^[3].Y:= Rect.Bottom;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.CreateCircle(const Radius: Single; const NumVertices: Integer);
var Angle: Single;
var Delta: Single;
var Index: Integer;
begin
  SetSize(NumVertices);

  if NumVertices > 0 then
  begin
    Angle:= PI / NumVertices;
    Delta:= (2.0 * PI) / NumVertices;

    for Index:= 0 to NumVertices-1 do
    begin
       FList^[Index].X:= Cos(Angle) * Radius;
       FList^[Index].Y:= Sin(Angle) * Radius;

       Angle:= Angle + Delta;
    end;
  end;
  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Add(const Point: TVector2f);
begin
  SetSize(Size + 1);

  FList^[Size-1].X:= Point.X;
  FList^[Size-1].Y:= Point.Y;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Add(const X, Y: Single);
begin
  SetSize(Size + 1);

  FList^[Size-1].X:= X;
  FList^[Size-1].Y:= Y;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Delete(Index: Integer);
begin
  Dec(FSize);

  if Index < FSize then
  begin
    System.Move(FList^[Index + 1], FList^[Index],  (FSize - Index) * SizeOf(TVector2f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.LoadShape(Reader: TPHXReader);
begin
  FSize:= Reader.ReadInteger;

  ReAllocMem(FList, FSize * SizeOf(TVector2f));

  Reader.Read(FList^, SizeOf(TVector2f) * FSize);

  CreateAxes;
end;
//------------------------------------------------------------------------------
procedure TPHXPolygon.SaveShape(Writer: TPHXWriter);
begin
  Writer.WriteInteger(FSize);

  Writer.Write(FList^, SizeOf(TVector2f) * FSize);
end;

//------------------------------------------------------------------------------
function TPHXPolygon.Clone: TPHXShape;
begin
  Result:= TPHXPolygon.Create;
  Result.Assign(Self);
//  Result.FName:= FName;
//  Result.SetSize(FSize);
//  Result.CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Assign(Shape: TPHXShape);
var Polygon: TPHXPolygon;
begin
  inherited Assign(Shape);

  Polygon:= TPHXPolygon(Shape);

  if Size <> Polygon.Size then
  begin
    // Match the size of the other polygon
    SetSize( Polygon.Size );
  end;

  // Copy
  Move(Polygon.List^, List^, FSize * SizeOf(TVector2f));


  CollisionAxes.Count:= Polygon.CollisionAxes.Count;
  Move(Polygon.CollisionAxes.List^, CollisionAxes.List^, CollisionAxes.Count * SizeOf(TVector2f));

 // Move(TPHXPolygon(Shape).^, FList^, FSize * SizeOf(TVector2f));
//  CreateAxes;
end;

//------------------------------------------------------------------------------
function TPHXPolygon.GetBoundingBox: TRectf;
var Index : Integer;
var Vector: TVector2f;
begin
  if FSize = 0 then Exit;
  
  Result.Top   := List^[0].Y;
  Result.Left  := List^[0].X;
  Result.Bottom:= List^[0].Y;
  Result.Right := List^[0].X;

  for Index:= 1 to FSize - 1 do
  begin
    Vector:= FList^[Index];

    if Vector.X > Result.Right  then Result.Right:= Vector.X;
    if Vector.X < Result.Left   then Result.Left := Vector.X;

    if Vector.Y > Result.Bottom then Result.Bottom:= Vector.Y;
    if Vector.Y < Result.Top    then Result.Top   := Vector.Y;
  end;
end;

//------------------------------------------------------------------------------
function TPHXPolygon.PointInside(const Point: TVector2f): Boolean;
var I,J: Integer;
begin
  Result:=False;
  J:= FSize - 1;
  for I := 0 to FSize - 1 do
  begin
    if ((
        ((FList^[I].Y <= Point.Y) and (Point.Y < FList^[J].Y)) or
        ((FList^[J].Y <= Point.Y) and (Point.Y < FList^[I].Y))
       ) and ( Point.X < (FList^[J].X - FList^[I].X) * ( Point.Y - FList^[I].Y) / (FList^[J].Y - FList^[I].Y) + FList^[I].X)) then
    begin
      Result:= not Result;
    end;
    J:= I;
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXPolygon.Transform(const Matrix: TMatrix4f);
var Vector: TVector2f;
var Index : Integer;
begin
  //  T.x = x * M.e11 + y * M.e12;
  //  T.y = x * M.e21 + y * M.e22;
  for Index:= 0 to FSize-1 do
  begin
    Vector:= FList^[Index];

    FList^[Index].X:= (Vector.X * Matrix.v[0]) + (Vector.Y * Matrix.v[4]) + Matrix.v[12];
    FList^[Index].Y:= (Vector.X * Matrix.v[1]) + (Vector.Y * Matrix.v[5]) + Matrix.v[13];
  end;
  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Translate(const Vector: TVector2f);
var Index  : Integer;
begin
  for Index:= 0 to FSize-1 do
  begin
    FList^[Index].X:= FList^[Index].X + Vector.X;
    FList^[Index].Y:= FList^[Index].Y + Vector.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Render(Canvas: TPHXCanvas; const Offset: TVector2f );
var Index : Integer;
var P1, P2: TVector2f;
begin
  if FSize <= 0 then Exit;

  P2.X:=Offset.X + FList^[FSize-1].X;
  P2.Y:=Offset.Y + FList^[FSize-1].Y;
  for Index:= 0 to FSize-1 do
  begin
    P1.X:= Offset.X + FList^[Index].X;
    P1.Y:= Offset.Y + FList^[Index].Y;

    Canvas.Line(P1, P2);

    P2:= P1;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.Project(const Axis: TVector2f; out Projection: TRangef);
var i: Integer;
var d: Single;
begin
  // Avoid index out of bounds errors
  if Size = 0 then
  begin
    Exit;
  end;

  d:= VectorDot(FList^[0], Axis);

  Projection.Min:= d;
  Projection.Max:= d;

  // Project all the vectors in the polygon on the axis
  for i:= 1 to Size - 1 do
  begin
    d:= VectorDot(FList^[i] , Axis);

    if (d < Projection.Min) then Projection.Min:= d  else
    if (d > Projection.Max) then Projection.Max:= d;
  end
end;



//------------------------------------------------------------------------------
function TPHXPolygon.GetCenter: TVector2f;
var vMin: TVector2f;
var vMax: TVector2f;
var Index : Integer;
var Vector: TVector2f;
begin
  vMin.X:= maxSingle;
  vMin.Y:= maxSingle;

  vMax.X:= MinSingle;
  vMax.Y:= MinSingle;

  for Index:= 0 to FSize-1 do
  begin
    Vector:= FList^[Index];

    if Vector.X > vMax.X then vMax.X:= Vector.X;
    if Vector.X < vMin.X then vMin.X:= Vector.X;
    
    if Vector.Y > vMax.Y then vMax.Y:= Vector.Y;
    if Vector.Y < vMin.Y then vMin.Y:= Vector.Y;
  end;
  Result.X:= (vMax.X + vMin.X) / 2;
  Result.Y:= (vMax.Y + vMin.Y) / 2;
end;

//------------------------------------------------------------------------------
procedure TPHXPolygon.SetSize(const AValue: Integer);
begin
  if Size <> AValue then
  begin
    FSize:= AValue;

    ReAllocMem(FList, FSize * SizeOf(TVector2f));
  end;
end;

//------------------------------------------------------------------------------
function TPHXPolygon.GetPoint(const Index: Integer): TVector2f;
begin
  Result:= FList^[Index];
end;


//------------------------------------------------------------------------------
procedure TPHXPolygon.SetPoint(const Index: Integer; const AValue: TVector2f);
begin
  FList^[Index]:= AValue;

  CreateAxes;
end;

{$ENDREGION}

{$REGION 'TPHXLine'}

// TPHXLine
//==============================================================================
constructor TPHXLine.Create;
begin
  inherited Create;

  FName:= 'Line';
  FKind:= PHX_SHAPE_LINE;

  // Setup the collision axes
  FCollisionAxes.Count:= 1;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.CreateAxes;
var Edge: TVector2f;
var Axis: TVector2f;
begin
  CollisionAxes.Count:= 1;

  Edge.X:= FMax.X - FMin.X;
  Edge.Y:= FMax.Y - FMin.Y;

  // Calculate the normal of the edge
  Axis.X:= -Edge.Y;
  Axis.Y:=  Edge.X;

  Axis:= VectorNormalize(Axis);

  CollisionAxes.List^[0]:= Axis;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.LoadShape(Reader: TPHXReader);
begin
  FMin:= Reader.ReadVector2f;
  FMax:= Reader.ReadVector2f;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.SaveShape(Writer: TPHXWriter);
begin
  Writer.WriteVector2f(FMin);
  Writer.WriteVector2f(FMax);
end;

//------------------------------------------------------------------------------
function TPHXLine.Clone: TPHXShape;
begin
  Result:= TPHXLine.Create;
  Result.Assign(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXLine.Assign(Shape: TPHXShape);
begin
  inherited Assign(Shape);
  
  FMin:= TPHXLine(Shape).FMin;
  FMax:= TPHXLine(Shape).FMax;

  CreateAxes;
end;

//------------------------------------------------------------------------------
function TPHXLine.GetBoundingBox: TRectf;
begin
  Result.Right := Min.X;
  Result.Left  := Min.X;
  Result.Bottom:= Min.Y;
  Result.Top   := Min.Y;

  if Max.X > Result.Right  then Result.Right := Max.X;
  if Max.X < Result.Left   then Result.Left  := Max.X;
  if Max.Y > Result.Bottom then Result.Bottom:= Max.Y;
  if Max.Y < Result.Top    then Result.Top   := Max.Y;
end;

//------------------------------------------------------------------------------
function TPHXLine.PointInside(const Point: TVector2f): Boolean;
var X,Y, U, L: Single;
begin
  // L = || Max - Min || ^ 2
  L:= VectorMagnitudeSqr( VectorSub(Max, Min) );

  if L = 0.0 then
  begin
    Result:= False;
    Exit;
  end;
  // Find the closest point on the line to the input point
  U:= (
       (Point.X - Min.X) * (Max.X - Min.X) +
       (Point.Y - Min.Y) * (Max.Y - Min.Y)
  ) / L;

  // Check so the interval is within the line
  if (U >= 0.0) and (U <= 1.0) then
  begin
    X:= Min.X + u * (Max.X - Min.X);
    Y:= Min.Y + u * (Max.Y - Min.Y);

    // The point is considered intersecting if it is within 2 units
    Result:= Sqr(Point.X - X) + Sqr(Point.Y - Y) < 4;
  end else
  begin
    Result:= False;
  end;
end;

// Transforms the shape by a matrix
//------------------------------------------------------------------------------
procedure TPHXLine.Transform(const Matrix: TMatrix4f);
begin
  // TODO: Rotate lines?
  FMin.X:= FMin.X + Matrix.v[12];
  FMin.Y:= FMin.Y + Matrix.v[13];

  FMax.X:= FMax.X + Matrix.v[12];
  FMax.Y:= FMax.Y + Matrix.v[13];
end;

// Translate the shape by a vector
//------------------------------------------------------------------------------
procedure TPHXLine.Translate(const Vector: TVector2f);
begin
  FMin.X:= FMin.X + Vector.X;
  FMin.Y:= FMin.Y + Vector.Y;

  FMax.X:= FMax.X + Vector.X;
  FMax.Y:= FMax.Y + Vector.Y;
end;


//------------------------------------------------------------------------------
procedure TPHXLine.Render(Canvas: TPHXCanvas; const Offset: TVector2f);
begin
  // Draw outline
  Canvas.Line(Offset.X + Min.X,
              Offset.Y + Min.Y,
              Offset.X + Max.X,
              Offset.Y + Max.Y);
end;

//------------------------------------------------------------------------------
procedure TPHXLine.Project(const Axis: TVector2f; out Projection: TRangef);
var d: Single;
begin
  d  := VectorDot(FMin, Axis);

  Projection.Min:= d;
  Projection.Max:= d;

  d:= VectorDot(FMax , Axis);

  if (d < Projection.Min) then Projection.Min:= d else
  if (d > Projection.Max) then Projection.Max:= d;
end;


//------------------------------------------------------------------------------
procedure TPHXLine.SetMax(const Value: TVector2f);
begin
  FMax := Value;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.SetMin(const Value: TVector2f);
begin
  FMin := Value;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.SetMaxX(const Value: Single);
begin
  FMax.X := Value;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.SetMaxY(const Value: Single);
begin
  FMax.Y := Value;

  CreateAxes;
end;


//------------------------------------------------------------------------------
procedure TPHXLine.SetMinX(const Value: Single);
begin
  FMin.X := Value;

  CreateAxes;
end;

//------------------------------------------------------------------------------
procedure TPHXLine.SetMinY(const Value: Single);
begin
  FMin.Y:= Value;

  CreateAxes;
end;

{$ENDREGION}

{$REGION 'TPHXCircle'}

// TPHXCircle
//==============================================================================
constructor TPHXCircle.Create;
begin
  inherited Create;
  FKind  := PHX_SHAPE_CIRCLE;
  FName  := 'Circle';
  FRadius:= Radius;
  FCenter:= Vector2f_Zero;

  // Setup the collision axes, this is not accurate as a cicle
  // can have infinite seperating axes, but its a ok approximation
  FCollisionAxes.Count:= 6;
  FCollisionAxes.List^[0].X:= 1;
  FCollisionAxes.List^[0].Y:= 0;

  FCollisionAxes.List^[1].X:= 0;
  FCollisionAxes.List^[1].Y:= 1;
  //
  FCollisionAxes.List^[2].X:= 0.86602538824;
  FCollisionAxes.List^[2].Y:= 0.5;

  FCollisionAxes.List^[3].X:= 0.5;
  FCollisionAxes.List^[3].Y:= 0.86602538824;

  FCollisionAxes.List^[4].X:= -0.86602538824;
  FCollisionAxes.List^[4].Y:=  0.5;

  FCollisionAxes.List^[5].X:= -0.5;
  FCollisionAxes.List^[5].Y:=  0.86602538824;
end;

//------------------------------------------------------------------------------
constructor TPHXCircle.Create(const Radius: Single);
begin
  Create;

  FRadius:= Radius;
end;

//------------------------------------------------------------------------------
procedure TPHXCircle.LoadShape(Reader: TPHXReader);
begin
  FCenter:= Reader.ReadVector2f;
  FRadius:= Reader.ReadSingle;
end;

//------------------------------------------------------------------------------
procedure TPHXCircle.SaveShape(Writer: TPHXWriter);
begin
  Writer.WriteVector2f(FCenter);
  Writer.WriteSingle  (FRadius);
end;

//------------------------------------------------------------------------------
function TPHXCircle.Clone: TPHXShape;
begin
  Result:= TPHXCircle.Create;
  Result.Assign(Self);
end;


//------------------------------------------------------------------------------
procedure TPHXCircle.Assign(Shape: TPHXShape);
begin
  inherited Assign(Shape);
  
  FCenter:= TPHXCircle(Shape).FCenter;
  FRadius:= TPHXCircle(Shape).FRadius;
end;

//------------------------------------------------------------------------------
function TPHXCircle.GetBoundingBox: TRectf;
begin
  Result.Right := Center.X + Radius;
  Result.Left  := Center.X - Radius;
  Result.Bottom:= Center.Y + Radius;
  Result.Top   := Center.Y - Radius;
end;

//------------------------------------------------------------------------------
function TPHXCircle.PointInside(const Point: TVector2f): Boolean;
begin
  Result:= (Sqr(Point.X - FCenter.X) + Sqr(Point.Y - FCenter.Y)) <= Sqr(FRadius);
end;


// Transforms the shape by a matrix
//------------------------------------------------------------------------------
procedure TPHXCircle.Transform(const Matrix: TMatrix4f);
begin
  FCenter.X:= FCenter.X + Matrix.v[12];
  FCenter.Y:= FCenter.Y + Matrix.v[13];
end;

// Translate the shape by a vector
//------------------------------------------------------------------------------
procedure TPHXCircle.Translate(const Vector: TVector2f);
begin
  FCenter.X:= FCenter.X + Vector.X;
  FCenter.Y:= FCenter.Y + Vector.Y;
end;

//------------------------------------------------------------------------------
procedure TPHXCircle.Render(Canvas: TPHXCanvas; const Offset: TVector2f);
begin
  Canvas.Circle(Offset.X + FCenter.X, Offset.Y + FCenter.Y, FRadius, 12);
end;

//------------------------------------------------------------------------------
procedure TPHXCircle.Project(const Axis: TVector2f; out Projection: TRangef);
var d, r: Single;
begin
  d:= VectorDot(Center, Axis);
  r:= Radius * VectorMagnitude(Axis);

  Projection.Min:= d - r;
  Projection.Max:= d + r;
end;

{$ENDREGION}

{$REGION 'TPHXShapeList'}

// TPHXShapeList
//==============================================================================
constructor TPHXShapeList.Create;
begin
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXShapeList.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.LoadFromStream(Stream: TStream);
var Header: array[1..9] of AnsiChar;
var Count : Integer;
var Index : Integer;
var Shape : TPHXShape;
var Kind  : TPHXShapeKind;
begin
  Header:= #0#0#0#0#0#0#0#0#0;
  Count := 0;

  Stream.Read(Header, SizeOf(Header));
  Stream.Read(Count , SizeOf(Count));

  if (Header <> 'PHXSHAPES') then
  begin
    TPHXLogger.Error('TPHXShapeList.LoadFromStream', 'Invalid shape file.');

    raise Exception.Create('Invalid shape file.');
  end;

  for Index:=0 to Count - 1 do
  begin
    // Read the shape kind
    Stream.Read(Kind, SizeOf(Kind));

    Shape:= CreateShape(Kind);
    Shape.LoadFromStream(Stream);

    FList.Add(Shape);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.SaveToStream(Stream: TStream);
var Header: array[1..9] of AnsiChar;
var Index : Integer;
var Count : Integer;
var Shape : TPHXShape;
begin
  Header:= 'PHXSHAPES';
  Count := FList.Count;

  Stream.Write(Header , SizeOf(Header));
  Stream.Write(Count  , SizeOf(Count));

  for Index:=0 to Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    // Write the shape kind
    Stream.Write(Shape.Kind, SizeOf(Shape.Kind));
    // Save the shape to the stream
    Shape.SaveToStream(Stream);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.Clear;
var Index: Integer;
begin
  for Index:=0 to FList.Count - 1 do
  begin
    TPHXShape(FList[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.LoadShape(const FileName: String): TPHXShape;
begin
  Result:= phxShape.LoadShape(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.Add(Shape: TPHXShape): TPHXShape;
begin
  Result:= Shape;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.AddPoint(const Name: String): TPHXPoint;
begin
  Result:= TPHXPoint.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.AddBox(const Name: String): TPHXBox;
begin
  Result:= TPHXBox.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.AddPolygon(const Name: String): TPHXPolygon;
begin
  Result:= TPHXPolygon.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.AddLine(const Name: String): TPHXLine;
begin
  Result:= TPHXLine.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.AddCircle(const Name: String): TPHXCircle;
begin
  Result:= TPHXCircle.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.Delete(const Index: Integer);
var Shape: TPHXShape;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Shape:= TPHXShape(FList[Index]);

  FList.Delete(Index);

  Shape.Free;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.Remove(const Shape: TPHXShape);
var Index: Integer;
begin
  Index:= FList.IndexOf(Shape);
  if Index <> -1 then
  begin
    FList.Delete(Index);

    Shape.Free;
  end;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.IndexOf(const Shape: TPHXShape): Integer;
begin
  Result:= FList.IndexOf(Shape);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.IndexOf(const Name: String): Integer;
var Index: Integer;
var Shape: TPHXShape;
begin
  for Index:=0 to FList.Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    if SameText(Shape.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;

  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.Find(const Name: String): TPHXShape;
var Index: Integer;
var Shape: TPHXShape;
begin
  for Index:=0 to FList.Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    if SameText(Shape.Name, Name) then
    begin
      Result:= Shape;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.Find(const Name: String; out Shape: TPHXShape): Boolean;
var Index  : Integer;
begin
  for Index:=0 to FList.Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    if SameText(Shape.Name, Name) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Shape := nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.ShapeAt(const X, Y: Single): TPHXShape;
var Pos  : TVector2f;
var Index: Integer;
var Shape: TPHXShape;
begin
  Pos.X:= X;
  Pos.Y:= Y;

  for Index:=0 to FList.Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    if Shape.PointInside(Pos) then
    begin
      Result:= Shape;
      Exit;
   end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.Render(Canvas: TPHXCanvas);
begin
  Render(Canvas, Vector2f_Zero);
end;

//------------------------------------------------------------------------------
procedure TPHXShapeList.Render(Canvas: TPHXCanvas; const Offset: TVector2f);
var Index: Integer;
var Shape: TPHXShape;
begin
  for Index:=0 to FList.Count - 1 do
  begin
    Shape:= TPHXShape(FList.List[Index]);

    Shape.Render(Canvas, Offset) ;
  end;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXShapeList.GetList: PShapeList;
begin
  Result:= PShapeList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXShapeList.GetItem(Index: Integer): TPHXShape;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXShape(FList[Index]);
end;

{$ENDREGION}




end.
