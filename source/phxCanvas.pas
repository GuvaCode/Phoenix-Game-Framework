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
unit phxCanvas;
//< Device independent implementation of pixel and fragment shaders for 2D scenes.

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxLogger,
  phxTypes,
  phxClasses,
  phxMath,
  phxGraphics,
  phxTexture;

const
  // Maximum number of vertices the canvas can buffer
  CanvasCapacityVertices = 16384;
  // Maximum number of inducues the canvas can buffer
  CanvasCapacityIndices  = 16384;

type

// Canvas class for rendering 2D primitives
//------------------------------------------------------------------------------
TPHXCanvas = class(TPHXCustomCanvas)
  private
    // The current primitive type
    FPrimitive: TPHXPrimitiveType;
    // The current texture
    FTexture: TPHXTexture;
    // The current blending mode
    FBlending: TPHXBlendMode;
    // The current color
    FColor: TColor4f;
    FTransform: TMatrix4f;

    FVertices: TPHXVertexList;
    FIndices : TPHXIndexList;
  protected
    // The number of drawcalls this canvas has produced
    FDrawCalls: Integer;
    // Flush the canvas
    procedure FlushCanvas; virtual; abstract;

    procedure SetPrimitive(const Value: TPHXPrimitiveType);
    procedure SetTexture(const Value: TPHXTexture);
    procedure SetBlending(const Value: TPHXBlendMode);
  public
    // Create a new canvas, use TPHXDevice.CreateCanvas instead
    constructor Create;
    // Destroys this canvas
    destructor Destroy; override;

    // Returns if the canvas needs flushing before adding a number of verticies and indicies
    function FlushRequired(const VertexCount, IndexCount: Integer): Boolean;

    // Flush the canvas, this sends the content of the vertex and index buffers to the graphics cards
    procedure Flush;

    // Low level rendering function for adding vertices and indicies to the canvas
    procedure Draw(const Primitive: TPHXPrimitiveType; Vertices: PVertexList; Indicies: PIndexList; VertexCount: Integer; IndexCount: Integer); overload;
    // Low level rendering function for adding vertices and indicies to the canvas
    procedure Draw(const Primitive: TPHXPrimitiveType; Vertices: PVertexList; Indicies: PIndexList; VertexCount: Integer; IndexCount: Integer; Texture: TPHXTexture); overload;

    // Change a vertex at a index in the vertex list
    procedure Vertex(Index: Integer; Position: TVector3f); overload;
    // Change a vertex at a index in the vertex list
    procedure Vertex(Index: Integer; Position: TVector3f; TexCoord: TVector2f); overload;

    // Draw a point
    procedure Point(const X, Y: Integer; const Size: Integer = 1); overload;
    // Draw a point
    procedure Point(const X, Y: Single; const Size: Integer = 1); overload;
    // Draw a point
    procedure Point(const Position: TVector2i; const Size: Integer = 1); overload;
    // Draw a point
    procedure Point(const Position: TVector2f; const Size: Integer = 1); overload;

    // Draw a line between two points
    procedure Line(const X1, Y1, X2, Y2: Integer); overload;
    // Draw a line between two points
    procedure Line(const X1, Y1, X2, Y2: Single); overload;
    // Draw a line between two points
    procedure Line(const P1, P2 : TVector2i); overload;
    // Draw a line between two points
    procedure Line(const P1, P2 : TVector2f); overload;
    // Draw a line between two points
    procedure Line(const P1, P2 : TVector3i); overload;
    // Draw a line between two points
    procedure Line(const P1, P2 : TVector3f); overload;

    // Draw a triangle between three points
    procedure Triangle(const P1, P2, P3: TVector2i); overload;
    // Draw a triangle between three points
    procedure Triangle(const P1, P2, P3: TVector2f); overload;

    // Draw a rectangle
    procedure Rectangle(const X1, Y1, X2, Y2: Single); overload;
    // Draw a rectangle
    procedure Rectangle(const Rect: TRectf); overload;
     // Draw a filled rectangle with custom texture coordinates
    procedure Rectangle(const Rect: TRectf; const Coord: TRectf); overload;

    // Draw a Circle
    procedure Circle(const Center: TVector2f; const Radius: Single; const Quality: Integer = 64); overload;
    // Draw a Circle
    procedure Circle(const X,Y: Single; const Radius: Single; const Quality: Integer = 64); overload;

    // Draw a Ellipse
    procedure Ellipse(const X,Y: Single; const xRadius, yRadius: Single; const Quality: Integer = 64); overload;
    // Draw a Ellipse
    procedure Ellipse(const Center: TVector2f; const Radius: TVector2f; const Quality: Integer = 64); overload;

    // Draw a arc
    procedure Arc(const Center: TVector2f; const Radius: TVector2f; const Angles: TRangef; const Quality: Integer = 32);

    // Draw a arrow from a start position to a end position, arrow size determines the size of the arror
    procedure Arrow(const StartPosition, EndPosition: TVector2f; const ArrowSize: Single);


    // Draw a filled triangle between three points
    procedure FilledTriangle(const P1, P2, P3: TVector3f ); overload;
    // Draw a filled triangle between three points
    procedure FilledTriangle(const P1, P2, P3: TVector2f ); overload;

    // Draw a filled rectangle
    procedure FilledRectangle(const X1, Y1, X2, Y2: Single); overload;
    // Draw a filled rectangle
    procedure FilledRectangle(const Rect: TRectf); overload;
    // Draw a filled rectangle with custom texture coordinates
    procedure FilledRectangle(const Rect: TRectf; const Coord: TRectf); overload;

    // Draw a filled ellipse
    procedure FilledEllipse(const Center, Radius: TVector2f; const Quality: Integer = 32);

    // Draw a filled arc
    procedure FilledArc(const Center: TVector2f; const Radius: TVector2f; const Angles: TRangef; const Quality: Integer = 32);

    // Draw a tiled background using the current texture
    procedure Background(const Width, Height: Integer);
    // Draw a overlay that can be used for fading the screen in and out
    procedure Overlay(const Width, Height: Integer; const Color: TColor4f);


    // Enable or disable clipping
    procedure SetClipEnabled(const Enabled: Boolean); virtual; abstract;
    // Set the clipping rectangle
    procedure SetClipRect(const Rect: TRecti); overload; virtual; abstract;
    // Set the clipping rectangle
    procedure SetClipRect(const Rect: TRectf); overload; virtual; abstract;


    // The primitive to render, changing this will flush the canvas
    property Primitive: TPHXPrimitiveType read FPrimitive write SetPrimitive;
    // Changes the texture, changing this will flush the canvas
    property Texture: TPHXTexture read FTexture write SetTexture;
    // Changes the blendmode, changing this will flush the canvas
    property Blending: TPHXBlendMode read FBlending write SetBlending;
    // The color to add to the verticies
    property Color: TColor4f read FColor write FColor;
    // Custom transformation matrix for transforming the verticies before rendering
    property Transform: TMatrix4f read FTransform write FTransform;

    // The vertex list
    property Vertices: TPHXVertexList read FVertices;
    // The index list
    property Indices: TPHXIndexList read FIndices;

    // The number of drawcalls
    property DrawCalls: Integer read FDrawCalls write FDrawCalls;
  end;

implementation

// TPHXCanvas
//==============================================================================
constructor TPHXCanvas.Create;
begin
  FColor    := clrWhite;
  FBlending := bmAlpha;
  FPrimitive:= PHX_TRIANGLES;
  FTransform:= Matrix4f_Identity;

  FVertices:= TPHXVertexList.Create(CanvasCapacityVertices);
  FIndices := TPHXIndexList.Create(CanvasCapacityIndices);
end;

//------------------------------------------------------------------------------
destructor TPHXCanvas.Destroy;
begin
  FVertices.Free;
  FIndices.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Flush;
begin
  if (FVertices.Count > 0) and (FIndices.Count > 0) then
  begin
    FlushCanvas;

    // Clear the verticies
    FVertices.Count:= 0;
    // Clear the indicies
    FIndices.Count:= 0;
  end;
end;

//------------------------------------------------------------------------------
function TPHXCanvas.FlushRequired(const VertexCount, IndexCount: Integer): Boolean;
begin
  Result:= (FVertices.Count + VertexCount > FVertices.Capacity) or (FIndices.Count + IndexCount > FIndices.Capacity);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Draw(const Primitive: TPHXPrimitiveType; Vertices: PVertexList; Indicies: PIndexList; VertexCount, IndexCount: Integer);
var VI: Integer;
var II: Integer;
var Index: Integer;
begin
  SetPrimitive(Primitive);

  if (FVertices.Count + VertexCount > FVertices.Capacity) or (FIndices.Count + IndexCount > FIndices.Capacity)  then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(VertexCount);

  // Copy the verticies to the buffer
  System.Move(Vertices^[0], FVertices.List^[VI], VertexCount * SizeOf(TPHXVertex));

  II:= FIndices.Alloc(IndexCount);

  // Copy the indicies to the buffer, we cant use a memcopy here as we have to alter the indexes
  for Index:= 0 to IndexCount - 1 do
  begin
    FIndices.List^[II + Index]:= Cardinal(VI) + Indicies^[Index];
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Draw(const Primitive: TPHXPrimitiveType; Vertices: PVertexList; Indicies: PIndexList; VertexCount: Integer; IndexCount: Integer; Texture: TPHXTexture);
var VI: Integer;
var II: Integer;
var Index: Integer;
begin
  SetTexture(Texture);
  SetPrimitive(Primitive);

  if (FVertices.Count + VertexCount > FVertices.Capacity) or (FIndices.Count + IndexCount > FIndices.Capacity)  then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(VertexCount);

  // Copy the verticies to the buffer
  System.Move(Vertices^[0], FVertices.List^[VI], VertexCount * SizeOf(TPHXVertex));

  II:= FIndices.Alloc(IndexCount);

  // Copy the indicies to the buffer, we cant use a memcopy here as we have to alter the indexes
  for Index:= 0 to IndexCount - 1 do
  begin
    FIndices.List^[II + Index]:= Cardinal(VI) + Indicies^[Index];
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Vertex(Index: Integer; Position: TVector3f);
begin
  FVertices.List^[Index].TexCoord.X:= 0;
  FVertices.List^[Index].TexCoord.Y:= 0;

  FVertices.List^[Index].Color:= Color;

  FVertices.List^[Index].Normal:= Vector3f_AxisZ;

  FVertices.List^[Index].Position.X:= Position.X;
  FVertices.List^[Index].Position.Y:= Position.Y;
  FVertices.List^[Index].Position.Z:= Position.Z;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Vertex(Index: Integer; Position: TVector3f; TexCoord: TVector2f);
begin
  FVertices.List^[Index].TexCoord.X:= TexCoord.X;
  FVertices.List^[Index].TexCoord.Y:= TexCoord.Y;

  FVertices.List^[Index].Color:= Color;

  FVertices.List^[Index].Normal:= Vector3f_AxisZ;

  FVertices.List^[Index].Position.X:= Position.X;
  FVertices.List^[Index].Position.Y:= Position.Y;
  FVertices.List^[Index].Position.Z:= Position.Z;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Point(const X,Y: Integer; const Size: Integer = 1);
begin
  Point(Vector2i(X,Y), Size);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Point(const X,Y: Single; const Size: Integer = 1);
begin
  Point(Vector2f(X,Y), Size);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Point(const Position: TVector2i; const Size: Integer = 1);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_POINTS);

  if FlushRequired(1, 1) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(1);

  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= Position.X;
  FVertices.List^[VI+0].Position.Y:= Position.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  II:= FIndices.Alloc(1);

  FIndices.List^[II+0]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Point(const Position: TVector2f; const Size: Integer = 1);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_POINTS);

  if FlushRequired(1, 1) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(1);

  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= Position.X;
  FVertices.List^[VI+0].Position.Y:= Position.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  II:= FIndices.Alloc(1);

  FIndices.List^[II+0]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const X1, Y1, X2, Y2: Integer);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= X1;
  FVertices.List^[VI+0].Position.Y:= Y1;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= X2;
  FVertices.List^[VI+1].Position.Y:= Y2;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const X1, Y1, X2, Y2: Single);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= X1;
  FVertices.List^[VI+0].Position.Y:= Y1;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= X2;
  FVertices.List^[VI+1].Position.Y:= Y2;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const P1, P2: TVector2i);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const P1, P2: TVector2f);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;


//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const P1, P2: TVector3i);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= P1.Z;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= P2.Z;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;


//------------------------------------------------------------------------------
procedure TPHXCanvas.Line(const P1, P2: TVector3f);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(2, 2) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(2);

  // Start point
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= P1.Z;

  FVertices.List^[VI+0].Color:= Color;

  // End point
  FVertices.List^[VI+1].TexCoord.X:= 0;
  FVertices.List^[VI+1].TexCoord.Y:= 0;

  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= P2.Z;

  FVertices.List^[VI+1].Color:= Color;

  II:= FIndices.Alloc(2);

  // Line #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
end;


//------------------------------------------------------------------------------
procedure TPHXCanvas.Triangle(const P1, P2, P3: TVector2i);
begin
  Line(P1, P2);
  Line(P2, P3);
  Line(P3, P1);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Triangle(const P1, P2, P3: TVector2f);
begin
  Line(P1, P2);
  Line(P2, P3);
  Line(P3, P1);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Rectangle(const X1, Y1, X2, Y2: Single);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(4, 6) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(4);

  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;
  FVertices.List^[VI+0].Position.X:= X1;
  FVertices.List^[VI+0].Position.Y:= Y1;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= FColor;
  FVertices.List^[VI+1].TexCoord.X:= 1;
  FVertices.List^[VI+1].TexCoord.Y:= 0;
  FVertices.List^[VI+1].Position.X:= X2;
  FVertices.List^[VI+1].Position.Y:= Y1;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= FColor;
  FVertices.List^[VI+2].TexCoord.X:= 1;
  FVertices.List^[VI+2].TexCoord.Y:= 1;
  FVertices.List^[VI+2].Position.X:= X2;
  FVertices.List^[VI+2].Position.Y:= Y2;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+3].Color:= FColor;
  FVertices.List^[VI+3].TexCoord.X:= 0;
  FVertices.List^[VI+3].TexCoord.Y:= 1;
  FVertices.List^[VI+3].Position.X:= X1;
  FVertices.List^[VI+3].Position.Y:= Y2;
  FVertices.List^[VI+3].Position.Z:= 0;

  II:= FIndices.Alloc(8);
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;

  FIndices.List^[II+2]:= VI+1;
  FIndices.List^[II+3]:= VI+2;

  FIndices.List^[II+4]:= VI+2;
  FIndices.List^[II+5]:= VI+3;

  FIndices.List^[II+6]:= VI+3;
  FIndices.List^[II+7]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Rectangle(const Rect: TRectf);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(4, 6) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(4);

  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;
  FVertices.List^[VI+0].Position.X:= Rect.Left;
  FVertices.List^[VI+0].Position.Y:= Rect.Top;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= FColor;
  FVertices.List^[VI+1].TexCoord.X:= 1;
  FVertices.List^[VI+1].TexCoord.Y:= 0;
  FVertices.List^[VI+1].Position.X:= Rect.Right;
  FVertices.List^[VI+1].Position.Y:= Rect.Top;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= FColor;
  FVertices.List^[VI+2].TexCoord.X:= 1;
  FVertices.List^[VI+2].TexCoord.Y:= 1;
  FVertices.List^[VI+2].Position.X:= Rect.Right;
  FVertices.List^[VI+2].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+3].Color:= FColor;
  FVertices.List^[VI+3].TexCoord.X:= 0;
  FVertices.List^[VI+3].TexCoord.Y:= 1;
  FVertices.List^[VI+3].Position.X:= Rect.Left;
  FVertices.List^[VI+3].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+3].Position.Z:= 0;

  II:= FIndices.Alloc(8);
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;

  FIndices.List^[II+2]:= VI+1;
  FIndices.List^[II+3]:= VI+2;

  FIndices.List^[II+4]:= VI+2;
  FIndices.List^[II+5]:= VI+3;

  FIndices.List^[II+6]:= VI+3;
  FIndices.List^[II+7]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Rectangle(const Rect: TRectf; const Coord: TRectf);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_LINES);

  if FlushRequired(4, 6) then
  begin
    Flush;
  end;

  VI:= FVertices.Alloc(4);

  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= Coord.Left;
  FVertices.List^[VI+0].TexCoord.Y:= Coord.Top;
  FVertices.List^[VI+0].Position.X:= Rect.Left;
  FVertices.List^[VI+0].Position.Y:= Rect.Top;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= FColor;
  FVertices.List^[VI+1].TexCoord.X:= Coord.Right;
  FVertices.List^[VI+1].TexCoord.Y:= Coord.Top;
  FVertices.List^[VI+1].Position.X:= Rect.Right;
  FVertices.List^[VI+1].Position.Y:= Rect.Top;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= FColor;
  FVertices.List^[VI+2].TexCoord.X:= Coord.Right;
  FVertices.List^[VI+2].TexCoord.Y:= Coord.Bottom;
  FVertices.List^[VI+2].Position.X:= Rect.Right;
  FVertices.List^[VI+2].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+3].Color:= FColor;
  FVertices.List^[VI+3].TexCoord.X:= Coord.Left;
  FVertices.List^[VI+3].TexCoord.Y:= Coord.Bottom;
  FVertices.List^[VI+3].Position.X:= Rect.Left;
  FVertices.List^[VI+3].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+3].Position.Z:= 0;

  II:= FIndices.Alloc(6);
  // Triangle #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
  FIndices.List^[II+2]:= VI+2;
  // Triangle #2
  FIndices.List^[II+3]:= VI+2;
  FIndices.List^[II+4]:= VI+3;
  FIndices.List^[II+5]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Circle(const Center: TVector2f; const Radius: Single; const Quality: Integer = 64);
begin
  Ellipse(Center, Vector2f(Radius, Radius), Quality);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Circle(const X,Y: Single; const Radius: Single; const Quality: Integer = 64);
begin
  Ellipse(Vector2f(X,Y), Vector2f(Radius, Radius), Quality);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Ellipse(const Center: TVector2f; const Radius: TVector2f; const Quality: Integer = 64);
var Index: Integer;
var Angle: Single;
var Delta: Single;
var VI: Integer;
var II: Integer;
var VC: Integer;
var VP: Integer;
begin
  SetPrimitive(PHX_LINES);

  VI:= FVertices.Alloc(Quality);

  Delta:= (2 * PI) / Quality;
  Angle:= 0;
  for Index:= 0 to Quality - 1 do
  begin
    // Current angle
    FVertices.List^[VI + Index].TexCoord.X:= 0;
    FVertices.List^[VI + Index].TexCoord.Y:= 0;

    FVertices.List^[VI + Index].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI + Index].Position.X:= Center.X + Cos(Angle) * Radius.X;
    FVertices.List^[VI + Index].Position.Y:= Center.Y + Sin(Angle) * Radius.Y;
    FVertices.List^[VI + Index].Position.Z:= 0;

    FVertices.List^[VI + Index].Color:= FColor;

    Angle:= Angle + Delta;
  end;

  // Draw the lines
  II:= FIndices.Alloc(Quality*2);
  VP:= VI + Quality - 1;
  for Index:=0 to Quality - 1 do
  begin
    VC:= VI + Index + 0;

    FIndices.List^[II + Index * 2 + 0]:= VP;
    FIndices.List^[II + Index * 2 + 1]:= VC;

    VP:= VC;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Ellipse(const X,Y: Single; const xRadius, yRadius: Single; const Quality: Integer = 64);
begin
  Ellipse(Vector2f(X,Y), Vector2f(xRadius, yRadius), Quality);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Arc(const Center, Radius: TVector2f; const Angles: TRangef; const Quality: Integer);
var Angle     : Single;
var AngleStart: Single;
var AngleEnd  : Single;
var AngleDelta : Single;
var Delta     : Single;
var Count     : Integer;
var Index     : Integer;
var SA        : Single;
var CA        : Single;
var VI         : Integer;
var II        : Integer;
begin
  SetPrimitive(PHX_LINES);
//  SetPrimitive(PHX_POINTS);

  AngleStart:= Angles.Min * DEG_TO_RAD;
  AngleEnd  := Angles.Max * DEG_TO_RAD;
  // Make shure the end angle is bigger then the start angle
  while AngleEnd < AngleStart do
  begin
    AngleEnd:= AngleEnd + 2 * PI;
  end;
  // More then one revolution
  while AngleEnd - AngleStart > 2 * PI do
  begin
    AngleEnd:= AngleEnd - 2 * PI;
  end;
  AngleDelta:= AngleEnd - AngleStart;

  // Nothing to draw
  if AngleDelta = 0 then Exit;

  // Get the number of requred verticies for the arc
  Count:= Round((AngleDelta / (2 * PI)) * Quality);

  // We need atleast three verticies
  if Count = 0 then Count:= 1;

  // We need one center vertex and one vertex for each detail step
  VI:= FVertices.Alloc(Count + 2);

  // Add the center vertex
  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0.5;
  FVertices.List^[VI+0].TexCoord.Y:= 0.5;
  FVertices.List^[VI+0].Position.X:= Center.X;
  FVertices.List^[VI+0].Position.Y:= Center.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  Angle:= AngleStart;
  Delta:= AngleDelta / (Count);
  // Add the outer vertexes
  for Index:= 1 to Count+1 do
  begin
    CA:= Cos(Angle);
    SA:= Sin(Angle);

    FVertices.List^[VI+Index].Color:= FColor;
    FVertices.List^[VI+Index].TexCoord.X:= 0.5      + CA * 0.5;
    FVertices.List^[VI+Index].TexCoord.Y:= 0.5      + SA * 0.5;
    FVertices.List^[VI+Index].Position.X:= Center.X + CA * Radius.X;
    FVertices.List^[VI+Index].Position.Y:= Center.Y + SA * Radius.Y;
    FVertices.List^[VI+Index].Position.Z:= 0;

    Angle:= Angle + Delta;
  end;

  II:= FIndices.Alloc(Count * 2 + 4);
  for Index:= 0 to Count do
  begin
    FIndices.List^[II+Index*2+0]:= VI + Index + 0;
    FIndices.List^[II+Index*2+1]:= VI + Index + 1;
  end;
  FIndices.List^[II+Count*2+2]:= 0;
  FIndices.List^[II+Count*2+3]:= Vertices.Count-1;
end;

//------------------------------------------------------------------------------
Procedure TPHXCanvas.Arrow(const StartPosition, EndPosition: TVector2f; const ArrowSize: Single);
var Length   : Single;
var Direction: TVector2f;
var Normal    : TVector2f;
var PArrowC: TVector2f;
var PArrow1: TVector2f;
var PArrow2: TVector2f;
begin
  Direction:= VectorSub(EndPosition, StartPosition);
  Length   := VectorMagnitude(Direction);
  Direction:= VectorNormalize(Direction);
  // Calculate the perpendicular vector to the diection vector
  Normal.X:= - Direction.Y;
  Normal.Y:=   Direction.X;

  // Calculate end position form the start position, direction and length
  //PEnd.X:= Position.X + Direction.X * Length;
  //PEnd.Y:= Position.Y + Direction.Y * Length;

  // Calculate the start position of the arrow
  PArrowC.X:= StartPosition.X + Direction.X * (Length - ArrowSize * 1.25);
  PArrowC.Y:= StartPosition.Y + Direction.Y * (Length - ArrowSize * 1.25);

  // Calculate the arrow vertex on the left side of the direction vector
  PArrow1.X:= PArrowC.X - Normal.X * ArrowSize * 0.5;
  PArrow1.Y:= PArrowC.Y - Normal.Y * ArrowSize * 0.5;

  // Calculate the arrow vertex on the right side of the direction vector
  PArrow2.X:= PArrowC.X + Normal.X * ArrowSize * 0.5;
  PArrow2.Y:= PArrowC.Y + Normal.Y * ArrowSize * 0.5;

  // Line from the start to the arrow position
  Line(StartPosition, PArrowC);
  // Perpendicular  line
  Line(PArrow1, PArrow2);
  // Line from left side to end position
  Line(PArrow1, EndPosition);
  // Line from right side to end position
  Line(PArrow2, EndPosition);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledTriangle(const P1, P2, P3: TVector2f);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  VI:= FVertices.Alloc(3);

  // Top - Left
  FVertices.List^[VI+0].TexCoord.X:= P1.X / 256;
  FVertices.List^[VI+0].TexCoord.Y:= P1.Y / 256;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+0].Color:= Color;

  // Top - Right
  FVertices.List^[VI+1].TexCoord.X:= P2.X / 256;
  FVertices.List^[VI+1].TexCoord.Y:= P2.Y / 256;
  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;
  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= 0;
  FVertices.List^[VI+1].Color:= Color;


  // Bottom - Right
  FVertices.List^[VI+2].TexCoord.X:= P3.X / 256;
  FVertices.List^[VI+2].TexCoord.Y:= P3.Y / 256;

  FVertices.List^[VI+2].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+2].Position.X:= P3.X;
  FVertices.List^[VI+2].Position.Y:= P3.Y;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= Color;

  II:= FIndices.Alloc(3);
  // Triangle #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
  FIndices.List^[II+2]:= VI+2;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledTriangle(const P1, P2, P3: TVector3f);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  VI:= FVertices.Alloc(3);

  // Top - Left
  FVertices.List^[VI+0].TexCoord.X:= P1.X / 256;
  FVertices.List^[VI+0].TexCoord.Y:= P1.Y / 256;

  FVertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+0].Position.X:= P1.X;
  FVertices.List^[VI+0].Position.Y:= P1.Y;
  FVertices.List^[VI+0].Position.Z:= P1.Z;

  FVertices.List^[VI+0].Color:= Color;

  // Top - Right
  FVertices.List^[VI+1].TexCoord.X:= P2.X / 256;
  FVertices.List^[VI+1].TexCoord.Y:= P2.Y / 256;
  FVertices.List^[VI+1].Normal:= Vector3f_AxisZ;
  FVertices.List^[VI+1].Position.X:= P2.X;
  FVertices.List^[VI+1].Position.Y:= P2.Y;
  FVertices.List^[VI+1].Position.Z:= P2.Z;
  FVertices.List^[VI+1].Color:= Color;


  // Bottom - Right
  FVertices.List^[VI+2].TexCoord.X:= P3.X / 256;
  FVertices.List^[VI+2].TexCoord.Y:= P3.Y / 256;

  FVertices.List^[VI+2].Normal:= Vector3f_AxisZ;

  FVertices.List^[VI+2].Position.X:= P3.X;
  FVertices.List^[VI+2].Position.Y:= P3.Y;
  FVertices.List^[VI+2].Position.Z:= P3.Z;

  FVertices.List^[VI+2].Color:= Color;

  II:= FIndices.Alloc(3);
  // Triangle #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
  FIndices.List^[II+2]:= VI+2;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledRectangle(const X1, Y1, X2, Y2: Single);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  VI:= FVertices.Alloc(4);

  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0;
  FVertices.List^[VI+0].TexCoord.Y:= 0;
  FVertices.List^[VI+0].Position.X:= X1;
  FVertices.List^[VI+0].Position.Y:= Y1;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= FColor;
  FVertices.List^[VI+1].TexCoord.X:= 1;
  FVertices.List^[VI+1].TexCoord.Y:= 0;
  FVertices.List^[VI+1].Position.X:= X2;
  FVertices.List^[VI+1].Position.Y:= Y1;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= FColor;
  FVertices.List^[VI+2].TexCoord.X:= 1;
  FVertices.List^[VI+2].TexCoord.Y:= 1;
  FVertices.List^[VI+2].Position.X:= X2;
  FVertices.List^[VI+2].Position.Y:= Y2;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+3].Color:= FColor;
  FVertices.List^[VI+3].TexCoord.X:= 0;
  FVertices.List^[VI+3].TexCoord.Y:= 1;
  FVertices.List^[VI+3].Position.X:= X1;
  FVertices.List^[VI+3].Position.Y:= Y2;
  FVertices.List^[VI+3].Position.Z:= 0;

  II:= FIndices.Alloc(6);
  // Triangle #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
  FIndices.List^[II+2]:= VI+2;
  // Triangle #2
  FIndices.List^[II+3]:= VI+2;
  FIndices.List^[II+4]:= VI+3;
  FIndices.List^[II+5]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledRectangle(const Rect: TRectf);
begin
  FilledRectangle(Rect, Rectf(0,0,1,1));
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledRectangle(const Rect: TRectf; const Coord: TRectf);
var VI: Integer;
var II: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  VI:= FVertices.Alloc(4);

  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= Coord.Left;
  FVertices.List^[VI+0].TexCoord.Y:= Coord.Top;
  FVertices.List^[VI+0].Position.X:= Rect.Left;
  FVertices.List^[VI+0].Position.Y:= Rect.Top;
  FVertices.List^[VI+0].Position.Z:= 0;

  FVertices.List^[VI+1].Color:= FColor;
  FVertices.List^[VI+1].TexCoord.X:= Coord.Right;
  FVertices.List^[VI+1].TexCoord.Y:= Coord.Top;
  FVertices.List^[VI+1].Position.X:= Rect.Right;
  FVertices.List^[VI+1].Position.Y:= Rect.Top;
  FVertices.List^[VI+1].Position.Z:= 0;

  FVertices.List^[VI+2].Color:= FColor;
  FVertices.List^[VI+2].TexCoord.X:= Coord.Right;
  FVertices.List^[VI+2].TexCoord.Y:= Coord.Bottom;
  FVertices.List^[VI+2].Position.X:= Rect.Right;
  FVertices.List^[VI+2].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+2].Position.Z:= 0;

  FVertices.List^[VI+3].Color:= FColor;
  FVertices.List^[VI+3].TexCoord.X:= Coord.Left;
  FVertices.List^[VI+3].TexCoord.Y:= Coord.Bottom;
  FVertices.List^[VI+3].Position.X:= Rect.Left;
  FVertices.List^[VI+3].Position.Y:= Rect.Bottom;
  FVertices.List^[VI+3].Position.Z:= 0;

  II:= FIndices.Alloc(6);
  // Triangle #1
  FIndices.List^[II+0]:= VI+0;
  FIndices.List^[II+1]:= VI+1;
  FIndices.List^[II+2]:= VI+2;
  // Triangle #2
  FIndices.List^[II+3]:= VI+2;
  FIndices.List^[II+4]:= VI+3;
  FIndices.List^[II+5]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledEllipse(const Center: TVector2f; const Radius: TVector2f; const Quality: Integer = 32);
var Angle: Single;
var Delta: Single;
var Index: Integer;
var SA   : Single;
var CA   : Single;
var VI   : Integer;
var II   : Integer;
var IC   : Integer;
var IP   : Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  // We need one center vertex and one vertex for each detail step
  VI:= FVertices.Alloc(Quality + 1);

  // Add the center vertex
  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0.5;
  FVertices.List^[VI+0].TexCoord.Y:= 0.5;
  FVertices.List^[VI+0].Position.X:= Center.X;
  FVertices.List^[VI+0].Position.Y:= Center.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  Delta:= (2 * PI) / Quality;
  Angle:= 0;
  // Add the outer vertexes
  for Index:= 1 to Quality do
  begin
    CA:= Cos(Angle);
    SA:= Sin(Angle);

    FVertices.List^[VI+Index].Color:= FColor;
    FVertices.List^[VI+Index].TexCoord.X:= 0.5      + CA * 0.5;
    FVertices.List^[VI+Index].TexCoord.Y:= 0.5      + SA * 0.5;
    FVertices.List^[VI+Index].Position.X:= Center.X + CA * Radius.X;
    FVertices.List^[VI+Index].Position.Y:= Center.Y + SA * Radius.Y;
    FVertices.List^[VI+Index].Position.Z:= 0;

    Angle:= Angle + Delta;
  end;

  // We will render one triangle per quality step
  II:= FIndices.Alloc(Quality * 3);

  IP:= VI + Quality;
  for Index:= 0 to Quality-1 do
  begin
    IC:= VI+Index+1;

    FIndices.List^[II+Index*3+0]:= VI;
    FIndices.List^[II+Index*3+1]:= IP;
    FIndices.List^[II+Index*3+2]:= IC;

    IP:= IC;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.FilledArc(const Center: TVector2f; const Radius: TVector2f; const Angles: TRangef; const Quality: Integer = 32);
var Angle     : Single;
var AngleStart: Single;
var AngleEnd  : Single;
var AngleDelta : Single;
var Delta     : Single;
var Count     : Integer;
var Index     : Integer;
var SA        : Single;
var CA        : Single;
var VI         : Integer;
var II        : Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  AngleStart:= Angles.Min * DEG_TO_RAD;
  AngleEnd  := Angles.Max * DEG_TO_RAD;
  // Make shure the end angle is bigger then the start angle
  while AngleEnd < AngleStart do
  begin
    AngleEnd:= AngleEnd + 2 * PI;
  end;
  // More then one revolution
  while AngleEnd - AngleStart > 2 * PI do
  begin
    AngleEnd:= AngleEnd - 2 * PI;
  end;
  AngleDelta:= AngleEnd - AngleStart;

  // Nothing to draw
  if AngleDelta = 0 then Exit;

  // Get the number of requred verticies for the arc
  Count:= Round((AngleDelta / (2 * PI)) * Quality);

  // We need atleast three verticies
  if Count = 0 then Count:= 1;


  // We need one center vertex and one vertex for each detail step
  VI:= FVertices.Alloc(Count + 2);

  // Add the center vertex
  FVertices.List^[VI+0].Color:= FColor;
  FVertices.List^[VI+0].TexCoord.X:= 0.5;
  FVertices.List^[VI+0].TexCoord.Y:= 0.5;
  FVertices.List^[VI+0].Position.X:= Center.X;
  FVertices.List^[VI+0].Position.Y:= Center.Y;
  FVertices.List^[VI+0].Position.Z:= 0;

  Angle:= AngleStart;
  Delta:= AngleDelta / (Count);
  // Add the outer vertexes
  for Index:= 1 to Count+1 do
  begin
    CA:= Cos(Angle);
    SA:= Sin(Angle);

    FVertices.List^[VI+Index].Color:= FColor;
    FVertices.List^[VI+Index].TexCoord.X:= 0.5      + CA * 0.5;
    FVertices.List^[VI+Index].TexCoord.Y:= 0.5      + SA * 0.5;
    FVertices.List^[VI+Index].Position.X:= Center.X + CA * Radius.X;
    FVertices.List^[VI+Index].Position.Y:= Center.Y + SA * Radius.Y;
    FVertices.List^[VI+Index].Position.Z:= 0;

    Angle:= Angle + Delta;
  end;

  // We will render one triangle per quality step
  II:= FIndices.Alloc(Count * 3);
  for Index:= 0 to Count-1 do
  begin
    FIndices.List^[II+Index*3+0]:= VI;
    FIndices.List^[II+Index*3+1]:= VI+Index+1;
    FIndices.List^[II+Index*3+2]:= VI+Index+2;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXCanvas.Overlay(const Width, Height: Integer; const Color: TColor4f);
begin
  Self.Color  := Color;
  Self.Texture:= nil;

  FilledRectangle(0,0, Width, Height);
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Background(const Width, Height: Integer);
var Rect : TRectf;
var Coord: TRectf;
begin
  Rect:= Rectf(0,0, Width, Height);

  if Assigned(Texture) then
  begin
    Coord:= Rectf(0.0, 0.0, Width / Texture.Width, Height / Texture.Height);
  end else
  begin
    Coord:= Rectf(0.0, 0.0, 1.0, 1.0);
  end;

  FilledRectangle(Rect, Coord);
end;





      (*
//------------------------------------------------------------------------------
procedure TPHXCanvas.Ellipse(const X, Y, xRadius, yRadius: Integer);
var Angle   : Integer;
var Current : TVector2f;
var Previous: TVector2f;
var VI: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  Previous.X:= X + Cos256(0) * xRadius;
  Previous.Y:= Y + Sin256(0) * yRadius;

  VI:= FVertices.Alloc(3 * 256 + 3);

  for Angle:=0 to 256 do
  begin
    Current.X:= X + Cos256(Angle) * xRadius;
    Current.Y:= Y + Sin256(Angle) * yRadius;

    // Center vertex
    FVertices.List^[VI].TexCoord.X:= 0.5;
    FVertices.List^[VI].TexCoord.Y:= 0.5;

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= X;
    FVertices.List^[VI].Position.Y:= Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Inc(VI);

    // Current angle
    FVertices.List^[VI].TexCoord.X:= (X - Current.X + xRadius) / ( 2 * xRadius);
    FVertices.List^[VI].TexCoord.Y:= (Y - Current.Y + yRadius) / ( 2 * yRadius);

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= Current.X;
    FVertices.List^[VI].Position.Y:= Current.Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Inc(VI);

    // Previous angle
    FVertices.List^[VI].TexCoord.X:= (X - Previous.X + xRadius) / ( 2 * xRadius);
    FVertices.List^[VI].TexCoord.Y:= (Y - Previous.Y + yRadius) / ( 2 * yRadius);

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= Previous.X;
    FVertices.List^[VI].Position.Y:= Previous.Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Previous:= Current;
  end;

  Flush;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.Ellipse(const X, Y, xRadius, yRadius: Single );
var Angle   : Integer;
var Current : TVector2f;
var Previous: TVector2f;
var VI: Integer;
begin
  SetPrimitive(PHX_TRIANGLES);

  Previous.X:= X + Cos256(0) * xRadius;
  Previous.Y:= Y + Sin256(0) * yRadius;

  VI:= FVertices.Alloc(3 * 256 + 3);

  for Angle:=0 to 256 do
  begin
    Current.X:= X + Cos256(Angle) * xRadius;
    Current.Y:= Y + Sin256(Angle) * yRadius;

    // Center vertex
    FVertices.List^[VI].TexCoord.X:= 0.5;
    FVertices.List^[VI].TexCoord.Y:= 0.5;

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= X;
    FVertices.List^[VI].Position.Y:= Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Inc(VI);

    // Current angle
    FVertices.List^[VI].TexCoord.X:= (X - Current.X + xRadius) / ( 2 * xRadius);
    FVertices.List^[VI].TexCoord.Y:= (Y - Current.Y + yRadius) / ( 2 * yRadius);

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= Current.X;
    FVertices.List^[VI].Position.Y:= Current.Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Inc(VI);

    // Previous angle
    FVertices.List^[VI].TexCoord.X:= (X - Previous.X + xRadius) / ( 2 * xRadius);
    FVertices.List^[VI].TexCoord.Y:= (Y - Previous.Y + yRadius) / ( 2 * yRadius);

    FVertices.List^[VI].Normal:= Vector3f_AxisZ;

    FVertices.List^[VI].Position.X:= Previous.X;
    FVertices.List^[VI].Position.Y:= Previous.Y;
    FVertices.List^[VI].Position.Z:= 0;

    FVertices.List^[VI].Color:= FColor;

    Previous:= Current;
  end;

  Flush;
end;

           *)

                     {
//------------------------------------------------------------------------------
procedure TPHXCanvas.Cube(const Position: TVector3f; const Size: TVector3f);
var Box: TBoxf;
var VI: Integer;
begin
  Box.MinX:= Position.X - Size.X * 0.5;
  Box.MaxX:= Position.X + Size.X * 0.5;

  Box.MinY:= Position.Y - Size.Y * 0.5;
  Box.MaxY:= Position.Y + Size.Y * 0.5;

  Box.MinZ:= Position.Z - Size.Z * 0.5;
  Box.MaxZ:= Position.Z + Size.Z * 0.5;

  VI:= FVertices.Alloc(8);

  // Top
  Vertex(VI+0, TVector3f.Create(Box.MinX, Box.MinY, Box.MaxZ), TVector2f.Create(0, 0));
  Vertex(VI+1, TVector3f.Create(Box.MaxX, Box.MinY, Box.MaxZ), TVector2f.Create(1, 0));
  Vertex(VI+2, TVector3f.Create(Box.MinX, Box.MaxY, Box.MaxZ), TVector2f.Create(0, 1));
  Vertex(VI+3, TVector3f.Create(Box.MaxX, Box.MaxY, Box.MaxZ), TVector2f.Create(1, 1));

  // Bottom verticies
  Vertex(VI+4, TVector3f.Create(Box.MinX, Box.MinY, Box.MinZ), TVector2f.Create(0, 0));
  Vertex(VI+5, TVector3f.Create(Box.MaxX, Box.MinY, Box.MinZ), TVector2f.Create(1, 0));
  Vertex(VI+6, TVector3f.Create(Box.MinX, Box.MaxY, Box.MinZ), TVector2f.Create(0, 1));
  Vertex(VI+7, TVector3f.Create(Box.MaxX, Box.MaxY, Box.MinZ), TVector2f.Create(1, 1));

  // MaxZ
  IndexedRectangle(VI+0, VI+1, VI+2, VI+3);
  // MinZ
  IndexedRectangle(VI+4, VI+5, VI+6, VI+7);

  // MaxX
  IndexedRectangle(VI+1, VI+3, VI+5, VI+7);
  // MinX
  IndexedRectangle(VI+0, VI+2, VI+4, VI+6);

  // MaxY
  IndexedRectangle(VI+2, VI+3, VI+6, VI+7);
  // MinY
  IndexedRectangle(VI+0, VI+1, VI+4, VI+5);


  BeginUpdate;
  try

    IndexedLine(VI+0, VI+1);
    IndexedLine(VI+1, VI+2);
    IndexedLine(VI+2, VI+3);
    IndexedLine(VI+3, VI+0);

    // Bottom
    IndexedLine(VI+4, VI+5);
    IndexedLine(VI+5, VI+6);
    IndexedLine(VI+6, VI+7);
    IndexedLine(VI+7, VI+4);
    // Sides
    IndexedLine(VI+0, VI+4);
    IndexedLine(VI+1, VI+5);
    IndexedLine(VI+2, VI+6);
    IndexedLine(VI+3, VI+7);
  finally
    EndUpdate;
  end;
end;
  }

  (*

//------------------------------------------------------------------------------
procedure TPHXDrawBuffer.Box(const Box: TBoxf);
var P: array[0..8] of TVector3f;
begin

  P[0].X:= Box.MinX;
  P[0].Y:= Box.MinY;
  P[0].Z:= Box.MinZ;

  P[1].X:= Box.MaxX;
  P[1].Y:= Box.MinY;
  P[1].Z:= Box.MinZ;

  P[2].X:= Box.MaxX;
  P[2].Y:= Box.MaxY;
  P[2].Z:= Box.MinZ;

  P[3].X:= Box.MinX;
  P[3].Y:= Box.MaxY;
  P[3].Z:= Box.MinZ;

  P[4].X:= Box.MinX;
  P[4].Y:= Box.MinY;
  P[4].Z:= Box.MaxZ;

  P[5].X:= Box.MaxX;
  P[5].Y:= Box.MinY;
  P[5].Z:= Box.MaxZ;

  P[6].X:= Box.MaxX;
  P[6].Y:= Box.MaxY;
  P[6].Z:= Box.MaxZ;

  P[7].X:= Box.MinX;
  P[7].Y:= Box.MaxY;
  P[7].Z:= Box.MaxZ;

  // TODO; Check triangle winding

  // Front Face
  Rectangle(P[0], P[1], P[2], P[3]);
  // Back Face
  Rectangle(P[4], P[5], P[6], P[7]);

  // Top Face
  Rectangle(P[0], P[1], P[5], P[4]);
  // Bottom Face
  Rectangle(P[2], P[3], P[7], P[6]);

  // Right face
  Rectangle(P[1], P[2], P[6], P[5]);
  // Left Face
  Rectangle(P[0], P[3], P[7], P[4]);

  if not FBatching then Flush;

end; *)
        {
//const GRID_COLOR  : TColor4f = (Red: 080/255; Green: 080/255; Blue: 080/255; Alpha: 080/255);
//const GRID_MARK   : TColor4f = (Red: 160/255; Green: 160/255; Blue: 160/255; Alpha: 160/255);
//const GRID_CENTER : TColor4f = (Red: 160/255; Green: 160/255; Blue: 000/255; Alpha: 160/255);

const GRID_COLOR  : TColor4f = (Red: 040/255; Green: 040/255; Blue: 040/255; Alpha: 040/255);
const GRID_MARK   : TColor4f = (Red: 080/255; Green: 080/255; Blue: 080/255; Alpha: 080/255);
const GRID_CENTER : TColor4f = (Red: 080/255; Green: 080/255; Blue: 000/255; Alpha: 080/255);

//------------------------------------------------------------------------------
procedure TPHXCanvas.GridX(const Position: TVector3f; const Size, Interval, Marks: Integer);
var X,Y: Integer;
var SizeDiv2: Single;
begin
  SetPrimitive(PHX_LINES);

  SizeDiv2:= Size / 2;

  X:= 1;
  while X < SizeDiv2 do
  begin
    if (X mod marks)  = 0 then
    begin
      FColor:= GRID_MARK;
    end else
    begin
      FColor:= GRID_COLOR;
    end;

    Line( TVector3f.Create(Position.X, Position.Y + X, Position.Z - SizeDiv2), TVector3f.Create(Position.X, Position.Y + X, Position.Z + SizeDiv2));
    Line( TVector3f.Create(Position.X, Position.Y - X, Position.Z - SizeDiv2), TVector3f.Create(Position.X, Position.Y - X, Position.Z + SizeDiv2));

    X:= X + Interval;
  end;

  Y:= 1;
  while Y < SizeDiv2 do
  begin
    if (Y mod marks)  = 0 then
    begin
      FColor:= GRID_MARK;
    end else
    begin
      FColor:= GRID_COLOR;
    end;

    Line( TVector3f.Create(Position.X, Position.Y - SizeDiv2, Position.Z + Y), TVector3f.Create(Position.X, Position.Y + SizeDiv2, Position.Z + Y));
    Line( TVector3f.Create(Position.X, Position.Y - SizeDiv2, Position.Z - Y), TVector3f.Create(Position.X, Position.Y + SizeDiv2, Position.Z - Y));

    Y:= Y + Interval;
  end;

  FColor:= GRID_CENTER;
  // Draw the center lines
  Line( TVector3f.Create(Position.X          , Position.Y, Position.Z - SizeDiv2), TVector3f.Create(Position.X           , Position.Y, Position.Z + SizeDiv2));
  Line( TVector3f.Create(Position.X -SizeDiv2, Position.Y, Position.Z           ), TVector3f.Create(Position.X + SizeDiv2, Position.Y, Position.Z));

  Flush;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.GridY(const Position: TVector3f; const Size, Interval, Marks: Integer);
var X,Y: Integer;
var SizeDiv2: Single;
begin
  SetPrimitive(PHX_LINES);

  SizeDiv2:= Size / 2;

  X:= 1;
  while X < SizeDiv2 do
  begin
    if (X mod marks)  = 0 then
    begin
      FColor:= GRID_MARK;
    end else
    begin
      FColor:= GRID_COLOR;
    end;

    Line( TVector3f.Create(Position.X + X, Position.Y, Position.Z - SizeDiv2), TVector3f.Create(Position.X + X, Position.Y, Position.Z + SizeDiv2));
    Line( TVector3f.Create(Position.X - X, Position.Y, Position.Z - SizeDiv2), TVector3f.Create(Position.X - X, Position.Y, Position.Z + SizeDiv2));

    X:= X + Interval;
  end;

  Y:= 1;
  while Y < SizeDiv2 do
  begin
    if (Y mod marks)  = 0 then
    begin
      FColor:= GRID_MARK;
    end else
    begin
      FColor:= GRID_COLOR;
    end;

    Line( TVector3f.Create(Position.X -SizeDiv2, Position.Y, Position.Z + Y), TVector3f.Create(Position.X + SizeDiv2, Position.Y, Position.Z + Y));
    Line( TVector3f.Create(Position.X -SizeDiv2, Position.Y, Position.Z - Y), TVector3f.Create(Position.X + SizeDiv2, Position.Y, Position.Z - Y));

    Y:= Y + Interval;
  end;

  FColor:= GRID_CENTER;
  // Draw the center lines
  Line( TVector3f.Create(Position.X          , Position.Y, Position.Z - SizeDiv2), TVector3f.Create(Position.X           , Position.Y, Position.Z + SizeDiv2));
  Line( TVector3f.Create(Position.X -SizeDiv2, Position.Y, Position.Z           ), TVector3f.Create(Position.X + SizeDiv2, Position.Y, Position.Z));

  Flush;
end;
      }

      (*
// X = Green
// Y = Blue
// Z = Maroon
//------------------------------------------------------------------------------
procedure TPHXCanvas.Origin(const Position: TVector3f; const Length: Single);
var VI: Integer;
begin
  VI:= FVertices.Alloc(6);
   {
  // X axis
  Color:= clrGreen;
  Vertex(VI+0, TVector3f.Create(Position.X - L2, Position.Y - L2, Position.Z - L2));
  Vertex(VI+1, TVector3f.Create(Position.X + L2, Position.Y - L2, Position.Z - L2));

  // Y Axis
  Color:= clrBlue;
  Vertex(VI+2, TVector3f.Create(Position.X - L2, Position.Y - L2, Position.Z - L2));
  Vertex(VI+3, TVector3f.Create(Position.X - L2, Position.Y + L2, Position.Z - L2));

  // Z-Axis
  Color:= clrMaroon;
  Vertex(VI+4, TVector3f.Create(Position.X - L2, Position.Y - L2, Position.Z - L2));
  Vertex(VI+5, TVector3f.Create(Position.X - L2, Position.Y - L2, Position.Z + L2));
       }
  // X axis
  Color:= clrGreen;
  Vertex(VI+0, TVector3f.Create(Position.X         , Position.Y         , Position.Z));
  Vertex(VI+1, TVector3f.Create(Position.X + Length, Position.Y         , Position.Z));

  // Y Axis
  Color:= clrBlue;
  Vertex(VI+2, TVector3f.Create(Position.X         , Position.Y         , Position.Z));
  Vertex(VI+3, TVector3f.Create(Position.X         , Position.Y + Length, Position.Z));

  // Z-Axis
  Color:= clrMaroon;
  Vertex(VI+4, TVector3f.Create(Position.X         , Position.Y         , Position.Z));
  Vertex(VI+5, TVector3f.Create(Position.X         , Position.Y         , Position.Z + Length));

  BeginUpdate;
  try
    // X-Axis
    IndexedLine(VI+0, VI+1);
    // Y-Axis
    IndexedLine(VI+2, VI+3);
    // Z-Axis
    IndexedLine(VI+4, VI+5);
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.BoundingBox(const Box: TBoxf);
var VI: Integer;
begin
  VI:= FVertices.Alloc(8);

  // Top
  Vertex(VI+0, TVector3f.Create(Box.MinX, Box.MinY, Box.MaxZ));
  Vertex(VI+1, TVector3f.Create(Box.MaxX, Box.MinY, Box.MaxZ));
  Vertex(VI+2, TVector3f.Create(Box.MaxX, Box.MaxY, Box.MaxZ));
  Vertex(VI+3, TVector3f.Create(Box.MinX, Box.MaxY, Box.MaxZ));

  // Bottom verticies
  Vertex(VI+4, TVector3f.Create(Box.MinX, Box.MinY, Box.MinZ));
  Vertex(VI+5, TVector3f.Create(Box.MaxX, Box.MinY, Box.MinZ));
  Vertex(VI+6, TVector3f.Create(Box.MaxX, Box.MaxY, Box.MinZ));
  Vertex(VI+7, TVector3f.Create(Box.MinX, Box.MaxY, Box.MinZ));

  BeginUpdate;
  try
    // Top
    IndexedLine(VI+0, VI+1);
    IndexedLine(VI+1, VI+2);
    IndexedLine(VI+2, VI+3);
    IndexedLine(VI+3, VI+0);

    // Bottom
    IndexedLine(VI+4, VI+5);
    IndexedLine(VI+5, VI+6);
    IndexedLine(VI+6, VI+7);
    IndexedLine(VI+7, VI+4);
    // Sides
    IndexedLine(VI+0, VI+4);
    IndexedLine(VI+1, VI+5);
    IndexedLine(VI+2, VI+6);
    IndexedLine(VI+3, VI+7);
  finally
    EndUpdate;
  end;
end;

        *)
//------------------------------------------------------------------------------
procedure TPHXCanvas.SetPrimitive(const Value: TPHXPrimitiveType);
begin
  // Flush the buffer if changing the primitive type
  if FPrimitive <> Value then
  begin
    Flush();

    FPrimitive:= Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCanvas.SetTexture(const Value: TPHXTexture);
begin
  if FTexture <> Value then
  begin
    Flush();

    FTexture:= Value;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXCanvas.SetBlending(const Value: TPHXBlendMode);
begin
  if FBlending <> Value then
  begin
    Flush();

    FBlending:= Value;
  end;
end;


end.
