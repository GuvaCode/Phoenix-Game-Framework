////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix 2D Game Library                                                 //
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
//    http://www.phoenixlib.net                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxPrimitives;
//< Classes for rendering 3D primitves

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxTypes,
  phxMath,
  phxClasses;

type

// Class for rendering primitives
//------------------------------------------------------------------------------
TPHXPrimitives = class
  private
    FBuffer: TPHXBuffer;
    FColor: TColor4f;
  public
    // Create a new primitives class
    constructor Create(ABuffer: TPHXBuffer);

    // Draw a line between two points
    procedure Line(const P1, P2 : TVector3f); overload;

    // Draw a triangle between three points
    procedure Rectangle(const P1, P2, P3, P4: TVector3f ); overload;

		// Draw a cube
    procedure Cube(const Position: TVector3f; const Size: TVector3f);
		//
    procedure Box(const Box: TBoxf);

    // Draw a grid
    procedure Grid(const Position: TVector3f; const Size, Interval, Marks: Integer);
    // Draw a grid
    procedure GridX(const Position: TVector3f; const Size, Interval, Marks: Integer);
    // Draw a grid
    procedure GridY(const Position: TVector3f; const Size, Interval, Marks: Integer);

    // Draw a origin cross showing the axes of the coordinate system
    procedure Origin(const Position: TVector3f; const Length: Single);

    // Render a bonding box
    procedure BoundingBox(const Box: TBoxf);

    // The buffer to render to
    property Buffer: TPHXBuffer read FBuffer;
    // The current color
    property Color: TColor4f read FColor write FColor;
  end;


implementation

//------------------------------------------------------------------------------
constructor TPHXPrimitives.Create(ABuffer: TPHXBuffer);
begin
  FBuffer:= ABuffer;
  FColor:= clrWhite;
end;

//------------------------------------------------------------------------------
procedure TPHXPrimitives.Line(const P1, P2: TVector3f);
var VI: Integer;
var II: Integer;
begin
  Buffer.Primitive:= PHX_LINES;

  VI:= Buffer.Vertices.Alloc(2);

  // Start point
  Buffer.Vertices.List^[VI+0].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+0].TexCoord.Y:= 0;

  Buffer.Vertices.List^[VI+0].Normal:= Vector3f_AxisZ;

  Buffer.Vertices.List^[VI+0].Position.X:= P1.X;
  Buffer.Vertices.List^[VI+0].Position.Y:= P1.Y;
  Buffer.Vertices.List^[VI+0].Position.Z:= P1.Z;

  Buffer.Vertices.List^[VI+0].Color:= FColor;

  // End point
  Buffer.Vertices.List^[VI+1].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+1].TexCoord.Y:= 0;

  Buffer.Vertices.List^[VI+1].Normal:= Vector3f_AxisZ;

  Buffer.Vertices.List^[VI+1].Position.X:= P2.X;
  Buffer.Vertices.List^[VI+1].Position.Y:= P2.Y;
  Buffer.Vertices.List^[VI+1].Position.Z:= P2.Z;

  Buffer.Vertices.List^[VI+1].Color:= FColor;

  II:= Buffer.Indices.Alloc(2);

  Buffer.Indices.List^[II+0]:= VI+0;
  Buffer.Indices.List^[II+1]:= VI+1;
end;

//------------------------------------------------------------------------------
procedure TPHXPrimitives.Rectangle(const P1, P2, P3, P4: TVector3f);
var VI: Integer;
var II: Integer;
begin
  Buffer.Primitive:= PHX_TRIANGLES;

  VI:= Buffer.Vertices.Alloc(4);

  Buffer.Vertices.List^[VI+0].Color:= FColor;
  Buffer.Vertices.List^[VI+0].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+0].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+0].Position.X:= P1.X;
  Buffer.Vertices.List^[VI+0].Position.Y:= P1.Y;
  Buffer.Vertices.List^[VI+0].Position.Z:= P1.Z;

  Buffer.Vertices.List^[VI+1].Color:= FColor;
  Buffer.Vertices.List^[VI+1].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+1].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+1].Position.X:= P2.X;
  Buffer.Vertices.List^[VI+1].Position.Y:= P2.Y;
  Buffer.Vertices.List^[VI+1].Position.Z:= P2.Z;

  Buffer.Vertices.List^[VI+2].Color:= FColor;
  Buffer.Vertices.List^[VI+2].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+2].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+2].Position.X:= P3.X;
  Buffer.Vertices.List^[VI+2].Position.Y:= P3.Y;
  Buffer.Vertices.List^[VI+2].Position.Z:= P3.Z;

  Buffer.Vertices.List^[VI+3].Color:= FColor;
  Buffer.Vertices.List^[VI+3].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+3].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+3].Position.X:= P4.X;
  Buffer.Vertices.List^[VI+3].Position.Y:= P4.Y;
  Buffer.Vertices.List^[VI+3].Position.Z:= P4.Z;

  II:= Buffer.Indices.Alloc(6);
  // Triangle #1
  Buffer.Indices.List^[II+0]:= VI+0;
  Buffer.Indices.List^[II+1]:= VI+1;
  Buffer.Indices.List^[II+2]:= VI+2;
  // Triangle #2
  Buffer.Indices.List^[II+3]:= VI+2;
  Buffer.Indices.List^[II+4]:= VI+3;
  Buffer.Indices.List^[II+5]:= VI+0;
end;

//------------------------------------------------------------------------------
procedure TPHXPrimitives.Cube(const Position: TVector3f; const Size: TVector3f);
var SizeDiv2: TVector3f;
var P: array[0..8] of TVector3f;
begin
  SizeDiv2.X:= Size.X * 0.5;
  SizeDiv2.Y:= Size.Y * 0.5;
  SizeDiv2.Z:= Size.Z * 0.5;

  P[0].X:= Position.X - SizeDiv2.X;
  P[0].Y:= Position.Y - SizeDiv2.Y;
  P[0].Z:= Position.Z - SizeDiv2.Z;

  P[1].X:= Position.X + SizeDiv2.X;
  P[1].Y:= Position.Y - SizeDiv2.Y;
  P[1].Z:= Position.Z - SizeDiv2.Z;

  P[2].X:= Position.X + SizeDiv2.X;
  P[2].Y:= Position.Y + SizeDiv2.Y;
  P[2].Z:= Position.Z - SizeDiv2.Z;

  P[3].X:= Position.X - SizeDiv2.X;
  P[3].Y:= Position.Y + SizeDiv2.Y;
  P[3].Z:= Position.Z - SizeDiv2.Z;

  P[4].X:= Position.X - SizeDiv2.X;
  P[4].Y:= Position.Y - SizeDiv2.Y;
  P[4].Z:= Position.Z + SizeDiv2.Z;

  P[5].X:= Position.X + SizeDiv2.X;
  P[5].Y:= Position.Y - SizeDiv2.Y;
  P[5].Z:= Position.Z + SizeDiv2.Z;

  P[6].X:= Position.X + SizeDiv2.X;
  P[6].Y:= Position.Y + SizeDiv2.Y;
  P[6].Z:= Position.Z + SizeDiv2.Z;

  P[7].X:= Position.X - SizeDiv2.X;
  P[7].Y:= Position.Y + SizeDiv2.Y;
  P[7].Z:= Position.Z + SizeDiv2.Z;

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
end;



//------------------------------------------------------------------------------
procedure TPHXPrimitives.Box(const Box: TBoxf);
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

end;
//------------------------------------------------------------------------------
procedure TPHXPrimitives.BoundingBox(const Box: TBoxf);
var VI: Integer;
var II: Integer;
begin
  Buffer.Primitive:= PHX_LINES;

  VI:= Buffer.Vertices.Alloc(8);
  // X- Y- Z-
  Buffer.Vertices.List^[VI+0].Color:= FColor;
  Buffer.Vertices.List^[VI+0].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+0].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+0].Position.X:= Box.MinX;
  Buffer.Vertices.List^[VI+0].Position.Y:= Box.MinY;
  Buffer.Vertices.List^[VI+0].Position.Z:= Box.MinZ;
  // X+ Y- Z-
  Buffer.Vertices.List^[VI+1].Color:= FColor;
  Buffer.Vertices.List^[VI+1].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+1].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+1].Position.X:= Box.MaxX;
  Buffer.Vertices.List^[VI+1].Position.Y:= Box.MinY;
  Buffer.Vertices.List^[VI+1].Position.Z:= Box.MinZ;
  // X- Y+ Z-
  Buffer.Vertices.List^[VI+2].Color:= FColor;
  Buffer.Vertices.List^[VI+2].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+2].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+2].Position.X:= Box.MinX;
  Buffer.Vertices.List^[VI+2].Position.Y:= Box.MaxY;
  Buffer.Vertices.List^[VI+2].Position.Z:= Box.MinZ;
  // X+ Y+ Z-
  Buffer.Vertices.List^[VI+3].Color:= FColor;
  Buffer.Vertices.List^[VI+3].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+3].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+3].Position.X:= Box.MaxZ;
  Buffer.Vertices.List^[VI+3].Position.Y:= Box.MaxY;
  Buffer.Vertices.List^[VI+3].Position.Z:= Box.MinZ;

  // X- Y- Z+
  Buffer.Vertices.List^[VI+4].Color:= FColor;
  Buffer.Vertices.List^[VI+4].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+4].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+4].Position.X:= Box.MinX;
  Buffer.Vertices.List^[VI+4].Position.Y:= Box.MinY;
  Buffer.Vertices.List^[VI+4].Position.Z:= Box.MaxZ;
  // X+ Y- Z+
  Buffer.Vertices.List^[VI+5].Color:= FColor;
  Buffer.Vertices.List^[VI+5].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+5].TexCoord.Y:= 0;
  Buffer.Vertices.List^[VI+5].Position.X:= Box.MaxX;
  Buffer.Vertices.List^[VI+5].Position.Y:= Box.MinY;
  Buffer.Vertices.List^[VI+5].Position.Z:= Box.MaxZ;
  // X- Y+ Z+
  Buffer.Vertices.List^[VI+6].Color:= FColor;
  Buffer.Vertices.List^[VI+6].TexCoord.X:= 1;
  Buffer.Vertices.List^[VI+6].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+6].Position.X:= Box.MinX;
  Buffer.Vertices.List^[VI+6].Position.Y:= Box.MaxY;
  Buffer.Vertices.List^[VI+6].Position.Z:= Box.MaxZ;
  // X+ Y+ Z+
  Buffer.Vertices.List^[VI+7].Color:= FColor;
  Buffer.Vertices.List^[VI+7].TexCoord.X:= 0;
  Buffer.Vertices.List^[VI+7].TexCoord.Y:= 1;
  Buffer.Vertices.List^[VI+7].Position.X:= Box.MaxZ;
  Buffer.Vertices.List^[VI+7].Position.Y:= Box.MaxY;
  Buffer.Vertices.List^[VI+7].Position.Z:= Box.MaxZ;


  II:= Buffer.Indices.Alloc(24);
  // Axis X, Y- Z+
  Buffer.Indices.List^[II+00]:= VI+0;
  Buffer.Indices.List^[II+01]:= VI+1;
  // Axis X, Y- Z-
  Buffer.Indices.List^[II+02]:= VI+4;
  Buffer.Indices.List^[II+03]:= VI+5;
  // Axis X, Y + Z+
  Buffer.Indices.List^[II+04]:= VI+2;
  Buffer.Indices.List^[II+05]:= VI+3;
  // Axis X, Y + Z-
  Buffer.Indices.List^[II+06]:= VI+6;
  Buffer.Indices.List^[II+07]:= VI+7;
  // Axis Z, X- Y-
  Buffer.Indices.List^[II+08]:= VI+0;
  Buffer.Indices.List^[II+09]:= VI+4;
  // Axis Z, X+ Y-
  Buffer.Indices.List^[II+10]:= VI+1;
  Buffer.Indices.List^[II+11]:= VI+5;
  // Axis Z, X- Y+
  Buffer.Indices.List^[II+12]:= VI+2;
  Buffer.Indices.List^[II+13]:= VI+6;
  // Axis Z, X+ Y+
  Buffer.Indices.List^[II+14]:= VI+3;
  Buffer.Indices.List^[II+15]:= VI+7;
  // Axis Y, X+ Y+
  Buffer.Indices.List^[II+16]:= VI+0;
  Buffer.Indices.List^[II+17]:= VI+2;
  // Axis Y, X+ Y+
  Buffer.Indices.List^[II+18]:= VI+1;
  Buffer.Indices.List^[II+19]:= VI+3;

  // Axis Y, X+ Y+
  Buffer.Indices.List^[II+20]:= VI+4;
  Buffer.Indices.List^[II+21]:= VI+6;
  // Axis Y, X+ Y+
  Buffer.Indices.List^[II+22]:= VI+5;
  Buffer.Indices.List^[II+23]:= VI+7;
end;


const GRID_COLOR  : TColor4f = (Red: 080/255; Green: 080/255; Blue: 080/255; Alpha: 080/255);
const GRID_MARK   : TColor4f = (Red: 160/255; Green: 160/255; Blue: 160/255; Alpha: 160/255);
const GRID_CENTER : TColor4f = (Red: 160/255; Green: 160/255; Blue: 000/255; Alpha: 160/255);

//------------------------------------------------------------------------------
procedure TPHXPrimitives.Grid(const Position: TVector3f; const Size, Interval, Marks: Integer);
var X,Y: Integer;
var SizeDiv2: Single;
begin
  //SetPrimitiveType(PHX_LINES);

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

    Line( Vector3f(Position.X + X, Position.Y, Position.Z - SizeDiv2), Vector3f(Position.X + X, Position.Y, Position.Z + SizeDiv2));
    Line( Vector3f(Position.X - X, Position.Y, Position.Z - SizeDiv2), Vector3f(Position.X - X, Position.Y, Position.Z + SizeDiv2));

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

    Line( Vector3f(Position.X -SizeDiv2, Position.Y, Position.Z + Y), Vector3f(Position.X + SizeDiv2, Position.Y, Position.Z + Y));
    Line( Vector3f(Position.X -SizeDiv2, Position.Y, Position.Z - Y), Vector3f(Position.X + SizeDiv2, Position.Y, Position.Z - Y));

    Y:= Y + Interval;
  end;

  FColor:= GRID_CENTER;
  // Draw the center lines
  Line( Vector3f(Position.X          , Position.Y, Position.Z - SizeDiv2), Vector3f(Position.X           , Position.Y, Position.Z + SizeDiv2));
  Line( Vector3f(Position.X -SizeDiv2, Position.Y, Position.Z           ), Vector3f(Position.X + SizeDiv2, Position.Y, Position.Z));

end;

//------------------------------------------------------------------------------
procedure TPHXPrimitives.GridX(const Position: TVector3f; const Size, Interval, Marks: Integer);
var X,Y: Integer;
var SizeDiv2: Single;
begin
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
end;

//------------------------------------------------------------------------------
procedure TPHXPrimitives.GridY(const Position: TVector3f; const Size, Interval, Marks: Integer);
var X,Y: Integer;
var SizeDiv2: Single;
begin
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
end;


// X = Green
// Y = Blue
// Z = Maroon
//------------------------------------------------------------------------------
procedure TPHXPrimitives.Origin(const Position: TVector3f; const Length: Single);
//var VI: Integer;
begin    (*
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
  *)
end;




end.
