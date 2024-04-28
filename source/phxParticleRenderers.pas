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
unit phxParticleRenderers;
//< Particle presety

interface

{$I phxConfig.inc}

uses SysUtils, Classes,

  phxTypes,
  phxClasses,
  phxMath,
  phxCanvas,
  phxParticle;


// Factory for the particle renderers for each particle shape
//------------------------------------------------------------------------------



// psPoint, Render the particles as points
procedure RenderPoint(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);

// psBillboard, Render the particles as billboarded quads
procedure RenderBillboard(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);

// psAlignZ, Render the particles as quads with the normal in the Z-Axis
procedure RenderAlignZ(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);

// psAlignZRotated, Render the particles as quads with the normal in the Z-Axis
procedure RenderAlignZRotated(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);

      {

// Render the particles as quads with the normal in the X-Axis
class procedure AlignX(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);
// Render the particles as quads with the normal in the Y-Axis
class procedure AlignY(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);

// Render the particles as quads with the normal in the Z-Axis
class procedure AlignZ(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);
// Render the particles as rotated quads with the normal in the Z-Axis
class procedure AlignZRotated(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);

// Render the particles as a trail with the normal in the Z-Axis, for engine trails and similar effects
class procedure TrailZ(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);

class procedure RenderTrail(Particles: TPHXParticleList; Verticies: TPHXVertexList; out Mode: TPHXPrimitiveType);   }




implementation

// Texture rectangle array
const PHXPAR_TEXTURE_CORDS: array[0..84] of TRectf =
(
  // 1x1
  (Left: 0.00; Top: 0.00; Right: 1.00; Bottom: 1.00),

  // 2x2
  (Left: 0.00; Top: 0.00; Right: 0.25; Bottom: 0.25),
  (Left: 0.25; Top: 0.00; Right: 0.50; Bottom: 0.25),
  (Left: 0.50; Top: 0.00; Right: 0.75;Bottom: 0.25),
  (Left: 0.75; Top: 0.00; Right: 1.00; Bottom: 0.25),

  // 4x4
  (Left: 0.00; Top: 0.00; Right: 0.25; Bottom: 0.25),
  (Left: 0.25; Top: 0.00; Right: 0.50; Bottom: 0.25),
  (Left: 0.50; Top: 0.00; Right: 0.75; Bottom: 0.25),
  (Left: 0.75; Top: 0.00; Right: 1.00; Bottom: 0.25),

  (Left: 0.00; Top: 0.25; Right: 0.25; Bottom: 0.50),
  (Left: 0.25; Top: 0.25; Right: 0.50; Bottom: 0.50),
  (Left: 0.50; Top: 0.25; Right: 0.75; Bottom: 0.50),
  (Left: 0.75; Top: 0.25; Right: 1.00; Bottom: 0.50),

  (Left: 0.00; Top: 0.50; Right: 0.25; Bottom: 0.75),
  (Left: 0.25; Top: 0.50; Right: 0.50; Bottom: 0.75),
  (Left: 0.50; Top: 0.50; Right: 0.75; Bottom: 0.75),
  (Left: 0.75; Top: 0.50; Right: 1.00; Bottom: 0.75),

  (Left: 0.00; Top: 0.75; Right: 0.25; Bottom: 1.00),
  (Left: 0.25; Top: 0.75; Right: 0.50; Bottom: 1.00),
  (Left: 0.50; Top: 0.75; Right: 0.75; Bottom: 1.00),
  (Left: 0.75; Top: 0.75; Right: 1.00; Bottom: 1.00),

  // 8x8
  (Left: 0.000; Top: 0.000; Right: 0.125; Bottom: 0.125),
  (Left: 0.125; Top: 0.000; Right: 0.250; Bottom: 0.125),
  (Left: 0.250; Top: 0.000; Right: 0.375; Bottom: 0.125),
  (Left: 0.375; Top: 0.000; Right: 0.500; Bottom: 0.125),
  (Left: 0.500; Top: 0.000; Right: 0.625; Bottom: 0.125),
  (Left: 0.625; Top: 0.000; Right: 0.750; Bottom: 0.125),
  (Left: 0.750; Top: 0.000; Right: 0.875; Bottom: 0.125),
  (Left: 0.875; Top: 0.000; Right: 1.000; Bottom: 0.125),

  (Left: 0.000; Top: 0.125; Right: 0.125; Bottom: 0.250),
  (Left: 0.125; Top: 0.125; Right: 0.250; Bottom: 0.250),
  (Left: 0.250; Top: 0.125; Right: 0.375; Bottom: 0.250),
  (Left: 0.375; Top: 0.125; Right: 0.500; Bottom: 0.250),
  (Left: 0.500; Top: 0.125; Right: 0.625; Bottom: 0.250),
  (Left: 0.625; Top: 0.125; Right: 0.750; Bottom: 0.250),
  (Left: 0.750; Top: 0.125; Right: 0.875; Bottom: 0.250),
  (Left: 0.875; Top: 0.125; Right: 1.000; Bottom: 0.250),

  (Left: 0.000; Top: 0.250; Right: 0.125; Bottom: 0.375),
  (Left: 0.125; Top: 0.250; Right: 0.250; Bottom: 0.375),
  (Left: 0.250; Top: 0.250; Right: 0.375; Bottom: 0.375),
  (Left: 0.375; Top: 0.250; Right: 0.500; Bottom: 0.375),
  (Left: 0.500; Top: 0.250; Right: 0.625; Bottom: 0.375),
  (Left: 0.625; Top: 0.250; Right: 0.750; Bottom: 0.375),
  (Left: 0.750; Top: 0.250; Right: 0.875; Bottom: 0.375),
  (Left: 0.875; Top: 0.250; Right: 1.000; Bottom: 0.375),

  (Left: 0.000; Top: 0.375; Right: 0.125; Bottom: 0.500),
  (Left: 0.125; Top: 0.375; Right: 0.250; Bottom: 0.500),
  (Left: 0.250; Top: 0.375; Right: 0.375; Bottom: 0.500),
  (Left: 0.375; Top: 0.375; Right: 0.500; Bottom: 0.500),
  (Left: 0.500; Top: 0.375; Right: 0.625; Bottom: 0.500),
  (Left: 0.625; Top: 0.375; Right: 0.750; Bottom: 0.500),
  (Left: 0.750; Top: 0.375; Right: 0.875; Bottom: 0.500),
  (Left: 0.875; Top: 0.375; Right: 1.000; Bottom: 0.500),


  (Left: 0.000; Top: 0.500; Right: 0.125; Bottom: 0.625),
  (Left: 0.125; Top: 0.500; Right: 0.250; Bottom: 0.625),
  (Left: 0.250; Top: 0.500; Right: 0.375; Bottom: 0.625),
  (Left: 0.375; Top: 0.500; Right: 0.500; Bottom: 0.625),
  (Left: 0.500; Top: 0.500; Right: 0.625; Bottom: 0.625),
  (Left: 0.625; Top: 0.500; Right: 0.750; Bottom: 0.625),
  (Left: 0.750; Top: 0.500; Right: 0.875; Bottom: 0.625),
  (Left: 0.875; Top: 0.500; Right: 1.000; Bottom: 0.625),

  (Left: 0.000; Top: 0.625; Right: 0.125; Bottom: 0.750),
  (Left: 0.125; Top: 0.625; Right: 0.250; Bottom: 0.750),
  (Left: 0.250; Top: 0.625; Right: 0.375; Bottom: 0.750),
  (Left: 0.375; Top: 0.625; Right: 0.500; Bottom: 0.750),
  (Left: 0.500; Top: 0.625; Right: 0.625; Bottom: 0.750),
  (Left: 0.625; Top: 0.625; Right: 0.750; Bottom: 0.750),
  (Left: 0.750; Top: 0.625; Right: 0.875; Bottom: 0.750),
  (Left: 0.875; Top: 0.625; Right: 1.000; Bottom: 0.750),

  (Left: 0.000; Top: 0.750; Right: 0.125; Bottom: 0.875),
  (Left: 0.125; Top: 0.750; Right: 0.250; Bottom: 0.875),
  (Left: 0.250; Top: 0.750; Right: 0.375; Bottom: 0.875),
  (Left: 0.375; Top: 0.750; Right: 0.500; Bottom: 0.875),
  (Left: 0.500; Top: 0.750; Right: 0.625; Bottom: 0.875),
  (Left: 0.625; Top: 0.750; Right: 0.750; Bottom: 0.875),
  (Left: 0.750; Top: 0.750; Right: 0.875; Bottom: 0.875),
  (Left: 0.875; Top: 0.750; Right: 1.000; Bottom: 0.875),

  (Left: 0.000; Top: 0.875; Right: 0.125; Bottom: 1.000),
  (Left: 0.125; Top: 0.875; Right: 0.250; Bottom: 1.000),
  (Left: 0.250; Top: 0.875; Right: 0.375; Bottom: 1.000),
  (Left: 0.375; Top: 0.875; Right: 0.500; Bottom: 1.000),
  (Left: 0.500; Top: 0.875; Right: 0.625; Bottom: 1.000),
  (Left: 0.625; Top: 0.875; Right: 0.750; Bottom: 1.000),
  (Left: 0.750; Top: 0.875; Right: 0.875; Bottom: 1.000),
  (Left: 0.875; Top: 0.875; Right: 1.000; Bottom: 1.000)
);



// psPoint, Render the particles as points
//------------------------------------------------------------------------------
procedure RenderPoint(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);
var Index   : Integer;
var VI      : Integer;
var II      : Integer;
var Particle: PPHXParticle;
begin
  Canvas.Primitive:= PHX_POINTS;

  // We need one vertex per particle
  VI:= Canvas.Vertices.Alloc( Particles.Count );
  // We need one index per particle
  II:= Canvas.Indices.Alloc( Particles.Count );

  for Index:=0 to Particles.Count - 1 do
  begin
    Particle:= @Particles.List[Index];

    // Top-Left vertex
    Canvas.Vertices.List^[VI].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Left;
    Canvas.Vertices.List^[VI].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Top;

    Canvas.Vertices.List^[VI].Position.X:= Particle^.Position.X;
    Canvas.Vertices.List^[VI].Position.Y:= Particle^.Position.Y;
    Canvas.Vertices.List^[VI].Position.Z:= Particle^.Position.Z;

    Canvas.Vertices.List^[VI].Normal:= Vector3f_AxisZ;
    Canvas.Vertices.List^[VI].Color := Particle^.Color;

    // Add indicies
    Canvas.Indices.List^[II]:= VI;

    Inc(VI);
    Inc(II);
  end;
end;


//------------------------------------------------------------------------------
procedure RenderBillboard(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);
begin

end;

//------------------------------------------------------------------------------
procedure RenderAlignZ(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);
var Index   : Integer;
var VI      : Integer;
var II      : Integer;
var Particle: PPHXParticle;
var Size   : Single;
begin
  Canvas.Primitive:= PHX_TRIANGLES;

  // We need four vertex per particle
  VI:= Canvas.Vertices.Alloc( Particles.Count * 4);
  // We need two triangles per particle
  II:= Canvas.Indices.Alloc( Particles.Count * 6);

  for Index:=0 to Particles.Count - 1 do
  begin
    Particle:= @Particles.List[Index];

    Size:= Particle^.Size * 0.5;

    // Top-Left vertex
    Canvas.Vertices.List^[VI+0].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+0].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+0].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Left;
    Canvas.Vertices.List^[VI+0].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Top;

    Canvas.Vertices.List^[VI+0].Position.X:= Particle^.Position.X - Size;
    Canvas.Vertices.List^[VI+0].Position.Y:= Particle^.Position.Y - Size;
    Canvas.Vertices.List^[VI+0].Position.Z:= Particle^.Position.Z;

    // Top-Right vertex
    Canvas.Vertices.List^[VI+1].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+1].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+1].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Right;
    Canvas.Vertices.List^[VI+1].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Top;

    Canvas.Vertices.List^[VI+1].Position.X:= Particle^.Position.X + Size;
    Canvas.Vertices.List^[VI+1].Position.Y:= Particle^.Position.Y - Size;
    Canvas.Vertices.List^[VI+1].Position.Z:= Particle^.Position.Z;

    // Bottom-Right vertex
    Canvas.Vertices.List^[VI+2].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+2].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+2].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Right;
    Canvas.Vertices.List^[VI+2].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Bottom;

    Canvas.Vertices.List^[VI+2].Position.X:= Particle^.Position.X + Size;
    Canvas.Vertices.List^[VI+2].Position.Y:= Particle^.Position.Y + Size;
    Canvas.Vertices.List^[VI+2].Position.Z:= Particle^.Position.Z;

    // Bottom-Left vertex
    Canvas.Vertices.List^[VI+3].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+3].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+3].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].left;
    Canvas.Vertices.List^[VI+3].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Bottom;

    Canvas.Vertices.List^[VI+3].Position.X:= Particle^.Position.X - Size;
    Canvas.Vertices.List^[VI+3].Position.Y:= Particle^.Position.Y + Size;
    Canvas.Vertices.List^[VI+3].Position.Z:= Particle^.Position.Z;

    // Triangle #1
    Canvas.Indices.List^[II+0]:= VI+0;
    Canvas.Indices.List^[II+1]:= VI+1;
    Canvas.Indices.List^[II+2]:= VI+2;
    // Triangle #2
    Canvas.Indices.List^[II+3]:= VI+2;
    Canvas.Indices.List^[II+4]:= VI+3;
    Canvas.Indices.List^[II+5]:= VI+0;

    Inc(VI, 4);
    Inc(II, 6);
  end;
end;

//------------------------------------------------------------------------------
procedure RenderAlignZRotated(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);
var Index   : Integer;
var VI      : Integer;
var II      : Integer;
var Particle: PPHXParticle;
var Size    : Single;
var Matrix  : TMatrix4f;
var Points  : array[0..3] of TVector3f;
begin
  Canvas.Primitive:= PHX_TRIANGLES;

  // We need four vertex per particle
  VI:= Canvas.Vertices.Alloc( Particles.Count * 4);
  // We need two triangles per particle
  II:= Canvas.Indices.Alloc( Particles.Count * 6);

  for Index:=0 to Particles.Count - 1 do
  begin
    Particle:= @Particles.List[Index];

    Size:= Particle^.Size * 0.5;
    Matrix:= Matrix_CreateRotationZ(Particle^.Angle);

    Points[0].X:=  - Size;
    Points[0].Y:=  - Size;
    Points[1].X:=  + Size;
    Points[1].Y:=  - Size;
    Points[2].X:=  + Size;
    Points[2].Y:=  + Size;
    Points[3].X:=  - Size;
    Points[3].Y:=  + Size;

    // Transform the verticies
    Points[0]:= Matrix_Transform(Matrix, Points[0]);
    Points[1]:= Matrix_Transform(Matrix, Points[1]);
    Points[2]:= Matrix_Transform(Matrix, Points[2]);
    Points[3]:= Matrix_Transform(Matrix, Points[3]);

    // Top-Left vertex
    Canvas.Vertices.List^[VI+0].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+0].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+0].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Left;
    Canvas.Vertices.List^[VI+0].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Top;

    Canvas.Vertices.List^[VI+0].Position.X:= Particle^.Position.X + Points[0].X;
    Canvas.Vertices.List^[VI+0].Position.Y:= Particle^.Position.Y + Points[0].Y;
    Canvas.Vertices.List^[VI+0].Position.Z:= Particle^.Position.Z;

    // Top-Right vertex
    Canvas.Vertices.List^[VI+1].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+1].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+1].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Right;
    Canvas.Vertices.List^[VI+1].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Top;

    Canvas.Vertices.List^[VI+1].Position.X:= Particle^.Position.X + Points[1].X;
    Canvas.Vertices.List^[VI+1].Position.Y:= Particle^.Position.Y + Points[1].Y;
    Canvas.Vertices.List^[VI+1].Position.Z:= Particle^.Position.Z;

    // Bottom-Right vertex
    Canvas.Vertices.List^[VI+2].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+2].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+2].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Right;
    Canvas.Vertices.List^[VI+2].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Bottom;

    Canvas.Vertices.List^[VI+2].Position.X:= Particle^.Position.X + Points[2].X;
    Canvas.Vertices.List^[VI+2].Position.Y:= Particle^.Position.Y + Points[2].Y;
    Canvas.Vertices.List^[VI+2].Position.Z:= Particle^.Position.Z;

    // Bottom-Left vertex
    Canvas.Vertices.List^[VI+3].Color     := Particle^.Color;
    Canvas.Vertices.List^[VI+3].Normal    := Vector3f_AxisZ;
    Canvas.Vertices.List^[VI+3].TexCoord.X:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].left;
    Canvas.Vertices.List^[VI+3].TexCoord.Y:= PHXPAR_TEXTURE_CORDS[ Particle^.Pattern ].Bottom;

    Canvas.Vertices.List^[VI+3].Position.X:= Particle^.Position.X + Points[3].X;
    Canvas.Vertices.List^[VI+3].Position.Y:= Particle^.Position.Y + Points[3].Y;
    Canvas.Vertices.List^[VI+3].Position.Z:= Particle^.Position.Z;

    // Triangle #1
    Canvas.Indices.List^[II+0]:= VI+0;
    Canvas.Indices.List^[II+1]:= VI+1;
    Canvas.Indices.List^[II+2]:= VI+2;
    // Triangle #2
    Canvas.Indices.List^[II+3]:= VI+2;
    Canvas.Indices.List^[II+4]:= VI+3;
    Canvas.Indices.List^[II+5]:= VI+0;

    Inc(VI, 4);
    Inc(II, 6);
  end;
end;


{$ENDREGION}


initialization
  RegisterParticleRenderer(psPoint    , RenderPoint);
  RegisterParticleRenderer(psBillboard, RenderBillboard);

  RegisterParticleRenderer(psAlignZ       , RenderAlignZ);
  RegisterParticleRenderer(psAlignZRotated, RenderAlignZRotated);
end.
