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
unit phxParticleAffectors;
//< Default particle affectors

interface

{$I phxConfig.inc}

uses SysUtils, Classes,

  phxTypes,
  phxClasses,
  phxMath,
  phxParticle;

type

// Attracts particles towards a point
//------------------------------------------------------------------------------
TPHXParticleAttractor = class(TPHXParticleAffector)
  private
    FPosition: TVector3f;
    FForce   : Single;
    FRelativeToSystem: Boolean;
  protected
    function GetName: string; override;

    procedure LoadAffector(Reader: TPHXReader); override;
    procedure SaveAffector(Writer: TPHXWriter); override;
  public
    constructor Create(AEffect: TPHXParticleEffect); override;

    // Affect all the particles in the particle system
    procedure Update(System: TPHXParticleSystem; DeltaTime: Double); override;

    // Center position of the atttractor
    property Position: TVector3f read FPosition write FPosition;
  published
    // Center position of the atttractor
    property PositionX: Single read FPosition.X write FPosition.X;
    // Center position of the atttractor
    property PositionY: Single read FPosition.Y write FPosition.Y;
    // Center position of the atttractor
    property PositionZ: Single read FPosition.Z write FPosition.Z;

    // The force to affect the particles with
    property Force : Single    read FForce    write FForce;
    // If the attractor position should be relative to the system position
    property RelativeToSystem: Boolean read FRelativeToSystem write FRelativeToSystem;
  end;

// Kills particles when they exits a box that is centered around the particle
// system position
//------------------------------------------------------------------------------
TPHXParticleCuller = class(TPHXParticleAffector)
  private
    FBounds: TBoxf;
    FRelativeToSystem: Boolean;
  protected
    function GetName: string; override;

    procedure LoadAffector(Reader: TPHXReader); override;
    procedure SaveAffector(Writer: TPHXWriter); override;
  public
    constructor Create(AEffect: TPHXParticleEffect); override;

    // Affect all the particles in the particle system
    procedure Update(System: TPHXParticleSystem; DeltaTime: Double); override;

    Property Bounds: TBoxf read FBounds write FBounds;
  published
    Property MinX: Single read FBounds.MinX write FBounds.MinX;
    Property MaxX: Single read FBounds.MaxX write FBounds.MaxX;
    Property MinY: Single read FBounds.MinY write FBounds.MinY;
    Property MaxY: Single read FBounds.MaxY write FBounds.MaxY;
    Property MinZ: Single read FBounds.MinZ write FBounds.MinZ;
    Property MaxZ: Single read FBounds.MaxZ write FBounds.MaxZ;
    // If the attractor position should be relative to the system position
    property RelativeToSystem: Boolean read FRelativeToSystem write FRelativeToSystem;
  end;

implementation

{$REGION 'TPHXParticleAttractor'}

// TPHXParticleAttractor
//==============================================================================
constructor TPHXParticleAttractor.Create(AEffect: TPHXParticleEffect);
begin
  inherited Create(AEffect);

  FPosition        := Vector3f_Zero;
  FRelativeToSystem:= True;
  FForce           := 100.0;
end;

//------------------------------------------------------------------------------
function TPHXParticleAttractor.GetName: string;
begin
  Result:= 'Attractor';
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAttractor.Update(System: TPHXParticleSystem; DeltaTime: Double);
var Index       : Integer;
var Particle    : PPHXParticle;
var Position    : TVector3f;
var Delta       : TVector3f;
var Distance    : Single;
var Acceleration: Single;
begin
  for Index:=0 to System.Particles.Count-1 do
  begin
    Particle:= @System.Particles.List[Index];

    if RelativeToSystem then
    begin
      Position.X:= (System.Position.X + Self.Position.X);
      Position.Y:= (System.Position.Y + Self.Position.Y);
      Position.Z:= (System.Position.Z + Self.Position.Z);
    end else
    begin
      Position.X:= Self.Position.X;
      Position.Y:= Self.Position.Y;
      Position.Z:= Self.Position.Z;
    end;
    Delta.X:= Position.X - Particle^.Position.X;
    Delta.Y:= Position.Y - Particle^.Position.Y;
    Delta.Z:= Position.Z - Particle^.Position.Z;

    Distance:= Sqrt( Sqr(Delta.X) + Sqr(Delta.Y) + Sqr(Delta.Z));

    if Distance > 1 then
    begin
      Acceleration:= Force / Distance;

      Delta.X:= Delta.X / Distance;
      Delta.Y:= Delta.Y / Distance;
      Delta.Z:= Delta.Z / Distance;
    end else
    begin
      Acceleration:= 0;

      Delta.X:= 0;
      Delta.Y:= 0;
      Delta.Z:= 0;
    end;

    Particle^.Velocity.X:= Particle^.Velocity.X + Delta.X * Acceleration * DeltaTime;
    Particle^.Velocity.Y:= Particle^.Velocity.Y + Delta.Y * Acceleration * DeltaTime;
    Particle^.Velocity.Z:= Particle^.Velocity.Z + Delta.Z * Acceleration * DeltaTime;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXParticleAttractor.LoadAffector(Reader: TPHXReader);
begin
  Reader.Read(FPosition        , SizeOf(FPosition));
  Reader.Read(FForce           , SizeOf(FForce));
  Reader.Read(FRelativeToSystem, SizeOf(FRelativeToSystem));
end;


//------------------------------------------------------------------------------
procedure TPHXParticleAttractor.SaveAffector(Writer: TPHXWriter);
begin
  Writer.Write(FPosition        , SizeOf(FPosition));
  Writer.Write(FForce           , SizeOf(FForce));
  Writer.Write(FRelativeToSystem, SizeOf(FRelativeToSystem));
end;

{$ENDREGION}

{$REGION 'TPHXParticleCuller'}

// TPHXParticleCuller
//==============================================================================
constructor TPHXParticleCuller.Create(AEffect: TPHXParticleEffect);
begin
  inherited Create(AEffect);

  FBounds.MinX:= -10000;
  FBounds.MinY:= -10000;
  FBounds.MinZ:= -10000;

  FBounds.MaxX:= 10000;
  FBounds.MaxY:= 10000;
  FBounds.MaxZ:= 10000;
end;

//------------------------------------------------------------------------------
function TPHXParticleCuller.GetName: string;
begin
  Result:= 'Culler';
end;

//------------------------------------------------------------------------------
procedure TPHXParticleCuller.Update(System: TPHXParticleSystem; DeltaTime: Double);
var Index   : Integer;
var Particle: PPHXParticle;
var Position: TVector3f;
begin
  for Index:=0 to System.Particles.Count-1 do
  begin
    Particle:= @System.Particles.List[Index];

    if RelativeToSystem then
    begin
      Position.X:= (System.Position.X + Particle^.Position.X);
      Position.Y:= (System.Position.Y + Particle^.Position.Y);
      Position.Z:= (System.Position.Z + Particle^.Position.Z);
    end else
    begin
      Position.X:= Particle^.Position.X;
      Position.Y:= Particle^.Position.Y;
      Position.Z:= Particle^.Position.Z;
    end;
    //Position.X:= System.Position.X - Particle^.Position.X;
    //Position.Y:= System.Position.Y - Particle^.Position.Y;
   // Position.Z:= System.Position.Z - Particle^.Position.Z;

    if (Position.X < Bounds.MinX) or
       (Position.X > Bounds.MaxX) or
       (Position.Y < Bounds.MinY) or
       (Position.Y > Bounds.MaxY) or
       (Position.Z < Bounds.MinZ) or
       (Position.Z > Bounds.MaxZ) then
    begin
      Particle^.Time:= Particle^.Life;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleCuller.LoadAffector(Reader: TPHXReader);
begin
  Reader.Read(FBounds, SizeOf(FBounds));
end;

//------------------------------------------------------------------------------
procedure TPHXParticleCuller.SaveAffector(Writer: TPHXWriter);
begin
  Writer.Write(FBounds, SizeOf(FBounds));
end;

{$ENDREGION}


initialization
  RegisterParticleAffector('Attractor', TPHXParticleAttractor);
  RegisterParticleAffector('Culler'   , TPHXParticleCuller);
end.
