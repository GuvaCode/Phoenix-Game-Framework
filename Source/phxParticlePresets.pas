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
unit phxParticlePresets;
//< Preset particle effects

interface

{$I phxConfig.inc}

uses SysUtils, Classes,

  phxTypes,
  phxMath,

  phxParticle,
  phxParticleAffectors;

type

//------------------------------------------------------------------------------
TPHXParticlePresets = class
  public
    class function Fire(AManager: TPHXParticleManager): TPHXParticleEffect;
    class function Jumpgate(AManager: TPHXParticleManager): TPHXParticleEffect;
    //class function Plasma: TPHXParticleEffect;
          {

    class function Rocket: TPHXParticleEffect;

    class function Test: TPHXParticleEffect;  }
  end;

implementation


(*


    // Name of the particle effect
    FName: String;
    // Maximum number of particles
    FQuota: Integer;
    // Duration of the particle system, zero is infinite
    FDuration: Single;
    // Delay in seconds before activating the system
    FActivationDelay: Single;
    // Parent particle effect
    FParent: TPHXParticleEffect;
    // Name of the parent effect
    FParentName: String;


    // If the particles should be moved with the owning particle system
    FLinkToSystem: Boolean;
    // If the particle system should be idle until the parent system is activated
    FIdleWhileParentActive: Boolean;

    // List of affectors for the particle system
    FAffectors: TPHXParticleAffectorList;

    //-- Graphs

    // Graph for the scale
    FGraphScale: TPHXGraph1f;
    // Graph for the absolute color
    FGraphColor: TPHXGraphCf;
    // Graph for the absolute visibility
    FGraphAlpha: TPHXGraph1f;
    // Graph for pattern index
    FGraphPattern: TPHXGraph1i;
    // Graph for the particle velocity factor
    FGraphVelocity: TPHXGraph3f;
    // Graph for the particle spin factor
    FGraphSpin: TPHXGraph1f;

    //--- Emission parameters

    // The emission mode, distance or time
    FEmissionMode         : TPHXEmissionMode;
    // Delay between each emission in seconds or units
    FEmissionDelay        : Single;
    // Number of particles to emit
    FEmissionCount        : Integer;
    // Repeat count of the effect, zero is infinite
    FEmissionRepeats      : Word;
    // The points to emit from
    FEmissionPoints       : TPHXVectorList3f;
    // Use the poins in order instead of randomly
    FEmissionPointsInOrder: Boolean;

    // Life interval for each particle, in seconds
    FLife: TPHXParticleParameter1f;

    //--- Apperance parameters

    // The texture of the particles
    FTexture     : TPHXTexture2D;
    // Name of the texture, for saving
    FTextureName : String;
    // The shape of each particle
    FShape       : TPHXParticleShape;
    // The blending of each particle
    FBlending    : TPHXParticleBlending;
    // Size of the patterns in the texture (1x1, 2x2, 4x4 or 8x8)
    FPatternSize : TPHXParticlePatternSize;
    // Min and max pattern index (0 <= Value <= GetParticlePatternCount(PatternSize) )
    FPatternIndex: TPHXParticleParameter1w;

    // Start color of the particles
    FColor: TPHXParticleParameterCf;
    // The color is grayscale
    FColorAsGrayscale: Boolean;

    // Size interval for each particle
    FSize: TPHXParticleParameter1f;
    // Growth interval for each particle
    FGrowth: TPHXParticleParameter1f;

    //--- Physics parameters

    // Initial direction
    FDirection: TVector3f;
    // Spread around the direction vector
    FSpread: TPHXParticleParameter1f;

    // Velocity per particle in units/sec, in the direction of the particle
    FVelocity: TPHXParticleParameter1f;
    // Spin per particle in degrees/sec (angular velocity)
    FSpin: TPHXParticleParameter1f;
    // Acceleration in world coordinates
    FAcceleration: TVector3f;

    *)

// TPHXParticlePresets
//------------------------------------------------------------------------------
class function TPHXParticlePresets.Fire(AManager: TPHXParticleManager): TPHXParticleEffect;
begin
  Result:= TPHXParticleEffect.Create(AManager);
  Result.Name:= 'Fire';

  Result.Quota    := 60;
  Result.Duration := 0;
  Result.Delay    := 0;

  Result.InitalUpdateCount   := 100;
  Result.InitalUpdateInterval:= 0.50;

  Result.PatternSize:=  ps4x4;
  Result.PatternIndex:= 8;
  Result.PatternVariance:= 0;

  Result.TextureName:= 'particles.png';


  //--- Emission parameters

    // The emission mode, distance or time
  Result.Emittor.Mode         := emTime;
    // Delay between each emission in seconds or units
  Result.Emittor.Delay        := 0.100;
    // Number of particles to emit
  Result.Emittor.Count        := 2;
    // Repeat count of the effect, zero is infinite
  Result.Emittor.Repeats      := 0;
    // The points to emit from
  Result.Emittor.CreateLine(100, Vector3f(-15,0,0), Vector3f(15, 0, 0));
    // Use the poins in order instead of randomly
  Result.Emittor.PointsInOrder:= False;

  // Life interval for each particle, in seconds
  Result.LifeValue   := 2;
  Result.LifeVariance:= 2;

    //--- Apperance parameters
     (*
  // The texture of the particles
  FTexture     : TPHXTexture2D;
  // Name of the texture, for saving
  FTextureName : String;
  *)
  // The shape of each particle
  Result.Shape       := psAlignZRotated;
  // The blending of each particle
  Result.Blending    := bmAdd;
  (*
  // Size of the patterns in the texture (1x1, 2x2, 4x4 or 8x8)
  FPatternSize : TPHXParticlePatternSize;
  // Min and max pattern index (0 <= Value <= GetParticlePatternCount(PatternSize) )
  FPatternIndex: TPHXParticleParameter1w;
  *)
  // Start color of the particles
  Result.ColorValue   := Color4f(0.6, 0.6, 0.6, 1.0);
  Result.ColorVariance:= Color4f(0.4, 0.4, 0.4, 1.0);
  // The color is grayscale
  Result.Grayscale:= True;

  // Size interval for each particle
  Result.SizeMin:= 24;
  Result.SizeMax:= 32;
  // Growth interval for each particle
  Result.GrowthMin:= -10;
  Result.GrowthMax:= -5;


  //--- Physics parameters

  // Initial direction
  Result.Direction:= VectorNegate(Vector3f_AxisY);
  // Spread around the direction vector
  Result.Spread:= 0;

  // Velocity per particle in units/sec, in the direction of the particle
  Result.VelocityMin:= 25;
  Result.VelocityMax:= 25;
  // Spin per particle in degrees/sec (angular velocity)
  Result.Spin:= - 45;
  Result.SpinVariance:=   45;

    // Acceleration in world coordinates
  Result.Acceleration:= Vector3f_Zero;

  //-- Graphs
  Result.Graphs.Alpha.Keys.Clear;
  Result.Graphs.Alpha.Keys.Add(0.00, 0.0);
  Result.Graphs.Alpha.Keys.Add(0.25, 1.0);
  Result.Graphs.Alpha.Keys.Add(1.00, 0.0);

  Result.Graphs.Color.Keys.Clear;
  Result.Graphs.Color.Keys.Add(0.00, clrRed);
  Result.Graphs.Color.Keys.Add(0.35, clrYellow);

  Result.Initialize;
end;

//------------------------------------------------------------------------------
class function TPHXParticlePresets.Jumpgate(AManager: TPHXParticleManager): TPHXParticleEffect;
var Jumpgate: TPHXParticleEffect;
begin
  Jumpgate:= TPHXParticleEffect.Create(AManager);

  //--- Basic parameters
  Jumpgate.Name           := 'Jumpgate';
  Jumpgate.Quota          := 100;
  Jumpgate.Duration       := 0;
  Jumpgate.Delay          := 0;

  with Jumpgate.Affectors.Add(TPHXParticleAttractor.Create(Jumpgate)) as TPHXParticleAttractor do
  begin
    Force:= 500;
  end;

  Jumpgate.InitalUpdateCount   := 10;
  Jumpgate.InitalUpdateInterval:= 0.50;

  //--- Graphs
  Jumpgate.Graphs.Scale.Keys.Clear;
  Jumpgate.Graphs.Scale.Keys.Add(0.000, 1.000);

  Jumpgate.Graphs.Color.Keys.Clear;
  Jumpgate.Graphs.Color.Keys.Add(0.000, clrWhite);

  Jumpgate.Graphs.Alpha.Keys.Clear;
  Jumpgate.Graphs.Alpha.Keys.Add(0.000, 0.000);
  Jumpgate.Graphs.Alpha.Keys.Add(0.100, 1.000);
  Jumpgate.Graphs.Alpha.Keys.Add(1.000, 0.000);

  Jumpgate.Graphs.EmissionCount.Keys.Clear;
  Jumpgate.Graphs.EmissionCount.Keys.Add(0.000, 1);

  Jumpgate.Graphs.Velocity.Keys.Clear;
  Jumpgate.Graphs.Velocity.Keys.Add(0.000, Vector3f(1.0000, 1.0000, 1.0000));

  Jumpgate.Graphs.Spin.Keys.Clear;
  Jumpgate.Graphs.Spin.Keys.Add(0.000, 1.000);


  //--- Emission parameters
  Jumpgate.Emittor.Mode   := emTime;
  Jumpgate.Emittor.Delay  := 0.100;
  Jumpgate.Emittor.Count  := 4;
  Jumpgate.Emittor.Repeats:= 0;

  Jumpgate.Emittor.Points.Clear;
  Jumpgate.Emittor.Points.Capacity:=100;
  Jumpgate.Emittor.Points.Add(Vector3f(50.0000, 0.0000, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.9013, 3.1395, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.6057, 6.2667, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.1144, 9.3691, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(48.4292, 12.4345, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(47.5528, 15.4509, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(46.4888, 18.4062, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(45.2414, 21.2890, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(43.8153, 24.0877, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(42.2164, 26.7913, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(40.4508, 29.3893, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(38.5257, 31.8712, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(36.4484, 34.2274, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(34.2273, 36.4484, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(31.8712, 38.5257, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(29.3893, 40.4509, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(26.7913, 42.2164, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(24.0877, 43.8153, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(21.2890, 45.2414, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(18.4062, 46.4888, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(15.4508, 47.5528, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(12.4345, 48.4292, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(9.3691, 49.1144, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(6.2666, 49.6057, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(3.1395, 49.9013, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(0.0000, 50.0000, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-3.1395, 49.9013, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-6.2667, 49.6057, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-9.3691, 49.1144, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-12.4345, 48.4292, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-15.4509, 47.5528, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-18.4063, 46.4888, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-21.2890, 45.2413, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-24.0877, 43.8153, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-26.7914, 42.2164, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-29.3893, 40.4508, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-31.8712, 38.5256, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-34.2274, 36.4484, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-36.4485, 34.2273, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-38.5257, 31.8712, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-40.4509, 29.3892, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-42.2164, 26.7913, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-43.8154, 24.0876, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-45.2414, 21.2889, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-46.4888, 18.4062, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-47.5528, 15.4508, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-48.4292, 12.4344, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.1144, 9.3690, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.6057, 6.2666, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.9013, 3.1395, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-50.0000, -0.0001, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.9013, -3.1396, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.6057, -6.2667, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-49.1144, -9.3691, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-48.4291, -12.4345, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-47.5528, -15.4509, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-46.4888, -18.4063, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-45.2413, -21.2890, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-43.8153, -24.0877, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-42.2164, -26.7914, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-40.4508, -29.3893, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-38.5256, -31.8713, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-36.4484, -34.2274, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-34.2273, -36.4485, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-31.8712, -38.5257, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-29.3892, -40.4509, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-26.7913, -42.2164, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-24.0876, -43.8154, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-21.2889, -45.2414, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-18.4062, -46.4888, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-15.4508, -47.5528, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-12.4344, -48.4292, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-9.3690, -49.1144, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-6.2666, -49.6057, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(-3.1395, -49.9013, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(0.0001, -50.0000, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(3.1396, -49.9013, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(6.2667, -49.6057, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(9.3691, -49.1143, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(12.4346, -48.4291, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(15.4509, -47.5528, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(18.4063, -46.4888, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(21.2890, -45.2413, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(24.0878, -43.8153, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(26.7914, -42.2164, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(29.3893, -40.4508, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(31.8713, -38.5256, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(34.2274, -36.4484, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(36.4485, -34.2273, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(38.5257, -31.8711, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(40.4509, -29.3892, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(42.2164, -26.7913, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(43.8154, -24.0876, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(45.2414, -21.2889, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(46.4889, -18.4061, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(47.5529, -15.4508, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(48.4292, -12.4344, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.1144, -9.3690, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.6057, -6.2666, 0.0000));
  Jumpgate.Emittor.Points.Add(Vector3f(49.9013, -3.1394, 0.0000));
  Jumpgate.Emittor.PointsInOrder:= False;
  Jumpgate.LifeValue:= 4;
  Jumpgate.LifeVariance:= 1;

  //--- Apperance parameters
  Jumpgate.TextureName      := 'particles.png';
  Jumpgate.Shape            := psAlignZ;
  Jumpgate.Blending         := bmAdd;
  Jumpgate.PatternSize      := ps4x4;
  Jumpgate.PatternIndex     := 9;
  Jumpgate.PatternVariance  := 0;
  Jumpgate.ColorValue       := Color4f(0.4902, 0.4902, 1.0000, 1.0000);
  Jumpgate.ColorVariance    := Color4f(0.0000, 0.0000, 1.0000, 1.0000);
  Jumpgate.Grayscale        := False;
  Jumpgate.SizeMin          := 16;
  Jumpgate.SizeMax          := 16;
  Jumpgate.GrowthMin        := 0;
  Jumpgate.GrowthMax        := 0;

  //--- Physics parameters
  Jumpgate.Direction        := Vector3f(1.0000, 0.0000, 0.0000);
  Jumpgate.Spread           := 180;
  Jumpgate.VelocityMin      := 0;
  Jumpgate.VelocityMax      := 0;
  Jumpgate.Spin             := 0;
  Jumpgate.SpinVariance     := 0;
  Jumpgate.Acceleration     := Vector3f(0.0000, 0.0000, 0.0000);

  Jumpgate.Initialize;

  Result:= Jumpgate;
end;

      (*
//------------------------------------------------------------------------------
class function TPHXParticlePresets.Plasma: TPHXParticleEffect;
var Plasma: TPHXParticleEffect;
begin

  Plasma:= TPHXParticleEffect.Create;

  //--- Basic parameters
  Plasma.Name           := 'Plasma';
  Plasma.Quota          := 100;
  Plasma.Duration       := 0;
  Plasma.ActivationDelay:= 0;


  //--- Graphs
  Plasma.Graphs.Scale.Keys.Clear;
  Plasma.Graphs.Scale.Keys.Add(0.000, 1.000);

  Plasma.Graphs.Color.Keys.Clear;
  Plasma.Graphs.Color.Keys.Add(0.000, Color4f(0.5569, 0.5569, 1.0000, 0.0000));
  Plasma.Graphs.Color.Keys.Add(1.000, clrNone);

  Plasma.Graphs.Alpha.Keys.Clear;
  Plasma.Graphs.Alpha.Keys.Add(0.000, 1.000);
  Plasma.Graphs.Alpha.Keys.Add(1.000, 0.000);

  Plasma.Graphs.EmissionCount.Keys.Clear;
  Plasma.Graphs.EmissionCount.Keys.Add(0.000, 1);

  Plasma.Graphs.Velocity.Keys.Clear;
  Plasma.Graphs.Velocity.Keys.Add(0.000, Vector3f(1.0000, 1.0000, 1.0000));

  Plasma.Graphs.Spin.Keys.Clear;
  Plasma.Graphs.Spin.Keys.Add(0.000, 1.000);


  //--- Emission parameters
  Plasma.EmissionMode   := emTime;
  Plasma.EmissionDelay  := 0.100;
  Plasma.EmissionCount  := 4;
  Plasma.EmissionRepeats:= 0;

  Plasma.EmissionPoints.Clear;
  Plasma.EmissionPoints.Capacity:=0;
  Plasma.EmissionPointsInOrder:= False;
  Plasma.LifeMin:= 1;
  Plasma.LifeMax:= 1;

  //--- Apperance parameters
  Plasma.TextureName      := 'particles.png';
  Plasma.Shape            := psAlignZ;
  Plasma.Blending         := bmAdd;
  Plasma.PatternSize      := ps4x4;
  Plasma.PatternIndexMin  := 11;
  Plasma.PatternIndexMax  := 11;
  Plasma.ColorMin         := clrWhite;
  Plasma.ColorMax         := clrWhite;
  Plasma.ColorAsGrayscale := False;
  Plasma.SizeMin          := 32;
  Plasma.SizeMax          := 32;
  Plasma.GrowthMin        := 0;
  Plasma.GrowthMax        := 0;

  //--- Physics parameters
  Plasma.Direction        := Vector3f(1.0000, 0.0000, 0.0000);
  Plasma.SpreadMin        := 180;
  Plasma.SpreadMax        := 180;
  Plasma.VelocityMin      := 30;
  Plasma.VelocityMax      := 30;
  Plasma.SpinMin          := 0;
  Plasma.SpinMax          := 0;
  Plasma.Acceleration     := Vector3f(0.0000, 0.0000, 0.0000);

  Plasma.Initialize;
  Result:= Plasma;
end;

//------------------------------------------------------------------------------
class function TPHXParticlePresets.Rocket: TPHXParticleEffect;
var Smoke: TPHXParticleEffect;
begin
  Smoke:= TPHXParticleEffect.Create;

  //--- Basic parameters
  Smoke.Name           := 'Smoke';
  Smoke.Quota          := 100;
  Smoke.Duration       := 0;
  Smoke.ActivationDelay:= 0;


  //--- Graphs
  Smoke.Graphs.Scale.Keys.Clear;
  Smoke.Graphs.Scale.Keys.Add(0.000, 1.000);
  Smoke.Graphs.Scale.Keys.Add(0.010, 1.500);

  Smoke.Graphs.Color.Keys.Clear;
  Smoke.Graphs.Color.Keys.Add(0.000, clrWhite);

  Smoke.Graphs.Alpha.Keys.Clear;
  Smoke.Graphs.Alpha.Keys.Add(0.000, 1.000);
  Smoke.Graphs.Alpha.Keys.Add(1.000, 0.000);

  Smoke.Graphs.EmissionCount.Keys.Clear;
  Smoke.Graphs.EmissionCount.Keys.Add(0.000, 1);

  Smoke.Graphs.Velocity.Keys.Clear;
  Smoke.Graphs.Velocity.Keys.Add(0.000, Vector3f(1.0000, 1.0000, 1.0000));

  Smoke.Graphs.Spin.Keys.Clear;
  Smoke.Graphs.Spin.Keys.Add(0.000, 1.000);


  //--- Emission parameters
  Smoke.EmissionMode   := emDistance;
  Smoke.EmissionDelay  := 8.000;
  Smoke.EmissionCount  := 1;
  Smoke.EmissionRepeats:= 0;

  Smoke.EmissionPoints.Clear;
  Smoke.EmissionPoints.Capacity:=0;
  Smoke.EmissionPointsInOrder:= False;
  Smoke.LifeMin:= 0.5;
  Smoke.LifeMax:= 1;

  //--- Apperance parameters
  Smoke.TextureName      := 'smoke_particle.png';
  Smoke.Shape            := psAlignZRotated;
  Smoke.Blending         := bmAlpha;
  Smoke.PatternSize      := ps1x1;
  Smoke.PatternIndexMin  := 0;
  Smoke.PatternIndexMax  := 0;
  Smoke.ColorMin         := clrWhite;
  Smoke.ColorMax         := clrWhite;
  Smoke.ColorAsGrayscale := False;
  Smoke.SizeMin          := 16;
  Smoke.SizeMax          := 8;
  Smoke.GrowthMin        := 16;
  Smoke.GrowthMax        := 16;

  //--- Physics parameters
  Smoke.Direction        := Vector3f(1.0000, 0.0000, 0.0000);
  Smoke.SpreadMin        := 180;
  Smoke.SpreadMax        := 180;
  Smoke.VelocityMin      := 0;
  Smoke.VelocityMax      := 0;
  Smoke.SpinMin          := 90;
  Smoke.SpinMax          := 90;
  Smoke.Acceleration     := Vector3f(0.0000, 0.0000, 0.0000);

  Smoke.Initialize;


 Result:= Smoke;
end;

//------------------------------------------------------------------------------
class function TPHXParticlePresets.Test: TPHXParticleEffect;
var Jumpgate: TPHXParticleEffect;
begin
  Jumpgate:= TPHXParticleEffect.Create;

  //--- Basic parameters
  Jumpgate.Name                := 'Jumpgate';
  Jumpgate.Quota               := 100;
  Jumpgate.Duration            := 0;
  Jumpgate.ActivationDelay     := 0;
  Jumpgate.InitalUpdateCount   := 10;
  Jumpgate.InitalUpdateInterval:= 0.5;

  with Jumpgate.Affectors.Add(TPHXAttractor.Create) as TPHXAttractor do
  begin
    Force:=150.000;
    PositionX:=0.000;
    PositionY:=0.000;
    PositionZ:=0.000;
    RelativeToSystem:=True;
  end;


  //--- Graphs
  Jumpgate.Graphs.Color.Keys.Clear;
  Jumpgate.Graphs.Color.Keys.Add(0.000, clrWhite);

  Jumpgate.Graphs.Alpha.Keys.Clear;
  Jumpgate.Graphs.Alpha.Keys.Add(0.000, 1.000);
  Jumpgate.Graphs.Alpha.Keys.Add(1.000, 0.000);

  Jumpgate.Graphs.Scale.Keys.Clear;
  Jumpgate.Graphs.Scale.Keys.Add(0.000, 1.000);

  Jumpgate.Graphs.Velocity.Keys.Clear;
  Jumpgate.Graphs.Velocity.Keys.Add(0.000, Vector3f(1.0000, 1.0000, 1.0000));

  Jumpgate.Graphs.Spin.Keys.Clear;
  Jumpgate.Graphs.Spin.Keys.Add(0.000, 1.000);

  Jumpgate.Graphs.EmissionCount.Keys.Clear;
  Jumpgate.Graphs.EmissionCount.Keys.Add(0.000, 1.000);


  //--- Emission parameters
  Jumpgate.EmissionMode   := emTime;
  Jumpgate.EmissionDelay  := 0.100;
  Jumpgate.EmissionCount  := 2;
  Jumpgate.EmissionRepeats:= 0;

  Jumpgate.EmissionPoints.Clear;
  Jumpgate.EmissionPoints.Capacity:=100;
  Jumpgate.EmissionPoints.Add(Vector3f(50.0000, 0.0000, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.9013, 3.1395, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.6057, 6.2667, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.1144, 9.3691, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(48.4292, 12.4345, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(47.5528, 15.4509, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(46.4888, 18.4062, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(45.2414, 21.2890, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(43.8153, 24.0877, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(42.2164, 26.7913, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(40.4508, 29.3893, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(38.5257, 31.8712, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(36.4484, 34.2274, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(34.2273, 36.4484, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(31.8712, 38.5257, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(29.3893, 40.4509, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(26.7913, 42.2164, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(24.0877, 43.8153, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(21.2890, 45.2414, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(18.4062, 46.4888, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(15.4508, 47.5528, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(12.4345, 48.4292, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(9.3691, 49.1144, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(6.2666, 49.6057, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(3.1395, 49.9013, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(0.0000, 50.0000, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-3.1395, 49.9013, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-6.2667, 49.6057, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-9.3691, 49.1144, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-12.4345, 48.4292, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-15.4509, 47.5528, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-18.4063, 46.4888, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-21.2890, 45.2413, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-24.0877, 43.8153, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-26.7914, 42.2164, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-29.3893, 40.4508, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-31.8712, 38.5256, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-34.2274, 36.4484, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-36.4485, 34.2273, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-38.5257, 31.8712, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-40.4509, 29.3892, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-42.2164, 26.7913, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-43.8154, 24.0876, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-45.2414, 21.2889, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-46.4888, 18.4062, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-47.5528, 15.4508, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-48.4292, 12.4344, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.1144, 9.3690, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.6057, 6.2666, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.9013, 3.1395, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-50.0000, -0.0001, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.9013, -3.1396, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.6057, -6.2667, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-49.1144, -9.3691, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-48.4291, -12.4345, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-47.5528, -15.4509, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-46.4888, -18.4063, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-45.2413, -21.2890, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-43.8153, -24.0877, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-42.2164, -26.7914, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-40.4508, -29.3893, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-38.5256, -31.8713, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-36.4484, -34.2274, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-34.2273, -36.4485, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-31.8712, -38.5257, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-29.3892, -40.4509, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-26.7913, -42.2164, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-24.0876, -43.8154, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-21.2889, -45.2414, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-18.4062, -46.4888, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-15.4508, -47.5528, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-12.4344, -48.4292, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-9.3690, -49.1144, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-6.2666, -49.6057, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(-3.1395, -49.9013, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(0.0001, -50.0000, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(3.1396, -49.9013, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(6.2667, -49.6057, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(9.3691, -49.1143, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(12.4346, -48.4291, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(15.4509, -47.5528, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(18.4063, -46.4888, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(21.2890, -45.2413, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(24.0878, -43.8153, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(26.7914, -42.2164, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(29.3893, -40.4508, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(31.8713, -38.5256, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(34.2274, -36.4484, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(36.4485, -34.2273, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(38.5257, -31.8711, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(40.4509, -29.3892, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(42.2164, -26.7913, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(43.8154, -24.0876, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(45.2414, -21.2889, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(46.4889, -18.4061, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(47.5529, -15.4508, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(48.4292, -12.4344, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.1144, -9.3690, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.6057, -6.2666, 0.0000));
  Jumpgate.EmissionPoints.Add(Vector3f(49.9013, -3.1394, 0.0000));
  Jumpgate.EmissionPointsInOrder:= False;
  Jumpgate.LifeMin:= 4;
  Jumpgate.LifeMax:= 5;

  //--- Apperance parameters
  Jumpgate.TextureName      := 'particles.png';
  Jumpgate.Shape            := psAlignZ;
  Jumpgate.Blending         := bmAdd;
  Jumpgate.PatternSize      := ps4x4;
  Jumpgate.PatternIndexMin  := 9;
  Jumpgate.PatternIndexMax  := 9;
  Jumpgate.ColorMin         := Color4f(0.4902, 0.4902, 1.0000, 1.0000);
  Jumpgate.ColorMax         := Color4f(0.4902, 0.4902, 1.0000, 1.0000);
  Jumpgate.ColorAsGrayscale := False;
  Jumpgate.SizeMin          := 16;
  Jumpgate.SizeMax          := 16;
  Jumpgate.GrowthMin        := 0;
  Jumpgate.GrowthMax        := 0;

  //--- Physics parameters
  Jumpgate.Direction        := Vector3f(1.0000, 0.0000, 0.0000);
  Jumpgate.SpreadMin        := 180;
  Jumpgate.SpreadMax        := 180;
  Jumpgate.VelocityMin      := 0;
  Jumpgate.VelocityMax      := 0;
  Jumpgate.SpinMin          := 0;
  Jumpgate.SpinMax          := 0;
  Jumpgate.Acceleration     := Vector3f(0.0000, 0.0000, 0.0000);

  Jumpgate.Initialize;



  Result:= Jumpgate;
end;

      *)






end.
