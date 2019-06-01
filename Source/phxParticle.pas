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
unit phxParticle;
//< Particle engine

interface

{$I phxConfig.inc}

uses
  Types, Classes, SysUtils,

  phxParticleGraphs,

  phxTypes,
  phxClasses,
  phxMath,
  phxDevice,
  phxCanvas,
  phxTexture;

const

// Version of the particle effect files
PHXPARTICLE_VERSION = 20;
// Default file extension for the effect files
PHXPARTICLE_EXT    = '.phxpar';

type

TPHXParticleManager = class;
TPHXParticleList    = class;
TPHXParticleEffect  = class;
TPHXParticleSystem  = class;

// Render callback used to render a list of particles
// @param(Particles List of parameters to render)
// @param(Canvas The canvas to render to for 2D particles)
// @param(Buffer The canvas to render to for 3D particles)
TPHXParticleRenderer = procedure(Particles: TPHXParticleList; Canvas: TPHXCanvas; Buffer: TPHXBuffer);

// Emission mode of an particle emittor
//------------------------------------------------------------------------------
TPHXEmissionMode = (
  // Emit particles over time
  emTime,
  // Emit particles over distance traveled
  emDistance
);

// Size of the pattterns for a particle, for speed only rectangular patterns is
// suppported
//------------------------------------------------------------------------------
TPHXParticlePatternSize = (
  // No pattern, use the full texture
  ps1x1 ,
  // 4 patterns, arranged in 2 rows and 2 columns
  ps2x2,
  // 16 patterns arranged in 4 rows and 4 columns
  ps4x4,
  // 64 patterns, arranged in 8 rows and 8 columns
  ps8x8
);

// Blending for each particle system
//------------------------------------------------------------------------------
TPHXParticleBlending = (
  // Normal, none blending
  bmNone,
  // Additive blending
  bmAdd,
  // Subtractive blending
  bmSub,
  // Alpha blending
  bmAlpha
);

// Shape of the particles, determines the particle renderer to use
//------------------------------------------------------------------------------
TPHXParticleShape = (
  // The particle is rendered as a single point
  psPoint,

  // The particle is rendered facing the camera
  psBillboard,
  // The particle is rendered facing the camera with spin
  psBillboardRotated,

  // The particle is rendered as a quad with the normal pointing in the X-Axis
  psAlignX,
  // The particle is rendered as a quad with the normal pointing in the X-Axis
  // and with particle spin
  psAlignXRotated,

  // The particle is rendered as a quad with the normal pointing in the Y-Axis
  psAlignY,
  // The particle is rendered as a quad with the normal pointing in the Y-Axis
  // and with particle spin
  psAlignYRotated,

  // The particle is rendered as a quad with the normal pointing in the Z-Axis,
  // the default for 2D particle systems
  psAlignZ,
  // The particle is rendered as a quad with the normal pointing in the Z-Axis
  // and with particle spin
  psAlignZRotated,

  // The particle is rendered as interconnected, billboarded quads
  psTrail,
  // The particle is rendered as interconnected axis aligned quads
  psTrailZ,

  // Custom particle shape, you have to register a renderer for this shape with RegisterRenderer
  psCustom
);

// Options for particle effects
//------------------------------------------------------------------------------
TPHXParticleOptions = set of (
  // If the particles should be moved with the owning particle system,
  poLinkToSystem,
  // If this particle system should be idle until the parent system is finished
  poQueued
);


{$REGION 'TPHXParticleParameter'}

// Word particle parameter
//------------------------------------------------------------------------------
TPHXParticleParameter1w = record
  public
    // Base value
    Value: Word;
    // Variance to add to the value
    Variance: Word;
  public
    class function Create(const AValue: Word): TPHXParticleParameter1w; overload; static;
    class function Create(const AValue, AVariance: Word): TPHXParticleParameter1w; overload; static;
  public
    // Return a random value between the minimum and maximum value of this parameter
    function RandomValue: Word;
  end;

// Float particle parameter
//------------------------------------------------------------------------------
TPHXParticleParameter1f = record
  public
    // Base value
    Value: Single;
    // Variance to add to the value
    Variance: Single;
  public
    class function Create(const AValue: Single): TPHXParticleParameter1f; overload; static;
    class function Create(const AValue, AVariance: Single): TPHXParticleParameter1f; overload; static;
  public
    // Return a random value between the minimum and maximum value of this parameter
    function RandomValue: Single;
  end;

// Vector3f particle parameter
//------------------------------------------------------------------------------
TPHXParticleParameter3f = record
  public
    Value: TVector3f;
    Variance: TVector3f;
  public
    class function Create(const AValue: TVector3f): TPHXParticleParameter3f; overload; static;
    class function Create(const AValue, AVariance: TVector3f): TPHXParticleParameter3f; overload; static;
  public
    // Return a random value between the minimum and maximum value of this parameter
    function RandomValue: TVector3f;
  end;

// Color particle parameter
//------------------------------------------------------------------------------

{ TPHXParticleParameterCf }

TPHXParticleParameterCf = record
  public
    Value: TColor4f;
    Variance: TColor4f;
  public
    class function Create(const AValue: TColor4f): TPHXParticleParameterCf; overload; static;
    class function Create(const AValue, AVariance: TColor4f): TPHXParticleParameterCf; overload; static;
  public
    // Return a random value between the minimum and maximum value of this parameter
    function RandomValue: TColor4f;
  end;

{$ENDREGION}



{$REGION 'TPHXParticle'}

// A single particle
//------------------------------------------------------------------------------
PPHXParticle = ^TPHXParticle;
TPHXParticle = record
  public
    // Initial values of the particles
    InitialPosition: TVector3f;
    InitialSize    : Single;
    InitialColor   : TColor4f;

    Energy: Single;
    // The time, in seconds since the particle was emitted
    Time: Single;
    // The lifetime of the particle
    Life: Single;
    // Size of the particle
    Size: Single;
    // Growth of the particle
    Growth: Single;
    // Pattern index of the particle
    Pattern: Byte;
    // The current color of the particle
    Color: TColor4f;
    // The position
    Position: TVector3f;
    // The velocity in units per second
    Velocity: TVector3f;
    // Rotation of the particle, the axis of rotation depends on the particle shape
    Angle: Single;
    // The rotation change in degrees per second (angular velocity)
    Spin: Single;
    // Bounding box of the particle
    Bounds: TRectf;
    Graph: Integer;

    // Previous particle position
    PreviousPosition: TVector3f;
    // This is the final values of the particle after affected by the graphs
   // FinalPattern: Byte;
  public
    // Creates a new particle
    class function Create: TPHXParticle; static;
  end;

PParticleList = ^TParticleList;
TParticleList = array[0..$00AFFFFF] of TPHXParticle;

// A list of particles
//------------------------------------------------------------------------------
TPHXParticleList = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PParticleList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXParticle;
    procedure SetItem(Index: Integer; const Value: TPHXParticle);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;
    // Add a item to the list
    procedure Add(const Value: TPHXParticle );
    // Deletes a particle in the list
    procedure Delete(Index: Integer);

    // Enshures room for a number of particles and returns the index of the first one
    function Alloc(ACount: Integer): Integer; overload;

    // The current number of items in the list
    property Count: Integer read FCount write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PParticleList read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TPHXParticle read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXParticleAffector'}

TPHXParticleAffectorClass = class of TPHXParticleAffector;

// The affector is a plugin for a particle emittor that adds new functionality
// to the particle effect
//------------------------------------------------------------------------------
TPHXParticleAffector = class(TPersistent)
  private
    FEffect: TPHXParticleEffect;
  protected
    function GetName: string; virtual;

    // Overrride this to load the affector to a stream
    procedure LoadAffector(Reader: TPHXReader); virtual; abstract;
    // Overrride this to save the affector to a stream
    procedure SaveAffector(Writer: TPHXWriter); virtual; abstract;
  public
    // Default constructor
    constructor Create(AEffect: TPHXParticleEffect); virtual;

    // Load the effect from a file.
    procedure LoadFromFile(const FileName: String);
    // Load the effect from a stream.
    procedure LoadFromStream(const Stream: TStream);

    // Save the effect to a file.
    procedure SaveToFile(const FileName: String);
    // Save the effect to a stream.
    procedure SaveToStream(const Stream: TStream);

    // Iniitalize the affector called when it is assigned to a particle system
    procedure Initialize; virtual;
    // Affect all the particles in the particle system
    procedure Update(System: TPHXParticleSystem; DeltaTime: Double); virtual; abstract;

    // The owning particle effect
    property Effect: TPHXParticleEffect read FEffect;
    // Name of the affector
    property Name: String read GetName;
  end;

PParticleAffectorList = ^TParticleAffectorList;
TParticleAffectorList = array[0..$00FFFFFF] of TPHXParticleAffector;

//------------------------------------------------------------------------------
TPHXParticleAffectors = class
  private
    FEffect: TPHXParticleEffect;
    FList: TList;

    function GetCount: Integer;
    function GetList: PParticleAffectorList;
    function GetItem(const Index: Integer): TPHXParticleAffector;
  public
    constructor Create(AEffect: TPHXParticleEffect);
    destructor Destroy; override;

    // Remove and free all affectors
    procedure Clear;

    // Add a affector to the list
    function Add(const Affector: TPHXParticleAffector): TPHXParticleAffector; overload;
    // Add a affector to the list
    function Add(const Affector: String): TPHXParticleAffector; overload;

    // Remove and free a effector from this list
    procedure Remove(const Affector: TPHXParticleAffector);

    // Load all affectors to a reader
    procedure LoadAffectors(Reader: TPHXReader);
    // Save all affectors from a reader
    procedure SaveAffectors(Writer: TPHXWriter);

    // Update all affectors
    procedure Initialize;
    // Update all affectors
    procedure Update(System: TPHXParticleSystem; DeltaTime: Double);

    // Returns the number of affectors in the list
    property Count: Integer read GetCount;
    // Returns a pointer to the list of affectors
    property List: PParticleAffectorList read GetList;
    // Returns a affector from the list
    property Items[const Index: Integer]: TPHXParticleAffector read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXParticleEmittor'}

// Shape for the emittor
//------------------------------------------------------------------------------
TPHXParticleEmittorShape = (
  // Unknown shape
  esCustom,
  // The emittor was creates as a line
  esLine,
  // The emittor was creates as a rectangle
  esRectangle,
  // The emittor was creates as a circle
  esCircle
);

// Emittor settings for a particle effect
//------------------------------------------------------------------------------
TPHXParticleEmittor = class
  private
   // The owning particle effect
    FEffect: TPHXParticleEffect;
    // Shape of the emittor, only used by the editor
    FShape: TPHXParticleEmittorShape;
    // The emission mode, distance or time
    FMode: TPHXEmissionMode;
    // Delay between each emission in seconds or units
    FDelay: Single;
    // Number of particles to emit
    FCount: Integer;
    // Repeat count of the effect, zero is infinite
    FRepeats: Word;
    // The points to emit from
    FPoints: TPHXVectorList3f;
    // Use the poins in order instead of randomly
    FPointsInOrder: Boolean;
  public
    // Create the particle emittor
    constructor Create(AEffect: TPHXParticleEffect);
    destructor Destroy; override;

    procedure LoadEmittor(Reader: TPHXReader);
    procedure SaveEmittor(Writer: TPHXWriter);

    // Create a line emittor
    procedure CreateLine(const Count: Integer; const Min: TVector3f; const Max: TVector3f);
    // Create a rectangle emittor
    procedure CreateRectangle(const Count: Integer; const ConstrainToSurface: Boolean; const Rect: TRectf);
    // Create a box emittor
    procedure CreateBox(const Count: Integer; const ConstrainToSurface: Boolean; const Box: TBoxf);
    // Create a circle emittor
    procedure CreateCircle(const Count: Integer; const ConstrainToSurface: Boolean; const Radius: Single);

    // Emit a single particle for a system
    procedure Emit(const System: TPHXParticleSystem; var Particle: TPHXParticle);

    // The owning particle effect
    property Effect: TPHXParticleEffect read FEffect;
    // Shape of the emittor, only used by the editor
    property Shape: TPHXParticleEmittorShape read FShape write FShape;
    // The emission mode, emit over time or distance traveled
    property Mode: TPHXEmissionMode read FMode write FMode;
    // Delay between each emission in seconds or units
    property Delay: Single read FDelay write FDelay;
    // Number of particles to emit
    property Count: Integer read FCount write FCount;
    // Repeat count of the effect, zero is infinite
    property Repeats: Word read FRepeats write FRepeats;
    // The points to emit from
    property Points: TPHXVectorList3f read FPoints;
    // Use the poins in order instead of randomly
    property PointsInOrder: Boolean read FPointsInOrder write FPointsInOrder;
  end;

{$ENDREGION}

{$REGION 'TPHXParticleEffect'}

// The file header for particle effects
//------------------------------------------------------------------------------
TPHXParticleHeader = record
  // The id of the effect file, should always be PHXPAR.
  Ident: array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// The particle efffect contains the parameters for a particle system
//------------------------------------------------------------------------------
TPHXParticleEffect = class(TPersistent)
  private
    FManager: TPHXParticleManager;
    // Parent particle effect
    FParent: TPHXParticleEffect;
    // Name of the particle effect
    FName: String;
    // Author of the effect
    FAuthor: String;
    // Version of the effect
    FVersion: String;
    // Comment of the effect
    FComment: String;

    // Maximum number of particles
    FQuota: Integer;
    // Duration of the particle effect, zero is infinite
    FDuration: Single;
    // Delay in seconds before activating the system
    FDelay: Single;

    // Number of updates to perform when the effect is assigned to a system
    FInitalUpdateCount: Integer;
    // Interval of of updates when the effect is assigned to a system
    FInitalUpdateInterval: Single;

    // If the particles should be moved with the owning particle system
    FLinkToSystem: Boolean;
    // If the particle system should be idle until the parent system is activated
    FIdleWhileParentActive: Boolean;

    FEmittor: TPHXParticleEmittor;
    // Life interval for each particle, in seconds
    FLife: TPHXParticleParameter1f;

    //--- Apperance parameters

    // The texture of the particles
    FTexture : TPHXTexture;
    // Name of the texture, for saving
    FTextureName : String;
    // The shape of each particle
    FShape: TPHXParticleShape;
    // The blending of each particle
    FBlending: TPHXParticleBlending;
    // Size of the patterns in the texture (1x1, 2x2, 4x4 or 8x8)
    FPatternSize: TPHXParticlePatternSize;
    // Min and max pattern index (0 <= Value <= GetParticlePatternCount(PatternSize) )
    FPatternIndex: Word;
    // Random variance to add to the particle index
    FPatternVariance: Word;

    // Start color of the particles
    FColor: TPHXParticleParameterCf;
    // The color is grayscale
    FGrayscale: Boolean;

    // Size interval for each particle
    FSize: TPHXParticleParameter1f;
    // Growth interval for each particle
    FGrowth: TPHXParticleParameter1f;

    //--- Physics parameters

    // Initial direction
    FDirection: TVector3f;
    // Spread around the direction vector in degrees
    FSpread: Single;

    // Velocity per particle in units/sec, in the direction of the particle
    FVelocity: TPHXParticleParameter1f;
    // Spin per particle in degrees/sec (angular velocity)
    FSpin: TPHXParticleParameter1f;
    // Acceleration in world coordinates
    FAcceleration: TVector3f;

    // Graphs for the particle effect
    FGraphs: TPHXParticleGraphs;
    // Affectors for the particle effect
    FAffectors: TPHXParticleAffectors;

    // Returns the renderer for this effect depending on the shape
    function GetRenderer: TPHXParticleRenderer;

    procedure SetLife     (const Value: TPHXParticleParameter1f);
    procedure SetLifeMax  (const Value: Single);
    procedure SetLifeMin  (const Value: Single);
    procedure SetTexture  (const Value: TPHXTexture);
    procedure SetParent   (const Value: TPHXParticleEffect);
    procedure SetDirection(const Value: TVector3f);
    procedure SetDirectionX(const Value: Single);
    procedure SetDirectionY(const Value: Single);
    procedure SetDirectionZ(const Value: Single);
    procedure SetTextureName(const Value: String);
  protected
    // Load the effect from a reader
    procedure LoadEffect(Reader: TPHXReader);
    // Save the effect to a writer
    procedure SaveEffect(Writer: TPHXWriter);
  public
    constructor Create(AManager: TPHXParticleManager);
    destructor Destroy; override;

    // Initialize the particle effect, call this before using the effect
    procedure Initialize;

    // Load the default settings
    procedure LoadDefaults;

    // Load the effect from a file.
    procedure LoadFromFile(const FileName: String);
    // Load the effect from a stream.
    procedure LoadFromStream(Stream: TStream);

    // Save the effect to a file.
    procedure SaveToFile(const FileName: String);
    // Save the effect to a stream.
    procedure SaveToStream(Stream: TStream);

    // The owning particle manager
    property Manager: TPHXParticleManager read FManager;
     // Parent particle effect
    property Parent: TPHXParticleEffect read FParent write SetParent;
    // Name of the particle effect
    property Name: String read FName write FName;
    // Author of the effect
    property Author: String read FAuthor write FAuthor;
    // Version of the effect
    property Version: String read FVersion write FVersion;
    // Comment of the effect
    property Comment: String read FComment write FComment;
    // Maximum number of particles
    property Quota: Integer read FQuota write FQuota;
    // Duration in seconds of the effect
    property Duration: Single read FDuration write FDuration;
    // Delay in seconds before activating the effect
    property Delay: Single read FDelay write FDelay;

    // Number of updates to perform when the effect is assigned to a system
    property InitalUpdateCount: Integer read FInitalUpdateCount write FInitalUpdateCount;
    // Interval of of updates when the effect is assigned to a system
    property InitalUpdateInterval: Single read FInitalUpdateInterval write FInitalUpdateInterval;

    // If the particles should be moved with the owning particle system
    property LinkToSystem: Boolean read FLinkToSystem write FLinkToSystem;
    // If the particle system should be idle until the parent system is activated
    property IdleWhileParentActive: Boolean read FIdleWhileParentActive write FIdleWhileParentActive;


    //--- Emission parameters
    property Emittor: TPHXParticleEmittor read FEmittor;
    // Life interval for each particle, in seconds
    property Life: TPHXParticleParameter1f read FLife write SetLife;
    // Minimum lifetime for each particle
    property LifeValue: Single read FLife.Value write SetLifeMin;
    // Maximum lifetime for each particle
    property LifeVariance: Single read FLife.Variance write SetLifeMax;

    //--- Apperance parameters

    // The texture of the particles
    property Texture: TPHXTexture read FTexture write SetTexture;
    // Name of the texture, for saving
    property TextureName: String read FTextureName write SetTextureName;
    // The shape of each particle
    property Shape: TPHXParticleShape read FShape write FShape;
    // The blending of each particle
    property Blending: TPHXParticleBlending read FBlending write FBlending;
    // Size of the patterns in the texture (1x1, 2x2, 4x4 or 8x8)
    property PatternSize: TPHXParticlePatternSize read FPatternSize write FPatternSize ;

    // Min and max pattern index (0 <= Value <= GetParticlePatternCount(PatternSize) )
    property PatternIndex: Word read FPatternIndex write FPatternIndex;
    // Random variance to add to the particle index
    property PatternVariance: Word read FPatternVariance write FPatternVariance;

    // Start color of the particles
    property Color: TPHXParticleParameterCf read FColor write FColor;
    // Minimum color
    property ColorValue: TColor4f read FColor.Value write FColor.Value;
    // Maximum color
    property ColorVariance: TColor4f read FColor.Variance write FColor.Variance;
    // The color is grayscale
    property Grayscale: Boolean read FGrayscale write FGrayscale;

    // Size interval for each particle
    property Size: TPHXParticleParameter1f read FSize write FSize;
    // Minimum size for each particle
    property SizeMin: Single read FSize.Value write FSize.Value;
    // Maximum size for each particle
    property SizeMax: Single read FSize.Variance write FSize.Variance;

    // Growth interval for each particle
    property Growth: TPHXParticleParameter1f read FGrowth write FGrowth;
    // Minimum growth for each particle
    property GrowthMin: Single read FGrowth.Value write FGrowth.Value;
    // Maximum growth for each particle
    property GrowthMax: Single read FGrowth.Variance write FGrowth.Variance;

    //--- Movement parameters

    // Initial direction of the particle system
    property Direction: TVector3f read FDirection write SetDirection;
    //  Initial direction along the X-Axis
    property DirectionX: Single read FDirection.X write SetDirectionX;
    //  Initial direction along the Y-Axis
    property DirectionY: Single read FDirection.Y write SetDirectionY;
    //  Initial direction along the Z-Axis
    property DirectionZ: Single read FDirection.Z write SetDirectionZ;

    // Spread interval to add to the direction, in degrees.
    property Spread: Single read FSpread write FSpread;

    // Velocity interval for each particle, units/second in the direction of the particle
    property Velocity: TPHXParticleParameter1f read FVelocity write FVelocity;
    // Minimum velocity for each particle
    property VelocityMin: Single read FVelocity.Value write FVelocity.Value;
    // Maximum velocity for each particle
    property VelocityMax: Single read FVelocity.Variance write FVelocity.Variance;

    // Spin interval for each particle in degrees per second
    property Spin: Single read FSpin.Value write FSpin.Value;
    // Minimum spin for each Particle
    property SpinVariance: Single read FSpin.Variance write FSpin.Variance;

    // Acceleration in world coordinates for each particle, in units/sec^
    property Acceleration: TVector3f read FAcceleration write FAcceleration;
    // Acceleration along the X-Axis
    property AccelerationX: Single read FAcceleration.X write FAcceleration.X;
    // Acceleration along the Y-Axis
    property AccelerationY: Single read FAcceleration.Y write FAcceleration.Y;
    // Acceleration along the Z-Axis
    property AccelerationZ: Single read FAcceleration.Z write FAcceleration.Z;

    // Graphs for the particle effect
    property Graphs: TPHXParticleGraphs read FGraphs;
    // Affectors for the particle effect
    property Affectors: TPHXParticleAffectors read FAffectors;


    //--- Runtime parameters

    // Return the renderer for the current shape
    property Renderer: TPHXParticleRenderer read GetRenderer;
  end;

// Container for a list of particle effects
//------------------------------------------------------------------------------

{ TPHXParticleEffects }

TPHXParticleEffects = class
  private
    FManager: TPHXParticleManager;
    FList: TList;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TPHXParticleEffect;
  public
    // Default constructor
    constructor Create(AManager: TPHXParticleManager);
    // Destroys the particle effect list
    destructor Destroy; override;

    // Remove and free all particle effects
    procedure Clear;

    // Add a new particle effect to the list
    function Add(const Name: String): TPHXParticleEffect; overload;
    // Add a existing particle effect to the list
    function Add(const Effect: TPHXParticleEffect): TPHXParticleEffect; overload;
    // Load a particle effect from a file and add it to the list
    function Load(const FileName: String): TPHXParticleEffect;

    // Returns the index of a particle effect or -1 if not found
    function IndexOf(const Name: String): Integer;
    // Returns the index of a particle effect or -1 if not found
    function Find(const Name: String): TPHXParticleEffect;

    // Return the number of items in the list
    property Count: Integer read GetCount;
    // Return a effect from the list
    property Items[const Index: Integer]: TPHXParticleEffect read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXParticleSystem'}

// The particle system is a instance of a particle effect
//------------------------------------------------------------------------------
TPHXParticleSystem = class
  private
    FManager  : TPHXParticleManager;
    FName     : String;
    FParent   : TPHXParticleSystem;
    FParticles: TPHXParticleList;

    FTime       : Single;
    FAlive      : Boolean;
    FActive     : Boolean;
    FPosition   : TVector3f;
    FDirection  : TVector3f;
    FBounds     : TBoxf;

    FEffect   : TPHXParticleEffect;
    FTexture  : TPHXTexture;
    FRenderer : TPHXParticleRenderer;


    FInitialized: Boolean;

    // Update the bounding box of the particle system
    procedure CalculateBounds;

    procedure SetEffect(const Value: TPHXParticleEffect);
  protected
    // Number of emissions so far for the system
    EmissionCounter: Integer;
    // Index of the emission point if Emittor.RandomOrder = False
    EmissionIndex: Integer;
    // Time of the next emission
    EmissionTime: Single;
    // Distance of the next emission
    EmissionDistance: Single;
    // Previous position for the emission
    EmissionPosition: TVector3f;

    procedure UpdateEmittor(FrameTime: Single);
    // Emits a single particle
   // procedure EmitParticle(const Delta: Single; var Particle: TPHXParticle);

    // Update all the parameters in the system
    procedure UpdateParticles(FrameTime: Single);
    procedure UpdateParticle(FrameTime: Single; var Particle: TPHXParticle);
  public
    // Create a new particle system
    constructor Create(AManager: TPHXParticleManager);
    // Destroy this particle system
    destructor Destroy; override;

    // Initialize the system, this is called when the current effect is changed
    procedure Initialize; overload;
    // Initialize the system with a new effect
    procedure Initialize(AEffect: TPHXParticleEffect); overload;
    // Reset the particle system
    procedure Reset;
    // Clear the particle system
    procedure Clear;
    // Update the particle system
    procedure Update(const FrameTime: Single);
    // Render the particles
    procedure Render(Canvas: TPHXCanvas);

    // Owning particle manager
    property Manager: TPHXParticleManager read FManager;
    // Name of the current effect
    property Name: String read FName write FName;
    // Parent particle system, might be null
    property Parent: TPHXParticleSystem read FParent write FParent;
    // List of particles
    property Particles: TPHXParticleList read FParticles;

    // Elapsed time since the system was activated
    property Time: Single read FTime write FTime;
    // If the particle system is alive
    property Alive: Boolean read FAlive write FAlive;
    // If the particle system is alive
    property Active: Boolean read FActive write FActive;
    // Current position of the particle system
    property Position: TVector3f read FPosition write FPosition;
    // Current X position
    property PositionX: Single read FPosition.X write FPosition.X;
    // Current Y position
    property PositionY: Single read FPosition.Y write FPosition.Y;
    // Current Z position
    property PositionZ: Single read FPosition.Z write FPosition.Z;
    // Current direction of the particle system
    property Direction : TVector3f read FDirection write FDirection;
    // Bounding box of the particle system
    property Bounds: TBoxf read FBounds;

    // The current effect
    property Effect: TPHXParticleEffect read FEffect write SetEffect;
    // The texture for the particle system.
    property Texture: TPHXTexture read FTexture write FTexture;
    // The current particle renderer
    property Renderer: TPHXParticleRenderer read FRenderer;
  end;

PParticleSystemList = ^TParticleSystemList;
TParticleSystemList = array[0..$00FFFFFF] of TPHXParticleSystem;

// List of particle systems
//------------------------------------------------------------------------------
TPHXParticleSystems = class
  private
    FManager : TPHXParticleManager;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PParticleSystemList;
    function GetItem(const Index: Integer): TPHXParticleSystem;

    procedure SetCount(const Value: Integer);
    procedure SetCapacity(const ACapacity: Integer);
  public
    // Create a new particle system
    constructor Create(AManager: TPHXParticleManager);
    // Destroy this particle system
    destructor Destroy; override;

    // Remove all alive systems
    procedure Clear;

    // Returs the next avaiable particle system
    function Add: TPHXParticleSystem;

    // Owning particle manager
    property Manager: TPHXParticleManager read FManager;
    // Number of active particle systems
    property Count: Integer read FCount write SetCount;
    // Total number of particle systems
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PParticleSystemList read FList;
    // Pointer to the internal list
    property Items[const Index: Integer]: TPHXParticleSystem read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXParticleManager'}

//------------------------------------------------------------------------------
TPHXParticleManager = class
  private
    FQuota   : Integer;
    FEffects : TPHXParticleEffects;
    FTextures: TPHXTextureList;
    FSystems : TPHXParticleSystems;

    procedure SetQuota(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Remove all alive and dead systems
    procedure Clear;

    // Update all particle systems
    procedure Update(const DeltaTime: Single);
    // Render all particle systems
    procedure Render(Canvas: TPHXCanvas);

    // Spawn a new particle system from a effect
    function Spawn(const Effect: String): TPHXParticleSystem; overload;
    function Spawn(const Effect: TPHXParticleEffect): TPHXParticleSystem; overload;
//    function Spawn(const Effect: TPHXParticleEffect; Initialize: Boolean = True): TPHXParticleSystem;

    // Maximum number of simultanious particle systems
    property Quota: Integer read FQuota write SetQuota;
    property Textures: TPHXTextureList read FTextures write FTextures;

    property Effects: TPHXParticleEffects read FEffects;
    property Systems: TPHXParticleSystems read FSystems;
   end;

{$ENDREGION}


// Get the renderer for a specific shape
function GetPatternCount(const Size: TPHXParticlePatternSize): Integer;
// Return the pattern index for a pattern size and pattern index
function GetPatternIndex(const Size: TPHXParticlePatternSize; const Pattern: Byte): Byte;

// Register a particle renderer for a given shape
procedure RegisterParticleRenderer(Shape: TPHXParticleShape; Renderer: TPHXParticleRenderer);
// Register a particle affector
procedure RegisterParticleAffector(const Name: String; Affector: TPHXParticleAffectorClass);

function getAffectorRegistryCount: Integer;
function getAffectorRegistryName(Index: Integer): String;
function getAffectorRegistryClass(Index: Integer): TPHXParticleAffectorClass;

implementation

uses phxParticleRenderers;

var RendererRegistry: array[TPHXParticleShape] of TPHXParticleRenderer;

// Register a particle renderer for a given shape
//------------------------------------------------------------------------------
procedure RegisterParticleRenderer(Shape: TPHXParticleShape; Renderer: TPHXParticleRenderer);
begin
  RendererRegistry[Shape]:= Renderer;
end;

//------------------------------------------------------------------------------
//function GetRenderer(const Shape: TPHXParticleShape): TPHXParticleRenderer;
//begin
//  Result:= RendererRegistry[Shape];
//end;

//------------------------------------------------------------------------------
var AffectorRegistry: array of record
  Name : String;
  Affector: TPHXParticleAffectorClass;
end;

// Register a particle affector
//------------------------------------------------------------------------------
procedure RegisterParticleAffector(const Name: String; Affector: TPHXParticleAffectorClass);
var Index: Integer;
begin
  Index:= Length(AffectorRegistry);

  SetLength(AffectorRegistry, Index+1 );

  AffectorRegistry[Index].Name := Name;
  AffectorRegistry[Index].Affector:= Affector;
end;

//------------------------------------------------------------------------------
function getAffectorRegistryCount: Integer;
begin
  Result:=  Length(AffectorRegistry);
end;

//------------------------------------------------------------------------------
function getAffectorRegistryName(Index: Integer): String;
begin
  Result:= AffectorRegistry[Index].Name;
end;
//------------------------------------------------------------------------------
function getAffectorRegistryClass(Index: Integer): TPHXParticleAffectorClass;
begin
  Result:= AffectorRegistry[Index].Affector;
end;


//------------------------------------------------------------------------------
function ParticleTextureSort(Item1, Item2: Pointer): Integer;
var S1: TPHXParticleSystem;
var S2: TPHXParticleSystem;
begin
  S1:= TPHXParticleSystem(Item1);
  S2:= TPHXParticleSystem(Item2);

  if Assigned(S1) and Assigned(S2) and Assigned(S1.Texture) and Assigned(S2.Texture) then
  begin
    {$IFDEF FPC}
    Result:= Pointer(@S1.Texture)- Pointer(@S2.Texture);
    {$ELSE}
    Result:= Cardinal(@S1.Texture)- Cardinal(@S2.Texture);
    {$ENDIF}
  end else
  begin
    Result:= 0;
  end;
end;


// Start index in the rect array
const PatternOffset: array[TPHXParticlePatternSize] of Byte = (00, 01, 05, 21);
// Number of rects
//const PHXPAR_PATTERN_COUNT: array[TPHXParticlePatternSize] of Byte = (01, 04, 16, 64);

//------------------------------------------------------------------------------
function GetPatternCount(const Size: TPHXParticlePatternSize): Integer;
begin
  Result:= 0;
  case Size of
    ps1x1: Result:= 01;
    ps2x2: Result:= 04;
    ps4x4: Result:= 16;
    ps8x8: Result:= 64;
  end;
end;

//------------------------------------------------------------------------------
function GetPatternIndex(const Size: TPHXParticlePatternSize; const Pattern: Byte): Byte;
begin
  Result:= 0;
  case Size of
    ps1x1: Result:= 00 + Pattern;
    ps2x2: Result:= 01 + Pattern;
    ps4x4: Result:= 05 + Pattern;
    ps8x8: Result:= 21 + Pattern;
  end;
end;

// Table of random values for generating particle parameters
var RandomTable: array[Word] of Single;
// Current index in the random table
var RandomIndex: Word;

//------------------------------------------------------------------------------
procedure RandomTableInit;
var Index: Word;
begin
  Randomize;

  RandomIndex:= 0;
  for Index:= Low(Word) to High(Word) do
  begin
    RandomTable[Index]:= Random;
  end;
end;

//------------------------------------------------------------------------------
function GetRandomValue(const Value: Word; const Variance: Word): Word; overload;
begin
  Result:= Value + Round(Variance * RandomTable[RandomIndex]);

  Inc(RandomIndex);
end;

//------------------------------------------------------------------------------
function GetRandomValue(const Value: Single; const Variance: Single): Single; overload;
begin
  Result:= Value + (Variance * RandomTable[RandomIndex]);

  Inc(RandomIndex);
end;

//------------------------------------------------------------------------------
function GetRandomValue(const Value: TColor4f; const Variance: TColor4f): TColor4f; overload;
begin
  Result.Red  := Value.Red   + (Variance.Red   * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Green:= Value.Green + (Variance.Green * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Blue := Value.Blue  + (Variance.Blue  * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Alpha:= Value.Alpha + (Variance.Alpha * RandomTable[RandomIndex]); Inc(RandomIndex);
end;


//------------------------------------------------------------------------------
function GetRandomIndex(const Value: Integer): Integer;
begin
  Result:= Trunc( Value * RandomTable[RandomIndex]);

  Inc(RandomIndex);
end;

//------------------------------------------------------------------------------
function RandomInterval(const Min, Max: Single): Single;
begin
  Result:= Min + ((Max - Min) * RandomTable[RandomIndex]);

  Inc(RandomIndex);
end;

{$REGION 'TPHXParticleParameter1w'}

// TPHXParticleParameter1w
//==============================================================================
class function TPHXParticleParameter1w.Create(const AValue: Word): TPHXParticleParameter1w;
begin
  Result.Value   := AValue;
  Result.Variance:= 0;
end;

//------------------------------------------------------------------------------
class function TPHXParticleParameter1w.Create(const AValue, AVariance: Word): TPHXParticleParameter1w;
begin
  Result.Value   := AValue;
  Result.Variance:= AVariance;
end;

//------------------------------------------------------------------------------
function TPHXParticleParameter1w.RandomValue: Word;
begin
  Result:= Value + Round(Variance * RandomTable[RandomIndex]);

  Inc(RandomIndex);
end;

{$ENDREGION}

{$REGION 'TPHXParticleParameter1f'}

// TPHXParticleParameter1f
//==============================================================================
class function TPHXParticleParameter1f.Create(const AValue: Single): TPHXParticleParameter1f;
begin
  Result.Value   := AValue;
  Result.Variance:= 0;
end;

//------------------------------------------------------------------------------
class function TPHXParticleParameter1f.Create(const AValue, AVariance: Single): TPHXParticleParameter1f;
begin
  Result.Value   := AValue;
  Result.Variance:= AVariance;
end;

//------------------------------------------------------------------------------
function TPHXParticleParameter1f.RandomValue: Single;
begin
  Result:= Value + Variance * RandomTable[RandomIndex];

  Inc(RandomIndex);
end;

{$ENDREGION}

{$REGION 'TPHXParticleParameter3f'}

// TPHXParticleParameter3f
//==============================================================================
class function TPHXParticleParameter3f.Create(const AValue: TVector3f): TPHXParticleParameter3f;
begin
  Result.Value   := AValue;
  Result.Variance:= Vector3f_Zero;
end;

//------------------------------------------------------------------------------
class function TPHXParticleParameter3f.Create(const AValue, AVariance: TVector3f ): TPHXParticleParameter3f;
begin
  Result.Value   := AValue;
  Result.Variance:= AVariance;
end;

//------------------------------------------------------------------------------
function TPHXParticleParameter3f.RandomValue: TVector3f;
begin
  Result.X:= Value.X + Variance.X * RandomTable[RandomIndex]; Inc(RandomIndex);
  Result.Y:= Value.Y + Variance.Y * RandomTable[RandomIndex]; Inc(RandomIndex);
  Result.Z:= Value.Z + Variance.Z * RandomTable[RandomIndex]; Inc(RandomIndex);
end;

{$ENDREGION}

{$REGION 'TPHXParticleParameterCf'}

// TPHXParticleParameter3f
//==============================================================================
class function TPHXParticleParameterCf.Create(const AValue: TColor4f): TPHXParticleParameterCf;
begin
  Result.Value   := AValue;
  Result.Variance:= clrNone;
end;

//------------------------------------------------------------------------------
class function TPHXParticleParameterCf.Create(const AValue, AVariance: TColor4f): TPHXParticleParameterCf;
begin
  Result.Value   := AValue;
  Result.Variance:= AVariance;
end;

//------------------------------------------------------------------------------
function TPHXParticleParameterCf.RandomValue: TColor4f;
begin
  Result.Red  := Value.Red   + (Variance.Red   * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Green:= Value.Green + (Variance.Green * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Blue := Value.Blue  + (Variance.Blue  * RandomTable[RandomIndex]); Inc(RandomIndex);
  Result.Alpha:= Value.Alpha + (Variance.Alpha * RandomTable[RandomIndex]); Inc(RandomIndex);
end;

{$ENDREGION}

{$REGION 'TPHXParticle'}

// TPHXParticle
//==============================================================================
class function TPHXParticle.Create: TPHXParticle;
begin
  // The time, in seconds since the particle was emitted
  Result.Time:= 0;
  // The lifetime of the particle
  Result.Life:= 1;
  // Size of the particle
  Result.Size:= 32;
  // Pattern index of the particle
  Result.Pattern:= 0;
  // The  color of the particle
  Result.Color:= clrWhite;
  // The  fade of the particle
//  Result.Fade:= clrNone;
  // The position
  Result.Position:= Vector3f_Zero;
  // The velocity in units per second
  Result.Velocity:= Vector3f_Zero;

  // Rotation of the particle, the axis of rotation depends on the particle shape
  Result.Angle:= 0;
  // The rotation change in degrees per second
  Result.Spin:= 0;

  Result.InitialPosition:= Result.Position;
  Result.InitialSize    := Result.Size;
  Result.InitialColor   := Result.Color;
end;

{$ENDREGION}

{$REGION 'TPHXParticleList'}

// TPHXParticleList
//==============================================================================
constructor TPHXParticleList.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXParticleList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.Add(const Value: TPHXParticle);
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
function TPHXParticleList.Alloc(ACount: Integer): Integer;
begin
  // Return the index of the first particle
  Result:= FCount;

  FCount:= FCount + ACount;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXParticle));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
    begin
      Delta := 16
    end else
    begin
      Delta := 4;
    end;
  end;

  SetCapacity(Count + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXParticle));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXParticleList.GetItem(Index: Integer): TPHXParticle;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXParticleList.SetItem(Index: Integer; const Value: TPHXParticle);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXParticleAffector'}

// TPHXParticleAffector
//==============================================================================
constructor TPHXParticleAffector.Create(AEffect: TPHXParticleEffect);
begin
  FEffect:= AEffect;
end;

//------------------------------------------------------------------------------
function TPHXParticleAffector.GetName: string;
begin
  Result:= ClassName;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffector.LoadFromFile(const FileName: String);
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
procedure TPHXParticleAffector.SaveToFile(const FileName: String);
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
procedure TPHXParticleAffector.LoadFromStream(const Stream: TStream);
var Reader: TPHXReader;
begin
  Reader:= TPHXReader.Create(Stream);
  try
    LoadAffector(Reader);
  finally
    Reader.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffector.SaveToStream(const Stream: TStream);
var Writer: TPHXWriter;
begin
  Writer:= TPHXWriter.Create(Stream);
  try
    SaveAffector(Writer);
  finally
    Writer.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffector.Initialize;
begin

end;

{$ENDREGION}


{$REGION 'TPHXParticleAffectors'}

// TPHXParticleAffector
//==============================================================================
constructor TPHXParticleAffectors.Create(AEffect: TPHXParticleEffect);
begin
  FEffect:= AEffect;
  FList  := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleAffectors.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.Clear;
var Index: Integer;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    TPHXParticleAffector(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXParticleAffectors.Add(const Affector: TPHXParticleAffector): TPHXParticleAffector;
begin
  Result:= Affector;

  FList.Add(Affector)
end;

//------------------------------------------------------------------------------
function TPHXParticleAffectors.Add(const Affector: String): TPHXParticleAffector;
var Index: Integer;
begin
  for Index:= Low(AffectorRegistry) to High(AffectorRegistry) do
  begin
    if SameText(AffectorRegistry[Index].Name, Affector) then
    begin
      Result:= AffectorRegistry[Index].Affector.Create(FEffect);

      Flist.Add(Result);

      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.Remove(const Affector: TPHXParticleAffector);
var Index: Integer;
begin
  Index:= FList.IndexOf(Affector);
  if (Index <> -1) then
  begin
    FList.Delete(Index);

    Affector.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.LoadAffectors(Reader: TPHXReader);
begin

end;


//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.SaveAffectors(Writer: TPHXWriter);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.Initialize;
var Index: Integer;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    TPHXParticleAffector(FList.List[Index]).Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleAffectors.Update(System: TPHXParticleSystem; DeltaTime: Double);
var Index: Integer;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    TPHXParticleAffector(FList.List[Index]).Update(System, DeltaTime);
  end;
end;

//------------------------------------------------------------------------------
function TPHXParticleAffectors.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXParticleAffectors.GetList: PParticleAffectorList;
begin
  Result:= PParticleAffectorList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXParticleAffectors.GetItem(const Index: Integer): TPHXParticleAffector;
begin
  Result:= TPHXParticleAffector(FList.List[Index]);
end;


{$ENDREGION}

{$REGION 'TPHXParticleEmittor'}

//------------------------------------------------------------------------------
constructor TPHXParticleEmittor.Create(AEffect: TPHXParticleEffect);
begin
  FEffect:= AEffect;
  FPoints:= TPHXVectorList3f.Create;
  FMode  := emTime;
  FShape := esCustom;
  FDelay := 0.1;
  FCount := 1;
  FRepeats:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleEmittor.Destroy;
begin
  FPoints.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.LoadEmittor(Reader: TPHXReader);
begin
  FShape  := TPHXParticleEmittorShape(Reader.ReadByte);
  FMode   := TPHXEmissionMode(Reader.ReadByte);
  FDelay  := Reader.ReadSingle;
  FCount  := Reader.ReadInteger;
  FRepeats:= Reader.ReadWord;
  FPointsInOrder:= Reader.ReadBoolean;

  FPoints.LoadFromStream(Reader.Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.SaveEmittor(Writer: TPHXWriter);
begin
  Writer.WriteByte(Ord(FShape));
  Writer.WriteByte(Ord(FMode));
  Writer.WriteSingle(FDelay);
  Writer.WriteInteger(FCount);
  Writer.WriteWord(FRepeats);
  Writer.WriteBoolean(FPointsInOrder);

  FPoints.SaveToStream(Writer.Stream);
end;
      (*
//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.Update(System: TPHXParticleSystem; var Info: TPHXParticleEmittorInfo; const DeltaTime: Single);
var Count: Integer;
var Index: Integer;

var UpdateTime: Single;
var UpdateDelta: Single;
begin
  Count:= 0;

  case Mode of
    // Emit by time
    emTime:
    begin
      Info.Time:= Info.Time + DeltaTime;

      if Delay > 0 then
      begin
        Count:= Trunc(Info.Time / Delay);
      end else
      begin
        Count:= 1;
      end;
      Info.Time:= Info.Time - (Count * Delay);
    end;
    // Emit by distance
    emDistance:
    begin
      Info.Distance:= Info.Distance + VectorMagnitude( VectorSub(System.Position, Info.Position) );

      if Delay > 0 then
      begin
        Count:= Trunc(Info.Distance / Delay);
      end else
      begin
        Count:= 1;
      end;
      Info.Distance:= Info.Distance - (Count * Delay);

      Info.Position:= System.Position;
    end;
  end;
  // The count contains the number of emissions, multiply with the number of particles
  Count:= Count * FCount;

  // No particles to emit
  if Count = 0 then Exit;

  // Limit the maximum number of particles
  if System.Particles.Count + Count > System.Particles.Capacity  then
  begin
    Count:= System.Particles.Capacity - System.Particles.Count;
  end;

  UpdateTime := 0;
  UpdateDelta:= DeltaTime / Count;
  // Emit particles, each particle will be updated by a part of the frametime
  for Index:= System.Particles.Count to System.Particles.Count + Count - 1 do
  begin
    Emit(System, Info, System.Particles.List^[Index]);

    UpdateTime:= UpdateTime + UpdateDelta;
  end;

  System.Particles.Count:= System.Particles.Count + Count;
end;
*)
// Emits a single particle
//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.Emit(const System: TPHXParticleSystem; var Particle: TPHXParticle);
var Index       : Integer;
var Velocity    : Single;
var Spread      : Single;
var SpreadMatrix: TMatrix4f;
var SpreadVector: TVector3f;
begin
  Particle.Energy       := 0.0;
  Particle.Time         := 0.0;
  Particle.Angle        := 0;
  Particle.Color        := GetRandomValue(Effect.ColorValue, Effect.ColorVariance);
  Particle.Life         := Effect.Life.RandomValue;
  Particle.Size         := Effect.Size.RandomValue;
  Particle.Growth       := Effect.Growth.RandomValue;
  Particle.Spin         := GetRandomValue(Effect.Spin,         Effect.SpinVariance);
  Particle.Pattern      := GetRandomValue(Effect.PatternIndex, Effect.PatternVariance) + PatternOffset[Effect.PatternSize];

  if Effect.Grayscale then
  begin
    Particle.Color.Red  := Particle.Color.Red;
    Particle.Color.Green:= Particle.Color.Red;
    Particle.Color.Blue := Particle.Color.Red;
  end;

  // Pick a initial position from the emission points
  if Points.Count > 0 then
  begin
    if PointsInOrder then
    begin
      Index:= System.EmissionIndex;

      System.EmissionIndex:= (System.EmissionIndex + 1) mod Points.Count;
    end else
    // Pick a random point
    begin
      Index:= GetRandomIndex( Points.Count);
    end;

    Particle.Position.X:= System.Position.X + Points.List^[Index].X;
    Particle.Position.Y:= System.Position.Y + Points.List^[Index].Y;
    Particle.Position.Z:= System.Position.Z + Points.List^[Index].Z;
  end else
  begin
    Particle.Position.X:= System.Position.X;
    Particle.Position.Y:= System.Position.Y;
    Particle.Position.Z:= System.Position.Z;
  end;
  Particle.Velocity.X:= 0;
  Particle.Velocity.Y:= 0;
  Particle.Velocity.Z:= 0;

  Velocity:= GetRandomValue(Effect.Velocity.Value, Effect.Velocity.Variance);
  Spread  := GetRandomValue(-Effect.Spread, 2 * Effect.Spread);

  // Apply the particle velocity depending on the orientation
  case Effect.Shape of
    psAlignX, psAlignXRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationX(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, System.Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignY, psAlignYRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationY(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, System.Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignZ, psAlignZRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationZ(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, System.Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psPoint, psBillboard, psBillboardRotated:
    begin
      // TODO: Should accelerate along the view plane, need the view matrix for that
     // SpreadMatrix:= Matrix_Rotation( RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread) );

     // Particle.Direction:= Matrix_Rotate(SpreadMatrix, Effect.Direction);
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
    psTrail, psTrailZ:
    begin
      // Not supported
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
  end;

  // Initial values of the particles
  Particle.InitialPosition:= Particle.Position;
  Particle.InitialSize    := Particle.Size;
  Particle.InitialColor   := Particle.Color;

  // Store the previous position of the particle
  Particle.PreviousPosition:= Particle.Position;

//  UpdateParticle(Delta, Particle);
end;


//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.CreateLine(const Count: Integer;
  const Min: TVector3f; const Max: TVector3f);
var Index: Integer;
begin
  Assert( Count > 0 );

  Points.Capacity:= Count;
  Points.Count   := Count;

  for Index := 0 to Count - 1 do
  begin
    Points.List[Index].X:= Min.X + (Max.X - Min.X) *  (Index / Count);
    Points.List[Index].Y:= Min.Y + (Max.Y - Min.Y) *  (Index / Count);
    Points.List[Index].Z:= Min.Z + (Max.Z - Min.Z) *  (Index / Count);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.CreateRectangle(const Count: Integer; const ConstrainToSurface: Boolean; const Rect: TRectf);
var Index: Integer;
var VPos: Single;
var VDelta   : Single;
var HPos: Single;
var HDelta   : Single;
begin
  Assert( Count > 0 );

  Points.Capacity:= Count;
  Points.Count   := Count;

  if ConstrainToSurface then
  begin
    VPos:= 0;
    HPos:= 0;

    HDelta:= (Rect.Right  - Rect.Left) / (Count / 4);
    VDelta:= (Rect.Bottom - Rect.Top ) / (Count / 4);

    for Index := 0 to Count - 1 do
    begin
      case Index mod 4 of
        0: // Add point on left edge
        begin

          Points.List[Index].X:= Rect.Left;
          Points.List[Index].Y:= Rect.Top + VPos;
          Points.List[Index].Z:= 0;

        end;
        1: // Add point on right edge
        begin
          Points.List[Index].X:= Rect.Right;
          Points.List[Index].Y:= Rect.Top + VPos;
          Points.List[Index].Z:= 0;

          VPos:=VPos + VDelta;
        end;
        2: // Add point on top edge
        begin
          Points.List[Index].X:= Rect.Left + HPos;
          Points.List[Index].Y:= Rect.Top;
          Points.List[Index].Z:= 0;
        end;
        3:  // Add point on bottom edge
        begin
          Points.List[Index].X:= Rect.Left + HPos;
          Points.List[Index].Y:= Rect.Bottom;
          Points.List[Index].Z:= 0;

          HPos:=HPos + HDelta;
        end;
      end;
    end;
  end else
  begin
    for Index := 0 to Count - 1 do
    begin
      Points.List[Index].X:= RandomInterval( Rect.Left, Rect.Right );
      Points.List[Index].Y:= RandomInterval( Rect.Top , Rect.Bottom);
      Points.List[Index].Z:= 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.CreateBox(const Count: Integer; const ConstrainToSurface: Boolean; const Box: TBoxf);
begin
  Assert( Count > 0 );

  Points.Capacity:= Count;
  Points.Count   := Count;
end;


//------------------------------------------------------------------------------
procedure TPHXParticleEmittor.CreateCircle(const Count: Integer; const ConstrainToSurface: Boolean; const Radius: Single);
var Index: Integer;
var Angle: Single;
var Delta: Single;
var Radius2    : Single;
begin
  Assert( Count > 0 );

  Points.Capacity:= Count;
  Points.Count   := Count;

  Delta:= 2 * PI /  Count;
  Angle:= 0;
  if ConstrainToSurface then
  begin
    for Index := 0 to Count - 1 do
    begin
      Points.List[Index].X:= Radius * Cos( Angle );
      Points.List[Index].Y:= Radius * Sin( Angle );
      Points.List[Index].Z:= 0;

      Angle:= Angle + Delta;
    end;
  end else
  begin
    for Index := 0 to Count - 1 do
    begin
      Radius2:= RandomInterval( -Radius, Radius );

      Points.List[Index].X:= Radius2 * Cos( Angle );
      Points.List[Index].Y:= Radius2 * Sin( Angle );
      Points.List[Index].Z:= 0;

      Angle:= Angle + Delta;
    end;
  end;
end;


{$ENDREGION}
        (*
{$REGION 'TPHXEmissionPoints'}


//------------------------------------------------------------------------------
procedure TPHXEmissionPoints.CreateLine(const Count: Integer; const Min, Max: TVector3f);
var Index: Integer;
begin
  Assert( Count > 0 );

  Self.Capacity:= Count;
  Self.Count   := Count;

  for Index := 0 to Count - 1 do
  begin
    List[Index].X:= Min.X + (Max.X - Min.X) *  (Index / Count);
    List[Index].Y:= Min.Y + (Max.Y - Min.Y) *  (Index / Count);
    List[Index].Z:= Min.Z + (Max.Z - Min.Z) *  (Index / Count);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEmissionPoints.CreateRectangle(const Count: Integer; const ConstrainToSurface: Boolean; const Rect: TRectf);
var Index: Integer;
var VPos: Single;
var VDelta   : Single;
var HPos: Single;
var HDelta   : Single;
begin
  Assert( Count > 0 );

  Self.Capacity:= Count;
  Self.Count   := Count;

  if ConstrainToSurface then
  begin
    VPos:= 0;
    HPos:= 0;

    HDelta:= (Rect.Right  - Rect.Left) / (Count / 4);
    VDelta:= (Rect.Bottom - Rect.Top ) / (Count / 4);

    for Index := 0 to Count - 1 do
    begin
      case Index mod 4 of
        0: // Add point on left edge
        begin

          List[Index].X:= Rect.Left;
//          Points.List[Index].Y:= TPHXParticleRandom.RandomInterval( Rect.Top , Rect.Bottom);
          List[Index].Y:= Rect.Top + VPos;
          List[Index].Z:= 0;

        end;
        1: // Add point on right edge
        begin
          List[Index].X:= Rect.Right;
//          Points.List[Index].Y:= TPHXParticleRandom.RandomInterval( Rect.Top , Rect.Bottom);
          List[Index].Y:= Rect.Top + VPos;
          List[Index].Z:= 0;

          VPos:=VPos + VDelta;
        end;
        2: // Add point on top edge
        begin
          List[Index].X:= Rect.Left + HPos;
        //  Points.List[Index].X:= TPHXParticleRandom.RandomInterval( Rect.Left, Rect.Right );
          List[Index].Y:= Rect.Top;
          List[Index].Z:= 0;
        end;
        3:  // Add point on bottom edge
        begin
          List[Index].X:= Rect.Left + HPos;
      //    Points.List[Index].X:= TPHXParticleRandom.RandomInterval( Rect.Left, Rect.Right );
          List[Index].Y:= Rect.Bottom;
          List[Index].Z:= 0;

          HPos:=HPos + HDelta;
        end;
      end;
    end;
  end else
  begin
    for Index := 0 to Count - 1 do
    begin
      List[Index].X:= TPHXParticleRandom.RandomInterval( Rect.Left, Rect.Right );
      List[Index].Y:= TPHXParticleRandom.RandomInterval( Rect.Top , Rect.Bottom);
      List[Index].Z:= 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEmissionPoints.CreateCircle(const Count: Integer; const ConstrainToSurface: Boolean; const Radius: Single);
var Index: Integer;
var Angle: Single;
var Delta: Single;
var Radius2    : Single;
begin
  Assert( Count > 0 );

  Self.Capacity:= Count;
  Self.Count   := Count;


  Delta:= 2 * PI /  Count;
  Angle:= 0;
  if ConstrainToSurface then
  begin
    for Index := 0 to Count - 1 do
    begin
      List[Index].X:= Radius * Cos( Angle );
      List[Index].Y:= Radius * Sin( Angle );
      List[Index].Z:= 0;

      Angle:= Angle + Delta;
    end;
  end else
  begin
    for Index := 0 to Count - 1 do
    begin
      Radius2:= TPHXParticleRandom.RandomInterval( -Radius, Radius );

      List[Index].X:= Radius2 * Cos( Angle );
      List[Index].Y:= Radius2 * Sin( Angle );
      List[Index].Z:= 0;

      Angle:= Angle + Delta;
    end;
  end;
end;

{$ENDREGION}

    *)
{$REGION 'TPHXParticleEffect'}


// TPHXParticleEffect
//==============================================================================
constructor TPHXParticleEffect.Create(AManager: TPHXParticleManager);
begin
  FManager       := AManager;
  FName          := 'Effect';


 // FEmissionPoints:= TPHXEmissionPoints.Create;

  // Graph for the scale
  FGraphs   := TPHXParticleGraphs.Create;
  FAffectors:= TPHXParticleAffectors.Create(Self);
  FEmittor  := TPHXParticleEmittor.Create(Self);


  LoadDefaults;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleEffect.Destroy;
begin
//  FEmissionPoints.Free;

  FEmittor.Free;
  FGraphs.Free;
  FAffectors.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.Initialize;
var Index: Integer;
begin
  // Initialize the particle graphs
  FGraphs.Initialize;

  // Initialize all affectors for this affect
  for Index:=0 to Affectors.Count-1 do
  begin
    Affectors.List^[Index].Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.LoadDefaults;
begin
  //FEmissionPointsInOrder:= False;

  // Maxinum number of particles.
  FQuota:= 100;
  // The name of the texture for the particles.
  FTexture:= nil;
  // Delay before starting the emission
  FDelay:= 0;

  // The size of the pattern
  FPatternSize:= ps1x1;
  // The index of the pattern to use
  FPatternIndex:= 0;
  // The index of the pattern to use
  FPatternVariance:= 0;
  // The blend mode to use.
  FBlending:= bmAdd;
  // The orientation of the particles
  FShape:= psAlignZ;

  FColor.Value:= clrWhite;
  FColor.Variance:= clrNone;
  FGrayscale:= False;

//  FColorVariance           := Color4f(0,0,0,0);
//  FColorVarianceAsGrayscale:= False;


 // EmissionPoints.Clear;
  // Emission by time or distance traveled
 // EmissionMode:= emTime;
  // The delay between each emission
  //EmissionDelay:= 0.25;
  // Number of particles to emit each emission interval
  //EmissionCount:= 1;


  // Direction
  FDirection:=  Vector3f_AxisX;
  // Minimum and maximum spread to add to the direction in degrees.
  FSpread:= 180;

  // Particle lifetime
  FLife:= TPHXParticleParameter1f.Create(1.0, 0.0);
  // Particle size
  FSize:= TPHXParticleParameter1f.Create(16.0, 0.0);
  // Particle growth
  FGrowth:= TPHXParticleParameter1f.Create(0,0 );
  // Velocity
  FVelocity:= TPHXParticleParameter1f.Create(20.0, 0.0);

  FAcceleration:= Vector3f_Zero;

  FSpin:= TPHXParticleParameter1f.Create(0, 0);

  FLinkToSystem:= False;

  FIdleWhileParentActive:= False;


  FGraphs.Scale.Keys.Clear;
  FGraphs.Scale.Keys.Add(0.0, 1.0);

  FGraphs.Color.Keys.Clear;
  FGraphs.Color.Keys.Add(0.0, clrWhite);

  FGraphs.Alpha.Keys.Clear;
  FGraphs.Alpha.Keys.Add(0.0, 1.0);
  FGraphs.Alpha.Keys.Add(1.0, 0.0);

  FGraphs.Velocity.Keys.Clear;
  FGraphs.Velocity.Keys.Add(0.0, Vector3f_One);

  FGraphs.Spin.Keys.Clear;
  FGraphs.Spin.Keys.Add(0.0, 1.0);

  FGraphs.EmissionCount.Keys.Clear;
  FGraphs.EmissionCount.Keys.Add(0.0, 1.0);


  Initialize;
end;


//------------------------------------------------------------------------------
procedure TPHXParticleEffect.LoadEffect(Reader: TPHXReader);
begin
 (*
  //-- Basic parameters

  ReadString(Stream, FName);

  Stream.Read(FQuota                , SizeOf(FQuota));
  Stream.Read(FDuration             , SizeOf(FDuration));
  Stream.Read(FDelay                , SizeOf(FDelay));

  Stream.Read(FInitalUpdateCount                , SizeOf(FInitalUpdateCount));
  Stream.Read(FInitalUpdateInterval                , SizeOf(FInitalUpdateInterval));

  Stream.Read(FLinkToSystem         , SizeOf(FLinkToSystem));
  Stream.Read(FIdleWhileParentActive, SizeOf(FIdleWhileParentActive));

  //FAffectors.LoadFromStream(Stream);

  //-- Graphs

 // FGraphs.LoadFromStream(Stream);

   //--- Emission parameters


  Stream.Read(FEmissionMode         , SizeOf(FEmissionMode));
  Stream.Read(FEmissionDelay        , SizeOf(FEmissionDelay));
  Stream.Read(FEmissionCount        , SizeOf(FEmissionCount));
  Stream.Read(FEmissionRepeats      , SizeOf(FEmissionRepeats));

  FEmissionPoints.LoadFromStream(Stream);

  Stream.Read(FEmissionPointsInOrder, SizeOf(FEmissionPointsInOrder));
  Stream.Read(FLife                 , SizeOf(FLife));


  //--- Apperance parameters

  ReadString(Stream, FTextureName);

  Stream.Read(FShape            , SizeOf(FShape));
  Stream.Read(FBlending         , SizeOf(FBlending));
  Stream.Read(FPatternSize      , SizeOf(FPatternSize));
  Stream.Read(FPatternIndex     , SizeOf(FPatternIndex));
  Stream.Read(FColor            , SizeOf(FColor));
  Stream.Read(FGrayscale        , SizeOf(FGrayscale));
  Stream.Read(FSize             , SizeOf(FSize));
  Stream.Read(FGrowth           , SizeOf(FGrowth));

  //--- Physics parameters

  Stream.Read(FDirection      , SizeOf(FDirection));
  Stream.Read(FSpread         , SizeOf(FSpread));
  Stream.Read(FVelocity       , SizeOf(FVelocity));
  Stream.Read(FSpin           , SizeOf(FSpin));
  Stream.Read(FAcceleration   , SizeOf(FAcceleration));
*)

  FGraphs.LoadGraphs(Reader);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SaveEffect(Writer: TPHXWriter);
begin
(*

  //-- Basic parameters

  WriteString(Stream, FName);

  Stream.Write(FQuota                , SizeOf(FQuota));
  Stream.Write(FDuration             , SizeOf(FDuration));
  Stream.Write(FDelay                , SizeOf(FDelay));
  Stream.Write(FInitalUpdateCount    , SizeOf(FInitalUpdateCount));
  Stream.Write(FInitalUpdateInterval , SizeOf(FInitalUpdateInterval));
  Stream.Write(FLinkToSystem         , SizeOf(FLinkToSystem));
  Stream.Write(FIdleWhileParentActive, SizeOf(FIdleWhileParentActive));


  //FAffectors.SaveToStream(Stream);

  //-- Graphs

  //FGraphs.SaveToStream(Stream);

   //--- Emission parameters


  Stream.Write(FEmissionMode         , SizeOf(FEmissionMode));
  Stream.Write(FEmissionDelay        , SizeOf(FEmissionDelay));
  Stream.Write(FEmissionCount        , SizeOf(FEmissionCount));
  Stream.Write(FEmissionRepeats      , SizeOf(FEmissionRepeats));

  FEmissionPoints.SaveToStream(Stream);

  Stream.Write(FEmissionPointsInOrder, SizeOf(FEmissionPointsInOrder));
  Stream.Write(FLife                 , SizeOf(FLife));


  //--- Apperance parameters

  WriteString(Stream, FTextureName);

  Stream.Write(FShape            , SizeOf(FShape));
  Stream.Write(FBlending         , SizeOf(FBlending));
  Stream.Write(FPatternSize      , SizeOf(FPatternSize));
  Stream.Write(FPatternIndex     , SizeOf(FPatternIndex));
  Stream.Write(FColor            , SizeOf(FColor));
  Stream.Write(FGrayscale , SizeOf(FGrayscale));
  Stream.Write(FSize             , SizeOf(FSize));
  Stream.Write(FGrowth           , SizeOf(FGrowth));

  //--- Physics parameters

  Stream.Write(FDirection      , SizeOf(FDirection));
  Stream.Write(FSpread         , SizeOf(FSpread));
  Stream.Write(FVelocity       , SizeOf(FVelocity));
  Stream.Write(FSpin           , SizeOf(FSpin));
  Stream.Write(FAcceleration   , SizeOf(FAcceleration));
*)
  FGraphs.SaveGraphs(Writer);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.LoadFromFile(const FileName: String);
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
procedure TPHXParticleEffect.SaveToFile(const FileName: String);
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
procedure TPHXParticleEffect.LoadFromStream(Stream: TStream);
var Header: TPHXParticleHeader;
var Reader: TPHXReader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident  ));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  // Check for a valid file
  if (Header.Ident <> 'PHXPAR') then
  begin
    raise Exception.Create('Not a valid Phoenix particle effect.');
  end;
  // Check the file version
  if (Header.Version <> PHXPARTICLE_VERSION) then
  begin
    raise Exception.Create('Particle effect version missmatch.');
  end;

  Reader:= TPHXReader.Create(Stream);
  try
    LoadEffect(Reader);
  finally
    Reader.Free;
  end;

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SaveToStream(Stream: TStream);
var Header: TPHXParticleHeader;
var Writer: TPHXWriter;
begin
  Header.Ident  :='PHXPAR';
  Header.Version:= PHXPARTICLE_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident  ));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Writer:= TPHXWriter.Create(Stream);
  try
    SaveEffect(Writer);
  finally
    Writer.Free;
  end;
end;
{
// Emits a single particle
//------------------------------------------------------------------------------
procedure TPHXParticleEffect.EmitParticle(System: TPHXParticleSystem; var Particle: TPHXParticle);
var Index       : Integer;
var Velocity    : Single;
var Spread      : Single;
var SpreadMatrix: TMatrix4;
var SpreadVector: TVector3f;
begin
  Particle.Energy       := 0.0;
  Particle.Time         := 0.0;
  Particle.Angle        := 0;
  Particle.Color        := TPHXParticleRandom.RandomInterval(Color);
  Particle.Life         := Life.RandomValue;
  Particle.Size         := Size.RandomValue;
  Particle.Growth       := Growth.RandomValue;
  Particle.Spin         := GetRandomValue(Spin, SpinVariance);
  Particle.Pattern      := GetRandomValue(PatternIndex, PatternVariance) + PatternOffset[PatternSize];

  if Grayscale then
  begin
    Particle.Color.Red  := Particle.Color.Red;
    Particle.Color.Green:= Particle.Color.Red;
    Particle.Color.Blue := Particle.Color.Red;
  end;

  // Pick a initial position from the emission points
  if EmissionPoints.Count > 0 then
  begin

    if EmissionPointsInOrder then
    begin
      Index:= System.EmissionIndex;

      System.EmissionIndex:= (System.EmissionIndex + 1) mod EmissionPoints.Count;
    end else
    // Pick a random point
    begin
      Index:= GetRandomIndex( EmissionPoints.Count);
    end;

    Particle.Position.X:= System.Position.X + EmissionPoints.List^[Index].X;
    Particle.Position.Y:= System.Position.Y + EmissionPoints.List^[Index].Y;
    Particle.Position.Z:= System.Position.Z + EmissionPoints.List^[Index].Z;
  end else
  begin
    Particle.Position.X:= System.Position.X ;
    Particle.Position.Y:= System.Position.Y ;
    Particle.Position.Z:= System.Position.Z ;
  end;
  Particle.Velocity.X:= 0;
  Particle.Velocity.Y:= 0;
  Particle.Velocity.Z:= 0;

  Velocity:= Self.Velocity.RandomValue;
  Spread  := TPHXParticleRandom.RandomInterval(-Self.Spread, Self.Spread);

  // Apply the particle velocity depending on the orientation
  case Shape of
    psAlignX, psAlignXRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationX(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignY, psAlignYRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationY(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignZ, psAlignZRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationZ(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psPoint, psBillboard, psBillboardRotated:
    begin
      // TODO: Should accelerate along the view plane, need the view matrix for that
     // SpreadMatrix:= Matrix_Rotation( RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread) );

     // Particle.Direction:= Matrix_Rotate(SpreadMatrix, Effect.Direction);
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
    psTrail, psTrailZ:
    begin
      // Not supported
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
  end;

  // Initial values of the particles
  Particle.InitialPosition:= Particle.Position;
  Particle.InitialSize    := Particle.Size;
  Particle.InitialColor   := Particle.Color;

  // Store the previous position of the particle
  Particle.PreviousPosition:= Particle.Position;

//  UpdateParticle(Delta, Particle);
end;
        }
//------------------------------------------------------------------------------
function TPHXParticleEffect.GetRenderer: TPHXParticleRenderer;
begin
  Result:= RendererRegistry[Shape];
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetLife(const Value: TPHXParticleParameter1f);
begin
  SetLifeMax(Value.Value);
  SetLifeMin(Value.Variance);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetLifeMax(const Value: Single);
begin
  if Value < 0 then
  begin
    FLife.Variance:= 0;
 end else
  begin
    FLife.Variance:= Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetLifeMin(const Value: Single);
begin
  if Value < 0 then
  begin
    FLife.Value:= 0;
  end else
  begin
    FLife.Value:= Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetDirection(const Value: TVector3f);
begin
  FDirection:= VectorNormalize(Value);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetDirectionX(const Value: Single);
begin
  FDirection.X := Value;

  FDirection:= VectorNormalize(FDirection);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetDirectionY(const Value: Single);
begin
  FDirection.Y := Value;

  FDirection:= VectorNormalize(FDirection);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetDirectionZ(const Value: Single);
begin
  FDirection.Z := Value;

  FDirection:= VectorNormalize(FDirection);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetParent(const Value: TPHXParticleEffect);
begin
  FParent := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffect.SetTexture(const Value: TPHXTexture);
begin
  FTexture := Value;

  if Assigned(FTexture) then
  begin
    FTextureName:= FTexture.Name;
  end else
  begin
    FTextureName:= '';
  end;
end;

procedure TPHXParticleEffect.SetTextureName(const Value: String);
begin
  FTextureName := Value;

  if Assigned(FManager.Textures) then
  begin
    FTexture:= FManager.Textures.Find(FTextureName);
  end;
end;

{$ENDREGION}

{$REGION 'TPHXParticleEffects'}

// TPHXParticleEffects
//==============================================================================
constructor TPHXParticleEffects.Create(AManager: TPHXParticleManager);
begin
  FManager:= AManager;
  FList   := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleEffects.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleEffects.Clear;
var Index: Integer;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    TPHXParticleEffect(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.Add(const Name: String): TPHXParticleEffect;
begin
  Result:= TPHXParticleEffect.Create(FManager);
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.Add(const Effect: TPHXParticleEffect): TPHXParticleEffect;
begin
  Result:= Effect;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.Load(const FileName: String): TPHXParticleEffect;
begin
  Result:= TPHXParticleEffect.Create(FManager);
  Result.LoadFromFile(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.IndexOf(const Name: String): Integer;
var Index: Integer;
var Effect: TPHXParticleEffect;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    Effect:= TPHXParticleEffect(FList.List[Index]);

    if SameText(Effect.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.Find(const Name: String): TPHXParticleEffect;
var Index: Integer;
var Effect: TPHXParticleEffect;
begin
  for Index:= 0 to FList.Count-1 do
  begin
    Effect:= TPHXParticleEffect(FList.List[Index]);

    if SameText(Effect.Name, Name) then
    begin
      Result:= Effect;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXParticleEffects.GetItem(const Index: Integer): TPHXParticleEffect;
begin
  Result:= TPHXParticleEffect(FList.List[Index]);
end;


{$ENDREGION}

{$REGION 'TPHXParticleSystem'}

// TPHXParticleSystem
//==============================================================================
constructor TPHXParticleSystem.Create(AManager: TPHXParticleManager);
begin
  FManager  := AManager;
  FParticles:= TPHXParticleList.Create;
  FRenderer := nil;
  FEffect   := nil;

  FPosition := TVector3f.Zero;
  FDirection:= TVector3f.AxisX;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleSystem.Destroy;
begin
  FParticles.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Initialize;
begin
  Initialize(FEffect);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Initialize(AEffect: TPHXParticleEffect);
var Index: Integer;
begin
  Assert( Assigned(AEffect), 'Assigning an null effect to a particle system is not allowed.');

  FEffect     := AEffect;
  FName       := AEffect.Name;
  FDirection  := AEffect.Direction;
  FRenderer   := AEffect.Renderer;
  FTexture    := AEffect.Texture;
  FInitialized:= True;

  // Copy the texture from the effect
  if Assigned(Effect.Texture) then
  begin
    FTexture:= Effect.Texture;
  end else
  // If the effect has no texture find it in the owner texture list
  if Assigned(FManager) and Assigned(FManager.Textures) then
  begin
    Effect.Texture:= FManager.Textures.Find(Effect.TextureName);
  end;

  FParticles.Capacity:= Effect.Quota;
  FParticles.Count   := 0;

  Reset;

  // Do the initial updating
  for Index:= 1 to Effect.InitalUpdateCount do
  begin
    Update(Effect.InitalUpdateInterval);
  end;

  FAlive := True;
  FActive:= True;
  FTime  := 0;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Reset;
begin
  FAlive           := True;
  FActive          := True;
  FTime            := 0;

  EmissionTime    := 0;
  EmissionDistance:= 0;
  EmissionCounter := Effect.Emittor.Repeats;

  FParticles.Count:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Clear;
begin
  FEffect:= nil;

  FParticles.Count:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.UpdateEmittor(FrameTime: Single);
var Count: Integer;
var Index: Integer;

var UpdateTime: Single;
var UpdateDelta: Single;
begin
  Count:= 0;

  case Effect.Emittor.Mode of
    // Emit by time
    emTime:
    begin
      EmissionTime:= EmissionTime + FrameTime;

      if Effect.Emittor.Delay > 0 then
      begin
        Count:= Trunc(EmissionTime / Effect.Emittor.Delay);
      end else
      begin
        Count:= 1;
      end;
      EmissionTime:= EmissionTime - (Count * Effect.Emittor.Delay);

    end;
    // Emit by distance
    emDistance:
    begin
      EmissionDistance:= EmissionDistance + VectorMagnitude( VectorSub(Position, EmissionPosition) );

      if Effect.Emittor.Delay > 0 then
      begin
        Count:= Trunc(EmissionDistance / Effect.Emittor.Delay);
      end else
      begin
        Count:= 1;
      end;
      EmissionDistance:= EmissionDistance - (Count * Effect.Emittor.Delay);

      EmissionPosition:= FPosition;
    end;
  end;
  // The count contains the number of emissions, multiply with the number of particles
  Count:= Count * Effect.Emittor.Count;

  // No particles to emit
  if Count = 0 then Exit;

  // Limit the maximum number of particles
  if Particles.Count + Count > Particles.Capacity  then
  begin
    Count:= Particles.Capacity - Particles.Count;
  end;

  UpdateTime := 0;
  UpdateDelta:= FrameTime / Count;
  // Emit particles, each particle will be updated by a part of the frametime
  for Index:= Particles.Count to Particles.Count + Count - 1 do
  begin
    Effect.Emittor.Emit(Self, Particles.List^[Index]);
   // EmitParticle(UpdateTime, Particles.List^[Index]);

    UpdateParticle(UpdateTime, Particles.List^[Index]);

    UpdateTime:= UpdateTime + UpdateDelta;
  end;

  Particles.Count:= Particles.Count + Count;
end;


//------------------------------------------------------------------------------
procedure TPHXParticleSystem.UpdateParticle(FrameTime: Single; var Particle: TPHXParticle);
var Graph: Integer;
begin
  Particle.PreviousPosition:= Particle.Position;

  Particle.Time  := Particle.Time + FrameTime;

  if Particle.Life > 0 then
  begin
    Particle.Energy:= (Particle.Time / Particle.Life);
  end else
  begin
    Particle.Energy:= 1.0;
  end;

  // Calculate the graph index
  Graph:= Round(PHXPARTICLE_GRAPH_COUNT * Particle.Energy);
  // Truncate the graph index
  if Graph <  0                       then Graph:= 0;
  if Graph >= PHXPARTICLE_GRAPH_COUNT then Graph:= PHXPARTICLE_GRAPH_COUNT-1;

  Particle.Graph:= Graph;
  // Link the particles to the system or move it
  if Effect.LinkToSystem then
  begin
    Particle.Position.X:= FPosition.X;
    Particle.Position.Y:= FPosition.Y;
    Particle.Position.Z:= FPosition.Z;
  end else
  begin
    Particle.Position.X:= Particle.Position.X + (Particle.Velocity.X * FrameTime) * Effect.Graphs.Velocity.Values^[Graph].X;
    Particle.Position.Y:= Particle.Position.Y + (Particle.Velocity.Y * FrameTime) * Effect.Graphs.Velocity.Values^[Graph].Y;
    Particle.Position.Z:= Particle.Position.Z + (Particle.Velocity.Z * FrameTime) * Effect.Graphs.Velocity.Values^[Graph].Z;

    Particle.Velocity.X:= Particle.Velocity.X + (Effect.Acceleration.X * FrameTime);
    Particle.Velocity.Y:= Particle.Velocity.Y + (Effect.Acceleration.Y * FrameTime);
    Particle.Velocity.Z:= Particle.Velocity.Z + (Effect.Acceleration.Z * FrameTime);
  end;
  // Spin the particle
  Particle.Angle:= Particle.Angle + (Particle.Spin * FrameTime) * Effect.Graphs.Spin.Values^[Graph];
  // Grow the particle
  Particle.Size:= Particle.InitialSize * Effect.Graphs.Scale.Values^[Graph];

  // Calculate the new color
  Particle.Color.Red  := Particle.InitialColor.Red    * Effect.Graphs.Color.Values^[Graph].Red;
  Particle.Color.Green:= Particle.InitialColor.Green  * Effect.Graphs.Color.Values^[Graph].Green;
  Particle.Color.Blue := Particle.InitialColor.Blue   * Effect.Graphs.Color.Values^[Graph].Blue;
  Particle.Color.Alpha:= Particle.InitialColor.Alpha  * Effect.Graphs.Alpha.Values^[Graph];
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.UpdateParticles(FrameTime: Single);
var Index   : Integer;
var Particle: PPHXParticle;
//var Graph   : Integer;
begin
  Index:= 0;

  while Index < Particles.Count  do
  begin
    Particle:= @Particles.List^[Index];

    Particle.Time:= Particle.Time + FrameTime;

    // Remove the particle if its dead
    if (Particle.Life > 0) and (Particle.Time >= Particle.Life) then
    begin
      Particles.Delete(Index);
    end else
    begin
      UpdateParticle(FrameTime, Particle^);

      Inc(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Update(const FrameTime: Single);
begin
  Assert( Assigned(FEffect), 'Updating a particle system without a effect is not allowed.');

  Assert(FInitialized, 'You must initialize the effect first');

  if Alive then
  begin
    FTime:= FTime + FrameTime;

    // Attach to the parent position
    if Assigned(Parent) then
    begin
      FPosition:= FParent.Position;
    end;

    // Check if the time has passed the emission time
    Active:= Active and ((Effect.Emittor.Repeats = 0) or ( EmissionCounter > 0 ));

    // Update the emittor, this will call Emit for all the needed particles
    if Active then
    begin
      // Update all existing particles
      UpdateParticles(FrameTime);
      // Emit new particles
      UpdateEmittor(FrameTime);
      // Call the affectors for all particles
      Effect.Affectors.Update(Self, FrameTime);

      CalculateBounds;
    end;

    Alive:= Active or (Particles.Count > 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.Render(Canvas: TPHXCanvas);
begin
  Assert( Assigned(FRenderer), 'Renderer might not be null');

  Assert(FInitialized, 'You must initialize the effect first');

  if Alive and (Particles.Count > 0) then
  begin
    Canvas.Primitive:= PHX_TRIANGLES;
    Canvas.Texture  := FEffect.Texture;

   //  Blending:= Canvas.Blending;
     case Effect.Blending of
      bmNone : Canvas.Blending:= TPHXBlendMode.bmNormal;
      bmAdd  : Canvas.Blending:= TPHXBlendMode.bmAdditive;
      bmSub  : Canvas.Blending:= TPHXBlendMode.bmMultiply;
      bmAlpha: Canvas.Blending:= TPHXBlendMode.bmAlpha;
    end;

    FRenderer(Particles, Canvas, nil);

//    Canvas.Flush;
//    Canvas.Blending:= Blending;
  end;
end;


     {
// Emits a single particle
//------------------------------------------------------------------------------
procedure TPHXParticleSystem.EmitParticle(const Delta: Single; var Particle: TPHXParticle);
var Index       : Integer;
var Velocity    : Single;
var Spread      : Single;
var SpreadMatrix: TMatrix4;
var SpreadVector: TVector3f;
begin
  Particle.Time         := 0;
  Particle.Angle        := 0;
  Particle.Color        := TPHXParticleRandom.RandomInterval(Effect.Color);
  Particle.Life         := Effect.Life.RandomValue;
  Particle.Size         := Effect.Size.RandomValue;
  Particle.Growth       := Effect.Growth.RandomValue;
  Particle.Spin         := Effect.Spin.RandomValue;
  Particle.Pattern      := GetRandomValue(Effect.PatternIndex, Effect.PatternVariance) + PatternOffset[Effect.PatternSize];

  if Effect.Grayscale then
  begin
    Particle.Color.Red  := Particle.Color.Red;
    Particle.Color.Green:= Particle.Color.Red;
    Particle.Color.Blue := Particle.Color.Red;
  end;

  // Pick a initial position from the emission points
  if Effect.EmissionPoints.Count > 0 then
  begin

    if Effect.EmissionPointsInOrder then
    begin
      Index:= EmissionIndex;

      EmissionIndex:= (EmissionIndex + 1) mod Effect.EmissionPoints.Count;
    end else
    // Pick a random point
    begin
      Index:= GetRandomIndex( Effect.EmissionPoints.Count);
    end;

    Particle.Position.X:= Position.X + Effect.EmissionPoints.List^[Index].X;
    Particle.Position.Y:= Position.Y + Effect.EmissionPoints.List^[Index].Y;
    Particle.Position.Z:= Position.Z + Effect.EmissionPoints.List^[Index].Z;
  end else
  begin
    Particle.Position.X:= Position.X ;
    Particle.Position.Y:= Position.Y ;
    Particle.Position.Z:= Position.Z ;
  end;
  Particle.Velocity.X:= 0;
  Particle.Velocity.Y:= 0;
  Particle.Velocity.Z:= 0;

  Velocity:= Effect.Velocity.RandomValue;
  Spread  := TPHXParticleRandom.RandomInterval(-Effect.Spread, Effect.Spread);

  // Apply the particle velocity depending on the orientation
  case Effect.Shape of
    psAlignX, psAlignXRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationX(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignY, psAlignYRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationY(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psAlignZ, psAlignZRotated:
    begin
      SpreadMatrix:= Matrix_CreateRotationZ(Spread);
      SpreadVector:= Matrix_Rotate(SpreadMatrix, Direction);

      Particle.Velocity.X:= Velocity * SpreadVector.X ;
      Particle.Velocity.Y:= Velocity * SpreadVector.Y;
      Particle.Velocity.Z:= Velocity * SpreadVector.Z;
    end;
    psPoint, psBillboard, psBillboardRotated:
    begin
      // TODO: Should accelerate along the view plane, need the view matrix for that
     // SpreadMatrix:= Matrix_Rotation( RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread) );

     // Particle.Direction:= Matrix_Rotate(SpreadMatrix, Effect.Direction);
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
    psTrail, psTrailZ:
    begin
      // Not supported
      Particle.Velocity.X:= 0;
      Particle.Velocity.Y:= 0;
      Particle.Velocity.Z:= 0;
    end;
  end;

  // Initial values of the particles
  Particle.InitialPosition:= Particle.Position;
  Particle.InitialSize    := Particle.Size;
  Particle.InitialColor   := Particle.Color;

  // Store the previous position of the particle
  Particle.PreviousPosition:= Particle.Position;

  UpdateParticle(Delta, Particle);
end;

       }

// Update the bounding box of the particle system
//------------------------------------------------------------------------------
procedure TPHXParticleSystem.CalculateBounds;
var Index   : Integer;
var Position: TVector3f;
var Size    : Single;
begin
  FBounds.MinX:= FPosition.X;
  FBounds.MaxX:= FPosition.X;

  FBounds.MinY:= FPosition.Y;
  FBounds.MaxY:= FPosition.Y;

  FBounds.MinZ:= FPosition.Z;
  FBounds.MaxZ:= FPosition.Z;

  for Index := 0 to Particles.Count - 1 do
  begin
    Position:= Particles.List^[Index].Position;
    Size    := Particles.List^[Index].Size * 0.5;

    if Position.X - Size < FBounds.MinX then FBounds.MinX:= Position.X - Size;
    if Position.X + Size > FBounds.MaxX then FBounds.MaxX:= Position.X + Size;

    if Position.Y - Size < FBounds.MinY then FBounds.MinY:= Position.Y - Size;
    if Position.Y + Size > FBounds.MaxY then FBounds.MaxY:= Position.Y + Size;

    if Position.Z - Size < FBounds.MinZ then FBounds.MinZ:= Position.Z - Size;
    if Position.Z + Size > FBounds.MaxZ then FBounds.MaxZ:= Position.Z + Size;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystem.SetEffect(const Value: TPHXParticleEffect);
begin
  if FEffect <> Value then
  begin
    FEffect:= Value;

    FInitialized:= False;
  end;
end;


{$ENDREGION}

{$REGION 'TPHXParticleSystems'}

// TPHXParticleSystems
//==============================================================================
constructor TPHXParticleSystems.Create(AManager: TPHXParticleManager);
begin
  FManager := AManager;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXParticleSystems.Destroy;
begin
  inherited;
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystems.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
function TPHXParticleSystems.Add: TPHXParticleSystem;
begin
  if FCount < FCapacity then
  begin
    Result:= FList^[FCount];

    Inc(FCount);
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystems.SetCapacity(const ACapacity: Integer);
var Index: Integer;
begin
  if FCapacity <> ACapacity then
  begin

    // Free removed systems
    if ACapacity < FCapacity then
    begin
      for Index:= ACapacity to FCapacity-1 do
      begin
        FList^[Index].Free;
        FList^[Index]:= nil;
      end;
    end;

    ReallocMem(FList, ACapacity * SizeOf(TPHXParticleSystem));

    // Create new systems
    for Index:= FCapacity to ACapacity-1 do
    begin
      FList^[Index]:= TPHXParticleSystem.Create(FManager);
    end;

    FCapacity:= ACapacity;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleSystems.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;

    if(FCount > FCapacity) then SetCapacity(FCount);
  end;
end;

//------------------------------------------------------------------------------
function TPHXParticleSystems.GetItem(const Index: Integer): TPHXParticleSystem;
begin
  Result:= FList^[Index];
end;


{$ENDREGION}

{$REGION 'TPHXParticleManager'}


// TPHXParticleManager
//==============================================================================
constructor TPHXParticleManager.Create;
begin
  FEffects:= TPHXParticleEffects.Create(Self);
  FSystems:= TPHXParticleSystems.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXParticleManager.Destroy;
begin
  FEffects.Free;
  FSystems.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleManager.Clear;
begin
  Systems.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleManager.Update(const DeltaTime: Single);
var Index : Integer;
var System: TPHXParticleSystem;
begin
  for Index:= 0 to Systems.Count - 1 do
  begin
    System:= Systems.List^[Index];

    System.Update(DeltaTime);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXParticleManager.Render(Canvas: TPHXCanvas);
var Index : Integer;
var System: TPHXParticleSystem;
begin
  for Index:= 0 to Systems.Count - 1 do
  begin
    System:= Systems.List^[Index];

    System.Render(Canvas);
  end;
end;

//------------------------------------------------------------------------------
function TPHXParticleManager.Spawn(const Effect: TPHXParticleEffect): TPHXParticleSystem;
begin
  if Assigned(Effect) then
  begin
    Result:= Systems.Add;

    if Assigned(Result) then
    begin
      Result.Effect:= Effect;
    end;
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXParticleManager.Spawn(const Effect: String): TPHXParticleSystem;
begin
  Result:= Spawn(Effects.Find(Effect));
end;

//------------------------------------------------------------------------------
procedure TPHXParticleManager.SetQuota(const Value: Integer);
begin
  if FQuota <> Value then
  begin
    FQuota:= Value;

    FSystems.SetCapacity(FQuota);
  end;
end;




{$ENDREGION}





initialization
  RandomTableInit;
finalization
end.
