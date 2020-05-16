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
unit phxSprite_Box2D;
//< Physic sprites using Box2D physics

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils, Math,

  phxLogger,
  phxTypes,
  phxMath,
  phxDevice,
  phxShape,
  phxSprite,

  UPhysics2D,
  UPhysics2DTypes;

const
  TimeStep: Double = 1 / 60;

type

// Body kind for sprites
//------------------------------------------------------------------------------
TPHXBodyKind = (
  bkStatic,
  bkDynamic,
  bkKinematic
);

// Phoenix sprite using Box2D physics
//------------------------------------------------------------------------------
TPHXPhysicsSprite = class(TPHXSprite)
  private
    FPhysicsBody : Tb2Body;

    FKind       : TPHXBodyKind;
    FDensity    : Double;
    FFriction   : Double;
    FRestitution: Double;

    // Create the Box2D body for this sprite
    procedure CreatePhysicsBody;

    function GetB2Position(const Position: TVector2f): TVector2;

    function GetPhysicsWorld: Tb2World;
  public
    // Creates a new sprite and adds it to the engine
    constructor Create(AEngine: TPHXSpriteEngine); override;
    // Destroy the sprite
    destructor Destroy; override;

    // Initialize the physics sprite
    procedure Initialize; override;
    // Update the physics sprite
    procedure Update(const DeltaTime: Double); override;

    // Apply an impulse at the sprite center;
    // Apply an impulse at the sprite center;
    procedure ApplyImpulse(const Impulse: TVector2d); overload;
    // Apply an impulse at a point
    procedure ApplyImpulse(const Impulse: TVector2d; const Position: TVector2f); overload;
    // Apply a explosion impulse to the sprites
    procedure ApplyExplosion(const Position: TVector2f; const Power: Double; const Radius: Single);

    // Converts a sprite position to a box2d position
    function SpriteToBody(const Position: TVector2f): TVector2;
    // Converts a box2d position to a sprite position
    function BodyToSprite(const Position: TVector2): TVector2f;

    // Return a clone of this sprite
    function Clone: TPHXSprite; override;
    // Copy all properties from another sprite to this sprite
    procedure Assign(Sprite: TPHXSprite); override;

    // The body kind
    property Kind: TPHXBodyKind read FKind write FKind;
    // The density, usually in kg/m^2.
    property Density: Double read FDensity write FDensity;
    // The friction coefficient, usually in the range [0,1].
    property Friction: Double read FFriction write FFriction;
    // The restitution (elasticity) usually in the range [0,1].
    property Restitution: Double read FRestitution write FRestitution;

    // The Box2D Body of the sprite
    property PhysicsBody: Tb2Body read FPhysicsBody;
    // The Box2D World of the sprite
    property PhysicsWorld: Tb2World read GetPhysicsWorld;
  end;

// Physics engine implementing Box2D physics
//------------------------------------------------------------------------------
TPHXPhysicsEngine = class(TPHXSpriteEngine)
  private
    FPhysicsWorld: Tb2World;
    // Time buffer for fixed timesteps
    FPhysicsTime: Double;

    FGravity : TVector2f;

    FScale   : Single;
    FScaleInv: Single;

    procedure SetScale(const Value: Single);
  public
    constructor Create(ADevice: TPHXDevice); override;
    destructor Destroy; override;

    // Initialize the physics engine
    procedure Initialize; override;
    // Update the physics engine
    procedure Update(const FrameTime: Double); override;

    // Apply an impulse at the sprite center;
    procedure ApplyImpulse(const Impulse: TVector2d); overload;
    // Apply an impulse at a point
    procedure ApplyImpulse(const Impulse: TVector2d; const Position: TVector2f); overload;
    // Apply a explosion impulse to the sprites
    procedure ApplyExplosion(const Position: TVector2f; const Power: Double; const Radius: Double);

    // Gravity of the world
    property Gravity: TVector2f read FGravity write FGravity;
    // Scale of the world this is the size of a pixel
    property Scale: Single read FScale write SetScale;
    // Inverted scale
    property ScaleInv: Single read FScaleInv;


    // The Box2D World
    property PhysicsWorld: Tb2World read FPhysicsWorld;
  end;

// Creates a box2D shape for a phoenix shape
function CreateShape(Sprite: TPHXPhysicsSprite; const Scale: Single): Tb2Shape;

implementation

// Tb2CircleShape
//------------------------------------------------------------------------------
function CreateShape(Sprite: TPHXPhysicsSprite; const Scale: Single): Tb2Shape;
var Index: Integer;
var ScaleInv: Single;
var Shape  : TPHXShape;
var Polygon: TPHXPolygon;
var Circle : TPHXCircle;

var b2Polygon : Tb2PolygonShape;
var b2Vertices: array of TVector2;
begin
  Shape:= Sprite.LinkedShape;

  ScaleInv:= 1 / Scale;

  if Assigned(Shape) then
  begin
    case Shape.Kind of
      //PHX_SHAPE_POINT: ;
      //PHX_SHAPE_BOX: ;
      //PHX_SHAPE_LINE: ;

      PHX_SHAPE_POLYGON:
      begin
        Polygon:= TPHXPolygon(Shape);

        SetLength(b2Vertices, Polygon.Size);
        try

          Result:= Tb2PolygonShape.Create;
          for Index := 0 to Polygon.Size-1 do
          begin
            b2Vertices[Index].x:= Polygon.List^[Index].X;
            b2Vertices[Index].y:= Polygon.List^[Index].y;
          end;
           Tb2PolygonShape(Result).SetVertices(@b2Vertices[0], Polygon.Size);

        finally
          SetLength(b2Vertices, 0);
        end;

      end;
      PHX_SHAPE_CIRCLE:
      begin
        Circle:= TPHXCircle(Shape);

        Result:= Tb2CircleShape.Create;

        Tb2CircleShape(Result).m_radius:= Circle.Radius / Scale;
      end;
      else
      begin
        Result:= Tb2PolygonShape.Create;

        Tb2PolygonShape(Result).SetAsBox(Sprite.Width * 0.5 / Scale, Sprite.Height * 0.5 / Scale);
      end;
    end;
  end else
  // No shape assigned, create a box shape for the sprite
  begin
    b2Polygon:= Tb2PolygonShape.Create;

    SetLength(b2Vertices, 4);
    try
      b2Vertices[0].y:= Sprite.LocalBounds.Top  * ScaleInv;
      b2Vertices[0].x:= Sprite.LocalBounds.Left * ScaleInv;

      b2Vertices[1].y:= Sprite.LocalBounds.Top   * ScaleInv;
      b2Vertices[1].x:= Sprite.LocalBounds.Right* ScaleInv;

      b2Vertices[2].y:= Sprite.LocalBounds.Bottom * ScaleInv;
      b2Vertices[2].x:= Sprite.LocalBounds.Right  * ScaleInv;

      b2Vertices[3].y:= Sprite.LocalBounds.Bottom * ScaleInv;
      b2Vertices[3].x:= Sprite.LocalBounds.Left   * ScaleInv;

      b2Polygon.SetVertices(@b2Vertices[0], 4);
    finally
      SetLength(b2Vertices, 0);
    end;
    Result:= b2Polygon;


   // Tb2PolygonShape(Result).SetAsBox(Sprite.Width * 0.5 / Scale, Sprite.Height * 0.5 / Scale);
  end;
end;


// TPHXPhysicsSprite
//==============================================================================
constructor TPHXPhysicsSprite.Create(AEngine: TPHXSpriteEngine);
begin
  Assert(AEngine is TPHXPhysicsEngine, 'To create a physic sprite the Engine must be a TPHXPhysicsEngine');

  inherited Create(AEngine);

  FKind      := bkStatic;
  FDensity    := 10.0;
  FFriction   := 0.3;
  FRestitution:= 0.0;
end;

//------------------------------------------------------------------------------
destructor TPHXPhysicsSprite.Destroy;
begin
  if Assigned(FPhysicsBody) then
  begin
    PhysicsWorld.DestroyBody(FPhysicsBody, False);
  end;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.CreatePhysicsBody;
var BodyDef: Tb2BodyDef;
var Shape  : Tb2Shape;
var fd     : Tb2FixtureDef;
begin
  if Assigned(FPhysicsBody) then
  begin
    PhysicsWorld.DestroyBody(FPhysicsBody, False);
  end;

  Shape:= CreateShape(Self, TPHXPhysicsEngine(Engine).Scale);

  Assert(Assigned(Shape), Name );

  bodyDef := Tb2BodyDef.Create;
  case Kind of
    bkStatic   : bodyDef.bodyType := b2_staticBody;
    bkDynamic  : bodyDef.bodyType := b2_dynamicBody;
    bkKinematic: bodyDef.bodyType := b2_kinematicBody;
  end;
  bodyDef.position.X:= Position.X * TPHXPhysicsEngine(Engine).FScaleInv;
  bodyDef.position.Y:= Position.Y * TPHXPhysicsEngine(Engine).FScaleInv;

  FPhysicsBody:= PhysicsWorld.CreateBody(bodyDef);

  fd := Tb2FixtureDef.Create;
  fd.shape      := Shape;
  fd.density    := FDensity;
  fd.friction   := FFriction;
  fd.restitution:= FRestitution;

  // Add the shape to the body.
  FPhysicsBody.CreateFixture(fd);
end;

//------------------------------------------------------------------------------
function TPHXPhysicsSprite.SpriteToBody(const Position: TVector2f): TVector2;
begin
  Result.X:= Position.X * TPHXPhysicsEngine(Engine).FScaleInv;
  Result.Y:= Position.Y * TPHXPhysicsEngine(Engine).FScaleInv;
end;

//------------------------------------------------------------------------------
function TPHXPhysicsSprite.BodyToSprite(const Position: TVector2): TVector2f;
begin
  Result.X:= Position.X * TPHXPhysicsEngine(Engine).FScale;
  Result.Y:= Position.Y * TPHXPhysicsEngine(Engine).FScale;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.Initialize;
begin
  inherited;

  // Already initialized
  if (PhysicsBody <> nil) or (PhysicsWorld = nil) then Exit;

  CreatePhysicsBody;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.Update(const DeltaTime: Double);
var b2Position: TVector2;
var b2Angle   : PhysicsFloat;
begin
  inherited;

  if Active then
  begin
    Assert( FPhysicsBody <> nil, 'You must initialize the physics sprite first.');

    b2Position:= FPhysicsBody.GetPosition;
    b2Angle   := FPhysicsBody.GetAngle * RAD_TO_DEG;

    b2Position.x:= b2Position.x * TPHXPhysicsEngine(Engine).FScale;
    b2Position.y:= b2Position.y * TPHXPhysicsEngine(Engine).FScale;

    MoveTo( Vector3f(b2Position.x, b2Position.y, Layer), b2Angle);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.ApplyImpulse(const Impulse: TVector2d);
var b2Impulse: TVector2;
begin
  Assert( FPhysicsBody <> nil, 'You must initialize the physics engine first');

  b2Impulse.x:= Impulse.X;
  b2Impulse.y:= Impulse.y;

  FPhysicsBody.ApplyLinearImpulse(b2Impulse, FPhysicsBody.GetWorldCenter);
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.ApplyImpulse(const Impulse: TVector2d; const Position: TVector2f);
var b2Impulse: TVector2;
var b2Point  : TVector2;
begin
  Assert( FPhysicsBody <> nil, 'You must initialize the physics engine first');

  b2Point.x:= Position.X * TPHXPhysicsEngine(Engine).FScaleInv;
  b2Point.y:= Position.Y * TPHXPhysicsEngine(Engine).FScaleInv;

  b2Impulse.x:= Impulse.X;
  b2Impulse.y:= Impulse.y;

  FPhysicsBody.ApplyLinearImpulse(b2Impulse, b2Point);
end;


//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.ApplyExplosion(const Position: TVector2f; const Power: Double; const Radius: Single);
var b2Point    : TVector2;
var b2Direction: TVector2;
var b2Distance : Single;
var b2Scale    : Single;
var b2Impulse  : TVector2;
begin
  b2Point:= GetB2Position(Position);

  b2Direction:= FPhysicsBody.GetWorldCenter - b2Point;
  b2Distance := b2Direction.Length;

  if b2Distance <= Radius then
  begin
    if b2Distance <> 0 then
    begin
      b2Scale:= 1 / b2Distance;

      // Normalize the direction vector
      b2Direction.X:= b2Direction.X / b2Distance;
      b2Direction.Y:= b2Direction.Y / b2Distance;
    end else
    begin
      b2Scale:= 1;

      b2Direction.x:= RandomFloat;
      b2Direction.y:= RandomFloat;
    end;

    b2Impulse.x:= (b2Direction.X * Power) * b2Scale;
    b2Impulse.y:= (b2Direction.y * Power) * b2Scale;

    FPhysicsBody.ApplyLinearImpulse(b2Impulse, FPhysicsBody.GetWorldCenter);
  end;
end;

//------------------------------------------------------------------------------
function TPHXPhysicsSprite.Clone: TPHXSprite;
begin
  Result:= TPHXPhysicsSprite.Create(Engine);
  Result.Assign(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsSprite.Assign(Sprite: TPHXSprite);
begin
  inherited Assign(Sprite);

  if Sprite is TPHXPhysicsSprite then
  begin
    FKind       := TPHXPhysicsSprite(Sprite).FKind;
    FDensity    := TPHXPhysicsSprite(Sprite).FDensity;
    FFriction   := TPHXPhysicsSprite(Sprite).FFriction;
    FRestitution:= TPHXPhysicsSprite(Sprite).FRestitution;
  end;

end;

//------------------------------------------------------------------------------
function TPHXPhysicsSprite.GetB2Position(const Position: TVector2f): TVector2;
begin
  Result.x:= Position.X * TPHXPhysicsEngine(Engine).FScaleInv;
  Result.y:= Position.Y * TPHXPhysicsEngine(Engine).FScaleInv;
end;

//------------------------------------------------------------------------------
function TPHXPhysicsSprite.GetPhysicsWorld: Tb2World;
begin
  Result:= TPHXPhysicsEngine(Engine).PhysicsWorld;
end;

 // TPHXPhysicsEngine
//==============================================================================
constructor TPHXPhysicsEngine.Create(ADevice: TPHXDevice);
begin
  inherited Create(ADevice);

  FGravity.X:= 0;
  FGravity.Y:= 10;

  // 1m = 32 pixels
  FScale   := 32;
  FScaleInv:= 1 / 32;

  FPhysicsTime := 0.0;
  FPhysicsWorld:= nil;



  // Define another box shape for our dynamic body.
 // shapeDef := Tb2PolygonShape.Create;
 // shapeDef.SetAsBox(1.0, 1.0);

end;

//------------------------------------------------------------------------------
destructor TPHXPhysicsEngine.Destroy;
begin
  inherited;

  if Assigned(FPhysicsWorld) then
  begin
    FPhysicsWorld.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.Initialize;
var b2Gravity: TVector2;
begin
  Assert(FInitialized = False);

  b2Gravity.x:= FGravity.X;
  b2Gravity.y:= FGravity.Y;

  FPhysicsWorld:= Tb2World.Create(b2Gravity);

  inherited Initialize;

  // Already initialized
//  if Assigned(FPhysicsWorld) then Exit;
end;


//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.Update(const FrameTime: Double);
begin
  Assert( FPhysicsWorld <> nil, 'You must initialize the physics engine first');

  FPhysicsTime:= FPhysicsTime + FrameTime;

  while FPhysicsTime > TimeStep do
  begin
    FPhysicsWorld.Step(TimeStep, 6, 2);

    FPhysicsTime:= FPhysicsTime - TimeStep;
  end;
//  FPhysicsWorld.Step(1.0 / 60.0, 6, 2);


  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.ApplyImpulse(const Impulse: TVector2d);
var Index  : Integer;
var Sprite: TPHXSprite;
begin
  for Index:= 0 to Sprites.Count - 1 do
  begin
    Sprite:= TPHXSprite( Sprites.List[Index] );

    if Sprite is TPHXPhysicsSprite then
    begin
      TPHXPhysicsSprite(Sprite).ApplyImpulse(Impulse);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.ApplyImpulse(const Impulse: TVector2d; const Position: TVector2f);
var Index  : Integer;
var Sprite: TPHXSprite;
begin
  for Index:= 0 to Sprites.Count - 1 do
  begin
    Sprite:= TPHXSprite( Sprites.List[Index] );

    if Sprite is TPHXPhysicsSprite then
    begin
      TPHXPhysicsSprite(Sprite).ApplyImpulse(Impulse, Position);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.ApplyExplosion(const Position: TVector2f; const Power: Double; const Radius: Double);
var Index  : Integer;
var Sprite: TPHXSprite;
begin
  for Index:= 0 to Sprites.Count - 1 do
  begin
    Sprite:= TPHXSprite( Sprites.List[Index] );

    if Sprite is TPHXPhysicsSprite then
    begin
      TPHXPhysicsSprite(Sprite).ApplyExplosion(Position, Power, Radius);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPhysicsEngine.SetScale(const Value: Single);
begin
  if Value > 0 then
  begin
    FScale   := Value;
    FScaleInv:= 1 / Value;
  end;
end;

       {

            // Use this method to keep principle of momentum conservation
            body1.ApplyLinearImpulse(lin_impulse, body1.GetWorldCenter);
            {$IFDEF OP_OVERLOAD}
          //  body2.ApplyLinearImpulse(-lin_impulse, body2.GetWorldCenter);


end.
