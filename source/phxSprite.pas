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
unit phxSprite;
{< @abstract(Sprite engine)

}

interface

{$I phxConfig.inc}

uses
  Classes, Types, SysUtils, Math,

  phxLogger,
  phxTypes,
  phxMath,
  phxDevice,
  phxCanvas,
  phxImage,
  phxFont,
  phxShape;

type

// Collision groups for collision filtering
//------------------------------------------------------------------------------
TPHXSpriteCollisionGroup = (
  // The sprite belongs to collision group 1
  cgGroup1,
  // The sprite belongs to collision group 2
  cgGroup2,
  // The sprite belongs to collision group 3
  cgGroup3,
  // The sprite belongs to collision group 4
  cgGroup4,
  // The sprite belongs to collision group 5
  cgGroup5,
  // The sprite belongs to collision group 6
  cgGroup6,
  // The sprite belongs to collision group 7
  cgGroup7,
  // The sprite belongs to collision group 8
  cgGroup8
);

// Filter for masking collisions
TPHXSpriteCollisionFilter = set of TPHXSpriteCollisionGroup;

// The collision mode for a sprite
//------------------------------------------------------------------------------
TPHXSpriteCollisionMode = (
  // The sprite is static, it will be tested for collisions but the collided
  // function will never be called
  cmStatic,
  // The sprite is a dynamic sprite it both generates and recieves collisions
  cmDynamic
);

const

// Constant for no group
cfFilterNone: TPHXSpriteCollisionFilter = [];
// Constant for all groups
cfFilterAll : TPHXSpriteCollisionFilter = [cgGroup1, cgGroup2, cgGroup3, cgGroup4, cgGroup5, cgGroup6, cgGroup7, cgGroup8];

type

// Forward declarations
TPHXSprite           = class;
TPHXSpriteList       = class;
TPHXSpriteEngine     = class;
TPHXSpriteCollider   = class;
TPHXSpriteCollisionList = class;

// Exception class for sprites
EPHXSpriteError = class(Exception);

{$REGION 'TPHXSprite'}

// @abstract(Basic sprite)
//------------------------------------------------------------------------------
TPHXSprite = class(TObject)
  private
    FEngine : TPHXSpriteEngine;
    FParent : TPHXSprite;
    FSprites: TPHXSpriteList;

    FName    : String;
    FTime    : Double;
    FVisible : Boolean;
    FAlive   : Boolean;
    FActive  : Boolean;

    FCollider: Boolean;
    FMode    : TPHXSpriteCollisionMode;
    FGroup   : TPHXSpriteCollisionGroup;

  	FPosition: TVector3f;
    FRotation: Single;

    FColor  : TColor4f;
    FImage  : String;
    FPattern: String;
    FShape  : String;

    //FCollisionGroup : TPHXSpriteGroup;
    //FCollisionFilter: TPHXSpriteGroups;
    //FCollisionData  : Pointer;

    // Relative transformation between the parent and this entiry
    FLocalTransform: TMatrix4f;
    // Absolute transformation in the world
    FFinalTransform: TMatrix4f;

    // Bounding box in releative sprite coordinates
    FLocalBounds: TRectf;
    // Bounding box in absolute world coordinates
    FFinalBounds: TRectf;

    // Shape in releative sprite coordinates
    FLocalShape: TPHXShape;
    // Shape in absolute world coordinates
    FFinalShape: TPHXShape;


    FLinkedImage  : TPHXImage;
    FLinkedPattern: TPHXPatternIndex;
    FLinkedShape  : TPHXShape;

    // The previous position, used to calculate displacement
    FPreviousPosition: TVector3f;

    // True when the transformation matrix needs updating
    TransformRequired: Boolean;

    // Find the linked image and pattern
    procedure FindLinked;
    // Update the size from the image and pattern
    procedure UpdateBounds;

    function GetDisplacement: TVector2f;
    function GetWidth: Integer;
    function GetHeight: Integer;

    procedure SetParent   (const Value: TPHXSprite);
    procedure SetPosition (const Value: TVector3f);
    procedure SetPositionX(const Value: Single);
    procedure SetPositionY(const Value: Single);
    procedure SetPositionZ(const Value: Single);
    procedure SetRotation (const Value: Single);
    procedure SetBounds   (const Value: TRectf);

    procedure SetCollider (const Value: Boolean);

    procedure SetImage(const Value: String); overload;
    procedure SetImageL(const Value: TPHXImage); overload;

    procedure SetPattern(const Value: String); overload;
    procedure SetPatternL(const Value: TPHXPatternIndex); overload;

    procedure SetShape(const Value: String); overload;
    procedure SetShapeL(const Value: TPHXShape); overload;
  public
    // Creates a new sprite and adds it to the engine
    constructor Create(AEngine: TPHXSpriteEngine); virtual;
    // Destroy the sprite
    destructor Destroy; override;

    // Initialize the sprite
    procedure Initialize; virtual;
    // Transform the sprite
    procedure Transform;
    // Update the sprite
    procedure Update(const DeltaTime: Double); virtual;
    // Render the sprite
    procedure Render; virtual;
    // Kill the sprite, the sprite is destroyed by the RemoveDead function in the engine
    procedure Kill;

    // Render the bounding box for this sprite
    procedure RenderBounds(Canvas: TPHXCanvas; Color: TColor4f);

    // Called when this sprite is clicked
    procedure Clicked(const X,Y: Integer); virtual;
    // Called when this sprite collides with another sprite
    procedure Collided(Sprite: TPHXSprite); virtual;
    // Called when this sprite is killed
    procedure Killed; virtual;

    // Returns true if the mouse is inside the entity bounding box
    function PointInSprite(const Point: TVector2f): Boolean;

    // Test if this sprite collides in the future
    // @param(Displacement The distance in the future to test up to)
    // @returns(True if the sprite will collide, else false)
    function Collide(const Displacement: TVector2f): Boolean; overload;
    // Test if this sprite collides in the future
    // @param(Displacement The distance in the future to test up to)
    // @param(Group The collision groups to test collisions against)
    // @returns(True if the sprite will collide, else false)
    function Collide(const Displacement: TVector2f; const Group: TPHXSpriteCollisionGroup): Boolean; overload;
    // Test if this sprite collides in the future
    // @param(Displacement The distance in the future to test up to)
    // @param(Filter The set of collision groups to test in)
    // @returns(True if the sprite will collide, else false)
    function Collide(const Displacement: TVector2f; const Filter: TPHXSpriteCollisionFilter): Boolean; overload;

    // Transform a vector using the entitys transformation matrix
    function SpriteToWorld(const Vector: TVector3f): TVector3f; overload;
    // Transform a vector using the entitys transformation matrix
    function SpriteToWorld(const Vector: TVector2f): TVector2f; overload;

    // Transform a screen coordinate to a entity coordinate
    function WorldToSprite(const Vector: TVector3f): TVector3f; overload;
    // Transform a screen coordinate to a entity coordinate
    function WorldToSprite(const Vector: TVector2f): TVector2f; overload;

    // Move the sprite to a position
    procedure MoveTo(const Position: TVector3f; const Rotation: Single);

    // Move the sprite a distance
    procedure Move(const DX, DY: Single);
    // Move the sprite to the left
    procedure MoveLeft(const Distance: Single);
    // Move the sprite to the right
    procedure MoveRight(const Distance: Single);
    // Move the sprite to the left
    procedure MoveUp(const Distance: Single);
    // Move the sprite to the right
    procedure MoveDown(const Distance: Single);

    // Move the sprite forward in the current direction
    procedure MoveForward(const Distance: Single);
    // Move the sprite backwards in the current direction
    procedure MoveBackward(const Distance: Single);

    // Move and rotates the sprite towards a point
    function MoveTowards(const FrameTime: Double; const Target: TVector2f; const LinearVelocity, AngularVelocity: Single): Boolean; overload;
    // Move and rotates the sprite towards a point
    function MoveTowards(const FrameTime: Double; const Target: TVector3f; const LinearVelocity, AngularVelocity: Single): Boolean; overload;

    // Strafe the entity left along the normal of the rotation vector
    procedure StrafeLeft(const Distance: Single);
    // Strafe the entity right along the normalof the rotation vector
    procedure StrafeRight(const Distance: Single);

    // Rotate the sprite to the left
    procedure RotateLeft(const Degrees: Single);
    // Rotate the sprite to the right
    procedure RotateRight(const Degrees: Single);
    // Rotates the sprite towards a point
    procedure RotateTowards(const Point: TVector2f; const Delta: Single);

    // Move a sprite to the center position of this sprite
    procedure AttatchTo(Sprite: TPHXSprite); overload;
    // Attatch another sprite to a tag in the image
    procedure AttatchTo(Sprite: TPHXSprite; const Tag: String ); overload;
    // Attatch another sprite to a tag in the image
    procedure AttatchTo(Sprite: TPHXSprite; const Tag: TPHXTag); overload;

    // Creates a box shape that fits this sprite
    procedure CreateDefaultShape;

    // Move the sprite to the front
    procedure BringToFront;
    // Send the sprite to the back
    procedure SendToBack;

    // Return a clone of this sprite
    function Clone: TPHXSprite; virtual;
    // Copy all properties from another sprite to this sprite
    procedure Assign(Sprite: TPHXSprite); virtual;

    // The sprite engine
    property Engine: TPHXSpriteEngine read FEngine;
    // The parent sprite
    property Parent: TPHXSprite read FParent write SetParent;
    // Child sprites
    property Sprites: TPHXSpriteList read FSprites;

    // The name of the sprite
    property Name: String read FName write FName;
     // Elapsed time
    property Time: Double read FTime write FTime;
    // Toggles if the entity is visible
    property Visible: Boolean read FVisible write FVisible;
    // If the sprite is alive.
    property Alive: Boolean read FAlive;
    // If the sprite is active
    property Active: Boolean read FActive write FActive;

    // Position of the sprite
    property Position: TVector3f read FPosition write SetPosition;
    // Current X-position
    property X: Single read FPosition.X write SetPositionX;
    // Current Y-position
    property Y: Single read FPosition.Y write SetPositionY;
    // Current Z-position
    property Layer: Single read FPosition.Z write SetPositionZ;
    // Current rotation
    property Rotation: Single read FRotation write SetRotation;
    // Bounding box of the sprite
    property Bounds: TRectf read FLocalBounds write SetBounds;

    // The imgage for the sprite
    property Image: String read FImage write SetImage;
    // The pattern index for the sprite
    property Pattern: String read FPattern write SetPattern;
    // This is the shape to be used for collision testing
    property Shape: String read FShape write SetShape;
    // The alpha value of the sprite
    property Alpha: Single read FColor.Alpha write FColor.Alpha;
    // The color of the sprite
    property Color: TColor4f read FColor write FColor;

    // If this sprite should be considered in the collision testing
    property Collider: Boolean read FCollider write SetCollider;
    // The collision mode of the sprite
    property Mode: TPHXSpriteCollisionMode read FMode write FMode;
    // The collision mode of the sprite
    property Group: TPHXSpriteCollisionGroup read FGroup write FGroup;

    //
    //property CollisionEnabled: Boolean read FCollisionData write FCollisionData;
    //
    //property CollisionGroup: TPHXSpriteCollisionMode read FCollisionGroup write FCollisionGroup;
    //
    //property CollisionMode: TPHXSpriteCollisionGroup read FCollisionFilter write FCollisionFilter;
    //
    //property CollisionData: Pointer read FCollisionData write FCollisionData;

    // Relative transformation between the parent and this sprite
    property LocalTransform: TMatrix4f read FLocalTransform;
    // Absolute transformation in world coordinates
    property FinalTransform: TMatrix4f read FFinalTransform;

    // Bounding box in releative sprite coordinates
 		property LocalBounds: TRectf read FLocalBounds;
    // Bounding box in absolute world coordinates
 		property FinalBounds: TRectf read FFinalBounds;

    // Shape in releative sprite coordinates
    property LocalShape: TPHXShape read FLocalShape;
    // Shape in absolute world coordinates
    property FinalShape: TPHXShape read FFinalShape;



    // Width of the sprite
    property Width: Integer read GetWidth;
    // Height of the sprite
    property Height: Integer read GetHeight;

    // The current image
    property LinkedImage: TPHXImage read FLinkedImage write SetImageL;
    // The current pattern
    property LinkedPattern: TPHXPatternIndex read FLinkedPattern write SetPatternL;
    // The current shape
    property LinkedShape: TPHXShape read FLinkedShape write SetShapeL;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteList'}

// List of sprites
PSpriteList = ^TSpriteList;
TSpriteList = array[0..$00FFFFFF] of TPHXSprite;

// Container for a list of sprites
//------------------------------------------------------------------------------
TPHXSpriteList = class
  private
    FEngine: TPHXSpriteEngine;
    FList: TList;

    function GetCount: Integer;
    function GetList: PSpriteList;
    function GetItem(Index: Integer): TPHXSprite;
  public
    constructor Create(AEngine: TPHXSpriteEngine); virtual;
    destructor Destroy; override;

    // Remove and destroy all sprites
    procedure Clear;

    // Add a sprite to the list
    procedure Add(Sprite: TPHXSprite);
    // Delete and free one sprite
    procedure Remove(Sprite: TPHXSprite);

    // Initialize all sprites
    procedure Initialize;
    // Transform all sprites
    procedure Transform;
    // Update all sprites
	  procedure Update(const DeltaTime: Double);
    // Render all sprites
    procedure Render; overload;
    // Render all sprites
    procedure Render(const Viewport: TRectf); overload;

    // Draw the bounding boxes for all sprites
    procedure DrawBounds(Canvas: TPHXCanvas);
    // Draw the shapes boxes for all sprites
    procedure DrawShapes(Canvas: TPHXCanvas);
    // Draw all sprites that are marked as colliders
    procedure DrawColliders(Canvas: TPHXCanvas);

    // Draw the name of all sprites
    procedure DrawNames(Font: TPHXFont; const Color: TColor4f);

    // Draw the displacement vector of all sprites
    procedure DrawDisplacement(Canvas: TPHXCanvas);

    // Sort all sprites by their layer
    procedure SortByLayer;

     // The owning sprite engine
    property Engine: TPHXSpriteEngine read FEngine;
    // Number of sprites in the list
    property Count: Integer read GetCount;
    // Pointer to the internal list
    property List: PSpriteList read GetList;
    // Get a sprite from the list
    property Item[Index: Integer]: TPHXSprite read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXAnimatedSprite'}

// A sprite with animation
//------------------------------------------------------------------------------
TPHXAnimatedSprite = class(TPHXSprite)
  private
    FAnimation: String;
    FAnimationItem: TPHXAnimation;

    FState: TPHXAnimationState;

    procedure FindLinked;

    procedure SetAnimation(const Value: String);
    procedure SetAnimationItem(const Value: TPHXAnimation);
  public
    // Creates a new animated sprite and adds it to the engine
    constructor Create(AEngine: TPHXSpriteEngine); override;

    // Reset the animation
    procedure Reset;
    // Updates the animation
    procedure Update(const DeltaTime: Double); override;

    // Return a clone of this sprite
    function Clone: TPHXSprite; override;
    // Copy all properties from another sprite to this sprite
    procedure Assign(Sprite: TPHXSprite); override;

    // The animation for the sprite
    property Animation: String read FAnimation write SetAnimation;
    // The currrent animation
    property AnimationItem: TPHXAnimation read FAnimationItem write SetAnimationItem;

    // The animation state
    property State: TPHXAnimationState read FState;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteFactory'}

// Class of sprites
TPHXSpriteClass = class of TPHXSprite;
// Event for creating a sprite
TPHXSpriteConstructor = function(Engine: TPHXSpriteEngine; const Name: String): TPHXSprite of object;

// This class is used by the sprite factory to create sprites by name
//------------------------------------------------------------------------------
TPHXSpriteFactoryItem = class
  private
    FName  : String;
    FSprite: TPHXSpriteClass;
    FEvent : TPHXSpriteConstructor;
  public
    // Create a new factory item
    constructor Create(AName: String);
    // Frees this factory item
    destructor Destroy; override;

    // Creates a new sprite from this factory item
    function CreateSprite(Engine: TPHXSpriteEngine): TPHXSprite;

    // Name of the factory item
    property Name: String read FName write FName;
    // The sprite class to create with this factory item
    property Sprite: TPHXSpriteClass read FSprite write FSprite;
    // The callback that is called to create the sprite
    property Event: TPHXSpriteConstructor read FEvent write FEvent;
  end;

TSpriteFactoryItemList = array[0..$00FFFFFF] of TPHXSpriteFactoryItem;
PSpriteFactoryItemList = ^TSpriteFactoryItemList;

// The factory is used to create sprites by name
//------------------------------------------------------------------------------
TPHXSpriteFactory = class
  private
    FEngine: TPHXSpriteEngine;
    FList  : TList;

    function GetCount: Integer;
    function GetList: PSpriteFactoryItemList;
    function GetItem(const Index: Integer): TPHXSpriteFactoryItem;
  public
    // Create a new sprite factory
    // @param(AEngine The owning sprite engine)
    constructor Create(AEngine: TPHXSpriteEngine);
    // Destroy this sprite factory
    destructor Destroy; override;

    // Remove and free all items
    procedure Clear;

    // Add a item to the list
    // @param(Name Name of factory the sprite)
    // @param(Sprite The class of sprite to create)
    function Add(const Name: String; Sprite: TPHXSpriteClass): TPHXSpriteFactoryItem; overload;
    // Add a item to the list
    // @param(Name Name of factory the sprite)
    // @param(Event The function callback that will create the sprite)
    function Add(const Name: String; Event: TPHXSpriteConstructor): TPHXSpriteFactoryItem; overload;

    // Find a item by name
    // @param(Name Name of the sprite to create)
    // @returns(The factory item for the sprite)
    function Find(const Name: String): TPHXSpriteFactoryItem; overload;
    // Find a item by name
    // @param(Name Name of the sprite to create)
    // @param(Item The found item)
    // @returns(True if the item existed in the list)
    function Find(const Name: String; out Item: TPHXSpriteFactoryItem): Boolean; overload;

    // Create a new item using this factory
    // @param(Name Name of the sprite to create)
    // @returns(The new sprite or null if the )
    function New(const Name: String): TPHXSprite;

    // The owning sprite engine
    property Engine: TPHXSpriteEngine read FEngine;
    // Return the number of items in the list
    property Count: Integer read GetCount;
    // Return a pointer to the internal list
    property List: PSpriteFactoryItemList read GetList;
    // Return a item in the list
    property Items[const Index: Integer]: TPHXSpriteFactoryItem read GetItem;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteCollision'}

// Contains a pair of sprites in a collision
//------------------------------------------------------------------------------
TPHXSpriteCollision = record
  public
    // The first sprite
    A: TPHXSprite;
    // The second sprite
    B: TPHXSprite;
  end;

// Array of sprite collision par
TSpriteCollisionList = array[0..$00FFFFFF] of TPHXSpriteCollision;
// Pointer to a array of sprite collision par
PSpriteCollisionList = ^TSpriteCollisionList;

// Contains a list of collision pairs
//------------------------------------------------------------------------------
TPHXSpriteCollisionList = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PSpriteCollisionList;

    procedure Grow;

    function GetItem(Index: Integer): TPHXSpriteCollision;

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a empty list
    constructor Create; overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;
    // Add a item to the list
    procedure Add(const A, B: TPHXSprite);

    // The current number of items in the list
    Property Count : Integer read FCount    write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PSpriteCollisionList read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TPHXSpriteCollision read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteCollider'}

// The collider class is responsible for testing for collisions
//------------------------------------------------------------------------------
TPHXSpriteCollider = class
  private
    FEngine : TPHXSpriteEngine;
    FSprites: TList;
    FTests  : Integer;
  protected
    // Return the potential colliders for a sprite
    function PotentialColliders(Sprite: TPHXSprite): TList; virtual;
  public
    // Create a new sprite collider
    constructor Create(AEngine: TPHXSpriteEngine); virtual;
    // Destroy the sprite collider
    destructor Destroy; override;

    // Update the collider
    procedure Update; virtual;
     // Collide all sprites
    procedure Collide; overload;

    // Add a sprite to the collider
    procedure Add(Sprite: TPHXSprite); virtual;
    // Remove a sprite from the collider
    procedure Remove(Sprite: TPHXSprite); virtual;
    // Move a sprite
    procedure Move(Sprite: TPHXSprite); virtual;

    // Tests two sprites for collisions
    function CollisionTest(const A, B: TPHXSprite): Boolean; overload;
    // Tests two sprites for collisions
    function CollisionTest(const A, B: TPHXSprite; const Displacement: TVector2f): Boolean; overload;

    // Collide a sprite against other sprites and add all collisions to a collision list
    function CollisionTest(const Sprite: TPHXSprite; const Filter: TPHXSpriteCollisionFilter; Collisions: TPHXSpriteCollisionList): Boolean; overload;
    // Collide a sprite against other sprites and add all collisions to a collision list
    function CollisionTest(const Sprite: TPHXSprite; const Displacement: TVector2f; const Filter: TPHXSpriteCollisionFilter; Collisions: TPHXSpriteCollisionList): Boolean; overload;

    // The owning engine
    property Engine: TPHXSpriteEngine read FEngine;
    // All sprites that is marked for collisions
    property Sprites: TList read FSprites;
    // Number of collision tests performed
    property Tests: Integer read FTests;
  end;

// Sprite collider using a spatial hash
// @exclude
//------------------------------------------------------------------------------
TPHXSpatialHashCollider = class(TPHXSpriteCollider)
  private
    FSize   : TVector2f;
    FSizeInv: TVector2f;
    FRows   : Integer;
    FCols   : Integer;
    FCells  : array of TList;
    FTemp   : TList;

    // Get the cell for a object
    procedure GetCell(const X, Y: Single; const Cells: PIntegerList; var Count: Integer);
  protected
    function PotentialColliders(Sprite: TPHXSprite): TList; override;
  public
    // Create a new sprite collider
    constructor Create(AEngine: TPHXSpriteEngine; const ACellSize: TVector2f); reintroduce;
    // Destroy the sprite collider
    destructor Destroy; override;

    // Clear all buckets
    procedure Clear;

    // Add a sprite to the collider
    procedure Add(Sprite: TPHXSprite); override;
    // Remove a sprite from the collider
    procedure Remove(Sprite: TPHXSprite); override;
    // Move a sprite
    procedure Move(Sprite: TPHXSprite); override;

    procedure GetNearby(Sprites: TList; const Rect: TRectf); overload;
    procedure GetNearby(Sprites: TList; Sprite: TPHXSprite); overload;


    // Render the
    procedure Render(Canvas: TPHXCanvas; Font: TPHXFont);

    property Size: TVector2f read FSize;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteCamera'}

// Sprite camera
//------------------------------------------------------------------------------
TPHXSpriteCamera = class
  private
    FDevice: TPHXDevice;
    FEngine: TPHXSpriteEngine;

    FScroll: TVector2f;


    procedure SetScrollX(const Value: Single);
    procedure SetScrollY(const Value: Single);

    function GetHeight  : Integer;
    function GetWidth   : Integer;
    function GetViewport: TRectf;
  public
    // Create a new sprite camera
    constructor Create(AEngine: TPHXSpriteEngine; ADevice: TPHXDevice);

    procedure CenterOn(Sprite: TPHXSprite);

    // Calculates parallax scrolling
    function MakeParallax(const X, Y: Single; const Factor: Single): TVector2f; overload;
    // Calculates parallax scrolling
    function MakeParallax(const Value: TVector2f; const Factor: Single): TVector2f; overload;

    // The phoenix device
    property Device: TPHXDevice read FDevice;
    // The owning sprite engine
    property Engine: TPHXSpriteEngine read FEngine;
    // Current scroll position
    property Scroll: TVector2f read FScroll;
    // The world scroll position
    property ScrollX: Single read FScroll.X write SetScrollX;
    // The world scroll position
    property ScrollY: Single read FScroll.Y write SetScrollY;

    // Width of the camera viewport
    property Width: Integer read GetWidth;
    // Height of the camera viewport
    property Height: Integer read GetHeight;

    property Viewport: TRectf read GetViewport;
  end;

{$ENDREGION}

{$REGION 'TPHXSpriteEngine'}

// Sprite options
//------------------------------------------------------------------------------
TPHXSpriteOptions = set of (
  // Sort sprites by the layer
  soSort,
  // Call the collide function from the update
  soCollide,
  // Cull sprites that are outside the viewport
  soCulling,
  // Limit scrolling so it doesnt go outside the engine bounds
  soLimitScroll
);

// Sprite engine
//------------------------------------------------------------------------------
TPHXSpriteEngine = class
  private
    FRoot       : TPHXSprite;
    FCollider   : TPHXSpriteCollider;
    FFactory    : TPHXSpriteFactory;
    FCamera     : TPHXSpriteCamera;
    FOptions    : TPHXSpriteOptions;

    FSprites  : TList;
    FDead     : TList;

    FWidth      : Integer;
    FHeight     : Integer;

    FImages    : TPHXImageList;
    FAnimations: TPHXAnimationList;
    FShapes    : TPHXShapeList;

    function GetCount: Integer;
    function GetBounds: TRectf;

    procedure SetCollider(const Value: TPHXSpriteCollider);
    procedure SetOptions (const Value: TPHXSpriteOptions);
  protected
    // Flag telling if the sprite engine has been initialized
    FInitialized: Boolean;
  public
    // Create a new sprite engine
    constructor Create(ADevice: TPHXDevice); overload; virtual;
    // Create a new sprite engine
    constructor Create(ADevice: TPHXDevice; const AWidth: Integer; AHeight: Integer); overload; virtual;
    // Free this sprite engine
    destructor Destroy; override;

    // Initialize the engine
    procedure Initialize; virtual;

    // Updates all the sprites, the collider and removes all sprites marked as dead
    // @param(DeltaTime The elapsed time since the last update)
    procedure Update(const DeltaTime: Double); virtual;
    // Collide all sprites
    procedure Collide; overload;

    // Draw all sprites
    procedure Render;

    // Draw the bounding boxes for all sprites
    procedure RenderBounds(Canvas: TPHXCanvas; const Color: TColor4f);
    // Draw the shapes for all sprites
    procedure RenderShapes(Canvas: TPHXCanvas; const Color: TColor4f);
    // Render all sprites that are marked as colliders
    procedure RenderColliders(Canvas: TPHXCanvas; const Color: TColor4f);
     // Draw the shapes boxes for all sprites
    procedure RenderNames(Font: TPHXFont; const Color: TColor4f);

    // Collide a sprite against the sprites in the engine
    procedure Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList); overload;
    // Collide a sprite against the sprites in the engine
    procedure Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList; const Filter: TPHXSpriteCollisionFilter); overload;
    // Collide a sprite against the sprites in the engine
    procedure Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList; const Group: TPHXSpriteCollisionGroup); overload;

    // Returns the sprite at a positions
    function SpriteAt(const X,Y: Single): TPHXSprite; overload;
    // Returns the sprite at a positions
    function SpriteAt(const Position: TVector2f): TPHXSprite; overload;

    // Converts a screen position to a world position
    function ScreenToWorld(const X, Y: Single): TVector2f; overload;
    // Converts a screen position to a world position
    function ScreenToWorld(const Position: TVector2f): TVector2f; overload;
    // Converts a screen position to a world position
    function ScreenToWorld(const Position: TVector2i): TVector2f; overload;

    // Converts a world position to a screen position
    function WorldToScreen(const X, Y: Single): TVector2f; overload;
    // Converts a world position to a screen position
    function WorldToScreen(const Position: TVector2f): TVector2f; overload;
    // Converts a world position to a screen position
    function WorldToScreen(const Position: TVector2i): TVector2f; overload;

     // Count the number of sprites by kind
    function CountSpritesByKind(const Kind: TPHXSpriteClass): Integer; overload;

    // Delete and destroy all dead sprites
    procedure RemoveDead;

    // The root sprite
    property Root: TPHXSprite read FRoot;
    // The sprite collider
    property Collider: TPHXSpriteCollider read FCollider write SetCollider;
    // The sprite factory
    property Factory: TPHXSpriteFactory read FFactory;
    // The sprite camera
    property Camera: TPHXSpriteCamera read FCamera;
    // Options for the sprite list
    property Options: TPHXSpriteOptions read FOptions write SetOptions;

    // List of all sprites
    property Sprites: TList read FSprites;
    // List of dead sprites, will be cleared by RemoveDead
    property Dead: TList read FDead;

    // Returns the number of sprites in the list
    property Count: Integer read GetCount;
    // Width of the world
    property Width: Integer read FWidth write FWidth;
    // Height of the world
    property Height: Integer read FHeight write FHeight;
    // Get the bounding box of the world
    property Bounds: TRectf read GetBounds;

    // List of images used by the ChangeImage method of the sprites
    property Images: TPHXImageList read FImages write FImages;
    // List of images used by the ChangeAnimation  method of the sprites
    property Animations: TPHXAnimationList read FAnimations write FAnimations;
    // List of images used by the ChangeShape  method of the sprites
    property Shapes: TPHXShapeList read FShapes write FShapes;
  end;

{$ENDREGION}

// Reference counter for the sprites
var SpriteRef: Integer = 0;

// Register a sprite class
procedure RegisterSprite(const Sprite: TPHXSpriteClass; const Name: AnsiString);
// Create a instance of a previously registered sprite
function CreateSprite(Engine: TPHXSpriteEngine; const Name: AnsiString): TPHXSprite;

implementation

resourcestring
  SNotInitialized = 'You must initialize the sprite engine first';

  SMissingImage     = 'The image "%s" was not found in the sprite engine.';
  SMissingPattern   = 'The pattern "%s" was not found in the sprite engine.';
  SMissingAnimation = 'The animation "%s" was not found in the sprite engine.';

// Registry of sprites
//------------------------------------------------------------------------------
var SpriteRegistry: array of record
  Name: AnsiString;
  Sprite: TPHXSpriteClass;
end;

// Register a sprite class
//------------------------------------------------------------------------------
procedure RegisterSprite(const Sprite: TPHXSpriteClass; const Name: AnsiString);
var Index: Integer;
begin
  Index:= Length(SpriteRegistry);

  SetLength(SpriteRegistry, Index + 1);

  SpriteRegistry[Index].Name  := Name;
  SpriteRegistry[Index].Sprite:= Sprite;
end;

//------------------------------------------------------------------------------
function CreateSprite(Engine: TPHXSpriteEngine; const Name: AnsiString): TPHXSprite;
var Index: Integer;
begin
  for Index:= Low(SpriteRegistry) to High(SpriteRegistry) do
  begin   
    if SpriteRegistry[Index].Name = Name then
    begin
      Result:= SpriteRegistry[Index].Sprite.Create(Engine);
      Exit;
    end;
  end;
  Result:= nil;
end;


//------------------------------------------------------------------------------
procedure SortSpritesByLayer(SortList: PSpriteList; L, R: Integer);
var I, J: Integer;
var P, T: TPHXSprite;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while (SortList[I].Layer - P.Layer) < 0 do
        Inc(I);
      while (SortList[J].Layer - P.Layer) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList[I];
          SortList[I] := SortList[J];
          SortList[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortSpritesByLayer(SortList, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
Function MakeSwept(const Rect: TRectf; const Displacement: TVector2f): TRectf;
begin
  if Displacement.X > 0 then
  begin
    Result.Left := Rect.Left;
    Result.Right:= Rect.Right + Displacement.X;
  end else
  begin
    Result.Left := Rect.Left + Displacement.X;
    Result.Right:= Rect.Right;
  end;

  if Displacement.Y > 0 then
  begin
    Result.Top   := Rect.Top;
    Result.Bottom:= Rect.Bottom + Displacement.Y;
  end else
  begin
    Result.Top   := Rect.Top + Displacement.Y;
    Result.Bottom:= Rect.Bottom;
  end;
end;

{$REGION 'TPHXSprite'}

// TPHXSprite
//==============================================================================
constructor TPHXSprite.Create(AEngine: TPHXSpriteEngine);
begin
  Assert(Assigned(AEngine));

  Inc(SpriteRef);

  FEngine     := AEngine;
  FName      := 'Sprite';
  FAlive     := True;
  FVisible   := True;
  FActive    := True;
  FTime      := 0;


  FPosition  := Vector3f_Zero;
  FRotation  := 0.0;
  FSprites   := TPHXSpriteList.Create(AEngine);

  FLocalBounds   := TRectF.Create(0,0,0,0);
  FFinalBounds   := TRectF.Create(0,0,0,0);
  FLocalTransform:= Matrix4f_Identity;
  FFinalTransform:= Matrix4f_Identity;

  FLocalShape:= nil;
  FFinalShape:= nil;

  FImage  := '';
  FPattern:= '';
  FColor  := clrWhite;

  FCollider:= False;
  FMode    := cmDynamic;
  FGroup   := cgGroup1;

 // FCollisionGroup := sgGroup1;
//  FCollisionFilter:= sgGroupAll;

  // Add this sprite to the list of all sprites
  FEngine.Sprites.Add(Self);
end;


//------------------------------------------------------------------------------
destructor TPHXSprite.Destroy;
begin
  SetParent(nil);

  // Remove the sprite from the list of colliders
  if FCollider and Assigned(Engine.Collider) then
  begin
    Engine.Collider.Remove(Self);
  end;

  // Remove this sprite from the list of all sprites
  FEngine.Sprites.Remove(Self);

  if Assigned(FLocalShape) then
  begin
    FreeAndNil(FLocalShape);
    FreeAndNil(FFinalShape);
  end;

  // Destroy child sprites
  FSprites.Free;

  Dec(SpriteRef);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Transform;
begin
  TransformRequired:= False;

  // Calculate our relative transformation matrix if we are moved
  FLocalTransform      := Matrix_CreateRotationZ(FRotation);
  FLocalTransform.v[12]:= FPosition.X;
  FLocalTransform.v[13]:= FPosition.Y;
  FLocalTransform.v[14]:= FPosition.Z;

  // If we have a parent entity transform our relative transformation with the
  // absolute transform of the parent, else our absolute transformation is our
  // relative transformation
  if Assigned(Parent) then
  begin
    FFinalTransform:= Matrix_Multiply(Parent.FinalTransform, Self.LocalTransform);
  end else
  begin
    FFinalTransform:= Self.FLocalTransform;
  end;

  FFinalBounds.Left  := FLocalBounds.Left   + FFinalTransform.v[12];
  FFinalBounds.Right := FLocalBounds.Right  + FFinalTransform.v[12];

  FFinalBounds.Top   := FLocalBounds.Top    + FFinalTransform.v[13];
  FFinalBounds.Bottom:= FLocalBounds.Bottom + FFinalTransform.v[13];

  if Assigned(FLocalShape) then
  begin
    FinalShape.Assign(LocalShape);
    FinalShape.Transform(FinalTransform);
  end;

  Sprites.Transform;

  if FCollider and Assigned(FEngine.Collider) then
  begin
    Engine.Collider.Move(Self);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Initialize;
begin
  FPreviousPosition:= FPosition;
  
  Transform();
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Update(const DeltaTime: Double);
begin
  Time:= Time + DeltaTime;
  
  // Transform the sprite
  if TransformRequired then
  begin
	  Transform();
  end;

  // Update the child sprites
  Sprites.Update(DeltaTime);
  
  FPreviousPosition:= FPosition;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Render;
var Transform: TMatrix4f;
begin
  if Assigned(FLinkedImage) then
  begin
    Transform:= FinalTransform;
    Transform.v[12]:= Transform.v[12] + Round(Engine.Camera.ScrollX);
    Transform.v[13]:= Transform.v[13] + Round(Engine.Camera.ScrollY);

    if FLinkedPattern >= 0 then
    begin
      FLinkedImage.DrawTransform(Transform, FColor, FPattern);
    end else
    begin
      FLinkedImage.DrawTransform(Transform, FColor);
    end;
  end;

  Sprites.Render;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.RenderBounds(Canvas: TPHXCanvas; Color: TColor4f);
var Rect: TRectf;
begin
  Canvas.Texture:= (nil);
  Canvas.Color  := Color;

  Rect.Left  := FinalBounds.Left    + Round(Engine.Camera.ScrollX);
  Rect.Top   := FinalBounds.Top     + Round(Engine.Camera.ScrollY);
  Rect.Right := FinalBounds.Right   + Round(Engine.Camera.ScrollX);
  Rect.Bottom:= FinalBounds.Bottom  + Round(Engine.Camera.ScrollY);

  Canvas.Rectangle(Rect);
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Kill;
var Index: Integer;
begin
  FAlive:= False;

  Killed;

  FEngine.Dead.Remove(Self);

  // Kill all child sprites
  for Index := 0 to Sprites.Count - 1 do
  begin
    Sprites.List^[Index].Kill;
  end;

  FEngine.Dead.Add(Self);
end;

//------------------------------------------------------------------------------
function TPHXSprite.PointInSprite(const Point: TVector2f): Boolean;
begin
  Result:= PointInRect(Point, FinalBounds);

  if Result and Assigned(FinalShape) then
  begin
    Result:= FinalShape.PointInside(Point)
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.Collide(const Displacement: TVector2f): Boolean;
begin
  if Assigned(Engine.Collider) then
  begin
    Result:= Engine.Collider.CollisionTest(Self, Displacement, cfFilterAll, nil);
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.Collide(const Displacement: TVector2f; const Filter: TPHXSpriteCollisionFilter): Boolean;
begin
  if Assigned(Engine.Collider) then
  begin
    Result:= Engine.Collider.CollisionTest(Self, Displacement, Filter, nil);
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.Collide(const Displacement: TVector2f; const Group: TPHXSpriteCollisionGroup): Boolean;
begin
  if Assigned(Engine.Collider) then
  begin
    Result:= Engine.Collider.CollisionTest(Self, Displacement, [Group], nil);
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.SpriteToWorld(const Vector: TVector3f): TVector3f;
begin
  Result:= Matrix_Transform(FFinalTransform, Vector);
end;

//------------------------------------------------------------------------------
function TPHXSprite.SpriteToWorld(const Vector: TVector2f): TVector2f;
begin
  Result:= Matrix_Transform(FFinalTransform, Vector);
end;

//------------------------------------------------------------------------------
function TPHXSprite.WorldToSprite(const Vector: TVector3f): TVector3f;
begin
  Result:= Matrix_TransformInv(FFinalTransform, Vector);
end;

//------------------------------------------------------------------------------
function TPHXSprite.WorldToSprite(const Vector: TVector2f): TVector2f;
begin
  Result:= Matrix_TransformInv(FFinalTransform, Vector);
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Clicked(const X, Y: Integer);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Collided(Sprite: TPHXSprite);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Killed;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveTo(const Position: TVector3f; const Rotation: Single);
begin
  FPosition.X:= Position.X;
  FPosition.Y:= Position.Y;
  FPosition.Z:= Position.Z;

  FRotation:= Rotation;

  TransformRequired:= True;
end;


//------------------------------------------------------------------------------
procedure TPHXSprite.Move(const DX, DY: Single);
begin
  FPosition.X:= FPosition.X + DX;
  FPosition.Y:= FPosition.Y + DY;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveLeft(const Distance: Single);
begin
  FPosition.X:= FPosition.X - Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveRight(const Distance: Single);
begin
  FPosition.X:= FPosition.X + Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveUp(const Distance: Single);
begin
  FPosition.Y:= FPosition.Y - Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveDown(const Distance: Single);
begin
  FPosition.Y:= FPosition.Y + Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveForward(const Distance: Single);
var Vector: TVector2f;
begin
  Vector:= Matrix_Rotate(FFinalTransform, Vector2f_AxisX);

  FPosition.X:= FPosition.X + Vector.X * Distance;
  FPosition.Y:= FPosition.Y + Vector.Y * Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.MoveBackward(const Distance: Single);
var Vector: TVector2f;
begin
  Vector:= Matrix_Rotate(FFinalTransform, Vector2f_AxisX);

  FPosition.X:= FPosition.X - Vector.X * Distance;
  FPosition.Y:= FPosition.Y - Vector.Y * Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
function TPHXSprite.MoveTowards(const FrameTime: Double; const Target: TVector2f; const LinearVelocity, AngularVelocity: Single): Boolean;
var DirectionEntity: TVector2f;
var DirectionPoint: TVector2f;

//var Position: TVector3f;

var Delta: Single;

var RotationDirection: Single;
var RotationRemaining: Single;
begin
  Delta:= FrameTime * AngularVelocity;

//  Position:= SpriteToWorld(FPosition);

  DirectionEntity:= Matrix_Rotate(FFinalTransform, Vector2f_AxisX);
  DirectionPoint.X:= Target.X - Position.X;
  DirectionPoint.Y:= Target.Y - Position.Y ;
  DirectionPoint:= VectorNormalize(DirectionPoint);

  RotationDirection:= (DirectionPoint.X * DirectionEntity.Y) - (DirectionPoint.Y * DirectionEntity.X) ;
  // Determine the direction to rotate with the cross product of the target
  // direction and current direction
//  {$IFDEF Windows}
//  RotationRemaining:= ArcCos( VectorDot(DirectionEntity, DirectionPoint) ) * RAD_TO_DEG;
//  {$ELSE}
  RotationRemaining:= ArcCos( VectorCross(DirectionEntity, DirectionPoint) ) * RAD_TO_DEG;
 // {$ENDIF}
  // If there is less distance remaining to move then the distance we will move
  // this frame just snap the rotation to the target rotation
  if RotationRemaining < Delta  then
  begin

    if RotationDirection > 0 then
    begin
      SetRotation(FRotation - RotationRemaining);
    end else
    begin
      SetRotation(FRotation + RotationRemaining);
    end;

  end else
  begin

    if RotationDirection > 0 then
    begin
      SetRotation(FRotation - Delta);
    end else
    begin
      SetRotation(FRotation + Delta);
    end;
  end;

  MoveForward(FrameTime * LinearVelocity);

  Result:= False;
  // Update the missile entity
 // Entity.Rotation := Vector3f(0,0,FRotation);
 // Entity.PositionX:= Entity.PositionX + FDirection.X * FVelocity * FrameTime;
 // Entity.PositionY:= Entity.PositionY + FDirection.Y * FVelocity * FrameTime;    }
end;

(*

// http://www.allegro.cc/forums/thread/591460/673459#target
// http://gmc.yoyogames.com/index.php?showtopic=184876
// http://forums.xna.com/forums/t/10203.aspx
// http://www.euclideanspace.com/maths/algebra/vectors/angleBetween/index.htm
// http://www.physicsforums.com/showthread.php?s=6f4a3ed3ef90d56b0b38716e082b4927&t=238212
//------------------------------------------------------------------------------
procedure TPHXTargetedMissileBehavior.Update(const FrameTime: Single);
var PositionMissile  : TVector3f;
var PositionTarget   : TVector3f;
var TargetDirection  : TVector2f;

var DistanceToTarget : Single;

var RotationDirection: Single;
var RotationRemaining: Single;
var RotationDelta    : Single;
begin
  PositionMissile:= Entity.EntityToScreen( Vector_Zero );
  PositionTarget := Target.EntityToScreen( Vector_Zero );

  TargetDirection.X:=PositionTarget.X - PositionMissile.X;
  TargetDirection.Y:=PositionTarget.Y - PositionMissile.Y;

  DistanceToTarget:= Magnitude(TargetDirection);

  // Normalise the target vector
//  if DistanceToTarget = 0 then
 // begin
 //    TargetDirection.X:= TargetDirection.X / DistanceToTarget;
 //    TargetDirection.Y:= TargetDirection.Y / DistanceToTarget;
 // end;

  if DistanceToTarget < ExplosionRadius then
  begin
    OnTargetReached(Entity, Target);
  end;

  // Determine the direction to rotate with the cross product of the target
  // direction and current direction
  RotationDirection:= (TargetDirection.x * FDirection.Y) - (TargetDirection.Y * FDirection.x) ;

  RotationRemaining:= ArcCos( VectorDot(FDirection, TargetDirection) ) * RAD_TO_DEG;

  // If there is less distance remaining to move then the distance we will move
  // this frame just snap the rotation to the target rotation
  if RotationRemaining < (TurningRate * FrameTime)  then
  begin
    FRotation :=ArcTan2(TargetDirection.Y, TargetDirection.X) * RAD_TO_DEG;
  end else
  begin
    RotationDelta:= - Sign(RotationDirection) * TurningRate * FrameTime;

    FRotation:= FRotation + RotationDelta;
  end;
  // Update the direction vector
  FDirection:= Vector2f_CreateFromAngle(FRotation, 1.0);

  // Update the missile entity
  Entity.Rotation := Vector3f(0,0,FRotation);
  Entity.PositionX:= Entity.PositionX + FDirection.X * FVelocity * FrameTime;
  Entity.PositionY:= Entity.PositionY + FDirection.Y * FVelocity * FrameTime;
end;
*)

//------------------------------------------------------------------------------
function TPHXSprite.MoveTowards(const FrameTime: Double; const Target: TVector3f; const LinearVelocity, AngularVelocity: Single): Boolean;
begin
  Result:= MoveTowards(FrameTime, Vector2f(Target), LinearVelocity, AngularVelocity);
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.StrafeLeft(const Distance: Single);
var Vector: TVector2f;
begin
  Vector:= Matrix_Rotate(FFinalTransform, Vector2f_AxisY);

  FPosition.X:= FPosition.X - Vector.X * Distance;
  FPosition.Y:= FPosition.Y - Vector.Y * Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.StrafeRight(const Distance: Single);
var Vector: TVector2f;
begin
  Vector:= Matrix_Rotate(FFinalTransform, Vector2f_AxisY);

  FPosition.X:= FPosition.X + Vector.X * Distance;
  FPosition.Y:= FPosition.Y + Vector.Y * Distance;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.RotateLeft(const Degrees: Single);
begin
  FRotation:= FRotation - Degrees;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.RotateRight(const Degrees: Single);
begin
  FRotation:= FRotation + Degrees;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.RotateTowards(const Point: TVector2f; const Delta: Single);
var DirectionEntity: TVector2f;
var DirectionPoint: TVector2f;

var Position: TVector3f;

var RotationDirection: Single;
var RotationRemaining: Single;
begin
  Position:= SpriteToWorld(FPosition);

  DirectionEntity:= Matrix_Rotate(FFinalTransform, Vector2f_AxisX);
  DirectionPoint.X:= Point.X - Position.X;
  DirectionPoint.Y:= Point.Y - Position.Y ;
  DirectionPoint:= VectorNormalize(DirectionPoint);

  RotationDirection:= (DirectionPoint.X * DirectionEntity.Y) - (DirectionPoint.Y * DirectionEntity.X) ;
  // Determine the direction to rotate with the cross product of the target
  // direction and current direction
  RotationRemaining:= ArcCos( VectorDot(DirectionEntity, DirectionPoint) ) * RAD_TO_DEG;

  // If there is less distance remaining to move then the distance we will move
  // this frame just snap the rotation to the target rotation
  if RotationRemaining < Delta  then
  begin

    if RotationDirection > 0 then
    begin
      SetRotation(FRotation - RotationRemaining);
    end else
    begin
      SetRotation(FRotation + RotationRemaining);
    end;

  end else
  begin

    if RotationDirection > 0 then
    begin
      SetRotation(FRotation - Delta);
    end else
    begin
      SetRotation(FRotation + Delta);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.AttatchTo(Sprite: TPHXSprite);
begin
  // If the sprite isnt a child of this entity transform it to world coordinates
  if Parent <> Sprite then
  begin
    FPosition:= Matrix_GetTranslation(Sprite.FinalTransform);
    FRotation:= Matrix_GetRotationZ  (Sprite.FinalTransform);
  end else
  begin
    FPosition:= Matrix_GetTranslation(Sprite.LocalTransform);
    FRotation:= Matrix_GetRotationZ  (Sprite.LocalTransform);
  end;

  Transform;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.AttatchTo(Sprite: TPHXSprite; const Tag: String);
var ATag  : TPHXTag;
begin
  Assert(Sprite.LinkedImage <> nil);

  if Sprite.LinkedImage.Tags.Find(Tag, ATag) then
  begin
    AttatchTo(Sprite, ATag);
  end else
  begin
    raise EPHXSpriteError.CreateFmt('The tag "%s" was not found in the image "%s".', [Tag, FImage]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.AttatchTo(Sprite: TPHXSprite; const Tag: TPHXTag);
var Matrix: TMatrix4f;
begin
  Matrix      := Matrix_CreateRotationZ(Tag.Rotation);
  Matrix.v[12]:= Tag.X;
  Matrix.v[13]:= Tag.Y;

  // If the sprite isnt a child of this entity transform it to world coordinates
  if Parent <> Sprite then
  begin
    Matrix:= Matrix_Multiply(Sprite.FinalTransform, Matrix);
  end else
  begin
    Matrix:= Matrix_Multiply(Sprite.LocalTransform, Matrix);
  end;

  FPosition:= Matrix_GetTranslation(Matrix);
  FRotation:= Matrix_GetRotationZ  (Matrix);

  Transform;
end;


//------------------------------------------------------------------------------
procedure TPHXSprite.UpdateBounds;
begin
  if Assigned(FLinkedImage)  then
  begin

    if (FLinkedPattern >= 0) and (FLinkedPattern < FLinkedImage.Patterns.Count) then
    begin
      FLocalBounds.Left  := - FLinkedImage.Patterns.List^[FLinkedPattern].Pivot.X;
      FLocalBounds.Top   := - FLinkedImage.Patterns.List^[FLinkedPattern].Pivot.Y;
      FLocalBounds.Right := - FLinkedImage.Patterns.List^[FLinkedPattern].Pivot.X + FLinkedImage.Patterns.List^[FLinkedPattern].Width;
      FLocalBounds.Bottom:= - FLinkedImage.Patterns.List^[FLinkedPattern].Pivot.Y + FLinkedImage.Patterns.List^[FLinkedPattern].Height;
    end else
    begin
      FLocalBounds.Left  := 0;
      FLocalBounds.Top   := 0;
      FLocalBounds.Right := FLinkedImage.Width;
      FLocalBounds.Bottom:= FLinkedImage.Height;
    end;
  end else
  begin
    FLocalBounds.Left  := 0;
    FLocalBounds.Top   := 0;
    FLocalBounds.Right := 0;
    FLocalBounds.Bottom:= 0;
  end;

  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.CreateDefaultShape;
var Shape: TPHXBox;
begin
  // Free the old shapes
  if Assigned(FLocalShape) then
  begin
    FreeAndNil(FLocalShape);
    FreeAndNil(FFinalShape);
  end;

  Shape:= TPHXBox.Create;
  try
    Shape.Name:= Name;
    Shape.CenterX:= (LocalBounds.Left   + LocalBounds.Right ) * 0.5;
    Shape.CenterY:= (LocalBounds.Top    + LocalBounds.Bottom) * 0.5;
    Shape.SizeX  := (LocalBounds.Right  - LocalBounds.Left);
    Shape.SizeY  := (LocalBounds.Bottom - LocalBounds.Top);

    // Set sprite makes two clones of the shape, thus we can free it afterwards
    FLocalShape:= Shape.Clone;
    FFinalShape:= Shape.Clone;
  finally
    Shape.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetParent(const Value: TPHXSprite);
begin
  if Assigned(FParent) then
  begin
    FParent.Sprites.Remove(Self);
  end;

  FParent := Value;

  if Assigned(FParent) then
  begin
    FParent.Sprites.Add(Self);
  end;

  Transform;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SendToBack;
begin
  if Assigned(FParent) then
  begin
    FParent.Sprites.FList.Remove(Self);
    FParent.Sprites.FList.Insert(0, Self);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.BringToFront;
begin
  if Assigned(FParent) then
  begin
    FParent.Sprites.FList.Remove(Self);
    FParent.Sprites.FList.Add(Self);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.Clone: TPHXSprite;
begin
  Result:= TPHXSprite.Create(Engine);
  Result.Assign(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.Assign(Sprite: TPHXSprite);
begin
  FName    := Sprite.FName;
  FTime    := Sprite.FTime;
  FVisible := Sprite.FVisible;
  FAlive   := Sprite.FAlive;
  FActive  := Sprite.FActive;

  FCollider:= Sprite.FCollider;
  FMode    := Sprite.FMode;
  FGroup   := Sprite.FGroup;

  FPosition:= Sprite.FPosition;
  FRotation:= Sprite.FRotation;

  FColor  := Sprite.FColor;
  FImage  := Sprite.FImage;
  FPattern:= Sprite.FPattern;
  FShape  := Sprite.FShape;

  // Relative transformation between the parent and this entiry
  FLocalTransform:= Sprite.FLocalTransform;
  // Absolute transformation in the world
  FFinalTransform:= Sprite.FFinalTransform;

  // Bounding box in releative sprite coordinates
  FLocalBounds:= Sprite.FLocalBounds;
  // Bounding box in absolute world coordinates
  FFinalBounds:= Sprite.FFinalBounds;

  if Assigned(FLocalShape) then FLocalShape.Free;
  if Assigned(FFinalShape) then FFinalShape.Free;

  // Shape in releative sprite coordinates
  FLocalShape:= Sprite.FLocalShape.Clone;
  // Shape in absolute world coordinates
  FFinalShape:= Sprite.FFinalShape.Clone;

  FLinkedImage  := Sprite.FLinkedImage;
  FLinkedPattern:= Sprite.FLinkedPattern;
  FLinkedShape  := Sprite.FLinkedShape;

  // The previous position, used to calculate displacement
  FPreviousPosition:= Sprite.FPreviousPosition;

  SetParent  (Sprite.Parent);
  SetCollider(Sprite.Collider);
 
  TransformRequired:= True;
end;

//------------------------------------------------------------------------------
function TPHXSprite.GetDisplacement: TVector2f;
begin
  Result.X:= FPosition.X - FPreviousPosition.X;
  Result.Y:= FPosition.Y - FPreviousPosition.Y;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetCollider(const Value: Boolean);
begin
  // Make shure the transformation matrix are updated before updating to collider
  Transform;

  if FCollider and Assigned(FEngine.Collider) then
  begin
    FEngine.Collider.Remove(Self);
  end;

  FCollider := Value;

  if FCollider and Assigned(FEngine.Collider) then
  begin

    FEngine.Collider.Add(Self);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetPosition(const Value: TVector3f);
begin
  if (FPosition.X <> Value.X) or (FPosition.Y <> Value.Y) or (FPosition.Z <> Value.Z) then
  begin
    FPosition := Value;

    TransformRequired:= True
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetPositionX(const Value: Single);
begin
  if FPosition.X <> Value then
  begin
    FPosition.X := Value;

    TransformRequired:= True;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetPositionY(const Value: Single);
begin
  if FPosition.Y <> Value then
  begin
    FPosition.Y := Value;

    TransformRequired:= True;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetPositionZ(const Value: Single);
begin
  if FPosition.Z <> Value then
  begin
    FPosition.Z := Value;

    TransformRequired:= True;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetRotation(const Value: Single);
begin
  if (FRotation <> Value) then
  begin
    FRotation:= Value;

    TransformRequired:= True;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetBounds(const Value: TRectf);
begin
  FLocalBounds := Value;

  // Update the world bounds
  FFinalBounds.Left  := FLocalBounds.Left   + FFinalTransform.v[12];
  FFinalBounds.Right := FLocalBounds.Right  + FFinalTransform.v[12];
  FFinalBounds.Top   := FLocalBounds.Top    + FFinalTransform.v[13];
  FFinalBounds.Bottom:= FLocalBounds.Bottom + FFinalTransform.v[13];
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.FindLinked;
begin
  if Assigned(FEngine.Images) then
  begin
    FLinkedImage:= FEngine.Images.Find(FImage);

    if Assigned(FLinkedImage) then
    begin
      FLinkedPattern:= FLinkedImage.Patterns.IndexOf(FPattern);
    end else
    begin
      FLinkedPattern:= -1;
    end;
  end else
  begin
    if (FImage <> '') then
    begin
      raise EPHXSpriteError.Create('The sprite engine contains no images.');
    end;

    FLinkedImage  := nil;
    FLinkedPattern:= -1;
  end;

  if Assigned(FEngine.Shapes) then
  begin
    FLinkedShape:= FEngine.Shapes.Find(FShape);

    // Create a clone of the shape
    if Assigned(FLinkedShape) then
    begin
      // Free the old shapes
      if Assigned(FLocalShape) then
      begin
        FreeAndNil(FLocalShape);
        FreeAndNil(FFinalShape);
      end;

      FLocalShape:= FLinkedShape.Clone;
      FFinalShape:= FLinkedShape.Clone;

      FinalShape.Transform(FinalTransform);
    end;
  end else
  begin
    if (FShape <> '') then
    begin
      raise EPHXSpriteError.Create('The sprite engine contains no shapes.');
    end;

    FLinkedShape:= nil;
  end;

  UpdateBounds;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetImage(const Value: String);
begin
  if FImage <> Value then
  begin
    FImage:= Value;

    FindLinked;

    if FLinkedImage = nil then
    begin
      raise Exception.CreateFmt(SMissingImage, [FImage]);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetImageL(const Value: TPHXImage);
begin
  FLinkedImage := Value;

  if Assigned(FLinkedImage) then
  begin
    FImage:= FLinkedImage.Name;

    UpdateBounds;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXSprite.SetPattern(const Value: String);
begin
  if FPattern <> Value then
  begin
    FPattern := Value;

    FindLinked;

    if FLinkedPattern < 0 then
    begin
      raise Exception.CreateFmt(SMissingPattern, [FPattern]);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetPatternL(const Value: TPHXPatternIndex);
begin
  FLinkedPattern:= Value;

  if Assigned(FLinkedImage) then
  begin
    if (FLinkedPattern >= 0) and (FLinkedPattern < FLInkedImage.Patterns.Count) then
    begin
      FPattern:= String(FLinkedImage.Patterns[FLinkedPattern].Name);

      UpdateBounds;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXSprite.SetShape(const Value: String);
begin
  if (Value <> FShape) then
  begin
    FShape:= Value;

    FindLinked;

    if FLinkedShape = nil then
    begin
      raise Exception.CreateFmt('Shape not found: %s', [FShape]);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSprite.SetShapeL(const Value: TPHXShape);
begin
  FLinkedShape := Value;

  // Create a clone of the shape
  if Assigned(FLinkedShape) then
  begin
    FShape:= FLinkedShape.Name;

    // Free the old shapes
    if Assigned(FLocalShape) then
    begin
      FreeAndNil(FLocalShape);
      FreeAndNil(FFinalShape);
    end;

    FLocalShape:= FLinkedShape.Clone;
    FFinalShape:= FLinkedShape.Clone;

    FinalShape.Transform(FinalTransform);
  end;
end;




//------------------------------------------------------------------------------
function TPHXSprite.GetWidth: Integer;
begin
  if Assigned(FLinkedImage)  then
  begin
    if (FLinkedPattern >= 0) and (FLinkedPattern < FLinkedImage.Patterns.Count) then
    begin
      Result:= FLinkedImage.Patterns.List^[FLinkedPattern].Width;
    end else
    begin
      Result:= FLinkedImage.Width;
    end;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSprite.GetHeight: Integer;
begin
  if Assigned(FLinkedImage)  then
  begin
    if (FLinkedPattern >= 0) and (FLinkedPattern < FLinkedImage.Patterns.Count) then
    begin
      Result:= FLinkedImage.Patterns.List^[FLinkedPattern].Height;
    end else
    begin
      Result:= FLinkedImage.Height;
    end;
  end else
  begin
    Result:= 0;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXSpriteList'}

// TPHXSpriteList
//==============================================================================
constructor TPHXSpriteList.Create(AEngine: TPHXSpriteEngine);
begin
  FEngine:= AEngine;
  FList  := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteList.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Clear;
begin
  while FList.Count > 0 do
  begin
    TPHXSprite(FList.List[0]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Add(Sprite: TPHXSprite);
begin
  FList.Add(Sprite);

  if soSort in Engine.Options then
  begin
    SortByLayer;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Remove(Sprite: TPHXSprite);
begin
  FList.Remove(Sprite);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Initialize;
var Index: Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite(FList.List[Index]);

    Sprite.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Update(const DeltaTime: Double);
var Index: Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite(FList.List[Index]);

    Sprite.Update(DeltaTime);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Transform;
var Index : Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite(FList.List[Index]);

    Sprite.Transform;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Render;
var Index : Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite(FList.List[Index]);

    if Sprite.Visible then
    begin
      Sprite.Render;

      Sprite.Sprites.Render;
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.Render(const Viewport: TRectf);
var Index : Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite(FList.List[Index]);

    if Sprite.Visible and OverlapRect(Viewport, Sprite.FinalBounds) then
    begin
      Sprite.Render;

      Sprite.Sprites.Render(Viewport);
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.DrawBounds(Canvas: TPHXCanvas);
var Index   : Integer;
var Sprite: TPHXSprite;
var Rect  : TRectf;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite( FList.List[Index] );

    if Sprite.Visible then
    begin
      Rect:= Sprite.FinalBounds;

      Rect.Left  := Rect.Left    + Sprite.Engine.Camera.ScrollX;
      Rect.Top   := Rect.Top     + Sprite.Engine.Camera.ScrollY;
      Rect.Right := Rect.Right   + Sprite.Engine.Camera.ScrollX;
      Rect.Bottom:= Rect.Bottom  + Sprite.Engine.Camera.ScrollY;

      Canvas.Rectangle(Rect);

      Sprite.Sprites.DrawBounds(Canvas);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.DrawShapes(Canvas: TPHXCanvas);
var Index   : Integer;
var Sprite: TPHXSprite;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite( FList.List[Index] );

    if Sprite.Visible then
    begin
      if  Assigned(Sprite.FinalShape) then
      begin
        Sprite.FinalShape.Render(Canvas, Sprite.Engine.Camera.Scroll);
      end;

      Sprite.Sprites.DrawShapes(Canvas);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.DrawColliders(Canvas: TPHXCanvas);
var Index   : Integer;
var Sprite: TPHXSprite;
var Rect  : TRectf;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite( FList.List[Index] );

    if Sprite.Visible then
    begin

      if Sprite.Collider then
      begin
        Rect.Left  := Sprite.FinalBounds.Left    + Sprite.Engine.Camera.ScrollX;
        Rect.Top   := Sprite.FinalBounds.Top     + Sprite.Engine.Camera.ScrollY;
        Rect.Right := Sprite.FinalBounds.Right   + Sprite.Engine.Camera.ScrollX;
        Rect.Bottom:= Sprite.FinalBounds.Bottom  + Sprite.Engine.Camera.ScrollY;

        Canvas.Rectangle(Rect);
      end;

      Sprite.Sprites.DrawColliders(Canvas);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.DrawNames(Font: TPHXFont; const Color: TColor4f);
var Index   : Integer;
var Sprite  : TPHXSprite;
var Position: TVector2f;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite( FList.List[Index] );

    if Sprite.Visible then
    begin
      Position.X:= Sprite.FinalBounds.Left + Engine.Camera.ScrollX;
      Position.Y:= Sprite.FinalBounds.Top  + Engine.Camera.ScrollY;

      Font.TextOut(Position.X, Position.Y, Sprite.Name, Color);

      Sprite.Sprites.DrawNames(Font, Color);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.DrawDisplacement(Canvas: TPHXCanvas);
var Index   : Integer;
var Sprite: TPHXSprite;
var P1, P2: TVector2f;
begin
  for Index:=0 to FList.Count-1 do
  begin
    Sprite:= TPHXSprite( FList.List[Index] );

    if Sprite.Visible then
    begin
      P1.X:= Sprite.Position.X - Engine.Camera.ScrollX;
      P1.Y:= Sprite.Position.Y - Engine.Camera.ScrollY;

      P2:= Sprite.GetDisplacement;

      Canvas.Line(P1, P2);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteList.SortByLayer;
begin
  if (Count > 0) then
  begin
    SortSpritesByLayer(PSpriteList(FList.List), 0, FList.Count - 1);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSpriteList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXSpriteList.GetList: PSpriteList;
begin
  Result:= PSpriteList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXSpriteList.GetItem(Index: Integer): TPHXSprite;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXSprite( FList.List[Index] );
end;

{$ENDREGION}

{$REGION 'TPHXAnimatedSprite'}

// TPHXAnimatedSprite
//==============================================================================
constructor TPHXAnimatedSprite.Create(AEngine: TPHXSpriteEngine);
begin
  inherited Create(AEngine);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.FindLinked;
begin
  if Assigned(FEngine.Animations) then
  begin
    FAnimationItem:= FEngine.Animations.Find(FAnimation);
  end else
  begin
    FAnimationItem:= nil;
  end;

  Reset;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.Reset;
begin
  if Assigned(FAnimationItem) then
  begin
    FState.Animation:= FAnimationItem;
    FState.Reset;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.Update(const DeltaTime: Double);
begin
  inherited;

  if Assigned(FAnimationItem) then
  begin
    FState.Update(DeltaTime);

    FPattern:= FState.Name;

    FLinkedPattern:= FState.Pattern;

    if FState.Finished then
    begin
      Kill;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAnimatedSprite.Clone: TPHXSprite;
begin
  Result:= TPHXAnimatedSprite.Create(Engine);
  Result.Assign(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.Assign(Sprite: TPHXSprite);
begin
  inherited Assign(Sprite);

  if Sprite is TPHXAnimatedSprite then
  begin
    FAnimation    := TPHXAnimatedSprite(Sprite).FAnimation;
    FAnimationItem:= TPHXAnimatedSprite(Sprite).FAnimationItem;

    FState:= TPHXAnimatedSprite(Sprite).FState;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.SetAnimation(const Value: String);
begin
  if FAnimation <> Value then
  begin
    FAnimation:= Value;

    FindLinked;

    if (FAnimation <> '') and (FAnimationItem = nil) then
    begin
      raise Exception.CreateFmt(SMissingAnimation, [FAnimation]);
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXAnimatedSprite.SetAnimationItem(const Value: TPHXAnimation);
begin
  if FAnimationItem <> Value then
  begin
    FAnimationItem:= Value;

    if Assigned(FAnimationItem) then
    begin
      FAnimation:= FAnimationItem.Name;

      Reset;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TPHXSpriteFactoryItem'}

// TPHXSpriteFactoryItem
//==============================================================================
constructor TPHXSpriteFactoryItem.Create(AName: String);
begin
  FName  := AName;
  FEvent := nil;
  FSprite:= nil;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteFactoryItem.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactoryItem.CreateSprite(Engine: TPHXSpriteEngine): TPHXSprite;
begin
  if Assigned(Event) then
  begin
    Result:= Event(Engine, Name);
  end else
  if Assigned(Sprite) then
  begin
    Result:= Sprite.Create(Engine);
  end else
  begin
    Result:= nil;
  end;
end;


{$ENDREGION}

{$REGION 'TPHXSpriteFactory'}

// TPHXSpriteFactory
//==============================================================================
constructor TPHXSpriteFactory.Create(AEngine: TPHXSpriteEngine);
begin
  FEngine:= AEngine;
  FList  := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteFactory.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteFactory.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    TPHXSpriteFactoryItem(FList.List).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.Add(const Name: String; Sprite: TPHXSpriteClass): TPHXSpriteFactoryItem;
begin
  Assert(Assigned(Sprite));

  Result:= TPHXSpriteFactoryItem.Create(Name);
  Result.Sprite:= Sprite;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.Add(const Name: String; Event: TPHXSpriteConstructor): TPHXSpriteFactoryItem;
begin
  Assert(Assigned(Event));

  Result:= TPHXSpriteFactoryItem.Create(Name);
  Result.Event:= Event;

  FList.Add(Result);
end;



//------------------------------------------------------------------------------
function TPHXSpriteFactory.Find(const Name: String): TPHXSpriteFactoryItem;
var Index: Integer;
var Item : TPHXSpriteFactoryItem;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Item:= TPHXSpriteFactoryItem(FList.List);

    if SameText(Item.Name, Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.Find(const Name: String; out Item: TPHXSpriteFactoryItem): Boolean;
var Index: Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Item:= TPHXSpriteFactoryItem(FList.List);

    if SameText(Item.Name, Name) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Item := nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.New(const Name: String): TPHXSprite;
var Item : TPHXSpriteFactoryItem;
begin
  if Find(Name, Item) then
  begin
    Result:= Item.CreateSprite(FEngine);
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.GetList: PSpriteFactoryItemList;
begin
  Result:= PSpriteFactoryItemList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXSpriteFactory.GetItem(const Index: Integer): TPHXSpriteFactoryItem;
begin
  Result:= TPHXSpriteFactoryItem(FList.List[Index]);
end;

{$ENDREGION}

{$REGION 'TPHXSpriteCamera'}

// TPHXSpriteCamera
//==============================================================================
constructor TPHXSpriteCamera.Create(AEngine: TPHXSpriteEngine; ADevice: TPHXDevice);
begin
  FDevice:= ADevice;
  FEngine:= AEngine;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCamera.CenterOn(Sprite: TPHXSprite);
begin
  SetScrollX(-Sprite.FinalTransform.v[12] + Device.Width  * 0.5);
  SetScrollY(-Sprite.FinalTransform.v[13] + Device.Height * 0.5);
end;

//------------------------------------------------------------------------------
function TPHXSpriteCamera.GetWidth: Integer;
begin
  Result:= Device.Width;
end;

//------------------------------------------------------------------------------
function TPHXSpriteCamera.MakeParallax(const X, Y: Single; const Factor: Single): TVector2f;
begin
  Result.X:= (X + FScroll.X) * Factor;
  Result.Y:= (Y + FScroll.Y) * Factor;
end;

//------------------------------------------------------------------------------
function TPHXSpriteCamera.MakeParallax(const Value: TVector2f; const Factor: Single): TVector2f;
begin
  Result:= MakeParallax(Value.X, Value.Y, Factor);
end;

//------------------------------------------------------------------------------
function TPHXSpriteCamera.GetHeight: Integer;
begin
  Result:= Device.Height;
end;

//------------------------------------------------------------------------------
function TPHXSpriteCamera.GetViewport: TRectf;
begin
  Result.Left  := -FScroll.X;
  Result.Right := -FScroll.X + Device.Width;
  Result.Top   := -FScroll.Y;
  Result.Bottom:= -FScroll.Y + Device.Height;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCamera.SetScrollX(const Value: Single);
begin
  if FScroll.X <> Value then
  begin
    FScroll.X:= Value;

    if soLimitScroll in Engine.Options then
    begin
      if Value > 0 then
      begin
        FScroll.X:= 0;
      end else
      if Value < Device.Width-Engine.Width then
      begin
        FScroll.X:= Device.Width-Engine.Width;
      end else
      begin
        FScroll.X:= Value;
      end;
    end else
    begin
      FScroll.X:= Value;
    end;

  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCamera.SetScrollY(const Value: Single);
begin
  if FScroll.Y <> Value then
  begin
    if soLimitScroll in Engine.Options then
    begin
      if Value > 0 then
      begin
        FScroll.Y:= 0;
      end else
      if Value < Device.Height-Engine.Height then
      begin
        FScroll.Y:= Device.Height-Engine.Height;
      end else
      begin
        FScroll.Y:= Value;
      end;
    end else
    begin
      FScroll.Y:= Value;
    end;
  end;
end;
{$ENDREGION}


{$REGION 'TPHXSpriteEngine'}

// TPHXSpriteEngine
//==============================================================================
constructor TPHXSpriteEngine.Create(ADevice: TPHXDevice);
begin
  Create(ADevice, 16536, 16536);
end;

//------------------------------------------------------------------------------
constructor TPHXSpriteEngine.Create(ADevice: TPHXDevice; const AWidth: Integer; AHeight: Integer);
begin
  FWidth    := AWidth;
  FHeight   := AHeight;
  FCollider := TPHXSpriteCollider.Create(Self);
  FFactory  := TPHXSpriteFactory.Create(Self);
  FCamera   := TPHXSpriteCamera.Create(Self, ADevice);
  FOptions  := [soCollide, soCulling];

  FSprites  := TList.Create;
  FDead     := TList.Create;

  FRoot:= TPHXSprite.Create(Self);
  FRoot.Name:= 'World';

  // We dont want the root sprite in the list of sprites
  FSprites.Remove(FRoot);


  FInitialized:= False;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteEngine.Destroy;
begin
  // Free the collider
  SetCollider(nil);

  FFactory.Free;
  FCamera.Free;

  FRoot.Free;
  FSprites.Free;
  FDead.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Initialize;
begin
  Root.Sprites.Initialize;

  FInitialized:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Update(const DeltaTime: Double);
begin
  Assert(FInitialized, SNotInitialized);

  Root.Sprites.Update(DeltaTime);

  if (soCollide in Options) and Assigned(Collider) then
  begin
    Collider.Update;
  end;

  RemoveDead;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Collide;
begin
  if Assigned(Collider) then
  begin
    Collider.Update;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Render;
begin
  Assert(FInitialized, SNotInitialized);

  if soCulling in Options then
  begin
    Root.Sprites.Render(Camera.Viewport);
  end else
  begin
    Root.Sprites.Render();
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.RenderBounds(Canvas: TPHXCanvas; const Color: TColor4f);
begin
  Canvas.Texture:= (nil);
  Canvas.Color  := Color;

  Root.Sprites.DrawBounds(Canvas);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.RenderShapes(Canvas: TPHXCanvas; const Color: TColor4f);
begin
  Canvas.Texture:= (nil);
  Canvas.Color  := Color;

  Root.Sprites.DrawShapes(Canvas);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.RenderColliders(Canvas: TPHXCanvas; const Color: TColor4f);
begin
  Canvas.Texture:= (nil);
  Canvas.Color  := Color;

  Root.Sprites.DrawColliders(Canvas);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.RenderNames(Font: TPHXFont; const Color: TColor4f);
begin
  Root.Sprites.DrawNames(Font, Color);
end;



(*
//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Collide;
begin
  if Assigned(Collider) then
  begin
    Collider.Update;
  end;
end;   *)

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList);
begin
  Assert(FInitialized, SNotInitialized);

  if Assigned(Collider) then
  begin
    Collider.CollisionTest(Sprite, cfFilterAll, Collisions);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList; const Group: TPHXSpriteCollisionGroup);
begin
  Assert(FInitialized, SNotInitialized);

  if Assigned(Collider) then
  begin
    Collider.CollisionTest(Sprite, [Group], Collisions);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.Collide(Sprite: TPHXSprite; Collisions: TPHXSpriteCollisionList; const Filter: TPHXSpriteCollisionFilter);
begin
  Assert(FInitialized, SNotInitialized);

  if Assigned(Collider) then
  begin
    Collider.CollisionTest(Sprite, Filter, Collisions);
  end;
end;



//------------------------------------------------------------------------------
function TPHXSpriteEngine.SpriteAt(const X, Y: Single): TPHXSprite;
begin
  Result:= SpriteAt( Vector2f(X, Y) );
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.SpriteAt(const Position: TVector2f): TPHXSprite;
var Index  : Integer;
var Sprite: TPHXSprite;
begin
  for Index:= FSprites.Count-1 downto 0 do
  begin
    Sprite:= TPHXSprite( FSprites.List[Index] );

    if Sprite.Visible and Sprite.PointInSprite(Position) then
    begin
      Result:= Sprite;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.ScreenToWorld(const X, Y: Single): TVector2f;
begin
  Result.X:= X - Camera.Scroll.X;
  Result.Y:= Y - Camera.Scroll.Y;
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.ScreenToWorld(const Position: TVector2f): TVector2f;
begin
  Result:= ScreenToWorld(Position.X, Position.Y);
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.ScreenToWorld(const Position: TVector2i): TVector2f;
begin
  Result:= ScreenToWorld(Position.X, Position.Y);
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.WorldToScreen(const X, Y: Single): TVector2f;
begin
  Result.X:= X + Camera.Scroll.X;
  Result.Y:= Y + Camera.Scroll.Y;
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.WorldToScreen(const Position: TVector2f): TVector2f;
begin
  Result:= WorldToScreen(Position.X, Position.Y);
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.WorldToScreen(const Position: TVector2i): TVector2f;
begin
  Result:= WorldToScreen(Position.X, Position.Y);
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.CountSpritesByKind( const Kind: TPHXSpriteClass): Integer;
var Index : Integer;
var Sprite: TPHXSprite;
begin
  Result:= 0;

  for Index:= 0 to FSprites.Count - 1 do
  begin
    Sprite:= TPHXSprite( FSprites.List[Index] );

    if Sprite.Alive and (Sprite is Kind) then
    begin
      Inc(Result);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.RemoveDead;
var Index : Integer;
var Sprite: TPHXSprite;
begin
  for Index:= 0 to FDead.Count - 1 do
  begin
    Sprite:= TPHXSprite( FDead.List[Index] );

    if Sprite.Alive then Continue;

    Sprite.Free;
  end;

  FDead.Clear;
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.GetCount: Integer;
begin
  Result:= FSprites.Count;
end;

//------------------------------------------------------------------------------
function TPHXSpriteEngine.GetBounds: TRectf;
begin
  Result.Left  := 0;
  Result.Top   := 0;
  Result.Right := FWidth;
  Result.Bottom:= FHeight;
end;



//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.SetCollider(const Value: TPHXSpriteCollider);
var Index : Integer;
var Sprite: TPHXSprite;
begin
  if FCollider = Value then Exit;

  if Assigned(FCollider) then
  begin
    // Remove all sprites from the ord collider
    for Index:= 0 to FSprites.Count - 1 do
    begin
      Sprite:= TPHXSprite( FSprites.List[Index] );

      if Sprite.Collider then
      begin
        FCollider.Remove(Sprite);
      end;
    end;

    FCollider.Free;
  end;

  FCollider:= Value;

  if Assigned(FCollider) then
  begin
    // Add all sprites to the new collider
    for Index:= 0 to FSprites.Count - 1 do
    begin
      Sprite:= TPHXSprite( FSprites.List[Index] );

      if Sprite.Collider then
      begin
        FCollider.Add(Sprite);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteEngine.SetOptions(const Value: TPHXSpriteOptions);
begin
  if FInitialized then
  begin
    raise EPHXSpriteError.Create('Setting sprite options after initialize is not supported.');
  end;

  if FOptions <> Value then
  begin
    FOptions:= Value;
  end;
end;

{$ENDREGION}



{$REGION 'TPHXSpriteCollisionList'}

// TPHXSpriteCollisionList
//==============================================================================
constructor TPHXSpriteCollisionList.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteCollisionList.Destroy;
begin
  FCount:= 0;

  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollisionList.Add(const A, B: TPHXSprite);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1].A:= A;
  FList^[Count - 1].B:= B;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollisionList.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollisionList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta:= FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta:= 16
  end else
  begin
    Delta:= 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
function TPHXSpriteCollisionList.GetItem(Index: Integer): TPHXSpriteCollision;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollisionList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXSpriteCollision));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollisionList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then
  begin
    SetCapacity(FCount);
  end;
end;

{$ENDREGION}

{$REGION 'TPHXSpriteBroadphase'}

// http://conkerjo.wordpress.com/2009/06/13/spatial-hashing-implementation-for-fast-2d-collisions/

{$REGION 'TPHXSpriteCollider'}

// TPHXSpriteCollider
//==============================================================================
constructor TPHXSpriteCollider.Create(AEngine: TPHXSpriteEngine);
begin
  FSprites:= TList.Create;
  FEngine := AEngine;
end;

//------------------------------------------------------------------------------
destructor TPHXSpriteCollider.Destroy;
begin
  FSprites.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TPHXSpriteCollider.PotentialColliders(Sprite: TPHXSprite): TList;
begin
  Result:= FSprites;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollider.Update;
begin
  Collide;
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollider.Collide;
var I, J: Integer;
var A, B: TPHXSprite;
var List: TList;
begin
  FTests:= 0;

  for I:=0 to Sprites.Count - 1 do
  begin
    A:= TPHXSprite( Sprites.List[I] );

    if (A.Collider = False) or (A.Mode <> cmDynamic) or (A.Alive = False) then Continue;

    // Get the potential colliders for the sprite
    List:= PotentialColliders(A);

    for J:=0 to List.Count - 1 do
    begin
      B:= TPHXSprite(List.List[J]);

      // Skip collision testing if any of the sprites are dead
      if (A = B) or (B.Collider = False) or (B.Alive = False) then Continue;

      // Test for collisions
      if CollisionTest(A,B) then
      begin
        A.Collided(B);
      end;

      Inc(FTests);
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXSpriteCollider.Add(Sprite: TPHXSprite);
begin
  FSprites.Add(Sprite);
end;


//------------------------------------------------------------------------------
procedure TPHXSpriteCollider.Remove(Sprite: TPHXSprite);
begin
  FSprites.Remove(Sprite);
end;

//------------------------------------------------------------------------------
procedure TPHXSpriteCollider.Move(Sprite: TPHXSprite);
begin

end;

//------------------------------------------------------------------------------
function TPHXSpriteCollider.CollisionTest(const A, B: TPHXSprite): Boolean;
begin
  // Test if the boinds collides
  Result:= OverlapRect(A.FinalBounds, B.FinalBounds);

  if Result and Assigned(A.FinalShape) and Assigned(B.FinalShape) then
  begin
    Result:= TPHXShape.Collide(A.FinalShape, B.FinalShape);
  end
end;

//------------------------------------------------------------------------------
function TPHXSpriteCollider.CollisionTest(const A, B: TPHXSprite; const Displacement: TVector2f): Boolean;
begin
  // Test if the boinds collides
  Result:= OverlapRect( MakeSwept(A.FinalBounds, Displacement), B.FinalBounds);

  if Result and Assigned(A.FinalShape) and Assigned(B.FinalShape) then
  begin
    Result:= TPHXShape.Collide(A.FinalShape, B.FinalShape, Displacement);
  end
end;

//------------------------------------------------------------------------------
function TPHXSpriteCollider.CollisionTest(const Sprite: TPHXSprite; const Filter: TPHXSpriteCollisionFilter; Collisions: TPHXSpriteCollisionList): Boolean;
var Index: Integer;
var Other: TPHXSprite;
var List: TList;
begin
  Result:= False;

  if Assigned(Collisions) then
  begin
    Collisions.Count:= 0;
  end;

  // Get the potential colliders for the sprite
  List:= PotentialColliders(Sprite);

  for Index:=0 to List.Count - 1 do
  begin
    Other:= TPHXSprite(List.List[Index]);

    // Skip collision testing if any of the sprites are dead
    if (Sprite = Other) or (Other.Collider = False) or (Other.Alive = False) or not (Other.Group in Filter) then Continue;

     // Test for collisions
    if CollisionTest(Sprite, Other) then
    begin
      Result:= True;

      if Assigned(Collisions) then
      begin
        Collisions.Add(Sprite, Other);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXSpriteCollider.CollisionTest(const Sprite: TPHXSprite; const Displacement: TVector2f; const Filter: TPHXSpriteCollisionFilter; Collisions: TPHXSpriteCollisionList): Boolean;
var Index: Integer;
var Other: TPHXSprite;
var List: TList;
begin
  Result:= False;

  if Assigned(Collisions) then
  begin
    Collisions.Count:= 0;
  end;

  // Get the potential colliders for the sprite
  List:= PotentialColliders(Sprite);

  for Index:=0 to List.Count - 1 do
  begin
    Other:= TPHXSprite(List.List[Index]);

    // Skip collision testing if any of the sprites are dead
    if (Sprite = Other) or (Other.Collider = False) or (Other.Alive = False) or not (Other.Group in Filter) then Continue;

      // Test for collisions
    if CollisionTest(Sprite, Other, Displacement) then
    begin
      Result:= True;

      if Assigned(Collisions) then
      begin
        Collisions.Add(Sprite, Other);
      end;
    end;
  end;
end;




{$ENDREGION}




{$REGION 'TPHXSpriteCollider'}




// http://gamedev.stackexchange.com/questions/15126/2d-spatial-partitioning-alternatives-to-spatial-hashes-and-quadtrees

// TPHXSpatialHashCollider
//==============================================================================
constructor TPHXSpatialHashCollider.Create(AEngine: TPHXSpriteEngine; const ACellSize: TVector2f);
var Index: Integer;
begin
  FSprites:= TList.Create;
  FEngine := AEngine;

  FTemp:= TList.Create;

  FSize     := ACellSize;
  FSizeInv.X:= 1 / ACellSize.X;
  FSizeInv.Y:= 1 / ACellSize.Y;

  FCols:= Round(AEngine.Width  / ACellSize.X);
  FRows:= Round(AEngine.Height / ACellSize.Y);

  SetLength(FCells, FCols * FRows);
  // Create all cell lists
  for Index:= Low(FCells) to High(FCells) do
  begin
    FCells[Index]:= TList.Create;
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXSpatialHashCollider.Destroy;
var Index: Integer;
begin
  // Create all cell lists
  for Index:= Low(FCells) to High(FCells) do
  begin
    FCells[Index].Free;
  end;
  SetLength(FCells, 0);

  FTemp.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.Clear;
var Index: Integer;
begin
  // Clear all cell lists
  for Index:= Low(FCells) to High(FCells) do
  begin
    FCells[Index].Count:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.GetCell(const X, Y: Single; const Cells: PIntegerList; var Count: Integer);
var Row  : Integer;
var Col  : Integer;
var Cell : Integer;
var Index: Integer;
begin
  Col:= Trunc(X / FSize.X);
  Row:= Trunc(Y / FSize.Y);

  if (Col >= 0) and (Col < FCols) and (Row >= 0) and (Row < FRows) then
  begin
    // Calculate the cell for the coordinate
    Cell:= Col + Row * FCols;

    // Check so the cell isnt already in the list
    for Index:= 0 to Count-1 do
    begin
      if Cells^[Index] = Cell then Exit;
    end;
    // Save the cell
    Cells[Count]:= Cell;

    Inc(Count);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.GetNearby(Sprites: TList; const Rect: TRectf);
var Cells : array[0..3] of Integer;
var Count : Integer;
var I, J  : Integer;
var List  : TList;
var Sprite: TPHXSprite;
begin
  Count:= 0;
  GetCell(Rect.Left , Rect.Top   , @Cells, Count);
  GetCell(Rect.Right, Rect.Top   , @Cells, Count);
  GetCell(Rect.Left , Rect.Bottom, @Cells, Count);
  GetCell(Rect.Right, Rect.Bottom, @Cells, Count);

  Sprites.Count:= 0;
  for I:= 0 to Count-1 do
  begin
    List:= FCells[Cells[I]];

    for J:= 0 to List.Count-1 do
    begin
      Sprite:= TPHXSprite(List.List[J]);

      if Sprites.IndexOf(Sprite) = -1 then
      begin
        Sprites.Add(Sprite);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.GetNearby(Sprites: TList; Sprite: TPHXSprite);
begin
  GetNearby(Sprites, Sprite.FinalBounds);
end;


//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.Add(Sprite: TPHXSprite);
var Cells: array[0..3] of Integer;
var Count: Integer;
var Index: Integer;
begin
  inherited;

  if not RectInRect(Sprite.FinalBounds, Engine.Bounds) then
  begin
    raise EPHXSpriteError.CreateFmt('The sprite %s is outside the world bounds', [Sprite.Name]);
  end;

  Count:= 0;
  GetCell(Sprite.FinalBounds.Left , Sprite.FinalBounds.Top   , @Cells, Count);
  GetCell(Sprite.FinalBounds.Right, Sprite.FinalBounds.Top   , @Cells, Count);
  GetCell(Sprite.FinalBounds.Left , Sprite.FinalBounds.Bottom, @Cells, Count);
  GetCell(Sprite.FinalBounds.Right, Sprite.FinalBounds.Bottom, @Cells, Count);

  for Index:= 0 to Count-1 do
  begin
    FCells[Cells[Index]].Add(Sprite);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.Remove(Sprite: TPHXSprite);
var Index: Integer;
begin
  inherited;

  // Remove the sprite from all cells
  for Index:= Low(FCells) to High(FCells) do
  begin
    FCells[Index].Remove(Sprite);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.Move(Sprite: TPHXSprite);
var Cells: array[0..3] of Integer;
var Count: Integer;
var Index: Integer;
begin
  inherited;

  if not RectInRect(Sprite.FinalBounds, Engine.Bounds) then
  begin
    raise EPHXSpriteError.CreateFmt('The sprite %s is outside the world bounds', [Sprite.Name]);
  end;

  // Create all cell lists
  for Index:= Low(FCells) to High(FCells) do
  begin
    FCells[Index].Remove(Sprite);
  end;

  Count:= 0;
  GetCell(Sprite.FinalBounds.Left , Sprite.FinalBounds.Top   , @Cells, Count);
  GetCell(Sprite.FinalBounds.Right, Sprite.FinalBounds.Top   , @Cells, Count);
  GetCell(Sprite.FinalBounds.Left , Sprite.FinalBounds.Bottom, @Cells, Count);
  GetCell(Sprite.FinalBounds.Right, Sprite.FinalBounds.Bottom, @Cells, Count);

  for Index:= 0 to Count-1 do
  begin
    FCells[Cells[Index]].Add(Sprite);
  end;
end;

//------------------------------------------------------------------------------
function TPHXSpatialHashCollider.PotentialColliders(Sprite: TPHXSprite): TList;
begin
  Result:= FTemp;

  GetNearby(Result, Sprite.FinalBounds);
end;

//------------------------------------------------------------------------------
procedure TPHXSpatialHashCollider.Render(Canvas: TPHXCanvas; Font: TPHXFont);
var Index: Integer;
var X, Y : Integer;
var Rect : TRectf;
var Pos  : TVector2f;
var List : TList;
begin
  Canvas.Color:= clrSilver;
  Canvas.Texture:= nil;
  for Y:= 0 to FRows - 1 do
  begin
    for X:= 0 to FCols - 1 do
    begin
      Rect.Left := X * FSize.X            + Engine.Camera.ScrollX;
      Rect.Right:= X * FSize.X + FSize.X  + Engine.Camera.ScrollX;

      Rect.Top   := Y * FSize.Y           + Engine.Camera.ScrollY;
      Rect.Bottom:= Y * FSize.Y + FSize.Y + Engine.Camera.ScrollY;

      Canvas.Rectangle(Rect);
    end;
  end;

  Index:= 0;
  for Y:= 0 to FRows - 1 do
  begin
    for X:= 0 to FCols - 1 do
    begin
      List:= FCells[Index];

      Pos.X:= X * FSize.X + Engine.Camera.ScrollX + 4;
      Pos.Y:= Y * FSize.Y + Engine.Camera.ScrollY + 4;

      Font.TextOut(Pos.X, Pos.Y, Format('%.3d: %d', [Index, List.Count]), clrSilver);

      Inc(Index);
    end;
  end;

end;


{$ENDREGION}






end.
