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
unit phxImage;
//< Classes for managing and rendering 2D images and animations

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxTypes,
  phxClasses,
  phxMath,
  phxEvents,
  phxDevice,
  phxGraphics,
  phxTexture,
  phxCanvas;

const

// Default file extension of images
PHXIMAGE_EXT = '.phximg';
// Version of the binary image files
PHXIMAGE_VERSION = 20;

// Default file extension of animations
PHXANIMATION_EXT  = '.phxani';
// Version of the binary animation files
PHXANIMATION_VERSION = 20;

// Constant for invalid pattern selection
PHXPATTERN_NONE = -1;

type

// Forward declarations
TPHXImage = class;

{$REGION 'TPHXPattern'}

// Datatype for pattern indicies
TPHXPatternIndex = type Integer;

// A single pattern in a image
//------------------------------------------------------------------------------
TPHXPattern = record
  public
    // Name of the pattern
    Name: String[128];
    // X location in the image
    X: Integer;
    // Y location in the image
    Y: Integer;
    // Width of the pattern
    Width: Integer;
    // Height of the pattern
    Height: Integer;
    // The pivot point
    Pivot: TVector2i;
    // Flip the pattern
    Flip: Boolean;
    // Mirror the pattern
    Mirror: Boolean;
  public
    // Creates a new pattern
    class function Create(const Name: String; X, Y, Width, Height: Integer): TPHXPattern; overload; static;
    // Creates a new pattern with a pivot point
    class function Create(const Name: String; X, Y, Width, Height: Integer; const Pivot: TVector2i): TPHXPattern; overload; static;
  end;

// Pattern pointer
PPHXPattern = ^TPHXPattern;

// Comparision function for patterns
TPHXPatternCompare = function(const A: TPHXPattern; const B: TPHXPattern): Integer;

// Contains the information needed to render a pattern
//------------------------------------------------------------------------------
TPHXPatternBuffer = record
  // Texture coordinates
  TexCoord: TRectf;
  // Vertex positions
  Position: TRectf;
end;

// Pointer to a array of patterns
PPatternList = ^TPatternList;
// Array of patterns
TPatternList = array[0.. $000FFFFF] of TPHXPattern;

// List of patterns
//------------------------------------------------------------------------------
TPHXPatternList = class
  private
    FImage   : TPHXImage;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PPatternList;

    FUpdateCount: Integer;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXPattern;
    procedure SetItem(Index: Integer; const Value: TPHXPattern);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a new empty pattern list
    constructor Create(AImage: TPHXImage);
    // Destroy the list and all patterns
    destructor Destroy; override;

    // Start updating the list
    procedure BeginUpdate;
    // End updating the list
    procedure EndUpdate;

    // Clears the list
    procedure Clear;

    // Add a new pattern to the list
    procedure Add(const Value: TPHXPattern ); overload;
    // Add a new pattern to the list
    procedure Add(const Name: String; X, Y, Width, Height: Integer); overload;
    // Add a new pattern to the list
    procedure Add(const Name: String; X, Y, Width, Height, PivotX, PivotY: Integer); overload;

    // Generate rectangular patterns
    procedure AddRectangular(const Width, Height: Integer; CenterPivots: Boolean = False); overload;
    // Generate rectangular patterns
    procedure AddRectangular(const Width, Height: Integer; PivotX, PivotY: Integer); overload;

    // Add a single pattern that covers the whole image
    // @param(CenterPivot Center the pivot in the pattern)
    procedure AddSingle(CenterPivot: Boolean = False); overload;
    // Add a single pattern that covers the whole image
    // @param(Name Name of the pattern to add, if empty the image name is used)
    // @param(CenterPivot Center the pivot in the pattern)
    procedure AddSingle(const Name: String; CenterPivot: Boolean = False); overload;

    // Deletes a pattern at an index
    procedure Delete(Index: Integer);

    // Load the patterns from a stream
    procedure LoadFromStream(Stream: TStream);
    // Save the patterns to a stream
    procedure SaveToStream(Stream: TStream);

    // Returns the index of a pattern, -1 if not found.
    function IndexOf(const Name: String): Integer;
    // Find a pattern in the list and return the index of the pattern
    function Find(const Name: String): TPHXPatternIndex; overload;

    // Find a pattern in the list, returns true if the pattern exists
    function Find(const Name: String; out Pattern: TPHXPattern): Boolean; overload;

    // Sort the pattern list
    procedure Sort(Compare: TPHXPatternCompare);
    // Copy all the patterns from another list
    procedure Assign(Patterns: TPHXPatternList);

    // Returns the bounding rectangle for the pattern at index
    function GetPatternBounds(const Index: Integer): TRectf;
    // Returns the index of an pattern at a coordinate
    function GetPatternAt(const X, Y: Integer): Integer;

    // The owning image
    property Image: TPHXImage read FImage;
    // Number of items in the pattern list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    property List: PPatternList read FList;
    // Gets and sets the patterns
    property Items[Index: Integer]: TPHXPattern read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXTag'}

// A tag is used to mark an interesting position in the image,
// could be used to specify weapon hardpoints, sprite attatchments etc
//------------------------------------------------------------------------------
TPHXTag = record
  public
    // Name of the tag
    Name: String[32];
    // The owning pattern index or -1 if it is a image tag, used to specify
    // the origin of the coordinate system for the tag
    Pattern: Integer;
    // Horisontal position of the tag in the image
    X: Single;
    // Vertical position of the tag in the image
    Y: Single;
    // Rotation of the tag in degrees
    Rotation: Single;
  public
    // Creates a new tag
    class function Create(const Name: String; X, Y: Single; const Pattern: Integer; Rotation: Single): TPHXTag; static;
  end;

PTagList = ^TTagList;
TTagList = array[0.. $000FFFFF] of TPHXTag;

// List of tags
//------------------------------------------------------------------------------
TPHXTagList = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PTagList;
    FImage: TPHXImage;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXTag;
    procedure SetItem(Index: Integer; const Value: TPHXTag);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a new empty pattern list
    constructor Create(const Image: TPHXImage);
    // Destroy the list and all patterns
    destructor Destroy; override;

    // Clears the list
    procedure Clear;

    // Add a new tag to the list
    procedure Add(const Value: TPHXTag ); overload;
    // Add a new tag to the list
    procedure Add(const Name: String; const X, Y: Integer); overload;
    // Add a new tag to the list
    procedure Add(const Name: String; const X, Y: Integer; const Pattern: Integer); overload;

    // Deletes a pattern at an index
    procedure Delete(Index: Integer);


    // Load the tags from a file
    procedure LoadFromFile(const FileName: String);
    // Load the tags from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the tags to a file
    procedure SaveToFile(const FileName: String);
    // Save the tags to a stream
    procedure SaveToStream(Stream: TStream);

    // Returns the index of a tag, -1 if not found.
    function IndexOf(const Name: String): Integer;
    // Find a pattern in the tag, returns true if the tag exists
    function Find(const Name: String): TPHXTag; overload;
    // Find a pattern in the tag, returns true if the tag exists
    function Find(const Name: String; out Tag: TPHXTag): Boolean; overload;

    // Fills the tag index list with up to count tags indicies, returns the number of tags
    function FindByPattern(const PatternIndex: Integer; const Indicies: PIntegerList; const Count: Integer): Integer;

    // Return a tag at a position in image coordinates
    function TagAtPosition(const X,Y: Integer; const Tolerance: Integer): Integer;

    // Converts a tag position to a image position
    function TagToImage(const Tag: TPHXTag; const Position: TVector2f): TVector2f;
    // Calculate the transformation matrix for a tag
    procedure TagTransform(Const Index: Integer; out Matrix: TMatrix4f);

    // Owning image
    property Image: TPHXImage read FImage;
    // Number of items in the pattern list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    property List: PTagList read FList;
    // Gets and sets the patterns
    property Items[Index: Integer]: TPHXTag read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXImage'}

// The image file header.
//------------------------------------------------------------------------------
TPHXImageHeader = record
  // The id of the image file, should always be PHXIMG.
  Ident: Array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// The image is a container for a texture and supports patterns and tags
//------------------------------------------------------------------------------
TPHXImage = class
  private
    FDevice  : TPHXDevice;
    FCanvas  : TPHXCanvas;
    FTexture : TPHXTexture;

    FName    : String;
    FAuthor  : String;
    FVersion : String;
    FComment : String;
    FWidth   : Integer;
    FHeight  : Integer;
    FPatterns: TPHXPatternList;
    FTags    : TPHXTagList;
  protected
    // Buffer for pattern texture coordinates and verticies
    PatternBuffer: array of TPHXPatternBuffer;

    // Helper functions for rendering patterns
    procedure DrawPattern(const Pattern: TPHXPatternBuffer; const Position: TVector3f; const Color: TColor4f); overload;
    // Helper functions for rendering patterns
    procedure DrawPattern(const Pattern: TPHXPatternBuffer; const Position: TVector3f; const Scale: TVector2f; const Color: TColor4f); overload;
    // Helper functions for rendering patterns
    procedure DrawPattern(const Pattern: TPHXPatternBuffer; const Transform: TMatrix4f; const Color: TColor4f); overload;
  public
    // Creates a new image for the specified device
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
    // Creates a new image with a predefined texture
    constructor CreateEx(ATexture: TPHXTexture);
    // Destroys the image
    destructor Destroy; override;

    // Load a image from a file.
    procedure LoadImage(const FileName: String); overload;
    // Load the image from a stream.
    procedure LoadImage(const FileName: String; Stream: TStream); overload;

    // Load the image froma  file
    procedure LoadFromFile(const FileName: String);
    // Load the image from stream
    procedure LoadFromStream(Stream: TStream);

    // Save the image to a file
    procedure SaveToFile(const FileName: String);
    // Save the image to a stream
    procedure SaveToStream(Stream: TStream);

    // Initializes the image, must be called before any rendering
    procedure Initialize;

    // Draw the image at a specified location.
    procedure Draw(const X, Y: Single); overload;
    // Draw a pattern at a specified location.
    procedure Draw(const X, Y: Single; const PatternIndex: Integer); overload;
    // Draw a pattern at a specified location.
    procedure Draw(const X, Y: Single; const PatternName: String); overload;

    // Draw the image at the specified location with the specified color.
    procedure Draw(const X, Y: Single; const Color : TColor4f); overload;
    // Draw a pattern at the specified location with the specified color.
    procedure Draw(const X, Y: Single; const Color : TColor4f; const PatternIndex: Integer); overload;
     // Draw a pattern at the specified location with the specified color.
    procedure Draw(const X, Y: Single; const Color : TColor4f; const PatternName: String); overload;

    // Draw the image rotated.
    procedure DrawRotate(const X,Y : Single; const Angle: Single); overload;
    // Draw a pattern rotated.
    procedure DrawRotate(const X,Y : Single; const Angle: Single; const PatternIndex: Cardinal); overload;
    // Draw a pattern rotated.
    procedure DrawRotate(const X,Y : Single; const Angle: Single; const PatternName: String); overload;

    // Draw the image using a specifed transformation matrix
    procedure DrawTransform(const Transform: TMatrix4f); overload;
    // Draw a pattern using a specifed transformation matrix
    procedure DrawTransform(const Transform: TMatrix4f; const PatternIndex: Integer);  overload;
    // Draw a pattern using a specifed transformation matrix
    procedure DrawTransform(const Transform: TMatrix4f; const PatternName: String);  overload;

    // Draw the image using a specifed transformation matrix and color
    procedure DrawTransform(const Transform: TMatrix4f; const Color: TColor4f); overload;
    // Draw a pattern using a specifed transformation matrix and color
    procedure DrawTransform(const Transform: TMatrix4f; const Color: TColor4f; const PatternIndex: Integer);  overload;
    // Draw a pattern using a specifed transformation matrix and color
    procedure DrawTransform(const Transform: TMatrix4f; const Color: TColor4f; const PatternName: String);  overload;

    // Draws the image tiled with NumX and NumY repeats.
    procedure TileDraw(const X, Y: Single; const NumX, NumY: Integer); overload;
    // Draws a pattern tiled with NumX and NumY repeats.
    procedure TileDraw(const X, Y: Single; const NumX, NumY: Integer; const PatternIndex: Integer); overload;
    // Draws a pattern tiled with NumX and NumY repeats.
    procedure TileDraw(const X, Y: Single; const NumX, NumY: Integer; const PatternName: String); overload;

    // Draws the image streched to fill the supplied rectangle.
    procedure StrechDraw(const X, Y: Single; const W, H: Integer); overload;
    // Draws a pattern streched to fill the supplied rectangle.
    procedure StrechDraw(const X, Y: Single; const W, H: Integer; const PatternIndex: Integer); overload;
    // Draws a pattern streched to fill the supplied rectangle.
    procedure StrechDraw(const X, Y: Single; const W, H: Integer; const PatternName: String); overload;

    // Draw the image at the specified location with additive blending
    procedure DrawAdd(const X, Y: Single; const Alpha: Single); overload;
    // Draw a pattern at the specified location with additive blending
    procedure DrawAdd(const X, Y: Single; const Alpha: Single; const PatternIndex: Integer); overload;
    // Draw a pattern at the specified location with additive blending
    procedure DrawAdd(const X, Y: Single; const Alpha: Single; const PatternName: String); overload;

    // Draw the image at the specified location with the specified alpha.
    procedure DrawAlpha(const X, Y: Single; const Alpha: Single); overload;
    // Draw a pattern at the specified location with the specified alpha.
    procedure DrawAlpha(const X, Y: Single; const Alpha: Single; const PatternIndex: Integer); overload;
    // Draw a pattern at the specified location with additive blending
    procedure DrawAlpha(const X, Y: Single; const Alpha: Single; const PatternName: String); overload;

    // Draw the image at the specified location with custom blending
    procedure DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode); overload;
    // Draw a pattern at the specified location with custom blending
    procedure DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode; const PatternIndex: Integer); overload;
    // Draw a pattern at the specified location with custom blending
    procedure DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode; const PatternName: String); overload;

    // Draws a selected area of the image. The area is the texture coordinates in pixels to draw
    procedure DrawArea(const X, Y: Integer; const Area: TRecti); overload;
    // Draws a selected area of the image. The area is the texture coordinates in pixels to draw
    procedure DrawArea(const Rect: TRectf; const Area: TRecti); overload;

    // Draw the image using a wave function
   // procedure DrawWaveHorz(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer); overload;
   // procedure DrawWaveHorz(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer; const PatternIndex: Integer); overload;

   // procedure DrawWaveVert(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer);

    // Change the pivot position of a pattern
    procedure ChangePivot(const PatternIndex: Integer; const Pivot: TVector2i);

    // The owning device
    property Device: TPHXDevice read FDevice;
    // The canvas that is rendered to
    property Canvas: TPHXCanvas read FCanvas;
    // The name of the image, used to search for textures in the imagelist.
    property Name: String read FName write FName;
    // Author of the image
    property Author: String read FAuthor write FAuthor;
    // Version of the image
    property Version: String read FVersion write FVersion;
    // Comment of the image
    property Comment: String read FComment write FComment;
    // The width of the image.
    property Width  : Integer read FWidth write FWidth;
    // The height of the image.
    property Height : Integer read FHeight write FHeight;
    // The patterns in the image
    property Patterns: TPHXPatternList read FPatterns;
    // The tags in the image
    property Tags: TPHXTagList read FTags;
    // The internal texture
    property Texture: TPHXTexture read FTexture;
  end;

{$ENDREGION}

{$REGION 'TPHXImageList'}

// Contains a list of images
//------------------------------------------------------------------------------
TPHXImageList = class
 private
    FDevice: TPHXDevice;
    FCanvas: TPHXCanvas;
    FItems : TList;

    function GetItem(Index: Integer): TPHXImage;
    function GetList: PPointerList;
  protected
    procedure LoadList(const Stream: TStream; const Count: Integer);
    procedure SaveList(const Stream: TStream; const Count: Integer);

    function GetCount: Integer;
  public
    // Creates a new imagelist
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
    // Creates a new imagelist without a device or canvas
    constructor CreateEx;
    // Default destructor
    destructor Destroy; override;

    // Removes all images from the list.
    procedure Clear;

    // Add a new image to the imagelist
    function Add: TPHXImage; overload; virtual;
    // Add a new image to the imagelist with the name
    function Add(const Name : string): TPHXImage; overload;
    // Add the supplied image to the imagelist
    function Add(const Image: TPHXImage): TPHXImage; overload;

    // Delete and free a image at a index
    procedure Delete(const Index: Integer);
    // Remove and free a image
    procedure Remove(const Image: TPHXImage);

    // Add and load a image from a file .
    function LoadImage(const FileName: String): TPHXImage; overload;
    // Add and load a image from a file .
    function LoadImage(const FileName: String; const Name: string): TPHXImage; overload;
    // Add and load a image from a stream
    function LoadImage(const FileName: String; const Stream: TStream): TPHXImage; overload;
    // Load all images in a folder
    procedure LoadImages(const Path: String; const Filter: String = '*.phximg');

    // Returns the index of a image or -1 if not found.
    function IndexOf(const Name: string): Integer;

    // Returns the image with the name if found else nil
    function Find(const Name: string): TPHXImage; overload;
    // Find a image in the list, returns true if the image was found.
    function Find(const Name: string; out Image: TPHXImage): Boolean; overload;
    // Find the index of a pattern in a image, returns -1 if not found
    function Find(const Image: String; const Pattern: String): Integer; overload;

    // Owning device
    property Device: TPHXDevice read FDevice;
    // Owning canvas
    property Canvas: TPHXCanvas read FCanvas;
    // Number of images in the imagelist.
    property Count: Integer read GetCount;
    // Returns the pointer to the internal list
    property List: PPointerList read GetList;
    // The images.
    property Items[Index: Integer]: TPHXImage read GetItem; default;
  end;

 {$ENDREGION}

{$REGION 'Animations'}

TPHXAnimation = class;

// The animation file header.
//------------------------------------------------------------------------------
TPHXAnimationHeader = record
  // The id of the image file, should always be PHXANI.
  Ident: array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// A single frame in the animation
//------------------------------------------------------------------------------
TPHXAnimationFrame = record
  // Name of the frame, should be same as the pattern name in the image
  Name: String[128];
  // The time in seconds to show this frame
  Time: Single;
  // The pattern index in the image for the frame
  Pattern: Integer;
end;

// Pointer to the array of animation frames
PAnimationFrameList = ^TAnimationFrameList;
// Array of animation frames
TAnimationFrameList = array[0..$00AFFFFF] of TPHXAnimationFrame;

// Container for a list of animation frames
//------------------------------------------------------------------------------
TPHXAnimationFrames = class
 private
    FOwner    : TPHXAnimation;
    FCount    : Integer;
    FCapacity : Integer;
    FList     : PAnimationFrameList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXAnimationFrame;
    procedure SetItem(Index: Integer; const Value: TPHXAnimationFrame);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a new list of animation frames
    constructor Create(AOwner: TPHXAnimation);
    // Destroy the list
    destructor Destroy; override;

    // Clears the list
    procedure Clear;

    // Add a new frame to the list
    procedure Add(const Value: TPHXAnimationFrame ); overload;
    // Add a new frame to the list
    procedure Add(const Name: String; const Time: Single; const Pattern: Integer); overload;
    // Add a new frame to the list
    procedure Add(const Name: String; const Time: Single; const Pattern: string); overload;

    // Delete a frame
    procedure Delete(Index: Integer);

    // Load the animation frames from a file
    procedure LoadFromFile(const FileName: String);
    // Load the animation frames from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the animation frames to a file
    procedure SaveToFile(const FileName: String);
    // Save the animation frames to a stream
    procedure SaveToStream(Stream: TStream);

    // Copy all the patterns from another list
    procedure Assign(List: TPHXAnimationFrames);

    // Owning animation
    property Owner: TPHXAnimation read FOwner;
    // Number of items in the pattern list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    property List: PAnimationFrameList read FList;
    // Gets and sets the patterns
    property Items[Index: Integer]: TPHXAnimationFrame read GetItem Write SetItem; default;
  end;

// Contains the state for a animation
//------------------------------------------------------------------------------
TPHXAnimationState = record
  public
    // The current animation
    Animation: TPHXAnimation;
    // The queued animation, when the current animation is finished this will start
    Queued: TPHXAnimation;
    // The animation is active
    Active : Boolean;
    // The animation is finished (only true if not looped)
    Finished: Boolean;
    // Elased time
    Time: Single;
    // Current frame index
    Frame: Integer;
    // Current pattern name
    Name: String;
    // Current pattern index
    Pattern: Integer;
  public
    // Create a new animation state
    class function Create(AAnimation: TPHXAnimation): TPHXAnimationState; static;
  public
    // Reset the animation state
    procedure Reset; overload;
    // Update the animation state
    procedure Update(const DeltaTime: Double);

    // Draw the animation at a coordinate
    procedure Draw(const X, Y: Integer); overload;
    // Draw the animation using a transformation matrix
    procedure Draw(const Transform: TMatrix4f); overload;
  end;

// A animation implement a sprite animation using a image
//------------------------------------------------------------------------------
TPHXAnimation = class(TObject)
  private
    // List of images to seach for images
    FImages: TPHXImageList;
    // The name of the animation
    FName: String;
    // Author of the animation
    FAuthor: String;
    // Version of the animation
    FVersion: String;
    // Comment of the animation
    FComment: String;
    // If the animation is restarting after reaching the last frame
    FLooped: Boolean;
    // Default framerate for the animation
    FFrameRate: Integer;

    // List of animation frames, each frame is a pattern index in the owning image
    FFrames: TPHXAnimationFrames;

    // The animation image name
    FImageName: String;
    // The animation image
    FImage: TPHXImage;

    function GetDuration: Single;

    procedure SetImage(const Value: TPHXImage);
    procedure SetImageName(const Value: String);
  public
    // Create a new animation
    constructor Create(AImages: TPHXImageList);
    // Destroy the animation
    destructor Destroy; override;

    // Load the animation from a file
    procedure LoadFromFile(const FileName: String);
    // Load the animation from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the animation to a file
    procedure SaveToFile(const FileName: String);
    // Save the animation to a stream
    procedure SaveToStream(Stream: TStream);
    (*
    // Reset a animation state to the default value
    procedure Reset(var State: TPHXAnimationState );
    // Update a animation state
    procedure Update(var State: TPHXAnimationState; FrameTime: Single);
    // Draw the animation at a coordinate
    procedure Draw(const State: TPHXAnimationState; const X, Y: Integer); overload;
    // Draw the animation using a transformation matrix
    procedure Draw(const State: TPHXAnimationState; const Transform: TMatrix4f); overload;
    *)

    // Calculate the time of each frame by setting the framerate in frames per second
    procedure SetFramerate(const Framerate: Integer);

    // List of images to seach for images
    property Images: TPHXImageList read FImages;
    // The name of the animation
    property Name: String  read FName write FName;
    // Author of the animation
    property Author: String read FAuthor write FAuthor;
    // Version of the animation
    property Version: String read FVersion write FVersion;
    // Comment of the animation
    property Comment: String read FComment write FComment;

    // If the animation is restarting after reaching the last frame
    property Looped: Boolean read FLooped write FLooped;
    // Default framerate for the animation
    property FrameRate: Integer read FFrameRate write FFrameRate;
    // Returns the duration of the animation in seconds
    property Duration: Single read GetDuration;
    // Get the list of frames of the animation
    property Frames: TPHXAnimationFrames read FFrames;

    // The image for this animation
    property Image: TPHXImage read FImage write SetImage;
    // Name of the image for the animation
    property ImageName: String read FImageName write SetImageName;
  end;

// Pointer to a array of animations
PAnimationList = ^TAnimationList;
// Array of animations
TAnimationList = array[0..$00FFFFFF] of TPHXAnimation;

// A container for a list of animations
//------------------------------------------------------------------------------
TPHXAnimationList = class
  private
    FList  : TList;
    FImages: TPHXImageList;

    function GetCount: Integer;
    function GetList: PAnimationList;
    function GetItem(const Index: Integer): TPHXAnimation;
  public
    constructor Create(AImages: TPHXImageList);
    destructor Destroy; override;

    // Clear and remove all animations
    procedure Clear;

    // Add and load an animation from a file
    function LoadAnimation(const FileName: String): TPHXAnimation;

    // Add a new animation
    function Add(const Name: String): TPHXAnimation; overload;
    // Add an existing animation
    function Add(const Animation: TPHXAnimation): TPHXAnimation; overload;

    // Return the index of an animation in the set
    function IndexOf(const Name: String): Integer;

    // Find a animation in the set, returns null if not find
    function Find(const Name: String): TPHXAnimation; overload;
    // Test if the set contains an animation and return it
    function Find(const Name: String; out Animation: TPHXAnimation): Boolean; overload;

    // Imagelist
    property Images: TPHXImageList read FImages;
    // The name of the animation
    property Count: Integer read GetCount;
    // Return a pointer to the internal list
    property List: PAnimationList read GetList;
    // Return an animations from the set
    property Items[const Index: Integer]: TPHXAnimation read GetItem; default;
  end;

// The animation set handles a set of animations with support for queuing animations
//------------------------------------------------------------------------------
TPHXAnimationSet = class
  private
    FName      : String;
    FAnimations: TPHXAnimationList;
    FQueue     : TList;

    FActive: TPHXAnimation;
    FState : TPHXAnimationState;
  public
    constructor Create(AAnimations: TPHXAnimationList);
    destructor Destroy; override;

    // Activate an animation by name
    procedure Activate(const Animation: String); overload;
    // Activate the supplied animation, its not requred to be a member of this animationset
    procedure Activate(const Animation: TPHXAnimation); overload;

    // Queue an animation to be played after the current animation is finished
    procedure Queue(const Animation: String); overload;
    // AQueue an animation to be played after the current animation is finished
    procedure Queue(const Animation: TPHXAnimation); overload;

    // Start the animation
    procedure Start;
    // Stop the animation
    procedure Stop;
    // Reset a animation state to the default value
    procedure Reset;
    // Update a animation state
    procedure Update(FrameTime: Single);

    // Draw the animation using a specified image
    procedure Draw(const X, Y: Integer); overload;
    // Draw the animation using a specified image
    procedure Draw(const Transform: TMatrix4f); overload;

    property Animations: TPHXAnimationList read FAnimations;
    // Get and set the name of the animation set
    property Name: String read FName write FName;
    // Current, active animation
    property Active: TPHXAnimation read FActive write FActive;
    // The animation state
    property State: TPHXAnimationState read FState write FState;
  end;

{$ENDREGION}

implementation

{$REGION 'TPHXPattern'}

//------------------------------------------------------------------------------
class function TPHXPattern.Create(const Name: string; X, Y, Width, Height: Integer): TPHXPattern;
begin
  if Length(Name) > 128 then
  begin
    raise Exception.Create('Maximum pattern name length is 128 characters.');
  end;

  Result.Name   := ShortString(Name);
  Result.X      := X;
  Result.Y      := Y;
  Result.Width  := Width;
  Result.Height := Height;
  Result.Pivot.X:= 0;
  Result.Pivot.Y:= 0;
  Result.Flip   := False;
  Result.Mirror := False;
end;

//------------------------------------------------------------------------------
class function TPHXPattern.Create(const Name: string; X, Y, Width, Height: Integer; const Pivot: TVector2i): TPHXPattern;
begin
  if Length(Name) > 128 then
  begin
    raise Exception.Create('Maximum pattern name length is 128 characters.');
  end;

  Result.Name  :=  ShortString(Name);
  Result.X     := X;
  Result.Y     := Y;
  Result.Width := Width;
  Result.Height:= Height;
  Result.Pivot := Pivot;
  Result.Flip   := False;
  Result.Mirror := False;
end;

{$ENDREGION}

{$REGION 'TPHXPatternList'}

//------------------------------------------------------------------------------
procedure QuickSortPatterns(SortList: PPatternList; L, R: Integer; Compare: TPHXPatternCompare);
var I, J: Integer;
var P, T: TPHXPattern;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while Compare(P, SortList^[I]) > 0 do
        Inc(I);
      while  Compare(P, SortList^[J]) < 0 do
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
      QuickSortPatterns(SortList, L, J, Compare);
    L := I;
  until I >= R;
end;


// TPHXPatternList
//==============================================================================
constructor TPHXPatternList.Create(AImage: TPHXImage);
begin
  FImage   := AImage;
  FCount    :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXPatternList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.EndUpdate;
begin
  Dec(FUpdateCount);

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCapacity + Delta);
end;


//------------------------------------------------------------------------------
procedure TPHXPatternList.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Add(const Value: TPHXPattern );
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Add(const Name: string; X, Y, Width, Height: Integer );
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1].Name   :=  ShortString(Name);
  FList^[Count - 1].X      := X;
  FList^[Count - 1].Y      := Y;
  FList^[Count - 1].Width  := Width;
  FList^[Count - 1].Height := Height;
  FList^[Count - 1].Pivot.X:= 0;
  FList^[Count - 1].Pivot.Y:= 0;
  FList^[Count - 1].Flip   := False;
  FList^[Count - 1].Mirror := False;

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Add(const Name: string; X, Y, Width, Height, PivotX, PivotY: Integer);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1].Name   :=  ShortString(Name);
  FList^[Count - 1].X      := X;
  FList^[Count - 1].Y      := Y;
  FList^[Count - 1].Width  := Width;
  FList^[Count - 1].Height := Height;
  FList^[Count - 1].Pivot.X:= PivotX;
  FList^[Count - 1].Pivot.Y:= PivotY;
  FList^[Count - 1].Flip   := False;
  FList^[Count - 1].Mirror := False;

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Exit;

  FCount := FCount-1;

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXPattern));

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.AddRectangular(const Width, Height: Integer; CenterPivots: Boolean = False);
var X, Y: Integer;
var DX, DY: Integer;
var Pattern: TPHXPattern;
begin
  // Skip blank images
  if (Image.Width = 0) or (Image.Height = 0) then Exit;

  // Avoid infinite loops
  if(Width  < 1) then
  begin
    DX:= 1;
  end else
  begin
    DX:= Width;
  end;

  if(Height < 1) then
  begin
    DY:= 1;
  end else
  begin
    DY:= Height;
  end;

  // Remove all current patterns
  SetCount(0);

  Y:=0;
  while Y < Image.Height do
  begin
    X:=0;
    while X < Image.Width do
    begin
      Pattern.Name    := ShortString( Format('Pattern %d', [ Count]) );
      Pattern.X       := X;
      Pattern.Y       := Y;
      Pattern.Width   := Width;
      Pattern.Height  := Height;

      if CenterPivots then
      begin
        Pattern.Pivot.X:= Trunc(Width  * 0.5);
        Pattern.Pivot.Y:= Trunc(Height * 0.5);
      end else
      begin
        Pattern.Pivot.X:= 0;
        Pattern.Pivot.Y:= 0;
      end;
      Pattern.Flip  := False;
      Pattern.Mirror:= False;

      Add(Pattern);

      Inc(X, DX);
    end;
    Inc(Y, DY);
  end;

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.AddRectangular(const Width, Height: Integer; PivotX, PivotY: Integer);
var X, Y: Integer;
var Pattern: TPHXPattern;
begin
  // Remove all current patterns
  SetCount(0);

  // Skip blank images
  if (Image.Width = 0) or (Image.Height = 0) then Exit;

  // Avoid infinite loops
  if(Width  < 1) or (Height < 1) then Exit;

  Y:=0;
  while Y < Image.Height do
  begin
    X:=0;
    while X < Image.Width do
    begin
      Pattern.Name   := ShortString( Format('Pattern %d', [ Count]) );
      Pattern.X      := X;
      Pattern.Y      := Y;
      Pattern.Width  := Width;
      Pattern.Height := Height;
      Pattern.Pivot.X:= PivotX;
      Pattern.Pivot.Y:= PivotY;
      Pattern.Flip   := False;
      Pattern.Mirror := False;

      Add(Pattern);

      Inc(X, Width);
    end;
    Inc(Y, Height);
  end;

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

procedure TPHXPatternList.AddSingle(CenterPivot: Boolean = False);
begin
  AddSingle(Image.Name, CenterPivot);
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.AddSingle(const Name: String; CenterPivot: Boolean = False);
var Pattern: TPHXPattern;
begin
  Pattern.Name    := ShortString(Name);
  Pattern.X       := 0;
  Pattern.Y       := 0;
  Pattern.Width   := Image.Width;
  Pattern.Height  := Image.Height;

  if CenterPivot then
  begin
    Pattern.Pivot.X := Trunc(Image.Width  * 0.5);
    Pattern.Pivot.Y := Trunc(Image.Height * 0.5);
  end else
  begin
    Pattern.Pivot.X := 0;
    Pattern.Pivot.Y := 0;
  end;
  Pattern.Flip  := False;
  Pattern.Mirror:= False;

  Add(Pattern);

  if FUpdateCount = 0 then
  begin
    FImage.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.LoadFromStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  for Index:=0 to FCount - 1 do
  begin
    Stream.Read(FList^[Index].Name    , SizeOf(FList^[Index].Name) );
    Stream.Read(FList^[Index].X       , SizeOf(FList^[Index].X) );
    Stream.Read(FList^[Index].Y       , SizeOf(FList^[Index].Y) );
    Stream.Read(FList^[Index].Width   , SizeOf(FList^[Index].Width) );
    Stream.Read(FList^[Index].Height  , SizeOf(FList^[Index].Height) );
    Stream.Read(FList^[Index].Pivot   , SizeOf(FList^[Index].Pivot) );
    Stream.Read(FList^[Index].Flip    , SizeOf(FList^[Index].Flip) );
    Stream.Read(FList^[Index].Mirror  , SizeOf(FList^[Index].Mirror) );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.SaveToStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Write(FCount, SizeOf(FCount));

  for Index:=0 to FCount - 1 do
  begin
    Stream.Write(FList^[Index].Name    , SizeOf(FList^[Index].Name) );
    Stream.Write(FList^[Index].X       , SizeOf(FList^[Index].X) );
    Stream.Write(FList^[Index].Y       , SizeOf(FList^[Index].Y) );
    Stream.Write(FList^[Index].Width   , SizeOf(FList^[Index].Width) );
    Stream.Write(FList^[Index].Height  , SizeOf(FList^[Index].Height) );
    Stream.Write(FList^[Index].Pivot   , SizeOf(FList^[Index].Pivot) );
    Stream.Write(FList^[Index].Flip    , SizeOf(FList^[Index].Flip) );
    Stream.Write(FList^[Index].Mirror  , SizeOf(FList^[Index].Mirror) );
  end;
end;


//------------------------------------------------------------------------------
function TPHXPatternList.IndexOf(const Name: string): Integer;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result:= Index;

      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXPatternList.Find(const Name: String): TPHXPatternIndex;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result:= Index;

      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXPatternList.Find(const Name: string; out Pattern: TPHXPattern): Boolean;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result := True;
      Pattern:= FList^[Index];

      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Sort(Compare: TPHXPatternCompare);
begin
  if (FList <> nil) and (Count > 0) then
  begin
    QuickSortPatterns(FList, 0, Count - 1, Compare);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.Assign(Patterns: TPHXPatternList);
var Index: Integer;
begin
  SetCapacity(Patterns.Count);
  SetCount   (Patterns.Count);

  for Index := 0 to Patterns.Count - 1 do
  begin
    FList^[Index]:= Patterns.FList^[Index];
  end;
end;

// Returns the bounding box for the pattern at index
//------------------------------------------------------------------------------
function TPHXPatternList.GetPatternBounds(const Index: Integer): TRectf;
begin
  Assert( (Index >= 0) and (Index < FCount) );

  Result.Left  :=                      - FList^[Index].Pivot.X;
  Result.Top   :=                      - FList^[Index].Pivot.Y;
  Result.Right := FList^[Index].Width  - FList^[Index].Pivot.X;
  Result.Bottom:= FList^[Index].Height - FList^[Index].Pivot.Y;
end;

//------------------------------------------------------------------------------
function TPHXPatternList.GetPatternAt(const X, Y: Integer): Integer;
var Index: Integer;
var Rect: TRecti;
begin
  For Index:=0 to FCount-1 do
  begin
    Rect.Left  := FList^[Index].X;
    Rect.Right := FList^[Index].X + FList^[Index].Width;
    Rect.Top   := FList^[Index].Y;
    Rect.Bottom:= FList^[Index].Y +  FList^[Index].Height;

    if PointInRect(X,Y, Rect )  then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXPattern));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;



//------------------------------------------------------------------------------
function TPHXPatternList.GetItem(Index: Integer): TPHXPattern;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXPatternList.SetItem(Index: Integer; const Value: TPHXPattern);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXTag'}

//------------------------------------------------------------------------------
class function TPHXTag.Create(const Name: String; X, Y: Single; const Pattern: Integer; Rotation: Single): TPHXTag;
begin
  if Length(Name) > 32 then
  begin
    raise Exception.Create('Maximum tag name length is 32 characters.');
  end;

  Result.Name    :=  ShortString(Name);
  Result.X       := X;
  Result.Y       := Y;
  Result.Rotation:= Rotation;
  Result.Pattern := Pattern;
end;


{$ENDREGION}

{$REGION 'TPHXTagList'}

// TPHXTagList
//==============================================================================
constructor TPHXTagList.Create(const Image: TPHXImage);
begin
  FImage   := Image;
  FCount   :=0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXTagList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.Grow;
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
procedure TPHXTagList.Add(const Value: TPHXTag );
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.Add(const Name: String; const X, Y: Integer);
begin
  SetCount(Count + 1);

  FList^[Count - 1].Name    := AnsiString(Name);
  FList^[Count - 1].X       := X;
  FList^[Count - 1].Y       := Y;
  FList^[Count - 1].Rotation:= 0;
  FList^[Count - 1].Pattern := PHXPATTERN_NONE;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.Add(const Name: String; const X, Y: Integer; const Pattern: Integer);
begin
  SetCount(Count + 1);

  FList^[Count - 1].Name    := AnsiString(Name);
  FList^[Count - 1].X       := X;
  FList^[Count - 1].Y       := Y;
  FList^[Count - 1].Rotation:= 0;
  FList^[Count - 1].Pattern := Pattern;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.Delete(Index: Integer);
begin
  If (Index < 0) or (Index >= FCount) then Exit;

  FCount := FCount-1;

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXTag));
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.LoadFromFile(const FileName: String);
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
procedure TPHXTagList.SaveToFile(const FileName: String);
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
procedure TPHXTagList.LoadFromStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  For Index:=0 to FCount-1 do
  begin
    Stream.Read(FList^[Index].Name    , SizeOf(FList^[Index].Name) );
    Stream.Read(FList^[Index].Pattern, SizeOf(FList^[Index].Pattern) );
    Stream.Read(FList^[Index].X       , SizeOf(FList^[Index].X) );
    Stream.Read(FList^[Index].Y       , SizeOf(FList^[Index].Y) );
    Stream.Read(FList^[Index].Rotation, SizeOf(FList^[Index].Rotation) );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.SaveToStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Write(FCount, SizeOf(FCount));

//  Stream.Write(FList^, SizeOf(TPHXTag) * FCount);
  For Index:=0 to FCount-1 do
  begin
    Stream.Write(FList^[Index].Name    , SizeOf(FList^[Index].Name) );
    Stream.Write(FList^[Index].Pattern , SizeOf(FList^[Index].Pattern) );
    Stream.Write(FList^[Index].X       , SizeOf(FList^[Index].X) );
    Stream.Write(FList^[Index].Y       , SizeOf(FList^[Index].Y) );
    Stream.Write(FList^[Index].Rotation, SizeOf(FList^[Index].Rotation) );
  end;
end;

//------------------------------------------------------------------------------
function TPHXTagList.IndexOf(const Name: String): Integer;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXTagList.Find(const Name: String): TPHXTag;
var Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText(  String(FList^[Index].Name), Name) then
    begin
      Result :=  FList^[Index];
      Exit;
    end;
  end;

  raise Exception.CreateFmt('The tag %s was not found in the image', [Name]);
end;

//------------------------------------------------------------------------------
function TPHXTagList.Find(const Name: String; out Tag: TPHXTag): Boolean;
var Index: Integer;
begin
  for Index:=0 to FCount-1 do
  begin
    if SameText(  String(FList^[Index].Name), Name) then
    begin
      Result := True;
      Tag    :=  FList^[Index];
      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXTagList.FindByPattern(const PatternIndex: Integer; const Indicies: PIntegerList; const Count: Integer): Integer;
var Index: Integer;
begin
  Result:= 0;
  // Empty list
  if Count <= 0 then Exit;

  For Index:=0 to FCount-1 do
  begin
    if FList^[Index].Pattern = PatternIndex then
    begin
      // Add the tag to the list
      Indicies^[Result]:= Index;

      Inc(Result);
      // The list is full, exit
      if Result = Count then Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXTagList.TagToImage(const Tag: TPHXTag; const Position: TVector2f): TVector2f;
begin
  if (Tag.Pattern >= 0) and (Tag.Pattern < Image.Patterns.Count) then
  begin
    Result.X:= Position.X + Image.Patterns.List^[Tag.Pattern].X + Tag.X + Image.Patterns.List^[Tag.Pattern].Pivot.X;
    Result.Y:= Position.Y + Image.Patterns.List^[Tag.Pattern].Y + Tag.Y + Image.Patterns.List^[Tag.Pattern].Pivot.Y;
  end else
  begin
    Result.X:= Position.X + Tag.X;
    Result.Y:= Position.Y + Tag.Y;
  end;
end;

//------------------------------------------------------------------------------
function TPHXTagList.TagAtPosition(const X, Y, Tolerance: Integer): Integer;
var Index   : Integer;
var Rect    : TRecti;
var Position: TVector2f;
begin
  For Index:=0 to FCount-1 do
  begin
    Position:= TagToImage(FList^[Index], Vector2f(0,0) );

    Rect.Left  := Round(Position.X - Tolerance);
    Rect.Top   := Round(Position.Y - Tolerance);
    Rect.Right := Round(Position.X + Tolerance);
    Rect.Bottom:= Round(Position.Y + Tolerance);

    if PointInRect(X,Y, Rect) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.TagTransform(const Index: Integer; out Matrix: TMatrix4f);
begin
  Matrix:= Matrix_CreateRotationZ(List^[Index].Rotation);
  Matrix.v[12]:= List^[Index].X;
  Matrix.v[13]:= List^[Index].Y;
end;


//------------------------------------------------------------------------------
procedure TPHXTagList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXTag));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------

function TPHXTagList.GetItem(Index: Integer): TPHXTag;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXTagList.SetItem(Index: Integer; const Value: TPHXTag);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXImage'}

// TPHXImage
//==============================================================================
constructor TPHXImage.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
begin
  Assert(ADevice <> nil, 'Creating an image with a nil device');
  Assert(ACanvas <> nil, 'Creating an image with a nil canvas');

  FDevice  := ADevice;
  FCanvas  := ACanvas;
  FTexture := ADevice.CreateTexture;
  FName    := 'Image';
  FWidth   := 0;
  FHeight  := 0;
  FPatterns:= TPHXPatternList.Create(Self);
  FTags    := TPHXTagList.Create(Self);

  SetLength(PatternBuffer, 1);
end;

//------------------------------------------------------------------------------
constructor TPHXImage.CreateEx(ATexture: TPHXTexture);
begin
  Assert(ATexture <> nil, 'Creating an image with a nil texture');

  FDevice  := nil;
  FCanvas  := nil;
  FTexture := ATexture;
  FName    := 'Image';
  FWidth   := 0;
  FHeight  := 0;
  FPatterns:= TPHXPatternList.Create(Self);
  FTags    := TPHXTagList.Create(Self);

  SetLength(PatternBuffer, 1);
end;

//------------------------------------------------------------------------------
destructor TPHXImage.Destroy;
begin
  SetLength(PatternBuffer, 0);

  FPatterns.Free;
  FTags.Free;
  FTexture.Free;
  inherited Destroy;
end;


//------------------------------------------------------------------------------
procedure TPHXImage.LoadImage(const FileName: String);
begin
  if SameText(ExtractFileExt(FileName), PHXIMAGE_EXT) then
  begin
    LoadFromFile(FileName);
  end else
  begin
    FTexture.LoadTexture(FileName);

    FTexture.Settings.Mipmaps  := False;
    FTexture.Settings.FilterMag:= tfLinear;
    FTexture.Settings.FilterMin:= tfLinear;
    FTexture.Settings.FilterMag:= tfNearest;
    FTexture.Settings.FilterMin:= tfNearest;
    FTexture.Settings.WrapS :=twClamp;
    FTexture.Settings.WrapT :=twClamp;

    FTexture.Build;

    FName  := ExtractFileName(FileName);
    FWidth := FTexture.Width;
    FHeight:= FTexture.Height;

    Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.LoadImage(const FileName: String; Stream: TStream);
begin
  {$IFDEF LOG_ENABLED}
  TPHXLogger.getInstance.Log(logInfo, 'TPHXImage.LoadImage', 'Loading image: %s (stream)', [FileName]);
  {$ENDIF}

  if SameText(ExtractFileExt(FileName), PHXIMAGE_EXT) then
  begin
    LoadFromStream(Stream);
  end else
  begin
    FTexture.LoadTexture(FileName, Stream);

    FTexture.Settings.Mipmaps  := False;
    FTexture.Settings.FilterMag:= tfLinear;
    FTexture.Settings.FilterMin:= tfLinear;

    FTexture.Build;

    FName  := ExtractFileName(FileName);
    FWidth := FTexture.Width;
    FHeight:= FTexture.Height;

    Initialize;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImage.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckImage, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckImage, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.LoadFromStream(Stream: TStream);
var Header     : TPHXImageHeader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXIMG') then
  begin
    //TPHXLog.Error('TPHXImage.LoadFromStream', _LINE_, 'Not a valid Phoenix image.');

    raise Exception.Create('Not a valid Phoenix image.');
  end;

  If (Header.Version <> PHXIMAGE_VERSION) then
  begin
   // TPHXLog.Error('TPHXImage.LoadFromStream', _LINE_, 'Image version missmatch [File: %d Code: %d].', [Header.Version, PHXIMAGE_VERSION]);

    raise Exception.CreateFmt('Image version missmatch [File: %d Code: %d].', [Header.Version, PHXIMAGE_VERSION]);
  end;

  // The name of the image.
  StreamReadString(Stream, FName);
  StreamReadString(Stream, FAuthor);
  StreamReadString(Stream, FVersion);
  StreamReadString(Stream, FComment);

  // The width of the image
  Stream.Read(FWidth, SizeOf(FWidth));
  // The height of the image
  Stream.Read(FHeight, SizeOf(FHeight));

  // Load the patterns
  Patterns.LoadFromStream(Stream);
  // Load the tags
  Tags.LoadFromStream(Stream);
  // Load the texture
  Texture.LoadFromStream(Stream);

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.SaveToStream(Stream: TStream);
var Header     : TPHXImageHeader;
begin
  Header.Ident  :='PHXIMG';
  Header.Version:= PHXIMAGE_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  // The name of the image.
  StreamWriteString(Stream, FName);
  StreamWriteString(Stream, FAuthor);
  StreamWriteString(Stream, FVersion);
  StreamWriteString(Stream, FComment);

  // The width of the image
  Stream.Write(FWidth, SizeOf(FWidth));
  // The height of the image
  Stream.Write(FHeight, SizeOf(Height));
  // Save the patterns
  Patterns.SaveToStream(Stream);
  // Save the tags
  Tags.SaveToStream(Stream);

  // Save the texture
  Texture.SaveToStream(Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Initialize;
var Index  : Integer;
var Pattern: PPHXPattern;
var WInv: Single;
var HInv: Single;
begin
  FWidth := FTexture.Width;
  FHeight:= FTexture.Height;

  // Avoid division by zero
  if (Width = 0) or (Height = 0) then
  begin
    WInv:= 0;
    HInv:= 0;
  end else
  begin
    WInv:= 1 / Width;
    HInv:= 1 / Height;
  end;

  SetLength(PatternBuffer, Patterns.Count + 1);

  // Add the full image as the first pattern
  PatternBuffer[0].TexCoord.Left  := 0.0;
  PatternBuffer[0].TexCoord.Top   := 0.0;
  PatternBuffer[0].TexCoord.Right := 1.0;
  PatternBuffer[0].TexCoord.Bottom:= 1.0;

  PatternBuffer[0].Position.Left  := 0;
  PatternBuffer[0].Position.Top   := 0;
  PatternBuffer[0].Position.Right := Width;
  PatternBuffer[0].Position.Bottom:= Height;

  for Index:=0 to Patterns.Count - 1 do
  begin
    Pattern:= @Patterns.List^[Index];

    if Pattern^.Mirror then
    begin
      // Mirror and flip
      if Pattern^.Flip then
      begin
        PatternBuffer[Index+1].TexCoord.Left := (Pattern^.X + Pattern^.Width ) * WInv;
        PatternBuffer[Index+1].TexCoord.Right:= (Pattern^.X                  ) * WInv;

        PatternBuffer[Index+1].TexCoord.Top   := (Pattern^.Y + Pattern^.Height) * HInv;
        PatternBuffer[Index+1].TexCoord.Bottom:= (Pattern^.Y                  ) * HInv;
      end else
      // Mirror
      begin
        PatternBuffer[Index+1].TexCoord.Left  := (Pattern^.X + Pattern^.Width ) * WInv;
        PatternBuffer[Index+1].TexCoord.Right := (Pattern^.X                  ) * WInv;

        PatternBuffer[Index+1].TexCoord.Top   := (Pattern^.Y                  ) * HInv;
        PatternBuffer[Index+1].TexCoord.Bottom:= (Pattern^.Y + Pattern^.Height) * HInv;
      end;
    end else
    begin
      // Flip
      if Pattern^.Flip then
      begin
        PatternBuffer[Index+1].TexCoord.Left  := (Pattern^.X                  ) * WInv;
        PatternBuffer[Index+1].TexCoord.Right := (Pattern^.X + Pattern^.Width ) * WInv;

        PatternBuffer[Index+1].TexCoord.Top   := (Pattern^.Y + Pattern^.Height) * HInv;
        PatternBuffer[Index+1].TexCoord.Bottom:= (Pattern^.Y                  ) * HInv;
      end else
      // No mirror and no flip
      begin
        PatternBuffer[Index+1].TexCoord.Left  := (Pattern^.X                  ) * WInv;
        PatternBuffer[Index+1].TexCoord.Right := (Pattern^.X + Pattern^.Width ) * WInv;

        PatternBuffer[Index+1].TexCoord.Top   := (Pattern^.Y                  ) * HInv;
        PatternBuffer[Index+1].TexCoord.Bottom:= (Pattern^.Y + Pattern^.Height) * HInv;
      end;
    end;

    PatternBuffer[Index+1].Position.Left  := 0               - Pattern^.Pivot.X;
    PatternBuffer[Index+1].Position.Top   := 0               - Pattern^.Pivot.Y;
    PatternBuffer[Index+1].Position.Right := Pattern^.Width  - Pattern^.Pivot.X;
    PatternBuffer[Index+1].Position.Bottom:= Pattern^.Height - Pattern^.Pivot.Y;
  end;


end;


////////////////////////////////////////////////////////////////////////////////
// Rendering functions                                                        //
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
procedure TPHXImage.DrawPattern(const Pattern: TPHXPatternBuffer; const Position: TVector3f; const Color: TColor4f);
var Vertices: array[0..3] of TPHXVertex;
var Indicies: array[0..5] of TPHXIndex;
begin
  // Top - left
  Vertices[0].Position.X:= Position.X + Pattern.Position.Left;
  Vertices[0].Position.Y:= Position.Y + Pattern.Position.Top;
  Vertices[0].Position.Z:= Position.Z + 0;
  Vertices[0].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[0].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[0].Color     := Color;
  Vertices[0].Normal    := Vector3f_AxisZ;

  // Top - right
  Vertices[1].Position.X:= Position.X + Pattern.Position.Right;
  Vertices[1].Position.Y:= Position.Y + Pattern.Position.Top;
  Vertices[1].Position.Z:= Position.Z + 0;
  Vertices[1].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[1].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[1].Color     := Color;
  Vertices[1].Normal    := Vector3f_AxisZ;

  // Bottom - Left
  Vertices[2].Position.X:= Position.X + Pattern.Position.Left;
  Vertices[2].Position.Y:= Position.Y + Pattern.Position.Bottom;
  Vertices[2].Position.Z:= Position.Z + 0;
  Vertices[2].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[2].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[2].Color     := Color;
  Vertices[2].Normal    := Vector3f_AxisZ;

  // Bottom - Right
  Vertices[3].Position.X:= Position.X + Pattern.Position.Right;
  Vertices[3].Position.Y:= Position.Y + Pattern.Position.Bottom;
  Vertices[3].Position.Z:= Position.Z + 0;
  Vertices[3].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[3].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[3].Color     := Color;
  Vertices[3].Normal    := Vector3f_AxisZ;

  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, FTexture);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawPattern(const Pattern: TPHXPatternBuffer; const Position: TVector3f; const Scale: TVector2f; const Color: TColor4f);
var Vertices: array[0..3] of TPHXVertex;
var Indicies: array[0..5] of TPHXIndex;
begin
  // Top - left
  Vertices[0].Position.X:= Position.X + Pattern.Position.Left * Scale.X;
  Vertices[0].Position.Y:= Position.Y + Pattern.Position.Top  * Scale.Y;
  Vertices[0].Position.Z:= Position.Z + 0;
  Vertices[0].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[0].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[0].Color     := Color;
  Vertices[0].Normal    := Vector3f_AxisZ;

  // Top - right
  Vertices[1].Position.X:= Position.X + Pattern.Position.Right * Scale.X;
  Vertices[1].Position.Y:= Position.Y + Pattern.Position.Top   * Scale.Y;
  Vertices[1].Position.Z:= Position.Z + 0;
  Vertices[1].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[1].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[1].Color     := Color;
  Vertices[1].Normal    := Vector3f_AxisZ;

  // Bottom - Left
  Vertices[2].Position.X:= Position.X + Pattern.Position.Left    * Scale.X;
  Vertices[2].Position.Y:= Position.Y + Pattern.Position.Bottom  * Scale.Y;
  Vertices[2].Position.Z:= Position.Z + 0;
  Vertices[2].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[2].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[2].Color     := Color;
  Vertices[2].Normal    := Vector3f_AxisZ;

  // Bottom - Right
  Vertices[3].Position.X:= Position.X + Pattern.Position.Right  * Scale.X;
  Vertices[3].Position.Y:= Position.Y + Pattern.Position.Bottom * Scale.Y;
  Vertices[3].Position.Z:= Position.Z + 0;
  Vertices[3].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[3].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[3].Color     := Color;
  Vertices[3].Normal    := Vector3f_AxisZ;

  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, FTexture);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawPattern(const Pattern: TPHXPatternBuffer; const Transform: TMatrix4f; const Color: TColor4f);
var Vertices: array[0..3] of TPHXVertex;
var Indicies: array[0..5] of TPHXIndex;
begin
  // Top - left
  Vertices[0].Position.X:= Pattern.Position.Left;
  Vertices[0].Position.Y:= Pattern.Position.Top;
  Vertices[0].Position.Z:= 0;
  Vertices[0].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[0].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[0].Color     := Color;
  Vertices[0].Normal    := Vector3f_AxisZ;

  // Top - right
  Vertices[1].Position.X:= Pattern.Position.Right;
  Vertices[1].Position.Y:= Pattern.Position.Top;
  Vertices[1].Position.Z:= 0;
  Vertices[1].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[1].TexCoord.Y:= Pattern.TexCoord.Top;
  Vertices[1].Color     := Color;
  Vertices[1].Normal    := Vector3f_AxisZ;

  // Bottom - Left
  Vertices[2].Position.X:= Pattern.Position.Left;
  Vertices[2].Position.Y:= Pattern.Position.Bottom;
  Vertices[2].Position.Z:= 0;
  Vertices[2].TexCoord.X:= Pattern.TexCoord.Left;
  Vertices[2].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[2].Color     := Color;
  Vertices[2].Normal    := Vector3f_AxisZ;

  // Bottom - Right
  Vertices[3].Position.X:= Pattern.Position.Right;
  Vertices[3].Position.Y:= Pattern.Position.Bottom;
  Vertices[3].Position.Z:= 0;
  Vertices[3].TexCoord.X:= Pattern.TexCoord.Right;
  Vertices[3].TexCoord.Y:= Pattern.TexCoord.Bottom;
  Vertices[3].Color     := Color;
  Vertices[3].Normal    := Vector3f_AxisZ;

  // Transform the vertex positions
  Vertices[0].Position:= Matrix_Transform(Transform, Vertices[0].Position);
  Vertices[1].Position:= Matrix_Transform(Transform, Vertices[1].Position);
  Vertices[2].Position:= Matrix_Transform(Transform, Vertices[2].Position);
  Vertices[3].Position:= Matrix_Transform(Transform, Vertices[3].Position);

  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, FTexture);
end;


//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawPattern(PatternBuffer[0], Position, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single; const PatternIndex: Integer);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    Draw(X, Y, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single; const Color: TColor4f);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawPattern(PatternBuffer[0], Position, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single; const Color : TColor4f; const PatternIndex : Integer);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.Draw(const X, Y: Single; const Color : TColor4f; const PatternName: String);
var PatternIndex: Integer;
var Position: TVector3f;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    Position.X:= X;
    Position.Y:= Y;
    Position.Z:= 0;

    DrawPattern(PatternBuffer[PatternIndex+1], Position, Color);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawRotate(const X, Y: Single; const Angle: Single);
var Transform: TMatrix4f;
begin
  Transform:= Matrix_CreateRotationZ(Angle);
  Transform.v[12]:= X;
  Transform.v[13]:= Y;
  Transform.v[14]:= 0;

  DrawPattern(PatternBuffer[0], Transform, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawRotate(const X, Y: Single; const Angle: Single; const PatternIndex: Cardinal);
var Transform: TMatrix4f;
begin
  Transform:= Matrix_CreateRotationZ(Angle);
  Transform.v[12]:= X;
  Transform.v[13]:= Y;
  Transform.v[14]:= 0;

  DrawPattern(PatternBuffer[PatternIndex+1], Transform, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawRotate(const X, Y: Single; const Angle: Single; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawRotate(X, Y, Angle, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f);
begin
  DrawPattern(PatternBuffer[0], Transform, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f; const PatternIndex: Integer);
begin
  DrawPattern(PatternBuffer[PatternIndex+1], Transform, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawPattern(PatternBuffer[PatternIndex+1], Transform, clrWhite);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f; const Color: TColor4f);
begin
  DrawPattern(PatternBuffer[0], Transform, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f; const Color: TColor4f; const PatternIndex: Integer);
begin
  DrawPattern(PatternBuffer[PatternIndex+1], Transform, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawTransform(const Transform: TMatrix4f; const Color: TColor4f; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawPattern(PatternBuffer[PatternIndex+1], Transform, Color);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.TileDraw(const X, Y: Single; const NumX, NumY: Integer);
var Position: TVector3f;
var cx, cy: Integer;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  For CY:=1 To NumY do
  begin
    Position.X:= X;
    For CX:=1 To NumX do
    begin
      DrawPattern(PatternBuffer[0], Position, clrWhite);

      Position.X:= Position.X + FWidth;
    end;
    Position.Y:= Position.Y + FHeight;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.TileDraw(const X, Y: Single; const NumX, NumY: Integer; const PatternIndex: Integer);
var Position: TVector3f;
var cx, cy: Integer;
begin
  Assert( (PatternIndex >= 0) and (PatternIndex < Patterns.Count), 'Invalid pattern index.');

  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  for cy:= 1 to NumY do
  begin
    Position.X:= X;

    for cx:= 1 to NumX do
    begin
      DrawPattern(PatternBuffer[PatternIndex+1], Position, clrWhite);

      Position.X:= Position.X + Patterns.List^[PatternIndex].Width;
    end;

    Position.Y:= Position.Y + Patterns.List^[PatternIndex].Height;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.TileDraw(const X, Y: Single; const NumX, NumY: Integer; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    TileDraw(X, Y, NumX, NumY, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.StrechDraw(const X, Y: Single; const W, H: Integer);
var Position: TVector3f;
var Scale   : TVector2f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Scale.X:= W / Width;
  Scale.Y:= H / Height;

  DrawPattern(PatternBuffer[0], Position, Scale, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.StrechDraw(const X, Y: Single; const W, H: Integer; const PatternIndex: Integer);
var Position: TVector3f;
var Scale   : TVector2f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Scale.X:= W / Patterns.List^[PatternIndex].Width;
  Scale.Y:= H / Patterns.List^[PatternIndex].Height;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, Scale, clrWhite);
 end;

 //------------------------------------------------------------------------------
procedure TPHXImage.StrechDraw(const X, Y: Single; const W, H: Integer; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    StrechDraw(X, Y, W, H, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAdd(const X, Y: Single; const Alpha: Single);
var Position: TVector3f;
var Color   : TColor4f;
var Blending: TPHXBlendMode;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  // Store the blend mode
  Blending:= Canvas.Blending;

  Canvas.Blending:= bmAdditive;
  // Change to additive blending
  DrawPattern(PatternBuffer[0], Position, Color);

  // Restore the blend mode
  Canvas.Blending:= Blending;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAdd(const X, Y: Single; const Alpha: Single; const PatternIndex: Integer);
var Position: TVector3f;
var Color   : TColor4f;
var Blending: TPHXBlendMode;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  // Store the blend mode
  Blending:= Canvas.Blending;
  // Change to additive blending
  Canvas.Blending:= bmAdditive;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, Color);

  // Restore the blend mode
  Canvas.Blending:= Blending;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAdd(const X, Y, Alpha: Single; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawAdd(X, Y, Alpha, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAlpha(const X, Y: Single; const Alpha: Single);
var Position: TVector3f;
var Color   : TColor4f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  DrawPattern(PatternBuffer[0], Position, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAlpha(const X, Y: Single; const Alpha: Single; const PatternIndex: Integer);
var Position: TVector3f;
var Color   : TColor4f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawAlpha(const X, Y, Alpha: Single; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawAlpha(X, Y, Alpha, PatternIndex);
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXImage.DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode);
var Position: TVector3f;
var Blending: TPHXBlendMode;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  // Store the blend mode
  Blending:= Canvas.Blending;

  Canvas.Blending:= Blend;
  // Change to additive blending
  DrawPattern(PatternBuffer[0], Position, Color);

  // Restore the blend mode
  Canvas.Blending:= Blending;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode; const PatternIndex: Integer);
var Position: TVector3f;
var Blending: TPHXBlendMode;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  // Store the blend mode
  Blending:= Canvas.Blending;
  // Change to additive blending
  Canvas.Blending:= Blend;

  DrawPattern(PatternBuffer[PatternIndex+1], Position, Color);

  // Restore the blend mode
  Canvas.Blending:= Blending;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawBlend(const X, Y: Single; const Color: TColor4f; const Blend: TPHXBlendMode; const PatternName: String);
var PatternIndex: Integer;
begin
  PatternIndex:= Patterns.IndexOf(PatternName);

  if PatternIndex <> -1 then
  begin
    DrawBlend(X, Y, Color, Blend, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawArea(const X, Y: Integer; const Area: TRecti);
var Rect: TRectf;
begin
  Rect.Left  := X;
  Rect.Right := X + (Area.Right  - Area.Left);
  Rect.Top   := Y;
  Rect.Bottom:= Y + (Area.Bottom - Area.Top);

  DrawArea(Rect, Area);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawArea(const Rect: TRectf; const Area: TRecti);
var TexCoord: TRectf;
var Vertices: array[0..3] of TPHXVertex;
var Indicies: array[0..5] of TPHXIndex;
begin
  TexCoord.Left  := (Area.Left  ) / FWidth;
  TexCoord.Right := (Area.Right ) / FWidth;
  TexCoord.Top   := (Area.Top   ) / FHeight;
  TexCoord.Bottom:= (Area.Bottom) / FHeight;

  // Top - left
  Vertices[0].Position.X:= Rect.Left;
  Vertices[0].Position.Y:= Rect.Top;
  Vertices[0].Position.Z:= 0;
  Vertices[0].TexCoord.X:= TexCoord.Left;
  Vertices[0].TexCoord.Y:= TexCoord.Top;
  Vertices[0].Color     := clrWhite;
  Vertices[0].Normal    := Vector3f_AxisZ;

  // Top - right
  Vertices[1].Position.X:= Rect.Right;
  Vertices[1].Position.Y:= Rect.Top;
  Vertices[1].Position.Z:= 0;
  Vertices[1].TexCoord.X:= TexCoord.Right;
  Vertices[1].TexCoord.Y:= TexCoord.Top;
  Vertices[1].Color     := clrWhite;
  Vertices[1].Normal    := Vector3f_AxisZ;

  // Bottom - Right
  Vertices[2].Position.X:= Rect.Right;
  Vertices[2].Position.Y:= Rect.Bottom;
  Vertices[2].Position.Z:= 0;
  Vertices[2].TexCoord.X:= TexCoord.Right;
  Vertices[2].TexCoord.Y:= TexCoord.Bottom;
  Vertices[2].Color     := clrWhite;
  Vertices[2].Normal    := Vector3f_AxisZ;

  // Bottom - Left
  Vertices[3].Position.X:= Rect.Left;
  Vertices[3].Position.Y:= Rect.Bottom;
  Vertices[3].Position.Z:= 0;
  Vertices[3].TexCoord.X:= TexCoord.Left;
  Vertices[3].TexCoord.Y:= TexCoord.Bottom;
  Vertices[3].Color     := clrWhite;
  Vertices[3].Normal    := Vector3f_AxisZ;

  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, FTexture);
end;




              {
//------------------------------------------------------------------------------
procedure TPHXImage.DrawEx(const Position: TVector3f; const Scale: TVector2f; const Rotation: Single; const Color: TColor4f);
var Transform: TMatrix4f;
begin
  Transform:= Matrix_RotationZ(Rotation);
  Transform[12]:= Position.X;
  Transform[13]:= Position.Y;
  Transform[14]:= Position.Z;

  DrawPattern(Buffer, PatternBuffer[0], Transform, Scale, Color);

  if not FBatching then Flush;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawEx(const Position: TVector3f; const Scale: TVector2f; const Rotation: Single; const Color: TColor4f; const PatternIndex: Integer);
var Transform: TMatrix4f;
begin
  Transform:= Matrix_RotationZ(Rotation);
  Transform[12]:= Position.X;
  Transform[13]:= Position.Y;
  Transform[14]:= Position.Z;

  DrawPattern(Buffer, PatternBuffer[PatternIndex+1], Transform, Scale, Color);

  if not FBatching then Flush;
end;
              }


(*

//------------------------------------------------------------------------------
procedure TPHXImage.TileDrawAdd(const X, Y: Single; const NumX, NumY: Integer; const Alpha : Single);
var Position: TVector3f;
var cx, cy: Integer;
var Color   : TColor4f;
begin
  Flush;

  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  For CY:=1 To NumY do
  begin
    Position.X:= X;
    For CX:=1 To NumX do
    begin
      DrawPattern(Buffer, PatternBuffer[0], Position, Color);

      Position.X:= Position.X + FWidth;
    end;
    Position.Y:= Position.Y + FHeight;
  end;
  FDevice.SetRenderState(PHX_BLENDMODE, PHX_BLENDMODE_ADDITIVE);
  FDevice.SetRenderState(PHX_DEPTHTEST, PHX_DEPTHTEST_OFF);

  FDevice.SetTexture(Texture);

  FDevice.Render(PHX_QUADS, Buffer);

  Buffer.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.TileDrawAdd(const X, Y: Single; const NumX, NumY: Integer; const Alpha : Single; const PatternIndex: Integer);
var Position: TVector3f;
var cx, cy: Integer;
var Color   : TColor4f;
begin
  Flush;

  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  Color.Red  := 1.0;
  Color.Green:= 1.0;
  Color.Blue := 1.0;
  Color.Alpha:= Alpha;

  For CY:=1 To NumY do
  begin
    Position.X:= X;
    For CX:=1 To NumX do
    begin
      DrawPattern(Buffer, PatternBuffer[PatternIndex+1], Position, Color);

      Position.X:= Position.X + Patterns.List^[PatternIndex].Width;
    end;
    Position.Y:= Position.Y + Patterns.List^[PatternIndex].Height;
  end;
  FDevice.SetRenderState(PHX_BLENDMODE, PHX_BLENDMODE_ADDITIVE);
  FDevice.SetRenderState(PHX_DEPTHTEST, PHX_DEPTHTEST_OFF);

  FDevice.SetTexture(Texture);

  FDevice.Render(PHX_QUADS, Buffer);

  Buffer.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawWaveHorz(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer);
begin
  Assert( Assigned(WaveFunct) );

  DrawWaveHorz(Rect, WaveFunct, Time, Detail, -1);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawWaveHorz(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer; const PatternIndex: Integer);
var Delta   : Single;
var Position: Single;
var X,Y     : Single;
var W,H     : Single;
var Index    : Integer;
begin
  Assert( Assigned(WaveFunct) );

  X       := 0;
//  Y       := 0;
  W       := Rect.Right - Rect.Left;
  H       := Rect.Bottom- Rect.Top;

  Delta   := Detail / W;
  Position:= 0;
  repeat
    Y:= WaveFunct( Frac(Position + Time) ) *  H;

    Index:= Buffer.Alloc(4);

    // Top - left
    Buffer.List^[Index].Position.X:= Rect.Left + X + PatternBuffer[PatternIndex+1].Position.Left;
    Buffer.List^[Index].Position.Y:= Rect.Top  + Y + PatternBuffer[PatternIndex+1].Position.Top;
    Buffer.List^[Index].Position.Z:= 0;

    Buffer.List^[Index].TexCoord.X:= PatternBuffer[PatternIndex+1].TexCoord.Left;
    Buffer.List^[Index].TexCoord.Y:= PatternBuffer[PatternIndex+1].TexCoord.Top;

    Buffer.List^[Index].Color:= clrWhite;

    Inc(Index);

    // Top-right
    Buffer.List^[Index].Position.X:= Rect.Left + X + PatternBuffer[PatternIndex+1].Position.Left + Detail;
    Buffer.List^[Index].Position.Y:= Rect.Top  + Y + PatternBuffer[PatternIndex+1].Position.Top;
    Buffer.List^[Index].Position.Z:= 0;

    Buffer.List^[Index].TexCoord.X:= PatternBuffer[PatternIndex+1].TexCoord.Right;
    Buffer.List^[Index].TexCoord.Y:= PatternBuffer[PatternIndex+1].TexCoord.Top;

    Buffer.List^[Index].Color:= clrWhite;

    Inc(Index);

    // Bottom - right
    Buffer.List^[Index].Position.X:= Rect.Left + X + PatternBuffer[PatternIndex+1].Position.Left + Detail;
    Buffer.List^[Index].Position.Y:= Rect.Top  + Y + PatternBuffer[PatternIndex+1].Position.Top  + H;
    Buffer.List^[Index].Position.Z:= 0;

    Buffer.List^[Index].TexCoord.X:= PatternBuffer[PatternIndex+1].TexCoord.Right;
    Buffer.List^[Index].TexCoord.Y:= PatternBuffer[PatternIndex+1].TexCoord.Bottom;

    Buffer.List^[Index].Color:= clrWhite;

    Inc(Index);

    // Bottom - left
    Buffer.List^[Index].Position.X:= Rect.Left + X + PatternBuffer[PatternIndex+1].Position.Left;
    Buffer.List^[Index].Position.Y:= Rect.Top  + Y + PatternBuffer[PatternIndex+1].Position.Top + H;
    Buffer.List^[Index].Position.Z:= 0;

    Buffer.List^[Index].TexCoord.X:= PatternBuffer[PatternIndex+1].TexCoord.Left;
    Buffer.List^[Index].TexCoord.Y:= PatternBuffer[PatternIndex+1].TexCoord.Bottom;

    Buffer.List^[Index].Color:= clrWhite;

//    Inc(Index);

        //Dest.Color3f(Position, Position, Position);

   // Dest.TexCoord2f(0,0); Dest.Vertex2f( X, Y + Offset);
   // Dest.TexCoord2f(0,0); Dest.Vertex2f( X, Y + Offset + Height);

    X:= X + Detail;

    Position:= Position + Delta;
  until Position > 1.0;

  if not FBatching then Flush;
//  Dest.Render(GL_QUAD_STRIP);
end;

//------------------------------------------------------------------------------
procedure TPHXImage.DrawWaveVert(const Rect: TRecti; const WaveFunct: TWaveFunc; const Time: Single; const Detail: Integer);
begin
  Assert( Assigned(WaveFunct) );
end;

*)

//------------------------------------------------------------------------------
procedure TPHXImage.ChangePivot(const PatternIndex: Integer; const Pivot: TVector2i);
begin
  Assert( (PatternIndex >= 0) and (PatternIndex < Patterns.Count), 'List index out of bounds');

  Patterns.List^[PatternIndex].Pivot:= Pivot;

  PatternBuffer[PatternIndex+1].Position.Left  := 0                                   - Patterns.List^[PatternIndex].Pivot.X;
  PatternBuffer[PatternIndex+1].Position.Top   := 0                                   - Patterns.List^[PatternIndex].Pivot.Y;
  PatternBuffer[PatternIndex+1].Position.Right := Patterns.List^[PatternIndex].Width  - Patterns.List^[PatternIndex].Pivot.X;
  PatternBuffer[PatternIndex+1].Position.Bottom:= Patterns.List^[PatternIndex].Height - Patterns.List^[PatternIndex].Pivot.Y;
end;

{$ENDREGION}

{$REGION 'TPHXImageList'}

// TPHXImageList
//==============================================================================
constructor TPHXImageList.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
begin
  Assert(Assigned(ADevice));
  Assert(Assigned(ACanvas));

  FDevice:= ADevice;
  FCanvas:= ACanvas;

  FItems:= TList.Create;
end;

//------------------------------------------------------------------------------
constructor TPHXImageList.CreateEx;
begin
  FDevice:= nil;
  FCanvas:= nil;

  FItems:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXImageList.Destroy;
begin
  Clear;

  FreeAndNil(FItems);

  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXImageList.SaveList(const Stream: TStream; const Count: Integer);
var Index: Integer;
var Image: TPHXImage;
begin
  for Index:=0 to Count - 1 do
  begin
    Image:=TPHXImage( FItems.List[Index]);

    Image.SaveToStream(Stream);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageList.LoadList(const Stream: TStream; const Count: Integer);
var Index: Integer;
var Image: TPHXImage;
begin
  Clear;
  for Index:=0 to Count - 1 do
  begin
    Image:= TPHXImage.Create(Device, Canvas);

    Image.LoadFromStream(Stream);

    FItems.Add(Image);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImageList.Clear;
var Index: Integer;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    TPHXImage( FItems.List[Index]).Free;
  end;
  FItems.Clear;
end;

//------------------------------------------------------------------------------
function TPHXImageList.Add: TPHXImage;
begin
  Result:= TPHXImage.Create(Device, Canvas);

  FItems.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXImageList.Add(const Name: string): TPHXImage;
begin
  Result:= TPHXImage.Create(Device, Canvas);
  Result.Name:=Name;

  FItems.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXImageList.Add(const Image: TPHXImage): TPHXImage;
begin
  Result:= Image;

  FItems.Add(Result);
end;

//------------------------------------------------------------------------------
procedure TPHXImageList.Delete(const Index: Integer);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  TPHXImage( FItems.List[Index] ).Free;

  FItems.Delete(Index);
end;

//------------------------------------------------------------------------------
procedure TPHXImageList.Remove(const Image: TPHXImage);
var Index: Integer;
begin
  Index:= FItems.IndexOf(Image);

  if (Index <> -1) then
  begin
    Delete(Index);

    Image.Free;
  end;
end;

//------------------------------------------------------------------------------
function TPHXImageList.LoadImage(const FileName: String): TPHXImage;
begin
  Result:= Add;
  Result.LoadImage(FileName);
end;

//------------------------------------------------------------------------------
function TPHXImageList.LoadImage(const FileName: String; const Name: string): TPHXImage;
begin
  Result:= Add;
  Result.LoadImage(FileName);
  Result.Name:= Name;
end;

//------------------------------------------------------------------------------
function TPHXImageList.LoadImage(const FileName: String; const Stream: TStream ): TPHXImage;
begin
  Result:= Add;
  Result.LoadImage(FileName, Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXImageList.LoadImages(const Path: String; const Filter: String = '*.phximg');
var Search: TSearchRec;
var Name   : String;
var Ext    : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.*', faAnyFile - faDirectory, Search) = 0 then
  try
    repeat
      Name:= IncludeTrailingPathDelimiter(Path) + Search.Name;
      Ext := ExtractFileExt(Name);

      if Pos('*' + Ext, Filter) > 0 then
      begin
        LoadImage(Path + Search.Name);
      end;

    until FindNext(Search) <> 0;
  finally
    FindClose(Search) ;
  end;
end;

//------------------------------------------------------------------------------
function TPHXImageList.IndexOf(const Name: string): Integer;
var Index: Integer;
var Image: TPHXImage;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Image:= TPHXImage( FItems.List[Index] );

    if SameText(Image.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;

  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXImageList.Find(const Name: string): TPHXImage;
var Index: Integer;
var Image: TPHXImage;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Image:= TPHXImage( FItems.List[Index] );

    if SameText(Image.Name, Name) then
    begin
      Result:= Image;
      Exit;
    end;
  end;
  Result:= nil;
end;


//------------------------------------------------------------------------------
function TPHXImageList.Find(const Name: string; out Image: TPHXImage): Boolean;
var Index: Integer;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Image:= TPHXImage( FItems.List[Index] );

    if SameText(Image.Name, Name) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXImageList.Find(const Image: String; const Pattern: String): Integer;
var AImage: TPHXImage;
begin
  if Find(Image, AImage) then
  begin
    Result:= AImage.Patterns.IndexOf(Pattern);
  end else
  begin
    Result:= PHXPATTERN_NONE;
  end;
end;

//------------------------------------------------------------------------------
function TPHXImageList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

//------------------------------------------------------------------------------
function TPHXImageList.GetList: PPointerList;
begin
  Result:= PPointerList(FItems.List);
end;

//------------------------------------------------------------------------------
function TPHXImageList.GetItem(Index: Integer): TPHXImage;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXImage( FItems.List[Index] );
end;

{$ENDREGION}

{$REGION 'TPHXAnimationFrames'}

// TPHXAnimationFrames
//==============================================================================
constructor TPHXAnimationFrames.Create(AOwner: TPHXAnimation);
begin
  FOwner   := AOwner;
  FCount   := 0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXAnimationFrames.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.LoadFromFile(const FileName: String);
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
procedure TPHXAnimationFrames.SaveToFile(const FileName: String);
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
procedure TPHXAnimationFrames.LoadFromStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);
  for Index := 0 to FCount - 1 do
  begin
    Stream.Read(FList^[Index].Name    , SizeOf(FList^[Index].Name   ) );
    Stream.Read(FList^[Index].Time    , SizeOf(FList^[Index].Time   ) );
    Stream.Read(FList^[Index].Pattern , SizeOf(FList^[Index].Pattern) );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.SaveToStream(Stream: TStream);
var Index: Integer;
begin
  Stream.Write(FCount, SizeOf(FCount));

  for Index := 0 to FCount - 1 do
  begin
     Stream.Write(FList^[Index].Name   , SizeOf(FList^[Index].Name   ) );
     Stream.Write(FList^[Index].Time   , SizeOf(FList^[Index].Time   ) );
     Stream.Write(FList^[Index].Pattern, SizeOf(FList^[Index].Pattern) );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Add(const Value: TPHXAnimationFrame );
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Add(const Name: String; const Time: Single; const Pattern: Integer );
begin
  SetCount(Count + 1);

  FList^[Count - 1].Name    := ShortString(Name);
  FList^[Count - 1].Time    := Time;
  FList^[Count - 1].Pattern := Pattern;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Add(const Name: String; const Time: Single; const Pattern: string);
var PatternIdx: Integer;
begin
  if Assigned(Owner) and Assigned(Owner.Image) then
  begin
    PatternIdx:= Owner.Image.Patterns.IndexOf(Pattern);

    Assert( PatternIdx >= 0, 'Pattern "' + Pattern + '" not found in the image "' + Owner.Image.Name + '".');

    SetCount(Count + 1);

    FList^[Count - 1].Name    := ShortString(Name);
    FList^[Count - 1].Time    := Time;
    FList^[Count - 1].Pattern := PatternIdx;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Delete(Index: Integer);
begin
  If (Index < 0) or (Index >= FCount) then Exit;

  FCount := FCount-1;

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXAnimationFrame));
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.Grow;
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
procedure TPHXAnimationFrames.Assign(List: TPHXAnimationFrames);
var Index: Integer;
begin
  SetCapacity(List.Count);
  SetCount   (List.Count);
  for Index := 0 to List.Count - 1 do
  begin
    FList^[Index]:= List.FList^[Index];
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXAnimationFrame));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXAnimationFrames.GetItem(Index: Integer): TPHXAnimationFrame;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationFrames.SetItem(Index: Integer; const Value: TPHXAnimationFrame);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}


{$REGION 'TPHXAnimationState'}

resourcestring
  SMissignAnimation = 'The animation state must have a assigned animation';

// TPHXAnimationState
//==============================================================================
class function TPHXAnimationState.Create(AAnimation: TPHXAnimation): TPHXAnimationState;
begin
  Result.Animation:= AAnimation;
  Result.Queued   := nil;
  Result.Active   := True;
  Result.Finished := False;
  Result.Time     := 0;
  Result.Frame    := 0;
  Result.Pattern  := 0;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationState.Reset;
begin
  Assert(Assigned(Animation), SMissignAnimation);

  Finished := False;
  Active   := True;
  Time     := 0;
  Frame    := 0;

  if Animation.Frames.Count > 0 then
  begin
    Pattern:= Animation.Frames.List^[0].Pattern;
  end else
  begin
    Pattern:= -1;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationState.Update(const DeltaTime: Double);
begin
  Assert(Assigned(Animation), SMissignAnimation);

  //  Only update if active
  if Active and (Animation.Frames.Count > 0) then
  begin
    // Add the time to the state
    Time:= Time + DeltaTime;

    // test if we shall change to the next frame
    if (Time > Animation.Frames[Frame].Time ) then
    begin
      Time:= Time - Animation.Frames[Frame].Time;

      Inc(Frame);

      // Test if we reached the end of the animation
      if (Frame >= Animation.Frames.Count) then
      begin
        // Check if looped
        if Animation.Looped then
        begin
          Time    := 0;
          Frame   := 0;
        end else
        begin
          // Is there a queued animation ?
          if Assigned(Queued) then
          begin
            Animation:= Queued;
            Queued   := nil;

            Reset;
          end else
          // Finish the animation
          begin
            Frame   := Animation.Frames.Count-1;
            Active  := False;
            Finished:= True;
          end;
        end;
      end;

      // Update the pattern index
      Pattern:=        Animation.Frames.List^[Frame].Pattern;
      Name   := String(Animation.Frames.List^[Frame].Name);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationState.Draw(const X, Y: Integer);
begin
  Assert(Assigned(Animation), SMissignAnimation);

  if Animation.Image <> nil then
  begin
    Animation.Image.Draw(X, Y, Pattern);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationState.Draw(const Transform: TMatrix4f);
begin
  Assert(Assigned(Animation), SMissignAnimation);

  if Animation.Image <> nil then
  begin
    Animation.Image.DrawTransform(Transform, Pattern);
  end;
end;




{$ENDREGION}

{$REGION 'TPHXAnimation'}

const DEFAULT_ANIMATION_NAME = 'Animation';

// TPHXAnimation
//==============================================================================
constructor TPHXAnimation.Create(AImages: TPHXImageList);
begin
  FImages:= AImages;
  FFrames:= TPHXAnimationFrames.Create(Self);
  FName  := DEFAULT_ANIMATION_NAME;

  FLooped:= False;
  FFrameRate:= 24;
end;

//------------------------------------------------------------------------------
destructor TPHXAnimation.Destroy;
begin
  FFrames.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckAnimation, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.SaveToFile(const FileName: String);
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
procedure TPHXAnimation.LoadFromStream(Stream: TStream);
var Header     : TPHXAnimationHeader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXANI') then
  begin
    {$IFDEF LOG_ENABLED}
    TPHXLog.Error('TPHXAnimation.LoadFromStream', 'Not a valid Phoenix animation.');
    {$ELSE}
    raise Exception.Create('Not a valid Phoenix animation.');
    {$ENDIF}
    Exit;
  end;

  If (Header.Version <> PHXANIMATION_VERSION) then
  begin
    {$IFDEF LOG_ENABLED}
    TPHXLog.Error('TPHXAnimation.LoadFromStream', 'Animation version missmatch [File: %d Code: %d].', [Header.Version, PHXANIMATION_VERSION]);
    {$ELSE}
    raise Exception.CreateFmt('Animation version missmatch [File: %d Code: %d].', [Header.Version, PHXANIMATION_VERSION]);
    {$ENDIF}
    Exit;
  end;

  // The name of the animation.
  StreamReadString(Stream, FName);
  StreamReadString(Stream, FAuthor);
  StreamReadString(Stream, FVersion);
  StreamReadString(Stream, FComment);

  StreamReadString(Stream, FImageName);

  Stream.Read(FLooped, SizeOf(FLooped));
  Stream.Read(FFrameRate, SizeOf(FFrameRate));

  FFrames.LoadFromStream(Stream);

  SetImageName(FImageName);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.SaveToStream(Stream: TStream);
var Header     : TPHXAnimationHeader;
begin
  Header.Ident  :='PHXANI';
  Header.Version:= PHXANIMATION_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  StreamWriteString(Stream, FName);
  StreamWriteString(Stream, FAuthor);
  StreamWriteString(Stream, FVersion);
  StreamWriteString(Stream, FComment);

  StreamWriteString(Stream, FImageName);

  Stream.Write(FLooped, SizeOf(FLooped));
  Stream.Write(FFrameRate, SizeOf(FFrameRate));

  FFrames.SaveToStream(Stream);
end;
                        (*
//------------------------------------------------------------------------------
procedure TPHXAnimation.Reset(var State: TPHXAnimationState);
begin
  State.Finished:= False;
  State.Active  := True;
  State.Time    := 0;
  State.Frame   := 0;

  if Frames.Count > 0 then
  begin
    State.Pattern:= Frames.List^[0].Pattern;
  end else
  begin
    State.Pattern:= -1;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.Update(var State: TPHXAnimationState; FrameTime: Single);
begin
  //  Only update if active
  if (State.Active) and (Frames.Count > 0) then
  begin
    // Add the time to the state
    State.Time := State.Time + FrameTime;

    // test if we shall change to the next frame
    if( State.Time > Frames[State.Frame].Time ) then
    begin
      State.Time:= State.Time - Frames[State.Frame].Time;

      Inc(State.Frame);

      // Test if we reached the end of the animation
      if( State.Frame >= Frames.Count) then
      begin

        // Check if looped
        if Looped then
        begin
          State.Time    := 0;
          State.Frame   := 0;
        end else
        begin
          State.Frame   := Frames.Count-1;
          State.Active  := False;
          State.Finished:= True;
        end;
      end;

      // Update the pattern index
      State.Pattern := Frames.List^[State.Frame].Pattern;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.Draw(const State: TPHXAnimationState; const X, Y: Integer);
begin
  Assert( Assigned(Image ) );

  Image.Draw(X, Y, FFrames[State.Frame].Pattern);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.Draw(const State: TPHXAnimationState; const Transform: TMatrix4f);
begin
  Assert( Assigned(Image ) );

  Image.DrawTransform(Transform, FFrames[State.Frame].Pattern);
end;
*)

//------------------------------------------------------------------------------
procedure TPHXAnimation.SetFramerate(const Framerate: Integer);
var Interval: Single;
var Index   : Integer;
begin
  if Framerate = 0 then
  begin
    Interval:= MaxSingle;
  end else
  begin
    Interval:= 1 / Framerate;
  end;

  for Index := 0 to Frames.Count - 1 do
  begin
    Frames.List^[Index].Time:= Interval;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAnimation.GetDuration: Single;
var Index   : Integer;
begin
  Result:= 0;
  for Index := 0 to Frames.Count - 1 do
  begin
    Result:= Result + Frames.List^[Index].Time;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(FImage) then
  begin
    FImageName:= FImage.Name;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimation.SetImageName(const Value: String);
begin
  FImageName := Value;

  if Assigned(FImages) then
  begin
    FImage:= FImages.Find(FImageName)
  end;
end;


{$ENDREGION}

{$REGION 'TPHXAnimationList'}

// TPHXAnimationList
//==============================================================================
constructor TPHXAnimationList.Create(AImages: TPHXImageList);
begin
  FList  := TList.Create;
  FImages:= AImages;
end;

//------------------------------------------------------------------------------
destructor TPHXAnimationList.Destroy;
begin
  Clear;

  FList.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationList.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    TPHXAnimation(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.Add(const Name: String): TPHXAnimation;
begin
  Result:= TPHXAnimation.Create(Images);
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.Add(const Animation: TPHXAnimation): TPHXAnimation;
begin
  Result:= Animation;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.LoadAnimation(const FileName: String): TPHXAnimation;
begin
  Result:= TPHXAnimation.Create(Images);
  Result.LoadFromFile(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.IndexOf(const Name: String): Integer;
var Index: Integer;
var Animation: TPHXAnimation;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Animation:= TPHXAnimation(FList.List[Index]);

    if SameText(Animation.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.Find(const Name: String): TPHXAnimation;
var Index: Integer;
var Animation: TPHXAnimation;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Animation:= TPHXAnimation(FList.List[Index]);

    if SameText(Animation.Name, Name) then
    begin
      Result:= Animation;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.Find(const Name: String; out Animation: TPHXAnimation): Boolean;
var Index  : Integer;
var Current: TPHXAnimation;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Current:= TPHXAnimation(FList.List[Index]);

    if SameText(Current.Name, Name) then
    begin
      Result   := True;
      Animation:= Current;
      Exit;
    end;
  end;
  Result   := False;
  Animation:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.GetList: PAnimationList;
begin
  Result:= PAnimationList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXAnimationList.GetItem(const Index: Integer): TPHXAnimation;
begin
  Assert( (Index >= 0) and (Index < FList.Count) );

  Result:= TPHXAnimation( FList.List[Index] );
end;


{$ENDREGION}

{$REGION 'TPHXAnimationSet'}

// TPHXAnimationSet
//==============================================================================
constructor TPHXAnimationSet.Create(AAnimations: TPHXAnimationList);
begin
  Assert(AAnimations <> nil);

  FQueue     := TList.Create;
  FAnimations:= FAnimations;
  FActive    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXAnimationSet.Destroy;
begin
  FQueue.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Activate(const Animation: String);
var Item: TPHXAnimation;
begin
  if Animations.Find(Animation, Item) then
  begin
    Activate(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Activate(const Animation: TPHXAnimation);
begin
  if FActive <> Animation then
  begin
    FActive:= Animation;

    FState.Animation:= FActive;

    Reset;
  end;
  FState.Active:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Queue(const Animation: String);
var Item: TPHXAnimation;
begin
  if Animations.Find(Animation, Item) then
  begin
    Queue(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Queue(const Animation: TPHXAnimation);
begin
  if Assigned(Active) then
  begin
    // If the active animation is looped we will interupt it
    if Active.Looped then
    begin
      Activate(Animation);
    end else
    // Else we wait until the current animation is finished
    begin
      FQueue.Add(Animation);
    end;

  end else
  begin
    Activate(Animation);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Reset;
begin
  if Assigned(FActive) then
  begin
    FState.Reset;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Start;
begin
  FState.Active:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Stop;
begin
  FState.Active:= False;

  FQueue.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Update(FrameTime: Single);
var Animation: TPHXAnimation;
begin
  if Assigned(FActive) then
  begin
    FState.Update(FrameTime);

    if FState.Finished and (FQueue.Count > 0) then
    begin
      Animation:= TPHXAnimation(FQueue.First);

      FQueue.Delete(0);

      Activate(Animation);
    end;

  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Draw( const X, Y: Integer);
begin
  if Assigned(FActive) then
  begin
    FState.Draw(X, Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationSet.Draw(const Transform: TMatrix4f);
begin
  if Assigned(FActive) then
  begin
    FState.Draw(Transform);
  end;
end;



{$ENDREGION}






end.
