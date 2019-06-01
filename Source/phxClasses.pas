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
unit phxClasses;
//< Generic types

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxLogger,
  phxTypes,
  phxMath;


type

{$REGION 'TPHXContentManager'}

// Exception class for content loading
EPHXContentError = class(Exception);

// Enueration of content types
//------------------------------------------------------------------------------
TPHXContentKind = (
  // Generic file
  ckFile,
  // Text file
  ckText,
  // Texture
  ckTexture,
  // Font
  ckFont,
  // Image
  ckImage,
  // Animation
  ckAnimation,
  // Audio
  ckAudio,
  // Model
  ckModel

);

// @abstract(The default content loader)
//
// @seealso(TPHXContentManager)
//------------------------------------------------------------------------------
TPHXContentLoader = class
  private
    FPath: String;
  public
    // Creates the default content loader
    constructor Create;
    // Get the expanded filename for a content
    //
    // @param(Name Name of the content)
    // @param(Kind Kind of the content)
    // @returns(The full filename of the content)
    function ExpandFileName(const Name: String; const Kind: TPHXContentKind): String; virtual;
    // Determine if a file exists
    //
    // @param(Name Name of the file to test)
    // @param(Kind The content kind of the file)
    // @returns(True of the file exits)
    function Exists(const Name: String; const Kind: TPHXContentKind): Boolean; virtual;
    // Create a stream for a file
    //
    // @param(Name Name of the file to open)
    // @param(Kind The content kind of the file)
    // @param(Mode the file open mode)
    // @returns(The filestream for the file, the caller must free the stream after use)
    function CreateStream(const Name: String; const Kind: TPHXContentKind; const Mode: Word): TStream; virtual;

    // The path to load content from
    property Path: String read FPath write FPath;
  end;

// @abstract(Helper class for loading content from files)
//
// The content manager class is a helper class for loading content using a content loader
//
// @bold(Loading content)
//
// @longcode(#
// // Load a texture from a file
// procedure TPHXTexture.LoadTexture(const FileName: String);
// var Stream: TStream;
// begin
//   Stream:= TPHXContentManager.Load(FileName, ckTexture, fmOpenRead or fmShareDenyNone);
//   try
//     LoadTexture(FileName, Stream);
//   finally
//     Stream.Free;
//   end;
// end;
// #)
//
// @bold(Changing the content loader)
//
// You can change the content loader by subclassing @link(TPHXContentLoader) and then
// setting that as the current loader via @link(TPHXContentManager.Loader)
//
// @longcode(#
// // Custom content loader
// TContentLoader = class(TPHXContentLoader)
//   public
//     function GetFileName(const Name: String; const Kind: TPHXContentKind): string; override;
//   end;
//
// // Returns the full path of a content
// function TContentLoader.GetFileName(const Name: String; const Kind: TPHXContentKind): string;
// begin
//   Result:= ExtractFilePath( ParamStr(0) ) + 'Content' + PathDelim + Name;
// end;
//
// // Assign a new loader to the content manager
// TPHXContentManager.Loader:= TContentLoader.Create;
// )
// @seealso(TPHXContentLoader)
//------------------------------------------------------------------------------
TPHXContentManager = class
  private
    // The current content loader
    class var FLoader: TPHXContentLoader;
    // Change the content loader
    class procedure SetLoader(const Value: TPHXContentLoader); static;
  public
    // Get the expanded filename for a content
    //
    // @param(Name Name of the content)
    // @param(Kind Kind of the content)
    // @returns(The full filename of the content)
    class function ExpandFileName(const Name: String; const Kind: TPHXContentKind): String; virtual;
    // Uses the current content loader to determine if a file exists
    //
    // @param(Name Name of the file to test)
    // @param(Kind The content kind of the file)
    // @returns(True of the file exits)
    // @seealso(TPHXContentLoader)
    class function Exists(const Name: String; const Kind: TPHXContentKind): Boolean; static;

    // Uses the current content loader to create a stream for a file
    //
    // @param(Name Name of the file to open)
    // @param(Kind The content kind of the file)
    // @param(Mode the file open mode)
    // @returns(The file stream for the file, the caller must free the stream after use)
    // @seealso(TPHXContentLoader)
    class function CreateStream(const Name: String; const Kind: TPHXContentKind; const Mode: Word): TStream; static;
    // Returns the current content loader
    class property Loader: TPHXContentLoader read FLoader write SetLoader;
  end;

{$ENDREGION}

{$REGION 'TPHXFiler'}

// Exception for TPHXReader and TPHXWriter
EPHXFilerError = class(Exception);

// Base filer class for the reader and writer classes
//------------------------------------------------------------------------------
TPHXFiler = class(TObject)
  private
    FStream: TStream;
    FStreamOwned: Boolean;
  public
    // Create a filer for a stream
    constructor Create(Stream: TStream; StreamOwned: Boolean = False);
    // Default destructor.
    destructor Destroy; override;

    // The current stream
    property Stream: TStream read FStream;
    // Determines if the filer owns the stream and will free it in the destructor
    property StreamOwned: Boolean read FStreamOwned;
  end;

{$ENDREGION}

{$REGION 'TPHXReader'}

// The reader is a helper class for writing properties to a stream
//------------------------------------------------------------------------------
TPHXReader = class(TPHXFiler)
  public
    // Create a reader for a stream
    constructor Create(Stream: TStream); overload;
    // Create a reader for a file
    constructor Create(const FileName: String); overload;

    // Read a buffer from the stream
    procedure Read(var Buffer; Count: Longint);

    // Read a boolean
    function ReadBoolean: Boolean;
    // Read a 8 bit integer
    function ReadByte: Byte;
    // Read a 16 bit integer
    function ReadWord: Word;
    // Read a 32 bit integer
    function ReadInteger: Integer;
    // Read a cardinal value
    function ReadCardinal: Cardinal;
    // Read a single precision float
    function ReadSingle: Single;
    // Read a double precision float
    function ReadDouble: Double;
    // Read a string
    function ReadString: String;
    // Read a color
    function ReadColor: TColor4f;
    // Read a TVector2f
    function ReadVector2f: TVector2f;
    // Read a TVector3f
    function ReadVector3f: TVector3f;
    // Read a rect
    function ReadRecti: TRecti;
  end;

{$ENDREGION}

{$REGION 'TPHXWriter'}

// The writer is a helper class for writing properties to a stream
//------------------------------------------------------------------------------
TPHXWriter = class(TPHXFiler)
  private
  public
    // Create a writer for a stream
    constructor Create(Stream: TStream); overload;
    // Create a writer for a file
    constructor Create(const FileName: String); overload;

    // Write a buffer to the stream
    procedure Write(const Buffer; Count: Longint);

    // Write a boolean
    procedure WriteBoolean(const Value: Boolean);
    // Write a 8 bit integer
    procedure WriteByte(const Value: Byte);
    // Write a 16 bit integer
    procedure WriteWord(const Value: Word);
    // Write a 32 bit integer
    procedure WriteInteger(const Value: Integer);
    // Write a cardinal
    procedure WriteCardinal(const Value: Cardinal);
    // Write a single precision float
    procedure WriteSingle(const Value: Single);
    // Write a double precision float
    procedure WriteDouble(const Value: Double);
    // Write a string
    procedure WriteString(const Value: WideString);
    // Write a color
    procedure WriteColor(const Value: TColor4f);
    // Write a TVector2f
    procedure WriteVector2f(const Value: TVector2f);
    // Write a TVector3f
    procedure WriteVector3f(const Value: TVector3f);
    // Write a rect
    procedure WriteRecti(const Value: TRecti);
 end;

{$ENDREGION}

{$REGION 'TPHXSingleList'}

// List container for floats
//------------------------------------------------------------------------------
TPHXSingleList = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PSingleList;

    procedure Grow;

    function  GetItem(Index: Integer): Single;
    procedure SetItem(Index: Integer; const Value: Single);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a empty list
    constructor Create; overload;
    // Creates a empty list with a specified capacity
    constructor Create(const Capacity: Integer); overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;

    // Add a item to the list
    procedure Add(const Value: Single );

    // Load the integer list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the integer list from a stream
    procedure LoadFromStream(const Stream: TStream);

    // Save the integer list to a file
    procedure SaveToFile(const FileName: String);
    // Save the integer list to a stream
    procedure SaveToStream(const Stream: TStream);

    // The current number of items in the list
    Property Count   : Integer read FCount    write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PSingleList read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: Single read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXVectorList2i'}

// List of TVector2i
//------------------------------------------------------------------------------
TPHXVectorList2i = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PVectorList2i;

    procedure Grow;

    function  GetItem(Index: Integer): TVector2i;
    procedure SetItem(Index: Integer; const Value: TVector2i);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create a empty list
    constructor Create; overload;
    // Create a list with a default capacity
    constructor Create(ACapacity: Integer); overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;

    // Add a item to the list
    procedure Add(const Vector: TVector2i); overload;
    // Add a item to the list
    procedure Add(const X, Y: Integer); overload;

    // Load the list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the list from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the list to a file
    procedure SaveToFile(const FileName: String);
    // Save the list to a stream
    procedure SaveToStream(Stream: TStream);

    // The current number of items in the list
    property Count   : Integer read FCount    write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PVectorList2i read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TVector2i read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXVectorList2f'}

// List of TVector2f
//------------------------------------------------------------------------------
TPHXVectorList2f = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PVectorList2f;

    procedure Grow;

    function  GetItem(Index: Integer): TVector2f;
    procedure SetItem(Index: Integer; const Value: TVector2f);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create a empty list
    constructor Create; overload;
    // Create a list with a default capacity
    constructor Create(ACapacity: Integer); overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;

    // Add a item to the list
    procedure Add(const Vector: TVector2f); overload;
    // Add a item to the list
    procedure Add(const X,Y: Single); overload;

    // Load the list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the list from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the list to a file
    procedure SaveToFile(const FileName: String);
    // Save the list to a stream
    procedure SaveToStream(Stream: TStream);

    // The current number of items in the list
    property Count: Integer read FCount    write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PVectorList2f read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TVector2f read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXVectorList3f'}

// List of TVector3f
//------------------------------------------------------------------------------
TPHXVectorList3f = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PVectorList3f;

    procedure Grow;

    function  GetItem(Index: Integer): TVector3f;
    procedure SetItem(Index: Integer; const Value: TVector3f);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create a empty list
    constructor Create; overload;
    // Create a list with a default capacity
    constructor Create(ACapacity: Integer); overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;

    // Add a item to the list
    procedure Add(const Vector: TVector3f); overload;
    // Add a item to the list
    procedure Add(const X, Y, Z: Single); overload;

    // Load the list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the list from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the list to a file
    procedure SaveToFile(const FileName: String);
    // Save the list to a stream
    procedure SaveToStream(Stream: TStream);

    // The current number of items in the list
    property Count: Integer read FCount  write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PVectorList3f read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TVector3f read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXColorList4f'}

// List of TPHXColorList4f
//------------------------------------------------------------------------------
TPHXColorList4f = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PColorList4f;

    procedure Grow;

    function  GetItem(Index: Integer): TColor4f;
    procedure SetItem(Index: Integer; const Value: TColor4f);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create a empty list
    constructor Create; overload;
    // Create a list with a default capacity
    constructor Create(ACapacity: Integer); overload;
    // Destroy the list
    destructor Destroy; override;

    // Removes all items from the list
    procedure Clear;

    // Add a item to the list
    procedure Add(const Color: TColor4f); overload;
    // Add a item to the list
    procedure Add(const R, G, B, A: Single); overload;

    // Load the list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the list from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the list to a file
    procedure SaveToFile(const FileName: String);
    // Save the list to a stream
    procedure SaveToStream(Stream: TStream);

    // The current number of items in the list
    property Count: Integer read FCount  write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PColorList4f read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TColor4f read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXRecti'}

 // Container for a TRecti
//------------------------------------------------------------------------------
TPHXRecti = class(TPersistent)
  private
    FValue: TRecti;

    function GetHeight: Integer;
    function GetWidth : Integer;
  public
    constructor Create; overload;
    constructor Create(ARect: TRecti); overload;
    constructor Create(Left, Top, Right, Bottom: Integer); overload;

    // Load the rect from a reader
    procedure LoadRect(Reader: TPHXReader);
    // Save the rect to a writer
    procedure SaveRect(Writer: TPHXWriter);

    // Tests if a point is inside the rectangle
    function PointInside(const X,Y: Integer): Boolean; overload;
    // Tests if a point is Integer the rectangle
    function PointInside(const Point: TVector2i): Boolean; overload;

    // The container rectangle
    property Value: TRecti read FValue  write FValue;
    // Width of the rectangle
    property Width: Integer read GetWidth;
    // Height of the rectangle
    property Height: Integer read GetHeight;
  published
    // Left position
    property Left: Integer read FValue.Left write FValue.Left;
    // Top position
    property Top: Integer read FValue.Top write FValue.Top;
    // Right position
    property Right: Integer read FValue.Right write FValue.Right;
    // Bottom position
    property Bottom: Integer read FValue.Bottom write FValue.Bottom;
  end;

{$ENDREGION}

{$REGION 'TPHXDictionary'}

// A single hash item in a dictionary
//------------------------------------------------------------------------------
THashRecord = record
  // Hash code of the key
  Hash: Cardinal;
  // The value
  Value: Pointer;
end;

PHashRecordList = ^THashRecordList;
THashRecordList = array[0..$00FFFFFF] of THashRecord;

//------------------------------------------------------------------------------
THashBucket = record
  // Number of items in the bucket
  Count: Integer;
  // Capacity of the bucket
  Capacity: Integer;
  // Indexes of the items in the bucket
  Items: array of Integer;
end;


//------------------------------------------------------------------------------
TPHXDictionary = class
  private
    FItems: PHashRecordList;
    FCount: Integer;
    FCapacity: Integer;

  public
    FBucketSize: Cardinal;
    FBucketList: array of THashBucket;
   private

    procedure Grow;

    procedure ClearBuckets;

    function GetValue(const Key: String): Pointer;
    function GetItem(const Index: Integer): Pointer;

    procedure SetCount(const Value: Integer);
    procedure SetCapacity(const Value: Integer);
    procedure SetValue(const Key: String; Value: Pointer);
  protected
    function FindItem(const Key: String): Integer; overload;
    function FindItem(const Hash: Cardinal): Integer; overload;
  public
    // Create a new dictionary with a specified table size
	  constructor Create(const ASize: Integer = 128);
    // Destroy the dictionary
    destructor Destroy; override;

    // Resize the table array
    procedure Resize(const Size: Integer);

    // Removes all items from the dictionary
    procedure Clear;
    // Add a item to the dictionary
    procedure Add(const Key: String; Value: Pointer); overload;
    // Add a item to the dictionary
    procedure Add(const Hash: Cardinal; Value: Pointer); overload;

    // Delete a item
    procedure Delete(const Key: String); overload;
   // Delete a item
    procedure Delete(const Hash: Cardinal); overload;

    // Returns true if the dictionary contains a key
    function ContainsKey(const Key: String): Boolean; overload;
    // Returns true if the dictionary contains a hash
    function ContainsKey(const Hash: Cardinal): Boolean; overload;

    // Returns true if the dictionary contains a value
    function ContainsValue(const Value: Pointer): Boolean;

    // The current number of items in the list
    property Count: Integer read FCount  write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PHashRecordList read FItems;
    // Return a item in the dictionary
    property Items[const Index: Integer]: Pointer read GetItem;
    // Return a item  in the dictionary
    property Value[const Key: String]: Pointer read GetValue write SetValue; default;
  end;

{$ENDREGION}

{$REGION 'TPHXNotifications'}

//------------------------------------------------------------------------------
TPHXNotification = (
  // Called after a device has been recreated
  dnContextCreated
);

//------------------------------------------------------------------------------
TPHXNotificationEvent = procedure(Notification: TPHXNotification) of object;


//------------------------------------------------------------------------------
TPHXNotifications = class
  private
    // Create the notification
    class procedure Create; static;
    // Create the notification class
    class procedure Free; static;

    // Remove all notifications
    class procedure Clear; static;
  public
    // Add a notification listener
    class procedure Add(Event: TPHXNotificationEvent); static;
    // Remove a notification listener
    class procedure Remove(Event: TPHXNotificationEvent); static;

    // Notify all notification listeners
    class procedure Notify(Notification: TPHXNotification); static;
  end;

{$ENDREGION}

{$REGION 'TPHXCustomDevice'}

// Marker class for the device class
// @exclude
//------------------------------------------------------------------------------
TPHXCustomDevice = class(TObject)
  public
  end;

{$ENDREGION}

{$REGION 'TPHXCustomCanvas'}

// Marker class for the canvas class
// @exclude
//------------------------------------------------------------------------------
TPHXCustomCanvas = class(TObject)
  public
  end;



{$ENDREGION}

{$REGION 'TPHXBuffer'}

const
  // Default capacity for the vertex list of the TPHXBuffer class
  BufferCapacityVertices = 130*130;
  // Default capacity for the index list of the TPHXBuffer class
  BufferCapacityIndices  = 130*130*6;

type



// Vertex usage for vertex and index buffers
//------------------------------------------------------------------------------
TPHXBufferUsage = (
  // The data store contents will be modified once and used at most a few times.
  PHX_USAGE_STREAM,
  // The data store contents will be modified once and used many times.
  PHX_USAGE_STATIC,
  // The data store contents will be modified repeatedly and used many times.
  PHX_USAGE_DYNAMIC
);

// Buffer for rendering primitives
//------------------------------------------------------------------------------
TPHXBuffer = class(TObject)
  private
    // Buffer useage
    FUseage: TPHXBufferUsage;
    // The primitive type
    FPrimitive: TPHXPrimitiveType;
    // The vertex list
    FVertices: TPHXVertexList;
    // The index list
    FIndices: TPHXIndexList;

    procedure SetUseage(const Value: TPHXBufferUsage);
    procedure SetPrimitive(const Value: TPHXPrimitiveType);
  public
    // Default constructor, use Device.CreateBuffer instead
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Resize the buffer
    procedure Resize(const VertexCapacity: Integer; const IndexCapacity: Integer); virtual; abstract;

    // Empty the buffer
    procedure Clear;
    // Bind the buffer;
    procedure Bind; virtual; abstract;
    // Upload the buffer to the device
    procedure Upload; virtual; abstract;
    // Render the contents of the buffer
//    procedure Render; virtual; abstract;

//    procedure RenderEx(Decl: TPHXVertexDeclaration); virtual; abstract;

    // The buffer mode
    property Useage: TPHXBufferUsage read FUseage write SetUseage;
    // The primitive to render
    property Primitive: TPHXPrimitiveType read FPrimitive write SetPrimitive;
    // The vertex list
    property Vertices: TPHXVertexList read FVertices;
    // The index list
    property Indices: TPHXIndexList read FIndices;
  end;

{$ENDREGION}

{$REGION 'TPHXDisplayMode'}

// Describes one video mode
//------------------------------------------------------------------------------
TPHXDisplayMode = record
  public
    //Description of the video mode
    Description : String[80];
    // The number of horisontal pixels
    Width: Integer;
    // The number of vertical pixels
    Height: Integer;
    // Fullscreen mode?
    Fullscreen: Boolean;
  public
    // Creates a new display mode
    class function Create(const ADescription: String; const AWidth: Integer; const AHeight: Integer): TPHXDisplayMode; static;
  end;

PDisplayModeList = ^TDisplayModeList;
TDisplayModeList = array[0..$00FFFFFF] of TPHXDisplayMode;

// Maintans a a list of supported video modes
//------------------------------------------------------------------------------
TPHXDisplayModes = class(TPersistent)
  private
    // Number of display modes
    FCount: Integer;
    // Capacity of the list
    FCapacity: Integer;
    // List of modes
    FList: PDisplayModeList;
    // Stringlist containing the descriptions of all supported display modes
    FDescriptions: TStrings;
    FDesktop : TPHXDisplayMode;

    //FSelected: TPHXDisplayMode;

    procedure Grow;

    function GetItem(Index: Integer): TPHXDisplayMode;

    procedure SetCount(const Value: Integer);
    procedure SetCapacity(const Value: Integer);
    function GetDescriptions: TStrings;
  public
    // Creates a new display object
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    procedure Clear;
    // Add a new video mode to the list
    procedure Add(const Mode: TPHXDisplayMode); overload;
    procedure Add(const Width, Height: Integer); overload;

    // Sort the mode list on increasing resolution
    procedure Sort;

    // Return the index of a video mode from its description
    function IndexOf(const Description: String): Integer; overload;
    // Returns a displaymode by the width and height
    function IndexOf(const Width, Height: Integer): Integer; overload;

   // procedure Select(const Index: Integer); overload;
    //procedure Select(const Mode: TPHXDisplayMode); overload;

    // Select the next higher resolution while still maintaining the aspect ratio
   // function SelectHigher: Boolean;
    // Select the next lower resolution while still maintaining the aspect ratio
   // function SelectLower: Boolean;

    // Number of videomodes
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal video mode list
    property List: PDisplayModeList read FList;
    // Get a video modes
    property Modes[Index: Integer]: TPHXDisplayMode read GetItem; default;
    // Stringlist containing the descriptions of all supported display modes
    property Descriptions: TStrings read GetDescriptions;
    // The mode of the desktop
    property Desktop: TPHXDisplayMode read FDesktop write FDesktop;

    // The selected displaymode
   // property Selected: TPHXDisplayMode read FSelected write FSelected;
  end;
{$ENDREGION}



// Return the elf hash of a ansistring
function ELFHash(const Value: AnsiString): Cardinal; overload;
// Return the elf hash of a string
function ELFHash(const Value: WideString): Cardinal; overload;

procedure Base64Encode(const Source, Dest: PByteArray; const Size: Integer); overload;
procedure Base64Decode(const Source, Dest: PByteArray; const Size: Integer); overload;

function Base64Encode(const Source: AnsiString): AnsiString; overload;
function Base64Decode(const Source: AnsiString): AnsiString; overload;

// Load a textfile encoded in UTF8 from a file
function LoadTextFileUTF8(const FileName: String): WideString; overload;
// Load a textfile encoded in UTF8 from a stream
function LoadTextFileUTF8(Stream: TStream): WideString; overload;

implementation


{$REGION 'Utils'}

//------------------------------------------------------------------------------
function ELFHash(const Value: AnsiString) : Cardinal;
var i: Cardinal;
var x: Cardinal;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x      := Result and $F0000000;
    if (x <> 0) then
    begin
      Result := Result xor (x shr 24);
    end;
    Result := Result and (not x);
  end;
end;

//------------------------------------------------------------------------------
function ELFHash(const Value: WideString) : Cardinal;
var i: Cardinal;
var x: Cardinal;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x      := Result and $F0000000;
    if (x <> 0) then
    begin
      Result := Result xor (x shr 24);
    end;
    Result := Result and (not x);
  end;
end;

const

// Encoding table for base 64
//------------------------------------------------------------------------------
Base64Table: array[0..63] of Byte = (65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
  76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100,
  101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
  116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
  43, 47);

//------------------------------------------------------------------------------
procedure Base64Encode(const Source, Dest: PByteArray; const Size: Integer);
var Index : Integer;
var iPtr  : Integer;
var oPtr  : Integer;
begin
  iPtr  := 0;
  oPtr  := 0;
  for Index:= 1 to (Size div 3) do
  begin
    Dest^[oPtr + 0]:= Base64Table[  Source^[iPtr] shr 2];
    Dest^[oPtr + 1]:= Base64Table[((Source^[iPtr] and 3) shl 4) + (Source^[iPtr + 1] shr 4)];
    Dest^[oPtr + 2]:= Base64Table[((Source^[iPtr + 1] and 15) shl 2) + (Source^[iPtr + 2] shr 6)];
    Dest^[oPtr + 3]:= Base64Table[  Source^[iPtr + 2] and 63];

    Inc(iPtr, 3);
    Inc(oPtr, 4);
  end;
  // Appends = as padding characters
  case (Size mod 3) of
    1:
    begin
      Dest^[oPtr + 0]:= Base64Table[ Source^[iPtr] shr 2];
      Dest^[oPtr + 1]:= Base64Table[(Source^[iPtr] and 3) shl 4];
      Dest^[oPtr + 2]:= Byte('=');
      Dest^[oPtr + 3]:= Byte('=');
    end;
    2:
    begin
      Dest^[oPtr + 0]:= Base64Table[  Source^[iPtr] shr 2];
      Dest^[oPtr + 1]:= Base64Table[((Source^[iPtr] and 3) shl 4) + (Source^[iPtr + 1] shr 4)];
      Dest^[oPtr + 2]:= Base64Table[( Source^[iPtr + 1] and 15) shl 2];
      Dest^[oPtr + 3]:= Byte('=');
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure Base64Decode(const Source, Dest: PByteArray; const Size: Integer);
var i, j: Integer;
var iPtr: Integer;
var oPtr: Integer;
var Temp: array[0..3] of Byte;
begin
  iPtr  := 0;
  oPtr  := 0;

  for i:= 1 to (Size div 4) do
  begin
    for j:= 0 to 3 do
    begin
      case Source^[iPtr] of
        65..90 : Temp[j]:= Source^[iPtr] - Ord('A');
        97..122: Temp[j]:= Source^[iPtr] - Ord('a') + 26;
        48..57 : Temp[j]:= Source^[iPtr] - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;

      Inc(iPtr);
    end;
    Dest^[oPtr]:= (Temp[0] shl 2) or (Temp[1] shr 4);

    if (Temp[2] <> $FF) and (Temp[3] = $FF) then
    begin
      Dest^[oPtr + 1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Inc(oPtr);
    end else
    if (Temp[2] <> $FF) then
    begin
      Dest^[oPtr + 1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Dest^[oPtr + 2]:= (Temp[2] shl 6) or  Temp[3];
      Inc(oPtr, 2);
    end;
    Inc(oPtr);
  end;
end;

//---------------------------------------------------------------------------
function Base64Encode(const Source: AnsiString): AnsiString;
var Size: Integer;
begin
  Size:= Length(Source);

  SetLength(Result, ((Size + 2) div 3) * 4);

  Base64Encode(@Source[1], @Result[1], Size);
end;

//---------------------------------------------------------------------------
function Base64Decode(const Source: AnsiString): AnsiString;
var Size: Integer;
begin
  Size:= Length(Source);

  SetLength(Result, ((Size + 2) div 4) * 3);

  Base64Decode(@Source[1], @Result[1], Size);
end;



 //------------------------------------------------------------------------------
function LoadTextFileUTF8(const FileName: String): WideString;
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckText, fmOpenRead or fmShareDenyNone);
  try
    Result:= LoadTextFileUTF8(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
function LoadTextFileUTF8(Stream: TStream): WideString;
const UTF8_Bom = #$EF#$BB#$BF;
var Data: AnsiString;
begin
  Result:= '';

  SetLength(Data, Stream.Size);
  try
    Stream.Read(Data[1], Stream.Size);

    if Copy(Data, 1, 3) = UTF8_Bom then
    begin
      {$IFDEF FPC}
      Result:= UTF8Decode( Copy(Data, 4, MaxInt) );
      {$ELSE}
      Result:= UTF8ToString( Copy(Data, 4, MaxInt) );
      {$ENDIF}
    end else
    begin
      {$IFDEF FPC}
      Result:= UTF8Decode(Data);
      {$ELSE}
      Result:= UTF8ToString(RawByteString(Data));
      {$ENDIF}
    end;
  finally
    SetLength(Data, 0);
  end;
end;

{$ENDREGION}

{$REGION 'TPHXContentManager'}


// TPHXContentLoader
//==============================================================================
constructor TPHXContentLoader.Create;
begin
  {$IFDEF DARWIN}
  FPath = '../Resources/' ;
  {$ELSE}
  FPath:= ExtractFilePath( ParamStr(0) );
  {$ENDIF}
end;

// Keep the original path to /Contents/Resources in the .app - file
//------------------------------------------------------------------------------
function TPHXContentLoader.ExpandFileName(const Name: String; const Kind: TPHXContentKind): String;
begin
  if FileExists(Name) or ( ExtractFileDrive(Name) <> '') then
  begin
    Result:= Name;
  end else
  begin
    Result:= Path + Name;

    Result:= StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
    Result:= StringReplace(Result, '\', PathDelim, [rfReplaceAll]);
  end;
end;

//------------------------------------------------------------------------------
function TPHXContentLoader.Exists(const Name: String; const Kind: TPHXContentKind): Boolean;
var FileName: String;
begin
  FileName:= Self.ExpandFileName(Name, Kind);

  Result:= FileExists(FileName);
end;

//------------------------------------------------------------------------------
function TPHXContentLoader.CreateStream(const Name: String; const Kind: TPHXContentKind; const Mode: Word): TStream;
var FileName: String;
begin
  FileName:= Self.ExpandFileName(Name, Kind);

  TPHXLogger.Info('TPHXDefaultLoader.Load', 'Loading content: %s', [Name]);

  Result:= TFileStream.Create(FileName, Mode);

  (*
  if FileExists(FileName) then
  begin
    Result:= TFileStream.Create(FileName, Mode);
  end else
  begin
    raise EPHXContentError.CreateFmt('Could not load the content "%s"'#13'The file "%s" doesnt exists', [Name, FileName]);
  end;
  *)
end;


// TPHXContentManager
//==============================================================================
class function TPHXContentManager.ExpandFileName(const Name: String; const Kind: TPHXContentKind): String;
begin
  Result:= FLoader.ExpandFileName(Name, Kind);
end;

//------------------------------------------------------------------------------
class function TPHXContentManager.Exists(const Name: String; const Kind: TPHXContentKind): Boolean;
begin
  Result:= FLoader.Exists(Name, Kind);
end;

//------------------------------------------------------------------------------
class function TPHXContentManager.CreateStream(const Name: String; const Kind: TPHXContentKind; const Mode: Word): TStream;
begin
  Result:= FLoader.CreateStream(Name, Kind, Mode);
end;

//------------------------------------------------------------------------------
class procedure TPHXContentManager.SetLoader(const Value: TPHXContentLoader);
begin
  Assert(Assigned(Value));

  if FLoader <> Value  then
  begin
    FLoader.Free;
    FLoader:= Value;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXFiler'}

// TPHXFiler
//==============================================================================
constructor TPHXFiler.Create(Stream: TStream; StreamOwned: Boolean = False);
begin
  Assert(Stream <> nil);

  FStream     := Stream;
  FStreamOwned:= StreamOwned;
end;

//------------------------------------------------------------------------------
destructor TPHXFiler.Destroy;
begin
  // Free the stream if we are the owner of it
  if FStreamOwned then
  begin
    Stream.Free;
  end;

  inherited;
end;

{$ENDREGION}

{$REGION 'TPHXReader'}

// TPHXReader
//==============================================================================
constructor TPHXReader.Create(Stream: TStream);
begin
  inherited Create(Stream, False);
end;

//------------------------------------------------------------------------------
constructor TPHXReader.Create(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyWrite);

  inherited Create(Stream, True);
end;

//------------------------------------------------------------------------------
procedure TPHXReader.Read(var Buffer; Count: Integer);
begin
  Stream.Read(Buffer, Count);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadBoolean: Boolean;
begin
  Stream.Read(Result, 1);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadByte: Byte;
begin
  Stream.Read(Result, 1);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadWord: Word;
begin
  Stream.Read(Result, 2);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadInteger: Integer;
begin
  Stream.Read(Result, 4);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadCardinal: Cardinal;
begin
  Stream.Read(Result, 4);
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadSingle: Single;
begin
  Stream.Read(Result, SizeOf(Single));
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadDouble: Double;
begin
  Stream.Read(Result, SizeOf(Double));
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadString: String;
var Header: array[1..6] of AnsiChar;
var Len   : Cardinal;
var Data  : WideString;
begin
  Header:= #0#0#0#0#0#0;
  Data  := '';
  Len   := 0;

  Stream.Read(Header, 6);
  Stream.Read(Len   , 4);

  // Check the header string
  if (Header <> 'PHXSTR') then
  begin
    raise EPHXFilerError.Create('Not a valid string.');
  end;

  if Len > 0 then
  begin
    SetLength(Data, len);

    Stream.ReadBuffer(Data[1], 2 * Len);

    Result:= String(Data);
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadColor: TColor4f;
begin
  Stream.Read(Result.Red  , SizeOf(Single));
  Stream.Read(Result.Green, SizeOf(Single));
  Stream.Read(Result.Blue , SizeOf(Single));
  Stream.Read(Result.Alpha, SizeOf(Single));
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadVector2f: TVector2f;
begin
  Stream.Read(Result.X, SizeOf(Single));
  Stream.Read(Result.Y, SizeOf(Single));
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadVector3f: TVector3f;
begin
  Stream.Read(Result.X, SizeOf(Single));
  Stream.Read(Result.Y, SizeOf(Single));
  Stream.Read(Result.Z, SizeOf(Single));
end;

//------------------------------------------------------------------------------
function TPHXReader.ReadRecti: TRecti;
begin
  Stream.Read(Result.Left  , SizeOf(Integer));
  Stream.Read(Result.Top   , SizeOf(Integer));
  Stream.Read(Result.Right , SizeOf(Integer));
  Stream.Read(Result.Bottom, SizeOf(Integer));
end;

{$ENDREGION}

{$REGION 'TPHXWriter'}

// TPHXWriter
//==============================================================================
constructor TPHXWriter.Create(Stream: TStream);
begin
  inherited Create(Stream, False);
end;

//------------------------------------------------------------------------------
constructor TPHXWriter.Create(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);

  inherited Create(Stream, True);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.Write(const Buffer; Count: Integer);
begin
  Stream.Write(Buffer, Count);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteBoolean(const Value: Boolean);
begin
  Stream.Write(Value, 1);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteByte(const Value: Byte);
begin
  Stream.Write(Value, 1);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteWord(const Value: Word);
begin
  Stream.Write(Value, 2);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteInteger(const Value: Integer);
begin
  Stream.Write(Value, 4);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteCardinal(const Value: Cardinal);
begin
  Stream.Write(Value, SizeOf(Cardinal));
end;


//------------------------------------------------------------------------------
procedure TPHXWriter.WriteDouble(const Value: Double);
begin
  Stream.Write(Value, SizeOf(Double));
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteSingle(const Value: Single);
begin
  Stream.Write(Value, SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteString(const Value: WideString);
var Header: array[1..6] of AnsiChar;
var Len   : Cardinal;
var Data  : WideString;
begin
  Header:= 'PHXSTR';
  Data  := WideString(Value);
  Len   := Length(Value);

  Stream.Write(Header, 6);
  Stream.Write(Len   , 4);

  if Len > 0 then
  begin
    Stream.WriteBuffer(Data[1], 2 * Len);
  end;

  //StreamWriteString(Stream, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteColor(const Value: TColor4f);
begin
  Stream.Write(Value.Red  , SizeOf(Single));
  Stream.Write(Value.Green, SizeOf(Single));
  Stream.Write(Value.Blue , SizeOf(Single));
  Stream.Write(Value.Alpha, SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteVector2f(const Value: TVector2f);
begin
  Stream.Write(Value.X, SizeOf(Single));
  Stream.Write(Value.Y, SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteVector3f(const Value: TVector3f);
begin
  Stream.Write(Value.X, SizeOf(Single));
  Stream.Write(Value.Y, SizeOf(Single));
  Stream.Write(Value.Z, SizeOf(Single));
end;

//------------------------------------------------------------------------------
procedure TPHXWriter.WriteRecti(const Value: TRecti);
begin
  Stream.Write(Value.Left  , SizeOf(Integer));
  Stream.Write(Value.Top, SizeOf(Integer));
  Stream.Write(Value.Right , SizeOf(Integer));
  Stream.Write(Value.Bottom, SizeOf(Integer));
end;

{$ENDREGION}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


{$REGION 'TPHXSingleList'}

// TPHXSingleList
//==============================================================================
constructor TPHXSingleList.Create;
begin

end;

//------------------------------------------------------------------------------
constructor TPHXSingleList.Create(const Capacity: Integer);
begin
  SetCapacity(Capacity);
end;

//------------------------------------------------------------------------------
destructor TPHXSingleList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.LoadFromFile(const FileName: String);
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
procedure TPHXSingleList.SaveToFile(const FileName: String);
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
procedure TPHXSingleList.LoadFromStream(const Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^ , SizeOf(Single) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.SaveToStream(const Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^ , SizeOf(Single) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.Add(const Value: Single);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(Single));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXSingleList.GetItem(Index: Integer): Single;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXSingleList.SetItem(Index: Integer; const Value: Single);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXVectorList2i'}

// TPHXVectorList2i
//==============================================================================
constructor TPHXVectorList2i.Create;
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
constructor TPHXVectorList2i.Create(ACapacity: Integer);
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;

  SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXVectorList2i.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.SaveToFile(const FileName: String);
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
procedure TPHXVectorList2i.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^ , SizeOf(TVector2i) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^ , SizeOf(TVector2i) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.Add(const Vector: TVector2i);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1]:= Vector;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.Add(const X, Y: Integer);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1].X:= X;
  FList^[Count - 1].Y:= Y;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.Clear;
begin
  FCount:= 0;
end;


//------------------------------------------------------------------------------
procedure TPHXVectorList2i.Grow;
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
procedure TPHXVectorList2i.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TVector2i));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXVectorList2i.GetItem(Index: Integer): TVector2i;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2i.SetItem(Index: Integer; const Value: TVector2i);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXVectorList2f'}


// TPHXVectorList2f
//==============================================================================
constructor TPHXVectorList2f.Create;
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
constructor TPHXVectorList2f.Create(ACapacity: Integer);
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;

  SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXVectorList2f.Destroy;
begin
  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.SaveToFile(const FileName: String);
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
procedure TPHXVectorList2f.LoadFromStream( Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^ , SizeOf(TVector2f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^ , SizeOf(TVector2f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.Add(const Vector: TVector2f);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1]:= Vector;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.Add(const X, Y: Single);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  FList^[Count - 1].X:= X;
  FList^[Count - 1].Y:= Y;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.Clear;
begin
  FCount:= 0;
end;


//------------------------------------------------------------------------------
procedure TPHXVectorList2f.Grow;
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
procedure TPHXVectorList2f.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TVector2f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXVectorList2f.GetItem(Index: Integer): TVector2f;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList2f.SetItem(Index: Integer; const Value: TVector2f);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXVectorList3f'}

// TPHXVectorList3f
//==============================================================================
constructor TPHXVectorList3f.Create;
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
constructor TPHXVectorList3f.Create(ACapacity: Integer);
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;

  SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXVectorList3f.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.SaveToFile(const FileName: String);
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
procedure TPHXVectorList3f.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^ , SizeOf(TVector3f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^ , SizeOf(TVector3f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.Add(const Vector: TVector3f);
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Vector;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.Add(const X, Y, Z: Single);
begin
  SetCount(Count + 1);

  FList^[Count - 1].X:= X;
  FList^[Count - 1].Y:= Y;
  FList^[Count - 1].Y:= Z;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  end;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TVector3f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXVectorList3f.GetItem(Index: Integer): TVector3f;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXVectorList3f.SetItem(Index: Integer; const Value: TVector3f);
begin
  FList^[Index]:= Value;
end;


{$ENDREGION}

{$REGION 'TPHXColorList4f'}

// TPHXColorList4f
//==============================================================================
constructor TPHXColorList4f.Create;
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
constructor TPHXColorList4f.Create(ACapacity: Integer);
begin
  FCapacity:= 0;
  FCount   := 0;
  FList    := nil;

  SetCapacity(ACapacity);
end;

//------------------------------------------------------------------------------
destructor TPHXColorList4f.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.SaveToFile(const FileName: String);
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
procedure TPHXColorList4f.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  Stream.Read(FList^ , SizeOf(TColor4f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCount, SizeOf(FCount));

  Stream.Write(FList^ , SizeOf(TColor4f) * FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.Add(const Color: TColor4f);
begin
  SetCount(Count + 1);

  FList^[Count - 1]:= Color;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.Add(const R, G, B, A: Single);
begin
  SetCount(Count + 1);

  FList^[Count - 1].Red  := R;
  FList^[Count - 1].Green:= G;
  FList^[Count - 1].Blue := B;
  FList^[Count - 1].Alpha:= A;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  end;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TColor4f));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;

//------------------------------------------------------------------------------
function TPHXColorList4f.GetItem(Index: Integer): TColor4f;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXColorList4f.SetItem(Index: Integer; const Value: TColor4f);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXRecti'}

// TPHXRecti
//==============================================================================
constructor TPHXRecti.Create;
begin
  FValue.Left  := 0;
  FValue.Top   := 0;
  FValue.Right := 0;
  FValue.Bottom:= 0;
end;

//------------------------------------------------------------------------------
constructor TPHXRecti.Create(ARect: TRecti);
begin
  FValue:= ARect;
end;

//------------------------------------------------------------------------------
constructor TPHXRecti.Create(Left, Top, Right, Bottom: Integer);
begin
  FValue.Left  := Left;
  FValue.Top   := Top;
  FValue.Right := Right;
  FValue.Bottom:= Bottom;
end;

//------------------------------------------------------------------------------
procedure TPHXRecti.LoadRect(Reader: TPHXReader);
begin
  FValue.Left  := Reader.ReadInteger;
  FValue.Top   := Reader.ReadInteger;
  FValue.Right := Reader.ReadInteger;
  FValue.Bottom:= Reader.ReadInteger;
end;

//------------------------------------------------------------------------------
procedure TPHXRecti.SaveRect(Writer: TPHXWriter);
begin
  Writer.WriteInteger(FValue.Left);
  Writer.WriteInteger(FValue.Top);
  Writer.WriteInteger(FValue.Right);
  Writer.WriteInteger(FValue.Bottom);
end;


//------------------------------------------------------------------------------
function TPHXRecti.PointInside(const X, Y: Integer): Boolean;
begin
  Result:= (X >= FValue.Left  ) and
           (X <  FValue.Right ) and
           (Y >= FValue.Top   ) and
           (Y <  FValue.Bottom);
end;

//------------------------------------------------------------------------------
function TPHXRecti.PointInside(const Point: TVector2i): Boolean;
begin
  Result:= (Point.X >= FValue.Left  ) and
           (Point.X <  FValue.Right ) and
           (Point.Y >= FValue.Top   ) and
           (Point.Y <  FValue.Bottom);
end;

//------------------------------------------------------------------------------
function TPHXRecti.GetHeight: Integer;
begin
  Result:= FValue.Bottom - FValue.Top;
end;

//------------------------------------------------------------------------------
function TPHXRecti.GetWidth: Integer;
begin
  Result:= FValue.Right - FValue.Left;
end;

{$ENDREGION}

{$REGION 'TPHXDictionary'}

// http://code.google.com/p/tiny-json/source/browse/trunk/Hashes.pas?r=2

// TPHXDictionary
//==============================================================================
constructor TPHXDictionary.Create(const ASize: Integer = 128);
begin
  Resize(ASize);
end;

//------------------------------------------------------------------------------
destructor TPHXDictionary.Destroy;
begin
  ClearBuckets;

  SetCapacity(0);
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXDictionary.Clear;
var Bucket: Integer;
begin
  FCount:= 0;

  // Reset all buckets
  for Bucket:= 0 to FBucketSize - 1 do
  begin
    FBucketList[Bucket].Count   := 0;
	  FBucketList[Bucket].Capacity:= 4;

	  SetLength(FBucketList[Bucket].Items, FBucketList[Bucket].Capacity);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Add(const Key: String; Value: Pointer);
var Hash: Cardinal;
begin
  Hash:= ELFHash(Key);

  Add(Hash, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Add(const Hash: Cardinal; Value: Pointer);
var Index: Integer;
var Bucket: Integer;
begin
  Index:= FCount;

  SetCount(FCount + 1);

  FItems^[Index].Hash:= Hash;
  FItems^[Index].Value:= Value;

  Bucket:= Hash mod FBucketSize;

  Inc(FBucketList[Bucket].Count);

  // Need to increase the bucket capacity
  if FBucketList[Bucket].Count > FBucketList[Bucket].Capacity then
  begin
    FBucketList[Bucket].Capacity:= FBucketList[Bucket].Capacity + 1;

    SetLength(FBucketList[Bucket].Items, FBucketList[Bucket].Capacity);
  end;

  // Add the item to the bucket
  FBucketList[Bucket].Items[FBucketList[Bucket].Count -1]:= Index;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Delete(const Key: String);
var Hash: Cardinal;
begin
  Hash:= ELFHash(Key);

  Delete(Hash);
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Delete(const Hash: Cardinal);
var Index : Integer;
var Bucket: Cardinal;
begin
  Index:= FindItem(Hash);

  If (Index < 0) or (Index >= FCount) then Exit;

  FCount := FCount-1;

  System.Move(FItems^[Index+1], FItems^[Index], (FCount - Index) * SizeOf(THashRecord));

  // Clear all buckets
  for Bucket:= 0 to FBucketSize - 1 do
  begin
	  FBucketList[Bucket].Count:= 0;
  end;

  // Insert all items
  for Index:= 0 to FCount - 1 do
  begin
    // Get the bucket for the item
    Bucket:= FItems^[Index].Hash mod FBucketSize;

    Inc(FBucketList[Bucket].Count);

    // Need to increase the bucket capacity
    if FBucketList[Bucket].Count > FBucketList[Bucket].Capacity then
    begin
      FBucketList[Bucket].Capacity:= FBucketList[Bucket].Capacity + 1;

      SetLength(FBucketList[Bucket].Items, FBucketList[Bucket].Capacity);
    end;

    // Add the item to the bucket
    FBucketList[Bucket].Items[FBucketList[Bucket].Count -1]:= Index;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  end;

  SetCapacity(FCount + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.ClearBuckets;
var Bucket: Integer;
begin
  // Clear all buckets
  for Bucket:= 0 to FBucketSize - 1 do
  begin
	  SetLength(FBucketList[Bucket].Items, 0);
  end;

  SetLength(FBucketList, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.Resize(const Size: Integer);
var Index : Integer;
var Bucket: Cardinal;
begin
  ClearBuckets;

  FBucketSize:= Size;

  SetLength(FBucketList, FBucketSize);
  // Reset all buckets
  for Bucket:= 0 to FBucketSize - 1 do
  begin
    FBucketList[Bucket].Count   := 0;
	  FBucketList[Bucket].Capacity:= 4;

	  SetLength(FBucketList[Bucket].Items, FBucketList[Bucket].Capacity);
  end;
  // Insert all items
  for Index:= 0 to FCount - 1 do
  begin
    // Get the bucket for the item
    Bucket:= FItems^[Index].Hash mod FBucketSize;

    Inc(FBucketList[Bucket].Count);

    // Need to increase the bucket capacity
    if FBucketList[Bucket].Count > FBucketList[Bucket].Capacity then
    begin
      FBucketList[Bucket].Capacity:= FBucketList[Bucket].Capacity + 1;

      SetLength(FBucketList[Bucket].Items, FBucketList[Bucket].Capacity);
    end;

    // Add the item to the bucket
    FBucketList[Bucket].Items[FBucketList[Bucket].Count -1]:= Index;
  end;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.FindItem(const Key: String): Integer;
var Hash: Cardinal;
begin
  Hash:= ELFHash(Key);

  Result:= FindItem(Hash);
end;

//------------------------------------------------------------------------------
function TPHXDictionary.FindItem(const Hash: Cardinal): Integer;
var Bucket: Cardinal;
var Index : Integer;
var Item  : Integer;
begin
  Bucket:= Hash mod FBucketSize;

  // Find the item in the bucket
  for Index:= 0 to FBucketList[Bucket].Count - 1 do
  begin
    Item:= FBucketList[Bucket].Items[Index];

    if FItems^[ Item ].Hash = Hash then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.ContainsKey(const Key: String): Boolean;
begin
  Result:= FindItem(Key) >= 0;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.ContainsKey(const Hash: Cardinal): Boolean;
begin
  Result:= FindItem(Hash) >= 0;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.ContainsValue(const Value: Pointer): Boolean;
var Index: Integer;
begin
  for Index := 0 to FCount - 1 do
  begin
    if FItems^[Index].Value = Value then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.GetItem(const Index: Integer): Pointer;
begin
  Result:= FItems[Index].Value;
end;

//------------------------------------------------------------------------------
function TPHXDictionary.GetValue(const Key: String): Pointer;
var Index: Integer;
begin
  Index:= FindItem(Key);

  if Index >= 0 then
  begin
    Result:= FItems^[Index].Value;
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.SetValue(const Key: String; Value: Pointer);
var Index: Integer;
begin
  Index:= FindItem(Key);

  if Index >= 0 then
  begin
    FItems^[Index].Value:= Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FItems, FCapacity * SizeOf(THashRecord));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDictionary.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;


{$ENDREGION}




{$REGION 'TPHXBuffer'}

// TPHXBuffer
//==============================================================================
constructor TPHXBuffer.Create;
begin
  FPrimitive:= PHX_TRIANGLES;

  FVertices:= TPHXVertexList.Create(BufferCapacityVertices);
  FIndices := TPHXIndexList.Create(BufferCapacityIndices);
end;

//------------------------------------------------------------------------------
destructor TPHXBuffer.Destroy;
begin
  FVertices.Free;
  FIndices.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXBuffer.Clear;
begin
  FVertices.Count:= 0;
  FIndices .Count:= 0;
end;




//------------------------------------------------------------------------------
procedure TPHXBuffer.SetPrimitive(const Value: TPHXPrimitiveType);
begin
  FPrimitive := Value;
end;

//------------------------------------------------------------------------------
procedure TPHXBuffer.SetUseage(const Value: TPHXBufferUsage);
begin
  FUseage := Value;
end;

{$ENDREGION}

{$REGION 'TPHXDisplayMode'}


// TPHXDisplayMode
//==============================================================================
class function TPHXDisplayMode.Create(const ADescription: String; const AWidth, AHeight: Integer): TPHXDisplayMode;
begin
  Result.Description:= ShortString(ADescription);
  Result.Width      :=             AWidth;
  Result.Height     :=             AHeight;
  Result.Fullscreen :=             False;
end;

{$ENDREGION}

{$REGION 'TPHXDisplay'}

const
  // Default display mode format
  DISPLAY_MODE_FORMAT = '%dx%d';

// TPHXDisplay
//==============================================================================
constructor TPHXDisplayModes.Create;
begin
  FDescriptions:= TStringList.Create;

  FDesktop.Description:= 'Desktop';
  FDesktop.Width      := 0;
  FDesktop.Height     := 0;
  FDesktop.Fullscreen := False;
end;

//------------------------------------------------------------------------------
destructor TPHXDisplayModes.Destroy;
begin
  FDescriptions.Free;
  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.Clear;
begin
  FCount:= 0;

  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.Add(const Mode: TPHXDisplayMode);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;


  FList^[FCount-1]:= Mode;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.Add(const Width, Height: Integer);
var Mode: TPHXDisplayMode;
begin
  Mode.Description:= ShortString(Format(DISPLAY_MODE_FORMAT, [Width, Height]));
  Mode.Width      := Width;
  Mode.Height     := Height;
  Mode.Fullscreen := False;

  Add(Mode);
end;

//------------------------------------------------------------------------------
procedure QuickSortDisplayModes(SortList: PDisplayModeList; L, R: Integer);
var I, J: Integer;
var P, T: TPHXDisplayMode;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SortList^[I].Width < P.Width do
        Inc(I);
      while SortList^[J].Width > P.Width do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortDisplayModes(SortList, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.Sort;
begin
  if (FList <> nil) and (Count > 1) then
  begin
    QuickSortDisplayModes(FList, 0, Count - 1);
  end;
end;

//------------------------------------------------------------------------------
function TPHXDisplayModes.IndexOf(const Description: String): Integer;
var Index: Integer;
begin
  for Index:=0 to Count - 1 do
  begin
    if SameText( String(FList^[Index].Description), Description) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXDisplayModes.IndexOf(const Width, Height: Integer): Integer;
var Index: Integer;
begin
  for Index:=0 to Count - 1 do
  begin
    if (FList^[Index].Width = Width) and (FList^[Index].Height = Height) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity:= Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXDisplayMode));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplayModes.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount:= Value;

    if FCount > FCapacity then SetCapacity(FCount);
  end;
end;

//------------------------------------------------------------------------------
function TPHXDisplayModes.GetDescriptions: TStrings;
var Index: Integer;
begin
  if FDescriptions.Count <> Count then
  begin
    FDescriptions.Clear;
    for Index := 0 to FCount-1 do
    begin
      FDescriptions.Add(String(FList^[Index].Description));
    end;
  end;
  Result:= FDescriptions;
end;

//------------------------------------------------------------------------------
function TPHXDisplayModes.GetItem(Index: Integer): TPHXDisplayMode;
begin
  Result:= FList^[Index];
end;

{$ENDREGION}

{$REGION 'TPHXNotifications'}

type PMethod = ^TMethod;

var Notifications: TList;


// TPHXNotifications
//==============================================================================
class procedure TPHXNotifications.Create;
begin
  Notifications:= TList.Create;
end;

//------------------------------------------------------------------------------
class procedure TPHXNotifications.Free;
begin
  Clear;

  Notifications.Free;
end;

//------------------------------------------------------------------------------
class procedure TPHXNotifications.Add(Event: TPHXNotificationEvent);
var Method: PMethod;
begin
  New(Method);

  Method^.Code:= TMethod(Event).Code;
  Method^.Data:= TMethod(Event).Data;

  Notifications.Add(Method);
end;

//------------------------------------------------------------------------------
class procedure TPHXNotifications.Clear;
var Index: Integer;
var Method: PMethod;
begin
  for Index := 0 to Notifications.Count-1 do
  begin
    Method:= PMethod(Notifications.List[Index]);

    Dispose(Method);
  end;
  Notifications.Clear;
end;

//------------------------------------------------------------------------------
class procedure TPHXNotifications.Remove(Event: TPHXNotificationEvent);
var Index: Integer;
var Method: PMethod;
begin
  for Index := 0 to Notifications.Count-1 do
  begin
    Method:= PMethod(Notifications.List[Index]);

    if (Method^.Code = TMethod(Event).Code) and (Method^.Data = TMethod(Event).Data) then
    begin
      Notifications.Delete(Index);
      Dispose(Method);
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
class procedure TPHXNotifications.Notify(Notification: TPHXNotification);
var Index: Integer;
var Method: PMethod;
begin
  for Index := 0 to Notifications.Count-1 do
  begin
    Method:= PMethod(Notifications.List[Index]);

    TPHXNotificationEvent(Method^)(Notification);
  end;
end;



{$ENDREGION}






initialization
  TPHXContentManager.FLoader:= TPHXContentLoader.Create;

  TPHXNotifications.Create;
finalization
  TPHXContentManager.FLoader.Free;

  TPHXNotifications.Free
end.
