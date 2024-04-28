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
unit phxGraphics;
//< Classes for texture loading and rendering

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxClasses;

const

// Version of the graphic files
PHXGRAPHIC_VERSION = 1;

type

// Supported pixel formats
//------------------------------------------------------------------------------
TPHXPixelFormat = (
  // Empty pixel format
  pfNone,
  // 8 bit image containing grayscale values
//  pfGray,
  // 8 bit image containing alpha values
  pfAlpha,
  // 24 bit
  pfRGB,
  // 32 bit
  pfRGBA
);

//------------------------------------------------------------------------------
TPHXPixelChannel = (
  // Red channel
  pcRed,
  // Green channel
  pcGreen,
  // Blue channel
  pcBlue,
  // Alpha channel
  pcAlpha,
  // Red, Green and Blue channels
  pcRGB,
  // Red, Green, Blue and Alpha channels
  pcRGBA
);

// A single pixel in a graphic image
//------------------------------------------------------------------------------
TPHXPixel =  record
  public
    // The value of the red component
    Red: Byte;
    // The value of the green channel
    Green: Byte;
    // The value of the blue channel
    Blue: Byte;
    // The value of the alphia channel
    Alpha: Byte;
  public
  end;

// Pixel pointer
PPHXPixel = ^TPHXPixel;

PPHXPixelList = ^TPHXPixelList;
TPHXPixelList = array[0..$00FFFFFF] of TPHXPixel;

// Function callback for getting one pixel in a image
TGetPixel = procedure(var Pixel: PByte; out Color: TPHXPixel);
// Function callback for getting one pixel in a image
TSetPixel = procedure(var Pixel: PByte; const Color: TPHXPixel);

///  Returns the size of one pixel for a given pixel format
function GetPixelFormatSize(const Format: TPHXPixelFormat): Byte;
// Return the get pixel function for a given format
function GetPixelFormatGetter(const Format: TPHXPixelFormat): TGetPixel;
// Return the set pixel function for a given format
function GetPixelFormatSetter(const Format: TPHXPixelFormat): TSetPixel;


// TODO: Loading and saving as DDS?
// http://4coder.org/delphi-source-code/917/kambi_vrml_game_engine/src/images/dds.pas.html

{$REGION 'TPHXGraphic'}

type

TPHXGraphicFiler = class;

// The header for graphic files
//------------------------------------------------------------------------------
TPHXGraphicHeader = record
  // The id of the texture file, should always be PHXGFX.
  Ident  : array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

//------------------------------------------------------------------------------
TPHXGraphicCompression = (
  // The graphic is stored as raw values
  gcUncompressed = 0,
  // The graphic is stored using ZLib Compression
  gcZLib = 1,
  // Save the graphic as a DDS image
  gcDDS = 2
);

// Container for a graphics image of a given size and format
//------------------------------------------------------------------------------
TPHXGraphic = record
  public
    // Width of the graphic
    Width: Integer;
     // Height
    Height: Integer;
    // Format of the pixels
    Format: TPHXPixelFormat;
    // Size in bytes of the image
    Size: Integer;
    // Pointer to the pixels
    Pixels: PByteArray;
  public
    // Creates a graphic
    class function Create(const AWidth, AHeight: Integer; const AFormat: TPHXPixelFormat; const ASize: Integer; const APixels: PByteArray): TPHXGraphic; static;
  public
    // Load this graphic from a file
    procedure LoadFromFile(const FileName: String);
    // Load this graphic from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save this graphic to a file
    procedure SaveToFile(const FileName: String; const Compression: TPHXGraphicCompression = gcZLib);
    // Save this graphic to a stream
    procedure SaveToStream(Stream: TStream; const Compression: TPHXGraphicCompression = gcZLib);

    // Resize the graphic, dont forget to free the pixel array whey you are done with it
    procedure Resize(const AWidth: Integer; const AHeight: Integer; const AFormat: TPHXPixelFormat);

    // Returns a pointer to the first pixel of a line
    function ScanLine(Line: Integer): PByte;
  end;

{$ENDREGION}

{$REGION 'TPHXGraphicFiler'}

// Class for loading and saving graphics images
//------------------------------------------------------------------------------
TPHXGraphicFiler = class
  protected
    // Read a graphic from a stream
    procedure LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic); virtual;
    // Write a graphic image to a stream
    procedure SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic); virtual;
  public
    constructor Create;

    // Register the supported file formats for this graphic filer
    procedure RegisterFileFormats; virtual; abstract;

    // Returns if this filer supports reading of a format
    function SupportsReading(const Extension: String): Boolean; virtual;
    // Returns if this filer supports writing of a format
    function SupportsWriting(const Extension: String): Boolean; virtual;

    // Load a graphic from a file
    procedure LoadFromFile(const FileName: String; out Graphic: TPHXGraphic);
    // Load a graphic from a stream
    procedure LoadFromStream(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);

    // Save a graphic to a file
    procedure SaveToFile(const FileName: String; const Graphic: TPHXGraphic); overload;
    // Save a graphic to a stream
    procedure SaveToStream(Stream: TStream; const Name: String; const Graphic: TPHXGraphic); overload;
  public
    // Register a graphic file format
    class procedure RegisterFileFormat(Filer: TPHXGraphicFiler; const Extension: String; const Name: String = '');
  end;

//------------------------------------------------------------------------------
TPHXGraphicReader = class(TPHXGraphicFiler)
end;

//------------------------------------------------------------------------------
TPHXGraphicWriter = class(TPHXGraphicFiler)
end;

{$ENDREGION}

{$REGION 'TPHXGraphicFormat'}

// A graphic format is a description of a file format and the graphic importer
//------------------------------------------------------------------------------
TPHXGraphicFormat = record
  public
    // File extension including the dot
    Extension: String[8];
    // Name of the format
    Name: String[32];
    // Importer to use to load this format, nil if loading isnt supported
    Filer: TPHXGraphicFiler;
  public
    // Creates a new graphic format
    class function Create(const Extension, Name: String; Filer: TPHXGraphicFiler): TPHXGraphicFormat; static;
  end;

//------------------------------------------------------------------------------
PGraphicFormatList = ^TGraphicFormatList;
TGraphicFormatList = array[0..$00FFFFFF] of TPHXGraphicFormat;

// Container for a list of graphic formats
//------------------------------------------------------------------------------
TPHXGraphicFormats = class
  private
    FCount: Integer;

    FCapacity: Integer;

    FList: PGraphicFormatList;

    procedure Grow;

    function GetFilter: String;
    function GetItem(Index: Integer): TPHXGraphicFormat;

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
    procedure SetItem    (Index: Integer; const Value: TPHXGraphicFormat);
  public
    // Create a new graphic format list
    constructor Create;
    // Free the list
    destructor Destroy; override;

    // Remove all formats from the list
    procedure Clear;

    // Add a graphic format
    procedure Add(const Format: TPHXGraphicFormat); overload;

    // Return the index of a graphic format for a filename
    function IndexOf(const FileName: String): Integer;
    // Find the graphic format for a filename
    function Find(const FileName: String): TPHXGraphicFormat; overload;
    // Find the graphic format for a filename
    function Find(const FileName: String; out Format: TPHXGraphicFormat): Boolean; overload;

    // Return true if reading of the selected file is supported
    function SupportsReading(const Filename: string): Boolean;
    // Return true if writing of the selected file is supported
    function SupportsWriting(const Filename: string): Boolean;

    // List all supported graphic formats
    procedure ListFormats(const Lines: TStrings);

    // Number of formats in the list
    property Count: Integer read FCount write SetCount;
    // Current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Return a pointer to the internal list of formats
    property List: PGraphicFormatList read FList;
    // Read or write a format in the list
    property Items[Index: Integer]: TPHXGraphicFormat read GetItem write SetItem;
    // Get a format by extension
    property Format[const Name: String]: TPHXGraphicFormat read Find; default;
    // Return a filter for a file dialog containing all supported graphic formats
    property Filter: String read GetFilter;
  end;

// Save a graphic to a stream using this format
//procedure SaveGraphic(Stream: TStream; const Graphic: TPHXGraphic);
// Load a graphic from a stream using this format
//procedure LoadGraphic(Stream: TStream; var Graphic: TPHXGraphic);

// List of supported graphic formats for saving and loading textures
function GraphicFormats: TPHXGraphicFormats;

{$ENDREGION}

{$REGION 'TPHXBitmap'}

type

// The bitmap is a class for manipulating the pixels of a graphic
//------------------------------------------------------------------------------

{ TPHXBitmap }

TPHXBitmap = class
  private
    FName: String;
    // Width
    FWidth : Integer;
    // Height
    FHeight: Integer;
    // The pixel format
    FFormat: TPHXPixelFormat;
    // Size in bytes of the bitmap
    FSize: Integer;
    // The bitmap pixels
    FPixels: PByteArray;

    //FGraphicFormat: TPHXGraphicFormat;
    //FCompressor: TPHXGraphicHandler;

   // Resizez the bitmap depending on the current width, height and format
    procedure ResizePixels;

    function GetGraphic: TPHXGraphic;
    function GetStride: Integer;

    function GetPixel(const X, Y: Integer): TPHXPixel;
    procedure SetPixel(const X, Y: Integer; const Value: TPHXPixel);

    procedure SetFormat(const AValue: TPHXPixelFormat);
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth (const AValue: Integer);
  protected
    FPixelGet: TGetPixel;
    FPixelSet: TSetPixel;
  public
    // Creates a empty bitmap
    constructor Create; overload;
    // Create a bitmap with a given size and format
    constructor Create(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat); overload;
    // Create a bitmap from a graphic
    constructor Create(const Graphic: TPHXGraphic); overload;
    // Defalt destructor
		destructor Destroy; override;

    // Load the bitmap from a file
    procedure LoadBitmap(const FileName: String); overload;
    // Load the bitmap from a stream
    procedure LoadBitmap(const FileName: String; Stream: TStream); overload;

    // Save the bitmap to a file
    procedure SaveBitmap(const FileName: String); overload;
    // Save the bitmap to a stream
    procedure SaveBitmap(const FileName: String; Stream: TStream); overload;

    // Resize the bitmap
    procedure Resize(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat);

    // Resize the canvas without modifying or scaling the pixel data
    procedure ResizeCanvas(const AWidth: Integer; const AHeight: Integer);
    // Resize the canvas to a power of two size without modifying or scaling the pixel data
    procedure ResizeCanvasPowerOfTwo;

    // Import the bitmap
    procedure Import(const Graphic: TPHXGraphic); overload;
    // Import the bitmap
    procedure Import(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat; Data: Pointer); overload;
    // Import the bitmap
    procedure Import(const Graphic: TPHXGraphic; const Channel: TPHXPixelChannel); overload;
    // Import the bitmap and swap color
    procedure ImportAndSwapColor(const Graphic: TPHXGraphic);

    // Fill with a color
    procedure Fill(const Red: Byte = 0; Green: Byte = 0; Blue: Byte = 0; Alpha: Byte = 0);

    procedure Convert(NewFormat: TPHXPixelFormat);

    // Load alpha from another bitmap
    procedure LoadAlphaFromBitmap(Bitmap: TPHXBitmap);

    // Copy a portion of another bitmap to this bitmap
    procedure CopyFrom(const Bitmap : TPHXBitmap ; const Source: TRecti; const Dest: TVector2i ); overload;
    // Copy a portion of another graphic to this bitmap
    procedure CopyFrom(const Graphic: TPHXGraphic; const Source: TRecti; const Dest: TVector2i ); overload;

    procedure CopyTo(const Graphic: TPHXGraphic; const Source: TRecti; const Dest: TVector2i ); overload;

    // Build the alpha match
    procedure BuildAlphaMatch(const Red, Green, Blue: Byte);

    // Returns a pointer to the first pixel of a given row
    function ScanLine(Row: Integer): pByte;

    // Draw a graphic onto this bitmap using alpha blending
    procedure Draw(const Graphic: TPHXGraphic; const SrcRect, DstRect: TRecti);

    // Name of the bitmap
    property Name: String read FName write FName;
    // Width of the bitmap
    property Width: Integer read FWidth write SetWidth;
    // Height of the bitmap
    property Height: Integer read FHeight write SetHeight;
    // Format of each pixel in the bitmap
    property Format: TPHXPixelFormat read FFormat write SetFormat;
    // Size of the bitmap in bytes
    property Size: Integer read FSize;
    // Pointer to the internal bitmap data
    property Pixels: PByteArray read FPixels;
    // Get the size of a row of the image in bytes
    property Stride: Integer read GetStride;

    property Pixel[const X,Y: Integer]: TPHXPixel read GetPixel write SetPixel; default;

    property Graphic: TPHXGraphic read GetGraphic;
    // The compressor to use to compress the pixel data when saving the texture
//    property GraphicFormat: TPHXGraphicFormat read FGraphicFormat write FGraphicFormat;
    // Importer to use to load this format, nil if loading isnt supported
  //  property GraphicFiler: TPHXGraphicFiler read FGraphicFormat.Filer write FGraphicFormat.Filer;
  end;

{$ENDREGION}

const
  // The default graphic compression to use
  DefaultGraphicCompression: TPHXGraphicCompression = gcZLib;


function isPowerOfTwo(Size: Integer): Boolean;
function nextPowerOfTwo(Size: Integer): Integer;

function SavePixels(Stream: TStream; const APixels: PByteArray; const ASize: Integer): Boolean;
function LoadPixels(Stream: TStream; const APixels: PByteArray; const ASize: Integer): Boolean;

implementation

uses

  {$IFDEF FPC}
  PASzlib //zlib //dzlib,
  {$ELSE}
//  phxZLib
  ZLib
  {$ENDIF};

//------------------------------------------------------------------------------
function isPowerOfTwo(Size: Integer): Boolean;
begin
  Result:=(Size > 0) and ((Size and (Size - 1)) = 0);
end;

//------------------------------------------------------------------------------
function nextPowerOfTwo(Size: Integer): Integer;
begin
  Result:=2;

  if(Size < 2) then Exit;

  while Result < Size do
  begin
    Result:= Result * 2;
  end;
end;


//------------------------------------------------------------------------------
function SavePixels(Stream: TStream; const APixels: PByteArray; const ASize: Integer): Boolean;
{$IFDEF FPC}
var Buffer    : Pchar;
var BufferSize: cardinal;
{$ELSE}
var Buffer    : Pointer;
var BufferSize: Cardinal;
{$ENDIF}
begin
  GetMem(Buffer, ASize);
  try
    BufferSize:= ASize;

    {$IFNDEF FPC}
    Result:= ZLib.compress(Buffer, BufferSize, @APixels^[0], ASize) = Z_OK;
    {$ELSE}
    Result:= PASzlib.compress(Buffer, BufferSize, @APixels^[0] , ASize) = Z_OK;
    {$ENDIF}

    Stream.Write(BufferSize, SizeOf(Cardinal));
    Stream.Write(Buffer^   , BufferSize);
  finally
    FreeMem(Buffer);

  end;
end;

//------------------------------------------------------------------------------
function LoadPixels(Stream: TStream; const APixels: PByteArray; const ASize: Integer): Boolean;
var DiskSize: Cardinal;
var OutSize   : Cardinal;

{$IFDEF FPC}
var Buffer    : Pchar;
var BufferSize: cardinal;
{$ELSE}
var Buffer    : Pointer;
var BufferSize: Cardinal;
{$ENDIF}
begin
  OutSize  := ASize;

  Stream.Read(DiskSize, SizeOf(Cardinal));

  BufferSize:= DiskSize;

  GetMem(Buffer, BufferSize);
  try
    Stream.Read(Buffer^, BufferSize);

    {$IFNDEF FPC}
    Result:= ZLib.uncompress(@APixels^[0], OutSize, Buffer, BufferSize) = Z_OK;
    {$ELSE}
    Result:= PASzlib.uncompress(@APixels^[0], OutSize, Buffer, BufferSize) = Z_OK;
    {$ENDIF}
  finally
    FreeMem(Buffer);
  end;

end;




{$REGION 'TPHXPixel'}

// pfNone
//==============================================================================
procedure GetPixel_None(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := 0;
  Color.Green:= 0;
  Color.Blue := 0;
  Color.Alpha:= 0;
end;

//------------------------------------------------------------------------------
procedure SetPixel_None(var Pixel: PByte; const Color: TPHXPixel);
begin
end;

// pfAlpha
//==============================================================================
procedure GetPixel_Alpha(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := 255;
  Color.Green:= 255;
  Color.Blue := 255;
  Color.Alpha:= Pixel^;
  Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_Alpha(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^:= Color.Alpha;
  Inc(Pixel);
end;

// pfGray
//==============================================================================
procedure GetPixel_Gray(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := Pixel^;
  Color.Green:= Pixel^;
  Color.Blue := Pixel^;
  Color.Alpha:= 255;
  Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_Gray(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^:= Color.Red;
  Inc(Pixel);
end;

// pfRed
//==============================================================================
procedure GetPixel_Red(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := Pixel^;
  Color.Green:= 0;
  Color.Blue := 0;
  Color.Alpha:= 255;
  Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_Red(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^:= Color.Red;
  Inc(Pixel);
end;

// pfGreen
//==============================================================================
procedure GetPixel_Green(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := 0;
  Color.Green:= Pixel^;
  Color.Blue := 0;
  Color.Alpha:= 255;
  Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_Green(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^:= Color.Green;
  Inc(Pixel);
end;

// pfBlue
//==============================================================================
procedure GetPixel_Blue(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := 0;
  Color.Green:= 0;
  Color.Blue := Pixel^;
  Color.Alpha:= 255;
  Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_Blue(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^:= Color.Blue;
  Inc(Pixel);
end;


// pfRGB
//==============================================================================
procedure GetPixel_RGB(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := Pixel^; Inc(Pixel);
  Color.Green:= Pixel^; Inc(Pixel);
  Color.Blue := Pixel^; Inc(Pixel);
  Color.Alpha:= 255;
end;

//------------------------------------------------------------------------------
procedure SetPixel_RGB(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^ := Color.Red;   Inc(Pixel);
  Pixel^ := Color.Green; Inc(Pixel);
  Pixel^ := Color.Blue;  Inc(Pixel);
end;

// pfRGBA
//==============================================================================
procedure GetPixel_RGBA(var Pixel: PByte; out Color: TPHXPixel);
begin
  Color.Red  := Pixel^; Inc(Pixel);
  Color.Green:= Pixel^; Inc(Pixel);
  Color.Blue := Pixel^; Inc(Pixel);
  Color.Alpha:= Pixel^; Inc(Pixel);
end;

//------------------------------------------------------------------------------
procedure SetPixel_RGBA(var Pixel: PByte; const Color: TPHXPixel);
begin
  Pixel^ := Color.Red;   Inc(Pixel);
  Pixel^ := Color.Green; Inc(Pixel);
  Pixel^ := Color.Blue;  Inc(Pixel);
  Pixel^ := Color.Alpha; Inc(Pixel);
end;
     {
//------------------------------------------------------------------------------
const PixelGetters: array[TPHXPixelFormat] of TGetPixel = (
  GetPixel_None,
  GetPixel_Gray,
  GetPixel_Alpha,
  GetPixel_RGB,
  GetPixel_RGBA
  );

//------------------------------------------------------------------------------
const PixelSetters: array[TPHXPixelFormat] of TSetPixel = (
  SetPixel_None,
  SetPixel_Gray,
  SetPixel_Alpha,
  SetPixel_RGB,
  SetPixel_RGBA
  );
       }

//------------------------------------------------------------------------------
function GetPixelFormatSize(const Format: TPHXPixelFormat): Byte;
begin
  Result:= 0;
  case Format of
    pfNone     : Result:= 0;
    pfAlpha    : Result:= 1;
    pfRGB      : Result:= 3;
    pfRGBA     : Result:= 4;
  end;
end;

//------------------------------------------------------------------------------
function GetPixelFormatGetter(const Format: TPHXPixelFormat): TGetPixel;
begin
  Result:= nil;
  case Format of
    pfNone     : Result:= {$IFDEF FPC}@{$ENDIF}GetPixel_None;
    pfAlpha    : Result:= {$IFDEF FPC}@{$ENDIF}GetPixel_Alpha;
    pfRGB      : Result:= {$IFDEF FPC}@{$ENDIF}GetPixel_RGB;
    pfRGBA     : Result:= {$IFDEF FPC}@{$ENDIF}GetPixel_RGBA;
  end;
end;

//------------------------------------------------------------------------------
function GetPixelFormatSetter(const Format: TPHXPixelFormat): TSetPixel;
begin
  Result:= nil;
  case Format of
    pfNone     : Result:= {$IFDEF FPC}@{$ENDIF}SetPixel_None;
    pfAlpha    : Result:= {$IFDEF FPC}@{$ENDIF}SetPixel_Alpha;
    pfRGB      : Result:= {$IFDEF FPC}@{$ENDIF}SetPixel_RGB;
    pfRGBA     : Result:= {$IFDEF FPC}@{$ENDIF}SetPixel_RGBA;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXGraphic'}

//------------------------------------------------------------------------------
procedure SaveGraphic(Stream: TStream; const Graphic: TPHXGraphic);
begin
  Stream.Write(Graphic.Width      , SizeOf(Integer));
  Stream.Write(Graphic.Height     , SizeOf(Integer));
  Stream.Write(Graphic.Format     , SizeOf(TPHXPixelFormat));
  Stream.Write(Graphic.Size       , SizeOf(Integer));

  SavePixels(Stream, Graphic.Pixels, Graphic.Size);
end;

//------------------------------------------------------------------------------
procedure LoadGraphic(Stream: TStream; var Graphic: TPHXGraphic);
begin
  Stream.Read(Graphic.Width      , SizeOf(Integer));
  Stream.Read(Graphic.Height     , SizeOf(Integer));
  Stream.Read(Graphic.Format     , SizeOf(TPHXPixelFormat));
  Stream.Read(Graphic.Size       , SizeOf(Integer));

  ReAllocMem(Graphic.Pixels, Graphic.Size);

  LoadPixels(Stream, Graphic.Pixels, Graphic.Size);
end;

// TPHXGraphic
//==============================================================================
class function TPHXGraphic.Create(const AWidth, AHeight: Integer; const AFormat: TPHXPixelFormat; const ASize: Integer; const APixels: PByteArray): TPHXGraphic;
begin
  Result.Width      := AWidth;
  Result.Height     := AHeight;
  Result.Format     := AFormat;
  Result.Size       := ASize;
  Result.Pixels     := APixels;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphic.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphic.LoadFromStream(Stream: TStream);
var Header : TPHXGraphicHeader;
var Compression: TPHXGraphicCompression;
begin
  Header.Ident := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  // Check header
  if (Header.Ident <> 'PHXGFX') then
  begin
    raise Exception.Create('Not a valid Phoenix graphics.');
  end;
  // Check versoion
  if (Header.Version <> PHXGRAPHIC_VERSION) then
  begin
    raise Exception.CreateFmt('Graphic version mismatch [File: %d Code: %d].', [Header.Version, PHXGRAPHIC_VERSION]);
  end;

  Stream.Read(Self.Width      , SizeOf(Integer));
  Stream.Read(Self.Height     , SizeOf(Integer));
  Stream.Read(Self.Format     , SizeOf(TPHXPixelFormat));
  Stream.Read(Self.Size       , SizeOf(Integer));

  ReAllocMem(Self.Pixels, Self.Size);

  Stream.Read(Compression, SizeOf(TPHXGraphicCompression));

  case Compression of
    gcUncompressed:
    begin
      Stream.Read(Self.Pixels, Self.Size);
    end;
    gcZLib:
    begin
      LoadPixels(Stream, Self.Pixels, Self.Size);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphic.SaveToFile(const FileName: String; const Compression: TPHXGraphicCompression);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmCreate);
  try
    SaveToStream(Stream, Compression);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphic.SaveToStream(Stream: TStream; const Compression: TPHXGraphicCompression = gcZLib);
var Header : TPHXGraphicHeader;
begin
  Header.Ident  := 'PHXGFX';
  Header.Version:= PHXGRAPHIC_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Stream.Write(Self.Width      , SizeOf(Integer));
  Stream.Write(Self.Height     , SizeOf(Integer));
  Stream.Write(Self.Format     , SizeOf(TPHXPixelFormat));
  Stream.Write(Self.Size       , SizeOf(Integer));

  Stream.Write(Compression, SizeOf(TPHXGraphicCompression));

  case Compression of
    gcUncompressed:
    begin
      Stream.Write(Self.Pixels, Self.Size);
    end;
    gcZLib:
    begin
       SavePixels(Stream, Self.Pixels, Self.Size);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphic.Resize(const AWidth: Integer; const AHeight: Integer; const AFormat: TPHXPixelFormat);
begin
  Width := AWidth;
  Height:= AHeight;
  Format:= AFormat;
  Size  := Width * Height * GetPixelFormatSize(Format);
  Pixels:= GetMemory(Size);
end;

//------------------------------------------------------------------------------
function TPHXGraphic.ScanLine(Line: Integer): PByte;
var PixelSize: Integer;
begin
  PixelSize:= GetPixelFormatSize(Format);

  Result:= @Pixels^[Line * Width * PixelSize];
end;

{$ENDREGION}

{$REGION 'TPHXGraphicFiler'}


// TPHXGraphicFiler
//==============================================================================
constructor TPHXGraphicFiler.Create;
begin
  RegisterFileFormats;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFiler.SupportsReading(const Extension: String): Boolean;
begin
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFiler.SupportsWriting(const Extension: String): Boolean;
begin
  Result:= False;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
begin
  raise Exception.CreateFmt('The graphic filer %s does not support loading.', [ClassName]);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);
begin
  raise Exception.CreateFmt('The graphic filer %s does not support saving.', [ClassName]);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.LoadFromFile(const FileName: String; out Graphic: TPHXGraphic);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream, FileName, Graphic);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.LoadFromStream(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
begin
  LoadGraphic(Stream, Name, Graphic);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.SaveToFile(const FileName: String; const Graphic: TPHXGraphic);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream, FileName, Graphic);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFiler.SaveToStream(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);
begin
  SaveGraphic(Stream, Name, Graphic);
end;



//------------------------------------------------------------------------------
class procedure TPHXGraphicFiler.RegisterFileFormat(Filer: TPHXGraphicFiler; const Extension: String; const Name: String = '');
var Format: TPHXGraphicFormat;
begin
  Format.Name     := AnsiString(Name);
  Format.Extension:= AnsiString(Extension);
  Format.Filer    := Filer;

  GraphicFormats.Add(Format)
end;



{$ENDREGION}

{$REGION 'TPHXGraphicFormat'}

// TPHXGraphicFormat
//------------------------------------------------------------------------------
class function TPHXGraphicFormat.Create(const Extension: String; const Name: String; Filer: TPHXGraphicFiler): TPHXGraphicFormat;
begin
  Result.Extension:= ShortString(Extension);
  Result.Name     := ShortString(Name);
  Result.Filer    := Filer;
end;

// List of supported graphic formats for saving and loading textures
var _GraphicFormats: TPHXGraphicFormats;

// List of supported graphic formats for saving and loading textures
//------------------------------------------------------------------------------
function GraphicFormats: TPHXGraphicFormats;
begin
  Result:= _GraphicFormats;
end;


// TPHXGraphicFormats
//==============================================================================
constructor TPHXGraphicFormats.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXGraphicFormats.Destroy;
begin
  SetCount(0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.Add(const Format: TPHXGraphicFormat);
var Index: Integer;
begin
  Index:= IndexOf( String(Format.Extension));

  if Index >= 0 then
  begin
    FList^[Index].Filer:= Format.Filer;
  end else
  begin
    SetCount(FCount + 1);

    FList^[FCount-1].Extension:= Format.Extension;
    FList^[FCount-1].Name     := Format.Name;
    FList^[FCount-1].Filer    := Format.Filer;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.Grow;
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
procedure TPHXGraphicFormats.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXGraphicFormat));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then Grow;
end;


//------------------------------------------------------------------------------
function TPHXGraphicFormats.IndexOf(const FileName: String): Integer;
var Extension: String;
var Index: Integer;
begin
  Extension:= ExtractFileExt(FileName);

  for Index := 0 to FCount - 1 do
  begin
    if SameText( String(FList^[Index].Extension), Extension) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.Find(const FileName: String; out Format: TPHXGraphicFormat): Boolean;
var Extension: String;
var Index: Integer;
begin
  Extension:= ExtractFileExt(FileName);

  for Index := 0 to FCount - 1 do
  begin
    if (FList^[Index].Extension = '*') or SameText( String(FList^[Index].Extension), Extension) then
    begin
      Format:= FList^[Index];

      Result:= True;

      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.Find(const FileName: String): TPHXGraphicFormat;
var Extension: String;
var Index: Integer;
begin
  Extension:= ExtractFileExt(FileName);

  if Extension[1] <> '.' then
  begin
    Extension:= '.' + Extension;
  end;

  for Index := 0 to FCount - 1 do
  begin
    if (FList^[Index].Extension = '*') or SameText( String(FList^[Index].Extension), Extension) then
    begin
      Result:= FList^[Index];

      Exit;
    end;
  end;

  raise Exception.Create('Unregistered graphic format "' + FileName + '"');
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.SupportsReading(const Filename: string): Boolean;
var Extension: String;
var Index: Integer;
begin
  Extension:= ExtractFileExt(FileName);

  for Index := 0 to FCount - 1 do
  begin
    if SameText( String(FList^[Index].Extension), Extension) and (FList^[Index].Filer.SupportsReading(Extension)) then
    begin
      Result:= True;

      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.SupportsWriting(const Filename: string): Boolean;
var Extension: String;
var Index: Integer;
begin
  Extension:= ExtractFileExt(FileName);

  for Index := 0 to FCount - 1 do
  begin
    if SameText( String(FList^[Index].Extension), Extension) and (FList^[Index].Filer.SupportsWriting(Extension)) then
    begin
      Result:= True;

      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.ListFormats(const Lines: TStrings);
var Index     : Integer;
var Name     : String;
var Extension: String;
begin
  for Index := 0 to FCount - 1 do
  begin
    Name     := String(FList^[Index].Name);
    Extension:= String(FList^[Index].Extension);

    if Name <> '' then
    begin
      Lines.Add(Name + ' (* ' + Extension + ')');
    end else
    begin
      Lines.Add(Extension);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.GetFilter: String;
var Index: Integer;
var Filter: String;
var Name  : String;
var Ext   : String;
begin

  Filter := Default(String);

  if Count = 0 then
  begin
    Result:= '';
    Exit;
  end;

  Result:= 'All supported texture formats|';
  for Index := 0 to Count - 1 do
  begin
    Filter:=Filter + ';*' + String(FList^[Index].Extension);
  end;
  Filter:= Filter + '|';

  for Index := 0 to Count - 1 do
  begin
    Name:= String(FList^[Index].Name);
    Ext := String(FList^[Index].Extension);

    if Name <> '' then
    begin
      Filter:= Filter + SysUtils.Format('%s (.%s)|*.%s|', [Name, Ext, Ext]);
    end else
    begin
      Filter:= Filter + SysUtils.Format('%s|*.%s|', [Ext, Ext]);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXGraphicFormats.GetItem(Index: Integer): TPHXGraphicFormat;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXGraphicFormats.SetItem(Index: Integer; const Value: TPHXGraphicFormat);
begin
  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXBitmap'}

//TPHXBitmap
//==============================================================================
constructor TPHXBitmap.Create;
begin
  FWidth   := 0;
  FHeight  := 0;
  FFormat  := pfNone;
  FSize    := 0;
  FPixels  := nil;

  FPixelGet:= nil;
  FPixelSet:= nil;
end;

//------------------------------------------------------------------------------
constructor TPHXBitmap.Create(const Graphic: TPHXGraphic);
begin
  Resize(Graphic.Width, Graphic.Height, Graphic.Format);

  Move(Graphic.Pixels^, FPixels^, FSize);
end;

//------------------------------------------------------------------------------
constructor TPHXBitmap.Create(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat);
begin
  Create();

  Resize(AWidth, AHeight, AFormat);
end;

//------------------------------------------------------------------------------
destructor TPHXBitmap.Destroy;
begin
  // Free pixel data
  Resize(0,0, pfNone);

  inherited Destroy;
end;


//------------------------------------------------------------------------------
procedure TPHXBitmap.Draw(const Graphic: TPHXGraphic; const SrcRect, DstRect: TRecti);
var SX, SY   : Integer;
var DX, DY   : Integer;
var GetPixelA : TGetPixel;
var GetPixelB : TGetPixel;
var SetPixel : TSetPixel;
var SrcPitch : Integer;
var DstPitch : Integer;
var SrcPixelA : pByte;
var SrcPixelB : pByte;
var DstPixel : pByte;
var SrcColor : TPHXPixel;
var DstColor : TPHXPixel;
var prvColor : TPHXPixel;
var AlphaSrc: Single;
var AlphaDst: Single;
begin
  if (Graphic.Width = 0) or (Graphic.Height = 0) or (Self.Width = 0) or (Self.Height = 0) then Exit;

  GetPixelA:= GetPixelFormatGetter(Graphic.Format);
  GetPixelB:= GetPixelFormatGetter(Self   .Format);
  SetPixel := GetPixelFormatSetter(Self   .Format);

  SrcPitch:= GetPixelFormatSize(Graphic.Format);
  DstPitch:= GetPixelFormatSize(Self   .Format);

  SY:= SrcRect.Top;
  DY:= DstRect.Top;

  if DY < 0 then
  begin
    SY:= SY - DY;
    DY:= 0;
  end;
  if SY < 0 then
  begin
    DY:= DY - SY;
    SY:= 0;
  end;

  while (SY < SrcRect.Bottom) and (SY < Graphic.Height) and (DY < DstRect.Bottom)  and (DY < Height) do
  begin
    SX:= SrcRect.Left;
    DX:= DstRect.Left;

    if DX < 0 then
    begin
      SX:= SX - DX;
      DX:= 0;
    end;
    if SX < 0 then
    begin
      DX:= DX - SX;
      SX:= 0;
    end;


    SrcPixelA:= @Graphic.Pixels^[ (SX + (SY * Graphic.Width)) * SrcPitch ];
    SrcPixelB:= @Self   .Pixels^[ (DX + (DY * Self   .Width)) * DstPitch ];
    DstPixel := SrcPixelB;
    while (SX < SrcRect.Right) and (SX < Graphic.Width) and (DX < DstRect.Right) and (DX < Width) do
    begin
      GetPixelA(SrcPixelA, SrcColor);
      GetPixelB(SrcPixelB, PrvColor);

      AlphaSrc:= SrcColor.Alpha * (1 / 255);
      AlphaDst:= 1 - AlphaSrc;

      DstColor.Red  := Round( (SrcColor.Red   * AlphaSrc) + (PrvColor.Red   * AlphaDst) );
      DstColor.Green:= Round( (SrcColor.Green * AlphaSrc) + (PrvColor.Green * AlphaDst) );
      DstColor.Blue := Round( (SrcColor.Blue  * AlphaSrc) + (PrvColor.Blue  * AlphaDst) );
      DstColor.Alpha:= 255;


      SetPixel(DstPixel, DstColor);

      Inc(SX);
      Inc(DX);
    end;
    Inc(SY);
    Inc(DY);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.LoadBitmap(const FileName: String);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  if GraphicFormats.Find(Filename, Format) then
  begin
    Format.Filer.LoadFromFile(FileName , Graphic);

    Resize(0, 0, pfNone);

    // Copy the information from the loaded graphic
    FWidth := Graphic.Width;
    FHeight:= Graphic.Height;
    FFormat:= Graphic.Format;
    FSize  := Graphic.Size;
    FPixels:= Graphic.Pixels;

    FPixelGet:= GetPixelFormatGetter(FFormat);
    FPixelSet:= GetPixelFormatSetter(FFormat);
  end else
  begin
    // LogError();
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.LoadBitmap(const FileName: String; Stream: TStream);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  if GraphicFormats.Find(Filename, Format) then
  begin
    Format.Filer.LoadFromStream(Stream, FileName , Graphic);

    Resize(0, 0, pfNone);

    // Copy the information from the loaded graphic
    FWidth := Graphic.Width;
    FHeight:= Graphic.Height;
    FFormat:= Graphic.Format;
    FSize  := Graphic.Size;
    FPixels:= Graphic.Pixels;

    FPixelGet:= GetPixelFormatGetter(FFormat);
    FPixelSet:= GetPixelFormatSetter(FFormat);
  end else
  begin
    // LogError();
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.SaveBitmap(const FileName: String);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  if GraphicFormats.Find(Filename, Format) then
  begin
    // Copy the information from the loaded graphic
    Graphic.Width := FWidth;
    Graphic.Height:= FHeight;
    Graphic.Format:= FFormat;
    Graphic.Size  := FSize;
    Graphic.Pixels:= FPixels;

    Format.Filer.SaveToFile(FileName, Graphic);
  end else
  begin
    // LogError();
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.SaveBitmap(const FileName: String; Stream: TStream);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  if GraphicFormats.Find(Filename, Format) then
  begin
    // Copy the information from the loaded graphic
    Graphic.Width := FWidth;
    Graphic.Height:= FHeight;
    Graphic.Format:= FFormat;
    Graphic.Size  := FSize;
    Graphic.Pixels:= FPixels;

    Format.Filer.SaveToStream(Stream, FileName, Graphic);
  end else
  begin
    // LogError();
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBitmap.ResizePixels;
var ASize: Integer;
begin
  // Calculate the size of the texture
  ASize:= FWidth * FHeight * GetPixelFormatSize(FFormat);

  // Resize the pixel block if the size is changed
  if FSize <> ASize then
  begin
    FSize   := ASize;

    ReAllocMem(FPixels, FSize);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.Resize(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat);
var ASize: Integer;
begin
  // Calculate the size of the texture
  ASize:= AWidth * AHeight * GetPixelFormatSize(AFormat);

  // Resize the pixel block if the size is changed
  if FSize <> ASize then
  begin
    ReAllocMem(FPixels, ASize);
  end;

  FSize   := ASize;
  FWidth  := AWidth;
  FHeight := AHeight;
  FFormat := AFormat;

  FPixelGet:= GetPixelFormatGetter(FFormat);
  FPixelSet:= GetPixelFormatSetter(FFormat);
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.ResizeCanvas(const AWidth: Integer; const AHeight: Integer);
var ASize    : Integer;
var APixels  : PByteArray;
var Index    : Integer;
var SrcStride: Integer;
var DstStride: Integer;
begin
  Assert(AWidth  >= FWidth);
  Assert(AHeight >= FHeight);

  // Calculate the size of the new texture
  ASize:= AWidth * AHeight * GetPixelFormatSize(FFormat);

  // Allocate a new buffer
  GetMem(APixels, ASize);

  SrcStride:= GetPixelFormatSize(FFormat) * FWidth;
  DstStride:= GetPixelFormatSize(FFormat) * AWidth;

  FillChar(APixels^, ASize, 0);

  // Copy the old lines to the new texture
  for Index := 0 to FHeight - 1 do
  begin
    Move(FPixels^[Index * SrcStride], APixels^[Index * DstStride], SrcStride);
  end;

  FreeMem(FPixels);

  FSize   := ASize;
  FWidth  := AWidth;
  FHeight := AHeight;
  FPixels := APixels;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.ResizeCanvasPowerOfTwo;
var Width: Integer;
var Height: Integer;
begin
  Width := nextPowerOfTwo(FWidth);
  Height:= nextPowerOfTwo(FHeight);

  ResizeCanvas(Width, Height);
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.Import(AWidth, AHeight: Integer; AFormat: TPHXPixelFormat; Data: Pointer);
begin
  Resize(AWidth, AHeight, AFormat);
  Move(Data^, FPixels^, FSize);
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.Import(const Graphic: TPHXGraphic; const Channel: TPHXPixelChannel);
var X, Y: Integer;
var GetPixel: TGetPixel;
var SetPixel: TSetPixel;
var SrcPixel: pByte;
var DstPixel: pByte;
var SrcColor: TPHXPixel;
var DstColor: TPHXPixel;
begin
  Resize(Graphic.Width, Graphic.Height, Graphic.Format);

  GetPixel:= GetPixelFormatGetter(Graphic.Format);
  SetPixel:= GetPixelFormatSetter(Self   .Format);

  SrcPixel:= @Graphic.Pixels^[0];
  DstPixel:= @Self   .Pixels^[0];
  for Y:= 0 to Height - 1 do
  begin
    for X:= 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      case Channel of
        pcRed:
        begin
          DstColor.Red  := SrcColor.Red;
          DstColor.Green:= SrcColor.Red;
          DstColor.Blue := SrcColor.Red;
          DstColor.Alpha:= 255;
        end;
        pcGreen:
        begin
          DstColor.Red  := SrcColor.Green;
          DstColor.Green:= SrcColor.Green;
          DstColor.Blue := SrcColor.Green;
          DstColor.Alpha:= 255;
        end;
        pcBlue:
        begin
          DstColor.Red  := SrcColor.Blue;
          DstColor.Green:= SrcColor.Blue;
          DstColor.Blue := SrcColor.Blue;
          DstColor.Alpha:= 255;
        end;
        pcAlpha:
        begin
          DstColor.Red  := SrcColor.Alpha;
          DstColor.Green:= SrcColor.Alpha;
          DstColor.Blue := SrcColor.Alpha;
          DstColor.Alpha:= 255;
        end;
        pcRGB:
        begin
          DstColor.Red  := SrcColor.Red;
          DstColor.Green:= SrcColor.Green;
          DstColor.Blue := SrcColor.Blue;
          DstColor.Alpha:= 255;
        end;
        pcRGBA:
        begin

          DstColor.Red  := SrcColor.Red;
          DstColor.Green:= SrcColor.Green;
          DstColor.Blue := SrcColor.Blue;
          DstColor.Alpha:= SrcColor.Alpha;

        end;
      end;
      SetPixel(DstPixel, DstColor);
    end;
  end;
end;

procedure TPHXBitmap.ImportAndSwapColor(const Graphic: TPHXGraphic);
var X, Y: Integer;
var GetPixel: TGetPixel;
var SetPixel: TSetPixel;
var SrcPixel: pByte;
var DstPixel: pByte;
var SrcColor: TPHXPixel;
var DstColor: TPHXPixel;
begin
  Resize(Graphic.Width, Graphic.Height, Graphic.Format);

  GetPixel:= GetPixelFormatGetter(Graphic.Format);
  SetPixel:= GetPixelFormatSetter(Self   .Format);

  SrcPixel:= @Graphic.Pixels^[0];
  DstPixel:= @Self   .Pixels^[0];
  for Y:= 0 to Height - 1 do
  begin
    for X:= 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);
      DstColor.Red  := SrcColor.Blue;
      DstColor.Green:= SrcColor.Green;
      DstColor.Blue := SrcColor.Red;
      DstColor.Alpha:= SrcColor.Alpha;
      SetPixel(DstPixel, DstColor);
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.Import(const Graphic: TPHXGraphic);
begin
  Resize(Graphic.Width, Graphic.Height, Graphic.Format);
  Move(Graphic.Pixels^, FPixels^, FSize);
end;

 //------------------------------------------------------------------------------
procedure TPHXBitmap.SetWidth(const AValue: Integer);
begin
  if Width <> AValue then
  begin
    FWidth:= AValue;
    ResizePixels;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBitmap.SetHeight(const AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight:= AValue;
    ResizePixels;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.SetFormat(const AValue: TPHXPixelFormat);
begin
  if Format <> AValue then
  begin
    FFormat:= AValue;
    FPixelGet:= GetPixelFormatGetter(FFormat);
    FPixelSet:= GetPixelFormatSetter(FFormat);
    ResizePixels;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.SetPixel(const X, Y: Integer; const Value: TPHXPixel);
var pPixel: pByte;
begin
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
  begin
    raise Exception.CreateFmt('TPHXBitmap.SetPixel: Coordinate %d, %d out of bounds.', [X,Y]);
  end;

  if Assigned(FPixelSet) then
  begin
     pPixel:= @FPixels^[0];
     Inc(pPixel, (X + Y * Width) * GetPixelFormatSize(FFormat) );
     FPixelSet(pPixel, Value);
  end;
end;

//------------------------------------------------------------------------------
function TPHXBitmap.GetPixel(const X, Y: Integer): TPHXPixel;
var pPixel: pByte;
begin
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
  begin
    raise Exception.CreateFmt('TPHXBitmap.GetPixel: Coordinate %d, %d out of bounds.', [X,Y]);
  end;

  if Assigned(FPixelGet) then
  begin
     pPixel:= @FPixels^[0];
     Inc(pPixel, (X + Y * Width) * GetPixelFormatSize(FFormat) );
     FPixelGet(pPixel, Result);
  end;
end;

//------------------------------------------------------------------------------
function TPHXBitmap.GetStride: Integer;
begin
  Result:= Width * GetPixelFormatSize(FFormat);
end;

//------------------------------------------------------------------------------
function TPHXBitmap.ScanLine(Row: Integer): pByte;
var pPixel: pByte;
begin
  if(Row < 0) or (Row >= Height) then
  begin
    raise Exception.Create('TPHXBitmap.getScanLine: Index out of bounds.');
  end;
  pPixel:= @FPixels^[0];
  Inc(pPixel, Row * Width * GetPixelFormatSize(FFormat));
  Result:= pPixel;
end;



//------------------------------------------------------------------------------
procedure TPHXBitmap.Convert(NewFormat: TPHXPixelFormat);
var PixelGet : TGetPixel;
var PixelSet   : TSetPixel;
var Source     : pByte;
var Dest       : pByte;
var Color      : TPHXPixel;
var X,Y        : Integer;
var Image      : pByteArray;
var ImageSize  : Integer;
begin
  if(FFormat = NewFormat) then Exit;

  Image     := nil;
  ImageSize := Width * Height * GetPixelFormatSize(NewFormat);

  PixelGet:= GetPixelFormatGetter(FFormat  );
  PixelSet:= GetPixelFormatSetter(NewFormat);

  ReallocMem(Image, ImageSize);

  Source:= @FPixels^[0];
  Dest  := @Image^[0];
  for Y:=1 to Height do
  begin
    for X:=1 to Width do
    begin
      PixelGet(Source, Color);
      PixelSet(Dest, Color);
    end;
  end;
  ReallocMem(FPixels, 0);

  FPixels  := Image;
  FSize    := ImageSize;
  FFormat  := NewFormat;
  FPixelGet:= GetPixelFormatGetter(FFormat);
  FPixelSet:= GetPixelFormatSetter(FFormat);
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.LoadAlphaFromBitmap(Bitmap: TPHXBitmap);
var Source1 : pByte;
var Source2 : pByte;
var Dest1   : pByte;
var X,Y     : Integer;
var Color1  : TPHXPixel;
var Color2  : TPHXPixel;
begin
  // Make shure we have a alpha channel
  if (Format <> pfRGBA) then Convert(pfRGBA);

  Source1:= @Self  .FPixels^[0];
  Source2:= @Bitmap.FPixels^[0];
  Dest1  := @Self  .FPixels^[0];
  for Y:=1 to Height do
  begin
    for X:=1 to Width do
    begin
      Self  .FPixelGet(Source1, Color1);
      Bitmap.FPixelGet(Source2, Color2);
      Color1.Alpha:= (Color2.Red + Color2.Green + Color2.Blue) div 3;
      FPixelSet(Dest1, Color1);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.BuildAlphaMatch(const Red, Green, Blue: Byte);
var Source : pByte;
var Dest   : pByte;
var X,Y    : Integer;

var Color : TPHXPixel;
begin
  // Make shure we have a alpha channel
  if (Format <> pfRGBA) then Convert(pfRGBA);

  Source:= @FPixels^[0];
  Dest  := @FPixels^[0];
  for Y:=1 to Height do
  begin
    for X:=1 to Width do
    begin
      FPixelGet(Source, Color);

      if( Color.Red = Red) and (Color.Green = Green) and (Color.Blue = Blue) then
        Color.Alpha:=0
      else
        Color.Alpha:=255;

      FPixelSet(Dest, Color);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.Fill(const Red: Byte = 0; Green: Byte = 0; Blue: Byte = 0; Alpha: Byte = 0);
var Color : TPHXPixel;
var Dest   : pByte;
var X,Y    : Integer;
begin
  Color.Red  :=Red;
  Color.Green:=Green;
  Color.Blue :=Blue;
  Color.Alpha:=Alpha;

  Dest:= @FPixels^[0];
  for Y:=1 to Height do
  begin
    for X:=1 to Width do
    begin
      FPixelSet(Dest, Color);
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.CopyFrom(const Bitmap: TPHXBitmap; const Source: TRecti; const Dest: TVector2i);
var SX, SY: Integer;
var DX, DY: Integer;
var Color : TPHXPixel;
begin
  // Make shure we have a alpha channel
  //if (Format <> pf32Bit) then Convert(pf32Bit);

  // TODO: Raw copy instead of get and set pixel

  SY:= Source.Top;
  DY:= Dest.Y;
  while SY <= Source.Bottom do
  begin
    SX:= Source.Left;
    DX:= Dest.X;
    while SX <= Source.Right do
    begin
      Color:= Bitmap.GetPixel(SX, SY);

      SetPixel(DX,DY, Color );

      Inc(SX);
      Inc(DX);
    end;
    Inc(SY);
    Inc(DY);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.CopyFrom(const Graphic: TPHXGraphic; const Source: TRecti; const Dest: TVector2i );
var SX, SY: Integer;
var DX, DY: Integer;
var Color : TPHXPixel;

var AGetPixel: TGetPixel;
var pPixel: pByte;
begin
  AGetPixel:= GetPixelFormatGetter(Graphic.Format);

  SY:= Source.Top;
  DY:= Dest.Y;
  while SY < Source.Bottom do
  begin
    SX:= Source.Left;
    DX:= Dest.X;
    while SX < Source.Right do
    begin
      pPixel:= @Graphic.Pixels^[0];

      Inc(pPixel, (SX + SY * Graphic.Width) * GetPixelFormatSize(Graphic.Format) );

      AGetPixel(pPixel, Color);

      SetPixel(DX,DY, Color );

      Inc(SX);
      Inc(DX);
    end;
    Inc(SY);
    Inc(DY);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmap.CopyTo(const Graphic: TPHXGraphic; const Source: TRecti; const Dest: TVector2i);
var SX, SY: Integer;
var DX, DY: Integer;
var Color : TPHXPixel;

var ASetPixel: TSetPixel;
var pPixel: pByte;
begin
  ASetPixel:= GetPixelFormatSetter(Graphic.Format);
  // Make shure to not copy outside the destination or source
//  if Source.Right >= Width then Source.Right:= Width-1;
//  if Source.Bottom >= Width then Source.Right:= Width-1;



  SY:= Source.Top;
  DY:= Dest.Y;
  while SY < Source.Bottom do
  begin
    SX:= Source.Left;
    DX:= Dest.X;
    while SX < Source.Right do
    begin
      pPixel:= @Graphic.Pixels^[0];

      Inc(pPixel, (DX + DY * Graphic.Width) * GetPixelFormatSize(Graphic.Format) );

      Color:= GetPixel(SX,SY);

      ASetPixel(pPixel, Color);

      Inc(SX);
      Inc(DX);
    end;
    Inc(SY);
    Inc(DY);
  end;
end;

//------------------------------------------------------------------------------
function TPHXBitmap.GetGraphic: TPHXGraphic;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.Format     := Format;
  Result.Size       := Size;
  Result.Pixels     := Pixels;
end;

{$ENDREGION}




initialization
  _GraphicFormats:= TPHXGraphicFormats.Create;
finalization
  _GraphicFormats.Free;
end.

