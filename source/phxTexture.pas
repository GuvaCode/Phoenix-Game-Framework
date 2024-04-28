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
unit phxTexture;
//< Texture classes

interface

{$I phxConfig.inc}

uses
  SysUtils, Types, Classes,

  phxLogger,
  phxTypes,
  phxClasses,
  phxGraphics;

const

// Default texture extension for textures
PHXTEXTURE_EXT     = '.phxtex';
// The current texture version
PHXTEXTURE_VERSION = 2;

type

TPHXTexture = class;

TPHXTextureEvent = procedure(Texture: TPHXTexture) of object;

// The texture file header.
//------------------------------------------------------------------------------
TPHXTextureHeader = record
  // The id of the texture file, should always be PHXTEX.
  Ident  : array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// Possible values for the texure minifying and magnification functions
//------------------------------------------------------------------------------
TPHXTextureFilter = (
  // GL_NEAREST
  tfNearest,
  // GL_LINEAR
  tfLinear,
  // GL_NEAREST_MIPMAP_NEAREST
  tfNearestMipmapNearest,
  // GL_LINEAR_MIPMAP_NEAREST
  tfLinearMipmapNearest,
  // GL_NEAREST_MIPMAP_LINEAR
  tfNearestMipmapLinear,
  // GL_LINEAR_MIPMAP_LINEAR
  tfLinearMipmapLinear
);

// The texture minifying function is used whenever the pixel being textured maps
// to an area greater than one texture element.
TPHXTextureFilterMin = tfNearest .. tfLinearMipmapLinear;
// The texture magnification function is used when the pixel being textured maps
// to an area less than or equal to one texture element
TPHXTextureFilterMag = tfNearest .. tfLinear;

// Specifies the type of the texture wrapping to use.
//------------------------------------------------------------------------------
TPHXTextureWrap = (
  // Repeat the textures
  twRepeat,
  // Clamp the texture coordinates between 0.0 and 1.0
  twClamp
);

{$REGION 'TPHXTextureSettings'}

// Settings for generating a texture
//------------------------------------------------------------------------------
TPHXTextureSettings = class
  private
    // Use mipmaps for the texture
    FMipmaps  : Boolean;
    // Wrap S
    FWrapS    : TPHXTextureWrap;
    // Wrap T
    FWrapT    : TPHXTextureWrap;
    // Texure minifying function
    FFilterMin: TPHXTextureFilterMin;
    // magnification minifying function
    FFilterMag: TPHXTextureFilterMag;
  public
    // Creates a new instance of the texture settings
    constructor Create;

    // Load the default texture settings
    procedure LoadDefaults;

    // Load the texture settings from a file
    procedure LoadFromFile(const FileName: String);
    // Load the texture settings from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the texture settings to a file
    procedure SaveToFile(const FileName: String);
    // Save the texture settings to a stream
    procedure SaveToStream(Stream: TStream);

    // Generate mipmaps for the texture
    property Mipmaps: Boolean read FMipmaps write FMipmaps;
    // Texture wrap
    property WrapS: TPHXTextureWrap read FWrapS write FWrapS;
    // Texture wrap
    property WrapT: TPHXTextureWrap read FWrapT write FWrapT;
    // Minifying function
    property FilterMin: TPHXTextureFilterMin read FFilterMin write FFilterMin;
    // Magnification function
    property FilterMag: TPHXTextureFilterMag read FFilterMag write FFilterMag;
  end;

{$ENDREGION}

{$REGION 'TPHXTexture'}

// Container for a 2D bitmap texture
//------------------------------------------------------------------------------
TPHXTexture = class(TObject)
  private
	  // Name of the texture
    FName: String;
    // Width
    FWidth : Integer;
    // Height
    FHeight: Integer;
    // The pixel format
    FFormat: TPHXPixelFormat;
    // Texture settings
    FSettings: TPHXTextureSettings;
    // Size in bytes of the texture pixels
    FSize: Integer;
    // The texture pixels
    FPixels: PByteArray;

    procedure Notification(Notification: TPHXNotification);

    function GetGraphic: TPHXGraphic;
    function GetEmpty: Boolean;
    function GetPixel(const X, Y: Integer): TPHXPixel;

    procedure SetWidth(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
		procedure SetFormat(const AValue: TPHXPixelFormat);
    procedure SetPixel(const X, Y: Integer; const Value: TPHXPixel);
  public
		// Creates a new, empty texture
		constructor Create; virtual;
    // Destroy the texture
		destructor Destroy; override;

    // Clear the texture
    procedure Clear;
    // Upload the texture to the video memory
    procedure Build; virtual;
		// Unload the texture from the video memory
    procedure Unload; virtual;

		// Load a texture from a file using a registered texture format
		procedure LoadTexture(const FileName: String); overload;
		// Load a texture from a stream using a registered texture format
    procedure LoadTexture(const FileName: String; Stream: TStream); overload;

    // Load a texture from a file using a registered texture format
    procedure SaveTexture(const FileName: String); overload;
    // Load a texture from a stream using a registered texture format
    procedure SaveTexture(const FileName: String; Stream: TStream); overload;

    // Load the texture from a phoenix texture file
    procedure LoadFromFile(const FileName: String);
    // Load the texture from a phoenix texture stream
    procedure LoadFromStream(Stream: TStream);

    // Save the texture to a phoenix texture file
    procedure SaveToFile(const FileName: String);
    // Saves the texture to a phoenix texture stream
    procedure SaveToStream(const Stream: TStream);

    // Resizes the texture buffer, does not affect the texture in video memory
    procedure Resize(const AWidth, AHeight: Integer; const AFormat: TPHXPixelFormat);

    // Import the pixel data
    procedure Import(const AWidth, AHeight: Integer; const  AFormat: TPHXPixelFormat; Data: Pointer); overload;
    // Import the pixel data from a graphic
    procedure Import(const Graphic: TPHXGraphic ); overload;
    // Import the pixel data from another texture
    procedure Import(const Texture: TPHXTexture ); overload;

    // Provides indexed access to each line of pixels.
    function ScanLine(Line: Integer): PByte;

    // Name of the texture
    property Name: String read FName write FName;
    // Width of the texture
    property Width: Integer read FWidth write SetWidth;
    // Height of the texture
    property Height: Integer read FHeight write SetHeight;
    // Format of each pixel in the texture
    property Format: TPHXPixelFormat read FFormat write SetFormat;
    // The texture settings
    property Settings: TPHXTextureSettings read FSettings;
    // Return theize of the texture in bytes
    property Size: Integer read FSize;
		// Return a pointer to the internal texture data
    property Pixels: PByteArray read FPixels;
    // Return the grapic struct for this texture
    property Graphic: TPHXGraphic read GetGraphic;
    // Returns if the texture is empty (width or height is zero)
    property Empty: Boolean read GetEmpty;
    // Get the grapic object for this texture
    property Pixel[const X, Y: Integer]: TPHXPixel read GetPixel write SetPixel;
  end;

{$ENDREGION}

{$REGION 'TPHXTextureList'}

PTextureList = ^TTextureList;
TTextureList = array[0..$00FFFFFF] of TPHXTexture;

// Container for a list of textures
//------------------------------------------------------------------------------
TPHXTextureList = class
  private
    FDevice: TPHXCustomDevice;
    FList  : TList;

    function GetCount: Integer;
    function GetList: PTextureList;
    function GetItem(const Index: Integer): TPHXTexture;
    function GetTexture(const Name: String): TPHXTexture;
  public
    // Creates a new texture list
    constructor Create(ADevice: TPHXCustomDevice);
    // Default destructor
    destructor Destroy; override;

    // Clear and free all textures
    procedure Clear;

    // Add a texture to the list
    function Add: TPHXTexture;
    // Add and load a texture to the list
		function LoadTexture(const FileName: String): TPHXTexture; overload;
    // Add and load a texture to the list
		function LoadTexture(const FileName: String; const Name: String): TPHXTexture; overload;
    // Add and load a texture to the list
    function LoadTexture(const FileName: String; Stream: TStream): TPHXTexture; overload;

    // Search for a texture by name and return the index
    function IndexOf(const Name: String): Integer;
    // Search for a texture by name and return the texture
    function Find(const Name: String): TPHXTexture;

    // The owning device
    property Device: TPHXCustomDevice read FDevice write FDevice;
    // Returns the number of textures in the list
    property Count: Integer read GetCount;
    // Returns a pointer to the internal list
    property List: PTextureList read GetList;
    // Return a texture from the list
    property Textures[const Name: String]: TPHXTexture read GetTexture;
    // Return a texture from the list
    property Items[const Index: Integer]: TPHXTexture read GetItem; default;
  end;

{$ENDREGION}

implementation

uses phxDevice;

{$REGION 'TPHXTextureSettings'}

// TPHXTextureSettings
//==============================================================================
constructor TPHXTextureSettings.Create;
begin
  // Load the default texture settings
  FMipmaps  := False;
  FFilterMin:= tfLinearMipmapLinear;
  FFilterMag:= tfLinear;
  FWrapS    := twRepeat;
  FWrapT    := twRepeat;
end;

//------------------------------------------------------------------------------
procedure TPHXTextureSettings.LoadDefaults;
begin
  FMipmaps  := True;
  FFilterMin:= tfLinearMipmapLinear;
  FFilterMag:= tfLinear;
  FWrapS    := twClamp;
  FWrapT    := twClamp;
end;

//------------------------------------------------------------------------------
procedure TPHXTextureSettings.LoadFromFile(const FileName: String);
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
procedure TPHXTextureSettings.SaveToFile(const FileName: String);
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
procedure TPHXTextureSettings.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FMipmaps  , SizeOf(FMipmaps));
  Stream.Read(FWrapS    , SizeOf(FWrapS));
  Stream.Read(FWrapT    , SizeOf(FWrapT));
  Stream.Read(FFilterMin, SizeOf(FFilterMin));
  Stream.Read(FFilterMag, SizeOf(FFilterMag));
end;

//------------------------------------------------------------------------------
procedure TPHXTextureSettings.SaveToStream(Stream: TStream);
begin
  Stream.Write(FMipmaps  , SizeOf(FMipmaps));
  Stream.Write(FWrapS    , SizeOf(FWrapS));
  Stream.Write(FWrapT    , SizeOf(FWrapT));
  Stream.Write(FFilterMin, SizeOf(FFilterMin));
  Stream.Write(FFilterMag, SizeOf(FFilterMag));
end;

{$ENDREGION}



{$REGION 'TPHXTexture'}

// TPHXTexture
//==============================================================================
constructor TPHXTexture.Create;
begin
  TPHXNotifications.Add(Notification);

  FName    := '';
  FWidth   := 0;
  FHeight  := 0;
  FSettings:= TPHXTextureSettings.Create;
  FWidth   := 0;
  FHeight  := 0;
  FFormat  := pfNone;
  FSize    := 0;
  FPixels  := nil;

  TPHXNotifications.Add(Notification);
end;

//------------------------------------------------------------------------------
destructor TPHXTexture.Destroy;
begin
  TPHXNotifications.Remove(Notification);

  Unload;

  // Free pixel data
  Resize(0,0, pfNone);

  FSettings.Free;

  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Notification(Notification: TPHXNotification);
begin
  if Notification = dnContextCreated then
  begin
    Build;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXTexture.Clear;
begin
  Resize(0,0, pfNone);
end;


//------------------------------------------------------------------------------
procedure TPHXTexture.Build;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Unload;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.LoadTexture(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmOpenRead or fmShareDenyNone);
  try
    LoadTexture(FileName, Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.LoadTexture(const FileName: String; Stream: TStream);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  FName:= ExtractFileName(FileName);

  if GraphicFormats.Find(Filename, Format) then
  begin
    Format.Filer.LoadFromStream(Stream, FileName  , Graphic);

    Resize(0, 0, pfNone);

    // Copy the information from the loaded graphic
    FWidth := Graphic.Width;
    FHeight:= Graphic.Height;
    FFormat:= Graphic.Format;
    FSize  := Graphic.Size;
    FPixels:= Graphic.Pixels;

    Build;
  end else
  begin
    // LogError();
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SaveTexture(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckTexture, fmCreate);
  try
    SaveTexture(FileName, Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SaveTexture(const FileName: String; Stream: TStream);
var Graphic: TPHXGraphic;
var Format : TPHXGraphicFormat;
begin
  FName:= ExtractFileName(FileName);

  if GraphicFormats.Find(Filename, Format) then
  begin
    // Copy the information from the loaded graphic
    Graphic.Width := FHeight;
    Graphic.Height:= FHeight;
    Graphic.Format:= FFormat;
    Graphic.Size  := FSize;
    Graphic.Pixels:= FPixels;

    Format.Filer.SaveToStream(Stream, FileName , Graphic);
  end else
  begin
    raise Exception.CreateFmt('Could not find a graphic importer for %s', [Filename])
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:=  TPHXContentManager.CreateStream(FileName, ckTexture, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:=  TPHXContentManager.CreateStream(FileName, ckTexture, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SaveToStream(const Stream: TStream);
var Header : TPHXTextureHeader;
begin
  Header.Ident  := 'PHXTEX';
  Header.Version:= PHXTEXTURE_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Stream.Write(FWidth      , SizeOf(Integer));
  Stream.Write(FHeight     , SizeOf(Integer));
  Stream.Write(FFormat     , SizeOf(TPHXPixelFormat));
  Stream.Write(FSize       , SizeOf(Integer));

  SavePixels(Stream, FPixels, FSize);
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.LoadFromStream(Stream: TStream);
var Header : TPHXTextureHeader;
begin
  Header.Ident := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));
  if (Header.Ident <> 'PHXTEX') then
  begin
    TPHXLogger.Error('TPHXTexture.LoadFromStream', 'Not a valid Phoenix texture.');

    raise Exception.Create('Not a valid Phoenix texture.');
  end;

  if (Header.Version <> PHXTEXTURE_VERSION) then
  begin
    TPHXLogger.Error('TPHXTexture.LoadFromStream', 'Texture version missmatch [File: %d Code: %d].', [Header.Version, PHXTEXTURE_VERSION]);

    raise Exception.CreateFmt('Texture version missmatch [File: %d Code: %d].', [Header.Version, PHXTEXTURE_VERSION]);
  end;

  Stream.Read(FWidth      , SizeOf(Integer));
  Stream.Read(FHeight     , SizeOf(Integer));
  Stream.Read(FFormat     , SizeOf(TPHXPixelFormat));
  Stream.Read(FSize       , SizeOf(Integer));

  ReAllocMem(FPixels, FSize);

  LoadPixels(Stream, FPixels, FSize);

  Build;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Resize(const AWidth, AHeight: Integer; const AFormat: TPHXPixelFormat);
var ASize: Integer;
begin
  // Calculate the size of the texture
  ASize:= AWidth * AHeight * GetPixelFormatSize(AFormat);

  // Resize the pixel block if the size is changed
  if (FSize <> ASize) then
  begin
    FSize:= ASize;

    ReAllocMem(FPixels, ASize);
  end;

  FWidth  := AWidth;
  FHeight := AHeight;
  FFormat := AFormat;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Import(const AWidth, AHeight: Integer; const AFormat: TPHXPixelFormat; Data: Pointer);
begin
  Resize(AWidth, AHeight, AFormat);

  Move(Data^, FPixels^, FSize);

  Build;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Import(const Graphic: TPHXGraphic);
begin
  Resize(Graphic.Width, Graphic.Height, Graphic.Format);

  Move(Graphic.Pixels^, FPixels^, FSize);

  Build;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.Import(const Texture: TPHXTexture);
begin
  Resize(Texture.Width, Texture.Height, Texture.Format);

  Move(Texture.Pixels^, FPixels^, FSize);

  Build;
end;

//------------------------------------------------------------------------------
function TPHXTexture.ScanLine(Line: Integer): PByte;
var PixelSize: Integer;
begin
  PixelSize:= GetPixelFormatSize(FFormat);

  Result:= @Pixels^[Line * FWidth * PixelSize];
end;

//------------------------------------------------------------------------------
function TPHXTexture.GetEmpty: Boolean;
begin
  Result:= (Width = 0) or (Height = 0);
end;

//------------------------------------------------------------------------------
function TPHXTexture.GetGraphic: TPHXGraphic;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.Format     := Format;
  Result.Size       := Size;
  Result.Pixels     := Pixels;
end;

//------------------------------------------------------------------------------
function TPHXTexture.GetPixel(const X, Y: Integer): TPHXPixel;
var Offset: Integer;
var Data  : PByte;
var Getter: TGetPixel;
begin
  Offset:= (Y * FWidth + X) * GetPixelFormatSize(FFormat);
  Data  := @Pixels^[Offset];
  Getter:= GetPixelFormatGetter(FFormat);

  Getter(Data, Result);
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SetPixel(const X, Y: Integer; const Value: TPHXPixel);
var Offset: Integer;
var Data  : PByte;
var Setter: TSetPixel;
begin
  Offset:= (Y * FWidth + X) * GetPixelFormatSize(FFormat);
  Data  := @Pixels^[Offset];
  Setter:= GetPixelFormatSetter(FFormat);

  Setter(Data, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SetWidth(const AValue: Integer);
begin
  if Width <> AValue then
  begin
    FWidth:= AValue;

    Resize(FWidth, FHeight, FFormat);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SetHeight(const AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight:= AValue;

    Resize(FWidth, FHeight, FFormat);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTexture.SetFormat(const AValue: TPHXPixelFormat);
begin
  if Format <> AValue then
  begin
    FFormat:= AValue;

    Resize(FWidth, FHeight, FFormat);
  end;
end;

{$ENDREGION}

{$REGION 'TPHXTextureList'}

// TPHXTextureList
//==============================================================================
constructor TPHXTextureList.Create(ADevice: TPHXCustomDevice);
begin
  FDevice:= ADevice;
  FList  := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXTextureList.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TPHXTextureList.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    TPHXTexture(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXTextureList.Add: TPHXTexture;
begin
  Result:= TPHXDevice(Device).CreateTexture;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.LoadTexture(const FileName: String): TPHXTexture;
begin
  Result:= TPHXDevice(Device).CreateTexture;
  Result.LoadTexture(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.LoadTexture(const FileName: String; const Name: String): TPHXTexture;
begin
  Result:= TPHXDevice(Device).CreateTexture;
  Result.LoadTexture(FileName);
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.LoadTexture(const FileName: String; Stream: TStream): TPHXTexture;
begin
  Result:= TPHXDevice(Device).CreateTexture;
  Result.LoadTexture(FileName, Stream);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.IndexOf(const Name: String): Integer;
var Index: Integer;
var Texture: TPHXTexture;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Texture:= TPHXTexture(FList.List[Index]);

    if SameText(Texture.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXTextureList.Find(const Name: String): TPHXTexture;
var Index: Integer;
var Texture: TPHXTexture;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Texture:= TPHXTexture(FList.List[Index]);

    if SameText(Texture.Name, Name) then
    begin
      Result:= Texture;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXTextureList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXTextureList.GetList: PTextureList;
begin
  Result:= PTextureList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.GetTexture(const Name: String): TPHXTexture;
begin
  Result:= Find(Name);
end;

//------------------------------------------------------------------------------
function TPHXTextureList.GetItem(const Index: Integer): TPHXTexture;
begin
  Result:= TPHXTexture(FList.List[Index]);
end;


{$ENDREGION}





end.
