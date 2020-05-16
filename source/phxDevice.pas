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
unit phxDevice;
//< Contains API independent rendering functions

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  {$IFDEF WIN32}
  Windows, // For loadlibrary
  {$ENDIF}

  phxLogger,
  phxTypes,
  phxClasses,
  phxMath,
  phxEvents,
  phxGraphics,

  phxCanvas,
  phxEffect,
  phxTexture;

const

// Default background color of the device
DefaultBackground : TColor4f = (Red: 0.109803922474384; Green: 0.141176477074623; Blue: 0.176470592617989; Alpha: 1);

// Default minumum depth of the depth buffer
ViewportMinDepth = -0.1;
// Default maximum depth of the depth buffer
ViewportMaxDepth = 1000.0;

type

IPHXDevice = interface;
TPHXProvider = class;
TPHXDevice   = class;

{$REGION 'Rendering Types'}

// Clear targets for TPHXDevice.Clear
//------------------------------------------------------------------------------
TPHXClearTarget = set of (
  // Indicates the buffers currently enabled for color writing.
  PHX_COLOR_BUFFER,
  // Indicates the depth buffer.
  PHX_DEPTH_BUFFER,
  // Indicates the accumulation buffer.
  PHX_ACCUM_BUFFER,
  // Indicates the stencil buffer.
  PHX_STENCIL_BUFFER
);

// Texture samplers (units)
//------------------------------------------------------------------------------
TPHXTextureSampler = (
  PHX_TEXTURE_1 = 0,
  PHX_TEXTURE_2 = 1,
  PHX_TEXTURE_3 = 2,
  PHX_TEXTURE_4 = 3,
  PHX_TEXTURE_5 = 4,
  PHX_TEXTURE_6 = 5,
  PHX_TEXTURE_7 = 6,
  PHX_TEXTURE_8 = 7
);

// Toggles wireframe
//------------------------------------------------------------------------------
TPHXWireframe = (
  // Drawed filled triangles
  wfFill,
  // Draw lines
  wfLines
);

{$ENDREGION}

{$REGION 'TPHXRenderTarget'}

// A render target for render to texture
//------------------------------------------------------------------------------
TPHXRenderTarget = class
  private
    FName: String;
    FWidth: Integer;
    FHeight: Integer;
  protected
    function GetTexture: TPHXTexture; virtual; abstract;
  public
    // Default constructor
    constructor Create;

    // Change the size of the render target
    procedure Resize(const AWidth, AHeight: Integer); virtual;

    // Clear the contents of the render target
    procedure Clear; virtual; abstract;

    // Save the render target to a bitmap
    procedure SaveToBitmap(Bitmap: TPHXBitmap);  virtual; abstract;

    // Name of the render target
    property Name: String read FName write FName;
    // Width of the render target
    property Width: Integer read FWidth;
    // Height of the render target
    property Height: Integer read FHeight;
    // The texture of the render target
    property Texture: TPHXTexture read GetTexture;
 end;

{$ENDREGION}

{$REGION 'TPHXProvider'}

// Provider class
TPHXProviderClass = class of TPHXProvider;

// The graphics API the provider is using
//------------------------------------------------------------------------------
TPHXProviderTarget = (
  // Unknown renderer api
  ptUnknown,
  // OpenGL targeting the 3.x profile
  ptOpenGL_3,
  // OpenGL ES
  ptOpenGL_ES,
  // Direct 3D 9.0c
  ptDirect3D9,
  // Direct 3D 10
  ptDirect3D10,
  // Direct 3D 11
  ptDirect3D11
);

// A provider is responsible for creating a device for a given graphics API
//------------------------------------------------------------------------------
TPHXProvider = class
  protected
    // Return the name of the provider
    function GetName: String; virtual;
    // The graphics API the provider is using
    function GetTarget: TPHXProviderTarget; virtual;
  public
    // Virtual constructor for the provider
    constructor Create; virtual;

    // Create the renderer for this provider
    function CreateRenderer: IPHXDevice; virtual; abstract;

    // The name of the provider
    property Name: String read GetName;
    // The name of the provider
    property Target: TPHXProviderTarget read GetTarget;
  end;

// Provider that loads the renderer from a library (DLL)
//------------------------------------------------------------------------------
TPHXProviderLibrary = class(TPHXProvider)
  private
    FLibraryName  : String;
    FLibraryHandle: THandle;
  protected
    function GetName: string; override;
  public
    constructor Create(const LibraryName: String); reintroduce;
    destructor Destroy; override;

    // Create the renderer for this provider
    function CreateRenderer: IPHXDevice; override;
  end;

// Provider that loads the renderer from a object
//------------------------------------------------------------------------------
TPHXProviderObject = class(TPHXProvider)
  private
    FInstance: TObject;
  public
    constructor Create(const Instance: TObject); reintroduce;
    destructor Destroy; override;

    // Create the renderer for this provider
    function CreateRenderer: IPHXDevice; override;
  end;

{$ENDREGION}

{$REGION 'IPHXDevice'}

// Flags for the window creation
//------------------------------------------------------------------------------
TPHXWindowFlags = set of (
  // Allow resizing of the window
  wfResizable,
  // Activate vertical syncronisation
  wfVerticalSync,
  // Show the cursor
  wfCursor
);


// Parameters that are sent to the initialize function of the device
//------------------------------------------------------------------------------
TPHXDeviceParameters = record
  // Title of the window
  Title: String;
  // Width of the window
  Width: Integer;
  // Height of the window
  Height: Integer;
  // Show in fullscreen
  Fullscreen: Boolean;
  // Event handler
  Events: TPHXEventInstance;
end;

// @exclude
// The renderer interface provides hardware independent rendering
//------------------------------------------------------------------------------

// The renderer interface provides hardware independent rendering
//------------------------------------------------------------------------------
IPHXDevice = interface
  ['{B55AC1D9-458E-4093-BB5C-6643DCE8F027}']

  {$REGION 'Private getters'}
  // Get the name of the device
  function GetName: String;
  // Get the version of the device
  function GetVersion: String;
  // Return the provider target of this renderer
  function GetTarget: TPHXProviderTarget;
  // Get the current window size
  function GetWidth: Integer;
  // Get the current window size
  function GetHeight: Integer;
  // Get the window flags
  function GetFlags: TPHXWindowFlags;
  {$ENDREGION}

  {$REGION 'Private setters'}
  // Set the window title
  procedure SetTitle(const Title: String);
  // Set the window flags
  procedure SetFlags(const Flags: TPHXWindowFlags);
  // Set the icon for the window
  procedure SetIcon(const Icon: String);
  {$ENDREGION}

  // Enumerate all supported display modes
  procedure EnumDisplayModes(const Modes: TPHXDisplayModes);

  // Initialize the renderer
  procedure Initialize(const Parameters: TPHXDeviceParameters);
  // Reinitializes the renderer using a new display mode
  procedure Reinitialize(const Parameters: TPHXDeviceParameters);
  // Finalize the renderer
  procedure Finalize;

  // Update the renderer
  procedure Update;
  // Clear the back buffers
  procedure Clear;
  // Flip the front and back buffers
  procedure Flip;

  // Creates a new render target
  function CreateRenderTarget: TPHXRenderTarget;
  // Creates a new canvas
  function CreateCanvas: TPHXCanvas;
  // Creates a new effect
  function CreateEffect: TPHXEffect;
  // Creates a new render buffer
  function CreateBuffer: TPHXBuffer;
  // Creates a new texture
  function CreateTexture: TPHXTexture;

  /// SetViewport can be used to draw on part of the screen
  procedure SetViewport(const Viewport: TViewport);
  // Sets the background color
  procedure SetClearColor(const Color: TColor4f);
  // Change the blend mode
  procedure SetBlending(const Value: TPHXBlendMode);
  // Toggles wireframe
  procedure SetWireframe(const Value: Boolean);
  // Enable or disable writing into the depth buffer
  procedure SetDepthMask(const Value: Boolean);
  // Enable or disable depth testing
  procedure SetDepthTest(const Value: Boolean);
  // Set the current render target
  procedure SetRenderTarget(Target: TPHXRenderTarget);

  // Get the name of the device
  property Name: String read GetName;
  // Get the version of the device
  property Version: String read GetVersion;
  // Return the provider target of this device
  property Target: TPHXProviderTarget read GetTarget;
  // Change the window title
  property Title: String  write SetTitle;
  // The width of the window
  property Width: Integer read GetWidth;
  // The height of the window
  property Height: Integer read GetHeight;
  // The window flags
  property Flags: TPHXWindowFlags read GetFlags write SetFlags;
  // The bitmap file containing the window icon
  property Icon: String write SetIcon;
end;

{$ENDREGION}

{$REGION 'TPHXDisplay'}

// Manager class for display modes
//------------------------------------------------------------------------------
TPHXDisplay = class
  private
    // The owning device
    FDevice: TPHXDevice;
    // List of all supported display modes
    FModes: TPHXDisplayModes;
    // The current display mode
    FMode: TPHXDisplayMode;
    // The previous display mode
    FPrevious: TPHXDisplayMode;

    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetFullscreen(const Value: Boolean);
  public
    // Create a new display
    constructor Create(ADevice: TPHXDevice);
    // Free this display
    destructor Destroy; override;

    // Apply the selected display mode
    procedure Apply;
    // Revert to the previous display mode
    procedure Revert;

    // Select the next higher resolution while still maintaining the aspect ratio
    function SelectHigher: Boolean;
    // Select the next lower resolution while still maintaining the aspect ratio
    function SelectLower: Boolean;

    // The owning device
    property Device: TPHXDevice read FDevice;
    // List of all supported display modes
    property Modes: TPHXDisplayModes read FModes;
    // The selected display mode
    property Mode: TPHXDisplayMode read FMode write FMode;
    // Width of the selected display mode
    property Width: Integer read FMode.Width write SetWidth;
    // Height of the selected display mode
    property Height: Integer read FMode.Height write SetHeight;
    // Toggles if the device should be opened in fullscreen
    property Fullscreen: Boolean read FMode.Fullscreen write SetFullscreen;
  end;


{$ENDREGION}

{$REGION 'TPHXDevice'}

// The device manages a graphics device and its window
//------------------------------------------------------------------------------
TPHXDevice = class(TPHXCustomDevice)
  private
    FProvider: TPHXProvider;
    FRenderer: IPHXDevice;
    FDisplay : TPHXDisplay;

    FTitle: String;
    FIcon : String;
    FFlags : TPHXWindowFlags;

    FWireframe: Boolean;


    procedure ResetRenderStates;

    function GetTarget: TPHXProviderTarget;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetBounds: TRecti;

    procedure SetFlags(AValue: TPHXWindowFlags);
    procedure SetIcon(AValue: String);
    procedure SetTitle(AValue: String);
  public
    // Creates a device from the default provicer
    constructor Create; overload;
    // Creates a device from a registered provider
    constructor Create(const AProvider: String); overload;
    // Creates a device from a custom provider
    constructor Create(const AProvider: TPHXProvider); overload;
    // Creates a device from a custom device
    constructor Create(const ADevice: IPHXDevice); overload;
    // Destroys the device
    destructor Destroy; override;

    // Initialize the device using the selected display mode
    // @seealso(TPHXDisplay)
    procedure Initialize; overload;

    // Initialize the device
    //
    // @param(Title The title of the window)
    // @param(Width Width of the window)
    // @param(Height Height of the window)
    procedure Initialize(const Title: String; const Width, Height: Integer); overload;

    // Initialize the device
    //
    // @param(Title The title of the window)
    // @param(Width Width of the window)
    // @param(Height Height of the window)
    // @param(Fullscreen Toggles if the window should be opened in fullscreen mode)
    procedure Initialize(const Title: String; const Width, Height: Integer; const Fullscreen: Boolean); overload;

    // Initialize the device using the selected display mode
    // @seealso(TPHXDisplay)
    procedure Reinitialize; overload;
    // Reinitializes the device using a new display mode
    procedure Reinitialize(const Width, Height: Integer; const Fullscreen: Boolean); overload;

    // Finalize the device
    procedure Finalize;

    // Create a new render target
    function CreateRenderTarget: TPHXRenderTarget;
    // Creates a new canvas
    function CreateCanvas: TPHXCanvas;
    // Creates a new basic effect
    function CreateEffect: TPHXEffect;
    // Creates a render buffer
    function CreateBuffer: TPHXBuffer;
    // Creates a new texture
    function CreateTexture: TPHXTexture;
    // Create a stencil mask
//    function CreateStencilMask: TPHXStencilMask;

    // Set the current render target
    procedure SetRenderTarget(Target: TPHXRenderTarget);


    /// Change the current graphical viewport
    procedure SetViewport(const Viewport: TViewport); overload;
    /// Change the current graphical viewport
    procedure SetViewport(const X, Y, Width, Height: Integer); overload;
    /// Change the current graphical viewport
    procedure SetViewport(const Width, Height: Integer); overload;

    // Sets the clear color
    procedure SetClearColor(const Color: TColor4f); overload;
    // Sets the background color
    procedure SetClearColor(const Red, Green, Blue: Single); overload;

    // Change the blend mode
    procedure SetBlending(const Value: TPHXBlendMode);
    // Toggles wireframe
    procedure SetWireFrame(const Value: Boolean);
    // Enable or disable writing into the depth buffer
    procedure SetDepthMask(const Value: Boolean);
    // Enable or disable depth testing
    procedure SetDepthTest(const Value: Boolean);

    // Update the renderer
    procedure Update;
    // Clear the back buffers
    procedure Clear;
    // Flip the front and back buffers
    procedure Flip;

    // The provider
    property Provider: TPHXProvider read FProvider;
    // The device interface
    property Renderer: IPHXDevice read FRenderer;
    // Return the provider target of this renderer
    property Target: TPHXProviderTarget read GetTarget;
    // Selected display mode
    property Display: TPHXDisplay read FDisplay;


    // Change the window title
    property Title: String read FTitle write SetTitle;
    // Returns the current width of the device window
    property Width: Integer read GetWidth;
    // Returns the current height of the device window
    property Height: Integer read GetHeight;
    // Read or change the whe window flags
    property Flags: TPHXWindowFlags read FFlags write SetFlags;
    // The bitmap file containing the window icon
    property Icon: String read FIcon write SetIcon;



    // Return the bounding rectangle of the display
    property Bounds: TRecti read GetBounds;

    // Toggles wireframe
    property WireFrame: Boolean read FWireFrame write SetWireFrame;

  end;

{$ENDREGION}


const

// Default window Flag values
// Each implementing renderer should set this values when resetting the device
//------------------------------------------------------------------------------
DefaultWindowFlags = [wfCursor, wfVerticalSync];


// Registers a provider
procedure RegisterProvider(const ProviderName: String; ProviderClass: TPHXProviderClass);
// Enumerate all provides
procedure EnumProviders(const Providers: TStrings);
// Creates a provider
function CreateProvider(const ProviderName: String): TPHXProvider;

implementation
{$IFDEF FPC}
uses dynlibs;
{$ENDIF}


resourcestring
  SRendererNotLoaded = 'The renderer is not loaded.';

  SDefaultTitle = 'Phoenix Game Framework';



{$REGION 'ProviderRegistry'}

// The default provider
var DefaultProvider:  TPHXProviderClass;

//------------------------------------------------------------------------------
var ProviderRegistry: array of record
  ProviderName : String;
  ProviderClass: TPHXProviderClass;
end;

//------------------------------------------------------------------------------
procedure RegisterProvider(const ProviderName: String; ProviderClass: TPHXProviderClass);
var Index: Integer;
begin
  Index:= Length(ProviderRegistry);

  SetLength(ProviderRegistry, Index+1 );

  ProviderRegistry[Index].ProviderName := ProviderName;
  ProviderRegistry[Index].ProviderClass:= ProviderClass;

  // If this is the first registered provider, set this as the default one
  if DefaultProvider = nil then
  begin
    DefaultProvider:= ProviderClass;
  end;
end;

// Enumerate all provides
//------------------------------------------------------------------------------
procedure EnumProviders(const Providers: TStrings);
var Index: Integer;
begin
  for Index := Low(ProviderRegistry) to High(ProviderRegistry) do
  begin
    Providers.Add( ProviderRegistry[Index].ProviderName );
  end;
end;

// Creates a provider from a provider name
//------------------------------------------------------------------------------
function CreateProvider(const ProviderName: String): TPHXProvider;
var Index: Integer;
begin
  for Index := Low(ProviderRegistry) to High(ProviderRegistry) do
  begin
    if SameText(ProviderRegistry[Index].ProviderName, ProviderName) or ProviderRegistry[Index].ProviderClass.ClassNameIs(ProviderName) then
    begin
      Result:= ProviderRegistry[Index].ProviderClass.Create;

      Exit;
    end;
  end;
  Result:= nil;
end;

{$ENDREGION}

{$REGION 'TPHXProvider'}

// TPHXProvider
//==============================================================================
constructor TPHXProvider.Create;
begin

end;

//------------------------------------------------------------------------------
function TPHXProvider.GetName: String;
begin
  Result:= ClassName;
end;

//------------------------------------------------------------------------------
function TPHXProvider.GetTarget: TPHXProviderTarget;
begin
  Result:= ptUnknown;
end;

{$ENDREGION}

{$REGION 'TPHXProviderLibrary'}

// TPHXProviderLibrary
//==============================================================================
constructor TPHXProviderLibrary.Create(const LibraryName: String);
begin
  inherited Create;

  FLibraryName  := ExpandFileName(LibraryName);
  FLibraryHandle:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXProviderLibrary.Destroy;
begin
  if FLibraryHandle <> 0 then
  begin
    FreeLibrary(FLibraryHandle)
  end;

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXProviderLibrary.GetName: string;
begin
  Result:= FLibraryName;
end;

//------------------------------------------------------------------------------
function TPHXProviderLibrary.CreateRenderer: IPHXDevice;
var CreateRenderer: function: IPHXDevice;
begin
  if not FileExists(FLibraryName) then
  begin
    raise Exception.CreateFmt('The renderer "%s" not found', [FLibraryName]);
  end;

  FLibraryHandle:= LoadLibrary(PChar(FLibraryName));

  if FLibraryHandle <> 0 then
  begin
    CreateRenderer:= GetProcAddress(FLibraryHandle, 'CreateRenderer');

    if Assigned(CreateRenderer) then
    begin
      Result:= CreateRenderer();

      if Result = nil then
      begin
        raise Exception.CreateFmt('The library "%s" did not return a valid renderer instance.', [FLibraryName]);
      end;

    end else
    begin
      raise Exception.CreateFmt('The library "%s" doesnt contains the entrypoint "CreateRenderer".', [FLibraryName]);
    end;

  end else
  begin
    RaiseLastOSError;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXProviderObject'}

// TPHXProviderObject
//==============================================================================
constructor TPHXProviderObject.Create(const Instance: TObject);
begin
  inherited Create;

  FInstance:= Instance;
end;

//------------------------------------------------------------------------------
destructor TPHXProviderObject.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXProviderObject.CreateRenderer: IPHXDevice;
var Renderer: IPHXDevice;
begin
  if Supports(FInstance, IPHXDevice, Renderer) then
  begin
    Result:= Renderer;
  end else
  begin
    raise Exception.Create('The instance doesnt support the IPHXRenderer interface.');
  end;
end;

{$ENDREGION}

{$REGION 'TPHXRenderTarget'}

// TPHXRenderTarget
//==============================================================================
constructor TPHXRenderTarget.Create;
begin
  FName  := '';
  FWidth := 1024;
  FHeight:= 1024;
end;

//------------------------------------------------------------------------------
procedure TPHXRenderTarget.Resize(const AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight:= AHeight;
end;

{$ENDREGION}



{$REGION 'TPHXDisplay'}

const
  // Default display mode format
  DISPLAY_MODE_FORMAT = '%dx%d';

// TPHXDevice
//==============================================================================
constructor TPHXDisplay.Create(ADevice: TPHXDevice);
begin
  FDevice:= ADevice;
  FModes := TPHXDisplayModes.Create;

  FMode.Description:= ShortString(Format(DISPLAY_MODE_FORMAT, [800, 600]));
  FMode.Width       := 800;
  FMode.Height      := 600;
  FMode.Fullscreen  := False;

  FPrevious:= FMode;
end;

//------------------------------------------------------------------------------
destructor TPHXDisplay.Destroy;
begin
  FModes.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplay.Apply;
begin
  FPrevious:= FMode;

  Device.Reinitialize(FMode.Width, FMode.Height, FMode.Fullscreen);
end;

//------------------------------------------------------------------------------
procedure TPHXDisplay.Revert;
begin
  FMode:= FPrevious;

  Device.Reinitialize(FMode.Width, FMode.Height, FMode.Fullscreen);
end;

//------------------------------------------------------------------------------
function TPHXDisplay.SelectHigher: Boolean;
var Start     : Integer;
var Index     : Integer;
var Mode      : TPHXDisplayMode;
var AFac, BFac: Integer;
var ANum, BNum: Integer;
var ADen, BDen: Integer;
begin
  AFac:= GreatestCommonDivisior(FMode.Width, FMode.Height);
  ANum:= FMode.Width  div AFac;
  ADen:= FMode.Height div AFac;

  Start:= Modes.IndexOf(FMode.Width, FMode.Height);

  if Start >= 0 then
  begin
    for Index:= Start+1 to Modes.Count - 1 do
    begin
      Mode:= Modes.List^[Index];

      BFac:= GreatestCommonDivisior(Mode.Width, Mode.Height);
      BNum:= Mode.Width  div BFac;
      BDen:= Mode.Height div BFac;

      if (ANum = BNum) and (ADen = BDen) then
      begin
        FMode:= Mode;

        Result:= True;
        Exit;
      end;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXDisplay.SelectLower: Boolean;
var Start     : Integer;
var Index     : Integer;
var Mode      : TPHXDisplayMode;
var AFac, BFac: Integer;
var ANum, BNum: Integer;
var ADen, BDen: Integer;
begin
  AFac:= GreatestCommonDivisior(FMode.Width, FMode.Height);
  ANum:= FMode.Width  div AFac;
  ADen:= FMode.Height div AFac;

  Start:= Modes.IndexOf(FMode.Width, FMode.Height);

  if Start >= 0 then
  begin
    for Index:=Start-1 downto 0 do
    begin
      Mode:= Modes.List^[Index];

      BFac:= GreatestCommonDivisior(Mode.Width, Mode.Height);
      BNum:= Mode.Width  div BFac;
      BDen:= Mode.Height div BFac;

      if (ANum = BNum) and (ADen = BDen) then
      begin
        FMode:= Mode;

        Result:= True;
        Exit;
      end;
    end;
  end;

  Result:= False;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplay.SetWidth(const Value: Integer);
begin
  if FMode.Width <> Value then
  begin
    FMode.Width := Value;
    FMode.Description:= ShortString(Format(DISPLAY_MODE_FORMAT, [FMode.Width, FMode.Height]));
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXDisplay.SetHeight(const Value: Integer);
begin
  if FMode.Height <> Value then
  begin
    FMode.Height := Value;
    FMode.Description:= ShortString(Format(DISPLAY_MODE_FORMAT, [FMode.Width, FMode.Height]));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDisplay.SetFullscreen(const Value: Boolean);
begin
  FMode.Fullscreen := Value;
end;



{$ENDREGION}

{$REGION 'TPHXDevice'}

// TPHXDevice
//==============================================================================
constructor TPHXDevice.Create;
begin
  // Check so a default provider is avaiable
  if DefaultProvider = nil then
  begin
    raise Exception.Create('No default provider is avaiable.');
  end;

  Create(DefaultProvider.Create);
end;

//------------------------------------------------------------------------------
constructor TPHXDevice.Create(const AProvider: String);
begin
  FProvider:= CreateProvider(AProvider);

  // Check so the provider is valid
  if FProvider = nil then
  begin
    raise Exception.CreateFmt('The provider "%" was not found.', [AProvider]);
  end;

  Create(FProvider);
end;

//------------------------------------------------------------------------------
constructor TPHXDevice.Create(const AProvider: TPHXProvider);
begin
  // Check so the provider is valid
  if AProvider = nil then
  begin
    raise Exception.Create('Creating a device without a provider is not allowed.');
  end;

  FDisplay  := TPHXDisplay.Create(Self);
  FProvider := AProvider;
  FRenderer := AProvider.CreateRenderer;

  FRenderer.EnumDisplayModes(FDisplay.Modes);

  FWireframe:= False;
  FTitle    := SDefaultTitle;
  FFlags    := DefaultWindowFlags;
end;

//------------------------------------------------------------------------------
constructor TPHXDevice.Create(const ADevice: IPHXDevice);
begin
  FDisplay  := TPHXDisplay.Create(Self);
  FProvider := nil;
  FRenderer := ADevice;

  FRenderer.EnumDisplayModes(FDisplay.Modes);

  FWireframe:= False;
  FTitle    := SDefaultTitle;
  FFlags    := DefaultWindowFlags;
end;


//------------------------------------------------------------------------------
destructor TPHXDevice.Destroy;
begin
  FDisplay.Free;

  FRenderer:= nil;

  if Assigned(FProvider) then
  begin
    FProvider.Free;
  end;

  inherited;
end;

//-----------------------------------------------------------------------------
procedure TPHXDevice.ResetRenderStates;
begin
  FRenderer.SetBlending(bmAlpha);
  FRenderer.SetWireFrame(FWireframe);
  FRenderer.SetDepthTest(False);
  FRenderer.SetDepthMask(True);

  SetBlending(bmAlpha);

  SetWireFrame(False);

  SetDepthTest(False);
  SetDepthMask(True);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Initialize;
begin
  Initialize(FTitle, FDisplay.Width, FDisplay.Height, FDisplay.Fullscreen);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Initialize(const Title: String; const Width, Height: Integer);
begin
  Initialize(Title, Width, Height, False);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Initialize(const Title: String; const Width, Height: Integer; const Fullscreen: Boolean);
var Parameters: TPHXDeviceParameters;
begin
  TPHXLogger.Info('TPHXDevice.Initialize', 'Initializing device');

  // Check so there is a valid renderer instance
  if FRenderer = nil then
  begin
    raise Exception.Create(SRendererNotLoaded);
  end;
  FTitle:= Title;

  FDisplay.Width      := Width;
  FDisplay.Height     := Height;
  FDisplay.Fullscreen := Fullscreen;

  Parameters.Title     := Title;
  Parameters.Width     := Width;
  Parameters.Height    := Height;
  Parameters.Fullscreen:= Fullscreen;
  Parameters.Events    := TPHXEvents.Instance;

  FRenderer.Initialize(Parameters);

  ResetRenderStates;

  SetClearColor(DefaultBackground);

  SetViewport(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Reinitialize;
begin
  Reinitialize(FDisplay.Width, FDisplay.Height, FDisplay.Fullscreen);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Reinitialize(const Width, Height: Integer; const Fullscreen: Boolean);
var Parameters: TPHXDeviceParameters;
begin
  FDisplay.Width      := Width;
  FDisplay.Height     := Height;
  FDisplay.Fullscreen := Fullscreen;

  Parameters.Title     := FTitle;
  Parameters.Width     := Width;
  Parameters.Height    := Height;
  Parameters.Fullscreen:= Fullscreen;
  Parameters.Events    := TPHXEvents.Instance;

  FRenderer.Reinitialize(Parameters);

  ResetRenderStates;

  SetClearColor(DefaultBackground);

  SetViewport(0, 0, Width, Height);
end;




//------------------------------------------------------------------------------
procedure TPHXDevice.Finalize;
begin
  FRenderer.Finalize;
end;

//------------------------------------------------------------------------------
function TPHXDevice.CreateRenderTarget: TPHXRenderTarget;
begin
  Result:= FRenderer.CreateRenderTarget;
end;

//------------------------------------------------------------------------------
function TPHXDevice.CreateCanvas: TPHXCanvas;
begin
  Result:= FRenderer.CreateCanvas;
end;

//------------------------------------------------------------------------------
function TPHXDevice.CreateEffect: TPHXEffect;
begin
  Result:= FRenderer.CreateEffect;
end;

//------------------------------------------------------------------------------
function TPHXDevice.CreateBuffer: TPHXBuffer;
begin
  Result:= FRenderer.CreateBuffer;
end;

//------------------------------------------------------------------------------
function TPHXDevice.CreateTexture: TPHXTexture;
begin
  Result:= FRenderer.CreateTexture;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetRenderTarget(Target: TPHXRenderTarget);
begin
  Renderer.SetRenderTarget(Target);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetViewport(const Viewport: TViewport);
begin
  FRenderer.SetViewport(Viewport);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetViewport(const X, Y, Width, Height: Integer);
var Viewport: TViewport;
begin
  Viewport.X       := X;
  Viewport.Y       := Y;
  Viewport.Width   := Width;
  Viewport.Height  := Height;
  Viewport.MinDepth:= ViewportMinDepth;
  Viewport.MaxDepth:= ViewportMaxDepth;

  FRenderer.SetViewport(Viewport);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetViewport(const Width, Height: Integer);
var Viewport: TViewport;
begin
  Viewport.X       := 0;
  Viewport.Y       := 0;
  Viewport.Width   := Width;
  Viewport.Height  := Height;
  Viewport.MinDepth:= ViewportMinDepth;
  Viewport.MaxDepth:= ViewportMaxDepth;

  FRenderer.SetViewport(Viewport);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetClearColor(const Color: TColor4f);
begin
  FRenderer.SetClearColor(Color);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetClearColor(const Red, Green, Blue: Single);
var Color: TColor4f;
begin
  Color.Red  := Red;
  Color.Green:= Green;
  Color.Blue := Blue;
  Color.Alpha:= 1.0;

  FRenderer.SetClearColor(Color);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetBlending(const Value: TPHXBlendMode);
begin
  FRenderer.SetBlending(Value);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetDepthMask(const Value: Boolean);
begin
  FRenderer.SetDepthMask(Value);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetDepthTest(const Value: Boolean);
begin
  FRenderer.SetDepthTest(Value);
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetWireFrame(const Value: Boolean);
begin
  if FWireFrame <> Value then
  begin
    FWireFrame:= Value;

    FRenderer.SetWireFrame(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Update;
begin
  FRenderer.Update;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Clear;
begin
  FRenderer.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.Flip;
begin
  FRenderer.Flip;
end;

//------------------------------------------------------------------------------
function TPHXDevice.GetTarget: TPHXProviderTarget;
begin
  Result:= FRenderer.GetTarget;
end;

//------------------------------------------------------------------------------
function TPHXDevice.GetWidth: Integer;
begin
  Result:= FRenderer.Width;
end;

//------------------------------------------------------------------------------
function TPHXDevice.GetHeight: Integer;
begin
  Result:= FRenderer.Height;
end;

//------------------------------------------------------------------------------
function TPHXDevice.GetBounds: TRecti;
begin
  Result.Left  := 0;
  Result.Top   := 0;
  Result.Right := FRenderer.Width;
  Result.Bottom:= FRenderer.Height;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetFlags(AValue: TPHXWindowFlags);
begin
  if FFlags <> AValue then
  begin
    FFlags:=AValue;

    FRenderer.Flags:= FFLags;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetIcon(AValue: String);
begin
  if FIcon <> AValue then
  begin
    FIcon:=AValue;

    FRenderer.Icon:= FIcon;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDevice.SetTitle(AValue: String);
begin
  if FTitle <> AValue then
  begin
    FTitle:=AValue;

    FRenderer.Title:= FTitle;
  end;
end;


{$ENDREGION}








{

//------------------------------------------------------------------------------
function TPHXRendererFactory.CreateRendererFromLibrary(const Flags: Cardinal = 0): IPHXRenderer;
var CreateRenderer: TPHXCreateRenderer;
var AName: String;
begin
  AName:= ExpandFileName(FName);

  if not FileExists(AName) then
  begin
    raise EPHXRendererFactoryException.CreateFmt('The renderer "%s" not found', [AName]);
  end;

  FHandle:= LoadLibrary(PChar(AName));

  if FHandle <> 0 then
  begin
    CreateRenderer:= GetProcAddress(FHandle, 'CreateRenderer');

    if Assigned(CreateRenderer) then
    begin
      FRenderer:= CreateRenderer(0);

      if FRenderer = nil then
      begin
        raise EPHXRendererFactoryException.CreateFmt('The renderer "%s" did not return a valid renderer instance.', [Name]);
      end;

    end else
    begin
      raise EPHXRendererFactoryException.CreateFmt('The renderer "%s" doesnt contains the entrypoint "CreateRenderer".', [Name]);
    end;

  end else
  begin
    RaiseLastOSError;
  end;

  Result:= FRenderer;
end;

//------------------------------------------------------------------------------
function TPHXRendererFactory.CreateRendererFromObject( AObject: TObject): IPHXRenderer;
var Renderer: IPHXRenderer;
begin
  if Supports(AObject, IPHXRenderer, Renderer) then
  begin
    Result:= Renderer;
  end else
  begin
    raise Exception.Create('The object doesnt support the IPHXRenderer interface.');
  end;
end;
}







end.
