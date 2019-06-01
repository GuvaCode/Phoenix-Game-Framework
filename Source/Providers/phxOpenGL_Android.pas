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
unit phxOpenGL_Android;
//< OpenGL provider for Android devices

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  dglOpenGL,

  phxLogger,
  phxTypes,
  phxClasses,
  phxEvents,
  phxDevice,
  phxGraphics,

  phxOpenGL,

  //egl
;

const
  ProviderName = 'OpenGL.Android';

type

{$REGION 'TPHXAndroidProvider'}

// Provider class for the OpenGL.SDL provider
//-----------------------------------------------------------------------------
TPHXAndroidProvider = class(TPHXProvider)
  protected
    // Return the name of the provider
    function GetName: string; override;
    // The graphics API the provider is using
    function GetTarget: TPHXProviderTarget; override;
  public
    // Create this provider
    constructor Create; override;

    // Create the renderer class
    function CreateRenderer: IPHXDevice; override;
  end;

{$ENDREGION}

{$REGION 'TPHXAndroidDevice'}

// OpenGL ES device for android
//-----------------------------------------------------------------------------
TPHXAndroidDevice = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;

    FTitle     : String;
    FWidth     : Integer;
    FHeight    : Integer;
    FFullscreen: Boolean;

    Surface: EGLSurface;
    Context: EGLContext;

    FFlags: TPHXWindowFlags;
  protected
    // Get the current window width
    function GetWidth: Integer; override;
    // Get the current window height
    function GetHeight: Integer; override;
    // Get the window flags
    function GetFlags: TPHXWindowFlags; override;

    // Set the window title
    procedure SetTitle(const Title: String); override;
    // Set the window flags
    procedure SetFlags(const Flags: TPHXWindowFlags); override;
    // Load a new window icon
    procedure SetIcon(const Icon: String); override;
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;

    // Enumerate all supported display modes
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); override;

    // Initialize the renderer
    procedure Initialize(const Parameters: TPHXDeviceParameters); override;
    // Reinitializes the renderer using a new display mode
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    // Finalize the renderer
    procedure Finalize; override;

    // Update the renderer
    procedure Update; override;
    // Flip the front and back buffers
    procedure Flip; override;
  end;


{$ENDREGION}

implementation

{$REGION 'TPHXAndroidProvider'}

// TPHXAndroidProvider
//==============================================================================
constructor TPHXAndroidProvider.Create;
begin
  inherited;

end;

//-----------------------------------------------------------------------------
function TPHXAndroidProvider.GetName: string;
begin
  Result:= ProviderName;
end;

//-----------------------------------------------------------------------------
function TPHXAndroidProvider.GetTarget: TPHXProviderTarget;
begin
  Result:= ptOpenGL_ES;
end;

//-----------------------------------------------------------------------------
function TPHXAndroidProvider.CreateRenderer: IPHXDevice;
begin
  Result:= TPHXAndroidDevice.Create;
end;

{$ENDREGION}


{$REGION 'TPHXAndroidDevice'}

// TPHXAndroidDevice
//==============================================================================
constructor TPHXAndroidDevice.Create;
begin
  inherited Create;

  FInitialized:= False;

  FTitle     := 'Phoenix Game Framework';
  FWidth     := 800;
  FHeight    := 600;
  FFullscreen:= False;
  FFlags     := DefaultWindowFlags;
end;

//------------------------------------------------------------------------------
destructor TPHXAndroidDevice.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.EnumDisplayModes(const Modes: TPHXDisplayModes);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.Initialize(const Parameters: TPHXDeviceParameters);
begin
  FTitle     := Parameters.Title;
  FWidth     := Parameters.Width;
  FHeight    := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

  if (dglOpenGL.InitOpenGL <> True) then
  begin
    raise Exception.Create('OpenGL Initialization failed.');
  end;

  dglOpenGL.ReadExtensions;

  // OpenGL failed to initialize, no OpenGL drivers installed?
  if not Assigned(glEnable) then
  begin
    raise Exception.Create('OpenGL Initialization failed.');
  end;

  InitializeOpenGL;

  FInitialized:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.Reinitialize( const Parameters: TPHXDeviceParameters);
var Flags: Cardinal;
begin
  FTitle     := Parameters.Title;
  FWidth     := Parameters.Width;
  FHeight    := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

  TPHXNotifications.Notify(dnContextCreated);
end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.Finalize;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.Update;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.Flip;
begin
  eglSwapBuffers(Display, Surface);
end;

//------------------------------------------------------------------------------
function TPHXAndroidDevice.GetWidth: Integer;
begin
  Result:= FWidth;
end;

//------------------------------------------------------------------------------
function TPHXAndroidDevice.GetHeight: Integer;
begin
  Result:= FHeight;
end;

//-----------------------------------------------------------------------------
function TPHXAndroidDevice.GetFlags: TPHXWindowFlags;
begin
  Result:= FFlags;
end;

//-----------------------------------------------------------------------------
procedure TPHXAndroidDevice.SetTitle(const Title: String);
begin
  FTitle:= Title;
end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.SetFlags(const Flags: TPHXWindowFlags);
begin
  FFlags:= Flags;
end;

//------------------------------------------------------------------------------
procedure TPHXAndroidDevice.SetIcon(const Icon: String);
begin
end;


{$ENDREGION}



initialization
  RegisterProvider(ProviderName, TPHXAndroidProvider);
end.
