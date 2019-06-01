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
unit phxOpenGL_GLFW;
//< OpenGL provider using GLFW

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  dglOpenGL,

  GLFW,

  phxLogger,
  phxTypes,
  phxClasses,
  phxEvents,
  phxDevice,
  phxGraphics,

  phxOpenGL;

const
  ProviderName = 'OpenGL.GLFW';

type

//-----------------------------------------------------------------------------
TPHXOpenGL_Provider_GLFW = class(TPHXProvider)
  protected
    function GetName: string; override;
    function GetTarget: TPHXProviderTarget; override;
  public
    constructor Create; override;

    function CreateRenderer: IPHXDevice; override;
  end;

// http://www.spacesimulator.net/wiki/index.php?title=Tutorials:SDL_Framework_%28OpenGL3.3%29
//-----------------------------------------------------------------------------
TPHXOpenGLRendererGLFW = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;

    FTitle     : String;
    FWidth     : Integer;
    FHeight    : Integer;
    FFullscreen: Boolean;

    FWindowFlags: TPHXWindowFlags;
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
    // Clear the back buffers
    procedure Clear; override;
    // Flip the front and back buffers
    procedure Flip; override;
  end;


implementation

// converts the fullscreen flag to the corresponding glfw flag
const glfwFullscreen : array[Boolean] of Integer = (
  GLFW_WINDOW,
  GLFW_FULLSCREEN
  );

{$REGION 'GLF event callbacks'}

var ShiftStates : TPHXShiftStates;
var MousePos    : TVector2i = (X: 0; Y: 0);

// Called glfw  when the window is closed
//------------------------------------------------------------------------------
function WindowCloseEvent: Integer; stdcall;
var Event: TPHXEvent;
begin
  Event.Event:= PHX_EVENT_QUIT;

  TPHXEvents.Notify(nil, Event);

  Result:=1;
end;

//------------------------------------------------------------------------------
procedure WindowSizeEvent(Width, Height: Integer); stdcall;
begin
  TPHXEvents.NotifyDeviceResized(nil, Width, Height);
end;

// Callback for keyboard events bound to glfw
//------------------------------------------------------------------------------
procedure KeyCallback(Key, Action: Integer); stdcall;
var Event: TPHXEvent;
begin
  case Action of
    GLFW_PRESS : begin
      case Key of
        GLFW_KEY_RSHIFT: Include(ShiftStates, ssShift);
        GLFW_KEY_LSHIFT: Include(ShiftStates, ssShift);
        GLFW_KEY_LCTRL : Include(ShiftStates, ssCtrl);
        GLFW_KEY_RCTRL : Include(ShiftStates, ssCtrl);
        GLFW_KEY_LALT  : Include(ShiftStates, ssAlt);
        GLFW_KEY_RALT  : Include(ShiftStates, ssAlt);
      end;

      Event.Keyboard.Event:= PHX_KEY_PRESSED;
      Event.Keyboard.Key  := TPHXVirtualKey(Key);
      Event.Keyboard.Shift:= ShiftStates;

      TPHXEvents.Notify(nil, Event);
    end;
    GLFW_RELEASE: begin

      case Key of
        GLFW_KEY_RSHIFT: Exclude(ShiftStates, ssShift);
        GLFW_KEY_LSHIFT: Exclude(ShiftStates, ssShift);
        GLFW_KEY_LCTRL : Exclude(ShiftStates, ssCtrl);
        GLFW_KEY_RCTRL : Exclude(ShiftStates, ssCtrl);
        GLFW_KEY_LALT  : Exclude(ShiftStates, ssAlt);
        GLFW_KEY_RALT  : Exclude(ShiftStates, ssAlt);
      end;

      Event.Keyboard.Event:= PHX_KEY_RELEASED;
      Event.Keyboard.Key  := TPHXVirtualKey(Key);
      Event.Keyboard.Shift:= ShiftStates;

      TPHXEvents.Notify(nil, Event);
    end;
  end;
end;

//-----------------------------------------------------------------------------
procedure CharCallback(Character, Action: Integer); stdcall;
var Event: TPHXEvent;
begin
  case Action of
    GLFW_PRESS: begin
      Event.Keyboard.Event:= PHX_KEY_CHARACTER;
      Event.Keyboard.Char := Chr(Character);

      TPHXEvents.Notify(nil, Event);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure MousePosCallback(X, Y: Integer); stdcall;
var Event: TPHXEvent;
begin
  Event.Mouse.Event:= PHX_MOUSE_MOVED;
  Event.Mouse.X    := X;
  Event.Mouse.Y    := Y;

  TPHXEvents.Notify(nil, Event);

  MousePos.X:= X;
  MousePos.Y:= Y;
end;

//------------------------------------------------------------------------------
procedure MouseButtonCallback(Button, Action: Integer); stdcall;
var Event: TPHXEvent;
begin
  case Action of
    GLFW_PRESS : begin
      Event.Mouse.Event := PHX_MOUSE_PRESSED;
      Event.Mouse.Button:= TPHXMouseButton(Button);
      Event.Mouse.X     := MousePos.X;
      Event.Mouse.Y     := MousePos.Y;
      Event.Mouse.Shift := ShiftStates;

      TPHXEvents.Notify(nil, Event);
    end;
    GLFW_RELEASE: begin

      Event.Mouse.Event := PHX_MOUSE_RELEASED;
      Event.Mouse.Button:= TPHXMouseButton(Button);
      Event.Mouse.X     := MousePos.X;
      Event.Mouse.Y     := MousePos.Y;
      Event.Mouse.Shift := ShiftStates;

      TPHXEvents.Notify(nil, Event);
    end;
  end;

end;

{$ENDREGION}


{$REGION 'TPHXOpenGL_Provider_GLFW'}

// TPHXProvider_OpenGL_GLFW
//==============================================================================
constructor TPHXOpenGL_Provider_GLFW.Create;
begin
  inherited;

end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_GLFW.GetName: string;
begin
  Result:= ProviderName;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_GLFW.GetTarget: TPHXProviderTarget;
begin
  Result:= ptOpenGL_3;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_GLFW.CreateRenderer: IPHXDevice;
begin
  Result:= TPHXOpenGLRendererGLFW.Create;
end;

{$ENDREGION}


{$REGION 'TPHXOpenGLRendererGLFW'}

//==============================================================================
constructor TPHXOpenGLRendererGLFW.Create;
begin
  inherited Create;

  FInitialized:= False;

  FTitle     := 'Phoenix Framework';
  FWidth     := 800;
  FHeight    := 600;
  FFullscreen:= False;
end;

//------------------------------------------------------------------------------
destructor TPHXOpenGLRendererGLFW.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.EnumDisplayModes( const Modes: TPHXDisplayModes);
begin
  // TODO: TPHXOpenGLRendererGLFW.EnumDisplayModes
end;

var  major, minor, rev: Integer;

// http://www.gamedev.net/topic/603282-3x-context-creation-in-glfw/
// http://stackoverflow.com/questions/9017023/cannot-deploy-glfw-3-2
//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Initialize(
  const Parameters: TPHXDeviceParameters);
begin
  FTitle := Parameters.Title;
  FWidth := Parameters.Width;
  FHeight:= Parameters.Height;

  if glfwInit() <> 1 then
  begin
    raise Exception.Create('Failed to initialize GLFW');
  end;

  glfwGetGLVersion(major, minor, rev);

 // glfwOpenWindowHint(GLFW_WINDOW_NO_RESIZE, GL_TRUE);
 // glfwOpenWindowHint(GLFW_OPENGL_VERSION_MAJOR, 3);
 // glfwOpenWindowHint(GLFW_OPENGL_VERSION_MINOR, 3);
 // glfwOpenWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  //  glfwOpenWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); //This is what fails for me, setting it to GL_FALSE works just fine


  //glfwOpenWindowHint(GLFW_FSAA_SAMPLES, 4); // 4x antialiasing
  //glfwOpenWindowHint(GLFW_OPENGL_VERSION_MAJOR, 3); // We want OpenGL 3.3
  //glfwOpenWindowHint(GLFW_OPENGL_VERSION_MINOR, 3);
  //glfwOpenWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE); //We don't want the old OpenGL

  // Use OpenGL Core v3.2
 // glfwOpenWindowHint(GLFW_OPENGL_VERSION_MAJOR, 3);
 // glfwOpenWindowHint(GLFW_OPENGL_VERSION_MINOR, 2);
 // glfwOpenWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);


  // Open OpenGL window
  if glfwOpenWindow(FWidth, FHeight, 0,0,0,0, 32,0, glfwFullscreen[Parameters.Fullscreen] ) <> 1 then
  begin
    glfwTerminate;
    Exit;
  end;

  glfwSetWindowTitle( PChar( FTitle ) );

  glfwSetWindowSizeCallback(@WindowSizeEvent);
  glfwSetWindowCloseCallback(@WindowCloseEvent);

  glfwSetKeyCallback(@KeyCallback);
  glfwSetCharCallback(@CharCallback);

  glfwSetMouseButtonCallback( @MouseButtonCallback );
  glfwSetMousePosCallback   ( @MousePosCallback );

  glfwSwapInterval(1) ;

    // Enable sticky mouse keys
  glfwEnable( GLFW_STICKY_MOUSE_BUTTONS );

  if (dglOpenGL.InitOpenGL <> True) then
  begin
    raise Exception.Create('OpenGL Initialization failed.');
  end;
  dglOpenGL.ReadExtensions;

  InitializeOpenGL;

end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Reinitialize(
  const Parameters: TPHXDeviceParameters);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Finalize;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Update;
begin
  if glfwGetWindowParam( GLFW_OPENED ) <> 1 then
  begin
    TPHXEvents.NotifyQuit(Self);
  end;


//  SDLEvent;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Clear;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );//or GL_SCISSOR_BIT);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.Flip;
begin
  glfwSwapBuffers();
end;


//------------------------------------------------------------------------------
function TPHXOpenGLRendererGLFW.GetWidth: Integer;
begin
  Result:= FWidth;
end;

//------------------------------------------------------------------------------
function TPHXOpenGLRendererGLFW.GetHeight: Integer;
begin
  Result:= FHeight;
end;


//-----------------------------------------------------------------------------
function TPHXOpenGLRendererGLFW.GetFlags: TPHXWindowFlags;
begin
  Result:= FWindowFlags;
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.SetTitle(const Title: String);
begin
  FTitle:= Title;
end;


//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.SetFlags(const Flags: TPHXWindowFlags);
begin
  FWindowFlags:= Flags;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGLRendererGLFW.SetIcon(const Icon: String);
begin

end;



{$ENDREGION}




initialization
  RegisterProvider(ProviderName, TPHXOpenGL_Provider_GLFW);
end.
