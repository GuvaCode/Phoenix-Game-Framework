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
unit phxOpenGL_SDL;
//< OpenGL provider using SDL

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

 // {$IFDEF FPC}
 // GL,
  //{$ELSE}
  dglOpenGL,
  //{$ENDIF}

  SDL,

  phxLogger,
  phxTypes,
  phxClasses,
  phxEvents,
  phxDevice,
  phxGraphics,

  phxOpenGL;

const
  ProviderName = 'OpenGL.SDL';

type

// Provider class for the OpenGL.SDL provider
//-----------------------------------------------------------------------------
TPHXOpenGL_Provider_SDL = class(TPHXProvider)
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

// OpenGL 2.X renderer using SDL
//-----------------------------------------------------------------------------
TPHXOpenGL_Renderer_SDL = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;

    FTitle     : String;
    FWidth     : Integer;
    FHeight    : Integer;
    FFullscreen: Boolean;

    ShiftStates : TPHXShiftStates;

    FFlags: TPHXWindowFlags;

    FScreen  : PSDL_Surface;
    FIcon    : PSDL_Surface;
    FIconName: String;

    // Load the sdl icon
    procedure LoadIcon;

    procedure InitJoysticks;

    procedure SDLEvent(SDLEvent: TSDL_Event);
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

implementation

uses phxInput;

{$REGION 'SDL Utils'}

//------------------------------------------------------------------------------
function SDLKeyToPHXKey(const Key: TSDLKey): TPHXVirtualKey;
begin
  Result:= TPHXVirtualKey(Key);

  case Key of
    // Arrows + Home/End pad
    SDLK_UP       : Result:= VK_UP;
    SDLK_DOWN     : Result:= VK_DOWN;
    SDLK_RIGHT    : Result:= VK_RIGHT;
    SDLK_LEFT     : Result:= VK_LEFT;
    SDLK_INSERT   : Result:= VK_INSERT;
    SDLK_HOME     : Result:= VK_HOME;
    SDLK_END      : Result:= VK_END;

    SDLK_PAGEUP   : Result:= VK_PAGEUP;
    SDLK_PAGEDOWN : Result:= VK_PAGEDOWN;

    // Numeric keypad
    SDLK_KP0          : Result:= VK_NUM_0;
    SDLK_KP1          : Result:= VK_NUM_1;
    SDLK_KP2          : Result:= VK_NUM_2;
    SDLK_KP3          : Result:= VK_NUM_3;
    SDLK_KP4          : Result:= VK_NUM_4;
    SDLK_KP5          : Result:= VK_NUM_5;
    SDLK_KP6          : Result:= VK_NUM_6;
    SDLK_KP7          : Result:= VK_NUM_7;
    SDLK_KP8          : Result:= VK_NUM_8;
    SDLK_KP9          : Result:= VK_NUM_9;
    SDLK_KP_PERIOD    : Result:= VK_NUM_DECIMAL;
    SDLK_KP_DIVIDE    : Result:= VK_NUM_DIVIDE;
    SDLK_KP_MULTIPLY  : Result:= VK_NUM_MULTIPLY;
    SDLK_KP_MINUS     : Result:= VK_NUM_SUBTRACT;
    SDLK_KP_PLUS      : Result:= VK_NUM_ADD;
    SDLK_KP_ENTER     : Result:= VK_NUM_ENTER;
    SDLK_KP_EQUALS    : Result:= VK_NUM_EQUAL;

    SDLK_RSHIFT: Result:= VK_RSHIFT;
    SDLK_LSHIFT: Result:= VK_LSHIFT;
    SDLK_RCTRL : Result:= VK_RCTRL;
    SDLK_LCTRL : Result:= VK_LCTRL;
    SDLK_RALT  : Result:= VK_RALT;
    SDLK_LALT  : Result:= VK_LALT;

    // Function keys
    SDLK_F1  : Result:= VK_F1;
    SDLK_F2  : Result:= VK_F2;
    SDLK_F3  : Result:= VK_F3;
    SDLK_F4  : Result:= VK_F4;
    SDLK_F5  : Result:= VK_F5;
    SDLK_F6  : Result:= VK_F6;
    SDLK_F7  : Result:= VK_F7;
    SDLK_F8  : Result:= VK_F8;
    SDLK_F9  : Result:= VK_F9;
    SDLK_F10 : Result:= VK_F10;
    SDLK_F11 : Result:= VK_F11;
    SDLK_F12 : Result:= VK_F12;
    SDLK_F13 : Result:= VK_F13;
    SDLK_F14 : Result:= VK_F14;
    SDLK_F15 : Result:= VK_F15;

    SDLK_ESCAPE     : Result:= VK_ESC;

    SDLK_TAB        : Result:= VK_TAB;
    SDLK_RETURN     : Result:= VK_RETURN;
    SDLK_BACKSPACE  : Result:= VK_BACKSPACE;
    SDLK_DELETE     : Result:= VK_DEL;
  end;
end;
    (*
  SDL_BUTTON_LEFT      = 1;
{$EXTERNALSYM SDL_BUTTON_LEFT}
  SDL_BUTTON_MIDDLE    = 2;
{$EXTERNALSYM SDL_BUTTON_MIDDLE}
  SDL_BUTTON_RIGHT     = 3;
{$EXTERNALSYM SDL_BUTTON_RIGHT}
  SDL_BUTTON_WHEELUP   = 4;
{$EXTERNALSYM SDL_BUTTON_WHEELUP}
  SDL_BUTTON_WHEELDOWN = 5;
{$EXTERNALSYM SDL_BUTTON_WHEELDOWN}
  SDL_BUTTON_X1        = 6;
{$EXTERNALSYM SDL_BUTTON_X1}
  SDL_BUTTON_X2        = 7;
{$EXTERNALSYM SDL_BUTTON_X2}
  *)
//------------------------------------------------------------------------------
function SDLButtonToPHXButton(const Button: Byte): TPHXMouseButton;
begin
  Result:= PHX_MOUSE_BUTTON_1;
  case Button of
    SDL_BUTTON_LEFT    : Result:= mbLeft;
    SDL_BUTTON_MIDDLE   : Result:= mbMiddle;
    SDL_BUTTON_RIGHT    : Result:= mbRight;
    SDL_BUTTON_WHEELUP  : Result:= PHX_MOUSE_BUTTON_4;
    SDL_BUTTON_WHEELDOWN: Result:= PHX_MOUSE_BUTTON_5;
    SDL_BUTTON_X1       : Result:= PHX_MOUSE_BUTTON_6;
    SDL_BUTTON_X2       : Result:= PHX_MOUSE_BUTTON_7;
  end;

end;


{$ENDREGION}

{$REGION 'TPHXOpenGL_Provider_SDL'}

// TPHXOpenGLProviderSDL
//==============================================================================
constructor TPHXOpenGL_Provider_SDL.Create;
begin
  inherited;

end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_SDL.GetName: string;
begin
  Result:= ProviderName;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_SDL.GetTarget: TPHXProviderTarget;
begin
  Result:= ptOpenGL_3;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Provider_SDL.CreateRenderer: IPHXDevice;
begin
  Result:= TPHXOpenGL_Renderer_SDL.Create;
end;

{$ENDREGION}

{$REGION 'TPHXOpenGL_Renderer_SDL'}

// TPHXOpenGL2RendererSDL
//==============================================================================
constructor TPHXOpenGL_Renderer_SDL.Create;
begin
  inherited Create;

  FInitialized:= False;
  FIcon       := nil;

  FTitle     := 'Phoenix Game Framework';
  FWidth     := 800;
  FHeight    := 600;
  FFullscreen:= False;
  FFlags     := DefaultWindowFlags;

  // Register SDL_Quit to be called at exit; makes sure things are cleaned up when we quit.
  ExitProc := @SDL_Quit;
end;

//------------------------------------------------------------------------------
destructor TPHXOpenGL_Renderer_SDL.Destroy;
begin

  if Assigned(FIcon) then
  begin
    SDL_FreeSurface(FIcon);

    FIcon:= nil;
  end;

  inherited;
end;
//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.EnumDisplayModes(const Modes: TPHXDisplayModes);
var SDLInit : Boolean;
var SDLInfo : PSDL_VideoInfo;
var SDLModes: PPSDL_Rect;
var SDLMode : SDL_Rect;
begin

  SDLInit:= SDL_WasInit(SDL_INIT_VIDEO ) = 0;
  try
    if SDLInit then
    begin
      SDL_Init( SDL_INIT_VIDEO);
    end;
    SDLInfo:= SDL_GetVideoInfo();

    // Set the desktop mode of the display
    Modes.Desktop:= TPHXDisplayMode.Create('Desktop', SDLInfo^.current_w, SDLInfo^.current_h);

    SDLModes:= SDL_ListModes(nil, SDL_FULLSCREEN or SDL_HWSURFACE );

    // There is not any mode available for the particular format
    if (SDLModes = PPSDL_Rect(0) ) then
    begin
      Exit;
    end;
    // Any dimension is okay for the given format
    if (SDLModes = PPSDL_Rect(-1) ) then
    begin
      Exit;
    end;

    Modes.Clear;
    repeat
      SDLMode:= (SDLModes^)^;

      Modes.Add(SDLMode.w, SDLMode.h);

      Inc(SDLModes);
    until (SDLModes^)=nil;

    Modes.Sort;
  finally
    if SDLInit then
    begin
      SDL_Quit();
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.Initialize(const Parameters: TPHXDeviceParameters);
var Version: TSDL_Version;
var Flags  : Cardinal;
var Info   : PSDL_VideoInfo;
begin
  FTitle     := Parameters.Title;
  FWidth     := Parameters.Width;
  FHeight    := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

  Version.major:= 0;
  Version.minor:= 0;
  Version.patch:= 0;

  SDL_VERSION(Version);

  TPHXLogger.Info('TPHXOpenGL_Renderer_SDL.Initialize', 'Initializing SDL Renderer, version: %d.%d.%d', [Version.major, Version.minor, Version.patch ]);

  // Initialize SDL's subsystems - in this case, only video.
  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_JOYSTICK ) < 0 ) then
  begin
    raise Exception.CreateFmt('Unable to init SDL: %s\n', [SDL_GetError()] );
  end;
  SDL_putenv('SDL_VIDEO_WINDOW_POS');
  SDL_putenv('SDL_VIDEO_CENTERED=1');

  SDL_WM_SetCaption(PAnsiChar( AnsiString(FTitle) ), PAnsiChar( AnsiString(FTitle) ));

  LoadIcon;

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );


  Flags:= SDL_OPENGL;
  if wfResizable in FFlags then
  begin
    Flags:= Flags or SDL_RESIZABLE;
  end;
  if FFullscreen then
  begin
    Flags:= Flags or SDL_FULLSCREEN;
  end;

  Info:= SDL_GetVideoInfo();

  // Attempt to create a window with the specified height and width.
  FScreen:= SDL_SetVideoMode( FWidth, FHeight, Info.vfmt.BitsPerPixel, Flags);

  // If we fail, return error.
  if ( FScreen = nil ) then
  begin
    TPHXLogger.Error('TPHXOpenGL_Renderer_SDL.Initialize', 'Unable to set up video: %s\n', [ SDL_GetError() ]);

    raise Exception.CreateFmt('Unable to set up video: %s\n', [SDL_GetError()] );
  end;

  if wfVerticalSync in FFlags then
  begin
    SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 1);
  end else
  begin
    SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 0);
  end;

  if wfCursor in FFlags then
  begin
    SDL_ShowCursor(SDL_ENABLE);
  end else
  begin
    SDL_ShowCursor(SDL_DISABLE);
  end;

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE( SDL_ENABLE );

  SDL_JoystickEventState(SDL_ENABLE);

  InitJoysticks;


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
procedure TPHXOpenGL_Renderer_SDL.Reinitialize( const Parameters: TPHXDeviceParameters);
var Info : PSDL_VideoInfo;
var Flags: Cardinal;
begin
  FTitle     := Parameters.Title;
  FWidth     := Parameters.Width;
  FHeight    := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

  Info:= SDL_GetVideoInfo();

  Flags:= SDL_OPENGL;
  if wfResizable in FFlags then
  begin
    Flags:= Flags or SDL_RESIZABLE;
  end;
  if FFullscreen then
  begin
    Flags:= Flags or SDL_FULLSCREEN;
  end;

  // Make shure to reset the blending, else the new blending mode might not be set
  SetBlending(bmNormal);

  FScreen:= SDL_SetVideoMode( FWidth, FHeight, Info.vfmt.BitsPerPixel, Flags);

  // If we fail, return error.
  if ( FScreen = nil ) then
  begin
    TPHXLogger.Error('TPHXOpenGL_Renderer_SDL.Initialize', 'Unable to set up video: %s\n', [ SDL_GetError() ]);

    raise Exception.CreateFmt('Unable to set up video: %s\n', [SDL_GetError()] );
  end;

  TPHXNotifications.Notify(dnContextCreated);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.Finalize;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.Update;
var Event: TSDL_Event;
begin
  // Poll for events, and handle the ones we care about.
  while SDL_PollEvent( @Event ) > 0 do
  begin
    SDLEvent(Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.Flip;
begin
  SDL_GL_SwapBuffers();
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.SDLEvent(SDLEvent: TSDL_Event);
var Event: TPHXEvent;
begin
  FillChar(Event, SizeOf(TPHXEvent), #0);

  case SDLEvent.type_ of
    SDL_KEYDOWN:
    begin
      if( SDLEvent.key.keysym.modifier and KMOD_CAPS > 0 ) then
      begin
        Include(ShiftStates, ssCaps)
      end else
      begin
        Exclude(ShiftStates, ssCaps);
      end;

      case SDLEvent.key.keysym.sym of
        SDLK_RSHIFT: Include(ShiftStates, ssShift);
        SDLK_LSHIFT: Include(ShiftStates, ssShift);
        SDLK_RCTRL : Include(ShiftStates, ssCtrl);
        SDLK_LCTRL : Include(ShiftStates, ssCtrl);
        SDLK_RALT  : Include(ShiftStates, ssAlt);
        SDLK_LALT  : Include(ShiftStates, ssAlt);
      end;

      Event.Keyboard.Event:= PHX_KEY_PRESSED;
      Event.Keyboard.Key  := SDLKeyToPHXKey(SDLEvent.key.keysym.sym);
      Event.Keyboard.Shift:= ShiftStates;

      TPHXEvents.Notify(Self, Event);

      Event.Keyboard.Event:= PHX_KEY_CHARACTER;
      Event.Keyboard.Char := Chr(SDLEvent.key.keysym.unicode);
      Event.Keyboard.Shift:= ShiftStates;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_KEYUP:
    begin

      case SDLEvent.key.keysym.sym of
        SDLK_RSHIFT: Exclude(ShiftStates, ssShift);
        SDLK_LSHIFT: Exclude(ShiftStates, ssShift);
        SDLK_RCTRL : Exclude(ShiftStates, ssCtrl);
        SDLK_LCTRL : Exclude(ShiftStates, ssCtrl);
        SDLK_RALT  : Exclude(ShiftStates, ssAlt);
        SDLK_LALT  : Exclude(ShiftStates, ssAlt);
      end;

      Event.Keyboard.Event:= PHX_KEY_RELEASED;
      Event.Keyboard.Key  := SDLKeyToPHXKey(SDLEvent.key.keysym.sym);
      Event.Keyboard.Shift:= ShiftStates;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_QUITEV:
    begin
      Event.Event:= PHX_EVENT_QUIT;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_MOUSEMOTION:
    begin
      Event.Mouse.Event:= PHX_MOUSE_MOVED;
      Event.Mouse.X    := SDLEvent.Motion.X;
      Event.Mouse.Y    := SDLEvent.Motion.Y;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_MOUSEBUTTONUP:
    begin
      Event.Mouse.Event := PHX_MOUSE_RELEASED;
      Event.Mouse.Button:= SDLButtonToPHXButton(SDLEvent.Button.Button);
      Event.Mouse.X     := SDLEvent.Motion.X;
      Event.Mouse.Y     := SDLEvent.Motion.Y;
      Event.Mouse.Shift := ShiftStates;
                  (*
      case SDLEvent.Button.Button of
        SDL_BUTTON_LEFT      : Event.Mouse.Button:= PHX_MOUSE_BUTTON_LEFT;
        SDL_BUTTON_RIGHT     : Event.Mouse.Button:= PHX_MOUSE_BUTTON_RIGHT;
        SDL_BUTTON_MIDDLE    : Event.Mouse.Button:= PHX_MOUSE_BUTTON_MIDDLE;
        SDL_BUTTON_WHEELUP   : Event.Mouse.Event := PHX_MOUSE_WHEELUP;
        SDL_BUTTON_WHEELDOWN : Event.Mouse.Event := PHX_MOUSE_WHEELDOWN;
      end;
              *)
      TPHXEvents.Notify(Self, Event);
    end;
    SDL_MOUSEBUTTONDOWN:
    begin
      Event.Mouse.Event := PHX_MOUSE_PRESSED;
      Event.Mouse.Button:= SDLButtonToPHXButton(SDLEvent.Button.Button);
      Event.Mouse.X     := SDLEvent.Motion.X;
      Event.Mouse.Y     := SDLEvent.Motion.Y;
      Event.Mouse.Shift := ShiftStates;
             (*
      case SDLEvent.Button.Button of
        SDL_BUTTON_LEFT      : Event.Mouse.Button:= PHX_MOUSE_BUTTON_LEFT;
        SDL_BUTTON_RIGHT     : Event.Mouse.Button:= PHX_MOUSE_BUTTON_RIGHT;
        SDL_BUTTON_MIDDLE    : Event.Mouse.Button:= PHX_MOUSE_BUTTON_MIDDLE;
      end;
          *)
      TPHXEvents.Notify(Self, Event);
    end;
    SDL_VIDEORESIZE:
    begin
      FWidth := SDLEvent.resize.w;
      FHeight:= SDLEvent.resize.h;

      Event.Device.Event := PHX_DEVICE_RESIZED;
      Event.Device.Width := FWidth;
      Event.Device.Height:= FHeight;

      TPHXEvents.Notify(Self, Event);
    end;
    // Application visibility event structure
    SDL_ACTIVEEVENT:
    begin
      if (SDLEvent.active.state and SDL_APPINPUTFOCUS) = SDL_APPINPUTFOCUS then
      begin

        if SDLEvent.active.gain = 1 then
        begin
          Event.Device.Event := PHX_DEVICE_ACTIVATED;
        end else
        begin
          Event.Device.Event:= PHX_DEVICE_DEACTIVATED;
        end;

        Event.Device.Width := FWidth;
        Event.Device.Height:= FHeight;
        Event.Device.Shift := ShiftStates;

        TPHXEvents.Notify(Self, Event);
      end;
    end;
    SDL_JOYAXISMOTION:
    begin
      Event.Joystick.Event   := PHX_JOYSTICK_AXIS;
      Event.Joystick.Index   := TPHXJoystickIndex(SDLEvent.jaxis.which);
      Event.Joystick.Axis    := TPHXJoystickAxis (SDLEvent.jaxis.axis);
      Event.Joystick.Position:=                   SDLEvent.jaxis.value;
      Event.Joystick.Shift   := ShiftStates;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_JOYBUTTONDOWN:
    begin
      Event.Joystick.Event       := PHX_JOYSTICK_PRESSED;
      Event.Joystick.Index       := TPHXJoystickIndex (SDLEvent.jbutton.which);
      Event.Joystick.Button      := TPHXJoystickButton(SDLEvent.jbutton.button+1);
      Event.Joystick.Shift       := ShiftStates;

      TPHXEvents.Notify(Self, Event);
    end;
    SDL_JOYBUTTONUP:
    begin
      Event.Joystick.Event       := PHX_JOYSTICK_RELEASED;
      Event.Joystick.Index       := TPHXJoystickIndex (SDLEvent.jbutton.which);
      Event.Joystick.Button      := TPHXJoystickButton(SDLEvent.jbutton.button+1);
      Event.Joystick.Shift       := ShiftStates;

      TPHXEvents.Notify(Self, Event);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.LoadIcon;
var colorkey: Cardinal;
begin
  if FileExists(FIconName) then
  begin
    FIcon:= SDL_LoadBMP( PAnsiChar(AnsiString(FIconName)));

    colorkey:= SDL_MapRGB(FIcon.format, 255, 0, 255);

    SDL_SetColorKey(FIcon, SDL_SRCCOLORKEY, colorkey);

    SDL_WM_SetIcon(FIcon, 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.InitJoysticks;
var Count: Integer;
var Index: Integer;
var Joystick: PSDL_Joystick;
begin
  Count:= SDL_NumJoysticks();

  for Index := 0 to Count-1 do
  begin
    Joystick:= SDL_JoystickOpen(Index);

    if ( Integer(Joystick.index) <= Ord(High(TPHXJoystickIndex))) then
    begin
      RegisterJoystick( TPHXJoystickIndex(Joystick.index), String(Joystick.name), Joystick.naxes, Joystick.nbuttons);
    end;
  end;

//  SDL_JoystickName()
end;


//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer_SDL.GetWidth: Integer;
begin
  Result:= FWidth;
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer_SDL.GetHeight: Integer;
begin
  Result:= FHeight;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_Renderer_SDL.GetFlags: TPHXWindowFlags;
begin
  Result:= FFlags;
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.SetTitle(const Title: String);
begin
  FTitle:= Title;

  SDL_WM_SetCaption( PAnsiChar(AnsiString( FTitle )), PAnsiChar(AnsiString( FTitle )));
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.SetFlags(const Flags: TPHXWindowFlags);
begin
  FFlags:= Flags;

  if not FInitialized then Exit;


  if wfVerticalSync in Flags then
  begin
    SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 1);
  end else
  begin
    SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 0);
  end;

  if wfCursor in Flags then
  begin
    SDL_ShowCursor(SDL_ENABLE);
  end else
  begin
    SDL_ShowCursor(SDL_DISABLE);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer_SDL.SetIcon(const Icon: String);
var colorkey: Cardinal;
begin
  FIconName:= Icon;

  if FInitialized and FileExists(FIconName) then
  begin
    FIcon:= SDL_LoadBMP( PAnsiChar(AnsiString(FIconName)));

    colorkey:= SDL_MapRGB(FIcon.format, 255, 0, 255);

    SDL_SetColorKey(FIcon, SDL_SRCCOLORKEY, colorkey);

    SDL_WM_SetIcon(FIcon, 0);
  end;
end;


{$ENDREGION}



initialization
  RegisterProvider(ProviderName, TPHXOpenGL_Provider_SDL);
end.
