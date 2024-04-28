unit phxOpenGL_SDL2;
// open gl provider for PGF
// guvacode@gmail.com
//

interface

uses
  SysUtils, Classes, dglOpenGL, SDL2, phxLogger, phxTypes, phxClasses, phxEvents, phxDevice, phxGraphics, phxOpenGL;

const
  ProviderName = 'OpenGL.SDL2';

type

  { TPHXOpenGL_Provider_SDL2 }

  TPHXOpenGL_Provider_SDL2 = class(TPHXProvider)
  protected
    function GetName: String; override;
    function GetTarget: TPHXProviderTarget; override;
  public
    constructor Create; override;
    function CreateRenderer: IPHXDevice; override;
  end;

  { TPHXOpenGLRendererSDL2 }

  TPHXOpenGLRendererSDL2 = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;
    FWindow: PSDL_Window;
    FSurface: PSDL_Surface;
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FFullscreen: Boolean;
    FWindowFlags: TPHXWindowFlags;
    ShiftStates : TPHXShiftStates;

    procedure SDLEvent(SDLEvent: TSDL_Event);
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetFlags: TPHXWindowFlags; override;
    procedure SetTitle(const Title: String); override;
    procedure SetFlags(const Flags: TPHXWindowFlags); override;
    procedure SetIcon(const Icon: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); override;
    procedure Initialize(const Parameters: TPHXDeviceParameters); override;
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    procedure Finalize; override;
    procedure Update; override;
    procedure Clear; override;
    procedure Flip; override;
  end;



implementation

function SDL2_KeyToPHXKey(const Key: Integer): TPHXVirtualKey;
begin
  case Key of
    SDLK_UP       : Result:= VK_UP;
    SDLK_DOWN     : Result:= VK_DOWN;
    SDLK_RIGHT    : Result:= VK_RIGHT;
    SDLK_LEFT     : Result:= VK_LEFT;
    SDLK_INSERT   : Result:= VK_INSERT;
    SDLK_HOME     : Result:= VK_HOME;
    SDLK_END      : Result:= VK_END;
    SDLK_PAGEUP   : Result:= VK_PAGEUP;
    SDLK_PAGEDOWN : Result:= VK_PAGEDOWN;
    SDLK_KP_0          : Result:= VK_NUM_0;
    SDLK_KP_1          : Result:= VK_NUM_1;
    SDLK_KP_2          : Result:= VK_NUM_2;
    SDLK_KP_3          : Result:= VK_NUM_3;
    SDLK_KP_4          : Result:= VK_NUM_4;
    SDLK_KP_5          : Result:= VK_NUM_5;
    SDLK_KP_6          : Result:= VK_NUM_6;
    SDLK_KP_7          : Result:= VK_NUM_7;
    SDLK_KP_8          : Result:= VK_NUM_8;
    SDLK_KP_9          : Result:= VK_NUM_9;
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

function SDL2_ButtonToPHXButton(const Button: Byte): TPHXMouseButton;
begin
  Result:= PHX_MOUSE_BUTTON_1;
  case Button of
    SDL_BUTTON_LEFT    : Result:= mbLeft;
    SDL_BUTTON_MIDDLE   : Result:= mbMiddle;
    SDL_BUTTON_RIGHT    : Result:= mbRight;
    SDL_BUTTON_X1       : Result:= PHX_MOUSE_BUTTON_6;
    SDL_BUTTON_X2       : Result:= PHX_MOUSE_BUTTON_7;
  end;
end;

{ TPHXOpenGL_Provider_SDL2 }

function TPHXOpenGL_Provider_SDL2.GetName: String;
begin
  Result:=ProviderName;
end;

function TPHXOpenGL_Provider_SDL2.GetTarget: TPHXProviderTarget;
begin
  Result:=ptOpenGL_3;
end;

constructor TPHXOpenGL_Provider_SDL2.Create;
begin
  inherited Create;
end;

function TPHXOpenGL_Provider_SDL2.CreateRenderer: IPHXDevice;
begin
  Result := TPHXOpenGLRendererSDL2.Create;
end;

{ TPHXOpenGLRendererSDL2 }

procedure TPHXOpenGLRendererSDL2.SDLEvent(SDLEvent: TSDL_Event);
var Event: TPHXEvent;
Check:integer;
begin
   FillChar(Event, SizeOf(TPHXEvent), #0);

   case SDLEvent.type_ of
     SDL_KEYDOWN:
     begin
      if SDLEvent.key.keysym.scancode = 0 then exit;
      if( SDLEvent.key.keysym.modifiers and KMOD_CAPS > 0 ) then
      begin
        Include(ShiftStates, ssCaps)
      end else
      begin
        Exclude(ShiftStates, ssCaps);
      end;
      Event.Keyboard.Event:= PHX_KEY_PRESSED;
      Event.Keyboard.Key  := SDL2_KeyToPHXKey(SDLEvent.key.keysym.sym);
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
      Event.Keyboard.Key  := SDL2_KeyToPHXKey(SDLEvent.key.keysym.sym);
      Event.Keyboard.Shift:= ShiftStates;
      TPHXEvents.Notify(Self, Event);
    end;
     SDL_QUIT_EVENT:
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
      Event.Mouse.Button:= SDL2_ButtonToPHXButton(SDLEvent.Button.Button);
      Event.Mouse.X     := SDLEvent.Motion.X;
      Event.Mouse.Y     := SDLEvent.Motion.Y;
      Event.Mouse.Shift := ShiftStates;
      TPHXEvents.Notify(Self, Event);
    end;
     SDL_MOUSEBUTTONDOWN:
    begin
      Event.Mouse.Event := PHX_MOUSE_PRESSED;
      Event.Mouse.Button:= SDL2_ButtonToPHXButton(SDLEvent.Button.Button);
      Event.Mouse.X     := SDLEvent.Motion.X;
      Event.Mouse.Y     := SDLEvent.Motion.Y;
      Event.Mouse.Shift := ShiftStates;
      TPHXEvents.Notify(Self, Event);
    end;
     SDL_WINDOWEVENT_RESIZED:
    begin    //SDL_GetWindowSize
      SDL_GetWindowSize(FWindow,FWidth,FHeight);
      Event.Device.Event := PHX_DEVICE_RESIZED;
      Event.Device.Width := FWidth;
      Event.Device.Height:= FHeight;
      TPHXEvents.Notify(Self, Event);
    end;
     SDL_WINDOWEVENT_ENTER :    //todo leave
    begin
        Event.Device.Event := PHX_DEVICE_ACTIVATED;
        Event.Device.Width := FWidth;
        Event.Device.Height:= FHeight;
        Event.Device.Shift := ShiftStates;
        TPHXEvents.Notify(Self, Event);
      end;
    SDL_WINDOWEVENT_LEAVE or SDL_WINDOWEVENT_FOCUS_LOST:    //todo leave
    begin
        Event.Device.Event := PHX_DEVICE_DEACTIVATED;
        Event.Device.Width := FWidth;
        Event.Device.Height:= FHeight;
        Event.Device.Shift := ShiftStates;
        TPHXEvents.Notify(Self, Event);
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

function TPHXOpenGLRendererSDL2.GetWidth: Integer;
begin
  Result:= FWidth;
end;

function TPHXOpenGLRendererSDL2.GetHeight: Integer;
begin
  Result:= FHeight;
end;

function TPHXOpenGLRendererSDL2.GetFlags: TPHXWindowFlags;
begin
  Result := FWindowFlags;
end;

procedure TPHXOpenGLRendererSDL2.SetTitle(const Title: String);
begin
  FTitle:= Title;
  SDL_SetWindowTitle(FWindow,PChar(FTitle));
end;

procedure TPHXOpenGLRendererSDL2.SetFlags(const Flags: TPHXWindowFlags);
begin
    FWindowFlags := Flags;
   if not FInitialized then Exit;

   if wfVerticalSync in FWindowFlags then
  begin
  SDL_GL_SetSwapInterval(1);
  end else
  begin
  SDL_GL_SetSwapInterval(0);
  end;



   if wfCursor in FWindowFlags then
  begin
    SDL_ShowCursor(SDL_ENABLE);
  end else
  begin
    SDL_ShowCursor(SDL_DISABLE);
  end;

end;

procedure TPHXOpenGLRendererSDL2.SetIcon(const Icon: String);
begin
 //SDL_SetWindowIcon
end;

constructor TPHXOpenGLRendererSDL2.Create;
begin
  inherited Create;
  FInitialized := False;
  FWindow := nil;
  FTitle := 'Phoenix Framework';
  FWidth := 800;
  FHeight := 600;
  FFullscreen := False;
  FWindowFlags := DefaultWindowFlags;
  ExitProc := @SDL_Quit;
end;

destructor TPHXOpenGLRendererSDL2.Destroy;
begin
  inherited Destroy;
end;

procedure TPHXOpenGLRendererSDL2.EnumDisplayModes(const Modes: TPHXDisplayModes);
begin

end;

procedure TPHXOpenGLRendererSDL2.Initialize(const Parameters: TPHXDeviceParameters);
var Flags_: Cardinal;
begin
  FTitle := Parameters.Title;
  FWidth := Parameters.Width;
  FHeight := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

  if SDL_Init( SDL_INIT_VIDEO ) < 0 then raise Exception.Create('Failed to initialize SDL2.');
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

  Flags_:= SDL_WINDOW_SHOWN;
  if wfResizable in FWindowFlags then
  begin
    Flags_:= SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE;
  end;

  if FFullscreen then
  begin
    Flags_:= Flags_ or SDL_WINDOW_FULLSCREEN or SDL_WINDOW_BORDERLESS;
  end;

  FWindow := SDL_CreateWindow(PChar(FTitle),SDL_WINDOWPOS_UNDEFINED,
  SDL_WINDOWPOS_UNDEFINED, FWidth, FHeight, SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE  );

  FSurface := SDL_GetWindowSurface(FWindow);

  if wfVerticalSync in FWindowFlags then
  begin
  SDL_GL_SetSwapInterval(1);
  end else
  begin
  SDL_GL_SetSwapInterval(0);
  end;

  SDL_JoystickEventState(SDL_ENABLE);
  //InitJoysticks;
  if (dglOpenGL.InitOpenGL <> True) then
    begin
      raise Exception.Create('OpenGL Initialization failed.');
    end;
    dglOpenGL.ReadExtensions;
    InitializeOpenGL;
    FInitialized:= True;
end;

procedure TPHXOpenGLRendererSDL2.Reinitialize(const Parameters: TPHXDeviceParameters);
begin

   //todo

  if ( FSurface = nil ) then
  begin
    TPHXLogger.Error('TPHXOpenGL_Renderer_SDL.Initialize', 'Unable to set up video: %s\n', [ SDL_GetError() ]);
    raise Exception.CreateFmt('Unable to set up video: %s\n', [SDL_GetError()] );
  end;
  TPHXNotifications.Notify(dnContextCreated);
end;

procedure TPHXOpenGLRendererSDL2.Finalize;
begin

end;

procedure TPHXOpenGLRendererSDL2.Update;
var Event: TSDL_Event;
begin
  while SDL_PollEvent( Event ) > 0 do
  begin
    SDLEvent(Event);
  end;
end;

procedure TPHXOpenGLRendererSDL2.Clear;
begin
  inherited Clear;
end;

procedure TPHXOpenGLRendererSDL2.Flip;
begin
  SDL_GL_SwapWindow(FWindow);
end;


initialization
  RegisterProvider(ProviderName, TPHXOpenGL_Provider_SDL2);


end.

